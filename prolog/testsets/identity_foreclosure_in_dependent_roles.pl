% ============================================================================
% CONSTRAINT STORY: identity_foreclosure_in_dependent_roles
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_identity_foreclosure_in_dependent_roles, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: identity_foreclosure_in_dependent_roles
 *   human_readable: Identity Foreclosure in Dependent Roles
 *   domain: social/psychological/organizational
 *
 * SUMMARY:
 *   Identity foreclosure in dependent roles describes the structural
 *   constraint whereby individuals in subordinate or dependent positions
 *   become unable to form or express identities outside the role's prescribed
 *   boundaries. This occurs across multiple domains: child-parent
 *   relationships, mentorship dynamics, employment hierarchies, caregiver
 *   arrangements, institutional positions, and community roles. The
 *   constraint exhibits hybrid characteristics: it genuinely coordinates
 *   behavior (roles do organize activity and provide predictability) while
 *   simultaneously narrowing available identity options for dependent agents.
 *   The mechanism is not purely external coercion but combines structural
 *   barriers (exit costs) with internalized identity fusion (the agent cannot
 *   imagine themselves outside the role). Theater increases over the interval
 *   as role maintenance becomes increasingly performative — the role's
 *   coordinating function atrophies while its identity-standardizing function
 *   strengthens. This pattern distinguishes identity foreclosure from pure
 *   role coordination: pure coordination would maintain extractiveness while
 *   theater remains low; pure extraction would maintain high theater while
 *   extractiveness grows. Identity foreclosure shows both growing —
 *   extraction accumulates through identity standardization, theater rises
 *   through performative role maintenance.
 *
 * KEY AGENTS:
 *   - Dependent Agent (powerless/identity_locked): Primary victim — structurally mobile but identity-fused with the role; cannot exercise exit despite availability
 *   - Secondary Dependent (moderate/constrained): Secondary victim — faces material exit barriers (financial, social, credential-based) alongside identity constraints
 *   - Role Gatekeeper (institutional/arbitrage): Primary beneficiary — captures coordination benefits and identity standardization labor; can reshape role or exit without identity loss
 *   - Identity Liberation Coalition (organized/constrained): Tertiary actor — organized advocates for identity diversity within roles; working to reduce foreclosure and create exit pathways
 *   - Institutional Role Maintenance System (institutional/arbitrage): System-level beneficiary — perpetuates role structure through theatrical reinforcement despite atrophied coordination function
 *   - Analytical Observer (analytical/analytical): At risk of naturalizing contingent arrangements as immutable laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(identity_foreclosure_in_dependent_roles, 0.58).
domain_priors:suppression_score(identity_foreclosure_in_dependent_roles, 0.65).
domain_priors:theater_ratio(identity_foreclosure_in_dependent_roles, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(identity_foreclosure_in_dependent_roles, extractiveness, 0.58).
narrative_ontology:constraint_metric(identity_foreclosure_in_dependent_roles, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(identity_foreclosure_in_dependent_roles, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(identity_foreclosure_in_dependent_roles, tangled_rope).
narrative_ontology:human_readable(identity_foreclosure_in_dependent_roles, "Identity Foreclosure in Dependent Roles").
narrative_ontology:topic_domain(identity_foreclosure_in_dependent_roles, "social/psychological/organizational").

domain_priors:requires_active_enforcement(identity_foreclosure_in_dependent_roles).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(identity_foreclosure_in_dependent_roles, role_gatekeepers).
narrative_ontology:constraint_beneficiary(identity_foreclosure_in_dependent_roles, identity_standardizers).
narrative_ontology:constraint_victim(identity_foreclosure_in_dependent_roles, dependent_agents).
narrative_ontology:constraint_victim(identity_foreclosure_in_dependent_roles, individual_identity_potential).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ROLE-BOUND AGENT (SNARE) — Structurally mobile (could leave the role) but identity-fused with the role itself. The agent's self-concept is constituted through the dependent position: 'I am the caregiver,' 'I am the student,' 'I am the assistant.' Exit would require abandoning not just the role but the identity built around it. Identity lock prevents exercising available exit options despite structural mobility.
constraint_indexing:constraint_classification(identity_foreclosure_in_dependent_roles, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: SECONDARY DEPENDENT (TANGLED ROPE) — Face genuine but surmountable barriers to exit: financial cost, social penalty, relocation burden, credential loss. Benefits modestly from the role relationship (mentorship, access, stability) while bearing extraction (labor demands, autonomy constraints, foreclosed identity options). Meaningful agency but constrained by costs.
constraint_indexing:constraint_classification(identity_foreclosure_in_dependent_roles, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ROLE GATEKEEPER (ROPE) — Institutional actor (employer, caregiver, mentor) who defines and enforces role expectations. Experiences the constraint as coordination: standardizing role behavior enables organizational function. Net beneficiary with exit options — can reshape the role, transfer responsibility, or exit entirely without personal identity loss.
constraint_indexing:constraint_classification(identity_foreclosure_in_dependent_roles, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: IDENTITY LIBERATION COALITION (ORGANIZED) — Social movements, therapy communities, and identity-affirming institutions recognize foreclosure and actively work to expand available identities within roles. See the constraint as hybrid: coordination function (roles do organize behavior) mixed with extractive identity narrowing. Organized enough to create alternative identity framings and exit pathways. Lower effective extraction due to coalitional agency.
constraint_indexing:constraint_classification(identity_foreclosure_in_dependent_roles, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INSTITUTIONAL ROLE MAINTENANCE (PITON) — Traditional role structures (family hierarchies, occupational categories, institutional positions) persist through inertia and theatrical reinforcement: coming-of-age rituals, role initiation ceremonies, identity badges. The theater (theater_ratio = 0.55) reflects that much of role maintenance is symbolic rather than functionally necessary. The underlying function has atrophied — individuals could coordinate behavior without identity foreclosure — but the constraint persists.
constraint_indexing:constraint_classification(identity_foreclosure_in_dependent_roles, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a naturalizing frame, role-identity fusion is treated as inevitable: 'People are their roles,' 'Identity is socially constructed through role assignment,' 'Foreclosure is the price of coordination.' This perspective risks treating contingent institutional arrangements (identity standardization) as immutable laws of human cognition. The engine's false summit detector will flag this — identity foreclosure is institutionally enforced, not naturally emergent.
constraint_indexing:constraint_classification(identity_foreclosure_in_dependent_roles, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(identity_foreclosure_in_dependent_roles_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(identity_foreclosure_in_dependent_roles, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(identity_foreclosure_in_dependent_roles, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(identity_foreclosure_in_dependent_roles, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(identity_foreclosure_in_dependent_roles, TR),
    TR >= 0.70.

:- end_tests(identity_foreclosure_in_dependent_roles_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts identity optionality from dependent agents while providing some genuine coordination benefits. The extraction is neither maximal (rope coordination exists) nor minimal (victims bear real costs). The value increased from 0.35 to 0.58 over the interval, reflecting accumulating identity standardization pressure. Suppression (0.65): High. Multiple barriers prevent independent identity formation: role expectations enforced through reward/penalty, financial dependency making exit costly, social isolation limiting reality-testing contacts, and internalized identity fusion making exit psychologically unthinkable. Suppression is high but not total — some agents find identity reformation paths despite barriers. Theater (0.55): Moderate-high. Role maintenance includes genuine coordination (scheduling, responsibility clarity) but increasingly relies on performative reinforcement: coming-of-age rituals, role-badge identity markers, ceremonial role transition moments. The theater has increased as role functionality has declined. Claimed type (Tangled Rope): Justified by presence of (1) genuine coordination function (roles reduce behavioral uncertainty, organize collaboration), (2) asymmetric extraction (identity foreclosure affects dependent agents disproportionately), and (3) active enforcement (role gatekeepers deliberately maintain identity boundaries through expectations and sanctions).
 *
 * PERSPECTIVAL GAP:
 *   The dependent agent and the role gatekeeper experience fundamentally different constraints. The dependent agent, identity-locked, perceives the role as inescapable — leaving the role feels like ceasing to exist (snare from their perspective). The gatekeeper, with arbitrage options, perceives the role as a coordination mechanism that they maintain and can modify or abandon at will (rope from their perspective). The secondary dependent, with material exit barriers but less identity fusion, perceives hybrid coordination-extraction (tangled_rope). The organized coalition perceives both real constraints and real agency in reform — they see extractiveness declining as identity-flexible role alternatives emerge (organized perspective sees lower χ). The institutional role maintenance system (piton) perceives its own degradation — it maintains role boundaries through increasingly performative mechanisms (ceremony, ritual, identity badging) because the functional coordination benefit has atrophied. The false summit (mountain) emerges when observers naturalize identity foreclosure as inevitable feature of human social cognition rather than contingent institutional enforcement. The perspectival gap reveals the core mandatrophy: is this 'just how roles work' (rope/mountain) or is it extractive identity standardization (snare/tangled_rope)? The answer: both are true, depending on structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from beneficiary/victim status and exit options. The dependent agent (identity_locked exit + victim status) has high d (~0.89) → high f(d) → high χ (strong experienced extraction). The role gatekeeper (beneficiary status + arbitrage exit) has low d (~0.15) → low/negative f(d) → negative/minimal χ (minimal experienced extraction). The secondary dependent (constrained exit + partial victim status) has moderate d (~0.55) → moderate f(d) → moderate χ. The organized coalition (constrained exit but organized power + partial beneficiary from movement growth + partial victim from system dynamics) has negotiated d (~0.40-0.50) → modified f(d) → lower χ due to coalitional agency. The piton institutional perspective (arbitrage exit + beneficiary from institutional continuity) has low d (~0.15) → negative f(d) but is overridden by theater_ratio gate (≥0.70 would override). At theater_ratio=0.55, the piton classification rests on the atrophied coordination function (inertial persistence), not on extraction. The analytical mountain perspective risks deriving d from naturalization (~0.72, treating identity fusion as inevitable) but the false summit detector flags this as misclassification.
 *
 * MANDATROPHY ANALYSIS:
 *   Identity foreclosure in dependent roles resolves mandatrophy through perspectival disambiguation. The constraint is NOT 'is this coordination or extraction?' but 'whose perspective are we taking?' From the gatekeeper's view, it is pure coordination (rope) — they are managing legitimate role boundaries. From the dependent agent's view, it is pure extraction (snare) — they cannot exercise identity optionality. From the analytical observer's view, it risks becoming a false natural law (mountain) — 'identity is socially constructed through roles, so foreclosure is inevitable.' The mandatrophy is not mislabeling but perspective-blindness: each single perspective misses the hybrid nature of the constraint. The tangled_rope classification at the system level captures both: genuine coordination function (role boundary maintenance) + asymmetric extraction (identity foreclosure disproportionately constrains dependent agents) + active enforcement (role gatekeepers deliberately standardize identity). The perspectival set together reveals that identity foreclosure is NOT an immutable feature of role coordination; it is a contingent enforcement mechanism that persists because gatekeepers benefit and dependent agents lack coalitional power. The organized perspective demonstrates this: as coalitions form and norms shift toward identity flexibility, the same coordination function can be achieved with reduced extractiveness (scaffold trajectory visible).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_vs_structural_trap,
    'Is the agent''s inability to exit due to identity fusion (internalized) or due to material barriers (structural)?',
    'Longitudinal tracking: Do agents who exit the role and remain geographically/financially distant show persistence of identity lock symptoms? If yes, the binding is internalized; if no, the binding was structural.',
    'If internalized: the constraint is more extractive than suppression metrics suggest — the agent carries the lock after exit. If structural: suppression metrics accurately capture exit barriers. Classification may shift from snare to tangled_rope if exit becomes materially feasible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_structural_trap, empirical, 'Whether identity lock is internalized or structurally imposed').

omega_variable(
    role_function_necessity,
    'Is the identity foreclosure functionally necessary for the role''s coordination goal, or is it an arbitrary normative enforcement?',
    'Comparative analysis: Can the same coordination outcome be achieved with role-holders who maintain diverse identities within the role? Case studies of alternative role frameworks (multiples roles per person, identity-flexible role definitions, identity-agnostic task assignment).',
    'If necessary: extractiveness may be reclassified as coordination cost (rope perspective strengthened). If arbitrary: extractiveness confirmed as enforced identity standardization (snare/tangled_rope confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(role_function_necessity, empirical, 'Whether identity foreclosure is functionally necessary').

omega_variable(
    identity_reformation_timeline,
    'After exiting the dependent role, how long does it take for role-based identity foreclosure to begin lifting? Is there a critical window after which identity reformation becomes increasingly difficult?',
    'Longitudinal identity narrative analysis: interviews/journals from agents at 3mo, 6mo, 1yr, 5yr post-exit; qualitative assessment of identity fluidity recovery.',
    'If reformation is rapid (months): identity lock is shallow, reclassify exit_options to constrained. If reformation is slow or incomplete (years+): identity lock is deep, confirm identity_locked and snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_reformation_timeline, empirical, 'Timeline for identity reformation after role exit').

omega_variable(
    coal_critical_mass_effect,
    'Do organized coalitions advocating identity liberation reach a critical mass that triggers norms shift, reducing extractiveness, or do they remain epistemically isolated?',
    'Network analysis: size of identity liberation advocacy communities; correlation with measurable changes in role-holder identity diversity; institutional adoption of identity-flexible role frameworks.',
    'If critical mass reached: coalition perspective becomes dominant, extractiveness declines, scaffold or rope classifications become more prevalent. If isolated: piton perspective dominates, extractiveness remains stable or increases through theatrical reinforcement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coal_critical_mass_effect, empirical, 'Whether identity liberation coalitions reach critical mass').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(identity_foreclosure_in_dependent_roles, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(iden_tr_t0, identity_foreclosure_in_dependent_roles, theater_ratio, 0, 0.38).
narrative_ontology:measurement(iden_tr_t10, identity_foreclosure_in_dependent_roles, theater_ratio, 10, 0.48).
narrative_ontology:measurement(iden_tr_t20, identity_foreclosure_in_dependent_roles, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(iden_be_t0, identity_foreclosure_in_dependent_roles, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(iden_be_t10, identity_foreclosure_in_dependent_roles, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(iden_be_t20, identity_foreclosure_in_dependent_roles, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(identity_foreclosure_in_dependent_roles, identity_coordination).
narrative_ontology:affects_constraint(identity_foreclosure_in_dependent_roles, mentorship_power_asymmetry).
narrative_ontology:affects_constraint(identity_foreclosure_in_dependent_roles, occupational_identity_lock).
narrative_ontology:affects_constraint(identity_foreclosure_in_dependent_roles, family_role_rigidity).

% DUAL FORMULATION NOTE:
% Identity foreclosure is a family of related constraints with different ε values depending on institutional context. Mentorship (ε≈0.48), family caregiving (ε≈0.62), occupational hierarchy (ε≈0.55), and institutional membership (ε≈0.51) each have distinct coordination functions and extraction mechanisms. This story treats the generic structural pattern; domain-specific stories in the network refine the analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(identity_foreclosure_in_dependent_roles, organized, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
