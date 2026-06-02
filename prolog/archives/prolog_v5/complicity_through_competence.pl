% ============================================================================
% CONSTRAINT STORY: complicity_through_competence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_complicity_through_competence, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: complicity_through_competence
 *   human_readable: Complicity Through Competence: Professional Excellence as Participation in Harm
 *   domain: organizational_psychology/ethics_of_expertise/systems_of_complicity
 *
 * SUMMARY:
 *   Complicity through competence describes the structural trap where
 *   professional excellence in role performance constitutes participation in
 *   organizational harm without requiring moral agreement with the harm. The
 *   constraint operates through the fusion of professional identity with role
 *   competence: the better you are at your job, the more effectively you
 *   enable the system, and the more your identity becomes constituted through
 *   that competence. This creates a cognitive lock where exit would require
 *   abandoning not just a career but a self-concept built on mastery and
 *   expertise. The constraint is distinct from simple economic dependency
 *   (which would be 'trapped' rather than 'identity_locked') and from
 *   ideological capture (where the professional agrees with the harm). The
 *   professional may see the harm clearly, may privately object, but
 *   continues high-quality performance because their identity is fused with
 *   being good at what they do. The theater ratio (0.58) reflects the
 *   performative ethics infrastructure that organizations build around
 *   complicit professionals: ethics training, compliance departments,
 *   whistleblower hotlines that exist to demonstrate concern while channeling
 *   dissent into ineffective processes. The extractiveness has increased over
 *   the interval (0.48 → 0.68) as organizations have become more
 *   sophisticated at leveraging professional identity as a binding mechanism,
 *   and as career paths have become more specialized (reducing exit options).
 *   This constraint is downstream of capability_compulsion_gradient (the
 *   structural pressure to use skills you have) and
 *   optimization_as_entrapment (the trap of incremental performance
 *   improvement), but represents a distinct mechanism: the fusion of
 *   competence with complicity.
 *
 * KEY AGENTS:
 *   - The Competent Professional: Primary victim (powerless/identity_locked) — identity fused with role competence; exit would require abandoning professional self-concept; sees harm but cannot stop performing well
 *   - The Mid-Career Specialist: Secondary victim (moderate/constrained) — faces high exit costs (specialized skills, sunk career investment, family obligations) but not yet identity-locked; still experiences role as separable from self
 *   - Organizational Leadership: Primary beneficiary (institutional/arbitrage) — captures value of professional competence while externalizing moral costs; can exit to other organizations without identity disruption
 *   - Professional Association: Mixed position (organized/mobile) — benefits from maintaining professional standards (which enable complicity) but also provides collective voice for ethical reform; experiences constraint as tangled rope
 *   - Harm Targets: Tertiary victim (powerless/trapped) — bear downstream consequences of professional competence applied to harmful ends; no voice in the system
 *   - Whistleblower Potential: Suppressed counterfactual (powerless/trapped) — the professionals who might have objected but were bound by identity fusion or career constraints
 *   - Analytical Observer: Sees the structural mechanism (analytical/analytical) — recognizes both the genuine coordination function (professional competence enables complex organizational tasks) and the extraction (competence becomes complicity when applied to harm)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(complicity_through_competence, 0.68).
domain_priors:suppression_score(complicity_through_competence, 0.72).
domain_priors:theater_ratio(complicity_through_competence, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(complicity_through_competence, extractiveness, 0.68).
narrative_ontology:constraint_metric(complicity_through_competence, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(complicity_through_competence, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(complicity_through_competence, snare).
narrative_ontology:human_readable(complicity_through_competence, "Complicity Through Competence: Professional Excellence as Participation in Harm").
narrative_ontology:topic_domain(complicity_through_competence, "organizational_psychology/ethics_of_expertise/systems_of_complicity").

domain_priors:requires_active_enforcement(complicity_through_competence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(complicity_through_competence, institutional_continuity).
narrative_ontology:constraint_beneficiary(complicity_through_competence, organizational_leadership).
narrative_ontology:constraint_beneficiary(complicity_through_competence, professional_reputation_systems).
narrative_ontology:constraint_victim(complicity_through_competence, individual_conscience).
narrative_ontology:constraint_victim(complicity_through_competence, harm_targets).
narrative_ontology:constraint_victim(complicity_through_competence, whistleblower_potential).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

constraint_indexing:constraint_classification(complicity_through_competence, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

constraint_indexing:constraint_classification(complicity_through_competence, snare,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(regional))).

constraint_indexing:constraint_classification(complicity_through_competence, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

constraint_indexing:constraint_classification(complicity_through_competence, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

constraint_indexing:constraint_classification(complicity_through_competence, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(complicity_through_competence_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(complicity_through_competence, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(complicity_through_competence, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(complicity_through_competence, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(complicity_through_competence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts from individual conscience and moral agency while benefiting institutional continuity and leadership. The professional's competence is leveraged for organizational ends that may conflict with their values, but the identity fusion prevents exit. The extraction is not total (0.68 rather than 0.85+) because some professionals do exit, some organizations do reform, and the competence itself has genuine value (it's not pure waste). The value has increased over the interval as organizations have become more sophisticated at binding professionals through identity rather than just economic dependency. Suppression (0.72): High. Multiple mechanisms suppress exit and voice: (1) Identity fusion makes exit psychologically costly (abandoning professional self-concept). (2) Specialized career paths reduce alternative employment options. (3) Routine normalizes harm (the banality of evil mechanism). (4) Performance metrics focus attention on role competence rather than ethical outcomes. (5) Organizational ethics infrastructure channels dissent into ineffective processes. (6) Professional reputation systems punish whistleblowing. Suppression is not total because some professionals do break the identity lock, some organizations do have genuine ethics mechanisms, and some professional associations do support whistleblowers. Theater ratio (0.58): Moderate-high. Organizations build substantial performative ethics infrastructure around complicit professionals: ethics training that teaches compliance rather than moral reasoning, whistleblower hotlines that document complaints without acting on them, diversity and inclusion initiatives that demonstrate concern while preserving power structures, compliance departments that exist to shield leadership from liability rather than prevent harm. The theater has increased over the interval as organizations have learned that visible ethics infrastructure can actually strengthen the complicity mechanism by providing professionals with the rationalization that 'the organization cares about ethics, so my role performance must be acceptable.' The theater is not total (0.58 rather than 0.75+) because some ethics infrastructure does function, some training does change behavior, and some compliance mechanisms do prevent harm.
 *
 * PERSPECTIVAL GAP:
 *   The identity-locked professional sees a snare — they are trapped by their own competence and cannot exit without abandoning their professional identity. The mid-career specialist also sees a snare but with slightly lower effective extraction because they retain some separation between role and self. Organizational leadership sees a rope — professional competence is a coordination mechanism that enables complex organizational tasks; the moral costs are externalized and not experienced by leadership. The professional association sees a tangled rope — genuine coordination (professional standards enable valuable work) mixed with extraction (those same standards bind professionals to harmful roles). The analytical observer also sees a tangled rope — the constraint has both a real coordination function (professional competence is genuinely valuable) and a real extraction mechanism (competence becomes complicity when applied to harmful ends). The perspectival gap is diagnostic: the beneficiaries experience coordination, the victims experience extraction, and the analytical view sees both. The identity_locked exit option is critical — it differentiates this constraint from simple economic dependency and reveals the cognitive mechanism that makes exit psychologically impossible despite structural mobility.
 *
 * DIRECTIONALITY LOGIC:
 *   The identity-locked professional (powerless/identity_locked) is the primary victim with maximum directionality (d ≈ 0.89). Their identity fusion with professional competence creates a cognitive lock that is structurally different from economic dependency. They could leave (they have marketable skills, financial resources, geographic mobility) but cannot because their self-concept is constituted through role performance. The mid-career specialist (moderate/constrained) faces high exit costs but is not yet identity-locked — they experience the constraint as a difficult choice rather than an impossible one (d ≈ 0.85). Organizational leadership (institutional/arbitrage) are the primary beneficiaries with low directionality (d ≈ 0.05) — they capture the value of professional competence while externalizing moral costs, and can exit to other organizations without identity disruption. The professional association (organized/mobile) has moderate directionality (d ≈ 0.55) — they benefit from professional standards that enable complicity but also bear reputational costs when complicity becomes visible, and they have collective voice to push for reform. The analytical observer (analytical/analytical) has the standard analytical directionality (d ≈ 0.72), seeing both the coordination function and the extraction mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   NOT YET RESOLVED. The constraint exhibits high extractiveness (0.68) and requires mandatrophy resolution per schema rules (extractiveness > 0.70 requires mandatrophy_resolved: true). However, the current analysis has not yet completed the mandatrophy resolution. The key question: Is professional competence applied to harmful ends better classified as (a) a snare (pure extraction of moral agency), (b) a tangled rope (genuine coordination value mixed with moral extraction), or (c) a mountain (an inherent feature of professional specialization that cannot be eliminated)? The analytical perspective currently classifies as tangled_rope, which suggests the mandatrophy resolution will land there: professional competence has genuine coordination value (it enables complex organizational tasks that benefit society) AND it creates complicity risk (that same competence can be applied to harmful ends). The resolution mechanism: demonstrate that the coordination function is real (professional competence does enable valuable work that couldn't happen otherwise) AND that the extraction is real (competence binds professionals to harmful roles through identity fusion). The constraint is not a mountain because the identity fusion is contingent (some professionals do break it, some organizations do structure roles to prevent it, some cultures have different professional identity norms). The constraint is not pure extraction because the competence itself has value. The tangled_rope classification captures both: genuine coordination function (professional standards enable complex work) mixed with asymmetric extraction (identity fusion traps competent professionals in complicit roles).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_knowledge_threshold,
    'At what point does professional competence require moral knowledge of downstream harm?',
    'Legal precedent analysis of professional liability cases; philosophical analysis of epistemic vs moral responsibility boundaries; empirical study of when professionals report awareness of harm',
    'If threshold is low (professionals should know): complicity is direct and extractiveness increases. If threshold is high (professionals can legitimately not know): constraint is coordination failure rather than extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(moral_knowledge_threshold, conceptual, 'Threshold at which professional competence implies moral knowledge of harm').

omega_variable(
    exit_cost_vs_complicity_severity,
    'How do we weigh the personal cost of exit against the severity of complicity?',
    'Moral philosophy frameworks (consequentialism vs deontology vs virtue ethics); empirical data on whistleblower outcomes vs harm prevented; cultural variation in professional loyalty norms',
    'If exit cost is weighted heavily: constraint is structural trap (snare). If complicity severity dominates: constraint is moral failure (individual responsibility). Determines whether identity_locked is legitimate or rationalization.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exit_cost_vs_complicity_severity, preference, 'Relative weighting of exit cost vs complicity severity').

omega_variable(
    routine_as_suppression_mechanism,
    'Does routine professional practice itself suppress moral awareness, or do professionals actively suppress evidence?',
    'Psychological studies of moral disengagement in professional contexts; organizational behavior research on normalization of deviance; comparison of error rates in routine vs novel tasks',
    'If routine suppresses passively: suppression is structural (high). If professionals actively suppress: suppression is agential (lower, but culpability higher). Changes whether identity_locked is cognitive capture or motivated reasoning.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(routine_as_suppression_mechanism, empirical, 'Whether routine itself suppresses moral awareness or requires active suppression').

omega_variable(
    competence_signal_vs_harm_correlation,
    'Is professional excellence genuinely correlated with harm production, or is the correlation spurious (both caused by institutional selection)?',
    'Longitudinal tracking of professional performance metrics vs ethical outcomes; natural experiments where institutional incentives change; cross-institutional comparison of competence-harm correlation',
    'If genuine correlation: the constraint is inherent to professional excellence (higher extractiveness). If spurious: the constraint is institutional design failure (lower extractiveness, more amenable to reform).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_signal_vs_harm_correlation, empirical, 'Whether professional excellence is genuinely correlated with harm or spuriously').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(complicity_through_competence, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_comp_tr_t0, complicity_through_competence, theater_ratio, 0, 0.35).
narrative_ontology:measurement(comp_comp_tr_t3, complicity_through_competence, theater_ratio, 3, 0.45).
narrative_ontology:measurement(comp_comp_tr_t6, complicity_through_competence, theater_ratio, 6, 0.52).
narrative_ontology:measurement(comp_comp_tr_t10, complicity_through_competence, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(comp_comp_be_t0, complicity_through_competence, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(comp_comp_be_t3, complicity_through_competence, base_extractiveness, 3, 0.56).
narrative_ontology:measurement(comp_comp_be_t6, complicity_through_competence, base_extractiveness, 6, 0.62).
narrative_ontology:measurement(comp_comp_be_t10, complicity_through_competence, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(complicity_through_competence, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is downstream of capability_compulsion_gradient (the structural pressure to use skills you have) and optimization_as_entrapment (the trap of incremental performance improvement). It represents a distinct mechanism: the fusion of professional identity with role competence creates complicity without requiring moral agreement. The upstream constraints establish the capability pressure and the optimization trap; this constraint describes what happens when professional identity becomes constituted through competent performance of a harmful role.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
