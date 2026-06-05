% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__functional_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_powers_allocation__functional_accommodation_reading, []).

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
 *   constraint_id: war_powers_allocation__functional_accommodation_reading
 *   human_readable: War Powers Allocation: Functional Accommodation Reading
 *   domain: constitutional_law/separation_of_powers
 *
 * SUMMARY:
 *   The functional accommodation reading of war powers allocation claims that
 *   the constitutional division of authority between Congress and the
 *   executive branch is context-dependent: imminent threats permit unilateral
 *   executive action without prior Congressional authorization, while
 *   prolonged campaigns require explicit Congressional authorization. This
 *   reading represents a compromise position between two other
 *   interpretations: the congressional primacy reading (Congress holds war
 *   power and the executive may only defend against sudden attack) and the
 *   inherent executive reading (the president as commander-in-chief holds
 *   inherent war-making authority constrained only by Constitutional text and
 *   practical Congressional override). The functional accommodation reading
 *   instantiates a tangled_rope constraint: it coordinates genuine
 *   institutional needs (rapid emergency response + sustained deliberative
 *   oversight) but creates asymmetric extraction (the executive gains
 *   unilateral action capacity; Congress loses its gating function in the
 *   gray zone between imminent threat and prolonged campaign). The
 *   constraint's extractiveness and suppression have increased over the
 *   30-year measurement interval as the executive's threat assessment has
 *   expanded and the 'imminent threat' exception has been invoked more
 *   frequently to bypass Congressional authorization.
 *
 * KEY AGENTS:
 *   - Executive Branch: Primary beneficiary (institutional/arbitrage) — gains unilateral action authority in gray zone without requirement to seek Congressional authorization; can frame future actions as imminent threats
 *   - Congress: Primary secondary beneficiary (organized/constrained) — maintains nominal authority for prolonged campaigns but faces high costs in challenging executive assertion after initial action; benefits from plausible deniability on tactical outcomes
 *   - Constitutional War Powers Authority: Primary victim (powerless/trapped) — the categorical principle that Congress declares war cannot exit the ambiguity created by functional accommodation; loses clarity and enforceability
 *   - Public Democratic Deliberation: Primary victim (powerless/trapped) — citizens cannot assess whether military action is constitutionally authorized when imminent threat exception bypasses deliberation; theater ratio high because process legitimacy is compromised
 *   - Constitutional War Declaration Mechanism: Secondary victim (institutional/arbitrage but degraded to piton) — formal Article I declaration process persists ceremonially while actual authorization occurs through AUMF or executive assertion; displacement to AUMF and executive authority shows degradation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__functional_accommodation_reading, 0.52).
domain_priors:suppression_score(war_powers_allocation__functional_accommodation_reading, 0.58).
domain_priors:theater_ratio(war_powers_allocation__functional_accommodation_reading, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__functional_accommodation_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__functional_accommodation_reading, "War Powers Allocation: Functional Accommodation Reading").
narrative_ontology:topic_domain(war_powers_allocation__functional_accommodation_reading, "constitutional_law/separation_of_powers").

domain_priors:requires_active_enforcement(war_powers_allocation__functional_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__functional_accommodation_reading, 'uke_kernel_war_powers_allocation_functional_accommodation_20260226').
narrative_ontology:cs_kernel_codification('uke_kernel_war_powers_allocation_functional_accommodation_20260226', formalized).
narrative_ontology:cs_authority_grounding('uke_kernel_war_powers_allocation_functional_accommodation_20260226', extraction).
narrative_ontology:cs_interpretation_layer_present('uke_kernel_war_powers_allocation_functional_accommodation_20260226').
narrative_ontology:cs_reading_relation('uke_kernel_war_powers_allocation_functional_accommodation_20260226', war_powers_allocation__congressional_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('uke_kernel_war_powers_allocation_functional_accommodation_20260226', war_powers_allocation__inherent_executive_reading, coexists_with).
narrative_ontology:cs_axiom('uke_kernel_war_powers_allocation_functional_accommodation_20260226', foundational, functional_context_dependent_authority_allocation).
narrative_ontology:cs_axiom_status(functional_context_dependent_authority_allocation, holdable).
narrative_ontology:cs_axiom_grounding('uke_kernel_war_powers_allocation_functional_accommodation_20260226', functional_context_dependent_authority_allocation, instrumental).
narrative_ontology:cs_axiom('uke_kernel_war_powers_allocation_functional_accommodation_20260226', foundational, imminent_threat_exception_requires_categorical_constraint).
narrative_ontology:cs_axiom_status(imminent_threat_exception_requires_categorical_constraint, holdable).
narrative_ontology:cs_axiom_grounding('uke_kernel_war_powers_allocation_functional_accommodation_20260226', imminent_threat_exception_requires_categorical_constraint, instrumental).
narrative_ontology:cs_reference_frame('uke_kernel_war_powers_allocation_functional_accommodation_20260226', framer_emergency_accommodation_intent).
narrative_ontology:cs_drift_state('uke_kernel_war_powers_allocation_functional_accommodation_20260226', contemporary_post_cold_war, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('uke_kernel_war_powers_allocation_functional_accommodation_20260226', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(war_powers_allocation__functional_accommodation_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__functional_accommodation_reading, executive_branch_operational_capability).
narrative_ontology:constraint_beneficiary(war_powers_allocation__functional_accommodation_reading, congressional_war_authorization_authority).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, constitutional_categorical_clarity).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, public_deliberation_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSTITUTIONAL CATEGORICAL CLARITY (SNARE) — The functional accommodation reading creates sustained ambiguity about who holds war powers authority. The categorical principle that 'Congress declares war' cannot exit from the extracted value of institutional clarity. Maximum experienced extraction through perpetual interpretive uncertainty.
constraint_indexing:constraint_classification(war_powers_allocation__functional_accommodation_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PUBLIC DELIBERATION INTEGRITY (SNARE) — Citizens cannot verify whether military action is authorized through constitutional process or executive assertion. The 'imminent threat' exception creates a perpetual gray zone where public judgment cannot assess legitimacy ex ante. Theater ratio increases because operational necessity is invoked to bypass deliberation.
constraint_indexing:constraint_classification(war_powers_allocation__functional_accommodation_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: CONGRESSIONAL WAR AUTHORIZATION AUTHORITY (TANGLED ROPE) — Congress maintains formal authority to authorize prolonged campaigns (genuine coordination function: requiring legislative deliberation for sustained commitments). But Congress faces high costs in challenging unilateral executive action after the fact (constrained exit: reasserting authority creates political friction). Congress benefits from being shielded from blame for tactical decisions while maintaining nominal final authority.
constraint_indexing:constraint_classification(war_powers_allocation__functional_accommodation_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: EXECUTIVE BRANCH OPERATIONAL CAPABILITY (ROPE) — The functional accommodation reading enables rapid response to imminent threats without legislative delay. The executive experiences the constraint as purely coordinative: it creates space for legitimate unilateral action when threats require immediate response. High arbitrage exit option (can act unilaterally in gray zone).
constraint_indexing:constraint_classification(war_powers_allocation__functional_accommodation_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL WAR DECLARATION CEREMONY (PITON) — The formal Article I war declaration process persists through institutional inertia. Formal declarations have become rare (last formal U.S. declaration: 1942) despite continuous military operations. The ceremony is performative — authorization for extended operations is ritualized through AUMF (Authorization for Use of Military Force) instead. Theater ratio high (0.75+) because the formal declaration mechanism is maintained but displaced.
constraint_indexing:constraint_classification(war_powers_allocation__functional_accommodation_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational scope, the functional accommodation reading represents a coherent framework: emergency authority (unilateral action) nested within deliberative authority (authorization for prolonged campaigns). This reading coordinates two genuine institutional needs (rapid response + democratic accountability). But the coordination is asymmetric — the executive has unilateral action capacity; Congress has only authorization authority post facto. Theater is high because the 'imminent threat' exception is invoked to bypass deliberation, not as a true emergency mechanism with automatic reversal.
constraint_indexing:constraint_classification(war_powers_allocation__functional_accommodation_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_powers_allocation__functional_accommodation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(war_powers_allocation__functional_accommodation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(war_powers_allocation__functional_accommodation_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_powers_allocation__functional_accommodation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(war_powers_allocation__functional_accommodation_reading, TR),
    TR >= 0.70.

:- end_tests(war_powers_allocation__functional_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The functional accommodation reading creates a gap between nominal authority (Congress declares war) and effective authority (executive acts in gray zone; Congress authorizes after fact or appropriates funds). The extraction is not maximal because Congress retains genuine authority for prolonged campaigns and can theoretically force a categorical choice on the executive. However, the gray zone (imminent threat vs. prolonged campaign) is large and contested, permitting substantial executive action without prior Congressional authorization. The measurement trajectory (0.35→0.45→0.52) reflects historical expansion of threat assessment scope and frequency of unilateral action invocation. Suppression (0.58): Moderate-high. Congressional actors face high costs in challenging executive threat assessment during active military operations (political/patriotic pressure to support troops). Citizens face informational barriers to assessing threat imminence ex ante. The executive faces low costs in invoking imminent threat and high costs only if Congressional challenge succeeds and public opinion shifts. Theater ratio (0.64): Moderate-high. The formal war declaration process persists ceremonially, but actual authorization occurs through AUMF, defense appropriations, or executive assertion. Congressional debate often affirms the executive's threat assessment rather than conducting independent evaluation. Theater has increased (0.40→0.64) as the gap between formal procedure and actual authorization practice has widened.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap divides the beneficiary (executive) and secondary beneficiary (Congress) from the victims (constitutional clarity, public deliberation). The executive sees the functional accommodation as pure coordination (Rope) — rapid response to legitimate threats. Congress sees mixed experience (Tangled Rope) — genuine authority maintained with asymmetric terms and constrained exit. The victims see pure extraction (Snare) — categorical authority lost, deliberation bypassed, clarity sacrificed. The analytical observer also classifies as Tangled Rope but recognizes that the asymmetry is substantial enough that the reading risks degradation toward Piton (ceremonial authority without function) if the executive systematically invokes the imminent-threat exception. The war declaration ceremony (Piton perspective) shows that formal constitutional process has atrophied while executive + legislative accommodation has become the operative mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is derived from the agent's structural position relative to the war powers allocation. Executive branch actors benefit from unilateral action capacity (low d, beneficiary status); Congressional actors face asymmetric terms (moderate d, mixed beneficiary/victim status — they benefit from authority but lose it in gray zone); victims of categorical clarity face extracted values (high d, trapped exit). The analytical observer (neutral position within the institutional structure) derives moderate-high d reflecting the asymmetric extraction embedded in the reading's core claim: the executive gains action authority; Congress and the public lose clarity and deliberation gating.
 *
 * MANDATROPHY ANALYSIS:
 *   The functional accommodation reading avoids mandatrophy by claiming to coordinate two genuine institutional needs: (1) Congress needs authority to ensure sustained military commitments have deliberative legitimacy, and (2) the executive needs authority to respond to genuine emergencies without legislative delay. The reading asserts that these can coexist through contextual allocation. However, the reading shows signs of unresolved mandatrophy if we examine whether the 'imminent threat' exception actually constrains executive action or merely provides rhetorical cover for expanded authority. If the exception is consistently invoked to bypass Congressional authorization (empirically resolvable through precedent analysis), the reading collapses into inherent executive power with Congressional accommodation as theater. The omega variables document this uncertainty.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    imminent_threat_definition_scope,
    'What temporal and operational criteria define ''imminent threat'' sufficiently to constrain the unilateral action exception?',
    'Precedent analysis: which prior executive unilateral actions were classified as imminent-threat responses? Correlation between threat imminence and action scope; does the executive''s assessment of imminence correlate with Congressional subsequent ratification or challenge?',
    'If definition is strict: unilateral action mechanism is narrowly bounded, and the reading approaches congressional primacy. If definition is expansive: the functional accommodation collapses into inherent executive power by stealth. This omega locates whether the reading''s core assumption (functional accommodation requires definitional constraint) is empirically stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imminent_threat_definition_scope, empirical, 'Temporal and operational boundaries of the imminent threat exception').

omega_variable(
    authorization_substitution_mechanism,
    'Does the congressional authorization mechanism (AUMF, defense appropriations) genuinely authorize prolonged campaigns or merely ratify executive assertions after the fact?',
    'Process analysis: do legislative debates over war authorizations independently evaluate the strategic rationale, or do they defer to executive threat assessment? Temporal comparison: authorization timing relative to initial unilateral action.',
    'If authorization is genuine deliberation: the tangled rope classification is correct (both coordination and asymmetric extraction). If authorization is post-hoc ratification: the constraint approaches pure snare for Congress (constrained authority with no meaningful deliberative function).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authorization_substitution_mechanism, empirical, 'Whether war authorization represents genuine deliberation or post facto ratification').

omega_variable(
    functional_accommodation_versus_constitutional_evasion,
    'Is the functional accommodation reading a genuine constitutional principle or a rationalization of de facto executive supremacy?',
    'Interpretive history: do the founding documents support the imminent-threat exception as a designed feature, or is it an accretion of executive practice? Comparative analysis: how do other democracies handle emergency war powers? Does the reading''s distinction (imminent vs. prolonged) correlate with actual constitutional text or is it judge-made doctrine?',
    'If designed feature: the reading is a legitimate constitutional principle with internal constraints. If accretion: the reading is committer-level naturalization of executive practice as if it were constitutional accommodation. This omega targets the kernel-level ambiguity: whether the war powers allocation is fundamentally about functional allocation or about suppressed contests for authority.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(functional_accommodation_versus_constitutional_evasion, conceptual, 'Whether functional accommodation is constitutional principle or rationalization of executive practice').

omega_variable(
    reading_survival_under_threat_inflation,
    'Can the functional accommodation reading survive when executive threat assessment is systematically inflated or when ''imminent'' threats materialize as false positives?',
    'Historical case analysis: instances where executive threat assessment was later revealed as inflated or incorrect; correlation with whether Congress subsequently challenged the authorization or the reading''s legitimacy eroded. Institutional learning: has institutional practice converged on tighter constraints on ''imminent'' in response to false positives, or has the exception expanded?',
    'If the reading is robust under threat inflation: suppression is high (institutional actors cannot exit the authority allocation even when threat claims fail). If the reading erodes with false positives: the constraint is sensitive to epistemic failures, and the reading''s viability depends on accurate threat assessment (which is partly outside the reading''s control).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_survival_under_threat_inflation, empirical, 'Resilience of functional accommodation reading under threat assessment inflation').

omega_variable(
    kernel_reading_committer_ambiguity,
    'Does the war powers kernel instantiate a genuine functional accommodation principle, or is the functional accommodation reading a cover story that legitimizes executive power-seeking while preserving congressional nominal authority?',
    'Examine whether the reading''s constraints (imminent vs. prolonged distinction) are self-enforcing or whether their violation is costless. If executive evasion of the constraint (claiming imminent threat for prolonged campaigns) triggers Congressional challenge with high frequency and success, the reading is structural. If evasion is costless or routine, the reading is committer-level theater masking executive dominance.',
    'If reading is genuine principle: the constraint is tangled rope with asymmetric terms that Congress can enforce. If reading is theater: the constraint approaches piton (ceremonial authority without function) or pure snare (executive dominance disguised as accommodation). This directly affects whether the reading forecloses, coexists with, or influences the sibling readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_committer_ambiguity, conceptual, 'Whether functional accommodation is constitutional principle or naturalized executive practice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__functional_accommodation_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(warpo_tr_t0, war_powers_allocation__functional_accommodation_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(warpo_tr_t15, war_powers_allocation__functional_accommodation_reading, theater_ratio, 15, 0.55).
narrative_ontology:measurement(warpo_tr_t30, war_powers_allocation__functional_accommodation_reading, theater_ratio, 30, 0.64).

% Extraction over time
narrative_ontology:measurement(warpo_be_t0, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(warpo_be_t15, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 15, 0.45).
narrative_ontology:measurement(warpo_be_t30, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 30, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(warpo_su_t0, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(warpo_su_t15, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 15, 0.52).
narrative_ontology:measurement(warpo_su_t30, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_powers_allocation__functional_accommodation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(war_powers_allocation__functional_accommodation_reading, war_powers_allocation__congressional_primacy_reading).
narrative_ontology:affects_constraint(war_powers_allocation__functional_accommodation_reading, war_powers_allocation__inherent_executive_reading).

% DUAL FORMULATION NOTE:
% The war_powers_allocation kernel instantiates three structurally distinct constraint stories, each representing a different reading with different extractiveness values and different perspectival structures. The functional_accommodation_reading (this story, ε=0.52) coordinates genuine institutional needs but creates asymmetric authority allocation. The congressional_primacy_reading (sibling, ε≈0.20) asserts categorical Congressional authority with minimal executive exception. The inherent_executive_reading (sibling, ε≈0.65) asserts executive dominance with Congressional override as nominal only. Each reading is a distinct constraint with its own beneficiary/victim structure, theater ratio, and measurement trajectory. They are linked through the kernel but not collapsed into one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
