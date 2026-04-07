% ============================================================================
% CONSTRAINT STORY: organizational_omerta
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_organizational_omerta, []).

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
 *   constraint_id: organizational_omerta
 *   human_readable: Organizational Omerta: The Code of Silence
 *   domain: organizational_dynamics/institutional_culture
 *
 * SUMMARY:
 *   Organizational omerta — the enforced code of silence that protects
 *   leadership from accountability for wrongdoing — is a structural
 *   constraint operating in nearly all hierarchical organizations. It
 *   functions as a pure extraction mechanism that concentrates power upward
 *   while suppressing truth-seeking and dissent. The constraint operates
 *   through both material barriers (employment dependency, retaliation risk,
 *   legal liability) and identity lock (employees whose professional identity
 *   is constituted through organizational membership cannot imagine exit
 *   without becoming different people). Omerta extracts compliance, silence,
 *   and complicity from trapped employees while benefiting senior leadership
 *   and institutional reputation managers. The extractiveness has increased
 *   over the 15-year measurement interval as digital monitoring, career
 *   atomization, and organizational control mechanisms have strengthened.
 *   Theater has also increased as organizations have deployed formal
 *   compliance mechanisms (hotlines, ethics training, investigation
 *   procedures) that are largely performative — they satisfy legal
 *   requirements and create appearance of accountability while actually
 *   funneling dissent back to the leadership they are meant to check. The
 *   constraint is self-perpetuating: each generation of insiders becomes
 *   trapped by omerta, and once trapped, many become enforcers of the code
 *   for the next generation, either out of investment in organizational
 *   legitimacy or because they internalize the suppression as normal.
 *
 * KEY AGENTS:
 *   - Whistleblower/Truth-Seeking Employee: Primary victim (powerless/trapped or identity_locked) — discovers wrongdoing but faces entrapment through material dependency or identity lock. Experiences maximum extraction.
 *   - Senior Leadership: Primary beneficiary (institutional/arbitrage) — controls the code, suppresses external accountability, maintains reputation. Experiences the constraint as coordination mechanism.
 *   - Complicit Middle Manager: Secondary actor (moderate/constrained) — enforces silence while managing team morale. Experiences mixed coordination and extraction.
 *   - Institutionalized Insider: Secondary victim (powerless/identity_locked) — long-tenured employee whose identity is fused with organization. Identity lock is the binding mechanism.
 *   - Compliance and Ethics Functions: Institutional theater (institutional/arbitrage) — formal investigative mechanisms that are largely performative. Maintain appearance of accountability without substance.
 *   - Analytical Observer: Observer (analytical/analytical) — sees omerta as pure extraction with minimal coordination function. Recognizes perpetuation cycle.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(organizational_omerta, 0.68).
domain_priors:suppression_score(organizational_omerta, 0.75).
domain_priors:theater_ratio(organizational_omerta, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(organizational_omerta, extractiveness, 0.68).
narrative_ontology:constraint_metric(organizational_omerta, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(organizational_omerta, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(organizational_omerta, snare).
narrative_ontology:human_readable(organizational_omerta, "Organizational Omerta: The Code of Silence").
narrative_ontology:topic_domain(organizational_omerta, "organizational_dynamics/institutional_culture").

domain_priors:requires_active_enforcement(organizational_omerta).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(organizational_omerta, senior_leadership).
narrative_ontology:constraint_beneficiary(organizational_omerta, institutional_reputation_managers).
narrative_ontology:constraint_victim(organizational_omerta, truth_seeking_employees).
narrative_ontology:constraint_victim(organizational_omerta, organizational_accountability).
narrative_ontology:constraint_victim(organizational_omerta, external_stakeholders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE WHISTLEBLOWER (SNARE) — Employee who discovers wrongdoing is trapped between organizational survival and ethical obligation. Material barriers to exit (financial dependency, employment history locked to organization, legal liability retaliation) combine with identity lock (professional identity constituted through organizational role). Cannot speak without destroying livelihood; cannot stay silent without complicity. Maximum experienced extraction.
constraint_indexing:constraint_classification(organizational_omerta, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE INSTITUTIONALIZED INSIDER (SNARE) — Long-tenured employee whose professional identity is fused with the organization. Has structural mobility (could seek external employment) but cannot exercise it because their identity frame is constituted through organizational membership. Breaking the code means becoming a different person — abandoning 20+ years of identity investment. The identity lock is the primary binding mechanism, more powerful than material barriers.
constraint_indexing:constraint_classification(organizational_omerta, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 3: SENIOR LEADERSHIP (ROPE) — Leadership perceives omerta as a coordination mechanism: unified messaging prevents external panic, preserves organizational trust, enables internal problem-solving without public interference. From leadership's position, the constraint solves a genuine coordination problem (preventing stakeholder overreaction). They have exit options through arbitrage (can shift to different organizations with reputation intact). They experience the constraint as enabling rather than extractive.
constraint_indexing:constraint_classification(organizational_omerta, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THE COMPLICIT MIDDLE MANAGER (TANGLED ROPE) — Intermediate managers face genuine coordination problem (implementing leadership directives while managing team morale) alongside asymmetric extraction (enforcing silence, suppressing dissent, bearing the load of internal moral compromise). They benefit from organizational stability and career advancement enabled by coordinated silence. They are constrained by career costs of defection but not trapped — exit is possible at significant but surmountable cost. Mixed experience of coordination and extraction.
constraint_indexing:constraint_classification(organizational_omerta, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: COMPLIANCE AND ETHICS FUNCTIONS (PITON) — Formal ethics and compliance mechanisms exist (hotlines, policies, investigation procedures) but are largely theatrical. They perform the function of appearing to investigate internally while protecting the organization from external liability. The formal mechanism persists through institutional inertia and legal requirement, not because it genuinely enables accountability. Whistleblower hotlines funnel reports back to leadership who suppress them. Theater ratio high — performative appearance of investigation without teeth.
constraint_indexing:constraint_classification(organizational_omerta, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: THE ANALYTICAL OBSERVER (SNARE) — From a civilizational perspective, organizational omerta is a pure extraction mechanism with minimal coordination function. The coordination rationale (unified messaging, stakeholder confidence) is post-hoc legitimation for a system that primarily extracts compliance through fear and identity lock. The constraint is self-perpetuating: each generation of insiders becomes trapped by the constraint they were forced to accept, then enforces it on the next generation. No time-limited sunset, no coordination benefit that couldn't be achieved through transparency.
constraint_indexing:constraint_classification(organizational_omerta, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(organizational_omerta_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(organizational_omerta, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(organizational_omerta, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(organizational_omerta, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(organizational_omerta, TR),
    TR >= 0.70.

:- end_tests(organizational_omerta_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts compliance, silence, and moral compromise from those it governs. Over the 15-year interval, extractiveness has increased as organizational surveillance and control mechanisms have strengthened (0.45 → 0.68), and as digital communication has reduced plausible deniability for wrongdoing. Suppression (0.75): Very high. Trapped employees face multiple barriers to exit and dissent: financial dependency on employment, career consequences of whistleblowing (blacklisting across industry), legal liability for disclosure (NDAs, intellectual property claims), and in some cases direct retaliation. Identity-locked employees face psychological barriers to exit that can be as effective as material barriers. Theater ratio (0.65): Moderate-high. Formal compliance mechanisms (hotlines, investigation procedures, ethics training) are substantially theatrical — they create appearance of accountability without providing meaningful channels for dissent or investigation. Over the interval, theater has increased as organizations have deployed these mechanisms while ensuring they funnel reports back to leadership (0.38 → 0.65).
 *
 * PERSPECTIVAL GAP:
 *   The widest perspectival gap is between the beneficiary perspective (leadership seeing rope/coordination) and the victim perspective (trapped employee seeing snare/extraction). Leadership genuinely solves a coordination problem through unified messaging and internal problem containment. But this legitimate coordination function does not require suppressing dissent to the degree that omerta achieves. The constraint extracts more than coordination requires. The identity-locked insider perspective reveals a gap within the victim class: some employees are materially trapped (would lose livelihood if they left), while others are identity-locked (have mobility but cannot exercise it). This gap is diagnostically important because it implies different intervention strategies — material constraints require structural changes (separation, external employment options), while identity locks require cognitive reframing (helping employees construct post-organizational identities). The piton perspective (theater ratio = 0.65) shows that formal compliance mechanisms are degraded — they perform the function of appearing to investigate rather than actually investigating. This suggests the constraint is slowly transitioning from pure extraction toward theatrical extraction as organizations respond to legal risk while preserving the substance of the code.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural position relative to the constraint. Leadership and reputation managers occupy beneficiary positions with arbitrage exit options (low d → negative effective extraction). Trapped employees occupy victim positions with no exit (high d → high effective extraction). Identity-locked employees have moderate-high d because their material mobility is real but cognitive trap prevents exercise of exit options — they are structurally constrained rather than completely trapped, but the identity lock creates effective immobility. Middle managers occupy mixed positions — they enforce the code and benefit from organizational stability (partial beneficiary status) but bear moral cost of complicity and face constraints on their own exit options (constrained rather than arbitrage). The analytical observer's directionality is derived from seeing the full structure from a neutral position, which produces high d (observer sees the constraint primarily targeting powerless agents) and analytical exit option.
 *
 * MANDATROPHY ANALYSIS:
 *   Organizational omerta demonstrates the mandatrophy in how institutional contexts naturalize extraction. Leadership genuinely coordinates internal problem-solving through the code of silence — there is a real coordination function. But the extracted cost (compliance, moral compromise, suppression of dissent) is far higher than coordination requires. The mandatrophy resolution is to distinguish between the minimal coordination function (keeping stakeholders informed through controlled channels without panic) and the maximal extraction function (total suppression of internal dissent). A transparent internal culture with controlled external communication would preserve the coordination benefit while eliminating most of the extraction cost. The persistence of maximum suppression reveals that leadership is extracting beyond what coordination justifies. The piton classification of formal ethics mechanisms shows how organizations deploy theater to maintain the extraction while appearing to address it — they create the appearance of accountability without the substance. This is the classic mandatrophy move: label extraction as coordination while using formal mechanisms to prevent genuine accountability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_vs_material_trap,
    'For any given trapped employee, is the binding mechanism identity lock (cognitive/internal) or material dependency (structural/external), or both?',
    'Post-exit trajectory analysis: if suppression persists after the employee leaves the organization, identity lock was primary. If suppression drops immediately, material dependency was primary. Survey of ex-employees on barriers that prevented earlier exit.',
    'If identity-locked agents predominate: the constraint is self-perpetuating through cognitive capture and will persist even as material incentives weaken. If materially trapped agents predominate: organizational restructuring (separation of reputation management from truth-seeking) could break the constraint. If both: the constraint is highly stable and requires both cognitive frame-breaking and material restructuring.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_material_trap, empirical, 'Primary mechanism of employee entrapment: identity lock vs material dependency').

omega_variable(
    coordination_function_magnitude,
    'How much of the organizational stability attributed to unified messaging actually derives from omerta, versus from other structural factors (competent operations, market position, leadership quality)?',
    'Natural experiment: organizations that dissolve omerta (post-scandal transparency, leadership change to transparency norms) and measure subsequent operational stability, stakeholder confidence, employee retention. Counterfactual: what organizational failures would have surfaced earlier under transparency?',
    'If coordination function is substantial (>0.40): classification shifts from Snare toward Tangled Rope; sunset logic becomes viable (transparency can be adopted if coupled with crisis management infrastructure). If coordination function is minimal (<0.15): omerta is pure extraction theater; classification remains Snare; sunset requires organizational dissolution or leadership replacement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_magnitude, empirical, 'Magnitude of genuine coordination function provided by organizational omerta').

omega_variable(
    intergenerational_perpetuation_mechanism,
    'How does each cohort of insiders enforce omerta on the next cohort? Is it through active indoctrination, selective hiring for cultural fit, or emergence from the trapped position itself?',
    'Organizational ethnography: interview multiple generations of employees about when they learned the code, who taught it, what happened to those who violated it. Track which cohort members became enforcement agents and what their stated rationale was.',
    'If active indoctrination: omerta is deliberately perpetuated and can be disrupted by leadership change. If cultural fit selection: the constraint self-selects and is harder to disrupt (would require hiring from outside the cultural type). If emergence from trapped position: the constraint is self-perpetuating through the cognitive and material lock that creates future enforcers from current victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intergenerational_perpetuation_mechanism, empirical, 'How organizational omerta perpetuates across employee cohorts').

omega_variable(
    extractiveness_scope_variability,
    'Does the extractiveness (and suppression) of omerta vary significantly across organizational levels, departments, or geographic regions, or is it uniformly high?',
    'Comparative analysis of incident reporting rates, whistleblower outcomes, and employee exit rates across organizational strata. Qualitative interviews on how the code is enforced in different contexts (C-suite vs frontline, technical teams vs sales, headquarters vs field operations).',
    'If uniformly high: the constraint is a monolithic institutional property. If variable: the constraint may be decomposable into separate stories per organizational level, each with different extractiveness values. Leadership teams might experience it as low-extraction rope while frontline workers experience snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extractiveness_scope_variability, empirical, 'Variation in omerta extractiveness across organizational levels').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(organizational_omerta, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(omerta_tr_t0, organizational_omerta, theater_ratio, 0, 0.38).
narrative_ontology:measurement(omerta_tr_t5, organizational_omerta, theater_ratio, 5, 0.52).
narrative_ontology:measurement(omerta_tr_t10, organizational_omerta, theater_ratio, 10, 0.65).
narrative_ontology:measurement(omerta_tr_t15, organizational_omerta, theater_ratio, 15, 0.7).

% Extraction over time
narrative_ontology:measurement(omerta_be_t0, organizational_omerta, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(omerta_be_t5, organizational_omerta, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(omerta_be_t10, organizational_omerta, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(omerta_be_t15, organizational_omerta, base_extractiveness, 15, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(organizational_omerta, enforcement_mechanism).
narrative_ontology:affects_constraint(organizational_omerta, psychological_safety_erosion).
narrative_ontology:affects_constraint(organizational_omerta, whistleblower_retaliation_risk).
narrative_ontology:affects_constraint(organizational_omerta, organizational_scandal_escalation).

% DUAL FORMULATION NOTE:
% Organizational omerta is the upstream constraint that enables and perpetuates downstream constraints (whistleblower retaliation, psychological safety erosion, scandal escalation). The code of silence is the structural glue that binds these phenomena together. Decomposition by organizational level or by type of wrongdoing (financial vs ethical vs safety violations) would produce separate stories with different extractiveness values, but the omerta constraint itself operates uniformly as the suppression mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(organizational_omerta, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
