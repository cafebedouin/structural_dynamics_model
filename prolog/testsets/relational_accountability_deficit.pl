% ============================================================================
% CONSTRAINT STORY: relational_accountability_deficit
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_relational_accountability_deficit, []).

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
 *   constraint_id: relational_accountability_deficit
 *   human_readable: Relational Accountability Deficit in Interpersonal and Institutional Power Dynamics
 *   domain: social/organizational/interpersonal
 *
 * SUMMARY:
 *   The relational accountability deficit is a structural constraint where
 *   power holders are insulated from consequences for harm to dependent
 *   parties, and the mechanisms ostensibly designed to enforce accountability
 *   become performative rather than functional. This constraint spans
 *   interpersonal relationships (mentorship dynamics, therapeutic
 *   relationships, abusive partnerships), organizational hierarchies
 *   (management-employee, institutional leadership), and state-citizen
 *   relationships. The deficit emerges when four conditions converge: (1)
 *   asymmetric power and dependency; (2) suppression mechanisms that prevent
 *   escalation (retaliation risk, isolation, gaslighting, institutional
 *   reputation logic); (3) identity fusion that prevents the dependent party
 *   from recognizing alternatives; and (4) capture or degradation of
 *   accountability apparatus. The constraint exhibits all six DR types from
 *   different perspectives, illustrating how the 'same' structural phenomenon
 *   — the absence of meaningful consequences for the powerful — appears as
 *   law, coordination, hybrid, temporary, degraded, or pure extraction
 *   depending on observer position.
 *
 * KEY AGENTS:
 *   - Dependent Party: Primary victim (powerless/trapped) — bears cost of accountability absence; cannot exit without severe personal/economic/relational cost
 *   - Power Holder: Primary beneficiary (institutional/arbitrage) — captures freedom from accountability; can escalate, hide, reframe without consequence
 *   - Institutionally Constrained Actor: Secondary victim (moderate/identity_locked) — identity fused with relationship; sees genuine coordination function alongside suppression norms
 *   - Accountability Movement: Organized agents (organized/constrained) — legal reform advocates, institutional auditing bodies, transparency movements building alternative mechanisms
 *   - Formal Accountability Apparatus: Institutional actor (institutional/arbitrage) — HR, ethics committees, compliance offices; sustains legitimacy through procedural performance while preserving power holder discretion
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent power asymmetry as inevitable feature of relationality
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(relational_accountability_deficit, 0.58).
domain_priors:suppression_score(relational_accountability_deficit, 0.65).
domain_priors:theater_ratio(relational_accountability_deficit, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(relational_accountability_deficit, extractiveness, 0.58).
narrative_ontology:constraint_metric(relational_accountability_deficit, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(relational_accountability_deficit, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(relational_accountability_deficit, tangled_rope).
narrative_ontology:human_readable(relational_accountability_deficit, "Relational Accountability Deficit in Interpersonal and Institutional Power Dynamics").
narrative_ontology:topic_domain(relational_accountability_deficit, "social/organizational/interpersonal").

domain_priors:requires_active_enforcement(relational_accountability_deficit).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(relational_accountability_deficit, power_holder).
narrative_ontology:constraint_beneficiary(relational_accountability_deficit, institutional_authority).
narrative_ontology:constraint_victim(relational_accountability_deficit, dependent_party).
narrative_ontology:constraint_victim(relational_accountability_deficit, institutional_accountability_mechanisms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT PARTY (SNARE) — Trapped by economic, legal, or relational dependency. Bears full cost of accountability absence. No credible exit option; cannot escalate without severe personal cost. Suppression is structural: isolation, information control, retaliation for complaint.
constraint_indexing:constraint_classification(relational_accountability_deficit, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: INSTITUTIONALLY CONSTRAINED ACTOR (TANGLED ROPE) — Identity-locked to the institution or role (professional identity, institutional loyalty, organizational culture internalization). Structurally mobile but cannot exercise exit because their identity is constituted through the institutional relationship. Experiences genuine coordination function (institutional legitimacy, role clarity) alongside extraction (tacit enforcement, silence norms).
constraint_indexing:constraint_classification(relational_accountability_deficit, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 3: POWER HOLDER (ROPE) — Experiences accountability deficit as pure coordination benefit. Low friction, no consequences, ability to govern without external check. Arbitrage options: can relocate, hide, reframe, or dissolve the relationship. Net beneficiary — constraint runs toward this agent.
constraint_indexing:constraint_classification(relational_accountability_deficit, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ACCOUNTABILITY MOVEMENT (SCAFFOLD) — Organized agents (legal reform, institutional auditing, transparency advocates) see the deficit as a temporary coordination failure with a sunset. Structured accountability mechanisms (reporting systems, external oversight, third-party mediation) are building alternative pathways. Chi is low because organized actors have agency and see an exit trajectory.
constraint_indexing:constraint_classification(relational_accountability_deficit, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: FORMAL ACCOUNTABILITY APPARATUS (PITON) — Institutional mechanisms (HR departments, ethics committees, compliance offices, internal review boards) are largely performative. Theater ratio high: produce reports, investigate, recommend — but structural changes are rare and enforcement selective. The apparatus maintains legitimacy through ritual while preserving power holder's discretion. Piton classification: degraded function persisting through institutional inertia.
constraint_indexing:constraint_classification(relational_accountability_deficit, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some power asymmetry is inherent to all human relationships and institutions; perfect accountability is impossible because verification costs are infinite and observer-dependent. This perspective risks naturalizing a contingent institutional arrangement as a law of social dynamics. Engine will detect this as a false summit: the 'inevitable' framing conceals active enforcement of silence norms.
constraint_indexing:constraint_classification(relational_accountability_deficit, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(relational_accountability_deficit_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(relational_accountability_deficit, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(relational_accountability_deficit, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(relational_accountability_deficit, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(relational_accountability_deficit, TR),
    TR >= 0.70.

:- end_tests(relational_accountability_deficit_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The power holder gains freedom from external check, ability to govern without constraint, and immunity from retaliation. But the extraction is not maximal because some dependent parties do exercise limited voice (complaints within channels, refusal of compliance), and accountability mechanisms, though degraded, occasionally produce consequences. The rising trajectory from 0.42 to 0.63 reflects that over time, as patterns persist without correction, extraction accumulates: the initial harm is compounded by institutional response failures, and dependent parties internalize their powerlessness. Suppression (0.65): High. Multiple suppression mechanisms operate: fear of retaliation, economic dependency, identity fusion, isolation from alternative perspectives, institutional loyalty demands, reputation costs for complaint, gaslighting about the relationship quality. These are sufficiently reinforced that most dependent parties do not attempt exit. Theater ratio (0.68): High and rising. The formal accountability apparatus produces extensive performative activity — investigations, reports, training sessions, policy revisions — but outcomes follow patterns that structurally benefit the power holder. The rising trajectory from 0.35 to 0.72 reflects that as institutional accountability becomes more visible (scandals, external pressure), the theater intensifies while functional accountability remains low. This is Goodhart drift: the performance of accountability substitutes for actual accountability.
 *
 * PERSPECTIVAL GAP:
 *   The dependent party sees snare — pure extraction with no escape. The power holder sees rope — coordination that enables both parties' function (their framing). The identity_locked actor sees tangled rope — genuine coordination (institutional legitimacy, role clarity) intertwined with suppression. The accountability movement sees scaffold with sunset — temporary deficit being solved by structural reforms. The formal apparatus sees piton — degraded verification ritual persisting through institutional inertia. The analytical observer risks seeing mountain — inevitable power asymmetry in all relationships. The perspectival gaps are maximal: snare vs rope is a complete disagreement about whether the constraint benefits both parties or extracts from one. This gap is the diagnostic signature: when one perspective sees snare and another sees rope on identical structural data, the constraint is operating through asymmetric information and identity capture.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's effective extractiveness (chi) derives from base extraction (0.58) scaled by directionality f(d) and scope sigma. The power holder has low d (beneficiary + arbitrage exit) producing negative or minimal chi — they perceive the constraint as enabling coordination with negligible personal extraction. The dependent party has high d (victim + trapped exit) producing maximum chi — they experience the constraint as pure extraction. The identity_locked actor has high d modulated by their constrained-to-mobile range due to their conflicted exit capacity; the engine interprets their identity lock as preventing them from perceiving their own mobility. The organized accountability movement has moderate d (constrained exit + victim-to-beneficiary mixed status) producing moderate chi. The formal apparatus has very low d (beneficiary + arbitrage) producing negative or minimal chi — they experience their role as enabling institutional function. The analytical observer has canonical d for analytical power (0.73), producing moderate chi, but risks misclassifying the pattern as immutable.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint resolves the mandatrophy by demonstrating that relational accountability deficit combines genuine coordination function (the power holder needs the relationship to have legitimacy; the dependent party needs institutional role clarity) with asymmetric extraction (the power holder can harm without consequence; the dependent party cannot). The tangled rope classification is mandatrophy-resistant because it holds both: (1) the coordination truth from the power holder's perspective — they do coordinate the relationship and derive legitimate benefit from its structure; (2) the extraction truth from the dependent party's perspective — the asymmetry permits extraction with impunity. The constraint cannot be resolved as pure coordination (rope) because the asymmetric suppression breaks the coordination assumption — if both parties equally needed the constraint, neither would need suppression. It cannot be resolved as pure extraction (snare) because the institutional framework genuinely does provide coordination benefits (role stability, legitimacy structure, conflict resolution pathways, however degraded). The mandatrophy resolution confirms that relational accountability deficit is fundamentally a tangled rope: it solves a coordination problem while enabling extraction from dependent parties.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_vs_constrained_boundary,
    'Is the dependent party''s binding mechanism identity-locked (cognitive/internalized) or constrained (material/external barriers)?',
    'Post-exit trajectory analysis: If the dependent party carries the suppression with them after exit (internalized norms, shame, identity dissonance), mechanism is identity-locked. If suppression ends with barrier removal, mechanism is constrained.',
    'If identity-locked: reclassify dependent party as identity_locked exit. Constraint is more deeply binding than material barriers suggest. If constrained: dependent party perspective is trapped or constrained depending on barrier severity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_constrained_boundary, empirical, 'Whether suppression is identity-locked or materially constrained').

omega_variable(
    institutional_capture_of_accountability,
    'Has the accountability apparatus been captured by the power holder, or is it genuinely independent but ineffective?',
    'Institutional analysis: Do accountability mechanisms consistently fail in systematically favorable ways to the power holder? Do leadership changes affect investigation outcomes? Is there evidence of selective enforcement?',
    'If captured: apparatus is a snare mechanism (enforces silence through appearance of investigation). If independent but ineffective: apparatus is piton (degraded function through resource constraints or complexity).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_capture_of_accountability, empirical, 'Whether accountability apparatus is captured or merely degraded').

omega_variable(
    relational_identity_fusion_depth,
    'What proportion of the constrained actor''s identity is constituted through the institutional relationship versus external identity anchors?',
    'Identity decomposition: Measure proportion of self-concept that depends on the institutional role. Are there alternative identity frameworks available to the actor? What would exit require in terms of identity reconstruction?',
    'If high fusion (>70%): identity_locked classification is robust. If moderate fusion (30-70%): classification depends on time horizon — piton at biographical, rope at generational. If low fusion (<30%): actor should be classified as constrained rather than identity_locked.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(relational_identity_fusion_depth, conceptual, 'Depth of identity fusion with institutional relationship').

omega_variable(
    suppression_persistence_post_visibility,
    'If the accountability deficit becomes publicly visible (scandal, leak, investigation), does the suppression mechanism weaken or persist through reframing?',
    'Longitudinal case analysis: Track suppression intensity before/after public exposure. Does visibility create actual accountability, or does reframing (denials, alternative narratives, procedural distraction) maintain suppression?',
    'If visibility weakens suppression: constraint is primarily structural, sustains through information asymmetry. If suppression persists: constraint has internalized components that survive information access.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_persistence_post_visibility, empirical, 'Whether suppression persists after public visibility').

omega_variable(
    scalar_independence_of_accountability_deficit,
    'Does relational accountability deficit occur at all scales (dyadic, organizational, state), or does it have scale-dependent properties?',
    'Comparative analysis across scales: dyadic relationships (mentor/mentee, manager/report, therapist/client), organizational (company/employee, institution/member), state (citizen/state, nation/nation). Do extraction mechanisms transfer or transform?',
    'If scale-invariant: single constraint story applies across all contexts. If scale-dependent: decompose into separate stories per scale. Current story targets relational/organizational scale.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scalar_independence_of_accountability_deficit, conceptual, 'Scale-dependence of accountability deficit structure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(relational_accountability_deficit, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(erad_tr_t0, relational_accountability_deficit, theater_ratio, 0, 0.35).
narrative_ontology:measurement(erad_tr_t5, relational_accountability_deficit, theater_ratio, 5, 0.52).
narrative_ontology:measurement(erad_tr_t10, relational_accountability_deficit, theater_ratio, 10, 0.68).
narrative_ontology:measurement(erad_tr_t15, relational_accountability_deficit, theater_ratio, 15, 0.72).

% Extraction over time
narrative_ontology:measurement(erad_be_t0, relational_accountability_deficit, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(erad_be_t5, relational_accountability_deficit, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(erad_be_t10, relational_accountability_deficit, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(erad_be_t15, relational_accountability_deficit, base_extractiveness, 15, 0.63).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(relational_accountability_deficit, attachment_coordination).
narrative_ontology:affects_constraint(relational_accountability_deficit, institutional_authority_capture).
narrative_ontology:affects_constraint(relational_accountability_deficit, internalized_suppression_narrative).
narrative_ontology:affects_constraint(relational_accountability_deficit, retaliation_threat_mechanism).

% DUAL FORMULATION NOTE:
% Relational accountability deficit is downstream of specific power asymmetries (economic dependency, legal authority, identity fusion) but represents a distinct structural constraint operating across multiple relationship types. Decomposition into three coupled stories: (1) institutional_authority_capture — the power holder's structural insulation from external checks; (2) internalized_suppression_narrative — the dependent party's identity-locked acceptance of the relationship; (3) retaliation_threat_mechanism — the suppression engine that prevents escalation. Current story treats the constraint holistically; linked stories model specific mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(relational_accountability_deficit, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
