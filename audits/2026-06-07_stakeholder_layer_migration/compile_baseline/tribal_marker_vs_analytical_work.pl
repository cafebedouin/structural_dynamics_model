% ============================================================================
% CONSTRAINT STORY: tribal_marker_vs_analytical_work
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tribal_marker_vs_analytical_work, []).

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
 *   constraint_id: tribal_marker_vs_analytical_work
 *   human_readable: Tribal Marker vs Analytical Work in Ideological Discourse
 *   domain: political_philosophy/rhetorical_analysis/ideological_discourse
 *
 * SUMMARY:
 *   The use of ideological terms like 'socialism,' 'freedom,' 'tyranny,'
 *   'justice' as tribal markers rather than operational descriptions creates
 *   a structural tension between coalition coordination and analytical
 *   clarity. This constraint exhibits the tangled rope pattern: it serves a
 *   genuine coordination function (rapid identification of coalition
 *   membership, efficient in-group communication, maintenance of coalition
 *   cohesion across diverse policy preferences) while simultaneously
 *   extracting from analytical clarity, policy deliberation quality, and
 *   cross-coalition dialogue. The constraint requires active enforcement
 *   through social penalty (accusations of ideological impurity, loss of
 *   coalition membership) and career risk (being labeled as 'not a real
 *   [socialist/libertarian/conservative]'). The theater_ratio (0.68) reflects
 *   that much discourse claiming to be about policy substance is actually
 *   performative tribal signaling — participants go through the motions of
 *   policy debate while the real function is coalition maintenance. The
 *   suppression trajectory shows enforcement intensification over the
 *   interval: as political polarization has increased, the social penalty for
 *   requesting operational definitions has risen, and the career risk of
 *   cross-coalition dialogue has grown. This is not a static constraint but
 *   one whose suppressive force has actively strengthened.
 *
 * KEY AGENTS:
 *   - Identity-Locked Participant: Primary victim (powerless/identity_locked) — individual whose political identity is constituted through marker usage; cannot exit without identity dissolution; bears maximum extraction
 *   - Cross-Coalition Dialogue Participant: Secondary victim (moderate/constrained) — faces career risk and social penalty but also benefits from coordination function; mixed experience
 *   - Coalition Leadership: Primary beneficiary (institutional/arbitrage) — captures coalition control benefits; can code-switch to operational language when negotiating with elites; net beneficiary
 *   - Media Amplifiers: Secondary beneficiary (institutional/arbitrage) — benefit from audience engagement driven by tribal marker usage; can exit to operational language in elite contexts
 *   - Deliberative Democracy Movement: Organized agents (organized/mobile) — building alternative discourse norms with sunset logic; sees constraint as temporary coordination failure
 *   - Academic Political Philosophy: Institutional actor (institutional/arbitrage) — maintains degraded public-facing function theatrically while core research program has abandoned engagement with operational definitions
 *   - Analytical Clarity: Abstract victim (powerless/trapped) — epistemic commons that cannot organize or exit; bears full cost of suppressed operational engagement
 *   - Policy Deliberation Quality: Abstract victim (powerless/trapped) — collective good that degrades under tribal marker dominance; no advocate and no exit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tribal_marker_vs_analytical_work, 0.48).
domain_priors:suppression_score(tribal_marker_vs_analytical_work, 0.62).
domain_priors:theater_ratio(tribal_marker_vs_analytical_work, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tribal_marker_vs_analytical_work, extractiveness, 0.48).
narrative_ontology:constraint_metric(tribal_marker_vs_analytical_work, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(tribal_marker_vs_analytical_work, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tribal_marker_vs_analytical_work, tangled_rope).
narrative_ontology:human_readable(tribal_marker_vs_analytical_work, "Tribal Marker vs Analytical Work in Ideological Discourse").
narrative_ontology:topic_domain(tribal_marker_vs_analytical_work, "political_philosophy/rhetorical_analysis/ideological_discourse").

domain_priors:requires_active_enforcement(tribal_marker_vs_analytical_work).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tribal_marker_vs_analytical_work, in_group_members).
narrative_ontology:constraint_beneficiary(tribal_marker_vs_analytical_work, ideological_coalition_leaders).
narrative_ontology:constraint_beneficiary(tribal_marker_vs_analytical_work, media_amplifiers).
narrative_ontology:constraint_victim(tribal_marker_vs_analytical_work, analytical_clarity).
narrative_ontology:constraint_victim(tribal_marker_vs_analytical_work, policy_deliberation_quality).
narrative_ontology:constraint_victim(tribal_marker_vs_analytical_work, cross_coalition_dialogue_participants).
narrative_ontology:constraint_vindicates(tribal_marker_vs_analytical_work, group_identity_primacy_over_truth_seeking).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IDENTITY-LOCKED PARTICIPANT (SNARE) — Individual whose political identity is constituted through tribal marker usage. Cannot exit because abandoning the markers would require abandoning the identity frame that makes their political participation legible to themselves. Structurally mobile (could use operational definitions) but functionally trapped by identity fusion with the coalition. Maximum experienced extraction — the constraint extracts cognitive labor (maintaining marker fluency) and forecloses analytical engagement without providing coordination benefit to this agent.
constraint_indexing:constraint_classification(tribal_marker_vs_analytical_work, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: CROSS-COALITION DIALOGUE PARTICIPANT (TANGLED ROPE) — Moderate-power agent attempting policy deliberation across ideological lines. Constrained by career risk (being labeled as ideologically impure by either coalition) and social penalty (loss of coalition membership). Benefits from the coordination function when markers successfully identify shared priors within a coalition, enabling efficient communication. Bears extraction cost when markers block engagement with operational definitions and concrete policy mechanisms. Mixed experience — genuine coordination value alongside substantial extraction.
constraint_indexing:constraint_classification(tribal_marker_vs_analytical_work, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: COALITION LEADERSHIP (ROPE) — Institutional actors (party leadership, ideological media, think tank directors) who benefit from marker-based coalition maintenance. Arbitrage-level exit — can code-switch to operational language when negotiating with elites while maintaining marker usage for mass communication. Experiences the constraint as pure coordination: tribal markers solve the genuine problem of maintaining coalition cohesion across diverse policy preferences. Net beneficiary — extraction flows toward this agent through enhanced coalition control and reduced need for policy specificity.
constraint_indexing:constraint_classification(tribal_marker_vs_analytical_work, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DELIBERATIVE DEMOCRACY MOVEMENT (SCAFFOLD) — Organized agents (citizens' assemblies, sortition advocates, deliberative polling practitioners) building alternative discourse norms with explicit sunset logic. See tribal marker dominance as a temporary coordination failure that deliberative institutions can bypass. Mobile exit — can operate in parallel discourse spaces with different norms. Low effective extraction because the movement has agency and sees a concrete path to norm change through institutional design.
constraint_indexing:constraint_classification(tribal_marker_vs_analytical_work, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ACADEMIC POLITICAL PHILOSOPHY (PITON) — The academic discipline's claim to adjudicate ideological disputes through conceptual analysis has atrophied. Journals publish increasingly technical work disconnected from public discourse; the discipline's public-facing function (clarifying contested political concepts) is maintained theatrically through op-eds and public intellectuals while the core research program has abandoned engagement with operational definitions of terms like 'socialism,' 'freedom,' 'justice' as they appear in mass discourse. The ritual persists through institutional inertia. Piton classification derives from theater gate, not from high experienced extraction.
constraint_indexing:constraint_classification(tribal_marker_vs_analytical_work, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational analytical perspective, tribal marker usage serves a genuine coordination function (rapid coalition identification, efficient in-group communication) while simultaneously extracting from analytical clarity and policy deliberation quality. The constraint is not reducible to either pure coordination or pure extraction — it is structurally hybrid. The analytical observer sees both the coordination value (markers do solve real collective action problems in mass politics) and the extraction mechanism (markers systematically suppress operational engagement and concrete policy analysis).
constraint_indexing:constraint_classification(tribal_marker_vs_analytical_work, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tribal_marker_vs_analytical_work_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(tribal_marker_vs_analytical_work, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(tribal_marker_vs_analytical_work, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(tribal_marker_vs_analytical_work, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(tribal_marker_vs_analytical_work, TR),
    TR >= 0.70.

:- end_tests(tribal_marker_vs_analytical_work_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. Coalition leadership captures substantial benefits (enhanced control, reduced need for policy specificity, audience engagement) while identity-locked participants and analytical clarity bear costs (cognitive labor maintaining marker fluency, foreclosed operational engagement, degraded deliberation quality). The extraction is not maximal because the constraint does provide genuine coordination value to some agents. Suppression (0.62): Moderate-high and rising. Significant barriers to operational engagement include social penalty (accusations of ideological impurity), career risk (loss of coalition membership or professional standing), and identity lock (for participants whose political identity is constituted through marker usage). The suppression trajectory shows enforcement intensification: as polarization has increased, the penalty for requesting operational definitions has grown. Theater ratio (0.68): High. Much discourse claiming to be about policy substance is performative tribal signaling. Participants go through the motions of policy debate (citing studies, invoking principles, making arguments) while the real function is coalition maintenance and in-group status signaling. The theater has increased over the interval as media incentives have shifted toward engagement-maximizing content and away from policy substance.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the tangled rope pattern from multiple perspectives. Coalition leadership sees pure coordination (Rope) — tribal markers solve the genuine problem of maintaining coalition cohesion. Identity-locked participants see pure extraction (Snare) — the constraint extracts cognitive labor and forecloses analytical engagement without providing them coordination benefit. Cross-coalition dialogue participants see the hybrid (Tangled Rope) — genuine coordination value alongside substantial extraction. The deliberative democracy movement sees a temporary problem with a sunset (Scaffold) — alternative institutions can bypass marker dominance. Academic political philosophy sees its own degraded ritual (Piton) — the discipline's public-facing function persists theatrically while the core research program has abandoned engagement. The analytical observer sees the structural hybrid (Tangled Rope) — irreducible coordination function coexisting with irreducible extraction mechanism. The perspectival gap is not a measurement error but the constraint's actual structure: it genuinely coordinates for some agents while genuinely extracting from others, and the same agent can experience both simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value is determined by the agent's structural position. Coalition leadership (institutional/arbitrage) are primary beneficiaries — they capture coalition control benefits and can code-switch to operational language when needed. The engine derives low d (beneficiary end) from their beneficiary status and arbitrage exit, producing low or negative effective extraction. Identity-locked participants (powerless/identity_locked) are primary victims — they bear cognitive labor costs and experience foreclosed analytical engagement. The engine derives high d (target end) from their victim status and identity-locked exit, producing high effective extraction. The identity lock is critical here: these agents are structurally mobile (could use operational definitions) but functionally trapped because their identity frame makes exit unthinkable. Cross-coalition dialogue participants (moderate/constrained) experience mixed directionality — they are listed as victims (bear extraction from degraded deliberation quality) but also benefit from the coordination function when markers successfully identify shared priors. The engine derives moderate d from their victim status modulated by constrained exit and partial beneficiary status. The analytical observer (analytical/analytical) sees the structural hybrid without experiencing extraction directly — analytical context produces low effective extraction regardless of base extractiveness.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that tangled rope is not a transitional state between rope and snare but a stable structural hybrid. The coordination function (rapid coalition identification, efficient in-group communication) is genuine and irreducible — it cannot be dismissed as mere cover story. The extraction mechanism (suppressed operational engagement, degraded deliberation quality, identity lock for some participants) is also genuine and irreducible — it cannot be dismissed as mere coordination cost. The constraint is BOTH coordinating AND extracting, and this duality is its stable state. The mandate (maintain coalition cohesion in mass politics) has not outlived its function — the function persists. But the function's execution necessarily extracts from analytical clarity and policy deliberation quality. This is the tangled rope signature: a constraint that would be classified as rope from the beneficiary's perspective and snare from the victim's perspective, with the analytical observer seeing both functions as structurally real and neither reducible to the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operational_definition_threshold,
    'What proportion of tribal marker usage would survive a norm requiring operational definitions before the coordination function collapses?',
    'Experimental deliberative forums with enforced operational definition norms; measurement of coalition cohesion and policy convergence rates under different discourse rules',
    'If >70% survives: markers are mostly coordinating around genuine policy clusters (Rope from more perspectives). If <30% survives: markers are mostly tribal signaling with minimal policy content (Snare from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_definition_threshold, empirical, 'Proportion of marker usage that survives operational definition requirement').

omega_variable(
    identity_lock_reversibility,
    'Can identity-locked participants be de-fused from tribal markers through deliberative exposure, or is the identity lock permanent within biographical timescales?',
    'Longitudinal studies of participants in sustained deliberative forums; tracking of discourse norm adoption and identity frame flexibility over 5-10 year periods',
    'If reversible: identity_locked should be reclassified as constrained (high-cost but surmountable exit). If permanent: identity_locked classification confirmed and the biographical-timescale mountain perception is structural.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_reversibility, empirical, 'Whether identity fusion with tribal markers is reversible at biographical timescales').

omega_variable(
    elite_vs_mass_discourse_gap,
    'Do coalition elites genuinely use operational definitions in private negotiation, or is the perceived elite/mass discourse gap itself a tribal marker (elites signaling sophistication)?',
    'Analysis of leaked private communications, closed-door negotiation transcripts, and elite policy working group documents; comparison of operational specificity in private vs public elite discourse',
    'If elites use operational definitions privately: the arbitrage exit is real and the institutional perspective''s Rope classification is structurally accurate. If elites use tribal markers privately: the arbitrage is performative and institutional actors are also identity-locked (reclassify institutional perspective toward Tangled Rope or Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_vs_mass_discourse_gap, empirical, 'Whether elite discourse actually uses operational definitions or merely performs sophistication').

omega_variable(
    deliberative_institution_scaling,
    'Can deliberative democracy institutions scale to mass politics while maintaining operational definition norms, or do they only function in small-group settings?',
    'Comparative analysis of deliberative institution outcomes at different scales (n=20 vs n=200 vs n=2000); measurement of discourse norm degradation as group size increases',
    'If scalable: Scaffold perspective confirmed — deliberative institutions provide a real sunset path. If non-scalable: Scaffold is aspirational rather than structural, and the constraint may be closer to Mountain (inherent to mass politics) than currently classified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deliberative_institution_scaling, empirical, 'Whether deliberative norms can scale to mass political discourse').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tribal_marker_vs_analytical_work, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tribal_marker_theater_t0, tribal_marker_vs_analytical_work, theater_ratio, 0, 0.52).
narrative_ontology:measurement(tribal_marker_theater_t3, tribal_marker_vs_analytical_work, theater_ratio, 3, 0.58).
narrative_ontology:measurement(tribal_marker_theater_t6, tribal_marker_vs_analytical_work, theater_ratio, 6, 0.63).
narrative_ontology:measurement(tribal_marker_theater_t10, tribal_marker_vs_analytical_work, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(tribal_marker_extract_t0, tribal_marker_vs_analytical_work, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(tribal_marker_extract_t3, tribal_marker_vs_analytical_work, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(tribal_marker_extract_t6, tribal_marker_vs_analytical_work, base_extractiveness, 6, 0.45).
narrative_ontology:measurement(tribal_marker_extract_t10, tribal_marker_vs_analytical_work, base_extractiveness, 10, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(tribal_marker_suppress_t0, tribal_marker_vs_analytical_work, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(tribal_marker_suppress_t3, tribal_marker_vs_analytical_work, suppression_requirement, 3, 0.54).
narrative_ontology:measurement(tribal_marker_suppress_t6, tribal_marker_vs_analytical_work, suppression_requirement, 6, 0.58).
narrative_ontology:measurement(tribal_marker_suppress_t10, tribal_marker_vs_analytical_work, suppression_requirement, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tribal_marker_vs_analytical_work, identity_coordination).
narrative_ontology:affects_constraint(tribal_marker_vs_analytical_work, policy_specificity_avoidance).
narrative_ontology:affects_constraint(tribal_marker_vs_analytical_work, coalition_maintenance_vs_policy_coherence).
narrative_ontology:affects_constraint(tribal_marker_vs_analytical_work, media_engagement_optimization).

% DUAL FORMULATION NOTE:
% Tribal marker usage is upstream of several related constraints in political discourse. Policy specificity avoidance (politicians using vague language to maintain coalition breadth) is partly enabled by tribal marker dominance — if the discourse norm required operational definitions, policy vagueness would be more costly. Coalition maintenance vs policy coherence (the tension between keeping diverse factions unified and maintaining consistent policy positions) is also downstream — tribal markers allow coalition maintenance without policy coherence by substituting identity signaling for policy agreement. Media engagement optimization (media incentives favoring polarizing content over policy substance) both affects and is affected by tribal marker usage in a feedback loop.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
