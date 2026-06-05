% ============================================================================
% CONSTRAINT STORY: iran_hijab_law
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_iran_hijab_law, []).

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
 *   constraint_id: iran_hijab_law
 *   human_readable: Mandatory Hijab Law in Iran
 *   domain: political/social/religious
 *
 * SUMMARY:
 *   The mandatory hijab law in the Islamic Republic of Iran, enforced by the
 *   state's morality police (Gasht-e Ershad), represents a high-extraction
 *   constraint combining religious doctrine with coercive state apparatus.
 *   Post-1979 revolutionary consolidation institutionalized hijab enforcement
 *   as both a symbol of Islamic governance and a mechanism for behavioral
 *   control. The constraint extracts compliance from women, religious
 *   minorities, and secular citizens through daily enforcement (checkpoints,
 *   arrests, fines) while positioning itself as coordinating religious/moral
 *   community standards. The state religious authority frames this as a
 *   coordination mechanism (Rope from their perspective), but the structural
 *   asymmetry — enforcement apparatus with monopoly on legitimate violence,
 *   trapped victims with no exit, degraded theater of compliance — reveals
 *   the constraint as a multi-layered Snare from those targeted. The theater
 *   ratio (0.65) reflects that visible enforcement is increasingly
 *   performative: women employ tactical noncompliance (styled headscarves,
 *   gradual loosening), enforcement officers face declining morale and
 *   corruption, and social norms have shifted substantially since 1979. The
 *   July 2024 suspension of morality police operations represents a critical
 *   inflection point; if permanent, the constraint would transition toward
 *   Scaffold with sunset. The women's rights movement experiences the
 *   constraint as both oppressive and organizing (Tangled Rope perspective) —
 *   state enforcement creates shared grievance that fuels activism.
 *
 * KEY AGENTS:
 *   - Iranian Women: Primary victim (powerless/trapped) — subjected to daily enforcement, no practical exit, bear full cost of non-compliance through arrest, fines, social stigma, family consequences
 *   - Religious Minorities and Secular Citizens: Secondary victim (powerless/constrained) — enforcement imposes state Islamic orthodoxy on non-Muslim populations and secular belief systems
 *   - State Religious Authority: Primary beneficiary (institutional/arbitrage) — maintains control over social behavior and moral regulation; consolidates religious governance legitimacy
 *   - Morality Police Apparatus (Gasht-e Ershad): Enforcement institution (institutional/constrained) — maintains coercive apparatus; experiences degrading functional capacity
 *   - Women's Rights Movement and Civil Society: Organized opposition (organized/mobile) — international networks, diaspora activism, underground resistance; experiences mixed constraint as oppression + organizing opportunity
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing post-1979 institutional arrangements as inherent Islamic governance or unchangeable cultural constants
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(iran_hijab_law, 0.68).
domain_priors:suppression_score(iran_hijab_law, 0.78).
domain_priors:theater_ratio(iran_hijab_law, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(iran_hijab_law, extractiveness, 0.68).
narrative_ontology:constraint_metric(iran_hijab_law, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(iran_hijab_law, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(iran_hijab_law, snare).
narrative_ontology:human_readable(iran_hijab_law, "Mandatory Hijab Law in Iran").
narrative_ontology:topic_domain(iran_hijab_law, "political/social/religious").

domain_priors:requires_active_enforcement(iran_hijab_law).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(iran_hijab_law, state_religious_authority).
narrative_ontology:constraint_beneficiary(iran_hijab_law, morality_enforcement_apparatus).
narrative_ontology:constraint_victim(iran_hijab_law, iranian_women).
narrative_ontology:constraint_victim(iran_hijab_law, religious_minorities).
narrative_ontology:constraint_victim(iran_hijab_law, secular_citizens).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IRAINIAN WOMEN (SNARE) — Trapped within national borders with no practical exit from the constraint. Daily enforcement via morality police creates coercive compliance. Exit costs include social stigma, arrest, prosecution, and family consequences. d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.95. Pure extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(iran_hijab_law, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RELIGIOUS MINORITIES & SECULAR CITIZENS (SNARE) — Constrained by state religious doctrine imposed on non-Muslim populations and secular citizens. Enforcement targets not just dress but religious orthodoxy. No exemption for non-Muslims or secular belief systems. d≈0.88, f(d)≈1.30, σ=1.0 → χ≈0.88. Effective extraction masks religious coercion.
constraint_indexing:constraint_classification(iran_hijab_law, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE RELIGIOUS AUTHORITY (ROPE) — Frames hijab law as coordination mechanism enforcing Islamic moral standards and social cohesion. Experiences the constraint as legitimate religious governance without acknowledging extraction. d≈0.10, f(d)≈0.05, σ=1.0 → χ≈0.03. Near-zero effective extraction from their framing; net beneficiary.
constraint_indexing:constraint_classification(iran_hijab_law, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: WOMEN'S RIGHTS MOVEMENT (TANGLED ROPE) — Organized agents with partial mobility (activism abroad, diaspora networks, international platforms) experience mixed constraint: the law both oppresses and creates organizing space. Coordination function: enforcement creates shared grievance; extraction function: state suppresses movement. d≈0.60, f(d)≈0.70, σ=1.0 → χ≈0.48. Active enforcement ≥0.46 satisfies tangled rope gate.
constraint_indexing:constraint_classification(iran_hijab_law, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: MORALITY POLICE APPARATUS (PITON) — Enforcement institution maintaining performative compliance. Theater ratio 0.65 reflects that visible enforcement (checkpoints, arrests, fines) is partly theatrical deterrent rather than functional regulation — many women employ tactical noncompliance (styled headscarves, gradual loosening). Enforcement has degraded over time as resistance techniques evolve. theater_ratio=0.65 approaches piton gate (≥0.70); combined with declining functional capacity, suggests institutional inertia.
constraint_indexing:constraint_classification(iran_hijab_law, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (FALSE SUMMIT) — From civilizational distance, observer might frame hijab law as inherent to Iranian sovereignty or unchangeable Islamic governance principle. However, structural data (ε=0.68, suppression=0.78, theater=0.65) contradicts mountain classification. This represents a false summit where contingent institutional arrangements (post-1979 revolutionary enforcement) are naturalized as civilizational constants. Engine false summit detector fires.
constraint_indexing:constraint_classification(iran_hijab_law, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(iran_hijab_law_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(iran_hijab_law, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(iran_hijab_law, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(iran_hijab_law, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(iran_hijab_law, TR),
    TR >= 0.70.

:- end_tests(iran_hijab_law_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High but not maximum. The constraint extracts substantial compliance costs (behavioral restriction, enforcement harassment, economic fines, social control) from targeted populations. However, extraction is not 0.85+ because degrading enforcement effectiveness and tactical noncompliance have created spaces of partial autonomy. The interval measurements show extractiveness rising from 0.45 (early post-1979 period) to 0.68 (current), reflecting that enforcement mechanisms intensified over time as initial revolutionary fervor normalized into institutional practice. Suppression (0.78): Very high. Coercive apparatus with monopoly on violence, criminalization of non-compliance, family/social pressure, arrest and prosecution mechanisms all severely constrain exit and voice options. Victims have minimal alternatives — non-wearing is criminalized, emigration is economically and socially costly, resistance is suppressed. Suppression exceeds extractiveness because the apparatus maintains control partly through threat alone. Theater ratio (0.65): Moderate-high. Visible enforcement (checkpoints, arrests) is increasingly performative as enforcement capacity degrades and tactical noncompliance becomes normalized. State rhetoric about moral community standards masks control function. The rising theater ratio (from 0.42 to 0.65) indicates that actual enforcement is declining while performative aspects increase — approaching piton thresholds but not yet there.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival gap across all six perspectives. The state religious authority (Rope) frames hijab enforcement as legitimate religious coordination, emphasizing community morality and Islamic governance. Women trapped in enforcement zones (Snare) experience it as coercive behavioral control with no exit. The women's rights movement (Tangled Rope) sees both oppression and organizing opportunity — the constraint creates shared grievance that mobilizes activism. The morality police apparatus (Piton) maintains enforcement through degraded practice and institutional inertia — the actual functional capacity to enforce is declining. Religious minorities (Snare) experience it as religious coercion, not moral coordination. The analytical observer (false summit) risks naturalizing the post-1979 institutional choice as an unchangeable aspect of Iranian sovereignty or Islamic governance when it is actually a contingent enforcement architecture that has weakened significantly. The widest gap separates the state authority's beneficiary perspective (Rope) from the victims' trapped experience (Snare) — the same enforcement apparatus is coordination for one and extraction for the other.
 *
 * DIRECTIONALITY LOGIC:
 *   State Religious Authority: Beneficiary + arbitrage → d≈0.12, f(d)≈0.08, σ=1.0 → χ≈0.05. Rope classification; net beneficiary experiencing constraint as low-cost coordination. Morality Police Apparatus: Institutional + constrained (by degrading legitimacy, officer morale, resource constraints) → d≈0.35, f(d)≈0.35, σ=1.0 → χ≈0.24. Piton classification reflects that enforcement institution maintains apparatus through inertia despite declining functional capacity. Iranian Women: Victim + trapped (no practical exit, domestic space monitored, social/family consequences) → d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.95. Snare classification; maximum extraction from the powerless. Religious Minorities: Victim + constrained (some diaspora exit options, international legal avenues, but costly and incomplete) → d≈0.88, f(d)≈1.30, σ=1.0 → χ≈0.88. Snare classification; high extraction with marginally more options than women. Women's Rights Movement: Organized + mobile (international networks, diaspora platforms, underground organizing) → d≈0.60, f(d)≈0.70, σ=1.0 → χ≈0.48. Tangled Rope; organized opposition creates coordination function while experiencing extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE WITH DEGRADING ENFORCEMENT: This constraint resolves mandatrophy by classifying primarily as Snare (high extraction, high suppression, χ=0.95 for powerless victims) while acknowledging the Piton perspective (enforcement apparatus maintaining degraded practice). The constraint is NOT mislabeled as pure coordination (Rope) — the state authority's beneficiary perspective does not define the constraint type for the engine; the asymmetric extraction from trapped victims does. Mandatrophy_resolved=true because (1) the snare classification is supported by ε=0.68 and suppression=0.78 meeting snare thresholds, (2) beneficiaries and victims are explicitly declared, (3) the architectural enforcement via Gasht-e Ershad is not a coordination solution but a coercive apparatus, and (4) the multiple perspectives (including state authority's Rope framing) reveal the mandatrophy risk (naturalizing extraction as coordination) but do not override the structural classification. The July 2024 suspension of morality police creates uncertainty about evolution toward Scaffold, but that remains an omega variable pending empirical resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_capacity_degradation,
    'How far has enforcement capacity degraded due to normalized resistance and changing social norms, and at what point does performative enforcement become functionally equivalent to optional compliance?',
    'Longitudinal data on arrest rates, fine collection, public compliance patterns, and tactical noncompliance prevalence over 10-year intervals; surveys of enforcement officer morale and actual checkpoint compliance rates',
    'If degradation near 50%: constraint shifts from Snare to Piton (institutional inertia). If degradation > 70%: constraint approaches Rope or Scaffold endpoint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_degradation, empirical, 'Extent of enforcement capacity degradation over time').

omega_variable(
    diaspora_exit_option_threshold,
    'At what proportion of women accessing exit via diaspora networks does the constraint shift from ''trapped'' to ''mobile'' for the broader population?',
    'Migration statistics, asylum claims on gender grounds, social network analysis of diaspora connections, cost-benefit analysis of exit routes (family separation, economic loss vs freedom)',
    'If diaspora exit reaches 5-10%: structural exit option shifts from trapped to constrained. If 20%+: exit shifts to mobile. Classification of powerless perspective changes from Snare to Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(diaspora_exit_option_threshold, empirical, 'Critical threshold for exit option availability via diaspora').

omega_variable(
    post_morality_police_expansion,
    'Following July 2024 suspension of morality police operations, is the constraint shifting from active enforcement (Snare) to theatrical maintenance (Piton) or dissolving toward Scaffold?',
    'Comparative analysis of enforcement patterns pre/post-suspension; tracking of hijab-related arrests, public compliance behavior, and state rhetoric; assessment of whether suspension is permanent or performative pause',
    'If suspension is permanent: constraint transforms to Scaffold with sunset clause. If performative pause: constraint maintains Snare classification but theater ratio increases. If rebounds: confirms Snare persistence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(post_morality_police_expansion, empirical, 'Classification evolution following morality police suspension').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(iran_hijab_law, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hijab_tr_t0, iran_hijab_law, theater_ratio, 0, 0.42).
narrative_ontology:measurement(hijab_tr_t20, iran_hijab_law, theater_ratio, 20, 0.55).
narrative_ontology:measurement(hijab_tr_t45, iran_hijab_law, theater_ratio, 45, 0.65).

% Extraction over time
narrative_ontology:measurement(hijab_be_t0, iran_hijab_law, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(hijab_be_t20, iran_hijab_law, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(hijab_be_t45, iran_hijab_law, base_extractiveness, 45, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(iran_hijab_law, enforcement_mechanism).
narrative_ontology:affects_constraint(iran_hijab_law, iran_women_labor_participation).
narrative_ontology:affects_constraint(iran_hijab_law, iran_religious_freedom_restriction).
narrative_ontology:affects_constraint(iran_hijab_law, iran_dissent_suppression).

% DUAL FORMULATION NOTE:
% The mandatory hijab law is upstream of women's labor participation constraints and religious freedom restrictions. Each downstream constraint has its own ε reflecting specific domain metrics (labor participation ε≈0.55, religious freedom ε≈0.72), but all share the structural enforcement apparatus. Network decomposition reveals that hijab law is not a unitary constraint but a control mechanism that feeds into multiple extraction vectors.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(iran_hijab_law, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
