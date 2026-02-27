% ============================================================================
% CONSTRAINT STORY: eu_unanimity_rule_foreign_policy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_unanimity_rule_foreign_policy, []).

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
 *   constraint_id: eu_unanimity_rule_foreign_policy
 *   human_readable: EU Unanimity Requirement for Foreign Policy and Financial Decisions
 *   domain: geopolitical/regulatory
 *
 * SUMMARY:
 *   The European Union's unanimity requirement for foreign policy and major
 *   financial decisions creates a structural hybrid between coordination
 *   mechanism and extraction system. All 27 member states must agree before
 *   the EU can impose sanctions, launch military operations, or commit major
 *   financial resources to foreign aid or defense. This rule protects smaller
 *   states' sovereignty and ensures no member bears externalized costs from
 *   collective decisions. Simultaneously, it enables veto coalitions to
 *   extract concessions from the majority and prevents the EU from responding
 *   with agility to crises. The constraint exhibits dramatically different
 *   classifications from different structural perspectives: small
 *   veto-holding states see protection (Rope); the supranational capacity for
 *   unified action sees paralysis (Snare); large powerful members see
 *   constrained leverage (Tangled Rope); reform coalitions see a temporary
 *   problem being solved (Scaffold); the treaty architecture establishment
 *   sees a persistent ritual (Piton); and the Westphalian sovereignty
 *   doctrine sees an immutable law (Mountain — false summit). The constraint
 *   has intensified over the past decade as geopolitical urgency increased
 *   (Ukraine, China, Middle East) while consensus became harder to achieve.
 *   Theater ratio has risen from 0.50 to 0.65 as negotiators engage in
 *   elaborate performance of consensus-seeking while functional alternatives
 *   (emergency procedures, coalitions of the willing) proliferate behind the
 *   scenes.
 *
 * KEY AGENTS:
 *   - Small Veto-Holding States (e.g., Hungary, Poland, Cyprus): Primary beneficiary (institutional/arbitrage) — use veto as leverage to extract concessions on budget, agricultural policy, or bilateral disputes; experience unanimity as protection of sovereignty
 *   - Large Powerful Members (France, Germany, Italy): Secondary beneficiary + victim (powerful/constrained) — benefit from EU legitimacy and collective action framework but constrained from unilateral policy imposition; experience extraction when small states block preferred sanctions or aid
 *   - EU Supranational Capacity (Commission, EEAS, Parliament): Primary victim (powerless/trapped) — cannot exit unanimity requirement; bears full cost of decision paralysis; abstract collective good unable to organize
 *   - Non-Veto-Aligned Majorities (typically 20+ states agreeing on foreign policy): Secondary victim (organized/constrained) — face extraction when small coalition holds veto; must offer concessions to move forward
 *   - QMV Reform Coalition (France, Germany, progressive reformers): Organized agents (organized/constrained) — building alternative pathways via treaty reform and emergency procedures; see unanimity as temporary problem with sunset
 *   - Treaty Architecture Establishment (Council Presidency, diplomats): Institutional actor (institutional/arbitrage) — maintains unanimity ritual through diplomatic convention and treaty text; benefits from existing rules
 *   - Analytical Observer (Geopolitical analyst): Civilizational view (analytical/analytical) — risks naturalizing contingent institutional choice as immutable law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_unanimity_rule_foreign_policy, 0.52).
domain_priors:suppression_score(eu_unanimity_rule_foreign_policy, 0.68).
domain_priors:theater_ratio(eu_unanimity_rule_foreign_policy, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_unanimity_rule_foreign_policy, extractiveness, 0.52).
narrative_ontology:constraint_metric(eu_unanimity_rule_foreign_policy, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(eu_unanimity_rule_foreign_policy, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_unanimity_rule_foreign_policy, tangled_rope).
narrative_ontology:human_readable(eu_unanimity_rule_foreign_policy, "EU Unanimity Requirement for Foreign Policy and Financial Decisions").
narrative_ontology:topic_domain(eu_unanimity_rule_foreign_policy, "geopolitical/regulatory").

domain_priors:requires_active_enforcement(eu_unanimity_rule_foreign_policy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_unanimity_rule_foreign_policy, small_member_states).
narrative_ontology:constraint_beneficiary(eu_unanimity_rule_foreign_policy, veto_holding_states).
narrative_ontology:constraint_victim(eu_unanimity_rule_foreign_policy, eu_unified_action_capacity).
narrative_ontology:constraint_victim(eu_unanimity_rule_foreign_policy, rapid_response_capabilities).
narrative_ontology:constraint_victim(eu_unanimity_rule_foreign_policy, non_veto_powerful_members).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EU UNIFIED ACTION CAPACITY (SNARE) — The supranational capacity for coordinated foreign policy cannot exit the unanimity requirement. Bears full cost of veto power: sanctions delayed, humanitarian responses blocked, strategic decisions paralyzed. d≈0.92, f(d)≈1.40, σ=1.1 → χ≈0.80. The constraint extracts from the system's collective agency.
constraint_indexing:constraint_classification(eu_unanimity_rule_foreign_policy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: SMALL VETO-HOLDING STATE (ROPE) — Unanimity requirement protects this actor's sovereignty and bargaining power; provides coordination benefit (ensures no decision imposes unilateral costs). Experiences the constraint as protection and leverage. d≈0.10, f(d)≈0.00, σ=1.1 → χ≈0.00. Net beneficiary; coordination function dominant.
constraint_indexing:constraint_classification(eu_unanimity_rule_foreign_policy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 3: LARGE POWERFUL MEMBER STATE (TANGLED ROPE) — Benefits from EU framework (market access, collective security); constrained by unanimity (cannot impose preferred policies unilaterally). Faces extraction: must compromise with smaller states or smaller powers with veto. Also benefits from coordination function (collective action on sanctions has more legitimacy and enforcement). d≈0.58, f(d)≈0.75, σ=1.1 → χ≈0.43. Mixed: coordination function + asymmetric extraction.
constraint_indexing:constraint_classification(eu_unanimity_rule_foreign_policy, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 4: QMV REFORM COALITION (SCAFFOLD) — Organized actors (France, Germany, Commission, Parliament) pushing for qualified majority voting (QMV) in foreign policy see unanimity as a temporary problem with a sunset. Treaty reform paths (gradual expansion of QMV, emergency procedures) are creating exit routes. d≈0.42, f(d)≈0.42, σ=1.1 → χ≈0.24. Low effective extraction because coalition perceives and is building toward a structural solution.
constraint_indexing:constraint_classification(eu_unanimity_rule_foreign_policy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: TREATY ARCHITECTURE RITUAL (PITON) — Unanimity requirement is a vestige of Westphalian state sovereignty doctrine. Theater ratio = 0.65: much of the constraint's enforcement is theatrical (sovereignty rhetoric, solemn treaty declarations) rather than functional (actual enforcement of unanimity blocking most decisions). The rule persists through institutional inertia — it's maintained because it's 'in the treaty' despite alternative decision mechanisms (emergency procedures, constructive abstention) being used in practice. theater_ratio≥0.70 not met; classify as piton via degenerated function: the rule's legitimacy narrative (sacred state autonomy) persists while functional alternatives proliferate.
constraint_indexing:constraint_classification(eu_unanimity_rule_foreign_policy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / SOVEREIGNTY DOCTRINE VIEW (MOUNTAIN) — From a civilizational perspective framed by Westphalian principles, the unanimity requirement appears as an immutable natural law: autonomous states can never be bound against their will. However, the structural data (ε=0.52, suppression=0.68, theater=0.65) contradicts mountain classification. This reveals the false summit: state sovereignty is not a law of physics; it's a political doctrine (contingent, historically specific). EU itself demonstrates that states can delegate authority to supranational institutions.
constraint_indexing:constraint_classification(eu_unanimity_rule_foreign_policy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: ALIGNED MAJORITY SUBGROUP (TANGLED ROPE) — When 20+ member states agree on foreign policy and a small coalition holds veto, the majority is extracted from but also benefits from the coordination ritual (collective legitimacy). Can exit via treaty change or coalition realignment but faces mobilization costs. d≈0.65, f(d)≈1.00, σ=1.1 → χ≈0.57. Moderate extraction.
constraint_indexing:constraint_classification(eu_unanimity_rule_foreign_policy, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_unanimity_rule_foreign_policy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(eu_unanimity_rule_foreign_policy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(eu_unanimity_rule_foreign_policy, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(eu_unanimity_rule_foreign_policy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(eu_unanimity_rule_foreign_policy, TR),
    TR >= 0.70.

:- end_tests(eu_unanimity_rule_foreign_policy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The unanimity requirement genuinely enables extraction: small states use veto to obtain budget rebates, agricultural protection, or bilateral concessions unrelated to the foreign policy question. The extraction is not total (EU still acts on many issues, states often abstain rather than veto) but is significant. Rising over the interval (0.35→0.52) reflects intensifying geopolitical crises (Ukraine 2014+, China assertiveness 2018+) making unanimity increasingly costly to both extract from and maintain. Suppression (0.68): High. Significant barriers to exit include treaty lock-in (changing unanimity requires all states to ratify), political lock-in (small states fear majority tyranny if unanimity ends), and legitimacy narrative (sovereignty rhetoric makes veto politically defensible). Suppression is structural, not scaled by scope. Theater ratio (0.65): Moderate-high, rising. Much of the constraint's enforcement is theatrical: elaborate consensus-seeking rituals (COREPER procedures, rotating presidencies, diplomatic wording), performative unanimity declarations. But functional alternatives exist and are increasingly used: emergency procedures (Article 44 TEU, constructive abstention) handle many actual decisions. Theater has risen over the interval as negotiators became more skilled at performing consensus while using workarounds. The rule persists through inertia (it's 'in the treaty') rather than function. Claimed type (tangled_rope): Justified by presence of coordination function (unanimity ensures distributed legitimacy for sanctions, prevents free-riding on collective defense) AND asymmetric extraction (small veto coalitions extract from majority). Both conditions required for tangled_rope gate satisfied.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence. Small veto-holding states classify it as Rope (pure coordination, protection of interests) and would fiercely oppose QMV (Snare from their perspective). The supranational capacity for unified action sees Snare (full extraction, cannot exit). Large powerful states see Tangled Rope (mixed: constrained by unanimity but benefit from legitimacy and coordination). The reform coalition sees Scaffold (temporary problem with a sunset via treaty change). The treaty establishment sees Piton (performative ritual maintained through inertia). The civilizational observer risks Mountain (naturalizing state sovereignty as law). These perspectives are structurally incompatible — one agent's protection is another's paralysis. The perspectival gap reveals that there is no single 'correct' classification; the constraint's identity depends on the observer's structural position relative to it.
 *
 * DIRECTIONALITY LOGIC:
 *   Small veto-holding state: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; veto is leverage and protection. Large powerful member: Victim + constrained → d≈0.60, f(d)≈0.80. Significant extraction; cannot unilaterally impose policy but must negotiate with veto holders. EU capacity: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction; abstract collective cannot exit and cannot organize. Aligned majority: Victim + mobile → d≈0.65, f(d)≈1.00. Moderate extraction; can theoretically realign or exit via treaty change but faces mobilization costs. QMV reform coalition: Organized + constrained → d≈0.42, f(d)≈0.40. Low effective extraction; coalition has agency and is building structural solution. Treaty establishment: Institutional + arbitrage → d≈0.05, f(d)≈-0.12. Net beneficiary; rule maintenance benefits existing institutional roles. The pattern shows high d for all victim groups (constrained exit, no arbitrage opportunities) and low d for beneficiaries (arbitrage exit, veto leverage).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: This constraint avoids the trap of mislabeling as pure extraction by explicitly identifying its coordination function (unanimity ensures distributed legitimacy, prevents unilateral enforcement of costly policies on unwilling minorities). The coordination function is real and important — without it, the EU would risk appearing as an imperial force imposing sanctions on dissenting states. The tangled_rope classification resolves the mandatrophy by acknowledging that the same rule creates both genuine coordination benefit AND asymmetric extraction. The false summit (mountain perspective) is caught: while sovereignty language naturalizes unanimity as immutable, the constraint is actually a contingent institutional choice — the EU has repeatedly reformed decision-making (Lisbon expanded QMV to ~80% of decisions), and further expansion is politically plausible (even if difficult). The piton observation (theater_ratio=0.65) is important: the rule's functional importance has declined (emergency procedures, constructive abstention, coalitions of the willing handle many decisions), but its theatrical importance has risen (maintaining the fiction of unanimity preserves legitimacy narrative). This is characteristic of constraints in transition — the piton perspective signals that the constraint is beginning to degrade and may be vulnerable to rapid institutional collapse if a crisis triggers treaty reform or coalition defection.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_delegation_boundary,
    'Is the unanimity requirement a protection of irreducible state sovereignty or a reversible institutional choice?',
    'Historical analysis of treaty evolution (Lisbon, Nice, Amsterdam); comparison with other supranational bodies (UN Security Council, IMF board). If QMV has expanded repeatedly without state dissolution, unanimity is a choice, not a boundary.',
    'If irreducible: mountain classification (false summit). If reversible: tangled_rope classification confirmed — extraction mechanism is institutional design, not natural law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_delegation_boundary, empirical, 'Whether unanimity is irreducible sovereignty or reversible institutional choice').

omega_variable(
    veto_leverage_distribution,
    'Do small veto-holding states capture more value from unanimity than large states sacrifice, or is the extraction symmetric?',
    'Quantitative analysis of veto incidents (how many times used), policy concessions extracted, resource transfers in exchange for agreement removal. Compare beneficiary group size and power distribution.',
    'If symmetric: rope classification (pure coordination). If asymmetric: tangled_rope confirmed — small states extract disproportionate value from larger states'' constrained exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_leverage_distribution, empirical, 'Distribution of veto leverage across member states').

omega_variable(
    functional_unanimity_replacement,
    'Are emergency procedures (constructive abstention, enhanced cooperation, passarelle clauses) already replacing unanimity functionally, making the rule a piton?',
    'Audit of foreign policy decisions in last 10 years: how many formally require unanimity? How many use alternative procedures? Measure theater_ratio via comparison of decision-making time with and without unanimity requirement.',
    'If alternatives handle >60% of functional decisions: piton classification confirmed (rule is maintained through inertia, not function). If <30%: tangled_rope confirmed (rule still constrains outcomes).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(functional_unanimity_replacement, empirical, 'Whether functional alternatives have replaced unanimity requirement').

omega_variable(
    qmv_treaty_reform_timeline,
    'What is the plausible timeline for qualified majority voting expansion in foreign policy via treaty reform?',
    'Political feasibility analysis: member state positions on QMV expansion, likelihood of next treaty reform, probability of ratification. Estimate when 23/27 majority + ratification majority becomes politically achievable.',
    'If <10 years: scaffold sunset is real and near. If 20+ years: scaffold classification is aspirational, not structural. Reverts to snare from EU capacity perspective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(qmv_treaty_reform_timeline, conceptual, 'Plausible timeline for QMV expansion in foreign policy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_unanimity_rule_foreign_policy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_unan_tr_t0, eu_unanimity_rule_foreign_policy, theater_ratio, 0, 0.5).
narrative_ontology:measurement(eu_unan_tr_t5, eu_unanimity_rule_foreign_policy, theater_ratio, 5, 0.58).
narrative_ontology:measurement(eu_unan_tr_t10, eu_unanimity_rule_foreign_policy, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(eu_unan_be_t0, eu_unanimity_rule_foreign_policy, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(eu_unan_be_t5, eu_unanimity_rule_foreign_policy, base_extractiveness, 5, 0.43).
narrative_ontology:measurement(eu_unan_be_t10, eu_unanimity_rule_foreign_policy, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_unanimity_rule_foreign_policy, enforcement_mechanism).
narrative_ontology:affects_constraint(eu_unanimity_rule_foreign_policy, eu_sanctions_regime_effectiveness).
narrative_ontology:affects_constraint(eu_unanimity_rule_foreign_policy, eu_military_capacity_coordination).
narrative_ontology:affects_constraint(eu_unanimity_rule_foreign_policy, qmv_expansion_treaty_reform).

% DUAL FORMULATION NOTE:
% The unanimity requirement should be decomposed into two distinct constraints if the analysis reveals that physical/logical limits on coordination differ from political limits on treaty reform. The current story treats unanimity as a single institutional rule with multiple perspectives; if ε changed significantly depending on whether one measured 'functional decision-making capacity' vs 'legal treaty requirement', separate stories would be required. Currently ε=0.52 reflects the blended institutional extractiveness (actual capacity reduction + legal requirement). If downstream analysis reveals these have different ε values (e.g., functional capacity ε=0.38 vs treaty obligation ε=0.65), decompose into upstream constraint (legal requirement) and downstream constraint (functional capacity impact).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eu_unanimity_rule_foreign_policy, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
