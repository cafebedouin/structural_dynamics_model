% ============================================================================
% CONSTRAINT STORY: fifth_republic_constitution__cohabitation_equilibrium_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fifth_republic_cohabitation_equilibrium, []).

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
 *   constraint_id: fifth_republic_constitution__cohabitation_equilibrium_reading
 *   human_readable: Fifth Republic Cohabitation Equilibrium: Dual Executive Authority Allocation
 *   domain: constitutional_law/political_systems
 *
 * SUMMARY:
 *   The Fifth Republic's dual executive system (president as
 *   ceremonial/foreign affairs custodian, prime minister as
 *   legislative/domestic administrator) produces a constitutional constraint
 *   that is simultaneously an equilibrium, an ambiguity, and a source of
 *   extraction. The cohabitation equilibrium reading holds that the 1958
 *   Constitution intentionally creates two executives whose power domains
 *   must be negotiated through practice, producing a stable (if contested)
 *   allocation mechanism. This reading emphasizes that cohabitation, rather
 *   than representing constitutional breakdown, represents the system
 *   functioning as designed: neither executive can unilaterally control the
 *   state without Assembly support, and both must negotiate authority
 *   boundaries. The extractiveness emerges from the fact that policy
 *   coherence and constitutional clarity are sacrificed to maintain this
 *   equilibrium — constitutional ambiguity is the mechanism enabling both
 *   executives to claim authority in contested domains. Cohabitation periods
 *   (1986-1988, 1993-1995, 2017-present) have demonstrated that the
 *   constraint is stable across electoral cycles, but increasingly
 *   theater-laden as executives invest in boundary performance rather than
 *   boundary clarity. The constraint's theater_ratio has risen from 0.45
 *   (early cohabitations, when Constitutional Court still issued binding
 *   interpretations) to 0.61 (contemporary, where Court avoids
 *   inter-executive disputes and executives negotiate through rhetoric and
 *   practice).
 *
 * KEY AGENTS:
 *   - President of the Republic: Primary beneficiary during periods of executive dominance (institutional/arbitrage) — retains foreign affairs custody and ceremonial authority that enables policy override
 *   - Prime Minister: Primary beneficiary during periods of Assembly dominance (institutional/mobile) — controls domestic agenda and legislative process
 *   - Policy Coherence and Constitutional Clarity: Primary victims (powerless/trapped) — abstract collective goods bearing the cost of ambiguous authority allocation
 *   - National Assembly: Secondary actor (institutional/arbitrage) — benefits from cohabitation's requirement that both executives negotiate legislative support
 *   - Constitutional Council/Court: Institutional actor (institutional/arbitrage) — maintains performative authority while actual interpretive power has atrophied
 *   - Electoral-Constitutional Reformers: Organized agents (organized/constrained) — advocate structural alternatives to equilibrium (proportional representation, constitutional amendment)
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements as immutable features of dual-executive design
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.48).
domain_priors:suppression_score(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.52).
domain_priors:theater_ratio(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__cohabitation_equilibrium_reading, tangled_rope).
narrative_ontology:human_readable(fifth_republic_constitution__cohabitation_equilibrium_reading, "Fifth Republic Cohabitation Equilibrium: Dual Executive Authority Allocation").
narrative_ontology:topic_domain(fifth_republic_constitution__cohabitation_equilibrium_reading, "constitutional_law/political_systems").

domain_priors:requires_active_enforcement(fifth_republic_constitution__cohabitation_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__cohabitation_equilibrium_reading, 'b1e23dd4-75c9-4745-9976-582d1a971918').
narrative_ontology:cs_kernel_codification('b1e23dd4-75c9-4745-9976-582d1a971918', fixed_text).
narrative_ontology:cs_authority_grounding('b1e23dd4-75c9-4745-9976-582d1a971918', extraction).
narrative_ontology:cs_interpretation_layer_present('b1e23dd4-75c9-4745-9976-582d1a971918').
narrative_ontology:cs_reading_relation('b1e23dd4-75c9-4745-9976-582d1a971918', fifth_republic_constitution__hyper_presidential_reading, coexists_with).
narrative_ontology:cs_reading_relation('b1e23dd4-75c9-4745-9976-582d1a971918', fifth_republic_constitution__parliamentary_constraint_reading, coexists_with).
narrative_ontology:cs_axiom('b1e23dd4-75c9-4745-9976-582d1a971918', foundational, executive_authority_negotiated_through_practice).
narrative_ontology:cs_axiom_status(executive_authority_negotiated_through_practice, holdable).
narrative_ontology:cs_axiom_grounding('b1e23dd4-75c9-4745-9976-582d1a971918', executive_authority_negotiated_through_practice, conventional).
narrative_ontology:cs_axiom('b1e23dd4-75c9-4745-9976-582d1a971918', foundational, dual_executive_institutional_stability).
narrative_ontology:cs_axiom_status(dual_executive_institutional_stability, holdable).
narrative_ontology:cs_axiom_grounding('b1e23dd4-75c9-4745-9976-582d1a971918', dual_executive_institutional_stability, empirically_contingent).
narrative_ontology:cs_reference_frame('b1e23dd4-75c9-4745-9976-582d1a971918', equilibrium_negotiated_cohabitation).
narrative_ontology:cs_drift_state('b1e23dd4-75c9-4745-9976-582d1a971918', contemporary_post_2017_cohabitation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b1e23dd4-75c9-4745-9976-582d1a971918', '2026-02-27T00:00:00Z').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__cohabitation_equilibrium_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, dominant_executive_actor).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, policy_domain_controller).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, policy_coherence).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, constitutional_clarity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POLICY COHERENCE (SNARE) — The abstract public good of clear governance authority cannot exit the ambiguity trap. Cohabitation produces structural incoherence: presidential domains (foreign affairs, military) and prime ministerial domains (domestic administration, legislative agenda) collide when crises cross domains. The constraint extracts from policy coherence through forced negotiation, vetoes, and role redefinition during each cohabitation period. Maximum experienced extraction because policy coherence has no voice in authority allocation and no exit route.
constraint_indexing:constraint_classification(fifth_republic_constitution__cohabitation_equilibrium_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DOMINANT EXECUTIVE ACTOR (TANGLED ROPE) — The actor controlling policy domains benefits from structural ambiguity: unclear authority boundaries allow the powerful executive to expand into the other's domain through practice and executive privilege claims. Genuine coordination function exists (cohabitation does stabilize shared institutions), but asymmetric extraction occurs when one executive captures momentum and legislative support. Mobility comes from capacity to appeal to voters/Assembly if cohabitation breaks down.
constraint_indexing:constraint_classification(fifth_republic_constitution__cohabitation_equilibrium_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: CONSTRAINED EXECUTIVE ACTOR (TANGLED ROPE) — The weaker executive faces extraction through structural subordination while technically retaining powers. Cannot exit without electoral loss (constrained exit), yet benefits from institutional stability and constitutional legitimacy. Experiences mixed coordination (shared institutional framework) and asymmetric extraction (blocked initiative, reversed policies, resource scarcity in their domain).
constraint_indexing:constraint_classification(fifth_republic_constitution__cohabitation_equilibrium_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: NATIONAL ASSEMBLY (ROPE) — The Assembly benefits from cohabitation's requirement that both executives negotiate legislative agenda. The Assembly experiences cohabitation as coordination mechanism (both executives must cultivate Assembly support) with minimal extraction cost. Assembly has arbitrage options (can shift support between executives, can trigger votes of no-confidence). Low effective extraction because the Assembly's power is enhanced during cohabitation relative to periods of unified control.
constraint_indexing:constraint_classification(fifth_republic_constitution__cohabitation_equilibrium_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ELECTORAL-CONSTITUTIONAL REFORMERS (SCAFFOLD) — Organized actors advocating constitutional clarification of executive roles see cohabitation as a temporary coordination failure with a structural sunset: electoral reforms (proportional representation, fixed term boundaries) and constitutional amendments (clarifying presidential vs prime ministerial domains) are being proposed to replace the equilibrium with clearer authority allocation. This perspective perceives the ambiguity as a solvable institutional design problem, not a stable constraint.
constraint_indexing:constraint_classification(fifth_republic_constitution__cohabitation_equilibrium_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, cohabitation represents an immutable structural property of any dual-executive system: authority cannot be simultaneously held by two agents without negotiation and ambiguity. This perspective naturalizes the constraint as inherent to constitutional design itself. However, the structural data (identifiable beneficiaries, extractive asymmetry during cohabitation periods, theater ratio indicating performative authority claims) suggests a false summit: the appearance of natural law masks contingent institutional arrangements that distribute authority based on electoral power, not constitutional clarity.
constraint_indexing:constraint_classification(fifth_republic_constitution__cohabitation_equilibrium_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: CONSTITUTIONAL COURT (PITON) — The Constitutional Court's interpretive authority over the ambiguous text has degraded into performative boundary maintenance. In early cohabitations (1986-1988, 1993-1995), the Court issued precise rulings clarifying domains. In contemporary cohabitations, the Court increasingly avoids deciding inter-executive disputes, treating cohabitation as requiring political resolution rather than constitutional adjudication. The Court's function persists through institutional habit and deference doctrine, but its actual power to resolve ambiguity has atrophied — theater ratio increases as the Court performs judicial review without substantive authority.
constraint_indexing:constraint_classification(fifth_republic_constitution__cohabitation_equilibrium_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fifth_republic_constitution__cohabitation_equilibrium_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fifth_republic_constitution__cohabitation_equilibrium_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fifth_republic_constitution__cohabitation_equilibrium_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fifth_republic_constitution__cohabitation_equilibrium_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(fifth_republic_constitution__cohabitation_equilibrium_reading, TR),
    TR >= 0.70.

:- end_tests(fifth_republic_constitution__cohabitation_equilibrium_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The constraint extracts from policy coherence through forced negotiation, veto, and authority redefinition. Policy domains cannot be clearly allocated, requiring both executives to expend political capital defending and negotiating boundaries. However, extractiveness is not as severe as snare-level (0.66+) because genuine coordination benefits exist — cohabitation prevents either executive from consolidating power, and maintains Assembly leverage. The moderate value reflects mixed coordination (stabilizing shared institutions) and asymmetric extraction (dominant executive capturing policy domains). Suppression (0.52): Moderate-high. Alternatives to the equilibrium are suppressed through constitutional entrenchment, electoral system design (two-round system favors unified governments vs proportional representation), and path dependency. Actors cannot easily exit cohabitation without constitutional amendment or electoral restructuring. However, suppression is not total — the Constitutional Court could issue clarifying jurisprudence, or electoral reform could change power distribution. Theater ratio (0.61): Moderate-high. Executives invest significant effort in performing authority they may not constitutionally possess (president overreaching on domestic policy, PM claiming prerogatives in foreign affairs). The rise from 0.45 to 0.61 reflects declining judicial authority — early Constitutional Council decisions provided objective boundary clarity, reducing theater; contemporary Court avoidance of inter-executive disputes forces executives to perform authority through rhetoric and practice.
 *
 * PERSPECTIVAL GAP:
 *   The cohabitation equilibrium reading produces dramatically different classifications from different agent perspectives. The constrained powerful executives (both president and PM during opposition Assembly periods) experience tangled_rope — some coordination benefit but significant extraction through blocked initiative and resource scarcity. The national Assembly experiences rope — cohabitation enhances legislative power and requires both executives to negotiate. The abstract public good (policy coherence) experiences snare — no voice, no exit, maximum extraction from ambiguous authority. The Constitutional Court experiences piton — its interpretive authority persists performatively but has atrophied functionally. The electoral-constitutional reformers experience scaffold — the ambiguity is temporary and solvable through structural reform. The analytical observer risks mountain — naturalizing the dual-executive ambiguity as immutable — but the structural data reveals this as false summit (identifiable beneficiaries, extractive asymmetry, theater). The gap reveals that 'cohabitation equilibrium' is not a natural law but a contingent institutional arrangement where different actors perceive the constraint through different structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from each agent's structural relationship to the constraint. The dominant executive (beneficiary with mobile exit options) derives low d → negative chi — they benefit from ambiguity and can mobilize electoral/Assembly support if cohabitation threatens their position. The constrained executive (beneficiary with constrained exit) derives moderate d → moderate positive chi — they benefit from the institutional framework but cannot easily exit without electoral loss. Policy coherence (victim, powerless/trapped) derives maximum d → maximum f(d) — no benefits, no exit options, bears full cost of ambiguity. The National Assembly (beneficiary, arbitrage) derives low d → negative chi — enhanced power during cohabitation, arbitrage options to shift support. The Constitutional Court (beneficiary, arbitrage) derives low d but positive chi offset by piton properties — institutional stability maintained through performative authority. The cohabitation equilibrium reading's overall extractiveness emerges from the distribution of beneficiaries (dominant/constrained executives, Assembly) and victims (policy coherence, constitutional clarity), weighted by their exit options and power levels.
 *
 * MANDATROPHY ANALYSIS:
 *   The cohabitation equilibrium reading resolves potential mandatrophy (classification ambiguity) by specifying that the constraint is TANGLED ROPE at the empirical level: genuine coordination function (stabilizing dual executive through forced negotiation, preventing unilateral consolidation) coexists with asymmetric extraction (dominant executive capturing policy domains, policy coherence bearing ambient cost). The constraint is not pure coordination (rope) because extractive asymmetry exists; it is not pure extraction (snare) because genuine coordination benefits exist. The analytical observer's mountain reading is a false summit — the 'natural law' framing of dual-executive ambiguity masks that the equilibrium is contingent on electoral power distribution, constitutional choice, and institutional path dependency. The piton reading (Constitutional Court's degraded authority) is real but secondary — the piton is a symptom of the equilibrium, not its cause. The scaffold reading (electoral-constitutional reformers' exit path) is real and represents genuine sunset logic for this constraint, though the sunset remains structurally distant (constitutional amendment requires supermajority, electoral reform faces entrenched interests).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cohabitation_beneficiary_identity,
    'Which executive (president or prime minister) is the true beneficiary of the cohabitation equilibrium, or does beneficiary status alternate with electoral outcomes?',
    'Historical analysis of policy domain control across cohabitation periods 1986-1988, 1993-1995, 2017; comparison of legislative success rates, policy reversals, and executive staff expansion for president vs PM across periods; distinction between constitutional beneficiary vs empirical beneficiary',
    'If president is structural beneficiary: cohabitation preserves presidential supremacy despite Assembly opposition (extraction favors executive). If PM is structural beneficiary: cohabitation requires presidential constraint (extraction runs opposite direction). If alternating: constraint is genuinely ambiguous and redistribution depends on electoral power, not constitutional design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cohabitation_beneficiary_identity, empirical, 'Which executive is the structural beneficiary of cohabitation equilibrium').

omega_variable(
    constitutional_text_ambiguity_necessity,
    'Is the ambiguity in the Fifth Republic''s executive authority distribution an inherent feature of dual-executive systems, or a contingent artifact of the 1958 Constitution''s drafting?',
    'Comparative constitutional analysis: examine how other dual-executive systems (Germany, Austria, Portugal, Romania) specify executive domains. Identify whether textual precision eliminates cohabitation dynamics or merely shifts them. Historical analysis of 1958 drafting: did ambiguity arise from intentional delegation to jurisprudence or unintended gap in constitutional specification?',
    'If inherent to dual systems: cohabitation is mountain (natural structural limit). If contingent artifact: cohabitation is tangled_rope or snare (institutional choice). If precision eliminates dynamics: false summit confirmed — the ''natural law'' view masks that constitutional clarity is technically feasible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_text_ambiguity_necessity, conceptual, 'Whether executive ambiguity is inherent to dual systems or contingent to French design').

omega_variable(
    reading_foreclosure_test,
    'Does the cohabitation equilibrium reading logically foreclose the hyper-presidential and parliamentary constraint readings, or do all three remain simultaneously live within different parties'' constitutional frameworks?',
    'Close reading of Constitutional Council jurisprudence and presidential/parliamentary rhetoric across cohabitation periods. Identify whether actors holding the equilibrium reading explicitly deny the premises of the other readings (foreclosure) or merely disagree on their applicability (coexistence). Document whether each reading is tied to specific constitutional moments (e.g., equilibrium dominant 1995-2017, hyper-presidential revival 2017-present).',
    'If foreclosed: the equilibrium reading is dominant and the others are incoherent framings that cannot be maintained. If coexisting: all three readings are live positions reflecting genuine constitutional ambiguity, and the constraint''s extractiveness comes from suppressing the competing readings. If influenced: the readings have upstream/downstream relationships (one reading''s dominance changes conditions for the others).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_test, conceptual, 'Relationship between cohabitation equilibrium and sibling constitutional readings').

omega_variable(
    theater_ratio_mechanism,
    'Is the increase in theater_ratio (0.45 → 0.61) driven by executives performing authority they lack, or by voters/Assembly imposing performance requirements to mask underlying ambiguity?',
    'Analysis of executive rhetoric and practice patterns: track instances where an executive claims authority beyond constitutional text (presidential foreign policy overreach, PM legislative agenda override), versus instances where the Assembly or voters demand ritual performances of legitimate authority. Distinguish executive performance from democratic performance.',
    'If executive-driven performance: extraction mechanism is active — executives use theater to claim territory. If voter/Assembly-driven: theater is suppression mechanism — citizens demand reassurance of authority clarity even when text is ambiguous. Mixed scenario: theater serves both functions, increasing over cohabitations as actors become more sophisticated in boundary theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_mechanism, empirical, 'Whether theater ratio increase is executive performance or democratic demand').

omega_variable(
    kernel_reading_ambiguity,
    'This constraint represents ONE reading (cohabitation equilibrium) of the contested Fifth Republic constitutional kernel. Are the sibling readings (hyper-presidential, parliamentary constraint) genuinely incompatible with this reading in a single constitutional framework, or do they represent different temporal phases of application of the same text?',
    'Constitutional jurisprudence analysis: trace how Constitutional Council interpretations have shifted across periods. Document whether the text itself changed (it did not materially) or interpretations changed (they did). Determine whether cohabitation equilibrium, hyper-presidential, and parliamentary readings can coexist as valid framings applied in different political contexts, or whether one reading must dominate for the framework to be coherent.',
    'If genuinely incompatible: one reading forecloses others (rare). If temporal phases: all three readings coexist but in different electoral moments. If interpretation-dependent: the kernel (text) is stable but reference frames shift, producing drift in reading dominance. The classification of the constraint (mountain vs tangled_rope) depends on whether the kernel is stable (mountain) or the reading is contestable (tangled_rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Relationship between cohabitation equilibrium reading and sibling constitutional readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__cohabitation_equilibrium_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cohabit_eq_theater_t0, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(cohabit_eq_theater_t10, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 10, 0.53).
narrative_ontology:measurement(cohabit_eq_theater_t20, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 20, 0.61).

% Extraction over time
narrative_ontology:measurement(cohabit_eq_extract_t0, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cohabit_eq_extract_t10, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(cohabit_eq_extract_t20, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 20, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(cohabit_eq_suppress_t0, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(cohabit_eq_suppress_t10, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 10, 0.47).
narrative_ontology:measurement(cohabit_eq_suppress_t20, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_republic_constitution__cohabitation_equilibrium_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fifth_republic_constitution__cohabitation_equilibrium_reading, fifth_republic_constitution__hyper_presidential_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__cohabitation_equilibrium_reading, fifth_republic_constitution__parliamentary_constraint_reading).

% DUAL FORMULATION NOTE:
% The cohabitation equilibrium reading is one of three structurally distinct constraint stories derived from the Fifth Republic constitutional kernel. The hyper-presidential reading (ε ≈ 0.25, mountain-like) emphasizes that the Constitution privileges the presidency despite cohabitation periods. The parliamentary constraint reading (ε ≈ 0.55, snare-like) emphasizes that the Assembly ultimately constrains both executives. The cohabitation equilibrium reading (ε = 0.48, tangled_rope) treats the ambiguity as functional and stable. The three readings are linked by network.affects_constraints because they represent competing interpretations of the same constitutional text — changes in which reading dominates the political discourse affect the extractiveness of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fifth_republic_constitution__cohabitation_equilibrium_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
