% ============================================================================
% CONSTRAINT STORY: proportionality_doctrine__balancing_critique_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_proportionality_doctrine__balancing_critique_reading, []).

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
 *   constraint_id: proportionality_doctrine__balancing_critique_reading
 *   human_readable: Proportionality Doctrine: Balancing as Judicial Preference Masquerading as Measurement
 *   domain: legal/constitutional_doctrine
 *
 * SUMMARY:
 *   The balancing-critique reading of proportionality doctrine identifies the
 *   constitutional test as a mechanism that performs the appearance of
 *   constraint while delivering judicial discretion. The critique traces from
 *   Schmitt through mid-20th-century German constitutional theory to
 *   contemporary Habermasian and post-structural accounts: proportionality's
 *   final step ('proportionate in the narrow sense') abandons doctrinal
 *   rule-boundedness and delegates to judicial intuition about whether a
 *   rights burden is acceptable. The critique observes that once courts move
 *   past the first three steps (legitimate aim, suitability, necessity) —
 *   which do constrain outcomes through rule-based reasoning — the final
 *   balancing stage contains no further criteria. Courts must weigh
 *   incommensurable goods (liberty vs. security, dignity vs. utility) without
 *   a measuring scale, and call the result 'proportionality.' This reading
 *   treats the doctrine as a tangled rope: it provides genuine coordination
 *   (a shared framework for constitutional reasoning that enables courts to
 *   claim rigor and comparability) but at the cost of suppressing rule-bound
 *   predictability and masking judicial preference-setting as neutral
 *   measurement. The beneficiary is the balancing judge and the authoritative
 *   court; the victims are legal certainty and the rights claimant's ability
 *   to predict outcomes. The extractiveness value reflects that the
 *   doctrine's theatrical mask (measurement language, four-step structure) is
 *   increasingly transparent to critics, yet institutional inertia sustains
 *   it.
 *
 * KEY AGENTS:
 *   - Rights Claimant (Ordinary Litigant): Primary victim (powerless/trapped) — bears full cost of discretionary balancing at the doctrine's final step; cannot predict or appeal to settled rule.
 *   - Lower Court Judge: Secondary actor (moderate/constrained) — benefits from proportionality's coordination function (structured discourse) while experiencing suppression of alternative methods; experiences doctrine as both enabling and constraining.
 *   - Constitutional/Supreme Court: Primary beneficiary (institutional/arbitrage) — uses proportionality to legitimate discretionary power; can redefine what counts as 'proportionate' to suit its jurisprudential trajectory.
 *   - Comparative Constitutional Network: Organized beneficiary (organized/constrained) — benefits from global export of proportionality as shared framework; locked into the framework's indeterminacy.
 *   - Formalist Constitutional Scholars: Secondary beneficiary (institutional/arbitrage) — defend proportionality through theoretical reconstruction; maintain the doctrine through academic commentary.
 *   - The Proportionality Doctrine Itself: Institutional structure (piton) — persists through inertia despite functional atrophy; maintained because alternatives have not been institutionalized.
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing the balancing step's indeterminacy as a law of constitutional reasoning rather than a contingent doctrinal choice.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(proportionality_doctrine__balancing_critique_reading, 0.52).
domain_priors:suppression_score(proportionality_doctrine__balancing_critique_reading, 0.58).
domain_priors:theater_ratio(proportionality_doctrine__balancing_critique_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(proportionality_doctrine__balancing_critique_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(proportionality_doctrine__balancing_critique_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(proportionality_doctrine__balancing_critique_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(proportionality_doctrine__balancing_critique_reading, tangled_rope).
narrative_ontology:human_readable(proportionality_doctrine__balancing_critique_reading, "Proportionality Doctrine: Balancing as Judicial Preference Masquerading as Measurement").
narrative_ontology:topic_domain(proportionality_doctrine__balancing_critique_reading, "legal/constitutional_doctrine").

domain_priors:requires_active_enforcement(proportionality_doctrine__balancing_critique_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(proportionality_doctrine__balancing_critique_reading, '18fa7934-639c-4567-b92d-530e412d16cc').
narrative_ontology:cs_kernel_codification('18fa7934-639c-4567-b92d-530e412d16cc', formalized).
narrative_ontology:cs_authority_grounding('18fa7934-639c-4567-b92d-530e412d16cc', lineage).
narrative_ontology:cs_interpretation_layer_present('18fa7934-639c-4567-b92d-530e412d16cc').
narrative_ontology:cs_reading_relation('18fa7934-639c-4567-b92d-530e412d16cc', proportionality_doctrine__global_export_reading, coexists_with).
narrative_ontology:cs_reading_relation('18fa7934-639c-4567-b92d-530e412d16cc', proportionality_doctrine__structured_reason_reading, coexists_with).
narrative_ontology:cs_axiom('18fa7934-639c-4567-b92d-530e412d16cc', foundational, balancing_stage_discretionary).
narrative_ontology:cs_axiom_status(balancing_stage_discretionary, holdable).
narrative_ontology:cs_axiom_grounding('18fa7934-639c-4567-b92d-530e412d16cc', balancing_stage_discretionary, empirically_contingent).
narrative_ontology:cs_axiom('18fa7934-639c-4567-b92d-530e412d16cc', foundational, measurement_language_fiction).
narrative_ontology:cs_axiom_status(measurement_language_fiction, holdable).
narrative_ontology:cs_axiom_grounding('18fa7934-639c-4567-b92d-530e412d16cc', measurement_language_fiction, deontological).
narrative_ontology:cs_reference_frame('18fa7934-639c-4567-b92d-530e412d16cc', constrained_judicial_reasoning).
narrative_ontology:cs_drift_state('18fa7934-639c-4567-b92d-530e412d16cc', contemporary_post_critique, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('18fa7934-639c-4567-b92d-530e412d16cc', '2026-02-27T14:32:18Z').
narrative_ontology:cs_kernel_id(proportionality_doctrine__balancing_critique_reading, proportionality_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(proportionality_doctrine__balancing_critique_reading, judicial_discretion).
narrative_ontology:constraint_victim(proportionality_doctrine__balancing_critique_reading, legal_certainty).
narrative_ontology:constraint_victim(proportionality_doctrine__balancing_critique_reading, rule_bound_predictability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RIGHTS CLAIMANT (SNARE) — The litigant invoking a fundamental right faces the proportionality balancing test at the end of the doctrinal road. At this juncture, the outcome depends not on rule-bound criteria but on judicial intuition about whether the burden is 'proportionate in the narrow sense.' The claimant cannot predict the outcome, cannot appeal to settled doctrine, and bears the full cost of judicial discretion masquerading as measurement. Maximum extraction: the constraint extracts legal certainty from the powerless agent.
constraint_indexing:constraint_classification(proportionality_doctrine__balancing_critique_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LOWER COURT JUDGE (TANGLED ROPE) — The lower court judge benefits from the balancing test as a coordination mechanism: it provides a structured four-step framework (legitimate aim, suitable means, necessary, proportionate in the narrow sense) that creates the appearance of rigorous reasoning, enabling him to articulate decisions and signal reasoned deliberation. Yet the judge is also constrained by the doctrine's performative character — the proportionality test appears to constrain judicial discretion but actually conceals it. The judge experiences both coordination (the framework structures discourse) and extraction (the framework masks his own discretionary moves). Moderate extraction because the judge has some agency in deploying the framework.
constraint_indexing:constraint_classification(proportionality_doctrine__balancing_critique_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CONSTITUTIONAL COURT (ROPE) — The supreme constitutional court experiences proportionality as a coordination mechanism that legitimates its own authority. The doctrine enables the court to claim that it is measuring, not choosing — that it is applying a neutral standard, not exercising political power. The balancing test coordinates legal discourse by creating a shared vocabulary (suitability, necessity, proportionality) that allows the court to communicate its reasoning in terms that sound technical rather than discretionary. Net benefit: the court uses proportionality to coordinate its own legitimacy. Exit options are arbitrage — the court can always redefine what counts as 'proportionate' and deploy the doctrine to suit its needs.
constraint_indexing:constraint_classification(proportionality_doctrine__balancing_critique_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COMPARATIVE CONSTITUTIONAL NETWORK (TANGLED ROPE) — Organized constitutional courts and law professors benefit from proportionality's global export as a shared framework for constitutional reasoning. The doctrine coordinates multinational constitutional discourse — courts in Canada, South Africa, and Strasbourg can reference the same four-step test and thereby align their jurisprudence. Yet this network is also constrained by the doctrine's inherent indeterminacy at the balancing stage. The more courts adopt proportionality, the more they become locked into a framework that performs constraint while enabling discretion. Organized agents have agency in interpreting the doctrine but are constrained by the need to maintain coherence with the global network. Moderate extraction because the coordination benefits are real even as the framework masks discretionary power.
constraint_indexing:constraint_classification(proportionality_doctrine__balancing_critique_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: SCHOLARLY DEFENSE / FORMALIST READING (PITON) — A subset of constitutional scholars continues to defend proportionality as a structured method of reason-giving, insisting that the four-step test does constrain judicial discretion by forcing courts to articulate their reasoning in a replicable form. This perspective treats proportionality as a genuine coordination mechanism that has merely been misapplied or misunderstood. The scholarly defense is piton-class because it is mostly performative: it maintains the doctrine through theoretical reconstruction and redefines proportionality's failures as implementation problems rather than structural limits. Theater ratio is high — the formalist reading produces extensive academic commentary that reconstructs proportionality as rational deliberation, but the method does not actually prevent the judicial preference-masquerading-as-measurement that critics identify.
constraint_indexing:constraint_classification(proportionality_doctrine__balancing_critique_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: BALANCING DOCTRINE AS INSTITUTIONAL INERTIA (PITON) — The proportionality doctrine itself, viewed as an entrenched institutional practice, is maintained through inertia and lack of feasible alternatives. Constitutional courts have invested heavily in proportionality reasoning; law professors have built careers on refining its categories; comparative constitutional law has treated it as a universal standard. The doctrine persists not because it solves the judicial discretion problem but because abandoning it would require courts to admit that their reasoning is less constrained than proportionality's language suggests. Theater ratio is high — the machinery of proportionality reasoning performs constraint while delivering discretion. The doctrine's function has atrophied, but its institutional form persists.
constraint_indexing:constraint_classification(proportionality_doctrine__balancing_critique_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LIMIT VIEW (MOUNTAIN) — From a civilizational, universal perspective, some degree of judicial discretion in applying fundamental rights may be an irreducible feature of legal systems: any method of balancing competing goods (liberty, security, dignity) will eventually reach a point where doctrine runs out and judgment takes over. This perspective treats the indeterminacy of proportionality's final step as a natural law of constitutional reasoning — not a contingent institutional feature but an immutable limit on how much judicial discretion can be cabined by rules. However, the structural data contradicts this classification: the extraction flows systematically toward judicial discretion and away from legal certainty, suggesting that the 'natural limit' is actually a contingent choice to stop justifying at the balancing stage. Engine false-summit detection will flag this as naturalization of institutional arrangement.
constraint_indexing:constraint_classification(proportionality_doctrine__balancing_critique_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(proportionality_doctrine__balancing_critique_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(proportionality_doctrine__balancing_critique_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(proportionality_doctrine__balancing_critique_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(proportionality_doctrine__balancing_critique_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(proportionality_doctrine__balancing_critique_reading, TR),
    TR >= 0.70.

:- end_tests(proportionality_doctrine__balancing_critique_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, rising over time. The doctrine's extractiveness derives primarily from the suppression of rule-bound predictability at the balancing stage. In the early phase (t=0, ε=0.38), proportionality offered genuine coordination benefits — the four-step test was relatively new and appeared to constrain judicial discretion more tightly. As the doctrine has matured and courts have repeatedly deployed the balancing stage to reach preferred outcomes, the theatrical character has become more visible. Critics have exposed that the measurement language is fiction. Yet the doctrine persists. Current value (t=30, ε=0.52) reflects that extractiveness has increased as the mask has worn thin but institutional resistance remains strong. Suppression (0.58): Moderate-high. The doctrine suppresses alternative methods of constraint (bright-line rules, categorical tiers, legislative specification) and suppresses the visibility of judicial discretion by wrapping it in measurement language. But suppression is not complete — the critique is now part of mainstream constitutional theory, and some courts are attempting to limit proportionality balancing. Theater ratio (0.68): High and rising. The proportionality test performs constraint through the machinery of the four-step framework, elaborate sub-categorizations, and measurement language. The academic literature on proportionality — thousands of articles refining categories, defending the method, or reconstructing it — is largely theatrical maintenance of the doctrine. The theater rises as criticism accumulates because defenders must invest more effort in reconstructing proportionality as rational decision-making.
 *
 * PERSPECTIVAL GAP:
 *   The key perspectival gap lies between the institutional actors (constitutional courts, scholarly defenders) who see proportionality as coordination and rule-based constraint, and the powerless agents (rights claimants, lower courts constrained by the doctrine) who experience it as discretionary extraction. The organized comparative network occupies an intermediate position: they benefit from the framework's global export but are constrained by its indeterminacy. The piton perspective reveals that institutional defense of proportionality is increasingly performative. The analytical observer's mountain view — treating balancing indeterminacy as natural to constitutional reasoning — risks naturalizing what the structural data shows is a contingent institutional choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from structural position relative to this specific constraint. The rights claimant (powerless/trapped) has d ≈ 0.95: maximum target, bears full cost of discretion. The lower court judge (moderate/constrained with mixed beneficiary-victim status) has d ≈ 0.55: benefits from coordination structure but is constrained by the doctrine's opacity. The constitutional court (institutional/arbitrage) has d ≈ 0.15: low directionality, full beneficiary — uses the doctrine to legitimate its own discretion and can redefine categories at will. The comparative network (organized/constrained) has d ≈ 0.45: moderate directionality, experiences both benefit (shared global framework) and cost (locked into indeterminacy). Scholarly defenders (institutional/arbitrage) have d ≈ 0.10: low directionality, benefit from the doctrine's continued existence through their professional investment in its defense. The piton perspective (institutional/arbitrage) has d ≈ 0.08: the doctrine itself is the actor here, and it is maintained through inertia, not active extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves mandatrophy by identifying proportionality as a genuine tangled rope: it coordinates legal discourse through its four-step structure and shared vocabulary, enabling courts to reason and citizens to understand judicial decisions in a shared language. This coordination function is real. Yet the doctrine also extracts: it suppresses rule-bound predictability by reserving unbounded discretion for the final balancing stage, and it extracts legitimacy through the fiction of measurement. The mandatrophy is resolved by showing that both functions are present and neither is illusory. The debate between critics and defenders is not about whether proportionality coordinates or extracts — both are true — but about the magnitudes and whether the trade-off is justified.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    balancing_indeterminacy_origin,
    'Is the indeterminacy of ''proportionate in the narrow sense'' inherent to weighing competing constitutional goods, or is it a contingent failure to specify further criteria at the final step?',
    'Comparative analysis of proportionality doctrine across courts: do courts that attempt to specify sub-criteria for the balancing stage achieve greater predictability? Does the indeterminacy persist at the same magnitude or diminish? Historical reconstruction: why did the German Constitutional Court stop at the balancing stage rather than developing further sub-rules?',
    'If inherent: balancing indeterminacy cannot be eliminated; the constraint remains snare-class for powerless agents. If contingent: alternative doctrinal formulations could reduce discretion; the balancing stage is a choice, not a necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balancing_indeterminacy_origin, empirical, 'Whether balancing indeterminacy is inherent to constitutional reasoning or contingent institutional choice').

omega_variable(
    measurement_fiction_function,
    'Does the fiction of proportionality as ''measurement'' serve a legitimate legitimation function (courts can claim they are reasoning, not choosing) or is it primarily a concealment mechanism for judicial discretion?',
    'Discourse analysis: court opinions deploying proportionality — do they signal genuine constraint, or does linguistic analysis reveal the measurement language as rhetorical cover? Empirical study: do courts cite the proportionality framework when it actually constrains them, or only when it licenses their preferred outcome?',
    'If legitimation function is net positive: the constraint''s suppression is justified by the transparency gains from forcing courts to articulate reasoning. If concealment dominates: suppression is pure extraction, and the classification should rise toward snare for institutional actors.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(measurement_fiction_function, empirical, 'Whether measurement language serves legitimation or concealment in proportionality doctrine').

omega_variable(
    reading_kernel_ambiguity,
    'Does the proportionality doctrine itself contain the seeds of the balancing critique, or is the critique external to the doctrine''s self-understanding?',
    'Doctrinal history: do the doctrine''s foundational texts (German Constitutional Court decisions, comparative jurisprudence) acknowledge the limits of the balancing stage? Do contemporary defenders attempt to reinforce or reformulate the balance stage in response to critique?',
    'If internal: the critique reveals something the doctrine''s own logic points toward; the doctrine is self-undermining. If external: the critique reframes the doctrine from outside; proportionality and its critique are incommensurable readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity, conceptual, 'Whether the balancing critique is internal or external to proportionality doctrine').

omega_variable(
    alternative_constraint_feasibility,
    'Could courts constrain judicial discretion in fundamental-rights cases through alternative methods (bright-line rules, categorical tiers, explicit delegation to legislatures) that are currently foreclosed by the proportionality regime?',
    'Doctrinal experiments: jurisdictions that have attempted to limit proportionality balancing with bright-line rules or categorical restrictions. Empirical outcome: did these alternatives reduce judicial discretion or merely displace it?',
    'If feasible alternatives exist: the choice to use proportionality is contingent; suppression of rule-bound predictability is not forced by the nature of constitutional law. If no feasible alternatives: balancing may be an inescapable feature of rights protection.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_constraint_feasibility, empirical, 'Whether feasible alternatives to proportionality balancing exist for constraining judicial discretion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(proportionality_doctrine__balancing_critique_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prop_bal_crit_tr_t0, proportionality_doctrine__balancing_critique_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(prop_bal_crit_tr_t15, proportionality_doctrine__balancing_critique_reading, theater_ratio, 15, 0.58).
narrative_ontology:measurement(prop_bal_crit_tr_t30, proportionality_doctrine__balancing_critique_reading, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(prop_bal_crit_be_t0, proportionality_doctrine__balancing_critique_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(prop_bal_crit_be_t15, proportionality_doctrine__balancing_critique_reading, base_extractiveness, 15, 0.45).
narrative_ontology:measurement(prop_bal_crit_be_t30, proportionality_doctrine__balancing_critique_reading, base_extractiveness, 30, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(prop_bal_crit_su_t0, proportionality_doctrine__balancing_critique_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(prop_bal_crit_su_t15, proportionality_doctrine__balancing_critique_reading, suppression_requirement, 15, 0.5).
narrative_ontology:measurement(prop_bal_crit_su_t30, proportionality_doctrine__balancing_critique_reading, suppression_requirement, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(proportionality_doctrine__balancing_critique_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(proportionality_doctrine__balancing_critique_reading, 0.12).
narrative_ontology:affects_constraint(proportionality_doctrine__balancing_critique_reading, proportionality_doctrine__global_export_reading).
narrative_ontology:affects_constraint(proportionality_doctrine__balancing_critique_reading, proportionality_doctrine__structured_reason_reading).

% DUAL FORMULATION NOTE:
% The proportionality_doctrine kernel decomposes into three constraint stories corresponding to three live readings of the doctrine. The balancing-critique reading (this story, ε=0.52) identifies proportionality as a tangled rope that coordinates while masking discretion. The global_export_reading (ε likely lower, rope or tangled_rope) treats proportionality as a successful global coordination mechanism. The structured_reason_reading (ε likely lower, rope) treats proportionality as structured transparency that constrains through reason-giving. The three stories have different ε values and different beneficiary/victim structures because they describe different constraints — different stabilized claims about what proportionality does. The kernel itself (the formal doctrine) is fixed; the readings are incommensurable interpretations of its function and legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
