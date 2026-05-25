% ============================================================================
% CONSTRAINT STORY: consent_as_structural_fiction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_consent_as_structural_fiction, []).

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
 *   constraint_id: consent_as_structural_fiction
 *   human_readable: Mandatory Arbitration as Consent Fiction
 *   domain: labor_law/dispute_resolution/corporate_governance
 *
 * SUMMARY:
 *   Mandatory arbitration clauses in employment contracts present a
 *   structural fiction: the legal doctrine of 'consent' applied to a
 *   mechanism that workers cannot meaningfully negotiate. The constraint
 *   operates through the Federal Arbitration Act's enforcement of arbitration
 *   agreements, but the bilateral contract framing obscures the unilateral
 *   imposition. 56.2% of private-sector nonunion workers are bound by
 *   arbitration clauses, rising to 67.7% at firms with 5000+ employees. The
 *   size correlation reveals the constraint's structural logic: large
 *   employers have sufficient bargaining power to impose arbitration
 *   universally, while smaller employers face higher relative costs and
 *   worker resistance. The constraint exhibits genuine coordination function
 *   (dispute resolution is necessary) alongside asymmetric extraction
 *   (workers lose access to courts, class actions, and public precedent). The
 *   theater ratio (0.68) reflects that the 'consent' ritual — signing the
 *   employment contract — is performative: workers sign because employment is
 *   conditioned on acceptance, not because they have evaluated and agreed to
 *   the arbitration terms. The constraint has accumulated extraction over
 *   time as arbitration clauses have expanded from executive contracts to
 *   frontline workers and as class action waivers have been bundled with
 *   arbitration mandates.
 *
 * KEY AGENTS:
 *   - Low-Wage Workers: Primary victim (powerless/trapped) — cannot negotiate contract terms, cannot afford to decline employment, lose access to courts and collective action
 *   - Professional Employees: Secondary victim (moderate/constrained) — have some labor market mobility but face industry-wide arbitration adoption; bear costs but retain some exit options
 *   - Large Employers: Primary beneficiary (institutional/arbitrage) — gain predictable dispute costs, avoid class actions, benefit from repeat-player advantage in arbitration
 *   - Arbitration Providers: Secondary beneficiary (institutional/arbitrage) — profit from employer-paid arbitration volume; career incentives favor employer-friendly outcomes
 *   - Labor Rights Coalition: Organized agents (organized/constrained) — legislative reform efforts (FAIR Act) face employer lobbying; see potential sunset but lack power to force it
 *   - Judicial System: Institutional actor (institutional/constrained) — enforces arbitration clauses under FAA precedent while recognizing the consent fiction; maintains ritual of contract doctrine despite structural reality
 *   - Collective Bargaining Capacity: Abstract victim (powerless/trapped) — class action waivers prevent aggregation of small-value claims; workers cannot organize around shared grievances
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(consent_as_structural_fiction, 0.58).
domain_priors:suppression_score(consent_as_structural_fiction, 0.72).
domain_priors:theater_ratio(consent_as_structural_fiction, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(consent_as_structural_fiction, extractiveness, 0.58).
narrative_ontology:constraint_metric(consent_as_structural_fiction, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(consent_as_structural_fiction, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(consent_as_structural_fiction, tangled_rope).
narrative_ontology:human_readable(consent_as_structural_fiction, "Mandatory Arbitration as Consent Fiction").
narrative_ontology:topic_domain(consent_as_structural_fiction, "labor_law/dispute_resolution/corporate_governance").

domain_priors:requires_active_enforcement(consent_as_structural_fiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(consent_as_structural_fiction, large_employers).
narrative_ontology:constraint_beneficiary(consent_as_structural_fiction, arbitration_providers).
narrative_ontology:constraint_victim(consent_as_structural_fiction, low_wage_workers).
narrative_ontology:constraint_victim(consent_as_structural_fiction, collective_bargaining_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

constraint_indexing:constraint_classification(consent_as_structural_fiction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

constraint_indexing:constraint_classification(consent_as_structural_fiction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

constraint_indexing:constraint_classification(consent_as_structural_fiction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

constraint_indexing:constraint_classification(consent_as_structural_fiction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

constraint_indexing:constraint_classification(consent_as_structural_fiction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

constraint_indexing:constraint_classification(consent_as_structural_fiction, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(consent_as_structural_fiction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(consent_as_structural_fiction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(consent_as_structural_fiction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(consent_as_structural_fiction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(consent_as_structural_fiction, TR),
    TR >= 0.70.

:- end_tests(consent_as_structural_fiction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Workers lose access to courts, public precedent, discovery rights, jury trials, and class actions. Employers gain cost predictability, repeat-player advantage, and insulation from collective claims. The extraction is substantial but not total — arbitration does provide some dispute resolution, and some workers (high-skill professionals) retain enough bargaining power to negotiate terms or exit. The value reflects that the coordination function (dispute resolution) is real but heavily asymmetric. Suppression (0.72): High. Workers face structural barriers to exit: employment is conditioned on arbitration acceptance, industry-wide adoption eliminates alternatives, low-wage workers cannot afford to decline jobs, and judicial enforcement under the FAA forecloses legal challenge. Suppression is not total (0.95+) because some workers have labor market mobility and some employers (small firms, competitive labor markets) face costs to imposing arbitration. Theater ratio (0.68): High. The 'consent' ritual is performative: workers sign arbitration clauses as a condition of employment, not as a result of negotiation or informed agreement. The bilateral contract framing obscures the unilateral imposition. Theater has increased over time as arbitration has expanded from negotiated executive agreements to non-negotiable frontline worker contracts.
 *
 * PERSPECTIVAL GAP:
 *   The low-wage worker sees a snare: mandatory arbitration is pure extraction with no meaningful consent and no exit. The professional employee sees a tangled rope: arbitration provides some dispute resolution benefit but extracts court access and collective action rights; they have some exit options but face industry-wide adoption. The large employer sees a rope: arbitration is a coordination mechanism that solves the problem of unpredictable litigation costs; they are net beneficiaries. The labor rights coalition sees a scaffold: mandatory arbitration is a temporary problem that legislative reform (FAIR Act) will sunset, though the timeline is uncertain and depends on political coalition strength. The judicial system sees a piton: the consent doctrine is a degraded ritual maintained through precedent inertia (FAA enforcement) despite recognition that 'consent' is fictional for most workers. The analytical observer sees a tangled rope: the constraint has genuine coordination function (dispute resolution is necessary) but embeds asymmetric extraction (loss of courts, class actions, public precedent) that the consent framing obscures.
 *
 * DIRECTIONALITY LOGIC:
 *   Large employers are primary beneficiaries with arbitrage exit options — they can choose whether to impose arbitration and can exit to litigation if arbitration becomes unfavorable. Their structural position yields low d (beneficiary + arbitrage) and low or negative effective extraction. Low-wage workers are primary victims with trapped exit options — they cannot negotiate contract terms, cannot afford to decline employment, and face industry-wide arbitration adoption. Their structural position yields high d (victim + trapped) and maximum effective extraction. Professional employees occupy a middle position: they are victims (lose court access) but have constrained rather than trapped exit (some labor market mobility, some negotiation leverage). Organized labor coalitions see a potential sunset (legislative reform) but lack the power to force it, yielding moderate extraction. The judicial system enforces the constraint while recognizing its fictional basis — it is constrained by FAA precedent but could exit through doctrinal revision. The analytical observer sees both the genuine coordination function (dispute resolution) and the asymmetric extraction (loss of collective action, repeat-player bias).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by demonstrating that 'consent' operates at two levels: the legal fiction (workers sign, therefore they consent) and the structural reality (workers sign because employment is conditioned on acceptance). The legal fiction enables the coordination function (dispute resolution) to be framed as bilateral agreement, obscuring the unilateral imposition. The perspectival gap reveals this: employers see coordination (rope), trapped workers see extraction (snare), and the analytical observer sees both (tangled rope). The mandatrophy is not 'is this coordination or extraction?' but 'whose perspective determines the classification?' The constraint's tangled rope classification at the analytical level reflects that both functions are structurally real: arbitration does resolve disputes (coordination) and does extract court access and collective action rights (extraction). The consent fiction is the mechanism that allows the extraction to be presented as coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_threshold_ambiguity,
    'At what level of economic duress does ''consent'' to arbitration become structurally meaningless?',
    'Empirical analysis of worker bargaining power by wage quintile, employment market conditions, and alternative employment availability; comparison of arbitration clause acceptance rates across economic conditions',
    'If threshold is low (workers retain meaningful choice even at low wages): classification shifts toward rope from more perspectives. If threshold is high (consent is fiction for most workers): classification shifts toward snare from more perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_threshold_ambiguity, conceptual, 'Economic duress threshold for meaningful consent').

omega_variable(
    arbitration_outcome_bias,
    'Do arbitration outcomes systematically favor employers beyond what would be expected from case selection effects?',
    'Controlled comparison of similar cases in arbitration vs litigation; analysis of repeat-player effects; examination of arbitrator selection mechanisms and career incentives',
    'If no systematic bias: extractiveness is lower, coordination function is genuine. If strong bias: extractiveness is higher, coordination function is theater.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(arbitration_outcome_bias, empirical, 'Whether arbitration outcomes show systematic employer bias').

omega_variable(
    class_action_waiver_impact,
    'Does the bundling of arbitration clauses with class action waivers constitute a separate extractive mechanism or an inherent feature of the arbitration constraint?',
    'Decomposition analysis: separate constraint stories for arbitration-without-waiver vs arbitration-with-waiver; measurement of extractiveness delta',
    'If separable: two linked constraints with different epsilon values. If inherent: single constraint with higher extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(class_action_waiver_impact, conceptual, 'Whether class action waiver is separable constraint').

omega_variable(
    legislative_sunset_probability,
    'What is the probability that legislative reform (e.g., Forced Arbitration Injustice Repeal Act) will meaningfully constrain mandatory arbitration within a generational timeframe?',
    'Political economy analysis of employer lobbying power, legislative coalition stability, judicial deference patterns; historical analysis of similar labor law reform attempts',
    'If high probability: scaffold classification from organized perspective is justified. If low probability: scaffold is aspirational rather than structural.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legislative_sunset_probability, preference, 'Probability of legislative reform success').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(consent_as_structural_fiction, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(consent_arb_tr_t0, consent_as_structural_fiction, theater_ratio, 0, 0.45).
narrative_ontology:measurement(consent_arb_tr_t10, consent_as_structural_fiction, theater_ratio, 10, 0.58).
narrative_ontology:measurement(consent_arb_tr_t20, consent_as_structural_fiction, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(consent_arb_be_t0, consent_as_structural_fiction, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(consent_arb_be_t10, consent_as_structural_fiction, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(consent_arb_be_t20, consent_as_structural_fiction, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(consent_as_structural_fiction, enforcement_mechanism).
narrative_ontology:affects_constraint(consent_as_structural_fiction, repeat_player_structural_advantage).

% DUAL FORMULATION NOTE:
% The consent fiction is downstream of the repeat-player advantage (employers face the same arbitrators repeatedly; workers do not) but represents a distinct structural constraint. The upstream constraint (repeat player advantage) is a mountain — an inherent feature of any dispute resolution system with institutional repeat players. The downstream constraint (consent as fiction) is a tangled rope — a contingent institutional arrangement that uses the consent doctrine to obscure unilateral imposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(consent_as_structural_fiction, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
