% ============================================================================
% CONSTRAINT STORY: npat_regime_delegitimization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npat_regime_delegitimization, []).

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
 *   constraint_id: npat_regime_delegitimization
 *   human_readable: NPAT Regime Delegitimization and Narrative Collapse
 *   domain: institutional/epistemic
 *
 * SUMMARY:
 *   NPAT regime delegitimization describes the constraint created when an
 *   epistemic regime (a set of normative standards, evidence hierarchies, and
 *   institutional structures) undergoes a process of narrative and
 *   institutional collapse. The NPAT regime—whatever specific normative
 *   framework it denotes—becomes the target of coordinated delegitimization
 *   attacks that destabilize its foundational consensus. This constraint
 *   exhibits the defining feature of Snare-type extraction: it creates severe
 *   asymmetric costs (loss of epistemic stability, institutional disorder,
 *   successor regime formation paralysis) while providing concentrated
 *   benefits to regime incumbents and challengers who control the
 *   delegitimization narrative. The constraint is enforced through a
 *   combination of narrative dominance, institutional gatekeeping, and
 *   suppression of alternative epistemic frameworks or regime-recovery
 *   strategies. The theater ratio (0.65) indicates substantial performative
 *   content: many delegitimization attacks are repetitions of prior
 *   critiques, rhetorical escalations that do not accumulate into specific
 *   falsifiable claims, and institutional theater where criticism flows but
 *   regime structures persist unchanged. The extraction intensity (0.68) is
 *   high because the delegitimization process prevents normal regime function
 *   and successor regime formation simultaneously, creating a stability
 *   vacuum with no coordinated exit.
 *
 * KEY AGENTS:
 *   - Epistemic Commons: Victim (powerless/trapped) — abstract collective knowledge base that has no representative and cannot organize defense; bears full cost of destabilization
 *   - Successor Regime Architects: Victim (powerless/trapped) — agents attempting to build post-NPAT institutional structures inherit a delegitimized epistemic commons with no foundation for consensus
 *   - Regime Incumbent Narrativists: Beneficiary (institutional/arbitrage) — control the delegitimization narrative and mobilize tribal identity; experience the constraint as beneficial coordination of regime defense
 *   - Institutional Actors (Universities, Agencies): Mixed (moderate/constrained) — depend on regime stability for their own institutional function but cannot unilaterally save the regime
 *   - Delegitimization Theater: Institutional actor (institutional/arbitrage) — the ritual of criticism itself has become an institution that persists independently of regime collapse
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing regime mortality and delegitimization cycles as immutable laws of institutional evolution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npat_regime_delegitimization, 0.68).
domain_priors:suppression_score(npat_regime_delegitimization, 0.72).
domain_priors:theater_ratio(npat_regime_delegitimization, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npat_regime_delegitimization, extractiveness, 0.68).
narrative_ontology:constraint_metric(npat_regime_delegitimization, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(npat_regime_delegitimization, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npat_regime_delegitimization, snare).
narrative_ontology:human_readable(npat_regime_delegitimization, "NPAT Regime Delegitimization and Narrative Collapse").
narrative_ontology:topic_domain(npat_regime_delegitimization, "institutional/epistemic").

domain_priors:requires_active_enforcement(npat_regime_delegitimization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npat_regime_delegitimization, regime_incumbent_narrativists).
narrative_ontology:constraint_victim(npat_regime_delegitimization, epistemic_commons).
narrative_ontology:constraint_victim(npat_regime_delegitimization, successor_regime_architects).
narrative_ontology:constraint_victim(npat_regime_delegitimization, reality_grounding_mechanisms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EPISTEMIC COMMONS (SNARE) — The collective knowledge base has no agent, no coordinator, and no exit. As NPAT regime delegitimization proceeds, the commons cannot defend itself. It bears maximum extraction: prior consensus frameworks are declared incoherent retroactively, evidence hierarchies are destabilized, and the commons has no mechanism to slow or stop the process. Trapped in the delegitimization cycle.
constraint_indexing:constraint_classification(npat_regime_delegitimization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SUCCESSOR REGIME ARCHITECTS (SNARE) — Agents who would build post-NPAT institutional structures are trapped in a paradox: the delegitimization process prevents them from establishing sufficient stability or credibility to propose alternatives. The NPAT regime's collapse delegitimizes not just itself but the very concept of epistemic regimes and normative standards. Successor builders inherit a devastated commons with no foundation.
constraint_indexing:constraint_classification(npat_regime_delegitimization, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: REGIME INCUMBENT NARRATIVISTS (ROPE) — Those invested in the NPAT regime's continuation experience the constraint as beneficial coordination. Delegitimization attacks create temporary solidarity, mobilize tribal identity, and generate narrative resources that reinforce institutional control. The incumbents experience this as protecting their epistemic commons, not attacking it. They have full arbitrage options: exit would mean losing narrative control.
constraint_indexing:constraint_classification(npat_regime_delegitimization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INSTITUTIONAL ACTORS DEPENDENT ON REGIME STABILITY (TANGLED ROPE) — Universities, research agencies, professional societies have genuine coordination functions enabled by NPAT frameworks (curricula, hiring standards, publication criteria). But they also benefit from the regime's inertia and face extraction when delegitimization destabilizes their institutional bases. Cannot exit without enormous reorganization cost, but experience both benefit and cost.
constraint_indexing:constraint_classification(npat_regime_delegitimization, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: DELEGITIMIZATION THEATER (PITON) — The rituals of regime attack (exposés, counter-narratives, methodological critiques) have become substantially performative. The theater persists through institutional inertia: criticism flows but the regime persists; delegitimization arguments are restated without accumulation or resolution. Theater ratio rises as the delegitimization cycle institutionalizes itself as a permanent feature rather than a transition process.
constraint_indexing:constraint_classification(npat_regime_delegitimization, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / REGIME MORTALITY VIEW (MOUNTAIN) — From a civilizational/universal perspective, all epistemic regimes are mortal. NPAT will eventually collapse or transform, and the delegitimization process is part of the natural succession cycle. This perspective treats regime delegitimization as an immutable feature of institutional evolution. However, this naturalizes the specific extraction mechanisms and suppression of alternatives as if they were laws of nature rather than contingent power dynamics.
constraint_indexing:constraint_classification(npat_regime_delegitimization, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npat_regime_delegitimization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(npat_regime_delegitimization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(npat_regime_delegitimization, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(npat_regime_delegitimization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(npat_regime_delegitimization, TR),
    TR >= 0.70.

:- end_tests(npat_regime_delegitimization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and rising. The delegitimization process imposes concentrated costs on the epistemic commons (loss of framework coherence, destabilization of evidence hierarchies, institutional disorder) while distributing benefits unevenly to regime incumbents (narrative control, tribal mobilization, preservation of authority despite instability). The measurement trajectory shows extractiveness rising from 0.32 to 0.68 over the observation interval, indicating that the initial delegitimization phase (substantive critique, attempt at regime reform) has escalated into high-extraction pathology where the delegitimization itself becomes the primary institutional mechanism. Suppression (0.72): Very high. Multiple suppression mechanisms operate simultaneously: (1) Narrative dominance by delegitimization actors prevents alternative framings (regime-recovery proposals, regime-adaptation strategies) from gaining traction. (2) Institutional gatekeeping by regime challengers prevents regime incumbents from controlling the delegitimization narrative or defending the regime's coherence. (3) Epistemic suppression: the delegitimization process destabilizes the very standards by which regime-recovery proposals could be evaluated, creating a catch-22 where successor frameworks inherit the same delegitimization pressures. Theater ratio (0.65): Moderate-high. The delegitimization theater consists of repetitive critiques (often valid in isolation but presented as novelty), rhetorical escalations (moving from specific falsifiable claims to incoherence/vagueness rhetoric), and institutional performances (public denunciations, institutional position-taking, loyalty signals) that do not accumulate into regime modification or successor regime formation. The theater rises over the interval as delegitimization becomes institutionalized as a permanent feature of regime discourse rather than a transition process.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates extreme perspectival divergence. The regime incumbent narrativists experience delegitimization as a coordination mechanism — they are defending the NPAT epistemic commons against incoherent challengers, mobilizing tribal solidarity, and preventing institutional chaos. They see Rope: solving the problem of consensus maintenance through narrative unity. The successor regime architects experience the opposite: delegitimization prevents them from establishing any credible framework because the delegitimization process has destabilized the very concept of epistemic regimes. They see Snare: trapped in paradox with no exit. The epistemic commons (abstract victim with no agent) experiences pure extraction: loss of framework coherence, destabilization of evidence standards, institutional order collapse. The institutional actors (universities, agencies) experience mixed costs and benefits: they depend on regime stability for their function but also benefit from positioning themselves as leading the regime transition. They see Tangled Rope: genuine coordination functions alongside extraction. The delegitimization theater itself experiences the constraint as beneficial piton: the ritual of criticism persists indefinitely without resolving into regime change or regime recovery. The analytical observer's mountain perspective risks naturalizing all of this as immutable institutional evolution — 'all regimes eventually delegitimize and collapse' — which naturalizes what are actually contingent power dynamics and suppression mechanisms.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective reflects the agent's structural position relative to the extraction flow. The epistemic commons is a victim without power or exit: d=0.95 (near-complete target status), f(d)≈1.42 (maximum f). Successor architects are victims with trapped exit: d=0.92 (near-complete target), f(d)≈1.38. Regime incumbents are beneficiaries with arbitrage exit: d=0.08 (near-complete beneficiary), f(d)≈-0.08 (negative f, flows toward them). Institutional actors dependent on regime stability are mixed (both benefits and costs): d=0.50 (symmetric), f(d)≈0.65 (moderate positive). The delegitimization theater itself is an institutional beneficiary: d=0.12, f(d)≈0.01. Scope modifier σ(S)=1.2 (global scope) amplifies extractiveness across all perspectives. The perspectival gaps are large: beneficiaries experience this as coordination (defending a commons); victims experience it as pure extraction (destabilization with no alternative). The analytical observer risks seeing this as an immutable law (regime mortality) when it is actually contingent on suppression of regime-recovery alternatives.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC: This constraint resolves mandatrophy by showing how the same structural mechanism (delegitimization process) can appear as pure extraction (Snare from victims), as coordination (Rope from beneficiaries), and as natural law (Mountain from analysts). The mandatrophy is not 'which type is correct?' but 'what does the extraction-to-coordination ratio reveal about the delegitimization mechanism's actual function?' If delegitimization were pure coordination (Rope), beneficiaries and victims would experience it similarly — they would both see the process as solving a collective epistemic problem. But they do not: beneficiaries see coordination and tribe-building; victims see destabilization and regime collapse. This perspectival gap indicates asymmetric extraction, not pure coordination. If delegitimization were pure extraction (Snare), it would require total suppression of regime-recovery alternatives. But some institutional actors retain partial agency (Tangled Rope from their perspective) and some beneficiaries experience it as Rope rather than Snare. This mixed pattern indicates that delegitimization is not pure extraction but a hybrid where the extraction is real but partial. The analytical observer's Mountain classification is a false summit: regime mortality is real, but the specific delegitimization mechanism's extraction intensity (0.68) is not an immutable law of nature — it is contingent on suppression (0.72) of regime-recovery alternatives and narrative dominance by delegitimization actors. The Snare classification is justified by the measurement trajectory (extractiveness rising 0.32→0.68) and the high suppression, but is not a natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    delegitimization_vector_intentionality,
    'Is the NPAT regime delegitimization primarily driven by articulated critiques of the regime''s actual failures, or is it primarily a power struggle where delegitimization serves as a proxy mechanism for regime displacement?',
    'Content analysis of delegitimization arguments: track whether critiques identify specific falsifiable claims vs invoke incoherence/vagueness as rhetorical strategy. Cross-reference with outcomes: do delegitimization campaigns lead to regime modification addressing critiques, or do they lead to replacement by structurally similar regimes?',
    'If critiques are substantive: NPAT regime delegitimization is a coordination problem (Rope). If primarily proxy power struggle: delegitimization is a Snare mechanism. The impact on successor regime stability differs sharply: substantive critique enables learning; power proxy delegitimization creates instability cascade.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(delegitimization_vector_intentionality, empirical, 'Whether delegitimization vectors are substantive critique or power proxy mechanisms').

omega_variable(
    suppression_of_regime_maintenance_alternatives,
    'Can the NPAT regime defend itself through adaptation without triggering delegitimization? What specific adaptation moves would be perceived as regime-internal evolution vs regime betrayal?',
    'Historical case study of previous regime transitions: identify which modification strategies were perceived as salvage vs betrayal. Current regime: monitor what range of adaptations are publicly permitted before delegitimization escalates.',
    'If adaptation range is wide: suppression is moderate (constrained exit exists). If adaptation range is narrow: suppression is high (any move toward survival is delegitimized as proof of regime''s bankruptcy). Affects classification of victim agents'' exit options.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_of_regime_maintenance_alternatives, empirical, 'Width of adaptation space before regime change is labeled betrayal').

omega_variable(
    successor_regime_credibility_paradox,
    'Can a post-NPAT epistemic regime establish sufficient credibility and normative force given that the delegitimization process has destabilized the concept of epistemic regimes themselves?',
    'Prospective: compare proposals for post-NPAT institutional arrangements against NPAT-era regime specifications. Retrospective (after transition): measure how long the successor regime sustains consensus before facing delegitimization attacks similar in structure to current NPAT attacks.',
    'If successor regimes can establish credibility: delegitimization is transitional (Scaffold). If successor regimes inherit all delegitimization pressures and collapse equally fast: delegitimization is structural (Snare). This determines whether the extraction is regime-specific or system-wide.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(successor_regime_credibility_paradox, empirical, 'Whether successor regimes can establish post-delegitimization credibility').

omega_variable(
    theater_ratio_threshold_for_regime_collapse,
    'What theater ratio threshold (performative delegitimization / substantive regime function) triggers actual regime collapse vs indefinite stasis?',
    'Historical survey of regime transitions: identify theater ratios at collapse point for past epistemic regimes. Current NPAT: track theater ratio progression; model tipping points from institutional dynamics literature.',
    'If threshold > 0.75: NPAT may persist indefinitely as a piton despite delegitimization. If threshold < 0.60: rapid collapse likely. Affects timeline for successor regime formation and extraction intensity along the generational horizon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_threshold_for_regime_collapse, empirical, 'Theater ratio threshold for regime collapse').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npat_regime_delegitimization, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npat_tr_t0, npat_regime_delegitimization, theater_ratio, 0, 0.38).
narrative_ontology:measurement(npat_tr_t3, npat_regime_delegitimization, theater_ratio, 3, 0.52).
narrative_ontology:measurement(npat_tr_t6, npat_regime_delegitimization, theater_ratio, 6, 0.61).
narrative_ontology:measurement(npat_tr_t9, npat_regime_delegitimization, theater_ratio, 9, 0.65).

% Extraction over time
narrative_ontology:measurement(npat_be_t0, npat_regime_delegitimization, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(npat_be_t3, npat_regime_delegitimization, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(npat_be_t6, npat_regime_delegitimization, base_extractiveness, 6, 0.62).
narrative_ontology:measurement(npat_be_t9, npat_regime_delegitimization, base_extractiveness, 9, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npat_regime_delegitimization, identity_coordination).
narrative_ontology:affects_constraint(npat_regime_delegitimization, epistemic_regime_succession).
narrative_ontology:affects_constraint(npat_regime_delegitimization, normative_standard_instability).

% DUAL FORMULATION NOTE:
% NPAT regime delegitimization is downstream of specific challenges to the regime's foundational claims (constraint: normative_standard_instability) but represents a distinct structural constraint on regime succession. The delegitimization process itself becomes an institutional mechanism with its own extractiveness and suppression levels, independent of whether the original critiques were valid.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(npat_regime_delegitimization, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
