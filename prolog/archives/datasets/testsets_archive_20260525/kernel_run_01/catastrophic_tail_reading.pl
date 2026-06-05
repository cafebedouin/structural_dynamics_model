% ============================================================================
% CONSTRAINT STORY: catastrophic_tail_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophic_tail_reading, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: catastrophic_tail_reading
 *   human_readable: Acceptable Risk Determined by Catastrophic-Tail Dominance
 *   domain: energy_policy/risk_governance/technology_assessment
 *
 * SUMMARY:
 *   The catastrophic-tail reading of acceptable risk in energy governance
 *   argues that rare but high-consequence events with irreversible
 *   consequences should dominate risk assessment, overriding probabilistic
 *   expected-value calculations. This reading prioritizes the experience of
 *   spatially concentrated populations facing land contamination, evacuation,
 *   and permanent displacement — whose tail-event risk is existential and
 *   irreversible — alongside broader public exposure to psychological trauma
 *   and trust erosion from living under potential catastrophic threat. The
 *   reading directly contests the expected-value framework, which aggregates
 *   low-probability × high-consequence events into manageable statistical
 *   averages. The catastrophic-tail reading claims this aggregation falsely
 *   commensures the incommensurable: psychological irreversibility and
 *   spatial concentration create non-additive harm structures where tail
 *   events cannot be balanced against expected benefits. Primary
 *   beneficiaries of this framing are anti-nuclear movements and renewable
 *   energy advocates, whose policy platforms are legitimized by
 *   tail-dominance logic. Primary victims are spatially concentrated
 *   populations with no exit option and distributed psychological publics
 *   bearing anticipatory anxiety. The constraint exhibits high suppression
 *   (68%) because the catastrophic-tail reading competes against
 *   institutional risk frameworks, industry expertise claims, and
 *   cost-of-energy arguments that all rely on expected-value aggregation.
 *
 * KEY AGENTS:
 *   - Spatially Concentrated Populations: Primary victims (powerless/trapped, generational horizon) — residents near nuclear facilities face irreversible land contamination and forced evacuation. No exit option; full bearing of tail consequences.
 *   - Distributed Psychological Publics: Secondary victims (moderate/constrained, biographical horizon) — broader population bearing anticipatory anxiety and trust erosion from catastrophic-event risk. Constrained by energy market dependence.
 *   - Nuclear Energy Industry: Net neutral / contextual beneficiary (institutional/arbitrage, immediate horizon) — benefits from present energy output; forecloses by catastrophic-tail reading; can arbitrage to renewables.
 *   - Renewable Energy and Anti-Nuclear Coalitions: Primary beneficiaries (organized/arbitrage, generational horizon) — legitimized by catastrophic-tail reading; directly mobilize spatial concentration and irreversibility narratives for policy advocacy.
 *   - Regulatory Bodies: Caught between frameworks (powerful/mobile, biographical horizon) — must reconcile expected-value industry expertise with tail-dominance public concern. Constrained by climate urgency and energy cost pressures.
 *   - Analytical Observer: At risk of false summit (analytical/analytical, civilizational horizon) — expected-value framework claims universal/mathematical grounding, masking contestable normative axioms about commensuration of irreversible harms.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophic_tail_reading, 0.58).
domain_priors:suppression_score(catastrophic_tail_reading, 0.68).
domain_priors:theater_ratio(catastrophic_tail_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophic_tail_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(catastrophic_tail_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(catastrophic_tail_reading, theater_ratio, 0.44).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophic_tail_reading, tangled_rope).
narrative_ontology:human_readable(catastrophic_tail_reading, "Acceptable Risk Determined by Catastrophic-Tail Dominance").
narrative_ontology:topic_domain(catastrophic_tail_reading, "energy_policy/risk_governance/technology_assessment").

domain_priors:requires_active_enforcement(catastrophic_tail_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(catastrophic_tail_reading, formalized).
narrative_ontology:cs_authority_grounding(catastrophic_tail_reading, extraction).
narrative_ontology:cs_kernel_id(catastrophic_tail_reading, acceptable_risk_for_energy).
narrative_ontology:cs_reading_relation(catastrophic_tail_reading, expected_value_reading, forecloses).
narrative_ontology:cs_reading_relation(catastrophic_tail_reading, precautionary_reading, coexists_with).
narrative_ontology:cs_axiom(catastrophic_tail_reading, foundational, irreversibility_tail_dominance).
narrative_ontology:cs_axiom_status(irreversibility_tail_dominance, holdable).
narrative_ontology:cs_axiom(catastrophic_tail_reading, foundational, spatial_concentration_moral_weight).
narrative_ontology:cs_axiom_status(spatial_concentration_moral_weight, holdable).
narrative_ontology:cs_reference_frame(catastrophic_tail_reading, tail_event_irreversibility_baseline).
narrative_ontology:cs_drift_state(catastrophic_tail_reading, post_fukushima_era, gap(axiom_overriding, substantial, true)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophic_tail_reading, renewable_energy_advocates).
narrative_ontology:constraint_beneficiary(catastrophic_tail_reading, anti_nuclear_movements).
narrative_ontology:constraint_beneficiary(catastrophic_tail_reading, precautionary_policy_communities).
narrative_ontology:constraint_victim(catastrophic_tail_reading, spatially_concentrated_populations).
narrative_ontology:constraint_victim(catastrophic_tail_reading, psychologically_traumatized_publics).
narrative_ontology:constraint_victim(catastrophic_tail_reading, long_tail_consequence_bearers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SPATIALLY CONCENTRATED VICTIMS (SNARE) — Residents within 50km of nuclear facilities face irreversible land contamination, evacuation, and loss of livelihood. Geographic immobility (property, community ties, economic infrastructure) creates structural trap. No viable exit option; maximum experienced extraction. The catastrophic-tail reading weights their burden heavily — their trauma and displacement are the primary concern, not statistical averaging.
constraint_indexing:constraint_classification(catastrophic_tail_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: DISTRIBUTED PSYCHOLOGICAL TRAUMA BEARERS (TANGLED ROPE) — Wider population bears low-probability but high-consequence psychological disturbance risk: living under uncertainty of potential catastrophic event, anticipatory anxiety, trust erosion in institutional safety claims. Benefits from energy supply continuity; constrained by difficulty of physically relocating or accessing alternative energy markets. The catastrophic-tail reading treats psychological irreversibility as a victim cost comparable to material damage.
constraint_indexing:constraint_classification(catastrophic_tail_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: NUCLEAR INDUSTRY (ROPE) — Experiences the constraint as coordination: communicating safety via risk quantification (expected value framing) enables continued operation. Benefits from present energy output and reduced climate impact relative to fossil fuels. Under catastrophic-tail reading, their risk assessment framework is delegitimized — tail dominance argument forecloses their expected-value case. High arbitrage capacity: can shift investment to renewable alternatives, though costlier.
constraint_indexing:constraint_classification(catastrophic_tail_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORS / PLANNERS (TANGLED ROPE) — Face coordination problem (safety assessment methodology) alongside extraction pressure from both industry (to approve projects) and public (to prevent accidents). Mobile in principle (can adopt alternative regulatory frameworks); constrained by political economy of energy costs and climate urgency. The catastrophic-tail reading constrains their decision space: tail-dominance logic restricts project approval to near-zero tail-event probability thresholds.
constraint_indexing:constraint_classification(catastrophic_tail_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ANTI-NUCLEAR / RENEWABLE COALITIONS (ROPE) — Primary beneficiaries of catastrophic-tail framing. The reading legitimizes their policy platform: tail-dominance logic forecloses nuclear energy and accelerates renewable investment. Experiences the constraint as pure coordination — organizing public concern around irreversibility and trauma narratives aligns dispersed victims into coherent policy bloc. High arbitrage: can shift between anti-nuclear position and climate mitigation strategies.
constraint_indexing:constraint_classification(catastrophic_tail_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / EXPECTED-VALUE FRAME (MOUNTAIN) — From a universal/civilizational perspective, risk assessment via expected value (probability × consequence) is a timeless principle of rational decision-making. Catastrophic-tail dominance is mathematically reducible to expectation under fat-tailed distributions — an immutable logical consequence of probability theory, not a reading-dependent framing. This perspective claims to transcend the kernel dispute. However, the analytical observer is captured by an unstated axiom: that expected-value aggregation is the appropriate commensuration metric for irreversible harms. The engine flags this as a false summit — the 'universal' mathematics naturalizes a contestable normative claim about how to weigh tail events.
constraint_indexing:constraint_classification(catastrophic_tail_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophic_tail_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(catastrophic_tail_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(catastrophic_tail_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophic_tail_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophic_tail_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The catastrophic-tail reading creates extraction pressure on spatially concentrated populations (trapped exit, generational irreversibility) and distributed psychological publics (constrained exit, biographical anticipatory burden). The extraction is not maximal (snare-level) because beneficiary populations (renewable advocates) do not directly extract material resources — they gain policy legitimacy and market advantage indirectly through risk reframing. The reading itself is an enforcement mechanism: it legitimizes exclusion of high-tail-probability technologies from deployment zones where tail events would be irreversible. Suppression (0.68): High. The catastrophic-tail reading suppresses competing risk framings through: (1) irreversibility as an undeniable moral factor (hard to argue tail consequences are reversible), (2) emotional and narrative resonance of spatial concentration (Chernobyl, Fukushima, Three Mile Island imagery dominates public memory), (3) psychological trauma as newly recognized victim category (expanding harm typology beyond material damage). However, suppression is not maximal (snare-level ≥0.60) because expected-value framing retains institutional authority through regulatory expertise and cost-analysis. Theater ratio (0.44): Moderate-low. The catastrophic-tail reading has relatively low theater because the causal mechanism is straightforward — irreversibility and spatial concentration are direct, observable properties. Risk assessments under this reading require concrete impact modeling (evacuation zones, contamination trajectories) rather than performative ritual. However, some theater persists because psychological trauma quantification and 'irreversibility' measurement involve interpretive choices and contestable thresholds.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between statistical and spatial framings of risk. Expected-value reading sees risk as a scalar property of activity (low probability × high consequence = manageable average); catastrophic-tail reading sees risk as a spatial phenomenon with concentration effects (same probability × consequence is catastrophic if concentrated locally, reversible if distributed globally). The spatial-concentration perspective (concentrated populations, local scope) classifies as snare under tail-dominance logic. The same risk under expected-value logic would classify as tangled rope or even rope (small aggregated expected harm relative to energy benefits). The beneficiary-perspective gap is also sharp: nuclear industry sees rope (communication/coordination of safety); renewable advocates see snare being revealed (tail-dominance reading unmasking hidden extraction). The analytical observer's mountain classification is a false summit — the 'universal' expected-value mathematics is a readable instantiation of one normative axiom about how to commensure irreversible harms.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position within the constraint. Spatially concentrated populations face maximal tail-event consequences (d ≈ 0.95, full target status); distributed psychological publics face secondary consequences (d ≈ 0.72, high-target status); nuclear industry faces delegitimation of expected-value framework (d ≈ 0.15, contextual beneficiary status); renewable advocates gain policy authority (d ≈ 0.08, clear beneficiary status); regulators face decision-making pressure from both sides (d ≈ 0.58, symmetric constraint); analytical expected-value observer is captured by unstated aggregation axiom (d ≈ 0.72 but flag as false summit — the mathematics naturalizes normative choice).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by clarifying that the dispute is not empirical (probability calculations differ negligibly across readings) but normative (how to weight irreversible tail consequences). The catastrophic-tail reading explicitly subordinates expected-value aggregation to spatial-concentration and irreversibility factors — it does not claim to produce lower probability estimates, but rather that low-probability × high-consequence structure is unsuitable for tail-dominated risks. The mandatrophy is: 'Can we use a single risk metric across all energy technologies, or must we choose a reading?' The catastrophic-tail reading's answer is that irreversible tail consequences create a distinct risk category (tangled_rope/snare by this reading) that cannot be aggregated with reversible risks via expected value. No single metric suffices; the reading choice determines the classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    irreversibility_metric_definition,
    'What operational definition distinguishes reversible vs irreversible consequences for risk dominance calculations?',
    'Temporal analysis: track recovery trajectories for Fukushima, Chernobyl, Three Mile Island populations. Distinguish physical remediation from psychological normalization. Quantify irreversibility as recovery half-life > 50 years or permanent exclusion zones.',
    'If broad irreversibility definition: many energy infrastructure risks qualify for tail dominance. If narrow (only physical contamination): psychological trauma and social fragmentation downweighted, expected-value framing regains authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(irreversibility_metric_definition, empirical, 'Operational definition of irreversibility for tail-event weighting').

omega_variable(
    spatial_concentration_extraction_coupling,
    'Does spatial concentration of tail-event consequences constitute extraction, or is it a morally relevant distributional property orthogonal to the risk assessment framework?',
    'Counterfactual analysis: if same probabilistic risk were distributed uniformly across global population (e.g., 0.001% per-capita long-tail death risk everywhere), would catastrophic-tail reading classify the constraint differently? If yes: extraction is coupled to spatial concentration. If no: tail dominance is frame-independent.',
    'If coupling is real: geographic proximity to hazard is an extraction mechanism. If distribution-invariant: tail dominance logic applies equally to dispersed and concentrated risks — catastrophic-tail reading is about event probability/consequence structure, not equity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(spatial_concentration_extraction_coupling, conceptual, 'Whether spatial concentration drives tail-dominance extraction or is orthogonal to it').

omega_variable(
    psychological_irreversibility_commensurability,
    'Are psychological trauma and anticipatory anxiety commensurable with physical harm for tail-dominance calculations? Can trauma be weighted equally to displacement, contamination, or mortality?',
    'Longitudinal psychological studies post-accident: PTSD prevalence, anxiety persistence, trust-erosion trajectories. Compare psychological burden trajectories to physical remediation timelines. If trauma recovery is faster than physical cleanup, psychological irreversibility claim is weakened.',
    'If trauma is fully commensurate: catastrophic-tail reading''s victim count expands to include broader psychological publics. If incommensurable or secondary: primary victims are spatially concentrated populations only — extractiveness drops to ~0.42.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(psychological_irreversibility_commensurability, empirical, 'Commensurateness of psychological trauma to material consequences in tail-dominance weighting').

omega_variable(
    kernel_reading_contest,
    'Is ''acceptable risk'' determined by probabilistic outcomes (expected value), tail-event dominance, or precautionary principles? Are these three readings of the same normative kernel, or fundamentally incommensurable decision paradigms?',
    'Meta-analysis of regulatory frameworks globally: which jurisdictions adopt which reading; whether jurisdictions have coherent risk axioms or eclectic borrowing across readings. Identify whether readings coexist within single regulatory authority or sort by geography/ideology.',
    'If readings are incommensurable paradigms (forecloses relation): the kernel dispute is irresolvable within a single framework — risk governance requires choosing a reading. If coexisting (coexists_with relation): jurisdictional pluralism allows different communities to adopt different readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether catastrophic-tail, expected-value, and precautionary readings are commensurate or foreclosing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophic_tail_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(catail_tr_t0, catastrophic_tail_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(catail_tr_t5, catastrophic_tail_reading, theater_ratio, 5, 0.41).
narrative_ontology:measurement(catail_tr_t10, catastrophic_tail_reading, theater_ratio, 10, 0.44).

% Extraction over time
narrative_ontology:measurement(catail_be_t0, catastrophic_tail_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(catail_be_t5, catastrophic_tail_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(catail_be_t10, catastrophic_tail_reading, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophic_tail_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(catastrophic_tail_reading, expected_value_reading).
narrative_ontology:affects_constraint(catastrophic_tail_reading, precautionary_reading).

% DUAL FORMULATION NOTE:
% The 'acceptable risk for energy' kernel decomposes into three structurally distinct constraints: (1) catastrophic_tail_reading — privileges irreversibility and spatial concentration; (2) expected_value_reading — uses probabilistic aggregation; (3) precautionary_reading — inverts burden of proof. Each reading instantiates a different ε and different beneficiary/victim structure. All three are network-linked because they are interpretive variants of the same kernel commitment. The network links represent interpretive influence, not causal dependency — adopting the catastrophic-tail reading does not cause the expected-value reading to cease existing, but it forecloses its authority within a single jurisdiction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophic_tail_reading, institutional, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
