% ============================================================================
% CONSTRAINT STORY: optimization_under_uncertainty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_optimization_under_uncertainty, []).

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
 *   constraint_id: optimization_under_uncertainty
 *   human_readable: Optimization Under Uncertainty: Extraction Through Deferral
 *   domain: decision_theory/epistemology/institutional_governance
 *
 * SUMMARY:
 *   Optimization under uncertainty represents a foundational tension in
 *   decision-making: the desire for rational, welfare-maximizing choices
 *   collides with irreducible epistemic limits on knowledge about future
 *   states. Institutional actors—governments, corporations, regulatory
 *   bodies, international organizations—invoke this tension as justification
 *   for centralizing decision-making authority in expert hands. The framing
 *   is intuitive: if the future is unknowable, technical expertise in
 *   modeling uncertainty and navigating complexity must reside with
 *   credentialed specialists. However, this constraint exhibits classic
 *   tangled-rope structure: genuine coordination problem (uncertainty does
 *   complicate decisions) paired with asymmetric extraction (centralized
 *   authority captures decision rents during the deferral window). The
 *   measurement trajectory shows extractiveness and theater ratio both rising
 *   over time—as institutional complexity increases and information
 *   asymmetries deepen, the apparatus of expert-led optimization becomes
 *   increasingly performative while maintaining its legitimacy through
 *   uncertainty claims. The suppression dimension reflects how centralized
 *   authority delegitimizes distributed decision-making (framing it as
 *   insufficiently expert) and restricts information flows (keeping
 *   uncertainty metrics, models, and decision criteria opaque to the public).
 *   At the analytical level, the constraint risks appearing as immutable
 *   epistemology (mountain)—'uncertainty is real, expertise is necessary'—but
 *   the structural evidence of beneficiaries and victims reveals this as a
 *   false summit: a contingent institutional choice naturalized as scientific
 *   truth.
 *
 * KEY AGENTS:
 *   - Centralized Authority (Expert Class): Primary beneficiary (institutional/arbitrage) — captures decision rents, maintains autonomy, avoids distributed accountability during deferral window
 *   - Affected Populations: Primary victim (powerless/trapped) — excluded from decisions affecting their lives, bear costs of deferred accountability, lack mechanisms to challenge expert claims
 *   - Distributed Decision-Makers (Local Authority): Secondary victim (moderate/constrained) — nominally retain authority but face structural pressure to defer; career and legitimacy costs for challenging expert consensus
 *   - Institutional Reformers: Secondary actor (powerful/mobile) — seek to distribute decision-making while managing uncertainty; can reshape but not exit the system
 *   - Technocratic Institutional Framework: Institutional structure (institutional/constrained) — persists through inertia; exhibits high theater as quantified metrics perform legitimacy for pre-made decisions
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional choice as irreducible epistemology
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(optimization_under_uncertainty, 0.58).
domain_priors:suppression_score(optimization_under_uncertainty, 0.65).
domain_priors:theater_ratio(optimization_under_uncertainty, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(optimization_under_uncertainty, extractiveness, 0.58).
narrative_ontology:constraint_metric(optimization_under_uncertainty, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(optimization_under_uncertainty, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(optimization_under_uncertainty, tangled_rope).
narrative_ontology:human_readable(optimization_under_uncertainty, "Optimization Under Uncertainty: Extraction Through Deferral").
narrative_ontology:topic_domain(optimization_under_uncertainty, "decision_theory/epistemology/institutional_governance").

domain_priors:requires_active_enforcement(optimization_under_uncertainty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(optimization_under_uncertainty, centralized_authority).
narrative_ontology:constraint_beneficiary(optimization_under_uncertainty, expert_class).
narrative_ontology:constraint_victim(optimization_under_uncertainty, distributed_decision_makers).
narrative_ontology:constraint_victim(optimization_under_uncertainty, affected_populations).
narrative_ontology:constraint_victim(optimization_under_uncertainty, epistemic_transparency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: AFFECTED POPULATION (SNARE) — Structurally excluded from decision-making under the justification that uncertainty requires expert authority. Cannot organize countervailing expertise. Bears full cost of deferred outcomes while decision authority accumulates benefits. High suppression through information asymmetry and delegitimation of lay decision-making capacity. No meaningful exit.
constraint_indexing:constraint_classification(optimization_under_uncertainty, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DISTRIBUTED DECISION-MAKERS / LOCAL AUTHORITY (SNARE) — Nominally retain decision-making power but face high barriers to exercise it: claims of insufficient expertise, centralized resource control, institutional pressure toward deference. Can theoretically exit centralized structures but face severe career and legitimacy costs. Suppression operates through expertise framing and resource dependency.
constraint_indexing:constraint_classification(optimization_under_uncertainty, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CENTRALIZED AUTHORITY / EXPERT CLASS (ROPE) — Benefits from uncertainty-justified consolidation. Experiences the constraint as a legitimate coordination problem: uncertainty does require expertise to navigate. Low perceived extraction because the beneficiaries see genuine function in their role. Arbitrage exit: can defer to markets, move between institutions, or shift problem definitions.
constraint_indexing:constraint_classification(optimization_under_uncertainty, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INSTITUTIONAL REFORMERS / PROCEDURAL DEMOCRACY (TANGLED ROPE) — Seek to distribute decision-making authority while acknowledging genuine uncertainty. See coordination benefit (distributed expertise does capture local knowledge) alongside extraction (centralized authority captures rents during deferral window). Mobilization capacity constrains exit; cannot simply leave the system but can reshape it.
constraint_indexing:constraint_classification(optimization_under_uncertainty, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: TECHNOCRATIC INSTITUTIONAL FRAMEWORK (PITON) — The apparatus of expert-led decision-making persists through institutional inertia despite degraded coordination function. Theater ratio high: quantified uncertainty metrics (confidence intervals, risk matrices, scenario planning) appear to solve the epistemic problem but mostly perform legitimacy for decisions already made by centralized authority. Alternatives (participatory decision-making, deliberative forums) remain marginalized despite evidence of local knowledge benefits.
constraint_indexing:constraint_classification(optimization_under_uncertainty, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FUNDAMENTAL EPISTEMOLOGY (MOUNTAIN) — From a universal perspective, uncertainty about future states is an irreducible feature of decision-making — no institutional arrangement can fully resolve it. Some form of deferral to expertise is inevitable. However, the structural data reveals this as a false summit: the claim that 'uncertainty requires centralized expertise' naturalizes a choice about WHERE expertise sits and WHO exercises authority over deferral. Distributed systems can also manage uncertainty through iteration, feedback, and local knowledge.
constraint_indexing:constraint_classification(optimization_under_uncertainty, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(optimization_under_uncertainty_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(optimization_under_uncertainty, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(optimization_under_uncertainty, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(optimization_under_uncertainty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(optimization_under_uncertainty, TR),
    TR >= 0.70.

:- end_tests(optimization_under_uncertainty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The measurement trajectory (0.35 → 0.58 over 20 years) reflects accumulating rents to centralized authority during deferral windows. The base value of 0.58 captures that optimization-under-uncertainty creates genuine asymmetry: the expert class benefits from decision authority, information control, and delayed accountability, while affected populations bear outcome costs without proportional decision input. This is not the maximal extraction of a pure snare (0.72+) because coordination function is genuine—uncertainty does require specialized modeling and risk assessment. The extraction is embedded in a legitimate coordination structure, not layered atop it. Suppression (0.65): High. Multiple mechanisms suppress distributed decision-making: (1) expertise framing—claims that decisions require credentialed specialists; (2) information asymmetry—uncertainty metrics and decision models remain opaque to publics; (3) legitimacy denial—distributed decision-making framed as reckless or uninformed; (4) institutional structure—resources and authority concentrated at center. The measurement trajectory shows suppression increasing from 0.48 to 0.65 as institutional complexity deepens and information gaps widen. Theater ratio (0.68): High. Quantified uncertainty metrics (confidence intervals, risk matrices, scenario planning, cost-benefit analyses) appear to solve the epistemic problem scientifically but perform legitimacy for decisions already constrained by institutional priorities. The apparatus has become substantially more performative over time as technical complexity has increased—more sophisticated models are deployed to justify the same centralized structures.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap lies between the beneficiary (institutional authority) and victims (affected populations, distributed decision-makers). The institutional beneficiary sees rope: uncertainty genuinely requires centralized expertise, and their decision authority is solving a legitimate coordination problem. The affected population sees snare: they are excluded from decisions affecting their lives and bear outcome costs without input or accountability. Local authorities see snare: they face pressure to defer to central expertise despite possessing relevant local knowledge. Institutional reformers see tangled rope: centralization has coordination benefits (aggregated expertise) but embedded extraction (concentrated rents and deferred accountability). The technocratic apparatus sees itself as a piton: expert decision-making persists through institutional continuity, but its functional role has degraded—quantified metrics perform legitimacy more than they enable better decisions. The analytical observer risks seeing a mountain (uncertainty is inherent, expertise is necessary) but structural analysis reveals this as a false summit: the choice to centralize decision authority is institutional, not epistemic.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position and exit capacity. Centralized authority benefits from optimization-under-uncertainty framing and experiences low effective extraction chi (beneficiary status + arbitrage exit). Affected populations face structural exclusion from decision-making (victim status) combined with inability to exit centralized systems (trapped exit) → high d → high f(d) → maximal experienced extraction chi. Distributed decision-makers have nominal authority but face suppression (victim status) and significant barriers to using it (constrained exit) → high d but less extreme than trapped → high but slightly lower chi than affected populations. Institutional reformers have power and mobility (mobile exit) but benefit only partially from centralization (moderate victim status) → moderate d → moderate chi. The institutional framework itself is constrained—it cannot exit even though it might benefit from decentralization (constrained exit despite beneficiary-adjacent status through inertia). The analytical observer's directionality is set at canonical value for their power atom (0.73 → f(d) ≈ 1.15), producing moderate experienced extraction reflective of their standing outside the mechanism.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    uncertainty_irreducibility_vs_information_asymmetry,
    'Is the suppression of distributed decision-making justified by irreducible epistemic limits on uncertainty, or is it an artifact of centralized control over information access?',
    'Comparative study of decision outcomes in systems with distributed vs centralized information access under identical uncertainty conditions; measurement of information quality gains from decentralization vs losses from slower aggregation',
    'If irreducible: uncertainty fundamentally demands centralized expertise — snare/rope classification sustained. If information-asymmetry artifact: centralization is contingent institutional choice — classification shifts toward greater snare severity and piton strength.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(uncertainty_irreducibility_vs_information_asymmetry, empirical, 'Whether suppression is justified by epistemic limits or information control').

omega_variable(
    deferral_window_extractiveness,
    'What proportion of measured extractiveness is legitimate risk-bearing compensation vs rent-seeking by decision authority during the deferral period?',
    'Longitudinal analysis of decision authority compensation and opportunity gains during deferral window vs post-outcome accountability; comparison to counterfactual compensation for equivalent risk-bearing in non-deferred structures',
    'If low legitimate proportion (<20%): extractiveness understated — should increase to 0.68+. If high (>60%): current extractiveness (0.58) is appropriate compensation for genuine uncertainty absorption.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deferral_window_extractiveness, empirical, 'Proportion of deferral gains that represent rent-seeking vs legitimate compensation').

omega_variable(
    expertise_falsifiability,
    'Under what conditions is the centralized expert class obligated to acknowledge that its decisions failed, and what mechanisms exist to transfer authority if failure is demonstrated?',
    'Audit of post-hoc accountability in centralized systems; documentation of cases where distributed decision-makers were proven correct despite expert contradiction; measurement of authority transfer rates following demonstrated failures',
    'If no falsifiability mechanism: expertise becomes unfalsifiable — classification shifts toward pure snare across all perspectives. If mechanisms exist: tangled rope classification sustained by possibility of accountability, though suppression may increase if mechanisms are rarely triggered.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(expertise_falsifiability, empirical, 'Whether expert decisions are falsifiable and accountable').

omega_variable(
    distributed_expertise_epistemic_capacity,
    'Does distributed decision-making capacity (aggregated local knowledge, real-time adaptation, contextual sensitivity) actually perform worse than centralized expertise for decisions under uncertainty, or does the performance gap primarily reflect information access asymmetries?',
    'Controlled comparison of decision quality metrics: speed of adaptation, error detection rate, outcome variance, and alignment with revealed preferences in systems that differ only in centralization dimension; separation of expertise quality from information access',
    'If distributed performs worse: centralization justified — rope becomes appropriate classification for beneficiaries. If equivalent or better when information-matched: decentralization is suppressed by institutional choice rather than epistemic necessity — snare and piton severity increase.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributed_expertise_epistemic_capacity, empirical, 'Whether distributed decision-making performs worse or is suppressed by information asymmetry').

omega_variable(
    false_summit_naturalization,
    'Is the framing ''uncertainty requires centralized expertise'' a genuine constraint or a normalized institutional narrative that benefits from appearing natural?',
    'Historical analysis of institutional emergence: does centralization precede demonstrated uncertainty problems, or do uncertainty problems emerge to justify post-hoc centralization? Comparison to non-centralized epistemic practices that successfully manage uncertainty.',
    'If genuine epistemic necessity: mountain classification from analytical perspective is justified. If institutional narrative: mountain classification is a false summit and should be overridden to tangled rope or snare depending on beneficiary/victim asymmetry. FSM detection may reclassify.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_naturalization, conceptual, 'Whether centralization is epistemic necessity or institutional narrative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(optimization_under_uncertainty, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(optunc_tr_t0, optimization_under_uncertainty, theater_ratio, 0, 0.42).
narrative_ontology:measurement(optunc_tr_t10, optimization_under_uncertainty, theater_ratio, 10, 0.58).
narrative_ontology:measurement(optunc_tr_t20, optimization_under_uncertainty, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(optunc_be_t0, optimization_under_uncertainty, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(optunc_be_t10, optimization_under_uncertainty, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(optunc_be_t20, optimization_under_uncertainty, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(optunc_su_t0, optimization_under_uncertainty, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(optunc_su_t10, optimization_under_uncertainty, suppression_requirement, 10, 0.61).
narrative_ontology:measurement(optunc_su_t20, optimization_under_uncertainty, suppression_requirement, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(optimization_under_uncertainty, resource_allocation).
narrative_ontology:affects_constraint(optimization_under_uncertainty, technocratic_legitimacy).
narrative_ontology:affects_constraint(optimization_under_uncertainty, epistemic_authority_concentration).
narrative_ontology:affects_constraint(optimization_under_uncertainty, procedural_accountability_deferral).

% DUAL FORMULATION NOTE:
% Optimization-under-uncertainty is upstream of multiple institutional constraints that inherit its authority structure. Technocratic legitimacy depends on the assumption that centralized expertise optimizes under uncertainty. Epistemic authority concentration depends on information asymmetries justified by technical complexity. Procedural accountability deferral depends on the framing that uncertainty prevents immediate outcome assessment. Each downstream constraint has its own extractiveness value reflecting the specific institutional mechanism; optimization-under-uncertainty provides the legitimating frame.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(optimization_under_uncertainty, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
