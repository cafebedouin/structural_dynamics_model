% ============================================================================
% CONSTRAINT STORY: brain_network_paradigm_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-28
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_brain_network_paradigm_2026, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: brain_network_paradigm_2026
 *   human_readable: Distributed Brain Network Scientific Paradigm
 *   domain: technological/scientific
 *
 * SUMMARY:
 *   The Distributed Brain Network Paradigm posits that cognitive functions
 *   emerge from the coordinated activity of large-scale brain networks,
 *   rather than discrete, localized regions. This framework, enabled by
 *   technologies like fMRI, has become dominant in neuroscience since the
 *   mid-2000s. While it provides a powerful coordination language for the
 *   field, it also creates significant structural pressures. High equipment
 *   costs, complex analytical methods, and institutional incentives create a
 *   high barrier to entry, suppressing alternative scientific approaches and
 *   channeling funding and career opportunities towards adherents. This
 *   creates a classic Tangled Rope structure: a genuine coordination function
 *   is intertwined with asymmetric extraction of career capital.
 *
 * KEY AGENTS:
 *   - Established Neuroscience Labs: Primary beneficiaries (institutional/arbitrage) - Leverage the paradigm to secure large grants and high-impact publications.
 *   - Early Career Researchers: Primary victims (powerless/trapped) - Must adopt the paradigm for career progression, bearing the costs of training and the pressure to generate publishable results from noisy data.
 *   - Researchers Using Alternative Methods: Secondary victims (moderate/constrained) - Their work is often devalued or considered less 'systems-level', facing funding and publication hurdles.
 *   - Funding Agencies: Institutional actors (institutional/constrained) - Enforce the paradigm through grant allocation but are also constrained by the scientific consensus.
 *   - Field Epistemic Reliability: Abstract victim (powerless/trapped) - Suffers from non-reproducible findings stemming from analytical flexibility and publication bias.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(brain_network_paradigm_2026, 0.55).
domain_priors:suppression_score(brain_network_paradigm_2026, 0.65).
domain_priors:theater_ratio(brain_network_paradigm_2026, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(brain_network_paradigm_2026, extractiveness, 0.55).
narrative_ontology:constraint_metric(brain_network_paradigm_2026, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(brain_network_paradigm_2026, theater_ratio, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(brain_network_paradigm_2026, tangled_rope).
narrative_ontology:human_readable(brain_network_paradigm_2026, "Distributed Brain Network Scientific Paradigm").
narrative_ontology:topic_domain(brain_network_paradigm_2026, "technological/scientific").

domain_priors:requires_active_enforcement(brain_network_paradigm_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(brain_network_paradigm_2026, established_neuroscience_labs).
narrative_ontology:constraint_beneficiary(brain_network_paradigm_2026, neuroimaging_equipment_vendors).
narrative_ontology:constraint_beneficiary(brain_network_paradigm_2026, high_impact_journals).
narrative_ontology:constraint_victim(brain_network_paradigm_2026, early_career_researchers).
narrative_ontology:constraint_victim(brain_network_paradigm_2026, researchers_using_alternative_methods).
narrative_ontology:constraint_victim(brain_network_paradigm_2026, field_epistemic_reliability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EARLY CAREER RESEARCHER (SNARE) — Must adopt the paradigm to secure a postdoctoral position or faculty job. Trapped by career incentives and the high cost of entry. Bears the cost of a steep learning curve and pressure to produce 'clean' network stories from noisy data. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.94.
constraint_indexing:constraint_classification(brain_network_paradigm_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ESTABLISHED PI (ROPE) — Benefits from the paradigm as a powerful coordination tool for attracting grants, recruiting talent, and publishing in high-impact journals. Can arbitrage between different research questions within the paradigm. Experiences the costs as necessary overhead for cutting-edge science. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.08.
constraint_indexing:constraint_classification(brain_network_paradigm_2026, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (TANGLED ROPE) — Recognizes both the genuine coordination function (providing a common language for systems neuroscience) and the significant extraction (channeling career and funding resources) and suppression (sidelining alternative methods). This is the canonical classification. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.76.
constraint_indexing:constraint_classification(brain_network_paradigm_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: RESEARCHER USING ALTERNATIVE METHODS (TANGLED ROPE) — Not trapped, but constrained. Their work (e.g., cellular, lesion studies) is de-prioritized for funding and prestige. They see the paradigm's utility but also feel its suppressive effect on methodological diversity. d≈0.85, f(d)≈1.15, σ=1.2 → χ≈0.76.
constraint_indexing:constraint_classification(brain_network_paradigm_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: FUNDING AGENCY (PITON) — The paradigm's function is partially degraded. While it generates high-profile publications, the officer is aware of the reproducibility issues and the high theater_ratio (0.60). The agency continues to fund it due to institutional inertia and lack of a clear successor paradigm, making it a Piton from their constrained perspective. The classification is marginal, as theater is not > 0.7, but reflects the view of a system maintained by inertia.
constraint_indexing:constraint_classification(brain_network_paradigm_2026, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(brain_network_paradigm_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(brain_network_paradigm_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(brain_network_paradigm_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(brain_network_paradigm_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(brain_network_paradigm_2026, TR),
    TR >= 0.70.

:- end_tests(brain_network_paradigm_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Represents the significant channeling of career capital, funding, and publication prestige towards researchers who adopt the paradigm. The 'researcher degrees of freedom' in complex data analysis pipelines allow for the 'extraction' of statistically significant results that may not be robust, benefiting individual careers at the expense of the field's epistemic health. Suppression (0.65): High. The immense cost of fMRI scanners and supporting infrastructure, coupled with the specialized expertise required for data analysis, creates a formidable barrier to entry and suppresses methodological diversity. Grant and hiring committees reinforce this. Theater Ratio (0.60): The visually compelling nature of brain network diagrams often obscures the profound statistical uncertainty and analytical choices underlying them, leading to a performative presentation of findings that can overstate their certainty.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark between the established PI, who sees a powerful tool for scientific coordination (Rope), and the early-career researcher, who experiences a coercive career filter (Snare). The PI has arbitrage; they can choose which network to study. The trainee is trapped; they must study networks to get a job. This difference in exit options and structural position explains the radically different classifications of the same underlying scientific framework.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (Established Labs, Vendors) have arbitrage and institutional power, leading to a low 'd' value and a perception of the paradigm as a low-extraction coordination tool (Rope). Victims (ECRs, alternative methodologists) are trapped or constrained, leading to a high 'd' value and experiencing the paradigm as highly extractive (Snare or Tangled Rope). The system's active enforcement by funding bodies and journals solidifies its Tangled Rope nature from an analytical view.
 *
 * MANDATROPHY ANALYSIS:
 *   This case prevents mandatrophy by correctly identifying a system with a dual function. Labeling the paradigm as a pure Snare would ignore its genuine, field-defining coordination benefits. Labeling it as a pure Rope would ignore the coercive career pressures and suppression of methodological diversity it imposes. The Tangled Rope classification acknowledges that a constraint can be both a valuable coordination standard and a mechanism for asymmetric extraction, depending entirely on the agent's structural relationship to it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causality_vs_correlation,
    'Is the observed functional connectivity causally responsible for cognitive functions, or is it merely a downstream correlation or epiphenomenon?',
    'Multi-modal experiments combining fMRI with causal manipulation techniques like transcranial magnetic stimulation (TMS) or optogenetics in animal models.',
    'Strong evidence for causality would shift the paradigm towards a Rope, validating its coordination function. Strong evidence for it being epiphenomenal would shift it towards a Piton, revealing its function as largely inertial.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(causality_vs_correlation, empirical, 'Distinguishing causal role from correlation in brain network activity').

omega_variable(
    reproducibility_crisis,
    'To what extent are findings within the paradigm reproducible, versus being artifacts of ''researcher degrees of freedom'' in data preprocessing and analysis?',
    'Large-scale, pre-registered replication projects using standardized analysis pipelines on shared datasets (e.g., ABCD, UK Biobank).',
    'High reproducibility would strengthen the Rope classification. Pervasive non-reproducibility would confirm the Snare perspective for junior researchers and push the analytical view towards a high-theater Piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reproducibility_crisis, empirical, 'Quantifying the impact of analytical flexibility on the reproducibility of findings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(brain_network_paradigm_2026, 2005, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(brai_tr_t0, brain_network_paradigm_2026, theater_ratio, 0, 0.35).
narrative_ontology:measurement(brai_tr_t10, brain_network_paradigm_2026, theater_ratio, 10, 0.5).
narrative_ontology:measurement(brai_tr_t20, brain_network_paradigm_2026, theater_ratio, 20, 0.6).

% Extraction over time
narrative_ontology:measurement(brai_be_t0, brain_network_paradigm_2026, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(brai_be_t10, brain_network_paradigm_2026, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(brai_be_t20, brain_network_paradigm_2026, base_extractiveness, 20, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(brain_network_paradigm_2026, information_standard).
narrative_ontology:affects_constraint(brain_network_paradigm_2026, network_models_of_psychopathology).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
