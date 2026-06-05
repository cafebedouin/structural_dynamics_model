% ============================================================================
% CONSTRAINT STORY: synaptic_plasticity_timescales
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_synaptic_plasticity_timescales, []).

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
 *   constraint_id: synaptic_plasticity_timescales
 *   human_readable: Synaptic Plasticity Timescales: Coordination vs. Extraction in Learning Mechanism Standardization
 *   domain: neuroscience/computational_neuroscience
 *
 * SUMMARY:
 *   Synaptic plasticity — the ability of synapses to change strength in
 *   response to activity — operates across multiple distinct timescales: from
 *   milliseconds (vesicle release, receptor binding) through seconds
 *   (presynaptic facilitation/depression, calcium signaling), to hours and
 *   days (protein synthesis-dependent long-term potentiation), to weeks
 *   (structural remodeling). The field has historically organized around a
 *   canonical set of timescales derived from molecular biology measurement
 *   capabilities: patch clamp whole-cell recordings (milliseconds to
 *   seconds), biochemical assays (minutes to hours), and behavioral learning
 *   protocols (hours to days). This constraint creates a structural tension
 *   between the genuine coordination need for standardized protocols enabling
 *   cross-lab validation and the extractive reductionism that occurs when
 *   slower or faster timescales are suppressed from the canonical framework.
 *   Systems neuroscience research asking about learning and memory must
 *   either reformulate questions to fit molecular timescales or accept
 *   marginalization in high-impact journals. Behavioral phenotypes that
 *   depend on timescale combinations outside the canonical range become
 *   invisible or misclassified as noise. The theater ratio (0.65) reflects
 *   the performative element: researchers fit canonical timescale models to
 *   data even when they know the models are insufficient, because grant
 *   agencies, journals, and peer networks reward the ritual. The constraint
 *   exhibits tangled rope structure: genuine coordination function
 *   (standardized protocols enable reproducibility) coexists with asymmetric
 *   extraction (faster/slower timescales bear the cost of invisibility).
 *
 * KEY AGENTS:
 *   - Molecular Neuroscience Laboratory: Primary beneficiary (institutional/arbitrage) — captures funding priority and high-impact publication advantage when research fits canonical timescale standards
 *   - Systems Neuroscience Community: Secondary victim (moderate/constrained) — must reformulate research questions around molecular timescales or accept reduced publication visibility and funding competitiveness
 *   - Behavioral Phenotype: Primary victim (powerless/trapped) — learning mechanisms depending on non-canonical timescales are rendered invisible; behavior is explained by models that suppress the timescale diversity actually driving the phenotype
 *   - Multi-Timescale Neuroscience Coalition: Organized alternative (organized/mobile) — NSF Brain Initiative, European Human Brain Project, and optogenetics consortia funding multi-timescale frameworks as parallel standards
 *   - Computational Model Standard: Institutional legacy (institutional/arbitrage) — Hodgkin-Huxley formalism and descendants persist through curricula inertia despite recognized limitations
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the multi-timescale reality of plasticity as a limitation to overcome rather than a feature to explain
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(synaptic_plasticity_timescales, 0.38).
domain_priors:suppression_score(synaptic_plasticity_timescales, 0.48).
domain_priors:theater_ratio(synaptic_plasticity_timescales, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(synaptic_plasticity_timescales, extractiveness, 0.38).
narrative_ontology:constraint_metric(synaptic_plasticity_timescales, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(synaptic_plasticity_timescales, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(synaptic_plasticity_timescales, tangled_rope).
narrative_ontology:human_readable(synaptic_plasticity_timescales, "Synaptic Plasticity Timescales: Coordination vs. Extraction in Learning Mechanism Standardization").
narrative_ontology:topic_domain(synaptic_plasticity_timescales, "neuroscience/computational_neuroscience").

domain_priors:requires_active_enforcement(synaptic_plasticity_timescales).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(synaptic_plasticity_timescales, molecular_neuroscience_labs).
narrative_ontology:constraint_beneficiary(synaptic_plasticity_timescales, computational_modelers).
narrative_ontology:constraint_victim(synaptic_plasticity_timescales, systems_neuroscience_community).
narrative_ontology:constraint_victim(synaptic_plasticity_timescales, behavioral_phenotype_fidelity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BEHAVIORAL PHENOTYPE (SNARE) — Cannot exit the constraint; behavior emerges from synaptic dynamics operating across multiple timescales simultaneously (milliseconds to hours). If the field enforces a single canonical timescale, behavioral diversity and learning mechanisms that rely on slower or faster scales are rendered invisible or misclassified as noise. Maximum extraction with no escape — the phenotype bears the cost of timescale reductionism.
constraint_indexing:constraint_classification(synaptic_plasticity_timescales, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SYSTEMS NEUROSCIENCE COMMUNITY (TANGLED ROPE) — Constrained by need to interface with molecular timescale measurements (patch clamp recordings, molecular biology publication standards) to justify research. Also benefits from standardized protocols enabling cross-lab comparison and meta-analysis. Significant extraction (must reformulate research questions around molecular timescales) but also genuine coordination function (enables collaborative science).
constraint_indexing:constraint_classification(synaptic_plasticity_timescales, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MOLECULAR NEUROSCIENCE LABORATORY (ROPE) — Primary beneficiary with arbitrage exit options (can publish in high-impact journals, secure funding, train students). Experiences the constraint as coordination: standardized timescale protocols enable reproducibility, cross-lab validation, and funding agency alignment. Net beneficiary — timescale standardization elevates molecular findings into epistemic authority.
constraint_indexing:constraint_classification(synaptic_plasticity_timescales, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MULTI-TIMESCALE NEUROSCIENCE COALITION (SCAFFOLD) — Organized agents (NSF multi-scale modeling initiatives, optogenetics consortia, integrated circuit neuron models) are building parallel standards that span millisecond to hour timescales without reducing them to a single canonical scale. Sunset logic applies: as multi-timescale frameworks become funded and validated, the single-timescale extraction mechanism loses institutional force. Low effective extraction because the coalition has agency and sees a funded exit path.
constraint_indexing:constraint_classification(synaptic_plasticity_timescales, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: COMPUTATIONAL MODEL STANDARD (PITON) — The Hodgkin-Huxley formalism and its descendants have become the default language for describing neural dynamics. This standard is substantially degraded: it captures single-neuron ion channel kinetics well but fails to represent network-level plasticity, neuromodulation, and learning across timescales. The standard persists through institutional inertia — textbooks, course curricula, funding agency RFPs — despite widespread recognition of its limitations. Theater ratio high because the ritual of fitting HH-type equations to data persists even when the model is known to be insufficient.
constraint_indexing:constraint_classification(synaptic_plasticity_timescales, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / TIMESCALE INVARIANCE VIEW (MOUNTAIN) — From a civilizational/universal perspective, synaptic plasticity inherently involves multiple timescales: receptor binding (microseconds), channel opening (milliseconds), synaptic transmission (tens of milliseconds), short-term plasticity (hundreds of milliseconds to seconds), long-term potentiation (hours to days), protein synthesis (hours), structural remodeling (days to weeks). No single timescale is sufficient — the constraint to reduce them is imposed by institutional convenience, not by the physics of learning. The mountain classification appears natural but is a false summit: the multiplicity of timescales is a feature of the system, not a limitation to overcome.
constraint_indexing:constraint_classification(synaptic_plasticity_timescales, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(synaptic_plasticity_timescales_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(synaptic_plasticity_timescales, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(synaptic_plasticity_timescales, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(synaptic_plasticity_timescales, TR),
    TR >= 0.70.

:- end_tests(synaptic_plasticity_timescales_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The canonical timescale standardization extracts value from systems-level research by forcing reformulation, but extraction is not severe because complementary funding mechanisms (NSF systems track, behavioral neuroscience) do exist and some cross-timescale work remains publishable. The extraction is real but partial — the field has not achieved total suppression of non-canonical timescales. Suppression (0.48): Moderate. Barriers include journal editorial preferences for molecular data, funding agency RFP language emphasizing cellular mechanisms, and curriculum emphasis on molecular timescale models. But these barriers are surmountable — alternative funding exists, field journals publish systems-level work, and younger labs increasingly work across timescales. Theater ratio (0.65): Moderate-high. The ritual of fitting canonical models (HH equations, synaptic filtering) to data persists widely even among researchers who recognize the models' limitations. The theater increased over the 30-year interval as molecular techniques became more accessible and as molecular findings gained more epistemic authority. Theater_ratio trajectory shows this increase: 0.42 → 0.65 over the measurement interval.
 *
 * PERSPECTIVAL GAP:
 *   The original gap is between beneficiary (molecular lab sees Rope) and victim (systems community sees Tangled Rope, behavioral phenotype sees Snare). The analytical observer risks a mountain classification by naturalizing timescale reduction as inherent to how learning works, when in fact it is contingent on institutional measurement capabilities and funding structures. The gap widens when the multi-timescale coalition perspective is included — organized agents see a scaffold (funded alternative pathway) while the molecular lab still sees rope (standardization as coordination). The piton classification of the computational standard is a separate diagnostic — it shows that the institutional model is degraded but persists.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the agent's position in the extraction flow. Molecular labs benefit from the constraint (low d, negative chi experienced) — they gain publication priority and funding advantage. Systems labs bear extraction costs while also receiving some benefits (moderate d, moderate positive chi experienced) — they can collaborate using molecular protocols but must suppress their own questions. The behavioral phenotype has no exit option and derives no benefit (high d, high chi experienced) — pure victim. The multi-timescale coalition has exit options and sees agency (lower d through arbitrage/mobile exit) — they experience the constraint as temporary and surmountable. The computational standard (piton classification) derives from high theater_ratio, not from high experienced extraction — the standard persists even though it is recognized as incomplete.
 *
 * MANDATROPHY ANALYSIS:
 *   STRUCTURE: This constraint resolves mandatrophy by showing that the 'reduction vs. multiple timescales' framing naturalizes a choice. The molecular timescale standard is real and useful for reproducibility (genuine coordination function). But it is also extractive toward researchers asking about system-level learning mechanisms that depend on timescale combinations (genuine extraction). Neither aspect is illusory — the tangled rope classification captures both. The false summit (analytical mountain) reveals that the analytical observer can rationalize single-timescale reduction as inherent to neuroscience when in fact it is an institutional choice. The scaffold classification shows that multi-timescale frameworks are already being built with institutional support — the constraint is not natural law but a temporary institutional equilibrium. Mandatrophy resolution: acknowledge that the coordination function is real AND acknowledge the extraction cost, then monitor whether the multi-timescale alternative pathways actually reduce the extraction over time (testing the sunset hypothesis).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    timescale_reduction_necessity,
    'Is timescale reduction a practical simplification or a fundamental distortion of learning mechanisms?',
    'Behavioral prediction accuracy: models restricted to single canonical timescale vs. models spanning identified timescale ranges; correlation with intact organism learning curves',
    'If practical simplification: constraint is primarily coordination (Rope from more perspectives). If fundamental distortion: constraint is primarily extraction (Snare from more perspectives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(timescale_reduction_necessity, empirical, 'Whether timescale reduction is necessary simplification or systematic distortion').

omega_variable(
    plasticity_rule_universality,
    'Are molecular plasticity rules (e.g., spike-timing-dependent plasticity) genuinely universal across brain regions and species, or are they context-dependent and multiply realized?',
    'Cross-species, cross-region comparative analysis of plasticity induction thresholds, timing windows, and magnitude saturation; identification of systematic variation correlated with circuit function',
    'If universal: molecular timescale standardization captures real constraint (reduces to coordination). If multiply realized: standardization is extractive reductionism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(plasticity_rule_universality, empirical, 'Whether plasticity rules are universal or context-dependent').

omega_variable(
    measurement_bandwidth_asymmetry,
    'Are certain timescales overrepresented in the literature because they are technically accessible (patch clamp, two-photon imaging) rather than because they are functionally dominant?',
    'Meta-analysis of timescale distribution in published plasticity experiments vs. computational modeling of learning that weights all identified timescales equally; correlation with behavioral importance',
    'If overrepresentation detected: field is performing measurement-driven reductionism (Snare structure). If timescales reflect functional importance: standardization is justified (Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_bandwidth_asymmetry, empirical, 'Whether timescale distribution in literature reflects function or measurement bias').

omega_variable(
    neuromodulation_timescale_integration,
    'How do neuromodulatory systems (dopamine, acetylcholine, serotonin) that operate on their own timescales (seconds to minutes) integrate with ''canonical'' plasticity mechanisms?',
    'Analysis of empirical plasticity experiments under controlled vs. natural neuromodulatory conditions; identification of neuromodulator-dependent plasticity rules not captured by canonical timescale standards',
    'If significant integration needed: canonical timescale standard is incomplete (extraction mechanism dominates). If modulation is peripheral effect: standard can accommodate modifications (coordination preserved).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neuromodulation_timescale_integration, empirical, 'Whether neuromodulation requires new timescale integration or fits existing standards').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(synaptic_plasticity_timescales, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(synpt_tr_t0, synaptic_plasticity_timescales, theater_ratio, 0, 0.42).
narrative_ontology:measurement(synpt_tr_t15, synaptic_plasticity_timescales, theater_ratio, 15, 0.58).
narrative_ontology:measurement(synpt_tr_t30, synaptic_plasticity_timescales, theater_ratio, 30, 0.65).
narrative_ontology:measurement(synpt_tr_t10, synaptic_plasticity_timescales, theater_ratio, 10, 0.51).

% Extraction over time
narrative_ontology:measurement(synpt_be_t0, synaptic_plasticity_timescales, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(synpt_be_t15, synaptic_plasticity_timescales, base_extractiveness, 15, 0.3).
narrative_ontology:measurement(synpt_be_t30, synaptic_plasticity_timescales, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(synpt_be_t10, synaptic_plasticity_timescales, base_extractiveness, 10, 0.27).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(synaptic_plasticity_timescales, information_standard).
narrative_ontology:affects_constraint(synaptic_plasticity_timescales, behavioral_learning_mechanism_visibility).
narrative_ontology:affects_constraint(synaptic_plasticity_timescales, cross_species_plasticity_comparison).
narrative_ontology:affects_constraint(synaptic_plasticity_timescales, neuromodulation_integration_framework).

% DUAL FORMULATION NOTE:
% Synaptic plasticity timescales has three related constraints: (1) the canonical timescale standardization (this story, ε=0.38); (2) the behavioral learning mechanism visibility problem (ε=0.52, Snare) — behavioral diversity is rendered invisible when explained only through canonical timescale models; (3) the cross-species plasticity comparison problem (ε=0.41, Tangled Rope) — species-specific plasticity variations are suppressed to fit universal timescale categories. Each has different ε and different victim groups. This story addresses the field-level standardization choice; the others address downstream consequences.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
