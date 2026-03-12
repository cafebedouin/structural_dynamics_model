% ============================================================================
% CONSTRAINT STORY: consciousness_substrate_independence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_consciousness_substrate_independence, []).

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
 *   constraint_id: consciousness_substrate_independence
 *   human_readable: Consciousness Substrate Independence Hypothesis
 *   domain: computational_neuroscience/philosophy_of_mind/whole_brain_emulation
 *
 * SUMMARY:
 *   The substrate independence hypothesis — that consciousness is a pattern
 *   of information processing instantiable on any physical substrate —
 *   structures the entire whole-brain emulation research program and much of
 *   contemporary philosophy of mind. This constraint exhibits the
 *   tangled_rope signature: it serves a genuine coordination function
 *   (enabling rigorous computational modeling, cross-disciplinary
 *   collaboration, empirical research programs) while simultaneously
 *   embedding asymmetric extraction (systematically marginalizing biological,
 *   phenomenological, and embodied approaches through funding concentration
 *   and institutional legitimacy rather than empirical superiority). The
 *   constraint's theater_ratio (0.65) reflects the increasing gap between the
 *   philosophical sophistication of consciousness attribution criteria and
 *   the actual epistemic warrant for those criteria. Behavioral equivalence
 *   tests, neural correlate matching, and information integration measures
 *   are treated as definitive evidence for phenomenal experience despite
 *   widespread acknowledgment within the field that these are at best
 *   necessary conditions, not sufficient ones. The constraint has accumulated
 *   extractiveness over its 16-year interval as initial theoretical pluralism
 *   has given way to computational functionalist hegemony, not through
 *   decisive empirical victories but through institutional momentum and
 *   capital concentration.
 *
 * KEY AGENTS:
 *   - Whole-Brain Emulation Industry: Primary beneficiary (institutional/arbitrage) — captures investment capital, talent, institutional legitimacy; substrate independence is foundational assumption enabling entire research program
 *   - Biological Uniqueness Frameworks: Primary victim (powerless/trapped) — abstract theoretical position with no institutional advocate; systematically defunded and dismissed; cannot exit computational functionalist framing
 *   - Embodied Cognition Researchers: Secondary victim (moderate/constrained) — face career pressure to engage computational models while maintaining theoretical commitments; mixed experience of coordination and extraction
 *   - Integrated Information Theory Coalition: Organized agents (organized/mobile) — IIT, GWT, HOT theories treat substrate independence as testable hypothesis with empirical sunset; have funding and agency
 *   - Turing Test Paradigm: Institutional actor (institutional/constrained) — maintains degraded behavioral equivalence criterion through inertia; recognizes own inadequacy but cannot exit
 *   - Computational Functionalists: Institutional actors (institutional/arbitrage) — experience substrate independence as logical necessity; risk naturalizing philosophical commitment as law of nature (false summit)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees both genuine coordination function and asymmetric extraction; tangled_rope classification reflects structural reality of mixed mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(consciousness_substrate_independence, 0.58).
domain_priors:suppression_score(consciousness_substrate_independence, 0.68).
domain_priors:theater_ratio(consciousness_substrate_independence, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(consciousness_substrate_independence, extractiveness, 0.58).
narrative_ontology:constraint_metric(consciousness_substrate_independence, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(consciousness_substrate_independence, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(consciousness_substrate_independence, tangled_rope).
narrative_ontology:human_readable(consciousness_substrate_independence, "Consciousness Substrate Independence Hypothesis").
narrative_ontology:topic_domain(consciousness_substrate_independence, "computational_neuroscience/philosophy_of_mind/whole_brain_emulation").

domain_priors:requires_active_enforcement(consciousness_substrate_independence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(consciousness_substrate_independence, whole_brain_emulation_industry).
narrative_ontology:constraint_beneficiary(consciousness_substrate_independence, computational_functionalists).
narrative_ontology:constraint_beneficiary(consciousness_substrate_independence, transhumanist_movement).
narrative_ontology:constraint_victim(consciousness_substrate_independence, biological_uniqueness_frameworks).
narrative_ontology:constraint_victim(consciousness_substrate_independence, phenomenological_research_programs).
narrative_ontology:constraint_victim(consciousness_substrate_independence, embodied_cognition_theorists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BIOLOGICAL UNIQUENESS FRAMEWORK (SNARE) — Trapped by the dominance of computational functionalism in funding, publication, and institutional legitimacy. Cannot exit the framing that treats biological specificity as implementation detail rather than constitutive feature. Bears full cost of epistemic marginalization — research programs investigating biological necessity for consciousness are systematically defunded and dismissed as mysterian or vitalist. Maximum experienced extraction from an abstract position with no institutional advocate.
constraint_indexing:constraint_classification(consciousness_substrate_independence, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EMBODIED COGNITION RESEARCHER (TANGLED ROPE) — Constrained by career incentives to engage with computational models while maintaining theoretical commitment to embodiment. Benefits from the computational infrastructure and cross-disciplinary collaboration the substrate independence hypothesis enables, but also bears extraction through marginalization of non-computational approaches. Mixed experience: genuine coordination on some research questions, asymmetric extraction on foundational assumptions.
constraint_indexing:constraint_classification(consciousness_substrate_independence, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: WHOLE-BRAIN EMULATION INDUSTRY (ROPE) — Primary beneficiary. Experiences the constraint as pure coordination: substrate independence is the foundational assumption that makes the entire research program coherent and fundable. Captures investment capital, talent, and institutional legitimacy. Can arbitrage between different computational approaches and funding sources. Net beneficiary with minimal experienced extraction.
constraint_indexing:constraint_classification(consciousness_substrate_independence, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTEGRATED INFORMATION THEORY COALITION (SCAFFOLD) — Organized research program (IIT, Global Workspace Theory, Higher-Order Thought theories) that treats substrate independence as a temporary working hypothesis to be tested empirically. Sees the constraint as coordination with a sunset: as measurement techniques improve (neural correlates of consciousness, phi measurements, causal structure analysis), empirical data will resolve whether substrate matters. Low effective extraction because the coalition has agency, funding, and sees an empirical exit path within 20-30 years.
constraint_indexing:constraint_classification(consciousness_substrate_independence, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: TURING TEST PARADIGM (PITON) — The behavioral equivalence criterion for consciousness attribution has degraded into a performative ritual. Institutional actors recognize that passing behavioral tests does not establish phenomenal experience, yet the paradigm persists through inertia in AI ethics, legal frameworks, and popular discourse. High theater ratio: the test is maintained not because it works but because no consensus alternative has replaced it. The paradigm sees its own inadequacy but cannot exit.
constraint_indexing:constraint_classification(consciousness_substrate_independence, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, substrate independence serves a genuine coordination function: it enables rigorous computational modeling, cross-disciplinary collaboration, and empirical research programs. However, it also embeds asymmetric extraction: the hypothesis systematically privileges computational approaches over biological, phenomenological, and embodied alternatives, not through empirical superiority but through institutional momentum and funding concentration. The analytical classification is tangled_rope because both the coordination function and the extractive mechanism are structurally real.
constraint_indexing:constraint_classification(consciousness_substrate_independence, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: COMPUTATIONAL FUNCTIONALIST FALSE SUMMIT (MOUNTAIN) — Strong computational functionalists experience substrate independence as an immutable logical truth: if consciousness is a pattern of information processing, then the substrate implementing that pattern is necessarily irrelevant. This perspective naturalizes a philosophical commitment as a law of nature. The engine's false summit detector will flag this as naturalization of a contingent theoretical framework — the 'logical necessity' is an artifact of definitional choices, not an empirical constraint.
constraint_indexing:constraint_classification(consciousness_substrate_independence, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(consciousness_substrate_independence_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(consciousness_substrate_independence, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(consciousness_substrate_independence, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(consciousness_substrate_independence, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(consciousness_substrate_independence, TR),
    TR >= 0.70.

:- end_tests(consciousness_substrate_independence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High. The substrate independence hypothesis captures significant career, funding, and institutional benefits for computational approaches while systematically marginalizing alternatives. The extraction is not total — some biological and phenomenological research persists — but the asymmetry is severe and increasing. The value reflects that much of the computational dominance is not earned through empirical superiority but through institutional momentum: the hypothesis is unfalsifiable in practice (any failure of an emulation can be attributed to insufficient fidelity rather than substrate dependence), yet it structures funding allocation as if it were established fact. Suppression (0.68): High. Significant barriers to alternative approaches include funding concentration in computational programs, publication bias favoring computational models, career risk for researchers challenging functionalist assumptions, and rhetorical dismissal of biological specificity as mysterian or vitalist. The suppression is not absolute — embodied cognition and phenomenological research programs exist — but they operate under severe structural disadvantage. Theater ratio (0.65): Moderate-high. The gap between philosophical sophistication and epistemic warrant is substantial and growing. Consciousness attribution criteria (behavioral equivalence, neural correlate matching, phi measurements) are treated as definitive despite widespread acknowledgment that they are at best necessary conditions. The theater has increased as the field has developed increasingly elaborate computational models without corresponding progress on the hard problem of phenomenal experience.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full indexical range from false summit (computational functionalist mountain) through coordination (WBE industry rope) to pure extraction (biological uniqueness snare). The WBE industry sees pure coordination — substrate independence solves the legitimate problem of making consciousness scientifically tractable. The IIT coalition sees a temporary hypothesis with an empirical sunset — measurement techniques will resolve the question within a generation. The Turing Test paradigm sees its own degraded ritual — behavioral equivalence persists through inertia, not function. Embodied cognition researchers see mixed coordination and extraction — the computational infrastructure both enables and constrains their work. Biological uniqueness frameworks see pure extraction — computational hegemony marginalizes their research program with no self-correction mechanism. The computational functionalist sees an immutable logical truth — but the analytical observer recognizes this as naturalization of a contingent philosophical commitment. The analytical classification is tangled_rope because both the coordination function (enabling rigorous modeling) and the extractive mechanism (marginalizing alternatives through institutional power rather than empirical superiority) are structurally real.
 *
 * DIRECTIONALITY LOGIC:
 *   The whole-brain emulation industry is the primary beneficiary: substrate independence is the foundational assumption that makes their entire research program coherent and fundable. They experience low directionality (d ≈ 0.10) — the constraint runs toward them, not away from them. Biological uniqueness frameworks are the primary victim: they cannot exit the computational functionalist framing that dominates funding and publication, and they bear the full cost of epistemic marginalization. They experience maximum directionality (d ≈ 0.95) — trapped with no institutional advocate. Embodied cognition researchers occupy a middle position: constrained by career incentives to engage computational models, but also benefiting from the infrastructure those models enable. They experience moderate-high directionality (d ≈ 0.60) — significant extraction but not maximal. The IIT coalition has organized agency and sees an empirical exit path, so they experience lower directionality (d ≈ 0.45) despite being partly victimized by computational hegemony. The computational functionalist false summit perspective experiences very low directionality (d ≈ 0.05) because they are institutional beneficiaries who have naturalized their theoretical commitment as logical necessity.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that substrate independence is neither pure coordination nor pure extraction — it is structurally both. The coordination function is genuine: computational models enable empirical research, cross-disciplinary collaboration, and rigorous hypothesis testing that would be impossible under purely phenomenological or biological approaches. The extractive mechanism is also genuine: the hypothesis systematically privileges computational approaches through funding concentration and institutional legitimacy, not through decisive empirical victories. The unfalsifiability of substrate independence in practice (any emulation failure can be attributed to insufficient fidelity) means the hypothesis functions as an extractive gate: it captures resources for computational approaches while providing no empirical pathway for biological alternatives to demonstrate their necessity. The tangled_rope classification captures this dual structure: real coordination value entangled with asymmetric extraction that operates through institutional power rather than knowledge superiority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    phenomenal_marker_sufficiency,
    'Do observable neural correlates of consciousness (NCCs) constitute sufficient evidence for phenomenal experience, or are they merely necessary conditions?',
    'Longitudinal tracking of NCC predictions vs subjective reports in edge cases (anesthesia awareness, locked-in syndrome, vegetative states); cross-species NCC comparison; emulation experiments with systematic substrate variation',
    'If sufficient: substrate independence is empirically testable and potentially falsifiable. If merely necessary: the hard problem remains, and substrate independence is unfalsifiable metaphysics masquerading as science.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(phenomenal_marker_sufficiency, conceptual, 'Whether neural correlates constitute sufficient evidence for consciousness').

omega_variable(
    emulation_fidelity_threshold,
    'What level of connectome fidelity is required to preserve consciousness: synaptic connectivity, molecular dynamics, quantum coherence, or something else?',
    'Systematic degradation experiments on increasingly detailed emulations; comparison of emulation behavior at different fidelity levels; identification of critical features whose omission destroys function',
    'If synaptic connectivity sufficient: substrate independence confirmed, WBE feasible. If molecular/quantum dynamics required: substrate independence false, biological specificity constitutive. If threshold unknowable: constraint is unfalsifiable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emulation_fidelity_threshold, empirical, 'Minimum connectome fidelity threshold for consciousness preservation').

omega_variable(
    philosophical_criteria_convergence,
    'Will competing philosophical frameworks for consciousness attribution (functionalism, biological naturalism, panpsychism, illusionism) converge on empirical criteria, or do they reflect irreducible value commitments?',
    'Historical analysis of theory convergence in other domains; identification of shared empirical predictions vs divergent metaphysical commitments; tracking of cross-framework researcher migration',
    'If convergence possible: substrate independence is a temporary scientific hypothesis. If irreducible: substrate independence is a preference-class omega, and the constraint''s extractiveness reflects value imposition rather than knowledge asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(philosophical_criteria_convergence, preference, 'Whether philosophical frameworks will converge on empirical criteria').

omega_variable(
    investment_capital_distortion,
    'Does the concentration of funding in substrate-independent approaches reflect genuine scientific promise, or does it create a self-fulfilling prophecy by starving alternative research programs?',
    'Counterfactual funding analysis: comparison of research productivity per dollar in computational vs biological consciousness research; tracking of researcher career paths and funding success rates by theoretical commitment; identification of promising biological approaches that failed due to funding rather than empirical failure',
    'If genuine promise: funding concentration is efficient allocation. If self-fulfilling: the constraint is extractive rent-seeking disguised as scientific consensus, and the suppression metric underestimates true barriers to alternative approaches.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(investment_capital_distortion, empirical, 'Whether funding concentration reflects scientific promise or creates self-fulfilling prophecy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(consciousness_substrate_independence, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(csi_tr_t0, consciousness_substrate_independence, theater_ratio, 0, 0.4).
narrative_ontology:measurement(csi_tr_t8, consciousness_substrate_independence, theater_ratio, 8, 0.52).
narrative_ontology:measurement(csi_tr_t16, consciousness_substrate_independence, theater_ratio, 16, 0.65).

% Extraction over time
narrative_ontology:measurement(csi_be_t0, consciousness_substrate_independence, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(csi_be_t8, consciousness_substrate_independence, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(csi_be_t16, consciousness_substrate_independence, base_extractiveness, 16, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(consciousness_substrate_independence, information_standard).
narrative_ontology:affects_constraint(consciousness_substrate_independence, connectome_sufficiency).

% DUAL FORMULATION NOTE:
% Consciousness substrate independence is downstream of connectome sufficiency: if connectome data is insufficient to specify consciousness, then substrate independence is false (biological specificity matters). The upstream constraint (connectome sufficiency) has its own extractiveness reflecting the empirical status of connectome completeness claims; this constraint has its own extractiveness reflecting the institutional and funding asymmetries that privilege computational approaches over biological ones.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
