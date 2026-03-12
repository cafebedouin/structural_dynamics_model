% ============================================================================
% CONSTRAINT STORY: incomplete_derivations_as_epistemic_load
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_incomplete_derivations_as_epistemic_load, []).

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
 *   constraint_id: incomplete_derivations_as_epistemic_load
 *   human_readable: Incomplete Derivations as Epistemic Load in Dark Matter Theory
 *   domain: theoretical_physics/high_energy_physics/cosmology
 *
 * SUMMARY:
 *   The incomplete derivations in dark matter theory create a structural
 *   tension between the need to organize empirical research effort (requiring
 *   a sufficiently specified theoretical framework) and the epistemic
 *   requirement that foundational derivations be completed before large-scale
 *   experimental investment. Five derivations are explicitly load-bearing
 *   rather than peripheral: (1) lattice QCD calculation of mass ratios, (2)
 *   complete Boltzmann equation solution for relic abundance including all
 *   quantum corrections, (3) UV completion determining chirality structure,
 *   (4) non-linear structure formation predicting halo profiles, (5)
 *   inflation coupling producing CMB-consistent isocurvature. The constraint
 *   exhibits tangled rope structure: the incomplete framework provides
 *   genuine coordination value (dimensional analysis and symmetry arguments
 *   organize phenomenology) while simultaneously extracting from experimental
 *   collaborations and early career researchers who bear the risk that
 *   completing the derivations may reveal internal contradictions. The
 *   theater_ratio (0.58) reflects that peer review has become performative:
 *   papers are accepted with dimensional estimates and symmetry arguments in
 *   place of complete derivations, and the standard for theoretical rigor has
 *   degraded over time as the community has normalized incompleteness. The
 *   primary observable is whether completing the five derivations produces
 *   consistent results or reveals that the framework's apparent coherence was
 *   an artifact of derivational incompleteness.
 *
 * KEY AGENTS:
 *   - Experimental Collaborations: Primary victim (powerless/trapped) — multi-year detector construction timelines commit resources before derivations are complete; cannot exit once committed; bear full cost if derivations produce contradictions
 *   - Early Career Researchers: Secondary victim (moderate/constrained) — career investment in research program with incomplete foundations; can pivot but at high cost; mixed experience of coordination benefit and extraction risk
 *   - Established Research Programs: Primary beneficiary (institutional/arbitrage) — framework organizes research effort while remaining flexible; can pivot to alternatives without penalty; experience incompleteness as productive feature enabling exploration
 *   - Theory Development Coalition: Organized agents (organized/mobile) — working on completing derivations; see incompleteness as temporary with sunset; control research agenda and resource allocation
 *   - Peer Review Ritual: Institutional actor (institutional/arbitrage) — maintains performative standard accepting dimensional estimates without complete derivations; degraded from original rigor through normalization of incompleteness
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees mixed structure of genuine coordination value and asymmetric extraction risk; constraint is contingent on resource allocation, not natural law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(incomplete_derivations_as_epistemic_load, 0.48).
domain_priors:suppression_score(incomplete_derivations_as_epistemic_load, 0.52).
domain_priors:theater_ratio(incomplete_derivations_as_epistemic_load, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(incomplete_derivations_as_epistemic_load, extractiveness, 0.48).
narrative_ontology:constraint_metric(incomplete_derivations_as_epistemic_load, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(incomplete_derivations_as_epistemic_load, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(incomplete_derivations_as_epistemic_load, tangled_rope).
narrative_ontology:human_readable(incomplete_derivations_as_epistemic_load, "Incomplete Derivations as Epistemic Load in Dark Matter Theory").
narrative_ontology:topic_domain(incomplete_derivations_as_epistemic_load, "theoretical_physics/high_energy_physics/cosmology").

domain_priors:requires_active_enforcement(incomplete_derivations_as_epistemic_load).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(incomplete_derivations_as_epistemic_load, theoretical_physics_community_maintaining_standards).
narrative_ontology:constraint_beneficiary(incomplete_derivations_as_epistemic_load, established_research_programs).
narrative_ontology:constraint_victim(incomplete_derivations_as_epistemic_load, premature_empirical_investment).
narrative_ontology:constraint_victim(incomplete_derivations_as_epistemic_load, experimental_collaborations).
narrative_ontology:constraint_victim(incomplete_derivations_as_epistemic_load, early_career_researchers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXPERIMENTAL COLLABORATION (SNARE) — Trapped by multi-year detector construction timelines and funding commitments made before theoretical derivations are complete. Cannot exit once resources are committed. Bears full cost of theoretical incompleteness: if the five derivations (mass ratio, abundance, chirality, halo profile, inflation coupling) produce contradictions when completed, years of experimental effort may target parameter space that doesn't exist. Maximum extraction — no agency to demand derivational completeness before committing resources.
constraint_indexing:constraint_classification(incomplete_derivations_as_epistemic_load, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EARLY CAREER RESEARCHER (TANGLED ROPE) — Constrained by publication pressure and job market timelines. Benefits from the theoretical framework (provides research direction, citation network, conference community) but also bears significant cost: career investment in a research program whose foundational derivations remain incomplete. Can pivot to adjacent fields but at high cost (lost specialization, weakened network). Mixed experience — genuine coordination function (the framework organizes research effort) alongside asymmetric extraction (senior theorists set agenda without completing derivations; early career researchers bear risk of foundational failure).
constraint_indexing:constraint_classification(incomplete_derivations_as_epistemic_load, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ESTABLISHED RESEARCH PROGRAM (ROPE) — Benefits from the incomplete derivations as a coordination mechanism: the framework is sufficiently specified to organize research effort (dimensional analysis provides scaling relations, symmetry arguments constrain model space) while remaining flexible enough to accommodate future theoretical developments. Arbitrage exit: can pivot to alternative frameworks (axions, sterile neutrinos, modified gravity) without career penalty. Experiences the constraint as productive coordination — the incomplete derivations are features, not bugs, enabling exploratory research before full theoretical closure.
constraint_indexing:constraint_classification(incomplete_derivations_as_epistemic_load, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THEORY DEVELOPMENT COALITION (SCAFFOLD) — Organized theorists working on completing the five derivations see the incompleteness as a temporary coordination problem with a sunset: lattice QCD calculations for mass ratios are improving; N-body simulations constrain halo profiles; inflation model-building is maturing. The coalition has agency and sees an exit path — the derivations will be completed within 10-20 years, at which point the epistemic load resolves. Low effective extraction because the coalition controls the research agenda and can allocate resources to derivational work.
constraint_indexing:constraint_classification(incomplete_derivations_as_epistemic_load, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: PEER REVIEW RITUAL (PITON) — The standard for accepting dark matter phenomenology papers without complete derivations has become performative: reviewers check dimensional consistency and symmetry arguments but do not require full Lagrangian derivations or proof that the five incomplete derivations will produce consistent results. The review process persists through institutional inertia — papers are accepted because similar papers were accepted previously, not because derivational completeness has been verified. Theater ratio reflects this degradation: the ritual of theoretical rigor is maintained while the substance (requiring complete derivations before empirical investment) has atrophied.
constraint_indexing:constraint_classification(incomplete_derivations_as_epistemic_load, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the incomplete derivations represent a genuine coordination function (organizing research effort around a promising framework) combined with asymmetric extraction (premature empirical investment bears risk that theoretical foundations may not close consistently). The constraint is not a natural law — the incompleteness is contingent on resource allocation and research priorities, not inherent to the physics. But it is also not pure extraction — the framework has genuine predictive content (relic abundance scaling, structure formation, collider signatures) even with incomplete derivations. Tangled rope classification reflects this mixed structure: real coordination value alongside real extraction risk.
constraint_indexing:constraint_classification(incomplete_derivations_as_epistemic_load, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(incomplete_derivations_as_epistemic_load_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(incomplete_derivations_as_epistemic_load, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(incomplete_derivations_as_epistemic_load, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(incomplete_derivations_as_epistemic_load, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(incomplete_derivations_as_epistemic_load, TR),
    TR >= 0.70.

:- end_tests(incomplete_derivations_as_epistemic_load_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The incomplete derivations extract from experimental collaborations (who commit resources before theoretical closure) and early career researchers (who bear career risk of foundational failure) while benefiting established programs (who maintain flexibility and can pivot). The extraction is not maximal because the framework does provide genuine coordination value — dimensional analysis and symmetry arguments have predictive content even without complete derivations. But the extraction is substantial because the risk of derivational inconsistency is borne asymmetrically by those with least agency to demand completeness. Suppression (0.52): Moderate-high. Significant barriers to demanding derivational completeness include: publication pressure favoring phenomenology over foundational work, funding concentration in experimental programs creating pressure to proceed before theory is complete, career incentives rewarding novel predictions over derivational rigor, and community normalization of incompleteness (the standard has shifted such that dimensional estimates are treated as sufficient). But suppression is not total — some theorists are working on completing derivations, and some funding agencies support foundational work. Theater ratio (0.58): Moderate-high and increasing. Peer review for dark matter phenomenology has become substantially performative: reviewers check dimensional consistency and symmetry arguments but do not require proof that the five incomplete derivations will close consistently. The ritual of theoretical rigor persists while the substance has degraded. The theater has increased over the 20-year interval as the community has normalized accepting papers with incomplete derivations, and the standard for what counts as 'sufficiently rigorous' has shifted downward.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural incompleteness appears differently from different positions. Experimental collaborations see a snare — they are trapped by resource commitments made before theoretical closure and bear full cost if derivations fail. Early career researchers see tangled rope — they benefit from the framework's coordination function but bear asymmetric career risk. Established programs see rope — the incompleteness is a productive feature enabling exploratory research before full theoretical closure. The theory development coalition sees scaffold — the incompleteness is temporary with a sunset as derivations are completed. The peer review ritual sees its own degradation as piton — the standard has atrophied but persists through inertia. The analytical observer sees tangled rope at the civilizational level — genuine coordination value combined with asymmetric extraction risk. The perspectival gap is not 'which type is correct?' but 'which structural position are you measuring from?' The constraint's type depends on the observer's power, exit options, and relationship to the extraction flow.
 *
 * DIRECTIONALITY LOGIC:
 *   Experimental collaborations are primary victims with trapped exit options — they commit multi-year resources before derivations are complete and cannot exit once committed. This produces high directionality (d ~ 0.92) and maximum experienced extraction. Early career researchers are secondary victims with constrained exit options — they can pivot to adjacent fields but at high cost (lost specialization, weakened network). This produces moderate-high directionality (d ~ 0.68) and significant but not maximal extraction. Established research programs are primary beneficiaries with arbitrage exit options — they benefit from the framework's coordination function while maintaining flexibility to pivot to alternatives. This produces low directionality (d ~ 0.12) and low or negative experienced extraction. The theory development coalition has organized power and mobile exit options — they control the research agenda and can allocate resources to derivational work. This produces moderate directionality (d ~ 0.45) reflecting their mixed position as both beneficiaries (framework organizes their research) and potential victims (if derivations fail to close). The peer review ritual has institutional power and arbitrage exit — it maintains the performative standard but could shift standards if community norms changed. This produces low directionality (d ~ 0.18) reflecting its role as enforcer rather than target of the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: This constraint resolves the mandatrophy by demonstrating that the incomplete derivations serve a genuine coordination function (organizing research effort around a promising framework with dimensional analysis and symmetry arguments providing predictive content) while simultaneously extracting from those who bear the risk of derivational failure (experimental collaborations committing resources before theoretical closure, early career researchers investing careers in potentially inconsistent framework). The coordination function is real — the framework has produced testable predictions and organized decades of empirical research. The extraction is also real — the risk that completing the five derivations will reveal internal contradictions is borne asymmetrically by those with least agency to demand completeness before committing resources. The tangled rope classification captures this mixed structure: neither pure coordination (rope) nor pure extraction (snare), but a hybrid where both functions coexist and the balance depends on the observer's structural position. The constraint is not a natural law (mountain) — the incompleteness is contingent on resource allocation and research priorities, not inherent to the physics. It is not a scaffold — there is no guaranteed sunset, only the coalition's belief that derivations will close consistently. It is not purely piton — the framework retains genuine predictive content despite the performative peer review. The tangled rope classification is the structural reality from the analytical perspective.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mass_ratio_derivation_consistency,
    'When the full lattice QCD calculation for dark matter mass ratios is completed, will it produce values consistent with the dimensional estimates currently used in phenomenology?',
    'Lattice QCD calculation at physical quark masses with controlled systematic errors; comparison to dimensional scaling relations m_DM ~ Lambda_QCD * (coupling)^n',
    'If inconsistent by factor > 3: large swaths of experimental parameter space currently being searched are ruled out, and years of detector development are wasted. If consistent: validates dimensional approach and reduces epistemic load.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mass_ratio_derivation_consistency, empirical, 'Whether lattice QCD mass ratios match dimensional estimates').

omega_variable(
    abundance_calculation_closure,
    'When the full Boltzmann equation for dark matter freeze-out is solved with complete cross-section calculations (including coannihilation, Sommerfeld enhancement, and bound state effects), will the predicted relic abundance Omega_DM naturally match the observed value ~0.27 without fine-tuning?',
    'Complete next-to-leading-order cross-section calculation; numerical solution of coupled Boltzmann equations; sensitivity analysis to identify fine-tuning',
    'If fine-tuning required (sensitivity > 10%): the ''WIMP miracle'' is revealed as selection bias, and the theoretical motivation for the framework collapses. If natural: validates the framework and justifies continued empirical investment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(abundance_calculation_closure, empirical, 'Whether relic abundance calculation closes without fine-tuning').

omega_variable(
    chirality_structure_determination,
    'When the UV completion of the dark matter theory is specified, will the chirality structure of dark matter couplings to Standard Model fermions be left-handed (as assumed in most phenomenology), right-handed, or vector?',
    'Construction of explicit UV-complete models (e.g., embedding in GUT or string theory); determination of low-energy effective operators from UV dynamics',
    'If right-handed or vector: collider signatures and direct detection cross-sections differ by orders of magnitude from current estimates, invalidating existing experimental constraints. If left-handed: validates current phenomenology.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(chirality_structure_determination, conceptual, 'Whether chirality structure matches phenomenological assumptions').

omega_variable(
    halo_profile_prediction,
    'When the full non-linear structure formation calculation is completed (including baryonic feedback, dark matter self-interactions, and substructure), will the predicted dark matter halo profile match the NFW or Einasto profiles assumed in indirect detection analyses?',
    'High-resolution N-body simulations with baryonic physics; comparison to observational constraints from rotation curves, lensing, and stellar kinematics',
    'If profiles differ significantly (e.g., cored vs cusped): indirect detection flux predictions change by factors of 10-100, and current null results may not constrain the theory. If profiles match: validates indirect detection strategy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(halo_profile_prediction, empirical, 'Whether predicted halo profiles match observational assumptions').

omega_variable(
    inflation_coupling_consistency,
    'When the coupling between the dark matter sector and the inflaton is fully specified, will the predicted dark matter isocurvature perturbations be consistent with CMB constraints (< 1% of adiabatic perturbations)?',
    'Construction of explicit inflation models with dark matter sector; calculation of isocurvature transfer functions; comparison to Planck constraints',
    'If isocurvature perturbations exceed CMB bounds: the dark matter production mechanism must be revised, potentially requiring late-time production or non-thermal mechanisms. If consistent: validates thermal production scenario.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inflation_coupling_consistency, empirical, 'Whether inflation coupling produces CMB-consistent isocurvature').

omega_variable(
    derivational_interdependence,
    'Are the five incomplete derivations (mass ratio, abundance, chirality, halo profile, inflation coupling) structurally independent, or will completing one derivation constrain or invalidate the assumptions used in the others?',
    'Systematic analysis of derivational dependencies; identification of shared assumptions and potential inconsistencies; construction of fully consistent models',
    'If strongly interdependent: completing derivations may reveal internal contradictions that invalidate the entire framework. If independent: derivations can be completed separately without risk of global inconsistency.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(derivational_interdependence, conceptual, 'Whether the five derivations are structurally independent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(incomplete_derivations_as_epistemic_load, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(incomp_deriv_tr_t0, incomplete_derivations_as_epistemic_load, theater_ratio, 0, 0.35).
narrative_ontology:measurement(incomp_deriv_tr_t10, incomplete_derivations_as_epistemic_load, theater_ratio, 10, 0.48).
narrative_ontology:measurement(incomp_deriv_tr_t20, incomplete_derivations_as_epistemic_load, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(incomp_deriv_be_t0, incomplete_derivations_as_epistemic_load, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(incomp_deriv_be_t10, incomplete_derivations_as_epistemic_load, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(incomp_deriv_be_t20, incomplete_derivations_as_epistemic_load, base_extractiveness, 20, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(incomplete_derivations_as_epistemic_load, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is downstream of dimensional_analogy_vs_lagrangian_derivation (the methodological choice to proceed with dimensional estimates rather than complete derivations). The upstream constraint is a mountain (the dimensional method is a legitimate tool in physics), but this downstream constraint (the epistemic load of proceeding with incomplete derivations) is a tangled rope (genuine coordination function combined with asymmetric extraction risk). The decomposition reflects that the method itself is not extractive, but the institutional practice of normalizing incompleteness creates extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
