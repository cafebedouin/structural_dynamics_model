% ============================================================================
% CONSTRAINT STORY: public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_health_primary, []).

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
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: public_health_primary
 *   human_readable: Public Health Intervention Legitimacy (Population Morbidity Reduction Primary)
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint story models ONE READING of the contested kernel
 *   'legitimate_health_intervention' — specifically, the
 *   'public_health_primary' reading that derives legitimacy from measurable
 *   reduction in population-level morbidity and mortality, and that frames
 *   individual refusal as externality imposition. Under this reading,
 *   unvaccinated individuals are victim-classified (bearing externality costs
 *   they impose, and bearing suppression costs of enforcement), while
 *   immunocompromised and high-transmission-risk populations are
 *   beneficiary-classified (experiencing protection via herd immunity
 *   coordination). The constraint exhibits tangled_rope structure: it
 *   possesses a genuine coordination function (preventing disease spread
 *   among vulnerable populations) alongside asymmetric extraction (employment
 *   termination, school exclusion, access restrictions applied to
 *   unvaccinated individuals). The extractiveness has risen substantially
 *   over the measurement interval (from 0.15 at policy initiation to 0.58 at
 *   peak enforcement), reflecting drift from epidemiologically-justified
 *   restrictions toward enforcement mechanisms that persist despite reduced
 *   disease prevalence. Theater ratio remains moderate (0.48) because the
 *   constraint retains empirical grounding in transmission science — it is
 *   not yet a purely performative piton — but rising theater indicates
 *   increasing disconnection between stated epidemiological justification and
 *   actual enforcement patterns. The kernel reading choice is critical: a
 *   sibling constraint (bodily_autonomy_primary) would invert the
 *   beneficiary/victim relationship, classify vaccination mandates as snare,
 *   and argue that individual autonomy is the legitimacy metric. Both
 *   readings are coherent; neither is 'naturally' true. The framework handles
 *   this through constraint decomposition, not by forcing readings into a
 *   single story.
 *
 * KEY AGENTS:
 *   - Immunocompromised Populations: Primary beneficiary (institutional/arbitrage) — protected by herd immunity; experience constraint as pure coordination enabling baseline functioning
 *   - Vaccine-Hesitant Individuals: Primary victim (moderate/constrained) — face high-cost exit (relocation, private employment, social isolation) or compliance with medical intervention they distrust
 *   - Medically Contraindicated Individuals: Structural victim (powerless/trapped) — no exit option; cannot vaccinate safely AND cannot access employment/education without vaccination; maximum experienced extraction
 *   - Public Health Authorities: Institutional beneficiary (institutional/arbitrage) — derive legitimacy and mandate scope from measurable mortality reduction; can revise policy without bearing costs of prior enforcement
 *   - Anti-Vaccination Organizations: Organized actor (organized/constrained) — provide epistemic alternative and informational coordination alongside facing institutional suppression of platforms and policy voice
 *   - Medical Licensing System: Institutional actor (institutional/mobile) — maintains performative authority through licensing requirements while actual clinical judgment is overridden by population-level protocols (piton perspective)
 *   - Proportionality Advocates: Organized coalition (organized/constrained) — arguing for sunset logic through risk-proportionate policy based on immunity threshold rather than universal mandates
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_primary, 0.58).
domain_priors:suppression_score(public_health_primary, 0.62).
domain_priors:theater_ratio(public_health_primary, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_primary, extractiveness, 0.58).
narrative_ontology:constraint_metric(public_health_primary, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(public_health_primary, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_primary, tangled_rope).
narrative_ontology:human_readable(public_health_primary, "Public Health Intervention Legitimacy (Population Morbidity Reduction Primary)").
narrative_ontology:topic_domain(public_health_primary, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(public_health_primary, formalized).
narrative_ontology:cs_authority_grounding(public_health_primary, extraction).
narrative_ontology:cs_interpretation_layer_present(public_health_primary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_primary, immunocompromised_populations).
narrative_ontology:constraint_beneficiary(public_health_primary, high_transmission_environments).
narrative_ontology:constraint_beneficiary(public_health_primary, vulnerable_age_groups).
narrative_ontology:constraint_victim(public_health_primary, vaccine_hesitant_individuals).
narrative_ontology:constraint_victim(public_health_primary, religious_objection_groups).
narrative_ontology:constraint_victim(public_health_primary, medical_contraindication_edge_cases).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MEDICALLY CONTRAINDICATED (SNARE) — Faces binary choice with no escape: accept exclusion from employment/public spaces or accept medical risk. No exit option exists; suppression is structural (cannot work without vaccination despite legitimate medical barrier). This agent experiences maximum extraction of bodily/economic autonomy.
constraint_indexing:constraint_classification(public_health_primary, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: VACCINE-HESITANT PARENT (SNARE) — Faces high-cost exit (relocation, private schooling, social isolation) or compliance. Suppression operates through institutional barriers (school exclusion, employment requirements) and epistemic asymmetry (medical expertise concentrated in state apparatus). Extraction manifests as forced medical intervention or deprivation of access. Moderate power gives illusory agency but constrained exit options generate snare dynamics.
constraint_indexing:constraint_classification(public_health_primary, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ANTI-VAX ADVOCACY (TANGLED ROPE) — Organized agents experience genuine coordination function (disseminating medical information, building epistemic alternatives) alongside extraction (public health authorities suppress counter-narratives, restrict media platforms, exclude these organizations from policy deliberation). Organized power provides some agency; constraints are institutional rather than material. The coordination function (alternative medical expertise claim) is real but contested as to empirical validity.
constraint_indexing:constraint_classification(public_health_primary, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: IMMUNOCOMPROMISED (ROPE) — Primary beneficiary. High vaccination rates reduce disease exposure, enabling participation in employment and public life that would otherwise be catastrophic health risk. Benefits from herd immunity coordination mechanism. Experiences the constraint as pure coordination — enables baseline functioning. Institutional power and arbitrage exit (can benefit under current regime without cost) generate rope classification.
constraint_indexing:constraint_classification(public_health_primary, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PUBLIC HEALTH AUTHORITIES (TANGLED ROPE) — Institutional actors see genuine coordination function (preventing disease spread, protecting vulnerable populations) alongside asymmetric extraction of bodily autonomy from non-compliant groups. Enforcement mechanisms (employment termination, access restrictions, school exclusion) generate extraction. Authorities benefit from legitimacy derived from measurable mortality reduction. Arbitrage position: they can exit constraints through policy revision without bearing costs of their own policies.
constraint_indexing:constraint_classification(public_health_primary, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: MEDICAL CREDENTIALING SYSTEM (PITON) — Maintains performative authority over medical practice through licensing and standards enforcement, increasingly disconnected from actual clinical judgment. Theater manifests in requirement for physician recommendations despite population-level policy override of physician discretion. Degraded function: individual physician expertise is invoked for legitimacy but overridden by population-level protocols. Persists through institutional inertia and regulatory framework coupling.
constraint_indexing:constraint_classification(public_health_primary, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: PROPORTIONALITY ADVOCATES (SCAFFOLD) — Organized agents arguing for differential requirements based on prior infection status, age-stratified risk, or medical contraindication. See constraint as temporary coordination failure solvable by risk-proportionate policy. Sunset clause embedded in logic: once population immunity reaches threshold through vaccination + prior infection, universal requirements become unnecessary. Constrained exit but perceives structural exit path.
constraint_indexing:constraint_classification(public_health_primary, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational scope, disease transmission imposes unavoidable externalities: unvaccinated individuals increase infection risk to others, and this externality is inherent to epidemiology, not constructed by policy. Individual refusal necessarily imposes costs on the population. This perspective sees the constraint as a natural law of epidemiology: externalities require coordination mechanisms. However, this naturalizes what the committer frame reveals as a reading choice: whether to measure legitimacy through population outcomes (this reading) or through individual bodily autonomy (sibling reading).
constraint_indexing:constraint_classification(public_health_primary, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_health_primary_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(public_health_primary, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(public_health_primary, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(public_health_primary, TR),
    TR >= 0.70.

:- end_tests(public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts bodily autonomy (forced vaccination or medical exclusion), economic autonomy (employment termination), and social participation (school exclusion, access restrictions). However, extractiveness is not maximal (0.66+) because (a) genuine disease transmission externalities exist, (b) some populations (immunocompromised) experience net benefit, and (c) enforcement is justified by measurable epidemiological outcomes at time of application. The rise from 0.15 to 0.58 over the interval reflects enforcement drift: extractiveness was low when vaccination coverage was needed and disease prevalence high; extractiveness rose as disease prevalence declined and enforcement persisted. Suppression (0.62): High. Multiple suppression mechanisms operate: institutional barriers (employment, education access), epistemic suppression (restriction of alternative medical expertise, platform removal of dissenting physicians), legal uncertainty (exemption processes are opaque), and identity suppression (vaccine refusal framed as dangerous irresponsibility rather than legitimate medical/ethical choice). Theater ratio (0.48): Moderate. The constraint retains empirical grounding in transmission science and population-level mortality reduction (low theater), but enforcement mechanisms increasingly operate through performative channels (showing workplace compliance, demonstrating exclusion) rather than epidemiological measurement. Theater has risen as disease prevalence declined and enforcement continued — the contrast reveals drift.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the diagnostic power of indexical classification under kernel reading conditions. The immunocompromised beneficiary sees rope (pure coordination). Public health authorities see rope (their perspective) or tangled_rope (external analysis). The vaccine-hesitant individual sees snare (exit blocked). Medical authorities see piton (performative ritual). Proportionality advocates see scaffold (sunset logic). Anti-vax organizations see tangled_rope (mixed coordination and suppression). The medically contraindicated see snare (no exit). The analytical observer risks seeing mountain (natural law of externalities) but the committer frame reveals this as naturalization of a reading choice. The gap reveals that 'legitimacy' is not discovered empirically; it is constituted by choice of measurement metrics (population outcomes vs individual autonomy). This is not perspectival relativism — the empirical facts (disease transmission, vaccine efficacy, side effects) are fixed. But the normative frame that determines which facts are relevant to legitimacy is a reading choice, not a natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's experienced extractiveness (χ) is computed from base extractiveness (ε=0.58), directionality (d derived from beneficiary/victim status + power + exit options), and scope (σ=1.0 for national scope). Beneficiaries (immunocompromised, authorities) derive low d values (0.10-0.25), producing negative or negligible χ — they experience the constraint as beneficial or costless. Victims with trapped exit (medically contraindicated) derive high d (0.95), producing χ ≈ 0.82 — they experience maximum extraction. Organized victims (anti-vax organizations) derive moderate-high d (0.50-0.65), producing χ ≈ 0.40-0.50 — they experience significant extraction but retain some organizational agency. The perspectival gap (rope vs snare vs tangled_rope) does NOT derive from different χ values; it derives from whether the agent perceives the extraction as necessary coordination cost (rope perspective), as extraction with mixed coordination (tangled_rope), or as pure extraction without coordination benefit (snare). The classification type reflects narrative interpretation of whether coordination function is genuine, not merely from χ magnitude.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy (threat of mislabeling pure extraction as coordination or vice versa) by showing how the kernel reading choice determines which classification is correct. Under PUBLIC_HEALTH_PRIMARY reading: unvaccinated individuals are externality imposers, their extraction is justified, the constraint is tangled_rope with genuine coordination function. Under the bodily_autonomy_primary sibling reading: mandatory vaccination is bodily autonomy extraction regardless of disease benefits, unvaccinated are victims exercising autonomy, the constraint is snare or tangled_rope with extraction dominating. Both readings satisfy all consistency conditions; neither can be rejected as empirically false. The mandatrophy is resolved not by choosing the 'true' type but by making the reading choice explicit and generating both constraints as separate stories. The corpus includes both; researchers using either constraint are accountable for their legitimacy metric choice. This is why the framework requires separate constraint stories for kernel readings rather than a single 'public health constraint' with measurement parameters.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_reading_choice,
    'Is the externality structure (unvaccinated → disease transmission → population risk) a natural law of epidemiology, or a policy choice about which harms to measure and which to center?',
    'Epistemological analysis: distinguish between empirical facts (disease transmission is real) and policy frame (population morbidity reduction is the legitimacy criterion). The sibling reading (bodily_autonomy_primary) would measure legitimacy through individual consent instead, making identical transmission facts support opposite conclusions.',
    'If natural law: mountain classification stands, extraction is necessary cost of coordination. If reading choice: mountain is false summit, constraint reclassifies as snare or tangled_rope depending on whether enforcement mechanisms are justified by chosen legitimacy metric.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_reading_choice, conceptual, 'Whether externality structure is natural law or policy reading').

omega_variable(
    harm_quantification_asymmetry,
    'How do we measure and compare harms: vaccination side effects (rare, distributed across vaccinated population) vs disease transmission risks (concentrated on unvaccinated and their contacts)?',
    'Epidemiological data: expected value calculations for each policy option across full risk distribution. Compare mortality/morbidity from vaccine adverse events vs from disease outcomes. Identify which harms are measured and which are externalized.',
    'If vaccine harms are negligible: snare classification of unvaccinated strengthens (they bear full externality cost). If vaccine harms are material: tangled_rope classification strengthens (mutual harm distribution). High measurement uncertainty here drives extractiveness uncertainty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_quantification_asymmetry, empirical, 'Quantification and weighting of distributed vaccine harms vs concentrated disease risks').

omega_variable(
    enforcement_proportionality_drift,
    'Do enforcement mechanisms (employment termination, school exclusion, access restrictions) remain proportionate to measurable disease risk reduction, or have they drifted beyond epidemiological justification into punitive territory?',
    'Temporal analysis: track enforcement severity over time relative to (a) disease prevalence, (b) vaccination coverage, (c) vaccine efficacy data, (d) alternative risk mitigation measures. Identify lag between changing epidemiology and policy revision.',
    'If proportionality maintained: tangled_rope with genuine coordination function. If proportionality has drifted: snare classification strengthens, theater_ratio rises (extraction continues despite epidemiological justification collapse). This is the key mandatrophy mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_proportionality_drift, empirical, 'Whether enforcement mechanisms remain proportionate to epidemiological evidence').

omega_variable(
    medical_contraindication_boundary,
    'How are legitimate medical contraindications identified and exempted? Is the boundary determined by individual physician judgment, population-level studies, or algorithmic screening?',
    'Policy analysis: compare stated contraindication criteria across jurisdictions and time periods. Identify cases where individual physician assessment conflicts with population-level policy. Track exemption approval rates and appeal procedures.',
    'If physician judgment controls: snare classification of contraindicated individuals weakens (they have exit path via medical exemption). If population-level policy controls: snare classification strengthens (individual clinical factors are overridden). This determines whether trapped classification is accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(medical_contraindication_boundary, empirical, 'Boundary determination for medical contraindications and exemption procedures').

omega_variable(
    kernel_reading_sibling_tension,
    'This constraint instantiates the ''public_health_primary'' reading of the legitimate_health_intervention kernel. The bodily_autonomy_primary sibling reading would declare unvaccinated individuals as beneficiaries (exercise of autonomy) and immunocompromised populations as partial victims (constrained by others'' autonomy choices). How should the framework handle these inversions?',
    'Cross-reading comparison: generate bodily_autonomy_primary story with inverted beneficiary/victim declarations. Compare ε, χ, and classification across readings. The framework handles this through constraint decomposition — two stories, two constraint_ids, linked via network.affects_constraints. This is not observable-dependence (which would be violation of ε-invariance); it is kernel reading choice (which is legitimate).',
    'If readers treat both constraints as equally valid: constraint stories function as epistemic devices revealing policy choice rather than discovering natural fact. If readers privilege public_health_primary: constraint story naturalizes one reading while suppressing sibling, instantiating the false summit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_sibling_tension, conceptual, 'How constraint framework handles kernel reading inversions and legitimacy metric choice').

omega_variable(
    epistemic_authority_closure,
    'Does the exclusive grant of epidemiological authority to public health agencies (suppressing alternative medical expertise, restricting information from vaccine-hesitant physicians, excluding dissenting experts from policy deliberation) constitute necessary coordination or extractive suppression of legitimate alternatives?',
    'Comparative epistemology: identify claims made by public health authorities that have been contradicted by subsequent evidence (e.g., vaccine sterility claims, duration of immunity, variant susceptibility). For each reversal, determine whether alternative experts were suppressed for making correct early claims.',
    'If authority claims are consistently borne out by evidence: epistemic monopoly is coordination function, suppression is legitimate. If authority claims are repeatedly contradicted: suppression of alternatives generates extraction masquerading as coordination. High impact on theater_ratio and suppression magnitude.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_authority_closure, empirical, 'Whether epistemic authority closure is necessary coordination or extractive expertise monopoly').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_primary, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(publ_tr_t0, public_health_primary, theater_ratio, 0, 0.28).
narrative_ontology:measurement(publ_tr_t6, public_health_primary, theater_ratio, 6, 0.38).
narrative_ontology:measurement(publ_tr_t12, public_health_primary, theater_ratio, 12, 0.48).

% Extraction over time
narrative_ontology:measurement(publ_be_t0, public_health_primary, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(publ_be_t6, public_health_primary, base_extractiveness, 6, 0.42).
narrative_ontology:measurement(publ_be_t12, public_health_primary, base_extractiveness, 12, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_primary, resource_allocation).
narrative_ontology:affects_constraint(public_health_primary, bodily_autonomy_primary).
narrative_ontology:affects_constraint(public_health_primary, proportionality_reading).
narrative_ontology:affects_constraint(public_health_primary, epistemic_authority_medical).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the contested kernel 'legitimate_health_intervention' into three constraint stories representing different legitimacy readings. public_health_primary measures legitimacy through population outcomes. bodily_autonomy_primary measures legitimacy through individual consent. proportionality_reading measures legitimacy through risk-proportionate policy. All three are structurally decomposable from the kernel; none is the 'true' constraint. The framework models them as separate stories linked via network.affects_constraints, each with its own ε, beneficiary/victim declarations, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(public_health_primary, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
