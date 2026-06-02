% ============================================================================
% CONSTRAINT STORY: neurodiversity_spectrum
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_neurodiversity_spectrum, []).

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
 *   constraint_id: neurodiversity_spectrum
 *   human_readable: The Social/Medical Model of the Neurodiversity Spectrum
 *   domain: social/medical
 *
 * SUMMARY:
 *   The social/medical model of neurodiversity represents a fundamental
 *   reframing of neurodevelopmental variation from pathology to diversity.
 *   This constraint operates at the intersection of medical authority,
 *   disability rights advocacy, institutional design, and individual
 *   identity. The same structural phenomenon — neurodevelopmental difference
 *   from the neurotypical mean — appears as immutable biological pathology
 *   (medical model), liberation from pathology stigma (neurodiversity
 *   advocates), temporary medicalization to be overcome through institutional
 *   design (scaffold), performative diagnosis-seeking for access (piton),
 *   mixed coordination-extraction (tangled rope for medical system and
 *   diagnosed individuals), pure extraction (snare for undiagnosed trapped
 *   individuals), or natural human variation requiring no 'model' at all
 *   (mountain from analytical view, but false summit). The constraint's
 *   evolution shows decreasing theater (0.72 to 0.58) as diagnosis-seeking
 *   becomes more strategic and less performative, but increasing
 *   extractiveness (0.35 to 0.52) as the neurodiversity framework is absorbed
 *   into institutional systems that require conformity to neurodiversity
 *   identity categories rather than abandoning categorization itself. The
 *   fundamental mandatrophy is whether the shift from 'deficit' to
 *   'difference' language actually reduces extraction or merely rebrand it.
 *
 * KEY AGENTS:
 *   - Undiagnosed/Unsupported Neurodivergent: Primary victim (powerless/trapped) — trapped within institutional systems designed for neurotypical cognition; no exit options within dominant institutions
 *   - Diagnosed Neurodivergent Individuals: Secondary victims (moderate/constrained) — access accommodations and identity affirmation through diagnosis but remain constrained by gatekeeping, resource scarcity, and ongoing institutional pressure to conform
 *   - Neurodiversity Advocates & Organizations: Primary beneficiaries (institutional/arbitrage) — architects and beneficiaries of social model reframing; control discourse and institutional accommodation frameworks
 *   - Medical/Psychiatric Diagnostic System: Institutional actor with mixed interests (institutional/constrained) — benefits from expanded diagnostic categories; bears reputational extraction from medicalization critique; maintains gatekeeping authority
 *   - Institutional Reform Coalition: Organized reformers (organized/constrained) — building alternative pathways (universal design, neurodiversity-affirming institutions) with sunset logic reducing dependence on diagnosis
 *   - Biomedical Universalism Establishment: Institutional maintenance actors (institutional/arbitrage) — maintain diagnostic ritual through bureaucratic inertia; benefit from continued diagnostic requirement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(neurodiversity_spectrum, 0.52).
domain_priors:suppression_score(neurodiversity_spectrum, 0.65).
domain_priors:theater_ratio(neurodiversity_spectrum, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(neurodiversity_spectrum, extractiveness, 0.52).
narrative_ontology:constraint_metric(neurodiversity_spectrum, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(neurodiversity_spectrum, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(neurodiversity_spectrum, tangled_rope).
narrative_ontology:human_readable(neurodiversity_spectrum, "The Social/Medical Model of the Neurodiversity Spectrum").
narrative_ontology:topic_domain(neurodiversity_spectrum, "social/medical").

domain_priors:requires_active_enforcement(neurodiversity_spectrum).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(neurodiversity_spectrum, neurodiversity_advocates).
narrative_ontology:constraint_beneficiary(neurodiversity_spectrum, disability_rights_organizations).
narrative_ontology:constraint_beneficiary(neurodiversity_spectrum, progressive_medical_practitioners).
narrative_ontology:constraint_victim(neurodiversity_spectrum, neurotypical_norm_enforcers).
narrative_ontology:constraint_victim(neurodiversity_spectrum, diagnostic_gatekeepers).
narrative_ontology:constraint_victim(neurodiversity_spectrum, standardized_institutional_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNDIAGNOSED/UNSUPPORTED NEURODIVERGENT (SNARE) — Trapped within institutional systems (education, workplace, healthcare) designed for neurotypical cognition. Cannot exit without bearing severe costs (homeschooling, unemployment, social isolation). Bears full extraction: forced masking, pathologization, institutional pressure to conform. Maximum experienced extraction — no alternatives available within dominant institutional frameworks.
constraint_indexing:constraint_classification(neurodiversity_spectrum, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DIAGNOSED NEURODIVERGENT WITH SUPPORT ACCESS (TANGLED ROPE) — Can access accommodations and identity affirmation through formal diagnosis, but constrained by gatekeeping practices, resource scarcity, and stigma. Benefits from neurodiversity framework (validation, community) but bears ongoing extraction (compliance with diagnostic criteria, insurance requirements, institutional surveillance). Mixed experience — genuine coordination gains alongside structural asymmetry.
constraint_indexing:constraint_classification(neurodiversity_spectrum, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: NEURODIVERSITY ADVOCATES & ORGANIZATIONS (ROPE) — Primary architects and beneficiaries of the social model reframing. Experience the constraint as coordination: redefining neurodivergence as variation rather than pathology enables coalition-building, resource allocation, and institutional accommodation frameworks. Net beneficiary position — extraction runs toward advocates, not away. Arbitrage exit: can shape discourse, build alternative institutions, secure funding.
constraint_indexing:constraint_classification(neurodiversity_spectrum, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MEDICAL/PSYCHIATRIC DIAGNOSTIC SYSTEM (TANGLED ROPE) — Constrained by the shift from pathology model to neurodiversity model. Benefits from expanded diagnostic categories (increased patient population, insurance billing, pharmaceutical markets) but bears reputational extraction from disability rights critique of medicalization. Provides genuine coordination function (diagnosis enables access to accommodations) while extracting through gatekeeping and normalization of medical authority. Active enforcement required — maintains diagnostic criteria, billing systems, institutional legitimacy.
constraint_indexing:constraint_classification(neurodiversity_spectrum, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INSTITUTIONAL REFORM COALITION (SCAFFOLD) — Organized agents (universal design advocates, neurodiversity-informed educators, progressive employers) see the medicalization bottleneck as temporary. Building parallel institutional pathways (universal design in education, neurodiversity-affirming workplaces, peer support networks) that reduce dependence on diagnostic gatekeeping. Sunset logic: as institutional design improves to accommodate neurodiversity without pathologization, the medical model's extraction mechanism loses force. Estimated timeline: 15-25 years for educational and workplace norms to mature in high-income nations.
constraint_indexing:constraint_classification(neurodiversity_spectrum, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: BIOMEDICAL UNIVERSALISM (PITON) — The assumption that a single biological/psychiatric classification system can and should apply globally persists through institutional inertia despite evidence of cultural variability, epistemic injustice in diagnostic categories, and disconnection from lived experience. Theater ratio (0.58) reflects performative diagnosis-seeking for access rather than actual clinical validation. The constraint maintains itself through bureaucratic ritual: diagnostic label-seeking is required to access accommodations, even when both the individual and practitioners understand the label as socially constructed.
constraint_indexing:constraint_classification(neurodiversity_spectrum, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL VARIATION VIEW (MOUNTAIN) — From a civilizational perspective, neurodevelopmental variation is a natural feature of human cognitive diversity. No society can 'solve' or 'cure away' the full spectrum of cognitive variation. Some extraction and suppression related to institutional design are irreducible — all institutional systems make tradeoffs that privilege some cognitive styles and burden others. However, the degree of extraction (0.52) and suppression (0.65) are NOT natural law signatures — they reflect contingent institutional choices, not immutable structural features. The engine's false summit detector will identify this as naturalization.
constraint_indexing:constraint_classification(neurodiversity_spectrum, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(neurodiversity_spectrum_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(neurodiversity_spectrum, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(neurodiversity_spectrum, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(neurodiversity_spectrum, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(neurodiversity_spectrum, TR),
    TR >= 0.70.

:- end_tests(neurodiversity_spectrum_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The neurodiversity framework has shifted from pure pathology (medical model extraction at 0.65-0.70) to mixed coordination-extraction. Coordination gains are real: identity affirmation, reduced stigma language, accommodation frameworks. But extraction persists: diagnosis remains gatekeeping mechanism, institutional systems still require conformity to neurodiversity identity labels, and undiagnosed individuals remain trapped. The value (0.52) reflects genuine progress in coordination but incomplete elimination of extraction. Suppression (0.65): High. Significant barriers remain: gatekeeping through diagnostic criteria, resource scarcity for accommodations, masking pressure in institutional contexts, stigma despite identity affirmation language, and exclusion of high-support-needs individuals from neurodiversity community. Institutional systems still suppress unconcealed neurodiversity through workplace productivity demands, educational pacing norms, and social conformity expectations. Theater ratio (0.58): Moderate-high. Diagnosis-seeking has become increasingly strategic — individuals seek diagnosis for access to accommodations rather than because diagnostic labels match lived experience. The constraint maintains itself through bureaucratic requirement (diagnosis needed for accommodations) even when both individual and practitioner understand the label as socially constructed. The decreasing theater trajectory (0.72 to 0.58) reflects shift from performative pathology-proving to strategic label-acquiring for resources.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural feature (neurodevelopmental variation requiring classification for institutional accommodation) generates fundamentally different classifications depending on agent position. Advocates see liberation (rope, tangible coordination gains from identity affirmation and acceptance). Medical practitioners see necessary professionalization (tangled rope, mixed service provision and gatekeeping). Undiagnosed individuals see only barriers (snare, trapped by systems requiring diagnosis they lack). Institutional reformers see temporary problem (scaffold, alternative design pathways emerging). The medical establishment's own perspective is piton — practitioners recognize diagnosis as partially performative (theater_ratio 0.58) yet maintain it through institutional requirement. The analytical observer risks false summit by treating neurodiversity itself (not institutional design choices) as the constraint. The perspectival gaps reveal that the 'solution' (neurodiversity framework) has itself become partially extractive — it has shifted extraction from 'you are broken' (medical model) to 'you must perform neurodiversity identity' (identity politics extraction) to 'you must get diagnosed to access accommodations' (institutional gatekeeping).
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is derived from structural position relative to the constraint. Undiagnosed trapped individuals experience high d (0.95) — full targets bearing extraction with no exit. Diagnosed individuals experience moderate d (0.55-0.65) — genuine benefits from framework (identity, accommodation access) but constrained by gatekeeping and ongoing institutional pressure. Neurodiversity advocates experience low d (0.10-0.20) — beneficiaries with arbitrage options (can shape discourse, build alternative institutions). Medical system experiences modified d (0.45-0.55) — benefits from diagnostic expansion but bears reputational cost from disability rights critique. Reform coalition experiences moderate d (0.40-0.50) — constrained by institutional inertia but has real agency to build alternatives. The piton classification derives from theater gate and institutional inertia: diagnostic ritual persists not because it effectively serves clinical function but because access systems are built on it. The mountain classification at analytical level risks naturalizing the extractive components as inherent to any classification system.
 *
 * MANDATROPHY ANALYSIS:
 *   CRITICAL CASE: The neurodiversity framework was developed specifically to resolve the mandatrophy — to distinguish coordination (celebration of diversity, identity affirmation, reasonable accommodations) from extraction (pathologization, normalization coercion, forced conformity). The framework argues: the social model is rope (pure coordination via social adaptation), while the medical model is snare (pure extraction via pathology labels). But the structural data shows this was partially mislabeled. The actual constraint is tangled_rope: the modern neurodiversity framework provides genuine coordination gains (identity, accommodation access, reduced stigma) but embeds new extraction mechanisms (diagnosis gatekeeping, identity conformity, institutional surveillance through diagnostic labels, exclusion of high-support-needs individuals from the neurodiversity 'celebration'). The theater ratio trajectory (0.72→0.58) shows movement toward reduced performative diagnosis-proving, but extractiveness trajectory (0.35→0.52) shows increasing constraint as neurodiversity becomes institutionalized. The mandatrophy is NOT resolved — it is inverted: the solution (neurodiversity framework) has become partially extracted itself. The true mandatrophy resolution would require: (1) decoupling accommodation access from diagnostic gatekeeping, (2) distinguishing neurodiversity (natural variation) from disability (support-requiring conditions), (3) removing masking pressure, and (4) building institutional capacity for unconcealed neurodiversity. Currently the framework provides language for coordination while maintaining structural extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    neurodiversity_vs_disability_distinction,
    'Is neurodiversity a neutral descriptor of cognitive variation, or does it collapse the distinction between neurotypes that cause genuine disability and those that merely differ from the norm?',
    'Empirical analysis of support service accessibility and quality of life outcomes for different neurodivergent subgroups; distinction between co-occurring conditions (intellectual disability, speech differences requiring support) vs. pure cognitive style variation',
    'If collapse occurs: neurodiversity framework masks support gaps for high-support-needs individuals; moderate extraction persists. If distinction maintained: framework enables resource allocation to genuinely disabled individuals while decoupling identity affirmation from support access.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neurodiversity_vs_disability_distinction, empirical, 'Whether neurodiversity framing masks support gaps for high-need individuals').

omega_variable(
    diagnostic_gatekeeping_necessity,
    'Is formal psychiatric diagnosis a necessary mechanism for accessing accommodations, or is it a contingent institutional design choice?',
    'Comparative analysis of accommodation access systems (universal design vs. diagnosis-based) in educational and employment settings; measurement of accommodation utilization rates before and after diagnostic requirement removal',
    'If necessary: diagnosis is genuine coordination mechanism; extraction component is lower-bound (0.35-0.40). If contingent: diagnosis is pure gatekeeping; extraction increases (0.60+) and accommodation access should be decoupled from pathology labels.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(diagnostic_gatekeeping_necessity, empirical, 'Whether diagnostic gatekeeping is necessary for accommodation access').

omega_variable(
    cultural_specificity_of_neurodiversity,
    'Do neurodiversity categories (autism, ADHD, dyslexia) reflect universal biological subtypes or culturally specific constructions reflecting Western education and workplace standards?',
    'Cross-cultural analysis of neurodevelopmental variation; comparison of diagnostic prevalence across different educational and occupational structures; analysis of neurodiversity language and concepts in non-Western frameworks',
    'If universal: categories apply globally; Western institutional design is one context among many. If culturally specific: neurodiversity framework risks exporting Western medicalization globally; indigenous and non-Western conceptualizations of cognitive difference are suppressed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cultural_specificity_of_neurodiversity, conceptual, 'Whether neurodiversity categories are culturally universal or specific').

omega_variable(
    masking_cost_vs_adaptation_benefit,
    'Does requiring neurodivergent individuals to mask (suppress neurodivergent traits in institutional contexts) cause net harm compared to the institutional adaptation costs of accommodating unconcealed neurodiversity?',
    'Longitudinal study of mental health, wellbeing, and functioning outcomes for masked vs. unmasked neurodivergent individuals controlling for support access; measurement of institutional adaptation costs in accommodation scenarios',
    'If masking causes net harm: suppression metric should increase (0.70+); removal of masking pressure becomes key to lowering extraction. If adaptation costs exceed masking costs: suppression justified as necessary tradeoff; extraction component is legitimate coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(masking_cost_vs_adaptation_benefit, empirical, 'Whether masking causes net psychological harm').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(neurodiversity_spectrum, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(neuro_tr_t0, neurodiversity_spectrum, theater_ratio, 0, 0.72).
narrative_ontology:measurement(neuro_tr_t5, neurodiversity_spectrum, theater_ratio, 5, 0.65).
narrative_ontology:measurement(neuro_tr_t10, neurodiversity_spectrum, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(neuro_be_t0, neurodiversity_spectrum, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(neuro_be_t5, neurodiversity_spectrum, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(neuro_be_t10, neurodiversity_spectrum, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(neurodiversity_spectrum, resource_allocation).
narrative_ontology:affects_constraint(neurodiversity_spectrum, medical_model_pathologization).
narrative_ontology:affects_constraint(neurodiversity_spectrum, institutional_accommodation_access).
narrative_ontology:affects_constraint(neurodiversity_spectrum, disability_identity_politics).

% DUAL FORMULATION NOTE:
% The neurodiversity spectrum constraint decomposes into three structurally distinct claims: (1) Medical pathology model (ε ≈ 0.70, snare) — neurodevelopmental difference as disease requiring cure; (2) Neurodiversity identity framework (ε ≈ 0.52, tangled_rope) — neurodevelopmental difference as natural variation to be affirmed; (3) Institutional accommodation access mechanism (ε ≈ 0.45, rope/scaffold) — resource allocation for support needs. The medical model and neurodiversity framework operate at different compression levels: the medical model is extracted FROM individuals; the neurodiversity framework is extracted through individuals via gatekeeping and identity conformity. These are not different observations of one constraint — they have different ε values reflecting different structural mechanisms. The network links show downstream effects: institutional accommodation access depends on both medical model infrastructure (diagnosis for insurance) and neurodiversity framework (identity affirmation enabling accommodation requests), creating a hybrid dependency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(neurodiversity_spectrum, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
