% ============================================================================
% CONSTRAINT STORY: disability_access_infrastructure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_disability_access_infrastructure, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: disability_access_infrastructure
 *   human_readable: Disability Access Infrastructure as Tangled Rope Coordination-Extraction Hybrid
 *   domain: social/economic/institutional
 *
 * SUMMARY:
 *   Disability access infrastructure encompasses the built environment,
 *   digital systems, institutional practices, and resource allocation
 *   mechanisms designed to enable disabled people's participation in social,
 *   economic, and civic life. This constraint exhibits the full spectrum of
 *   DR classification because different structural actors experience the same
 *   infrastructure as serving radically different functions: coordination,
 *   extraction, degradation, and immutable law. The disabled population
 *   experiences it as a snare — trapped by physical environment and
 *   institutional barriers with minimal exit options. Accessibility advocates
 *   experience it as extractive (snare from identity-locked position) or
 *   mixed (tangled rope from organized coalition position). Institutional
 *   beneficiaries (urban planners, developers, administrators) experience it
 *   as coordination mechanism enabling their own market participation. The
 *   ADA compliance framework exhibits high theater (0.62) because regulatory
 *   compliance is substantially performative: ramps exist but are poorly
 *   maintained, accessible parking is misused, accessible websites fail
 *   audits. The constraint has modestly improved over the 10-year interval
 *   (extractiveness declining from 0.72 to 0.58) due to increasing organized
 *   advocacy and emerging universal design norms, but suppression remains
 *   high (0.68) because the constraint remains structurally embedded in built
 *   environment, economic incentives, and institutional power asymmetries.
 *
 * KEY AGENTS:
 *   - Disabled Population: Primary victim (powerless/trapped) — trapped by physical environment and legal/economic barriers; cannot exit constraint; bears mobility/autonomy extraction
 *   - Accessibility Advocates: Secondary victim (powerless/identity_locked) — structurally mobile but identity-fused with advocacy field; perceives constraint as unchangeable within biographical horizon due to identity frame
 *   - Disability Rights Coalition: Organized victim group (organized/constrained) — advocacy organizations, disability service providers, legal advocates; have some agency and exit paths but face significant constraints
 *   - Urban Planners: Primary beneficiary (institutional/arbitrage) — capture benefits of accessibility standards while distributing costs to disabled population; experience constraint as enabling coordination
 *   - Real Estate Developers: Primary beneficiary (institutional/arbitrage) — can extract value through accessibility premium, segregation, or minimal compliance; highly mobile
 *   - Municipal Administrators: Powerful hybrid actor (powerful/mobile) — face genuine coordination problems but also extract through selective enforcement and regulatory capture
 *   - ADA Compliance System: Institutional framework (institutional/arbitrage) — maintains performative accessibility through inertia and regulatory capture; theater persists despite degraded function
 *   - Universal Design Movement: Organized future-oriented agent (organized/constrained) — sees accessibility as temporary coordination problem being resolved by design integration; perceives sunset pathway
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional choices as immutable laws of human diversity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(disability_access_infrastructure, 0.58).
domain_priors:suppression_score(disability_access_infrastructure, 0.68).
domain_priors:theater_ratio(disability_access_infrastructure, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(disability_access_infrastructure, extractiveness, 0.58).
narrative_ontology:constraint_metric(disability_access_infrastructure, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(disability_access_infrastructure, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(disability_access_infrastructure, tangled_rope).
narrative_ontology:human_readable(disability_access_infrastructure, "Disability Access Infrastructure as Tangled Rope Coordination-Extraction Hybrid").
narrative_ontology:topic_domain(disability_access_infrastructure, "social/economic/institutional").

domain_priors:requires_active_enforcement(disability_access_infrastructure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(disability_access_infrastructure, urban_planners).
narrative_ontology:constraint_beneficiary(disability_access_infrastructure, real_estate_developers).
narrative_ontology:constraint_beneficiary(disability_access_infrastructure, municipal_administrators).
narrative_ontology:constraint_victim(disability_access_infrastructure, disabled_population).
narrative_ontology:constraint_victim(disability_access_infrastructure, accessibility_advocates).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISABLED PERSON (SNARE) — Trapped by physical environment and legal/economic barriers. Cannot exit the constraint (must navigate cities/buildings to participate in employment, healthcare, social life). Bears full cost of inaccessible infrastructure. No alternatives; maximum suppression. The constraint extracts mobility, autonomy, and participation.
constraint_indexing:constraint_classification(disability_access_infrastructure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: ACCESSIBILITY ADVOCATE (SNARE/IDENTITY_LOCKED) — Structurally mobile (could change careers, leave advocacy field) but identity-fused with the accessibility movement. Professional identity, social bonds, and moral commitment are constituted through this constraint. Exit would require abandoning the identity frame. Perceives constraint as unchangeable within biographical horizon because the frame that would permit perceiving change is inaccessible from within their identity. High experienced extraction through epistemic closure and identity fusion.
constraint_indexing:constraint_classification(disability_access_infrastructure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: DISABILITY RIGHTS COALITION (TANGLED_ROPE) — Organized agents with some agency and exit paths (can lobby, litigate, build alternative accessible spaces) but face significant constraints (resource barriers, political opposition, legal complexity). Experience both coordination (standards development, mutual support) and extraction (barriers to full participation). Coalition power moderates experienced extraction from snare levels but does not eliminate it.
constraint_indexing:constraint_classification(disability_access_infrastructure, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: URBAN PLANNER (ROPE) — Experiences constraint as coordination mechanism solving genuine collective action problem: how to integrate mobility needs into built environment design. Benefits from standards (ADA, accessibility codes) that provide arbitrage opportunity (can profit from accessible design while appearing compliant). Sees the constraint as enabling rather than extractive because they capture the benefits of coordination while distributing costs to disabled population.
constraint_indexing:constraint_classification(disability_access_infrastructure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 5: REAL ESTATE DEVELOPER (ROPE) — Experiences accessibility requirements as coordination mechanism that enables market participation. Can extract value through accessibility premium (marketing accessible units as luxury), segregation (accessible housing as separate product line), or minimal compliance (meeting letter of law with minimal cost). High arbitrage capacity — can exit to markets with lower accessibility standards, can lobby for regulatory weakening, can lobby for public subsidies. Perceives constraint as manageable coordination.
constraint_indexing:constraint_classification(disability_access_infrastructure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: MUNICIPAL ADMINISTRATOR (TANGLED_ROPE) — Faces genuine coordination problems (integrating accessibility into existing infrastructure, managing budget constraints) but also extracts from disabled population through selective enforcement, regulatory capture by developers, and selective investment in accessible infrastructure. Mobile enough to transfer to other municipalities but embedded in institutional ecosystem. Sees genuine coordination function alongside extraction mechanism.
constraint_indexing:constraint_classification(disability_access_infrastructure, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ADA COMPLIANCE SYSTEM (PITON) — Performative accessibility framework: ramps exist but are poorly maintained, elevators break down and are not repaired, accessible parking fills with non-disabled vehicles, accessible websites fail accessibility audits. Theater ratio is high (0.62) because the system maintains theatrical compliance while functional accessibility remains degraded. The constraint persists through institutional inertia and regulatory capture — the compliance framework is maintained because alternatives (genuine accessible design, universal design, resource allocation to disability services) haven't fully replaced it, not because it works.
constraint_indexing:constraint_classification(disability_access_infrastructure, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: UNIVERSAL DESIGN MOVEMENT (SCAFFOLD) — Organized agents (architects, designers, advocates) see accessibility infrastructure as a temporary coordination failure being superseded by universal design principles where accessible features are integrated at the design stage rather than retrofitted. Low effective extraction because agents see clear exit pathway (design standards shift from compliance theater to genuine inclusion). Sunset mechanism: as universal design becomes default practice, explicit accessibility retrofitting becomes obsolete. Estimated sunset: 20-30 years for design norms to fully shift.
constraint_indexing:constraint_classification(disability_access_infrastructure, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From civilizational/universal perspective, physical/cognitive diversity is inherent to human populations; therefore accessibility needs are 'natural' and immutable. This perspective risks naturalizing what is actually a contingent institutional choice about how to distribute the costs of accommodating diversity. The mountain classification is a false summit: structural data reveals that accessibility infrastructure is a designed institutional arrangement, not a law of nature.
constraint_indexing:constraint_classification(disability_access_infrastructure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(disability_access_infrastructure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(disability_access_infrastructure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(disability_access_infrastructure, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(disability_access_infrastructure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(disability_access_infrastructure, TR),
    TR >= 0.70.

:- end_tests(disability_access_infrastructure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts mobility, autonomy, and participation from disabled population while the gap between disabled and non-disabled access opportunities persists. The value reflects improvement from historical 0.72 due to legal enforcement and advocacy, but structural extraction remains significant because the cost of accessibility is distributed unequally — primarily borne by disabled people through reduced mobility/participation rather than by institutions through design investment. The beneficiary (institutional actors) captures value through market positioning and regulatory compliance without full internalization of accessibility costs. Suppression (0.68): High. Multiple layers: (1) Physical environment barriers (steps, lack of elevators, poor surface conditions); (2) Economic barriers (accessible housing/services cost premium; disabled income below poverty line); (3) Legal/bureaucratic barriers (eligibility determination, documentation requirements, appeals processes); (4) Institutional barriers (employer discrimination, medical gatekeeping); (5) Social/cognitive barriers (internalized ableism, low expectations, isolation from community). Theater ratio (0.62): Moderate-high. The ADA compliance system exhibits significant performative content: regulations exist but enforcement is weak; accessible features are installed but not maintained; standards require accessibility but do not mandate functional inclusion. The theater increased from 0.55 to 0.62 over the interval as compliance theater has become more sophisticated (visible ramps, accessible parking, accessible websites) while functional access gaps persist (maintenance failures, segregation, digital accessibility failures). The theater reflects what scholars term 'performative accessibility' — the appearance of inclusion without substantive participation changes.
 *
 * PERSPECTIVAL GAP:
 *   Maximum perspectival gap characterizes this constraint. The disabled person (powerless/trapped) classifies as snare; the institutional beneficiary (institutional/arbitrage) classifies as rope; the organized coalition (organized/constrained) sees tangled rope. The gap reflects genuine structural asymmetry: the constraint functions as pure coordination for beneficiaries (enabling their market participation) and pure extraction for victims (blocking their participation). This is precisely what tangled rope should capture at the analytical level — genuine coordination function that is asymmetrically distributed. The piton perspective (ADA compliance theater) reveals that even the institutional frameworks designed to mitigate extraction have themselves degraded into performative ritual. The scaffold perspective (universal design) offers exit path that could resolve the tangled rope into pure rope if design integration actually eliminates the asymmetry. The mountain perspective is a false summit — it naturalizes institutional choice as immutable law.
 *
 * DIRECTIONALITY LOGIC:
 *   Each structural position produces different d values and thus different experienced extractiveness (chi). Disabled people as full victims + trapped exit → d ≈ 0.92 → f(d) ≈ 1.35. Accessibility advocates as full victims + identity_locked exit → d ≈ 0.88 → f(d) ≈ 1.25. Disability coalition as partial victims + constrained exit → d ≈ 0.58 → f(d) ≈ 0.75. Institutional beneficiaries as full beneficiaries + arbitrage exit → d ≈ 0.12 → f(d) ≈ -0.02. Municipal administrators as mixed + mobile exit → d ≈ 0.50 → f(d) ≈ 0.65. The derivation shows why chi varies dramatically by perspective even though ε is constant (0.58): the sigmoid f(d) maps the structural position directly to experienced extractiveness. Victims with high d experience chi ≈ 0.78 (high extraction); beneficiaries with low d experience chi ≈ -0.01 (net benefit). This structural divergence IS the tangled rope: one constraint that functions as coordination for some and extraction for others.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy through clear structural differentiation: beneficiaries genuinely benefit from the coordination function (accessibility standards do enable participation); victims genuinely suffer from the extraction mechanism (unequal cost distribution); the tangled rope classification correctly identifies both functions simultaneously. The risk of false negatives (calling it pure rope) occurs when privileged perspectives (institutional beneficiaries) observe from positions of high arbitrage and misclassify the constraint as coordination-only. The risk of false positives (calling it pure snare) occurs when powerless perspectives experience maximum extraction and ignore the genuine coordination function. The analytical framework prevents both errors by requiring perspectival multiplicity: the engine cannot classify as mountain (natural law) when structural data shows beneficiaries/victims; cannot classify as rope (pure coordination) when suppression ≥ 0.40 and victims exist; cannot classify as snare (pure extraction) when genuine coordination function reduces extraction from maximal levels. The mandatrophy is resolved by showing that all six types are legitimate readings from different structural positions, but the analytical observer (at civilizational scope) must see tangled rope — the true structure that manifests differently to different actors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_mechanism_ambiguity,
    'Is suppression (0.68) primarily structural (legal barriers, economic cost of accessible design) or internalized (disabled people have internalized inferiority, low expectations, identity fusion with constraint)?',
    'Post-exit suppression analysis: disabled people who emigrate to highly accessible environments (countries with universal design norms); comparison of self-advocacy rates before/after access improvements; identity drift in accessibility advocates after major policy victories',
    'If structural: suppression reflects real material barriers; removing barriers reduces suppression immediately. If internalized: suppression persists after barrier removal; constraint must address cognitive/identity components. If both: effective suppression is higher than 0.68 suggests because targets carry suppression with them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_ambiguity, empirical, 'Whether suppression is structural or internalized').

omega_variable(
    coordination_function_authenticity,
    'Does the accessibility constraint solve genuine coordination problems (integrating diverse mobility needs into shared infrastructure) or is coordination purely a cover story for resource extraction from disabled population?',
    'Comparative analysis of accessibility outcomes in high-enforcement jurisdictions vs low-enforcement; correlation between accessibility investment and disabled employment/participation rates; testing whether accessibility standards actually enable coordination vs merely distribute costs',
    'If authentic coordination: constraint classification as Tangled Rope is correct. If extraction only: constraint should reclassify as Snare across more perspectives. If varies by jurisdiction: decompose into separate constraint stories per enforcement regime.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_authenticity, empirical, 'Whether accessibility constraint solves genuine coordination or masks pure extraction').

omega_variable(
    identity_lock_mechanism_disability_advocacy,
    'For accessibility advocates classified as identity_locked, is the binding mechanism ideological commitment (worldview that frames accessibility as moral imperative), relational identity (self-concept fused with advocacy community), professional identity (career path dependent on advocacy), or organizational identity (organization has become its mission)?',
    'Narrative analysis of advocate career transitions; study of advocates who left the field (what frame shift enabled exit?); analysis of organizations that shifted mission away from accessibility (how did identity reorganize?)',
    'If ideological: identity lock may persist across career changes; exit requires worldview shift. If relational: identity lock may dissolve with community dissolution or new community formation. If professional: identity lock may be broken by alternative career paths. If organizational: identity lock persists institutionally even as personnel turnover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_disability_advocacy, conceptual, 'Mechanism of identity fusion in disability advocacy field').

omega_variable(
    universal_design_timeline,
    'Will universal design principles actually replace accessibility retrofitting as default practice within the 20-30 year sunset timeline, or will accessibility theater persist due to path dependence and regulatory capture?',
    'Longitudinal tracking of design education curricula; analysis of building code evolution in jurisdictions with strongest universal design adoption; comparative study of retrofit-first vs design-first accessibility outcomes',
    'If universal design adoption accelerates: scaffold classification is correct and sunset is real. If adoption stalls: scaffold becomes piton (theater persists due to inertia rather than genuine sunset mechanism).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(universal_design_timeline, empirical, 'Whether universal design movement will achieve sunset timeline or stall as piton').

omega_variable(
    platform_accessibility_decomposition,
    'Should digital accessibility (websites, apps, software interfaces) be analyzed as separate constraint story from physical/built environment accessibility, or can they be unified under single constraint framework?',
    'Comparison of ε values: physical accessibility retrofitting vs digital accessibility compliance; analysis of whether enforcement mechanisms, beneficiaries, victims are same or different across domains',
    'If separate: write two constraint stories (physical_accessibility_infrastructure, digital_accessibility_infrastructure) with different ε, different theater ratios, different enforcement mechanisms. If unified: constraint covers both domains but risks conflating distinct structural dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_accessibility_decomposition, empirical, 'Whether digital and physical accessibility should decompose into separate constraints').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(disability_access_infrastructure, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dai_tr_t0, disability_access_infrastructure, theater_ratio, 0, 0.55).
narrative_ontology:measurement(dai_tr_t5, disability_access_infrastructure, theater_ratio, 5, 0.59).
narrative_ontology:measurement(dai_tr_t10, disability_access_infrastructure, theater_ratio, 10, 0.62).

% Extraction over time
narrative_ontology:measurement(dai_be_t0, disability_access_infrastructure, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(dai_be_t5, disability_access_infrastructure, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(dai_be_t10, disability_access_infrastructure, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(disability_access_infrastructure, resource_allocation).
narrative_ontology:boltzmann_floor_override(disability_access_infrastructure, 0.18).
narrative_ontology:affects_constraint(disability_access_infrastructure, employment_discrimination_disabled_workers).
narrative_ontology:affects_constraint(disability_access_infrastructure, medical_gatekeeping_access_determination).
narrative_ontology:affects_constraint(disability_access_infrastructure, ableist_social_norms_inclusion).

% DUAL FORMULATION NOTE:
% Disability access infrastructure is a constraint family decomposable into at least three structurally distinct claims with different ε values: (1) physical/built environment accessibility (ε≈0.58, tangled rope with high theater from ADA compliance), (2) digital accessibility (ε≈0.45, emerging rope as standards mature), (3) institutional accessibility practices (ε≈0.62, tangled rope with high identity-lock mechanisms). This story focuses on the built environment; digital accessibility should be analyzed as separate constraint with its own perspectives and measurements.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(disability_access_infrastructure, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
