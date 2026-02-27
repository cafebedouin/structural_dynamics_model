% ============================================================================
% CONSTRAINT STORY: naming_as_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_naming_as_control, []).

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
 *   constraint_id: naming_as_control
 *   human_readable: The Ontological Hegemony: Naming as Control
 *   domain: social/political/linguistic
 *
 * SUMMARY:
 *   Naming as control represents one of the most subtle and durable forms of
 *   institutional extraction: the power to define the vocabulary through
 *   which social reality is perceived, adjudicated, and administered. This
 *   constraint operates across legal systems, medical taxonomies,
 *   administrative categories, educational curricula, and scientific
 *   nomenclature. A dominant institution exercises ontological hegemony by
 *   establishing official categories and definitions that become the
 *   prerequisites for institutional access: legal personhood, disease
 *   classification, racial/ethnic categories, gender terminology, even the
 *   names of places and peoples themselves. The extraction occurs because
 *   subaltern communities are forced to adopt the dominant vocabulary to
 *   access institutions (healthcare, courts, schools, employment), thereby
 *   losing the authority to define their own experience. The constraint
 *   exhibits asymmetric costs and benefits: the dominant institution gains
 *   coordination efficiency and the ability to standardize administration,
 *   while subaltern communities bear the cost of linguistic assimilation,
 *   ontological erasure, and the delegitimization of alternative ways of
 *   knowing. The constraint's suppression component (0.68) reflects multiple
 *   barriers: prohibition of minority languages in official contexts,
 *   institutional non-recognition of alternative taxonomies, career penalties
 *   for using non-dominant nomenclature, and the sheer momentum of inherited
 *   naming authority. The theater component (0.64) reflects that
 *   institutional naming carries ritual significance (official gazette,
 *   institutional adoption, legal force) beyond its operational necessity —
 *   much of the enforcement is performative rather than active coercion.
 *
 * KEY AGENTS:
 *   - Subaltern Communities: Primary victims (powerless/trapped) — forced to adopt dominant vocabulary to access institutions; lose authority to define own experience
 *   - Alternative Ontologies: Primary victims (powerless/trapped) — indigenous taxonomies, non-Western medical systems, alternative social classifications structurally delegitimized
 *   - Linguistic Minorities: Secondary victims (moderate/trapped) — children must assimilate to dominant language to succeed institutionally
 *   - Dominant Institution: Primary beneficiary (institutional/arbitrage) — gains coordination efficiency and administrative standardization through unified vocabulary
 *   - Social Justice Movements: Organized resistance (organized/constrained) — decolonial movements, feminist redefinition campaigns, LGBTQ+ nomenclature struggles
 *   - Colonial Naming Authority: Institutional maintainer (institutional/arbitrage) — historical practice of imposing foreign taxonomies maintained through inherited prestige and inertia
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent naming regimes as universal logical necessities
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(naming_as_control, 0.58).
domain_priors:suppression_score(naming_as_control, 0.68).
domain_priors:theater_ratio(naming_as_control, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(naming_as_control, extractiveness, 0.58).
narrative_ontology:constraint_metric(naming_as_control, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(naming_as_control, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(naming_as_control, snare).
narrative_ontology:human_readable(naming_as_control, "The Ontological Hegemony: Naming as Control").
narrative_ontology:topic_domain(naming_as_control, "social/political/linguistic").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(naming_as_control, dominant_institution).
narrative_ontology:constraint_victim(naming_as_control, subaltern_communities).
narrative_ontology:constraint_victim(naming_as_control, alternative_ontologies).
narrative_ontology:constraint_victim(naming_as_control, linguistic_minorities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBALTERN COMMUNITIES (SNARE) — Cannot exit the naming regime without losing access to institutional resources (legal recognition, education, healthcare). Trapped in a linguistic structure that delegitimizes their own categories and experiences. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.80.
constraint_indexing:constraint_classification(naming_as_control, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ALTERNATIVE ONTOLOGIES (SNARE) — Indigenous taxonomies, non-Western medical systems, alternative social classifications are structurally delegitimized by institutional naming authority. No mechanism for these systems to coexist with equal authority; must either assimilate or remain invisible. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.97.
constraint_indexing:constraint_classification(naming_as_control, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: LINGUISTIC MINORITIES (SNARE) — Children of minority language communities must adopt dominant-language categories to succeed institutionally (school, medicine, law). Trapped between linguistic worlds; cannot exit without cultural assimilation. d≈0.85, f(d)≈1.15, σ=0.9 → χ≈0.62.
constraint_indexing:constraint_classification(naming_as_control, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 4: DOMINANT INSTITUTION (ROPE) — Legal and administrative system experiences naming standardization as pure coordination: unified vocabulary enables contract enforcement, legal proceedings, bureaucratic efficiency. Institution extracts naming authority as a legitimate prerogative of statehood. d≈0.08, f(d)≈-0.08, σ=1.0 → χ≈-0.05.
constraint_indexing:constraint_classification(naming_as_control, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: SOCIAL JUSTICE MOVEMENTS (TANGLED ROPE) — Organized resistance to naming hegemony (decolonial movements, feminist redefinition campaigns, LGBTQ+ nomenclature struggles) achieves real coordination (establishing new legitimacy categories) while facing active suppression. d≈0.60, f(d)≈0.75, σ=1.0 → χ≈0.44.
constraint_indexing:constraint_classification(naming_as_control, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: COLONIAL NAMING AUTHORITY (PITON) — The institutional practice of imposing foreign taxonomies (geographic names, botanical classification, medical nosology) is maintained largely through institutional inertia and historical prestige, not active extraction. Theater_ratio=0.64 reflects that renaming carries ritual significance (official gazette, institutional adoption) but does not produce ongoing value extraction — the extraction already occurred during the historical imposition phase. Maintains power through inherited authority, not active enforcement.
constraint_indexing:constraint_classification(naming_as_control, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE UNIVERSALISM (MOUNTAIN) — Risk that civilizational perspective naturalizes contingent naming regimes as universal logical necessities (e.g., 'English is the language of science by necessity'). Engine's false summit detector applies here: the structural data (ε=0.58, suppression=0.68, theater=0.64) contradicts mountain classification. Appearance of natural law masks institutional power.
constraint_indexing:constraint_classification(naming_as_control, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(naming_as_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(naming_as_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(naming_as_control, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(naming_as_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(naming_as_control, TR),
    TR >= 0.70.

:- end_tests(naming_as_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.58): High-moderate. The dominant institution derives sustained value from naming standardization: simplified contract enforcement, unified legal taxonomy, efficient administration. Subaltern communities bear asymmetric costs: mandatory linguistic assimilation, loss of ontological autonomy, delegitimization of alternative knowledge systems. Extractiveness is not at maximum (0.70+) because naming coordination does provide genuine efficiency benefits to all communities (reduction in contract ambiguity, medical clarity), but these benefits accrue asymmetrically. The extraction is real: the dominant institution captures the value of naming authority (prestige, administrative power) while imposing costs on minorities who must adopt foreign categories. Suppression (0.68): Moderate-high. Multiple enforcement mechanisms: institutional non-recognition of minority taxonomies, prohibition or discouragement of minority language use in official contexts, career penalties for non-compliance, educational systems that teach only dominant-language categories. Suppression is structural (invisibility of alternatives) and active (explicit prohibition in many contexts). Theater ratio (0.64): Moderate-high. Naming standardization carries significant ritual and symbolic weight beyond operational necessity: official gazettes, formal name-change procedures, institutional adoption ceremonies. Much of the enforcement is performative — the symbolic authority of 'official naming' often matters more than practical enforcement. Temporal trajectory: Extractiveness and theater have both increased over the measurement interval (0-100), reflecting that naming hegemony has intensified as institutional complexity has grown. Early phases (0-50) showed lower extraction because alternative ontologies had more institutional space; later phases (50-100) show consolidation as dominant naming standards have penetrated deeper into social institutions.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a severe perspectival collapse in how extraction is perceived. Subaltern communities see Snare (pure extraction with no meaningful coordination benefit — the cost of adopting foreign terminology far exceeds the coordination benefit they receive). Linguistic minorities see Snare (trapped in a system where institutional access requires ontological assimilation). Alternative ontologies see maximum Snare (structurally delegitimized; no coexistence mechanism). The dominant institution sees Rope (pure coordination — unified naming enables legal certainty and administrative efficiency; the institution does not perceive extraction, only service provision). Social justice movements see Tangled Rope (genuine coordination benefit from clarity, combined with asymmetric extraction from forced assimilation). The colonial naming authority sees Piton (inherited authority, maintained through ritual but not active enforcement — the hard work of suppression is already done; maintenance is mostly inertia). The analytical observer risks Mountain (falsely naturalizing contingent naming regimes as universal logical necessities). The gap between Snare (victim view) and Rope (institutional view) reveals the core extraction mechanism: the institution extracts naming authority by reframing what should be a symmetric coordination problem as a unidirectional service provision.
 *
 * DIRECTIONALITY LOGIC:
 *   Subaltern communities: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction minus epsilon. Forced adoption of dominant vocabulary; cannot exit without losing institutional access. Alternative ontologies: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction. Structurally delegitimized; no mechanism for coexistence with equal authority. Linguistic minorities: Victim + trapped → d≈0.85, f(d)≈1.15. High extraction. Must assimilate to dominant language to access education and employment; limited exit options. Dominant institution: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Net beneficiary. Extracts naming authority as a legitimate prerogative; experiences naming standardization as pure coordination service. Social justice movements: Organized + constrained → d≈0.60, f(d)≈0.75. Moderate-to-high extraction with agency to resist. Constrained by institutional power but capable of establishing alternative naming authority (e.g., feminist terminology, decolonial categories). Colonial naming authority: Institutional + arbitrage → d≈0.08, f(d)≈-0.08. Piton classification emerges from theater gate (0.64 ≥ 0.70 threshold approached), not from high chi; extraction already occurred during colonial period, maintenance is inertial. Analytical observer: Analytical → d≈0.72, f(d)≈1.15. Mountain classification is perspectival risk (naturalizes contingent arrangements); false summit.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_vs_coordination,
    'Is the ontological hegemony fundamentally an extraction mechanism or a legitimate coordination service with extractive side effects?',
    'Comparative institutional analysis: jurisdictions with pluralistic naming authority (multilingual legal systems, indigenous co-governance) vs monolithic regimes; measurement of actual transaction costs from name standardization vs coordination benefits',
    'If coordination-primary: constraint should classify as Tangled Rope from institutional perspective, not Rope. If extraction-primary: reinforces Snare classification from subaltern perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_vs_coordination, conceptual, 'Whether naming standardization is fundamentally coordination or extraction').

omega_variable(
    invisibility_vs_active_suppression,
    'Does the naming regime actively suppress alternative ontologies or merely fail to recognize them (invisibility as structural result rather than intentional enforcement)?',
    'Historical documentation of deliberate suppression campaigns (language prohibition, official taxonomy rejection) vs passive institutional non-recognition; analysis of whether suppression decreases when alternative categories gain institutional advocates',
    'If active suppression: Snare classification reinforced; suppression field ≥0.60 confirmed. If passive invisibility: might reclassify as Piton (theater-driven rather than coercion-driven) or Rope with severe perspectival gap.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(invisibility_vs_active_suppression, empirical, 'Whether suppression is active or structural invisibility').

omega_variable(
    exit_option_degradation,
    'What constitutes a real exit from the naming regime? Can subaltern communities maintain alternative ontologies while participating in dominant institutions, or is institutional access structurally incompatible with naming autonomy?',
    'Case studies of code-switching effectiveness; measurement of institutional penalties for non-compliance with dominant ontology; analysis of whether minority language users retain ontological autonomy in private/community contexts',
    'If true exit exists (dual ontologies possible): exit_options should be ''constrained'' not ''trapped''; perspectival gap narrows. If no exit exists: confirms d≈0.92-0.95 and Snare classification from subaltern perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_option_degradation, empirical, 'Whether subaltern communities can maintain alternative ontologies while accessing dominant institutions').

omega_variable(
    temporal_horizon_of_extraction,
    'Is the extraction from naming hegemony a one-time historical imposition (colonial period) or an ongoing generational extraction mechanism?',
    'Measurement of suppression intensity over time; analysis of naming regime elasticity (whether counter-naming succeeds); tracking of institutional resource allocation to naming standardization vs accommodation',
    'If one-time: constraint might degrade to Piton over long historical intervals. If ongoing: Snare classification sustained across time horizons.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(temporal_horizon_of_extraction, empirical, 'Whether naming extraction is historical or ongoing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(naming_as_control, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naming_tr_t0, naming_as_control, theater_ratio, 0, 0.45).
narrative_ontology:measurement(naming_tr_t50, naming_as_control, theater_ratio, 50, 0.55).
narrative_ontology:measurement(naming_tr_t100, naming_as_control, theater_ratio, 100, 0.64).

% Extraction over time
narrative_ontology:measurement(naming_be_t0, naming_as_control, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(naming_be_t50, naming_as_control, base_extractiveness, 50, 0.48).
narrative_ontology:measurement(naming_be_t100, naming_as_control, base_extractiveness, 100, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(naming_as_control, information_standard).
narrative_ontology:boltzmann_floor_override(naming_as_control, 0.45).
narrative_ontology:affects_constraint(naming_as_control, epistemic_injustice).
narrative_ontology:affects_constraint(naming_as_control, curriculum_hegemony).
narrative_ontology:affects_constraint(naming_as_control, medical_taxonomy_colonialism).
narrative_ontology:affects_constraint(naming_as_control, gender_category_imposition).

% DUAL FORMULATION NOTE:
% The ontological hegemony decomposes into domain-specific constraints: medical taxonomy colonialism (imposition of Linnaean/biomechanical categories over indigenous healing systems), curriculum hegemony (institutional control of knowledge through standardized language and concepts), epistemic injustice (structural devaluation of subaltern knowledge due to naming authority), and gender category imposition (institutional definitions of sex/gender that exclude non-binary and indigenous categories). Each subdomain has its own ε reflecting the degree of extraction relative to coordination benefit. All are downstream of the general naming-as-control mechanism and share the same institutional beneficiary (dominant state apparatus).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(naming_as_control, analytical, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
