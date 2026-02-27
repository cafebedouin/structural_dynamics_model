% ============================================================================
% CONSTRAINT STORY: naming_as_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   The ontological hegemony is a structural constraint in which dominant
 *   institutions (states, courts, medical establishments, academic
 *   disciplines) exercise power by defining the authoritative vocabulary
 *   through which reality is interpreted and acted upon. This is not merely
 *   linguistic preference or standardization — it is the enforcement of
 *   specific categories as legally binding, epistemically privileged, or
 *   morally mandatory while rendering alternative naming systems invisible,
 *   pathological, or criminal. The constraint exhibits the full spectrum of
 *   DR classification depending on perspective. For institutional stewards
 *   managing communication, it functions as coordination (Rope). For
 *   marginalized communities whose categories are erased, it functions as
 *   pure extraction (Snare). For organized resistance movements, it is a
 *   mixed coordination-extraction hybrid (Tangled Rope). For translation
 *   initiatives, it is a temporary bridge (Scaffold). For historical
 *   vocabulary archives, it persists through institutional inertia (Piton).
 *   For civilizational analysis, there is a risk of mistaking functional
 *   necessity for logical inevitability (false Mountain). The constraint's
 *   theater_ratio has risen over the measurement interval as formal
 *   institutions increasingly ritualize terminology enforcement
 *   (institutional review boards, standardized diagnostic manuals, legal
 *   terminology updates) while actual communication increasingly diverges
 *   from official vocabulary through code-switching and counter-terminology.
 *   The extractiveness has increased as the hegemonic regime has extended
 *   into more domains (medical, psychological, legal, educational,
 *   environmental) with each domain extension deepening the cost of using
 *   alternative categories.
 *
 * KEY AGENTS:
 *   - Institutional Stewards: Primary beneficiary (institutional/arbitrage) — dictionary makers, legal drafters, state authorities, medical credentialing bodies. Control of terminology definition confers epistemic authority and institutional legitimacy. Zero exit cost; full ability to redefine vocabulary unilaterally.
 *   - Marginalized Naming Communities: Primary victim (powerless/trapped) — Indigenous peoples, colonized populations, queer communities, disabled people, linguistic minorities. Categories either erased entirely or pathologized; no exit option; suppression both formal (legal penalties) and informal (social stigma). Epistemic authority completely subordinated.
 *   - Organized Resistance Movements: Secondary victim/organized actor (organized/constrained) — language revival initiatives, decolonial linguistic projects, queer terminology communities. Can mobilize alternative vocabularies internally but cannot fully escape the hegemonic regime's reach. Constrained exit: operate within or around the dominant system.
 *   - Alternative Ontological Frameworks: Abstract victim — kinship systems, spiritual taxonomies, ecological classifications, gender categories from non-hegemonic cultures. Cannot defend themselves; exist only insofar as communities maintain them despite active suppression.
 *   - Translation Layer Initiatives: Bridge actor (organized/constrained) — bilingual education, legal translation services, international protocol harmonization. Explicitly designed to lower the extraction cost by creating temporary bridges. Sunset logic: intended to become unnecessary.
 *   - Analytical Observer: Sees full structure from civilizational perspective; at risk of naturalizing contingency as necessity.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(naming_as_control, 0.58).
domain_priors:suppression_score(naming_as_control, 0.72).
domain_priors:theater_ratio(naming_as_control, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(naming_as_control, extractiveness, 0.58).
narrative_ontology:constraint_metric(naming_as_control, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(naming_as_control, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(naming_as_control, tangled_rope).
narrative_ontology:human_readable(naming_as_control, "The Ontological Hegemony: Naming as Control").
narrative_ontology:topic_domain(naming_as_control, "social/political/linguistic").

domain_priors:requires_active_enforcement(naming_as_control).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(naming_as_control, dominant_institutional_apparatus).
narrative_ontology:constraint_beneficiary(naming_as_control, legal_terminology_stewards).
narrative_ontology:constraint_victim(naming_as_control, marginalized_naming_communities).
narrative_ontology:constraint_victim(naming_as_control, alternative_ontological_frameworks).
narrative_ontology:constraint_victim(naming_as_control, linguistic_epistemic_minorities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SILENCED COMMUNITIES (SNARE) — Groups whose ontological categories are erased or criminalized by dominant naming regimes have no exit. Their concepts (kinship systems, spiritual taxonomies, ecological classifications, gender categories) are rendered invisible, pathologized, or illegal. The constraint extracts epistemic authority while suppressing alternative framings. Zero degrees of freedom.
constraint_indexing:constraint_classification(naming_as_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: RESISTANT NAMING COMMUNITIES (TANGLED ROPE) — Organized movements (Indigenous language revival, queer terminology projects, decolonial linguistic efforts) mobilize alternative naming schemes but must operate within or around the dominant vocabulary. They achieve some coordination (mutual recognition via shared terminology) but face active suppression and institutional coercion. Constrained exit: they can organize internally but cannot fully escape the hegemonic naming regime's reach.
constraint_indexing:constraint_classification(naming_as_control, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL STEWARDS (ROPE) — Dictionary makers, legal drafters, educational authorities, and state certification bodies experience the naming regime as a coordination mechanism. Defining shared vocabulary solves the collective action problem of communication standardization. Their arbitrage capacity (ability to redefine terminology, launch neologisms, establish precedent) means they experience the constraint as enabling rather than constraining. Low experienced extraction.
constraint_indexing:constraint_classification(naming_as_control, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HISTORICAL VOCABULARY ARCHIVES (PITON) — The constraint persists partly through institutional inertia in language itself. Historical naming taxonomies (colonial ethnographies, psychiatric nomenclature, legal categories) continue to structure thought even as their origins are forgotten and their function atrophied. High theater ratio: practitioners use inherited vocabulary without examining its extraction logic. Degraded constraint — maintained by habituation, not active utility.
constraint_indexing:constraint_classification(naming_as_control, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INSTITUTIONAL TRANSLATION INITIATIVES (SCAFFOLD) — Bilingual education programs, legal translation services, international protocol development, and technical standard harmonization represent temporary scaffolding between naming regimes. These initiatives are explicitly designed to bridge the hegemonic vocabulary and alternative ontologies. Low extractiveness because they have sunset logic built in: the goal is for the scaffold to become unnecessary as mutual intelligibility is achieved. Constrained exit because translation work is resource-intensive and temporary.
constraint_indexing:constraint_classification(naming_as_control, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / LOGICAL NECESSITY (MOUNTAIN) — From a universalist perspective, some shared ontological framework is logically necessary for communication and coordination. Any functional system requires a common vocabulary; therefore, some naming authority is inevitable. This perspective risks naturalizing contingent institutional choices as universal requirements. Engine will flag as false summit: the necessity is functional (communication works better with shared terms) but not logical (many different naming regimes could solve this equally well).
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
    constraint_indexing:constraint_classification(naming_as_control, TypeOther, context(agent_power(organized), _, _, _)),
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
 *   Extractiveness (0.58): Moderate-high. The dominant institution extracts epistemic authority, institutional legitimacy, and policy-setting capacity through terminology control. The extraction is not maximal (0.80+) because some counter-terminology survives and some communities maintain alternative vocabularies despite suppression. The extraction is substantial because the cost of using non-hegemonic terminology is high across multiple domains (legal validity, medical recognition, educational certification). The measurement trajectory (0.35→0.58) reflects expansion of the hegemonic regime into additional domains over the interval. Suppression (0.72): High. Suppression operates through multiple mechanisms: legal prohibition (criminal liability for certain terminology), institutional gatekeeping (medical credentials require acceptance of official taxonomy), social coercion (stigma for using non-standard categories), and epistemic closure (alternative frameworks excluded from authoritative knowledge production). Suppression is not total (0.85+) because code-switching persists, communities maintain underground alternative vocabularies, and some institutional spaces permit pluralism. Theater ratio (0.65): Moderate-high. Institutional practice increasingly emphasizes ritualized terminology enforcement (diagnostic manuals, style guides, approved terminology lists) that persists even when functional benefit is unclear. The theater has increased over the interval as institutions have added more formal terminology management structures (review boards, compliance training) while actual communication diverges increasingly from official vocabulary. The constraint exhibits both coordination function (shared terminology does enable communication) and extraction logic (the power to define which terminology is shared and binding confers asymmetric benefit).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is exceptionally wide and reveals the full extractive structure. The institutional steward (Rope) experiences the constraint as solving the genuine problem of communication standardization — they benefit from the authority to set terminology and experience this as a coordination role. The silenced community (Snare) experiences the same constraint as pure extraction — their categories are erased, their capacity to speak their own reality is suppressed, and they have no exit option. The resistant movement (Tangled Rope) experiences it as mixed coordination and coercion — they can organize internally around alternative terminology but face institutional opposition and suppression. The translation initiative (Scaffold) experiences it as temporary bridging work with sunset logic. The piton perspective sees the constraint as historical inertia maintained through ritual. The false mountain perspective risks naturalizing the regime's claim that some shared vocabulary is logically necessary (true in the abstract) as justification for THIS SPECIFIC regime's hegemony (false). The perspectival gap reveals mandatrophy: the constraint cannot be classified as pure coordination (rope) because it exhibits high suppression, multiple victims, and asymmetric extraction. It cannot be classified as pure extraction (snare) because it performs a genuine coordination function. The tangled rope classification correctly captures that both functions are real and both are essential to understanding the constraint's operation.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position relative to the naming regime. Institutional stewards have arbitrage exit options (ability to redefine terminology unilaterally, move between jurisdictions with different vocabularies, establish precedent) combined with beneficiary status (they control the definition process). Engine derives low d → negative f(d) → they experience negative χ (the constraint subsidizes their epistemic authority). Marginalized communities have trapped exit options (cannot avoid the hegemonic vocabulary in legal, medical, educational contexts) combined with victim status (their categories are erased). Engine derives high d → high f(d) → they experience high χ (the constraint extracts from them maximally). Organized resistance movements have constrained (not trapped, not arbitrage) exit options — they can mobilize alternative vocabularies within their community but cannot fully escape the hegemonic regime's reach in formal institutions. Engine derives moderate-high d reflecting their mixed structural position. Translation initiatives have constrained exit and beneficiary-like status (they solve a coordination problem) which produces moderate d reflecting that they have some agency but limited freedom to redefine the terms they are bridging. The piton perspective is institutional (arbitrage capacity) but sees its own function as degraded, producing low d but high theater. The false mountain perspective is analytical (canonical d ≈ 0.73) which produces moderate f(d) and is correctly rejected as a false summit by the engine's natural law gates.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint is legitimately tangled rope, not rope disguised as snare or snare disguised as rope. The coordination function is real: shared terminology does enable cross-group communication and coordination. The extraction function is equally real: the power to define 'shared' terminology confers asymmetric epistemic authority and institutional legitimacy that benefits the stewards at the expense of those whose categories are erased. The mandatrophy is resolved by showing that BOTH functions are structurally necessary for the constraint to operate. The regime requires the appearance of purely coordinating (it justifies itself on communication necessity), but it also requires the extraction (the whole point is to establish hegemonic authority through terminology). The regime cannot function as pure rope because it requires suppression — if vocabulary choice were genuinely pluralistic, the extraction would collapse. The regime cannot function as pure snare because it must solve a real coordination problem — if shared communication were not actually achieved, institutional actors would have no incentive to enforce it. The tangled rope classification is mandatrophy-resolving because it identifies the constraint as NECESSARILY hybrid. The very extractiveness (0.58) that distinguishes it from rope (≤0.45) derives from the need to maintain hegemony over terminology definition. Remove the extraction, and the coordination benefit degrades. The measurement trajectory confirms mandatrophy resolution: as the regime extended into more domains (increasing extractiveness), it simultaneously performed more coordination functions (maintaining theater through ritualized terminology management). The increase in theater ratio reflects institutional elaboration of the naming regime, not degradation into piton — the theater serves the extraction by making the hegemony appear inevitable and necessary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    necessity_vs_contingency,
    'Is linguistic standardization a logical necessity (some shared vocabulary is required) or a contingent institutional choice (many different vocabularies could work equally well)?',
    'Cross-cultural communication analysis examining multilingual success; comparison of outcomes under different standardization regimes (plural official languages vs monolingual mandate)',
    'If necessity: mountain classification holds. If contingency: snare/tangled_rope from non-beneficiary perspectives. Current evidence leans strongly toward contingency: successful multilingual polities exist; imposed monolingualism typically produces worse coordination outcomes than plural recognition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(necessity_vs_contingency, conceptual, 'Whether linguistic standardization is logically necessary or institutionally contingent').

omega_variable(
    coercion_threshold,
    'What level of active enforcement is required to maintain a naming regime, and does variation in enforcement level correlate with legitimacy or mere institutional power?',
    'Historical analysis of naming regimes under different enforcement intensities; correlation between enforcement and adoption rates; measurement of resistance and alternative terminology survival under varying suppression levels',
    'High enforcement correlation with illegitimacy suggests the constraint is snare/tangled_rope (requires coercion). Low enforcement correlation suggests genuine coordination function (rope). Current data shows strong positive correlation: more coercive regimes experience higher resistance, suggesting low natural adoption.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_threshold, empirical, 'Relationship between enforcement intensity and regime legitimacy').

omega_variable(
    epistemic_irreversibility,
    'Once a dominant naming regime establishes hegemony, how difficult is it to de-hegemony? Are the cognitive grooves permanent or reversible?',
    'Longitudinal study of successful naming regime changes (decolonization language recovery, terminology overhauls); measurement of cognitive accessibility of alternative categories before and after re-naming campaigns; analysis of intergenerational transmission of vocabulary change',
    'If irreversible: suppression ≥ 0.85 (mountain-like permanence). If reversible but costly: suppression 0.60–0.75 (tangled rope). If readily reversible: suppression ≤ 0.50 (rope). Current evidence shows high reversibility but substantial temporal and resource cost, supporting suppression ~0.72.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_irreversibility, empirical, 'Reversibility of ontological hegemony once established').

omega_variable(
    theater_functional_boundary,
    'At what point does the naming regime''s institutional maintenance become purely performative rather than functionally coordinating?',
    'Analysis of terminology use in actual communication vs formal institutional contexts; measurement of gap between official vocabulary and lived language practices; examination of code-switching patterns and unofficial terminology prevalence',
    'If theater_ratio > 0.75: constraint approaches piton (institutional inertia). If theater_ratio < 0.40: constraint is genuine rope (functional coordination). Current estimate 0.65 reflects that formal institutions heavily enforce terminology while actual practice shows significant code-switching and unofficial usage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_functional_boundary, empirical, 'Functional vs performative maintenance of hegemonic naming regimes').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(naming_as_control, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naming_tr_t0, naming_as_control, theater_ratio, 0, 0.45).
narrative_ontology:measurement(naming_tr_t50, naming_as_control, theater_ratio, 50, 0.58).
narrative_ontology:measurement(naming_tr_t100, naming_as_control, theater_ratio, 100, 0.65).

% Extraction over time
narrative_ontology:measurement(naming_be_t0, naming_as_control, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(naming_be_t50, naming_as_control, base_extractiveness, 50, 0.5).
narrative_ontology:measurement(naming_be_t100, naming_as_control, base_extractiveness, 100, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(naming_as_control, information_standard).
narrative_ontology:affects_constraint(naming_as_control, epistemic_gatekeeping).
narrative_ontology:affects_constraint(naming_as_control, medical_taxonomy_hegemony).
narrative_ontology:affects_constraint(naming_as_control, legal_terminology_regime).
narrative_ontology:affects_constraint(naming_as_control, identity_category_enforcement).

% DUAL FORMULATION NOTE:
% The ontological hegemony decomposes into multiple domain-specific constraints (medical taxonomy, legal terminology, identity categorization) that share the same structural mechanism but have different ε values and victim populations. All are linked as a constraint family through shared use of institutional naming authority as extraction mechanism. The family-level constraint (naming_as_control) operates at ε=0.58 with suppression 0.72. Domain-specific constraints may have higher or lower extractiveness depending on enforcement intensity in that domain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(naming_as_control, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
