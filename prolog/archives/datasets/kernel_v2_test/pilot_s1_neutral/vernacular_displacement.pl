% ============================================================================
% CONSTRAINT STORY: vernacular_displacement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vernacular_displacement, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: vernacular_displacement
 *   human_readable: Vernacular Displacement in Language Revitalization
 *   domain: sociolinguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   Vernacular displacement in language revitalization represents a
 *   structural tension between the institutional coordination necessary to
 *   revive a non-native language (centralized standardization, canonical
 *   authorities, prescriptive education) and the linguistic conditions
 *   required for true revitalization (native generative competence,
 *   acceptance of natural variation, emergent vernacular innovation). The
 *   Hebrew language revival—from liturgical-only status (1800s) through
 *   modern revitalization (1900s onward) to contemporary living language
 *   (2000s+)—illustrates how the same institutional arrangements can be
 *   understood as pure coordination (rope), necessary temporary scaffolding
 *   (scaffold), degraded ritual (piton), mixed coordination-extraction
 *   (tangled rope), or pure extraction (snare), depending on the observer's
 *   structural position. The constraint exhibits characteristic features of a
 *   commitment system organized around a contested kernel: whether 'Hebrew as
 *   a living language' means (1) preservation through recitation and textual
 *   study (liturgical reading) or (2) reconstruction into native generative
 *   speech (vernacular reading). These readings coexist in tension; neither
 *   forecloses the other logically, but they create fundamentally different
 *   structural arrangements and extract differently from speakers.
 *
 * KEY AGENTS:
 *   - Vernacular Speakers: Primary victims (powerless/identity_locked) — native speakers of suppressed forms (Yiddish, Ladino, local Arabic dialects) whose linguistic identities are treated as problems to be solved by displacement toward Hebrew canonical forms
 *   - Liturgical Preservationists: Primary beneficiaries (institutional/arbitrage) — religious institutions, academies, and textual authorities whose institutional legitimacy grounds itself in maintaining canonical Hebrew standards and preventing corruption of sacred texts
 *   - Community Language Learners: Secondary victims (moderate/constrained) — second-language learners of revived Hebrew who experience both the coordination benefit (access to linguistic heritage) and extraction (pressure to conform to canonical standards rather than develop living vernacular norms)
 *   - Revitalization Movement Organizers: Organized coordinating agents (organized/constrained) — educational programs, media initiatives, and cultural centers driving the revival; experience the constraint as temporary scaffolding with a sunset (moving from centralized authority toward distributed generative competence)
 *   - Prescriptive Grammar Establishment: Institutional maintainers (institutional/arbitrage) — academic language committees, dictionary authorities, pronunciation standardizers whose performative function (defending boundaries against language change) increasingly exceeds their actual linguistic utility
 *   - Linguistic Ecologists: Analytical victims (analytical/analytical) — structural linguists and language documentation specialists who track the constraint's impact on other languages in the region (displacement of Yiddish, suppression of Ladino, marginalization of Palestinian Arabic variants) and on the revived language's own linguistic ecology
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vernacular_displacement, 0.55).
domain_priors:suppression_score(vernacular_displacement, 0.48).
domain_priors:theater_ratio(vernacular_displacement, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vernacular_displacement, extractiveness, 0.55).
narrative_ontology:constraint_metric(vernacular_displacement, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(vernacular_displacement, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vernacular_displacement, tangled_rope).
narrative_ontology:human_readable(vernacular_displacement, "Vernacular Displacement in Language Revitalization").
narrative_ontology:topic_domain(vernacular_displacement, "sociolinguistics/language_revitalization/commitment_systems").

domain_priors:requires_active_enforcement(vernacular_displacement).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vernacular_displacement, 'faf4bc3d-2dcc-4398-ad83-56532c200163').
narrative_ontology:cs_kernel_codification('faf4bc3d-2dcc-4398-ad83-56532c200163', fixed_text).
narrative_ontology:cs_authority_grounding('faf4bc3d-2dcc-4398-ad83-56532c200163', extraction).
narrative_ontology:cs_interpretation_layer_present('faf4bc3d-2dcc-4398-ad83-56532c200163').
narrative_ontology:cs_reading_relation('faf4bc3d-2dcc-4398-ad83-56532c200163', vernacular_displacement__hebrew_liturgical_preservation_reading, coexists_with).
narrative_ontology:cs_reading_relation('faf4bc3d-2dcc-4398-ad83-56532c200163', vernacular_displacement__hebrew_textual_fidelity_reading, coexists_with).
narrative_ontology:cs_axiom('faf4bc3d-2dcc-4398-ad83-56532c200163', foundational, native_generation_constitutes_linguistic_life).
narrative_ontology:cs_axiom_status(native_generation_constitutes_linguistic_life, holdable).
narrative_ontology:cs_axiom_grounding('faf4bc3d-2dcc-4398-ad83-56532c200163', native_generation_constitutes_linguistic_life, deontological).
narrative_ontology:cs_axiom('faf4bc3d-2dcc-4398-ad83-56532c200163', secondary, prescriptive_standardization_instrumentally_necessary_for_revitalization).
narrative_ontology:cs_axiom_status(prescriptive_standardization_instrumentally_necessary_for_revitalization, overridden).
narrative_ontology:cs_axiom_grounding('faf4bc3d-2dcc-4398-ad83-56532c200163', prescriptive_standardization_instrumentally_necessary_for_revitalization, empirically_contingent).
narrative_ontology:cs_axiom('faf4bc3d-2dcc-4398-ad83-56532c200163', foundational, vernacular_innovation_permitted_in_mature_living_language).
narrative_ontology:cs_axiom_status(vernacular_innovation_permitted_in_mature_living_language, holdable).
narrative_ontology:cs_axiom_grounding('faf4bc3d-2dcc-4398-ad83-56532c200163', vernacular_innovation_permitted_in_mature_living_language, deontological).
narrative_ontology:cs_reference_frame('faf4bc3d-2dcc-4398-ad83-56532c200163', hebrew_living_language_daily_vernacular).
narrative_ontology:cs_drift_state('faf4bc3d-2dcc-4398-ad83-56532c200163', contemporary_mature_revitalization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('faf4bc3d-2dcc-4398-ad83-56532c200163', '2026-02-26T14:23:47Z').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vernacular_displacement, liturgical_preservationists).
narrative_ontology:constraint_beneficiary(vernacular_displacement, institutional_language_authorities).
narrative_ontology:constraint_victim(vernacular_displacement, vernacular_speakers).
narrative_ontology:constraint_victim(vernacular_displacement, language_ecological_diversity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VERNACULAR SPEAKER (SNARE) — Native speakers of the suppressed language face identity fusion with the suppressed form; cannot exit without abandoning linguistic identity. The revival movement demands displacement toward canonical forms while constraining the generative flexibility that makes a language alive in actual use. Trapped by identity — their native dialect is the problem the constraint defines away.
constraint_indexing:constraint_classification(vernacular_displacement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: COMMUNITY LANGUAGE LEARNER (TANGLED ROPE) — Second-language learners of the revived form experience both coordination (learning access to linguistic heritage) and extraction (pressure to conform to canonical standards rather than develop living vernacular norms). Resources for learning are abundant; resources for innovation are constrained.
constraint_indexing:constraint_classification(vernacular_displacement, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: LITURGICAL AUTHORITY (ROPE) — Religious and academic institutions benefit from the constraint's preservation function: canonical texts remain stable, institutional legitimacy grounds itself in textual fidelity, and the authority structure maintains gatekeeping power over 'correct' usage. Experiences the constraint as pure coordination: maintaining the linguistic commons through standardization.
constraint_indexing:constraint_classification(vernacular_displacement, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REVITALIZATION MOVEMENT (SCAFFOLD) — Language revival organizations (educational programs, media initiatives, cultural centers) experience the constraint as a temporary coordination problem with a sunset clause. Early revival requires standardization and centralized authority (schools teaching canonical forms); mature revitalization requires distributed generative competence and acceptance of natural linguistic variation. The scaffold perspective sees enforcement (standardization) as transitional — the goal is a self-sustaining living language that does not require institutional enforcement.
constraint_indexing:constraint_classification(vernacular_displacement, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PRESCRIPTIVE GRAMMAR ESTABLISHMENT (PITON) — Academic and institutional gatekeepers maintain elaborate prescriptive grammars, pronunciation standards, and usage authorities (academies of language, lexical committees) whose primary function is performative: legitimizing the authority's role as arbiter of correctness. The actual linguistic work (community communication) has partially moved to vernacular innovation and informal generative processes; the prescriptive apparatus persists through institutional inertia and theater. Theater ratio reflects that grammar committees spend labor defending boundaries against living language change rather than serving communication needs.
constraint_indexing:constraint_classification(vernacular_displacement, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: NATURAL LINGUISTIC UNIVERSALS VIEW (MOUNTAIN) — From a civilizational/universal analytical perspective, some degree of variation between written standards and vernacular speech is an immutable property of language: prescriptive-descriptive gaps are inherent to all languages and appear across all human communities and epochs. This perspective naturalizes the constraint as a law of linguistics. However, the structural data (identifiable institutional beneficiaries, active enforcement, measured suppression, theater performance) reveals this as a false summit — the constraint is not a natural law but a contingent institutional arrangement naturalizing what it constructs.
constraint_indexing:constraint_classification(vernacular_displacement, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vernacular_displacement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(vernacular_displacement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(vernacular_displacement, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(vernacular_displacement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(vernacular_displacement, TR),
    TR >= 0.70.

:- end_tests(vernacular_displacement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate-high, declining over the 75-year interval from 0.72 to 0.48. Early extraction was severe because revitalization required institutional enforcement of canonical forms against speakers' native vernacular competence. The declining trend reflects two simultaneous processes: (1) as institutional coordination succeeded, centralized authority became less necessary (native speakers now exist who do not remember suppressed forms), and (2) as community language competence matured, informal vernacular norms developed that the prescriptive apparatus could no longer suppress, creating space for linguistic innovation. The extraction was steepest during the period when institutional coordination was most necessary and most coercive (early-to-mid 20th century). Suppression (0.48): Moderate, also declining from 0.75 to 0.42. Early suppression was enforced through institutional gatekeeping (education systems enforcing canonical forms, religious institutions controlling liturgical practice, employment incentives concentrating in domains using formal Hebrew). The declining suppression reflects both the success of institutional standardization (fewer barriers needed when canonical forms are native competence) and the emergence of informal social networks where vernacular innovation can occur without institutional permission. Theater ratio (0.68, rising from 0.45 to 0.74): High and rising. The prescriptive apparatus (language academies, grammar authorities, pronunciation standards) increasingly performs a theatrical function: its labor goes into defending boundaries against natural language change rather than serving actual communication needs. As living vernacular competence has matured, the prescriptive machinery has become more elaborate and more performative—elaborate because it must work harder to prevent change, performative because the change is inevitable anyway and the machinery serves to legitimize the authority rather than to achieve linguistic coordination.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates divergent classification from different structural positions. The vernacular speaker experiences snare—their native linguistic identity is the problem the constraint defines away, and they cannot exit without linguistic death-of-self. The institutional liturgical authority experiences rope—the constraint solves a genuine coordination problem (preserving sacred texts and institutional legitimacy) with no extraction from their perspective. The community learner experiences tangled rope—gaining access to linguistic heritage while being constrained from innovation. The revitalization movement experiences scaffold—the institutional enforcement is seen as temporary, necessary for achieving distributed native competence, with a sunset when that goal is reached. The prescriptive apparatus experiences piton—the elaborate rules persist through institutional inertia even as the actual linguistic work has moved elsewhere. The analytical observer risks experiencing mountain—treating the prescriptive-descriptive gap as a natural law of all languages—but the structural data (identifiable institutional beneficiaries, measurable enforcement effort, rising theater ratio, declining but still substantial suppression) reveals this as a false summit. The gap is not natural but constructed, maintained by institutions with clear interests in its persistence.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are determined by each agent's structural relationship to the extraction flow. Liturgical authorities are beneficiaries: the constraint maintains their institutional authority and preserves the textual integrity they derive legitimacy from. They have arbitrage-grade exit (they can always shift to other languages or reduce emphasis on language standards without existential threat to their institution). Their d is low (~0.15), producing negative effective extractiveness (they experience subsidy). Vernacular speakers are victims: the constraint suppresses their native linguistic identity and requires displacement toward canonical forms. They are identity-locked (cannot exit without linguistic death-of-self) and powerless (no institutional platform). Their d is high (~0.85), producing maximum experienced extraction. Community learners occupy an intermediate position: beneficiaries of learning access (beneficiary status) but constrained by conformity pressure (constrained exit, not arbitrage). Their d is moderate (~0.50), producing tangled-rope experience. The revitalization organizers have constrained exit (committed to the movement) but organized power (institutional capacity), producing moderate d and scaffold classification. The prescriptive apparatus benefits from the constraint's persistence (maintains gatekeeping authority, creates employment for standardization labor) but experiences their own institutional arrangement as degraded (piton classification reflects that they know the machinery is mostly theater). Their d is beneficiary-weighted (~0.20) but their theater_ratio is high, indicating the classification is piton (inertial) rather than rope (pure coordination).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: This constraint initially exhibits classic mandatrophy — the institutional mandate (revive Hebrew as a living language) has been achieved, yet the enforcement machinery (canonical standardization, prescriptive authorities, institutional gatekeeping) persists and continues to extract from speakers. The constraint's original function (coordinating insufficient native competence through centralized standardization) has been replaced by different institutional logic (preserving institutional authority, defending boundaries against natural language change, maintaining theater of linguistic correctness). The resolution comes from recognizing that mandatrophy is not failure but diagnosis. The rising theater_ratio over time (0.45 → 0.74) reveals the mechanism: as the coordination function succeeded (native competence matured), the enforcement function transitioned from necessary scaffolding (scaffold perspective) to institutional theater (piton perspective). The constraint changed from tangled rope (mixed coordination-extraction during revitalization) to piton (degraded function maintained as performance) as its founding problem (insufficient native Hebrew speakers) was solved but its institutional apparatus persisted. The mandatrophy is resolved by recognizing the transition: the institutional arrangements that were extractive and necessary during early revitalization have become extractive and unnecessary. The prescriptive apparatus continues to perform its boundary-defending function through elaborating rules and defending against natural language change, but this function no longer serves the original mandate and instead serves institutional self-preservation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    literacy_bias_measurement,
    'Does the observed suppression of vernacular forms stem from the inherent properties of the language or from the literacy-institutional bias of measurement systems that privilege written standards?',
    'Comparison of oral corpus data (recorded speech, song, narrative traditions) against prescriptive grammar judgments; measurement of vernacular innovation rates in informal speech vs formal writing; historical analysis of which linguistic features were ''problems'' before institutional standardization existed',
    'If literacy bias: much of the suppression disappears when measured in oral contexts. The constraint recalculates as a coordination problem with lower extractiveness. If inherent: the suppression measurement stands and the tangled-rope classification is validated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(literacy_bias_measurement, empirical, 'Literacy bias in measuring suppression of vernacular forms').

omega_variable(
    native_generation_sufficiency,
    'Is ''living language'' defined by native intergenerational transmission of colloquial competence, or does institutional reproduction (schools, liturgy, media) count as adequate linguistic life?',
    'Longitudinal tracking of child language acquisition: percentage of children acquiring the language as a primary native vernacular vs second language through formal instruction; measurement of spontaneous generation of novel linguistic structures outside institutional contexts; historical precedent from other revived languages (Irish, Basque, Catalan) on the sufficiency threshold for ''living language'' status',
    'If native generation required: current institutional success (widespread literacy, media presence, academic study) may not constitute genuine revival. The constraint reclassifies as snare for vernacular communities. If institutional reproduction sufficient: the revival has achieved its goal and the constraint''s extractiveness should be declining over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(native_generation_sufficiency, conceptual, 'Definition of ''living language'' — native generation vs institutional reproduction').

omega_variable(
    canonical_text_authority_grounding,
    'Does the canonical text (liturgical corpus, founding literature) ground authority through genuine linguistic necessity or through institutional path-dependency and theological commitment?',
    'Comparative analysis: examine whether prescriptive rules derived from canonical texts actually improve linguistic clarity/efficiency vs merely preserve textual fidelity; analysis of which canonical features were productive in original contexts vs which are archaic artifacts; examine whether alternative canonical bases (spoken corpora, regional literatures) would produce structurally different prescriptive systems',
    'If linguistic necessity: standardization around canonical forms represents legitimate coordination. If path-dependent institutional commitment: standardization is extractive constraint maintained by theological/institutional inertia rather than linguistic function. Reclassifies toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(canonical_text_authority_grounding, conceptual, 'Grounding of canonical text authority — linguistic necessity vs institutional path-dependency').

omega_variable(
    natural_law_vs_constructed_constraint,
    'Is the prescriptive-descriptive gap a natural law inherent to all human languages, or a constructed institutional arrangement specific to literacy-based societies with centralized language authorities?',
    'Cross-linguistic and cross-historical analysis: measurement of prescriptive-descriptive gap variation across languages with different authority structures (oral cultures with fluent speaker consensus vs institutionalized academies); examination of whether the gap size correlates with institutional standardization efforts rather than linguistic properties; analysis of whether pre-institutional and post-institutional versions of the same language show systematically different gap magnitudes',
    'If natural law: the mountain perspective is validated and the constraint is inherent to linguistic experience. If constructed: the mountain is a false summit and the constraint is a contingent institutional arrangement subject to redesign.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_constraint, empirical, 'Natural law vs constructed institutional arrangement: prescriptive-descriptive gap magnitude and causation').

omega_variable(
    vernacular_innovation_suppression_mechanism,
    'What is the primary mechanism suppressing vernacular innovation: explicit institutional enforcement (correction, punishment, gatekeeping of prestige), internalized identity capture (speakers who have fused their identity with canonical forms), or structural economic incentives (careers and status concentrated in institutional domains)?',
    'Ethnographic and sociolinguistic analysis of correction episodes: who corrects whom, in what contexts, with what consequences; psychological testing of identity fusion (do speakers report shame/loss-of-identity when using non-canonical forms); economic analysis of opportunity structures (do speakers pursuing education/careers face material incentives toward canonical usage); longitudinal tracking of which speakers innovate vernacular forms and what consequences they report experiencing',
    'If explicit enforcement: suppression can be reduced through policy change; the constraint is maintained by force and could be dismantled. If identity fusion: suppression persists even after enforcement machinery is removed; the constraint has internalized; reclassifies toward mountain from the speaker''s perspective. If economic incentive: suppression follows material interest; the constraint is extractive but may be addressable through alternative opportunity structures.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vernacular_innovation_suppression_mechanism, empirical, 'Mechanism of vernacular suppression — enforcement vs identity fusion vs economic incentive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vernacular_displacement, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vd_theater_early_20th, vernacular_displacement, theater_ratio, 0, 0.45).
narrative_ontology:measurement(vd_theater_mid_20th, vernacular_displacement, theater_ratio, 25, 0.55).
narrative_ontology:measurement(vd_theater_late_20th, vernacular_displacement, theater_ratio, 50, 0.68).
narrative_ontology:measurement(vd_theater_early_21st, vernacular_displacement, theater_ratio, 75, 0.74).

% Extraction over time
narrative_ontology:measurement(vd_extraction_early_20th, vernacular_displacement, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(vd_extraction_mid_20th, vernacular_displacement, base_extractiveness, 25, 0.65).
narrative_ontology:measurement(vd_extraction_late_20th, vernacular_displacement, base_extractiveness, 50, 0.55).
narrative_ontology:measurement(vd_extraction_early_21st, vernacular_displacement, base_extractiveness, 75, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(vd_suppression_early_20th, vernacular_displacement, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(vd_suppression_mid_20th, vernacular_displacement, suppression_requirement, 25, 0.62).
narrative_ontology:measurement(vd_suppression_late_20th, vernacular_displacement, suppression_requirement, 50, 0.5).
narrative_ontology:measurement(vd_suppression_early_21st, vernacular_displacement, suppression_requirement, 75, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vernacular_displacement, identity_coordination).
narrative_ontology:affects_constraint(vernacular_displacement, yiddish_institutional_displacement).
narrative_ontology:affects_constraint(vernacular_displacement, ladino_minority_language_suppression).
narrative_ontology:affects_constraint(vernacular_displacement, linguistic_purity_doctrine_enforcement).

% DUAL FORMULATION NOTE:
% Vernacular displacement as a constraint operates at multiple nested levels: (1) the language-revitalization level (Hebrew institutional coordination), (2) the regional-linguistic-ecology level (displacement of Yiddish, Ladino, Palestinian Arabic), and (3) the commitment-system level (contested kernel of what constitutes 'living language'). The three affected constraints represent decomposition along these levels. Each has distinct ε values: language revitalization exhibits moderate extractiveness (0.55); institutional displacement of competing languages exhibits higher extractiveness (0.65+); and the commitment-system contest exhibits the full range of classification depending on which reading is adopted (0.0 mountain to 0.85 snare). These are not alternative measurements of one constraint but structurally distinct constraints with interdependent causation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
