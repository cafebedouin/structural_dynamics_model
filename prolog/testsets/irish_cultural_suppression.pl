% ============================================================================
% CONSTRAINT STORY: irish_cultural_suppression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_irish_cultural_suppression, []).

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
 *   constraint_id: irish_cultural_suppression
 *   human_readable: Irish Cultural Suppression (16th-20th centuries)
 *   domain: cultural_political/postcolonial
 *
 * SUMMARY:
 *   Irish cultural suppression (16th-20th centuries) represents a systematic
 *   extraction mechanism targeting language, religious practice, education,
 *   and cultural institutions. The constraint operated through layered legal
 *   frameworks (Penal Laws, education restrictions, land policy) that created
 *   comprehensive barriers to cultural transmission while generating
 *   extractive benefits (labor, land rent, political submission) for English
 *   crown administration and Protestant landlord elites. The constraint's
 *   evolution reveals the piton mechanism: by the late 19th century, formal
 *   suppression was increasingly performative (theater_ratio rising from 0.35
 *   to 0.62) even as extractive mechanisms persisted. Cultural nationalism,
 *   literary revival, and organized resistance demonstrated functional
 *   failure of cultural erasure, yet institutional suppression machinery
 *   continued through inertia. The constraint was finally undermined by Irish
 *   independence and deliberate cultural revival efforts, but residual
 *   mechanisms persisted (education curricula bias, language attitude stigma)
 *   long after legal suppression ended.
 *
 * KEY AGENTS:
 *   - Irish peasant class: Primary victims (powerless/trapped) — landless laborers facing legal prohibition on education, language, cultural assembly; comprehensive economic dependence on English landlords
 *   - Irish language speakers: Primary victims (powerless/trapped) — legal liability for Gaelic transmission; intergenerational language loss through forced English-medium education
 *   - Irish Catholic gentry: Secondary victims/organized challengers (organized/constrained) — restricted education and commercial access; provided coordination function in parallel institutional development but constrained by penal restrictions
 *   - English crown administration: Primary beneficiary (institutional/arbitrage) — captures political control, land extraction, tax revenue, military recruitment; maintains enforcement infrastructure
 *   - Protestant landlord class: Primary beneficiary (powerful/arbitrage) — captures land surplus, tenant control, political authority; mobile exit option through land ownership and administrative roles
 *   - British imperial apparatus (late period): Institutional maintainer (institutional/constrained) — perpetuates culturally suppressive curricula and administrative structures through inertia despite functional failure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(irish_cultural_suppression, 0.68).
domain_priors:suppression_score(irish_cultural_suppression, 0.85).
domain_priors:theater_ratio(irish_cultural_suppression, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(irish_cultural_suppression, extractiveness, 0.68).
narrative_ontology:constraint_metric(irish_cultural_suppression, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(irish_cultural_suppression, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(irish_cultural_suppression, snare).
narrative_ontology:human_readable(irish_cultural_suppression, "Irish Cultural Suppression (16th-20th centuries)").
narrative_ontology:topic_domain(irish_cultural_suppression, "cultural_political/postcolonial").

domain_priors:requires_active_enforcement(irish_cultural_suppression).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(irish_cultural_suppression, english_crown_administration).
narrative_ontology:constraint_beneficiary(irish_cultural_suppression, protestant_landlord_class).
narrative_ontology:constraint_victim(irish_cultural_suppression, irish_catholic_population).
narrative_ontology:constraint_victim(irish_cultural_suppression, irish_language_speakers).
narrative_ontology:constraint_victim(irish_cultural_suppression, irish_cultural_institutions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IRISH PEASANT CLASS (SNARE) — Landless or tenant-dependent laborers with legal prohibition on education, language practice, and cultural assembly. Exit barriers are comprehensive: economic dependence on English landlords, legal restrictions on movement, suppression of alternative livelihood pathways through cultural institutions. No coordination benefit; pure extraction of labor and cultural compliance.
constraint_indexing:constraint_classification(irish_cultural_suppression, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: IRISH LANGUAGE SPEAKERS (SNARE) — Prohibition on Gaelic language in schools, courts, and official contexts. Penal Laws created legal liability for Irish-medium education and cultural practice. No exit option: full suppression of native language transmission creates intergenerational extraction — each generation loses fluency and cultural continuity.
constraint_indexing:constraint_classification(irish_cultural_suppression, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: IRISH GENTRY AND CATHOLIC ELITE (TANGLED ROPE) — Organized actors with some negotiating capacity but facing significant extraction and identity suppression. Provided minimal access to education and commerce in exchange for political submission; benefited from coordination of legal disputes and commercial networks but constrained by penal restrictions. Mixed experience: some benefit from institutional participation, significant extraction through political loyalty demands.
constraint_indexing:constraint_classification(irish_cultural_suppression, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: ENGLISH CROWN ADMINISTRATION (ROPE) — Primary beneficiary. Experiences the constraint as coordination mechanism: cultural suppression enables political control, land extraction, tax collection, and military recruitment. Enforcement infrastructure (penal laws, landlord deputization, military garrison) generates direct benefits. High arbitrage capacity — can revoke restrictions selectively or relocate administrative focus.
constraint_indexing:constraint_classification(irish_cultural_suppression, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PROTESTANT LANDLORD CLASS (ROPE) — Secondary beneficiaries capturing land, labor surplus, and political authority through cultural suppression mechanisms. Experiences constraint as coordination: penal laws provide legal framework for rent extraction and tenant control. Mobile exit option — land ownership provides arbitrage capacity; can shift to new estates or administrative roles if local resistance increases.
constraint_indexing:constraint_classification(irish_cultural_suppression, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: BRITISH IMPERIAL APPARATUS (PITON) — By late 19th century, cultural suppression had become largely performative theater maintained through institutional inertia. Gaelic revival, cultural nationalism, and literary renaissance demonstrated that suppression of language and culture was failing functionally. Yet the legal and administrative apparatus persisted through bureaucratic momentum, education curricula resistant to change, and cultural prejudice embedded in institutions. Theater ratio (0.58) reflects gap between performative cultural suppression and its actual efficacy.
constraint_indexing:constraint_classification(irish_cultural_suppression, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE NATURAL LAW (MOUNTAIN) — Risk of naturalizing the constraint as inherent cultural hierarchy or inevitable colonial dominance ('it was just how power worked then'). This naturalization obscures the deliberate institutional mechanisms (penal laws, education restrictions, land policy) that made suppression possible. The analytical perspective must reject the mountain framing — this is a snare contingent on specific enforcement infrastructure, not a law of nature.
constraint_indexing:constraint_classification(irish_cultural_suppression, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(irish_cultural_suppression_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(irish_cultural_suppression, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(irish_cultural_suppression, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(irish_cultural_suppression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(irish_cultural_suppression, TR),
    TR >= 0.70.

:- end_tests(irish_cultural_suppression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint generates substantial asymmetric extraction for beneficiaries: direct benefits include land rent (Irish tenants paying above-market rates under legal discrimination), labor surplus (suppressed wages for Irish workers legally excluded from skilled trades), and political submission (military recruitment, tax collection, administrative compliance). The value reflects that suppression was multi-channel extraction, not single-mechanism rent-seeking. Peak extraction at t=300 (0.72) reflects peak penal law enforcement and industrial-era economic integration. Suppression (0.85): Very high. Comprehensive barriers to exit: legal prohibition on education and cultural practice (enforceable through courts and landlord agents), economic dependence (land monopoly and occupational restrictions), geographic isolation (limited internal migration pathways for Irish population), and systematic undermining of alternative institutions (suppression of native schools, religious orders, cultural assemblies). Barriers operated across legal, economic, and institutional dimensions simultaneously. Theater ratio (0.58): Moderate-high, rising over the interval. Early period (t=0-150) shows primarily functional suppression — penal laws actively enforced, extraction mechanisms directly tied to cultural prohibition. Middle period (t=150-300) shows increasing theater as gap emerges between legal suppression and actual cultural practice (Gaelic revival literature, underground schools, private language transmission). Late period (t=300-400) shows substantial performative content — curricula nominally suppress Irish, administrative structures maintain anti-Irish bias, cultural prejudice embedded in institutions, but actual suppression of living culture has substantially failed. The piton classification derives from this theater rise: the constraint persists through institutional inertia despite functional failure.
 *
 * PERSPECTIVAL GAP:
 *   The constraint's classification ranges from snare (powerless agents perceive pure extraction with no exit) to piton (institutional apparatus maintains degraded suppression through theatrical persistence) to rope (beneficiaries perceive coordination mechanism for political control). The original research group (English administration) sees coordination — suppression enables efficient political integration and resource extraction. The powerless Irish population sees pure extraction — legal barriers to language transmission, economic dependence, no coordination benefit. Organized Irish actors (gentry, nationalist movements) see mixed extraction and coordination — some institutional participation enabled alongside asymmetric suppression. The late-period British apparatus sees theater — the performative cultural suppression persists through educational curricula and cultural prejudice despite manifest functional failure. This perspectival gap is diagnostic: as the constraint transitions from functional snare to performative piton, the classification diversity increases. Early period shows more uniform snare across victim perspectives (all experience severe extraction); late period shows more diversity as theater mechanisms partially decouple from actual cultural suppression outcomes.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) derives from structural position: who benefits, who bears costs, what are exit options. English administration benefits maximally (d ≈ 0.05: full beneficiary with arbitrage exit). Protestant landlord class benefits significantly (d ≈ 0.10: beneficiary with arbitrage/mobile exit). Irish peasantry bears maximum costs (d ≈ 0.95: full victim with trapped exit — legal prohibition, economic dependence, no alternative livelihood). Irish gentry occupy intermediate position (d ≈ 0.60: mixed position with constrained exit — some institutional participation, significant suppression, high cost to exit). The sigmoid f(d) transforms directionality into effective extraction multipliers: powerless trapped agents experience maximum chi even if base extraction is moderate; institutional beneficiaries with arbitrage exit experience minimal or negative effective extraction. The network effect: beneficiary directionality creates low/negative chi (appears as coordination to them), victim directionality creates high chi (appears as pure extraction to them), explaining the perpectival gap.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by showing classification diversity is real perspectival difference, not analytical ambiguity. The snare classification is stable from victim perspectives (trapped agents perceive pure extraction). The rope classification is stable from beneficiary perspectives (arbitrage-exit agents perceive coordination). The piton classification emerges in late period (institutional theater rising as functional suppression fails). The false summit (mountain from analytical civilizational perspective) must be rejected: the constraint is not inherent to human cultural interaction or inevitable colonial dominance, but contingent on specific legal-economic-institutional mechanisms that can be (and were) dismantled. The mandatrophy does not resolve by finding a single 'correct' type, but by recognizing that the perspectival distribution itself is diagnostic: a constraint that appears as pure snare to all powerless victims, rope to all institutional beneficiaries, and piton to late-period maintainers is demonstrating high structural clarity. The constraint is doing what it does — extracting from the culturally dominated, benefiting the culturally dominant — and different agents experience that extraction relative to their structural position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_internalization_depth,
    'What proportion of measured suppression was structural (legal/economic barriers) versus internalized (colonized consciousness, shame in cultural identity)?',
    'Post-independence cultural recovery patterns; correlation between legal suppression removal and cultural practice revival; linguistic analysis of internalized language attitudes in Irish speech communities',
    'If primarily structural: removal of penal laws should enable rapid cultural recovery (supports sunset logic). If primarily internalized: cultural recovery requires generational healing even after legal barriers removed (supports persistent piton or identity-locked exit mechanisms).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_depth, empirical, 'Structural versus internalized mechanisms in cultural suppression').

omega_variable(
    identity_locked_vs_trapped_boundary,
    'For Irish speakers and cultural practitioners, does suppression function as a trapped exit (material barriers to language transmission) or identity_locked exit (colonized consciousness preventing self-perception as culturally viable)?',
    'Historical documentation of language transmission choices within Irish families during suppression period; linguistic anthropology of code-switching and language shift attitudes; accounts of cultural practice in private vs public spheres',
    'If trapped: law removal should restore language practice (Penal Law repeal → language recovery). If identity_locked: removal of laws alone insufficient — psychological reframing of Irish cultural value required (explains 20th century Gaelic revival as requiring nationalist identity reconstruction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_locked_vs_trapped_boundary, conceptual, 'Whether exit is materially trapped or cognitively identity-locked').

omega_variable(
    extractive_vs_assimilationist_intent,
    'Was cultural suppression primarily extractive (maximizing labor/resource flow to English beneficiaries) or assimilationist (intent to absorb Irish into English cultural identity)?',
    'Analysis of penal law structure and incentive design; examination of selective application across different social strata; historical intent documentation from English administrators',
    'If extractive: classification as snare is robust (pure extraction logic). If assimilationist: constraint may have had residual coordination function (civilization narrative) that some agents partially internalized, making aspects tangled_rope or identity_coordination rather than pure snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extractive_vs_assimilationist_intent, conceptual, 'Whether mechanism was pure extraction or cultural assimilation').

omega_variable(
    geographic_scale_variation,
    'Did suppression intensity vary significantly across Irish regions (Ulster plantation areas vs. Connacht vs. Munster), creating local rope-like coordination in some areas alongside regional snares?',
    'Regional comparison of penal law enforcement, land distribution patterns, language decline trajectories, and institutional integration across provinces',
    'If variation is significant: constraint may decompose into multiple regional stories with different ε values. If uniform: single story appropriate. Network relationships between regional variants should be declared.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geographic_scale_variation, empirical, 'Geographic variation in suppression intensity and mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(irish_cultural_suppression, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ics_tr_t0, irish_cultural_suppression, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ics_tr_t150, irish_cultural_suppression, theater_ratio, 150, 0.48).
narrative_ontology:measurement(ics_tr_t300, irish_cultural_suppression, theater_ratio, 300, 0.58).
narrative_ontology:measurement(ics_tr_t400, irish_cultural_suppression, theater_ratio, 400, 0.62).

% Extraction over time
narrative_ontology:measurement(ics_be_t0, irish_cultural_suppression, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(ics_be_t150, irish_cultural_suppression, base_extractiveness, 150, 0.68).
narrative_ontology:measurement(ics_be_t300, irish_cultural_suppression, base_extractiveness, 300, 0.72).
narrative_ontology:measurement(ics_be_t400, irish_cultural_suppression, base_extractiveness, 400, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(irish_cultural_suppression, identity_coordination).
narrative_ontology:boltzmann_floor_override(irish_cultural_suppression, 0.12).
narrative_ontology:affects_constraint(irish_cultural_suppression, penal_laws_economic_extraction).
narrative_ontology:affects_constraint(irish_cultural_suppression, irish_education_system_subordination).
narrative_ontology:affects_constraint(irish_cultural_suppression, land_monopoly_tenant_extraction).
narrative_ontology:affects_constraint(irish_cultural_suppression, linguistic_stigma_internalization).

% DUAL FORMULATION NOTE:
% Irish cultural suppression is the over-constraint linking three structurally distinct mechanisms: legal suppression (penal laws with specific enforcement), economic extraction (land monopoly and occupational restriction), and identity coordination (institutional embedding of cultural hierarchy). Each sub-mechanism has its own ε value and should be modeled as separate constraint stories. Cultural suppression as a unified story represents the aggregate effect; decomposition enables precision on which mechanisms persisted longest (identity internalization), which were most extractive (economic), and which failed first (legal enforcement).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(irish_cultural_suppression, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
