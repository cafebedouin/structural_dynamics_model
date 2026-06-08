% ============================================================================
% CONSTRAINT STORY: hebrew_living_language_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_living_language_flat_control, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hebrew_living_language_flat_control
 *   human_readable: Hebrew as Living Language Commitment
 *   domain: sociolinguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   The commitment 'Hebrew is a living language' operated under fundamentally
 *   different operational definitions across two millennia, creating a
 *   constraint whose classification depends critically on which definition of
 *   'living' is adopted and from whose structural position it is evaluated.
 *   From 70 CE (destruction of Second Temple, end of Hebrew vernacular use)
 *   through the 1880s, Hebrew was maintained as a liturgical and literary
 *   language with no native speakers and no vernacular domain coverage — yet
 *   religious authorities consistently claimed it was 'living' because it was
 *   actively used in prayer, study, and religious writing. The Zionist
 *   language revival movement (1880s-1920s) redefined 'living' to require
 *   vernacular use and native speakers, creating a new operational standard
 *   that delegitimized the prior two millennia of liturgical use as 'dormant'
 *   or 'dead' language. This redefinition enabled the suppression of Yiddish
 *   and other Jewish diaspora languages as 'not truly Jewish' while
 *   positioning Hebrew revival as restoration rather than innovation. By the
 *   1948 establishment of Israel, Hebrew had achieved the Zionist definition
 *   of 'living' (native speakers, full domain coverage), but at the cost of
 *   substantial extraction from Yiddish-speaking communities and from
 *   religious authorities who lost control over Hebrew's sacred boundaries.
 *   The constraint exhibits cyclical dynamics: extraction peaked during the
 *   active revival period (1920-1970, value 0.65-0.85 for suppression) when
 *   Yiddish was most aggressively suppressed, then declined as Hebrew's
 *   vernacular status became established fact and the definitional contest
 *   resolved in favor of the Zionist operational standard. Theater ratio
 *   shows similar cyclical pattern: rising during the period when 'living
 *   language' claims were most contested and performative (1880s-1970s), then
 *   declining as the empirical reality of Hebrew vernacular use made the
 *   claim less dependent on assertion.
 *
 * KEY AGENTS:
 *   - Zionist Movement Institutions: Primary beneficiary (institutional/arbitrage) — captured definitional authority over 'living language' and used it to legitimize Hebrew revival and Yiddish suppression; net beneficiary of the constraint
 *   - Religious Continuity Authorities: Mixed position (institutional/constrained) — benefited from Hebrew's continued transmission and institutional support, but lost control over sacred-language boundaries when vernacularization occurred; tangled_rope experience
 *   - Modern Hebrew Speakers: Secondary beneficiary (moderate/mobile) — gained a shared vernacular language and national linguistic identity; benefited from the revival without bearing the costs of the transition
 *   - Yiddish-Speaking Communities: Primary victim (powerless/identity_locked) — experienced linguistic delegitimization and institutional suppression; could not exit without abandoning Jewish communal participation; identity-locked because Yiddish was constitutive of diaspora Jewish identity
 *   - Liturgical-Only Practitioners: Secondary victim (moderate/constrained) — lost exclusive control over Hebrew usage norms and sacred boundaries; constrained by need to maintain Hebrew transmission while contesting vernacularization
 *   - Linguistic Accuracy Standards: Abstract victim (powerless/trapped) — the definitional ambiguity of 'living language' enabled contested claims to persist without empirical resolution; sociolinguistic precision was sacrificed to ideological goals
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language_flat_control, 0.48).
domain_priors:suppression_score(hebrew_living_language_flat_control, 0.62).
domain_priors:theater_ratio(hebrew_living_language_flat_control, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language_flat_control, extractiveness, 0.48).
narrative_ontology:constraint_metric(hebrew_living_language_flat_control, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(hebrew_living_language_flat_control, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_living_language_flat_control, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(hebrew_living_language_flat_control, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language_flat_control, tangled_rope).
narrative_ontology:human_readable(hebrew_living_language_flat_control, "Hebrew as Living Language Commitment").
narrative_ontology:topic_domain(hebrew_living_language_flat_control, "sociolinguistics/language_revitalization/commitment_systems").

domain_priors:requires_active_enforcement(hebrew_living_language_flat_control).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language_flat_control, 'c7260286-6c65-42c2-af78-fd5ecde01c49').
narrative_ontology:cs_kernel_codification('c7260286-6c65-42c2-af78-fd5ecde01c49', distributed).
narrative_ontology:cs_authority_grounding('c7260286-6c65-42c2-af78-fd5ecde01c49', distributed).
narrative_ontology:cs_created_at('c7260286-6c65-42c2-af78-fd5ecde01c49', '').

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(hebrew_living_language_flat_control, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language_flat_control, zionist_movement_institutions).
narrative_ontology:constraint_beneficiary(hebrew_living_language_flat_control, religious_continuity_authorities).
narrative_ontology:constraint_beneficiary(hebrew_living_language_flat_control, modern_hebrew_speakers).
narrative_ontology:constraint_victim(hebrew_living_language_flat_control, yiddish_speaking_communities).
narrative_ontology:constraint_victim(hebrew_living_language_flat_control, liturgical_only_practitioners).
narrative_ontology:constraint_victim(hebrew_living_language_flat_control, linguistic_accuracy_standards).
narrative_ontology:constraint_vindicates(hebrew_living_language_flat_control, linguistic_revitalization_possibility).
narrative_ontology:constraint_vindicates(hebrew_living_language_flat_control, sacred_vernacular_compatibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: YIDDISH DIASPORA (SNARE) — Identity-locked within traditional Jewish practice where Hebrew was liturgical only. The Zionist redefinition of 'living' to mean vernacular use suppressed Yiddish as the actual living Jewish language. Could not exit the linguistic reclassification without abandoning Jewish institutional participation. Experienced the constraint as extraction: their linguistic reality was delegitimized by a definitional shift they had no power to contest.
constraint_indexing:constraint_classification(hebrew_living_language_flat_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 2: LITURGICAL PRACTITIONERS (TANGLED ROPE) — Constrained by the need to maintain Hebrew's sacred status while navigating a world where 'living language' increasingly meant secular vernacular use. Benefited from Hebrew's continued transmission and institutional support, but bore costs when sacred-language boundaries were violated by vernacular normalization. Mixed coordination (preservation of transmission) and extraction (loss of control over usage norms).
constraint_indexing:constraint_classification(hebrew_living_language_flat_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ZIONIST INSTITUTIONS (ROPE) — Primary beneficiaries of the redefinition. Experienced the constraint as coordination: establishing Hebrew as a living vernacular solved the genuine problem of creating a shared language for diverse immigrant populations in Palestine/Israel. Had arbitrage-level exit (could have chosen another language, Yiddish, or multilingualism) but the Hebrew choice served both practical and ideological goals. Net beneficiary of the definitional shift.
constraint_indexing:constraint_classification(hebrew_living_language_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REVITALIZATION MOVEMENTS (SCAFFOLD) — Organized agents (language activists, endangered language communities, UNESCO frameworks) see Hebrew's trajectory as a temporary proof-of-concept with sunset logic: the claim 'Hebrew is living' was a transitional assertion needed to bootstrap vernacular use, but once established, the constraint dissolves into ordinary linguistic fact. The definitional ambiguity was scaffolding for the revival process, not a permanent extraction mechanism.
constraint_indexing:constraint_classification(hebrew_living_language_flat_control, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LINGUISTIC NATURALISM (MOUNTAIN) — From a civilizational perspective, 'living language' is an objective sociolinguistic category: a language is living if it has native speakers using it for full communicative range across domains. By this definition, Hebrew was not living in 1880 (liturgical only, no native speakers) and is living in 2026 (millions of native speakers, full domain coverage). The definitional ambiguity is resolved by empirical criteria. However, this perspective risks naturalizing what was actually a contingent institutional achievement — the analytical observer may be seeing a false summit.
constraint_indexing:constraint_classification(hebrew_living_language_flat_control, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: POST-REVIVAL LITURGICAL (PITON) — Contemporary religious authorities maintaining the claim 'Hebrew is living' in liturgical contexts where the original function (preserving transmission against language death) has been superseded by secular vernacular reality. The assertion persists through institutional inertia and identity maintenance, but the coordination function (ensuring Hebrew survival) is now redundant. What remains is largely performance of continuity with pre-revival tradition.
constraint_indexing:constraint_classification(hebrew_living_language_flat_control, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_living_language_flat_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hebrew_living_language_flat_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hebrew_living_language_flat_control, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_living_language_flat_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hebrew_living_language_flat_control, TR),
    TR >= 0.70.

:- end_tests(hebrew_living_language_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The constraint extracted substantially from Yiddish-speaking communities (linguistic delegitimization, institutional suppression) and from religious authorities (loss of sacred-language control), while benefiting Zionist institutions (definitional authority, ideological legitimacy) and modern Hebrew speakers (shared vernacular). The extraction was not maximal because genuine coordination occurred: Hebrew revival did solve the real problem of creating a shared language for diverse immigrant populations. The value reflects that the coordination function and extraction mechanism were genuinely intertwined — this is a tangled rope, not a pure snare. Suppression (0.62): Moderate-high. Significant active enforcement was required to suppress Yiddish and establish Hebrew as the dominant Jewish language: institutional policies (Hebrew-only schools, workplace language requirements), social pressure (Yiddish speakers stigmatized as 'diaspora mentality'), and ideological campaigns ('Hebrew is the only authentic Jewish language'). Suppression was highest during the active revival period (1920-1970, value 0.85) and has declined as Hebrew's dominance became self-sustaining. Theater ratio (0.35): Moderate. The claim 'Hebrew is living' had substantial performative content during the contested period (1880s-1970s) when the operational definition was disputed and the empirical reality was ambiguous. Asserting 'Hebrew is living' was partly a speech act attempting to make it true, not merely describing an existing fact. Theater ratio peaked during the most contested period (1920-1970, value 0.55) and has declined as the empirical reality of Hebrew vernacular use made the claim less dependent on assertion. Current value (0.35) reflects residual performativity in liturgical contexts where the claim persists despite functional redundancy.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how definitional ambiguity enables extraction while maintaining coordination function. Yiddish-speaking communities experienced pure extraction (snare) — their linguistic reality was delegitimized by a definitional shift they could not contest. Liturgical practitioners experienced mixed coordination and extraction (tangled_rope) — they benefited from Hebrew's transmission but lost control over usage norms. Zionist institutions experienced coordination (rope) — they solved a genuine language-planning problem while capturing definitional authority. Contemporary revitalization movements see scaffolding (scaffold) — the definitional ambiguity was temporary infrastructure for the revival process. The analytical observer risks seeing natural law (mountain) — 'living language' is an objective sociolinguistic category — but this naturalizes what was actually a contingent institutional achievement dependent on state power and ideological mobilization. The perspectival gap is not 'which definition is correct?' but 'whose structural position determines which definition gets institutionalized?' The constraint's classification depends on whether you measure from the seat of the beneficiary (coordination), the victim (extraction), or the analytical observer (false summit risk).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural relationship to the definitional shift and its enforcement. Zionist institutions are primary beneficiaries (d ≈ 0.1-0.2) — they captured definitional authority and used it to legitimize their language-planning goals; arbitrage-level exit options (could have chosen another language) combined with institutional power produce low d and negative effective extraction (they collect from the constraint). Yiddish-speaking communities are primary victims (d ≈ 0.8-0.9) — they bore the costs of linguistic delegitimization and suppression; identity-locked exit options (could not abandon Yiddish without dissolving diaspora Jewish identity) combined with powerlessness produce high d and maximum effective extraction. Religious authorities occupy a middle position (d ≈ 0.4-0.5) — they benefited from Hebrew's transmission but lost control over sacred boundaries; constrained exit options (needed to maintain Hebrew transmission) combined with institutional power produce moderate d. Modern Hebrew speakers are secondary beneficiaries (d ≈ 0.2-0.3) — they gained a shared vernacular without bearing transition costs; mobile exit options combined with moderate power produce low-moderate d. The directionality spread (0.1 to 0.9) reflects genuine structural differentiation: the same constraint extracts maximally from trapped victims and subsidizes arbitrage-positioned beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that tangled_rope classification captures constraints where coordination and extraction are structurally inseparable. The Hebrew revival genuinely solved a coordination problem (creating a shared language for diverse immigrants) AND genuinely extracted from Yiddish speakers (linguistic delegitimization) through the same mechanism (redefining 'living language' to require vernacular use). The coordination function does not excuse the extraction, and the extraction does not negate the coordination — both are structural facts. The constraint is not mislabeled rope (ignoring Yiddish suppression) or mislabeled snare (ignoring genuine language-planning coordination). It is correctly classified as tangled_rope from the analytical perspective, with perspectival variation reflecting different agents' structural positions: victims see snare, beneficiaries see rope, and the analytical observer sees the hybrid structure. The mandatrophy question 'is this coordination or extraction?' is resolved by 'it is both, and the classification system must capture that.'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    living_language_definition_ambiguity,
    'What operational definition of ''living language'' determines whether the commitment was true across the two-millennium span?',
    'Sociolinguistic consensus on necessary and sufficient conditions for linguistic vitality: native speakers, domain coverage, intergenerational transmission, or merely continued use in any form?',
    'If ''living'' requires native speakers: the commitment was false 1880-1920, true after. If ''living'' means any active use: the commitment was continuously true. If ''living'' means liturgical transmission counts: no discontinuity exists. The classification shifts from tangled_rope (contested definition enabling extraction) to rope (genuine coordination under shared definition) or mountain (objective linguistic fact).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(living_language_definition_ambiguity, conceptual, 'Operational definition of ''living language'' across historical contexts').

omega_variable(
    liturgical_vernacular_boundary,
    'Was the liturgical-only use of Hebrew (70 CE - 1880s) a form of ''living'' language, or does ''living'' require vernacular use?',
    'Historical linguistics analysis of liturgical language vitality; comparison with Latin, Sanskrit, Classical Arabic in similar roles; sociolinguistic theory of language death vs dormancy',
    'If liturgical use counts as ''living'': Zionist language planning was continuation, not revival, and the Yiddish suppression was pure extraction (snare from more perspectives). If liturgical use is ''dormant'' not ''living'': Zionist revival was genuine coordination solving a real problem (rope from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liturgical_vernacular_boundary, conceptual, 'Whether liturgical-only use constitutes linguistic vitality').

omega_variable(
    yiddish_suppression_necessity,
    'Was the suppression of Yiddish (and other Jewish diaspora languages) necessary for Hebrew revival, or was it extractive overreach?',
    'Counterfactual analysis of multilingual language planning outcomes; comparison with other successful language revivals (Irish, Māori, Welsh) that did not suppress competing languages; sociolinguistic research on diglossia and language competition',
    'If suppression was necessary: the extraction was coordination cost (tangled_rope confirmed). If suppression was unnecessary: the extraction was pure rent-seeking by Zionist institutions (snare from more perspectives), and the ''living language'' claim was cover for linguistic imperialism within the Jewish community.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(yiddish_suppression_necessity, empirical, 'Necessity of Yiddish suppression for Hebrew revival success').

omega_variable(
    sacred_vernacular_tension,
    'Does vernacularization of a sacred language constitute desecration, preservation, or transformation?',
    'Theological and anthropological analysis of sacred language boundaries; comparative study of other sacred-to-vernacular transitions (Arabic, Sanskrit); community testimony from religious authorities across denominations',
    'If desecration: the ''living language'' claim extracted from religious communities by violating sacred boundaries (higher extractiveness, snare from religious perspectives). If preservation: the claim coordinated successful transmission (rope from more perspectives). If transformation: the claim was contested legitimately (tangled_rope confirmed, with irreducible normative disagreement).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sacred_vernacular_tension, preference, 'Normative status of sacred language vernacularization').

omega_variable(
    revitalization_replicability,
    'Is Hebrew''s trajectory replicable for other endangered languages, or was it contingent on unique historical circumstances?',
    'Comparative analysis of language revitalization attempts; identification of necessary conditions (institutional support, population concentration, ideological motivation, literacy tradition); success/failure rate analysis',
    'If replicable: the ''living language'' commitment is a generalizable coordination mechanism (rope/scaffold from more perspectives), and Hebrew is a proof-of-concept for linguistic revitalization. If contingent: the commitment was specific to Zionist state-building and cannot be separated from its extractive elements (tangled_rope/snare from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revitalization_replicability, empirical, 'Generalizability of Hebrew revival model to other languages').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language_flat_control, 0, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebrew_living_theater_70ce, hebrew_living_language_flat_control, theater_ratio, 0, 0.1).
narrative_ontology:measurement(hebrew_living_theater_570ce, hebrew_living_language_flat_control, theater_ratio, 500, 0.12).
narrative_ontology:measurement(hebrew_living_theater_1070ce, hebrew_living_language_flat_control, theater_ratio, 1000, 0.15).
narrative_ontology:measurement(hebrew_living_theater_1570ce, hebrew_living_language_flat_control, theater_ratio, 1500, 0.2).
narrative_ontology:measurement(hebrew_living_theater_1870ce, hebrew_living_language_flat_control, theater_ratio, 1800, 0.28).
narrative_ontology:measurement(hebrew_living_theater_1920ce, hebrew_living_language_flat_control, theater_ratio, 1850, 0.45).
narrative_ontology:measurement(hebrew_living_theater_1970ce, hebrew_living_language_flat_control, theater_ratio, 1900, 0.55).
narrative_ontology:measurement(hebrew_living_theater_2020ce, hebrew_living_language_flat_control, theater_ratio, 1950, 0.35).

% Extraction over time
narrative_ontology:measurement(hebrew_living_extract_70ce, hebrew_living_language_flat_control, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(hebrew_living_extract_570ce, hebrew_living_language_flat_control, base_extractiveness, 500, 0.18).
narrative_ontology:measurement(hebrew_living_extract_1070ce, hebrew_living_language_flat_control, base_extractiveness, 1000, 0.22).
narrative_ontology:measurement(hebrew_living_extract_1570ce, hebrew_living_language_flat_control, base_extractiveness, 1500, 0.25).
narrative_ontology:measurement(hebrew_living_extract_1870ce, hebrew_living_language_flat_control, base_extractiveness, 1800, 0.3).
narrative_ontology:measurement(hebrew_living_extract_1920ce, hebrew_living_language_flat_control, base_extractiveness, 1850, 0.42).
narrative_ontology:measurement(hebrew_living_extract_1970ce, hebrew_living_language_flat_control, base_extractiveness, 1900, 0.65).
narrative_ontology:measurement(hebrew_living_extract_2020ce, hebrew_living_language_flat_control, base_extractiveness, 1950, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(hebrew_living_suppress_70ce, hebrew_living_language_flat_control, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(hebrew_living_suppress_1870ce, hebrew_living_language_flat_control, suppression_requirement, 1800, 0.25).
narrative_ontology:measurement(hebrew_living_suppress_1920ce, hebrew_living_language_flat_control, suppression_requirement, 1850, 0.7).
narrative_ontology:measurement(hebrew_living_suppress_1970ce, hebrew_living_language_flat_control, suppression_requirement, 1900, 0.85).
narrative_ontology:measurement(hebrew_living_suppress_2020ce, hebrew_living_language_flat_control, suppression_requirement, 1950, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language_flat_control, identity_coordination).

% DUAL FORMULATION NOTE:
% This is the flat construction control for the Hebrew living language substrate. No decomposition into readings is performed. Contestation over operational definitions of 'living language' is captured through perspectival disagreement and omega variables rather than through separate reading stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
