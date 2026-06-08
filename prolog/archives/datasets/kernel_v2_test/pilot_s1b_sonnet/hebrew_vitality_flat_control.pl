% ============================================================================
% CONSTRAINT STORY: hebrew_vitality_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_vitality_flat_control, []).

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
    narrative_ontology:flat_control_of/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hebrew_vitality_flat_control
 *   human_readable: Hebrew as a Living Language
 *   domain: sociolinguistics/language_revitalization/jewish_studies
 *
 * SUMMARY:
 *   Hebrew vitality as a constraint operates across multiple timescales and
 *   linguistic registers. The claim 'Hebrew is a living language' is
 *   simultaneously true (Hebrew never ceased as a liturgical and literary
 *   language across two millennia) and contested (Hebrew's status as a
 *   vernacular mother tongue required active institutional construction
 *   beginning in the late 19th century). The constraint exhibits both genuine
 *   coordination function (enabling communication for a geographically
 *   dispersed, multilingual population) and asymmetric extraction
 *   (suppression of Yiddish, Ladino, Judeo-Arabic, and other Jewish
 *   languages; loss of intergenerational cultural transmission for non-Hebrew
 *   heritage). The extraction peaked during the state-building period
 *   (1948-1970) when institutional suppression was most intense, and has
 *   declined as native Hebrew-speaking generations matured and as digital
 *   access to suppressed languages has improved. The theater ratio reflects
 *   the gap between performative claims of 'natural revival' and the
 *   documented institutional enforcement mechanisms.
 *
 * KEY AGENTS:
 *   - Yiddish-Speaking Communities: Primary victims (powerless/trapped) — bore direct language loss and cultural erasure during immigration and state-building periods
 *   - Zionist State Apparatus: Primary beneficiary (institutional/arbitrage) — captured linguistic standardization as a nation-building tool; extracted compliance through education, administration, and cultural policy
 *   - Hebrew Language Academy: Institutional beneficiary (institutional/arbitrage) — agenda-setting authority over lexical and pedagogical standards; net beneficiary of linguistic coordination function
 *   - Ulpan System Operators: Institutional beneficiaries (institutional/arbitrage) — language education infrastructure that mediates immigrant integration; benefits from mandatory Hebrew acquisition
 *   - Second-Generation Israelis: Mixed position (moderate/constrained) — native speakers who benefit from linguistic membership but face constrained access to non-Hebrew Jewish heritage
 *   - Diaspora Linguistic Autonomy: Abstract victim (powerless/trapped) — the capacity for non-Israeli Jewish communities to maintain linguistic self-determination was structurally constrained by Israeli cultural hegemony
 *   - Polyglot Revival Movement: Organized agents (organized/mobile) — contemporary initiatives to revitalize Yiddish, Ladino, and other suppressed languages; see Hebrew hegemony as temporary phase
 *   - Analytical Observer: Sees tangled rope structure — genuine coordination and asymmetric extraction inseparably intertwined
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_vitality_flat_control, 0.35).
domain_priors:suppression_score(hebrew_vitality_flat_control, 0.4).
domain_priors:theater_ratio(hebrew_vitality_flat_control, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_vitality_flat_control, extractiveness, 0.35).
narrative_ontology:constraint_metric(hebrew_vitality_flat_control, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(hebrew_vitality_flat_control, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_vitality_flat_control, tangled_rope).
narrative_ontology:human_readable(hebrew_vitality_flat_control, "Hebrew as a Living Language").
narrative_ontology:topic_domain(hebrew_vitality_flat_control, "sociolinguistics/language_revitalization/jewish_studies").

domain_priors:requires_active_enforcement(hebrew_vitality_flat_control).

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(hebrew_vitality_flat_control, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_vitality_flat_control, zionist_state_apparatus).
narrative_ontology:constraint_beneficiary(hebrew_vitality_flat_control, ulpan_system_operators).
narrative_ontology:constraint_beneficiary(hebrew_vitality_flat_control, modern_hebrew_literary_establishment).
narrative_ontology:constraint_beneficiary(hebrew_vitality_flat_control, religious_educational_institutions).
narrative_ontology:constraint_victim(hebrew_vitality_flat_control, yiddish_speaking_communities).
narrative_ontology:constraint_victim(hebrew_vitality_flat_control, diaspora_linguistic_autonomy).
narrative_ontology:constraint_victim(hebrew_vitality_flat_control, non_hebrew_jewish_languages).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: YIDDISH-SPEAKING IMMIGRANT (SNARE) — Arrived in mandatory Palestine or early Israeli state with native Yiddish competence. Faced institutional suppression of Yiddish in schools, public administration, and cultural life. No exit option — geographic, economic, and political circumstances made remaining in place mandatory. Hebrew acquisition became survival requirement, not coordination benefit. Experienced the language regime as pure extraction: loss of communicative autonomy, cultural erasure, intergenerational transmission disruption.
constraint_indexing:constraint_classification(hebrew_vitality_flat_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: SECOND-GENERATION ISRAELI (TANGLED ROPE) — Native Hebrew speaker with access to employment, education, and civic participation through linguistic membership. Benefits from coordination function: Hebrew enables communication across formerly fragmented diasporic communities. But also bears costs: limited access to non-Hebrew Jewish cultural heritage, normative pressure against multilingualism, career penalties for insufficient Hebrew fluency in specific registers. Mixed experience — both coordinated and constrained.
constraint_indexing:constraint_classification(hebrew_vitality_flat_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HEBREW LANGUAGE ACADEMY (ROPE) — Institutional beneficiary with agenda-setting authority over lexical standardization, orthographic norms, and pedagogical frameworks. Experiences the constraint as pure coordination: solving the genuine problem of linguistic interoperability for a multilingual immigrant society. Has arbitrage-level exit — can shift between advocacy, policy influence, and academic prestige as institutional context changes. Net beneficiary of the arrangement.
constraint_indexing:constraint_classification(hebrew_vitality_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DIASPORA EDUCATIONAL NETWORK (TANGLED ROPE) — Organized agents (synagogue schools, community centers, Hillel chapters) benefit from Hebrew as a unifying educational standard and cultural touchstone, but also face resource allocation trade-offs: teaching Modern Hebrew competes with teaching Yiddish, Ladino, Judeo-Arabic, or other heritage languages. Constrained exit — can reduce Hebrew emphasis but faces institutional pressure and funding incentives tied to Israeli cultural alignment. Mixed coordination and extraction.
constraint_indexing:constraint_classification(hebrew_vitality_flat_control, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: POLYGLOT REVIVAL MOVEMENT (SCAFFOLD) — Organized agents (National Yiddish Book Center, Ladino language preservation initiatives, Judeo-Arabic cultural projects) see Hebrew hegemony as a temporary phase in Jewish linguistic history. Active work to revitalize suppressed languages creates alternative pathways. The sunset logic: as digitization, diaspora cultural confidence, and post-Zionist scholarship mature, the exclusive claim that Hebrew alone is the living Jewish language weakens. Estimated sunset: 2-3 generations for linguistic pluralism norms to establish institutional footing.
constraint_indexing:constraint_classification(hebrew_vitality_flat_control, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ZIONIST HISTORIOGRAPHY / NATURALIZED VIEW (MOUNTAIN) — From the institutional perspective embedded in state education and nationalist historiography, Hebrew vitality is presented as the natural outcome of historical forces: the language was 'revived' because it was always the authentic Jewish language, waiting to be reawakened. This framing naturalizes contingent institutional choices (Eliezer Ben-Yehuda's activism, mandatory schooling policies, Yiddish suppression campaigns) as inevitable restoration of natural order. The analytical observer sees this as a false summit: the 'naturalness' claim masks identifiable beneficiaries and victims.
constraint_indexing:constraint_classification(hebrew_vitality_flat_control, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Hebrew vitality as a constraint solves a genuine coordination problem: enabling communication and cultural production for a geographically dispersed, linguistically fragmented population. But it does so through asymmetric costs: Yiddish speakers bore language loss; Mizrahi Jews faced pressure to abandon Judeo-Arabic; diaspora autonomy was constrained by Israeli linguistic hegemony. The coordination function and extraction mechanism are inseparable — both exist, neither reducible to the other. This is the paradigmatic tangled rope structure.
constraint_indexing:constraint_classification(hebrew_vitality_flat_control, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_vitality_flat_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hebrew_vitality_flat_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hebrew_vitality_flat_control, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(hebrew_vitality_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The constraint extracts from non-Hebrew Jewish linguistic communities through institutional suppression, cultural devaluation, and intergenerational transmission disruption. But the extraction is not maximal — Hebrew also provides genuine coordination value, and contemporary revival efforts have partially reversed the suppression. The value reflects the net balance: real costs borne by Yiddish speakers and others, but not pure rent-seeking. Suppression (0.40): Moderate. Significant historical suppression (mandatory schooling in Hebrew, Yiddish-language press closures, social stigma against 'old-world' languages) but not total — private linguistic practice persisted, and suppression has declined substantially since the 1970s. Theater ratio (0.38): Moderate. The 'natural revival' narrative is partly performative — it naturalizes contingent institutional choices as inevitable restoration. But the performance is not total: Hebrew vitality does rest on genuine vernacular acquisition by multiple generations, not merely on ritual maintenance. The measurements show extraction and suppression peaking during state consolidation (1948-1970) and declining as native speakers mature and digital access reduces barriers to suppressed languages.
 *
 * PERSPECTIVAL GAP:
 *   The powerless Yiddish-speaking immigrant experiences pure extraction (snare) — Hebrew acquisition was survival requirement, not coordination benefit. The institutional Hebrew Language Academy experiences pure coordination (rope) — solving the genuine problem of linguistic interoperability. The second-generation Israeli experiences mixed coordination and extraction (tangled rope) — benefits from linguistic membership but bears costs of heritage language loss. The diaspora educational network faces resource trade-offs (tangled rope) — Hebrew teaching competes with other heritage languages. The polyglot revival movement sees a temporary phase with sunset logic (scaffold) — linguistic pluralism norms are being reconstructed. The nationalist historiography naturalizes the process as inevitable (mountain / false summit) — masks identifiable beneficiaries and victims. The analytical observer sees the paradigmatic tangled rope: coordination function and extraction mechanism inseparably intertwined, neither reducible to the other.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: Zionist state apparatus, ulpan operators, Hebrew Language Academy, modern literary establishment, and religious educational institutions all derive institutional authority, funding, or cultural capital from Hebrew's status as the dominant living Jewish language. These agents have arbitrage-level exit options (can shift between advocacy, policy, and academic contexts) and experience the constraint as coordination. Victims: Yiddish-speaking communities, diaspora linguistic autonomy, and speakers of Ladino, Judeo-Arabic, and other Jewish languages bore the costs of suppression, cultural erasure, and heritage loss. These agents had trapped or constrained exit options — geographic, economic, and political circumstances made remaining in place mandatory or high-cost. The directionality computation derives from these structural relationships: beneficiaries with arbitrage exit see low effective extraction (rope classification); victims with trapped exit see high effective extraction (snare classification); agents with mixed positions see moderate extraction (tangled rope classification).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that Hebrew vitality is neither pure coordination nor pure extraction. The coordination function is genuine: Hebrew enables communication and cultural production for a fragmented population. The extraction mechanism is also genuine: non-Hebrew Jewish languages were actively suppressed, and linguistic autonomy was constrained. The tangled rope classification from the analytical perspective captures this irreducible duality. The false summit risk (mountain classification from nationalist historiography) is the naturalization of contingent institutional choices: the claim that Hebrew vitality was inevitable restoration of natural order, rather than the outcome of identifiable policy decisions with identifiable beneficiaries and victims. The scaffold perspective (polyglot revival movement) shows that the exclusive Hebrew hegemony claim is weakening as linguistic pluralism norms mature — the constraint's extraction component has a sunset, even if the coordination component persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sacred_vs_vernacular_continuity,
    'Is the ''living language'' claim grounded in liturgical continuity (Hebrew never ceased as a sacred/literary language) or vernacular revival (Hebrew was successfully re-nativized as a mother tongue)?',
    'Linguistic historiography disambiguating sacred register persistence from vernacular acquisition patterns. If ''living'' means liturgical: continuity is real but uncontested. If ''living'' means vernacular: the claim depends on suppression of Yiddish and other spoken Jewish languages.',
    'If sacred continuity: vitality claim is descriptively true but trivial (Latin is also ''living'' in this sense). If vernacular revival: vitality claim depends on institutional enforcement and has identifiable victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sacred_vs_vernacular_continuity, conceptual, 'Whether ''living language'' claim rests on sacred or vernacular continuity').

omega_variable(
    counterfactual_yiddish_trajectory,
    'Absent Zionist institutional suppression, would Yiddish have remained the dominant vernacular for Ashkenazi Jews, or would modernization pressures have shifted Jewish linguistic practice toward majority languages (Russian, English, German) regardless?',
    'Comparative analysis of Yiddish retention rates in communities without Israeli institutional contact (Hasidic enclaves, Soviet Yiddish cultural apparatus pre-1948, Argentine Jewish community). Correlation between institutional suppression and language shift velocity.',
    'If Yiddish would have persisted: Hebrew vitality rests on active suppression (higher extraction). If Yiddish was already declining: Hebrew filled a vacuum (lower extraction, more coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_yiddish_trajectory, empirical, 'Counterfactual trajectory of Yiddish absent institutional Hebrew promotion').

omega_variable(
    multilingual_equilibrium_viability,
    'Could a multilingual equilibrium (Hebrew + Yiddish + Ladino + Judeo-Arabic as co-official languages) have achieved the coordination function without the observed linguistic homogenization?',
    'Comparative cases: Switzerland (4 official languages), India (22 scheduled languages), Belgium (3 official languages). Assessment of administrative overhead, inter-community communication costs, and cultural autonomy preservation in multilingual vs monolingual polities of similar size.',
    'If multilingual equilibrium viable: Hebrew exclusivity was a choice, not a necessity (higher extraction). If administratively infeasible: monolingual coordination was structurally required (lower extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(multilingual_equilibrium_viability, empirical, 'Whether multilingual coordination equilibrium was structurally viable').

omega_variable(
    generational_extraction_transfer,
    'Do second- and third-generation native Hebrew speakers experience linguistic extraction, or has the extraction been fully absorbed by the first-generation victims?',
    'Intergenerational cultural heritage access surveys. Measurement of non-Hebrew Jewish text accessibility (Yiddish literature, Ladino folk songs, Judeo-Arabic poetry) for successive generations. If access drops sharply: extraction persists as cultural disinheritance. If stable or recovering: extraction was one-time cost.',
    'If extraction transferred generationally: the constraint''s victim set expands beyond first-generation immigrants. If extraction absorbed: later generations are net beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generational_extraction_transfer, empirical, 'Whether linguistic extraction transfers across generations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality_flat_control, 0, 145).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(heb_vital_theater_1880, hebrew_vitality_flat_control, theater_ratio, 0, 0.2).
narrative_ontology:measurement(heb_vital_theater_1910, hebrew_vitality_flat_control, theater_ratio, 30, 0.3).
narrative_ontology:measurement(heb_vital_theater_1948, hebrew_vitality_flat_control, theater_ratio, 68, 0.45).
narrative_ontology:measurement(heb_vital_theater_1970, hebrew_vitality_flat_control, theater_ratio, 90, 0.5).
narrative_ontology:measurement(heb_vital_theater_1995, hebrew_vitality_flat_control, theater_ratio, 115, 0.42).
narrative_ontology:measurement(heb_vital_theater_2025, hebrew_vitality_flat_control, theater_ratio, 145, 0.38).

% Extraction over time
narrative_ontology:measurement(heb_vital_extract_1880, hebrew_vitality_flat_control, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(heb_vital_extract_1910, hebrew_vitality_flat_control, base_extractiveness, 30, 0.28).
narrative_ontology:measurement(heb_vital_extract_1948, hebrew_vitality_flat_control, base_extractiveness, 68, 0.52).
narrative_ontology:measurement(heb_vital_extract_1970, hebrew_vitality_flat_control, base_extractiveness, 90, 0.58).
narrative_ontology:measurement(heb_vital_extract_1995, hebrew_vitality_flat_control, base_extractiveness, 115, 0.48).
narrative_ontology:measurement(heb_vital_extract_2025, hebrew_vitality_flat_control, base_extractiveness, 145, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(heb_vital_suppress_1880, hebrew_vitality_flat_control, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(heb_vital_suppress_1910, hebrew_vitality_flat_control, suppression_requirement, 30, 0.25).
narrative_ontology:measurement(heb_vital_suppress_1948, hebrew_vitality_flat_control, suppression_requirement, 68, 0.7).
narrative_ontology:measurement(heb_vital_suppress_1970, hebrew_vitality_flat_control, suppression_requirement, 90, 0.65).
narrative_ontology:measurement(heb_vital_suppress_1995, hebrew_vitality_flat_control, suppression_requirement, 115, 0.5).
narrative_ontology:measurement(heb_vital_suppress_2025, hebrew_vitality_flat_control, suppression_requirement, 145, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_vitality_flat_control, identity_coordination).
narrative_ontology:affects_constraint(hebrew_vitality_flat_control, yiddish_cultural_transmission).
narrative_ontology:affects_constraint(hebrew_vitality_flat_control, ladino_language_preservation).
narrative_ontology:affects_constraint(hebrew_vitality_flat_control, judeo_arabic_literary_heritage).

% DUAL FORMULATION NOTE:
% Hebrew vitality as a constraint is upstream of specific Jewish language preservation efforts. The vitality claim establishes Hebrew as the dominant linguistic identity marker, which structurally constrains resources and legitimacy available to other Jewish languages. Each suppressed language has its own constraint story with its own extractiveness values; Hebrew vitality is the shared structural dependency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
