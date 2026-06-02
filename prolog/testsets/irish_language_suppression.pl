% ============================================================================
% CONSTRAINT STORY: irish_language_suppression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_irish_language_suppression, []).

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
 *   constraint_id: irish_language_suppression
 *   human_readable: Irish Language Suppression as Colonial Extraction
 *   domain: colonial_policy/linguistic_suppression/cultural_control
 *
 * SUMMARY:
 *   Irish language suppression is a 400+ year constraint operating through
 *   distinct extractive phases: (1) Active legal suppression via Penal Laws
 *   (1600-1829) combining direct prohibition with economic incentives
 *   favoring English; (2) Post-independence institutional persistence
 *   (1920-1980) where Ireland as an independent state maintained
 *   education-based suppression momentum despite removing the primary
 *   colonial beneficiary; (3) Contemporary cultural preservation
 *   (1980-present) where organized language communities are building
 *   alternative institutional pathways with sunset logic embedded in
 *   community revival rather than state mandate. The constraint exhibits the
 *   classical snare signature during the active suppression era
 *   (extractiveness 0.78-0.82, suppression 0.88, minimal theater) because the
 *   extraction mechanism is explicit and functional: the colonial
 *   administration extracts political compliance and labor-market advantage
 *   from linguistic hegemony, while the Irish population is trapped with no
 *   exit option. Post-independence, the constraint shifts toward piton
 *   classification as theater rises (0.75 by 1980) — the education system
 *   maintains Irish language requirements through institutional inertia
 *   despite the original extraction mechanism (colonial domination) having
 *   been removed. Contemporary revival movements represent a scaffold
 *   structure: organized agents (Gaelchultúr, Irish-medium schools,
 *   broadcasting) are creating alternative transmission pathways with an
 *   implicit sunset clause — as cultural institutions prove capable of
 *   sustaining Irish, the formal education mandate becomes redundant. The
 *   false-summit detection system will identify the civilizational analytical
 *   perspective (treating suppression as a natural law of imperial
 *   governance) as a naturalization of a constructed extractive mechanism.
 *   This is the core mandatrophy: distinguishing between legitimate
 *   governance coordination and extractive exploitation using linguistic
 *   homogenization as cover.
 *
 * KEY AGENTS:
 *   - English Colonial Administration (1600-1922): Institutional beneficiary (institutional/arbitrage) — captures governance simplification, administrative efficiency, and political compliance through linguistic standardization
 *   - Anglo-Irish Landlord Class (1600-1922): Institutional beneficiary (institutional/arbitrage) — extracts depressed wages justified by cultural inferiority, simplified tenant management, and cultural hegemony
 *   - Irish Native Population (1600-1922): Primary victim (powerless/trapped) — faces legal prohibition, economic penalties, and social stigmatization; trapped within colonial jurisdiction with no viable exit
 *   - Irish Linguistic Commons (1600-present): Abstract victim (powerless/trapped) — systematic destruction of language transmission with no agent to advocate for preservation during suppression era
 *   - Irish Catholic Church (1600-1920): Mixed institutional actor (moderate/constrained) — both beneficiary (maintains unified authority) and victim (loses Irish-language religious culture); enforces normalization while losing religious transmission capacity
 *   - Independent Irish State (1922-1980): Institutional maintainer (institutional/constrained) — inherits suppression structure; maintains education-based language policy despite removing extraction beneficiary; trapped by path-dependent institutions
 *   - Irish Language Preservation Movement (1980-present): Organized agents (organized/constrained) — building alternative institutions; see constraint as temporary with sunset logic embedded in community capacity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(irish_language_suppression, 0.78).
domain_priors:suppression_score(irish_language_suppression, 0.88).
domain_priors:theater_ratio(irish_language_suppression, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(irish_language_suppression, extractiveness, 0.78).
narrative_ontology:constraint_metric(irish_language_suppression, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(irish_language_suppression, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(irish_language_suppression, snare).
narrative_ontology:human_readable(irish_language_suppression, "Irish Language Suppression as Colonial Extraction").
narrative_ontology:topic_domain(irish_language_suppression, "colonial_policy/linguistic_suppression/cultural_control").

domain_priors:requires_active_enforcement(irish_language_suppression).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(irish_language_suppression, english_colonial_administration).
narrative_ontology:constraint_beneficiary(irish_language_suppression, anglo_irish_landlord_class).
narrative_ontology:constraint_victim(irish_language_suppression, irish_native_population).
narrative_ontology:constraint_victim(irish_language_suppression, irish_linguistic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IRISH NATIVE POPULATION — SNARE (1600-1920). Trapped within colonial jurisdiction with no exit. English monolingualism becomes a survival requirement: Irish-speaking peasants face economic penalties (hiring discrimination, land access restrictions), legal penalties (Penal Laws prohibiting Irish education and cultural institutions), and social penalties (stigmatization of Gaelic identity). Exit would require abandonment of cultural identity and linguistic heritage. The constraint extracts labor value (depressed wages for Irish workers perceived as culturally inferior) and political compliance (cultural assimilation reduces autonomous collective identity). Suppression is maximum — language is prohibited in schools, courts, and administrative contexts. The victims cannot organize a linguistic commons or coordinate resistance within the constraint itself.
constraint_indexing:constraint_classification(irish_language_suppression, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: IRISH LINGUISTIC COMMONS — SNARE (1600-1920). Abstract collective good — the transmission of Irish as a living language across generations. During active suppression, this commons faces systematic destruction with no exit mechanism. Parents self-suppress their children's Irish to enable economic survival in English-dominant labor markets. Each generation loses native speakers. The constraint extracts cultural capital and extractive power (Irish identity becomes subordinate to English-derived status markers). Suppression manifests as institutional prohibition (Penal Laws) and economic pressure (wages/employment favor English speakers). The linguistic commons has no agent to advocate for it, no exit option, no counter-power. Pure extraction — no coordination function.
constraint_indexing:constraint_classification(irish_language_suppression, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 3: ENGLISH COLONIAL ADMINISTRATION — ROPE (1600-1920). From the colonial administrative perspective, language suppression is a coordination mechanism: unified governance requires linguistic standardization. The administration sees itself as solving a coordination problem — how to administer a diverse territory through a single administrative language and legal system. English monolingualism enables centralized control, reduces administrative complexity, and creates a unified labor market. The administration experiences the constraint as coordination with extraction as a side effect, not the primary function. Exit from this constraint would require tolerating linguistic pluralism in governance, which the administration does not perceive as feasible. The administration captures benefits (simplified governance, labor market control) and does not bear the suppression costs (those fall on Irish-speaking population).
constraint_indexing:constraint_classification(irish_language_suppression, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: ANGLO-IRISH LANDLORD CLASS — ROPE (1600-1920). From the landlord perspective, Irish suppression coordinates an asymmetric labor market. Landlords benefit from depressed Irish wages (cultural inferiority stigma justifies lower pay), simplified tenant management (English-only contracts and communications), and cultural hegemony (Anglicization ensures tenant deference to English-derived authority). The landlords experience the constraint as coordination — it solves the problem of managing a large population with minimal overhead and maximum extraction. Exit would require dismantling the cultural hierarchy that justifies wage depression. The landlords capture maximum benefit (labor extraction, political compliance) while bearing minimal cost.
constraint_indexing:constraint_classification(irish_language_suppression, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: IRISH CATHOLIC CHURCH — TANGLED ROPE (1600-1920). The Church occupies a paradoxical position. It benefits from linguistic suppression in some dimensions (unified liturgy, centralized authority over diverse congregations, reduced risk of heterodox Irish religious movements) but bears extraction costs in others (prohibition of Irish language in some contexts, loss of Irish-language religious culture, pressure to use English in education and confessional practice). The Church is both beneficiary (controls religious authority regardless of language) and victim (loses Irish-language religious transmission). This is genuine Tangled Rope: active enforcement required (the Church enforces linguistic normalization through education), coordination function present (unified religious authority), but asymmetric extraction (some Church functions benefit from suppression while others are constrained). The Church can exit through language accommodation, but faces institutional costs (reduced standardization, increased complexity).
constraint_indexing:constraint_classification(irish_language_suppression, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: IRISH LANGUAGE EDUCATION SYSTEMS (PITON, 1920s-1980s). After Irish independence (1922), the Free State/Irish Republic attempted to revive Irish through compulsory education programs. However, the revival mechanism became substantially performative (theater_ratio ≥ 0.70) by mid-20th century: students learned Irish grammar in school but faced no social incentive to speak it (employment markets favored English); textbooks became theatrical (focused on literary Gaelic disconnected from living speech); examination requirements became compliance rituals rather than competence gates. The constraint persists (students must complete Irish education) despite loss of functional revival purpose. It is maintained by institutional inertia — the education system has 'become' Irish language revival — but the primary function (actually creating Irish-speaking communities) has atrophied. Theater ratio rises from 0.35 (active suppression with clear extraction function) to 0.75 (vestigial revival mechanism maintained through compliance expectation).
constraint_indexing:constraint_classification(irish_language_suppression, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 7: CONTEMPORARY IRISH LANGUAGE PRESERVATION MOVEMENT (SCAFFOLD, 1980s-present). Organized agents (Gaelchultúr organizations, Irish-medium schools, community language programs, media broadcasting in Irish) have constructed alternative institutional pathways for Irish transmission that bypass the degraded formal education system. These pathways create genuine coordination functions (Irish-language media coordinate cultural identity; Irish-medium schools create immersion coordination). The constraint has a sunset clause implicit in the mechanism: as Irish becomes viable through cultural institutions rather than compulsory education, the formal education requirement loses force. The organized movement experiences the constraint as temporary (educational mandate will become obsolete as cultural transmission succeeds through community choice). Exit is constrained but visible — if community Irish-language capacity reaches critical mass, the formal mandate becomes redundant. Theater ratio drops as actual cultural transmission replaces performative education.
constraint_indexing:constraint_classification(irish_language_suppression, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER — MOUNTAIN (CIVILIZATIONAL VIEW). From a civilizational/universal analytical perspective, one might perceive Irish suppression as a natural law of colonial governance: large empires must impose linguistic standardization for administrative efficiency. Language consolidation around the imperial language is treated as inevitable, not contingent. However, this perspective will trigger the false-summit detection mechanism: the structural data declares beneficiaries (colonial administration, landlord class) and victims (Irish population, linguistic commons), showing that the 'naturalness' is a framing choice, not an empirical fact. The constraint is constructed and extractive, not an immutable law of governance.
constraint_indexing:constraint_classification(irish_language_suppression, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(irish_language_suppression_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(irish_language_suppression, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(irish_language_suppression, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(irish_language_suppression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(irish_language_suppression, TR),
    TR >= 0.70.

:- end_tests(irish_language_suppression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78, currently 0.38 post-2000): During active suppression (1600-1920), extractiveness is maximum because the mechanism is explicitly extractive — linguistic hegemony directly translates to labor market control, political compliance, and cultural subordination. The colonial administration and landlord class capture direct benefit (simplified governance, wage depression, political authority). Post-independence, extractiveness declines (0.72 by 1920, 0.65 by 1950, 0.38 by 2000) because the primary extraction mechanism (colonial exploitation) is removed. Remaining extractiveness in mid-20th century reflects path-dependent institutional structures (education system designed for English) and residual economic advantages of English fluency in global markets. Contemporary low extractiveness (0.38) reflects that linguistic suppression is no longer a primary extraction mechanism — Irish-medium education and community revival have created alternative pathways not structured around extraction. Suppression (0.88, constant during active era; declining post-1980): During Penal Laws period, suppression is maximum — Irish is legally prohibited in courts, education, and administrative contexts. Economic pressure ensures suppression persists even after legal prohibition relaxes (1829). Post-1922 suppression declines because formal prohibition is removed and state policy flips to support revival (though education mandate persists). Contemporary suppression is substantially lower (0.35-0.45) because community Irish-language contexts provide viable alternatives to suppression-based enforcement. Theater ratio (0.35 active era → 0.75 by 1950 → 0.62 by 2000): During active suppression, theater is low because the extraction mechanism is explicit and functional — suppression serves a clear purpose (political compliance, labor extraction). The performative dimension is minimal; the constraint functions as intended. Post-1920, theater rises sharply (0.58 by 1950, 0.75 by 1980) because the education system is maintaining Irish language requirements even though the original extraction purpose (colonial political dominance) is gone. The education mandate becomes increasingly performative — students learn Irish grammar for exams but have no social incentive to speak it; textbooks focus on literary Gaelic disconnected from living speech. This is the piton signature: performative maintenance of a defunct functional mechanism. Contemporary theater declines (0.62 by 2000) because organized community movements are creating genuine cultural transmission outside the performative education system.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence — from snare (victims' view during suppression) to rope (beneficiaries' coordination view) to piton (post-independence institutional view) to scaffold (contemporary preservation movement view) to mountain (analytical naturalization view). The original Irish population experienced the constraint as a snare with no exit: legal prohibition + economic pressure + cultural stigmatization creates a lock with no visible escape route. The colonial administration experienced it as rope: language coordination that solves governance problems while generating valuable side effects (labor market control, political compliance). The Irish Catholic Church experienced it as tangled rope: benefits from unified authority but loses Irish-language religious transmission. The post-independence Irish state experienced it as piton: inherited institutional structures that became constituted by their own failure (education system designed for mandatory Irish but generating no living speakers). The contemporary language movement experiences it as scaffold: alternative institutions with sunset logic (as community Irish capacity grows, formal mandate becomes obsolete). The civilizational analytical observer risks experiencing it as mountain: imperial language consolidation as a natural law. However, the structural data (beneficiaries = colonial administration + landlords; victims = Irish population + linguistic commons) triggers false-summit detection, revealing that the 'naturalness' is a naturalization of constructed extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined by each agent's structural position relative to the extraction flow. Victims (Irish population, linguistic commons) with trapped exit options derive high d (0.92-0.95 for trapped agents) → high f(d) (1.35-1.42) → high experienced extractiveness chi. Beneficiaries (colonial administration, landlords) with arbitrage exit options derive low d (0.05-0.15) → negative f(d) (-0.12 to -0.01) → negative or zero effective extraction (they experience coordination, not extraction). The Church as mixed actor derives moderate d (0.50-0.55 from constrained + mixed benefit/victim) → moderate f(d) (0.65-0.75). Post-independence Ireland as institutional maintainer derives higher d (0.60-0.70 from constrained + residual victim status even as formal power) because the state inherited the suppression structure and can only exit through institutional redesign (high cost). The contemporary language movement as organized beneficiary derives lower d (0.35-0.45 from constrained exit + genuine community benefit) → moderate f(d) (0.35-0.50). These directionality values are not overridden — they follow directly from beneficiary/victim declarations and exit options without explicit adjustment.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: Irish language suppression resolves the mandatrophy by demonstrating that the six classifications are not competing answers but sequential observations of the same constraint across historical phases. Active suppression (1600-1920) is genuinely snare: high extraction (explicit), high suppression (legal + economic), low theater (functional mechanism). The snare classification is not refuted by independence or by contemporary revival efforts — it remains accurate for the suppression era. Post-independence persistence (1920-1980) is genuinely piton: extractiveness declines (original beneficiary removed), but theater rises sharply (0.75 by 1980) because the education system maintains the constraint through institutional inertia despite its primary function (colonial extraction) being gone. The piton classification does not contradict the snare classification — it describes what happened when the snare lost its primary extraction mechanism but persisted through institutional momentum. Contemporary revival (1980-present) is genuinely scaffold: organized agents build alternative pathways, theater declines (0.62 by 2000) as actual cultural transmission replaces performative education, extractiveness continues declining (0.38 by 2000). The scaffold classification does not deny the snare or piton history — it describes the contemporary mechanism. The analytical mountain (treating suppression as natural law) is a false summit: the structural data (beneficiaries + victims) reveal that the constraint is constructed, not natural. The false-summit detection system identifies that someone (colonial administration, landlord class, or analytical observer) is naturalizing what is actually a designed extraction mechanism. This resolves the mandatrophy: the apparent contradiction between snare/piton/scaffold and mountain is resolved by recognizing that the mountain perspective is a naturalization attempt, not a legitimate classification. The constraint's true nature is snare-become-piton-become-scaffold, not a natural law of governance. No single type 'wins'; instead, the sequence of types across phases reveals the structure: extraction mechanism → institutional persistence of defunct mechanism → organized alternative pathways.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_coercion_vs_economic_incentive,
    'What proportion of Irish language abandonment resulted from direct legal prohibition versus economic incentives favoring English monolingualism?',
    'Historical analysis of Penal Laws enforcement rates versus labor market segregation by language; comparison of regions with strict legal enforcement versus those with primarily economic pressure; linguistic shift patterns post-1829 (when some Penal Laws relaxed but economic incentives persisted)',
    'If primarily legal: suppression is removable through policy change (as post-1922 revival policy attempted). If primarily economic: suppression persists as long as English dominance creates wage/employment premiums (explaining why Irish never revived despite independence and policy support). If mixed: revival requires both policy change AND economic restructuring.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_coercion_vs_economic_incentive, empirical, 'Relative contribution of legal prohibition versus economic incentives to language shift').

omega_variable(
    critical_mass_threshold_for_language_viability,
    'Below what percentage of native speakers does a language become unviable for intergenerational transmission, even with institutional support?',
    'Longitudinal tracking of Irish speaker populations through 20th century; comparison with other minority language cases (Welsh, Basque, Catalan); measurement of actual intergenerational transmission rates in Irish-medium households versus general population',
    'If threshold crossed by mid-20th century: current revival efforts are preservational, not revivalist (constraint goal is different from what is structurally possible). If threshold not yet crossed: revival is still possible with sufficient institutional investment. This determines whether the scaffold perspective''s sunset clause is realistic or aspirational.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(critical_mass_threshold_for_language_viability, empirical, 'Critical mass threshold for language viability').

omega_variable(
    colonial_extraction_legitimacy_frame,
    'Did the English colonial administration frame linguistic suppression as necessary governance (coordination narrative) or as civilizational extraction (explicit superiority narrative)?',
    'Historical analysis of colonial administrative documents, policy rationales, and public discourse; comparison of framing in different colonial contexts (India, Africa); examination of whether administrators believed the coordination narrative or treated it as rhetorical cover for extraction',
    'If primarily coordination narrative: the rope perspective captures the administrator''s genuine frame (they saw language policy as neutral governance requirement). If primarily extraction narrative: administrators knew they were extracting cultural capital and used coordination language as cover (snare is more accurate). This affects how we classify the false summit detection — is it revealing hidden extraction or naturalization of legitimate governance?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colonial_extraction_legitimacy_frame, conceptual, 'Colonial framing of suppression as governance versus cultural extraction').

omega_variable(
    post_colonial_institutional_persistence_mechanism,
    'Why did Irish language suppression persist through institutional momentum even after political independence removed the extractive beneficiary (colonial administration)?',
    'Comparison of Irish language policy 1922-1980 with other post-colonial linguistic revivals; analysis of teacher training, textbook production, and employment structures that became path-dependent on English; interviews/analysis of why Irish-medium schools expanded only from 1980s onward despite 60+ years of independence and policy support',
    'If institutional inertia is dominant: the piton classification is accurate — the education system became constituted by its failure, and revival required external organizational pressure (Gaelchultúr movement). If political/economic choices dominate: Ireland could have achieved revival earlier through different resource allocation, and post-colonial persistence is not institutional necessity but political choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_colonial_institutional_persistence_mechanism, empirical, 'Mechanism sustaining linguistic suppression post-independence').

omega_variable(
    irish_language_identity_lock_degree,
    'To what extent is Irish identity constituted through linguistic revival commitment versus maintained independently of language revival outcomes?',
    'Sociological analysis of Irish identity formation in non-Irish-speaking communities; comparison of identity persistence across generations in diaspora populations (where institutional Irish support is absent); measurement of identity satisfaction in English-only Irish communities versus Irish-medium communities',
    'If identity is highly language-dependent: current revival efforts are identity-essential, not merely cultural preservation. If identity persists independently: revival is valuable but not identity-constitutive. This affects whether non-Irish-speaking Irish people experience the constraint as identity_locked or merely as cultural loss.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(irish_language_identity_lock_degree, empirical, 'Degree of Irish identity constitution through language').

omega_variable(
    false_summit_colonial_naturalization,
    'Is the mountain classification (civilizational natural law of imperial governance) a genuine epistemological position or a naturalization of constructed colonial extraction?',
    'Comparative colonial history: did all empires suppress indigenous languages, or did some accommodate linguistic pluralism? Analysis of whether language consolidation was technically necessary for governance or chosen as extraction mechanism. Comparison with non-imperial multilingual governance systems (medieval Europe, Ottoman administrative pluralism, contemporary multilingual states).',
    'If genuinely natural: mountain classification is correct — language consolidation is an inherent requirement of large-scale governance. If naturalized extraction: false summit detection is triggered, revealing that colonial administration chose suppression as an extraction mechanism while framing it as inevitable. This is the core mandatrophy resolution for this constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_colonial_naturalization, conceptual, 'Whether colonial linguistic suppression is natural governance requirement or constructed extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(irish_language_suppression, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irls_theater_1600, irish_language_suppression, theater_ratio, 0, 0.15).
narrative_ontology:measurement(irls_theater_1800, irish_language_suppression, theater_ratio, 200, 0.22).
narrative_ontology:measurement(irls_theater_1920, irish_language_suppression, theater_ratio, 320, 0.35).
narrative_ontology:measurement(irls_theater_1950, irish_language_suppression, theater_ratio, 350, 0.58).
narrative_ontology:measurement(irls_theater_1980, irish_language_suppression, theater_ratio, 380, 0.75).
narrative_ontology:measurement(irls_theater_2000, irish_language_suppression, theater_ratio, 400, 0.62).

% Extraction over time
narrative_ontology:measurement(irls_extr_1600, irish_language_suppression, base_extractiveness, 0, 0.82).
narrative_ontology:measurement(irls_extr_1800, irish_language_suppression, base_extractiveness, 200, 0.78).
narrative_ontology:measurement(irls_extr_1920, irish_language_suppression, base_extractiveness, 320, 0.72).
narrative_ontology:measurement(irls_extr_1950, irish_language_suppression, base_extractiveness, 350, 0.65).
narrative_ontology:measurement(irls_extr_1980, irish_language_suppression, base_extractiveness, 380, 0.52).
narrative_ontology:measurement(irls_extr_2000, irish_language_suppression, base_extractiveness, 400, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(irish_language_suppression, identity_coordination).
narrative_ontology:affects_constraint(irish_language_suppression, anglo_irish_cultural_hegemony).
narrative_ontology:affects_constraint(irish_language_suppression, irish_education_system_language_policy).

% DUAL FORMULATION NOTE:
% Irish language suppression is the primary constraint; it has two network dependents that represent downstream effects. Anglo-Irish cultural hegemony is the broader institutional structure that language suppression sustains. Irish education system language policy is the post-independence institutional persistence mechanism that inherited the suppression framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(irish_language_suppression, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
