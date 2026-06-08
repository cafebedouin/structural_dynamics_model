% ============================================================================
% CONSTRAINT STORY: state_capacity_expansion
% ============================================================================
% Version: 2.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_capacity_expansion, []).

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
 *   constraint_id: state_capacity_expansion
 *   human_readable: State Capacity Expansion via Alphabet Reform: Turkey 1928
 *   domain: political_linguistics/state_formation/institutional_capacity
 *
 * SUMMARY:
 *   Turkey's 1928 alphabet reform (Law 1353) represents a state capacity
 *   expansion mechanism tested under extreme conditions: installation of a
 *   new commitment-system kernel (Latin-script literacy) with effectively
 *   zero pre-existing practitioners and a 4-month implementation window. The
 *   reform abolished Ottoman Arabic script through top-down decree,
 *   immediately rendering traditional literacy credentials obsolete and
 *   forcing total institutional re-education. This constraint instantiates a
 *   tangled-rope structure: genuine coordination benefit (unified script
 *   reduces administrative friction, enables standardized bureaucracy, census
 *   efficiency) layered with severe asymmetric extraction (traditional
 *   practitioners lose occupational status, religious authorities lose
 *   interpretive monopoly, Ottoman cultural continuity is ruptured). The
 *   reform is neither pure coordination (Rope) nor pure extraction (Snare),
 *   but a hybrid where the coordination function serves as the legitimating
 *   cover for extraction. From the state administrative apparatus
 *   perspective, the constraint is pure coordination—the efficiency gains are
 *   real and substantial. From the traditional literacy practitioner
 *   perspective, it is pure extraction—skills are instantly worthless and
 *   re-education is coerced. The constraint tests whether a state can install
 *   a new institutional kernel with no pre-existing reading, demonstrating
 *   that organizational will and coercive capacity can override the typical
 *   path-dependence of commitment systems.
 *
 * KEY AGENTS:
 *   - State Administrative Apparatus: Primary beneficiary (institutional/arbitrage) — captures efficiency gains, expands census/tax/military coordination capacity, achieves linguistic homogeneity for standardized administration
 *   - Traditional Literacy Practitioners (Ulema, Scribes, Clerks): Primary victim (powerless/trapped) — career skills rendered obsolete overnight; occupational monopoly on literacy dismantled; forced re-education; no exit mechanism available
 *   - Religious Institutional Authority (Vakif, Ulema Networks): Secondary victim (organized/constrained) — loses interpretive monopoly on scripture through script monopoly loss; forced to adopt new script in administrative dealings; can resist but faces institutional suppression
 *   - Urban Middle-Class Intellectuals: Mixed position (moderate/constrained) — benefits from administrative efficiency and national integration but bears cost of cultural deracination and compulsory ideological assimilation to Kemalist modernization
 *   - Kemalist Modernization Ideology: Beneficiary (non-agent, vindicated proposition) — the reform vindicates the claim that Turkey can be rapidly transformed into a modern nation-state through top-down institutional innovation
 *   - Ottoman Institutional Legacy: Piton actor (institutional/arbitrage) — formally preserved as historical continuity but functionally degraded and replaced; persists theatrically in institutional memory
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the contingent reform as a natural law of state development (false summit candidate)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_capacity_expansion, 0.68).
domain_priors:suppression_score(state_capacity_expansion, 0.72).
domain_priors:theater_ratio(state_capacity_expansion, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_capacity_expansion, extractiveness, 0.68).
narrative_ontology:constraint_metric(state_capacity_expansion, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(state_capacity_expansion, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_capacity_expansion, tangled_rope).
narrative_ontology:human_readable(state_capacity_expansion, "State Capacity Expansion via Alphabet Reform: Turkey 1928").
narrative_ontology:topic_domain(state_capacity_expansion, "political_linguistics/state_formation/institutional_capacity").

domain_priors:requires_active_enforcement(state_capacity_expansion).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_capacity_expansion, '27ac0dd5-031f-44c4-aa8d-0d9e7d108b3d').
narrative_ontology:cs_kernel_codification('27ac0dd5-031f-44c4-aa8d-0d9e7d108b3d', fixed_text).
narrative_ontology:cs_authority_grounding('27ac0dd5-031f-44c4-aa8d-0d9e7d108b3d', extraction).
narrative_ontology:cs_interpretation_layer_present('27ac0dd5-031f-44c4-aa8d-0d9e7d108b3d').
narrative_ontology:cs_reading_relation('27ac0dd5-031f-44c4-aa8d-0d9e7d108b3d', state_capacity_expansion__ottoman_script_reading, forecloses).
narrative_ontology:cs_reading_relation('27ac0dd5-031f-44c4-aa8d-0d9e7d108b3d', state_capacity_expansion__islamic_religious_authority_reading, forecloses).
narrative_ontology:cs_reading_relation('27ac0dd5-031f-44c4-aa8d-0d9e7d108b3d', state_capacity_expansion__pluralist_institutional_reading, influences).
narrative_ontology:cs_axiom('27ac0dd5-031f-44c4-aa8d-0d9e7d108b3d', foundational, turkish_identity_requires_latin_script).
narrative_ontology:cs_axiom_status(turkish_identity_requires_latin_script, holdable).
narrative_ontology:cs_axiom_grounding('27ac0dd5-031f-44c4-aa8d-0d9e7d108b3d', turkish_identity_requires_latin_script, conventional).
narrative_ontology:cs_axiom('27ac0dd5-031f-44c4-aa8d-0d9e7d108b3d', foundational, ottoman_institutional_continuity_incompatible_modernity).
narrative_ontology:cs_axiom_status(ottoman_institutional_continuity_incompatible_modernity, holdable).
narrative_ontology:cs_axiom_grounding('27ac0dd5-031f-44c4-aa8d-0d9e7d108b3d', ottoman_institutional_continuity_incompatible_modernity, instrumental).
narrative_ontology:cs_axiom('27ac0dd5-031f-44c4-aa8d-0d9e7d108b3d', secondary, secular_state_authority_supersedes_religious_authority).
narrative_ontology:cs_axiom_status(secular_state_authority_supersedes_religious_authority, holdable).
narrative_ontology:cs_axiom_grounding('27ac0dd5-031f-44c4-aa8d-0d9e7d108b3d', secular_state_authority_supersedes_religious_authority, deontological).
narrative_ontology:cs_axiom('27ac0dd5-031f-44c4-aa8d-0d9e7d108b3d', secondary, rapid_institutional_transformation_possible_via_state_decree).
narrative_ontology:cs_axiom_status(rapid_institutional_transformation_possible_via_state_decree, holdable).
narrative_ontology:cs_axiom_grounding('27ac0dd5-031f-44c4-aa8d-0d9e7d108b3d', rapid_institutional_transformation_possible_via_state_decree, empirically_contingent).
narrative_ontology:cs_reference_frame('27ac0dd5-031f-44c4-aa8d-0d9e7d108b3d', ottoman_multilingual_institutional_framework).
narrative_ontology:cs_drift_state('27ac0dd5-031f-44c4-aa8d-0d9e7d108b3d', post_reform_consolidation, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('27ac0dd5-031f-44c4-aa8d-0d9e7d108b3d', '2026-02-26T14:32:00Z').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_capacity_expansion, state_administrative_apparatus).
narrative_ontology:constraint_beneficiary(state_capacity_expansion, kemalist_modernization_ideology).
narrative_ontology:constraint_victim(state_capacity_expansion, traditional_literacy_practitioners).
narrative_ontology:constraint_victim(state_capacity_expansion, religious_institutional_authority).
narrative_ontology:constraint_victim(state_capacity_expansion, pre_reform_cultural_continuity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRADITIONAL LITERACY PRACTITIONER (SNARE) — Trapped in Arabic-script literacy with zero institutional support for continuation. Career skills instantly rendered occupationally obsolete; re-education mandated; cultural identity fused with script continuity. No exit: state apparatus monopolizes education, publishing, administration. Maximum extraction: the agent bears full cost of transition while state captures administrative efficiency gains. Traditional ulema, scribes, and Islamic scholars face loss of monopoly on literacy authority.
constraint_indexing:constraint_classification(state_capacity_expansion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: URBAN MIDDLE-CLASS INTELLECTUAL (TANGLED ROPE) — Constrained by state pedagogy but also benefits from unified national script and reduced complexity in administration. Experiences genuine coordination problem (common script reduces transaction costs) layered with extraction (compulsory re-education, cultural displacement, pressure to assimilate Kemalist ideology). Moderate exit cost: can resist individually but faces employment barriers and social stigma. Benefits from state capacity expansion (faster administration, reduced administrative friction) but pays the cost of cultural deracination.
constraint_indexing:constraint_classification(state_capacity_expansion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE ADMINISTRATIVE APPARATUS (ROPE) — Net beneficiary experiencing the constraint as pure coordination: unified script reduces administrative friction, enables standardized bureaucracy, facilitates census-taking, tax collection, and military administration. High arbitrage: can exit to other efficiency mechanisms (numeric codes, transliteration) but Latin script is the chosen vector. Primary beneficiary of extracted coordination value. Extraction IS the mechanism through which state capacity expands — the coordination is genuine, but it flows asymmetrically to the administrative center.
constraint_indexing:constraint_classification(state_capacity_expansion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: OTTOMAN INSTITUTIONAL LEGACY (PITON) — The pre-existing Ottoman administrative apparatus is partially preserved but functionalized into a new state form. The theater here is the rhetorical maintenance of Ottoman continuity alongside actual institutional transformation. The Ottoman scribal tradition (muharrirs, imperial bureaucrats) is formally disbanded and replaced; its residual institutional memory persists theatrically in continuity claims but functionally degrades into historical archives. Theater ratio high (0.58) because the institutional claim of continuity masks the severance. Piton classification reflects that the legacy institution is preserved as performance, not as operative function.
constraint_indexing:constraint_classification(state_capacity_expansion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: KEMALIST MODERNIZATION COALITION (SCAFFOLD) — Organized actors (state planners, education bureaucrats, nationalist intellectuals) see the alphabet reform as a temporary institutional mechanism to achieve permanent modernization. The constraint carries an implicit sunset: once Latin-script literacy is universal (estimated 2–3 generations), the enforcement apparatus can relax — the script becomes naturalized, and the state no longer needs to actively suppress Arabic-script literacy. Benefits from capacity expansion but understands the mechanism as transitional. Sunset logic: unified script → universal literacy → naturalized state capacity → enforcement infrastructure can diminish.
constraint_indexing:constraint_classification(state_capacity_expansion, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: RELIGIOUS INSTITUTIONAL AUTHORITY (TANGLED_ROPE) — Organized religious institutions (awqaf system, ulema networks, madrasas) experience the reform as both extraction and coordination. Genuine coordination benefit: unified script simplifies administrative interfaces between religious endowments and state. But severe asymmetric extraction: religious literacy authority (control of scriptural interpretation through script monopoly) is systematically dismantled. Constrained exit: cannot leave the national jurisdiction; can only attempt internal resistance (teaching Arabic in secret schools, theological justification for script continuity) which is suppressed. The religious institution is forced to both participate in the coordination (use new script in administrative dealings) and pay the extraction cost (loss of interpretive monopoly).
constraint_indexing:constraint_classification(state_capacity_expansion, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURALIZATION FRAME (MOUNTAIN) — From a civilizational/universal perspective, linguistic unification is treated as an immutable requirement of state formation: 'no state can exist without linguistic homogeneity' (Benedict Anderson thesis, naturalizes contingent institutional choices). This perspective risks treating the alphabet reform as a natural law of state development rather than a contingent political extraction mechanism. The perspective emerges naturally from functional state theory but represents a false summit: the 'necessity' of Latin script was not inherent to Turkish state formation — it was a choice made by agents with specific power and ideology. The engine's false summit detector will identify this as naturalization of a contingent institutional arrangement backed by organized political will and coercive capacity.
constraint_indexing:constraint_classification(state_capacity_expansion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_capacity_expansion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(state_capacity_expansion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(state_capacity_expansion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_capacity_expansion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(state_capacity_expansion, TR),
    TR >= 0.70.

:- end_tests(state_capacity_expansion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): Moderate-high. The reform extracts substantial value for the state apparatus (administrative efficiency, military capacity, standardized census administration) while imposing severe costs on traditional practitioners. The initial low value (0.15 at t=0) reflects the pre-announcement period when the constraint has not yet been imposed; the spike to 0.52 at t=2 captures the immediate enforcement period and peak coercion; the sustained elevation through t=10 (0.65-0.68) reflects the active re-education and suppression phase; the decline by t=20 (0.42) models gradual naturalization as Latin-script literacy becomes universal and suppression apparatus relaxes. Suppression (0.72): High. The state maintains active suppression machinery: legal prohibition on Arabic-script publishing in official contexts, educational monopoly on Latin-script pedagogy, employment penalties for non-Latin-script literacy, ideological delegitimization of Ottoman/Islamic script as 'backward.' The trajectory shows intense suppression at t=1-3 (0.72-0.75, immediately post-reform), sustained enforcement through t=7 (0.68), gradual relaxation as naturalization occurs (0.45 by t=15), and substantial persistence even at t=30 (0.25, residual suppression through cultural stigma). Theater ratio (0.58): Moderate-high. The state's stated rationale (administrative efficiency) is genuine, but the reform serves dual purposes: efficiency AND ideological assimilation. The performative element includes: (1) rhetorical claims that Latin script is 'natural' for Turkish despite zero pre-existing practitioners; (2) theatrical maintenance of Ottoman institutional continuity while actually dismantling Ottoman administrative structures; (3) framing forced re-education as 'enlightenment' rather than coercion. The theater remains elevated (0.55-0.60) throughout the period because the efficiency gains are real (reducing theater-ratio magnitude) but the ideological assimilation narrative is continuous (sustaining theater-ratio elevation).
 *
 * PERSPECTIVAL GAP:
 *   This constraint generates a maximum perspectival gap between beneficiary and victim. The state apparatus experiences the constraint as pure coordination (Rope): 'We are solving the genuine problem of administrative friction through linguistic unification.' The traditional practitioner experiences it as pure extraction (Snare): 'My skills are worthless, I am being coerced to re-educate, my identity is being erased.' The religious institution experiences it as tangled rope: 'The unified script does reduce administrative friction for our endowment management, but at the cost of our interpretive authority.' The Kemalist coalition experiences it as a temporary scaffold: 'We are installing a new capacity mechanism that will become naturalized within a generation.' The Ottoman legacy experiences it as a piton: 'We are formally preserved but functionally degraded.' The analytical observer risks a false summit: 'Linguistic unification is a natural law of state development.' Each perspective has a genuine structural foundation—the constraint really does coordinate and really does extract simultaneously—but the salience of each function depends entirely on the observer's structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) flows from power × exit options × beneficiary/victim status. The state apparatus (institutional/arbitrage) benefits from the constraint and has arbitrage options (could achieve efficiency through transliteration, parallel scripts, etc.); derivation chain produces low d (~0.1-0.2), yielding negative χ (subsidy). The traditional practitioner (powerless/trapped) is a victim with zero exit options; derivation produces high d (~0.9), yielding maximum χ (high extraction). The religious institution (organized/constrained) is a victim with limited but non-zero exit options (can attempt internal resistance, teach in secret); derivation produces high d (~0.75), yielding high χ (substantial extraction). The urban intellectual (moderate/constrained) is a mixed position (moderate extraction benefit from coordination, cost from cultural displacement); derivation produces moderate d (~0.55), yielding moderate χ. The beneficiary and victim declarations are the primary inputs to this chain: beneficiaries feed low d; victims feed high d. The engine derives d automatically from these declarations plus power and exit options; no manual directionality calculation is needed or appropriate.
 *
 * MANDATROPHY ANALYSIS:
 *   ACTIVE MANDATROPHY: The state's founding mandate for the alphabet reform was 'achieve administrative efficiency and national linguistic unification to enable state capacity expansion.' This mandate has been ACHIEVED by t=10-20: Latin-script literacy is nearly universal, administrative efficiency gains are realized, state capacity has expanded substantially, linguistic unification is complete. However, the constraint PERSISTS (suppression_requirement still 0.25 at t=30, theater_ratio still 0.55) even though the founding mandate is satisfied. Mandatrophy resolution requires examining whether (1) persistence is due to new mandates emerging (e.g., ideological enforcement of Kemalist cultural modernization has become the mandate, separate from administrative efficiency), or (2) the original mandate was never the real driver (extraction was the real function, and 'efficiency' was the cover story). The measurement trajectory suggests: efficiency mandate satisfied by t=10 (extractiveness drops from peak 0.68 to 0.65 as naturalization increases), but suppression remains high through t=15, suggesting a second mandate (ideological assimilation/erasure of Ottoman identity) continues driving enforcement after the stated mandate is complete. This is classic mandatrophy: the original justification (efficiency) no longer explains persistence; the constraint persists through institutional inertia, ideological commitment, or unstated extraction agenda. Mandatrophy_resolved field is FALSE because the tension between founding mandate and actual persistence is unresolved and requires ongoing analyst attention.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    literacy_continuity_vs_rupture,
    'Is the alphabet reform a rupture in literacy continuity (extraction from traditional practitioners) or a natural phase transition in state capacity development (coordination for administrative efficiency)?',
    'Comparison of literacy rates pre/post reform, retention of Arabic-script literacy in underground/religious contexts, demographic analysis of who lost occupational status vs. who gained administrative access. Longitudinal survey of perceived constraint type from different cohorts across 50-year horizon.',
    'If rupture dominates: Snare classification confirmed across victim perspectives. If transition dominates: Rope classification gains credibility. The empirical answer determines whether the state''s capacity expansion is legitimate coordination or extractive coercion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(literacy_continuity_vs_rupture, empirical, 'Whether alphabet reform constitutes literacy rupture or natural state development phase transition').

omega_variable(
    kemalist_ideology_naturalization,
    'Did Kemalist modernization ideology use alphabet reform as a technical efficiency mechanism, or did alphabet reform serve as a primary instrument for ideological assimilation and erasure of Ottoman/Islamic identity?',
    'Archival analysis of Atatürk and early Turkish state policy documents; comparison of stated justifications (administrative efficiency) vs. actual policy effects (suppression of Arabic literacy, isolation of religious education, cultural homogenization); cross-reference with contemporary sources from affected communities describing perceived intent and impact.',
    'If technical efficiency dominates: Tangled Rope classification is correct (genuine coordination + extraction). If ideological assimilation dominates: Snare classification gains ground (extraction using coordination cover). The answer determines whether suppression (0.72) reflects proportionate state enforcement or disproportionate cultural erasure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kemalist_ideology_naturalization, empirical, 'Primary function of alphabet reform: administrative efficiency vs. ideological assimilation').

omega_variable(
    alphabetic_kernel_codification,
    'Can the alphabet itself be treated as a commitment-system kernel (a contested foundational claim about what counts as ''Turkish''), or is it merely a technical implementation detail of state capacity rather than a normative commitment?',
    'Textual analysis of Turkish constitutional and educational law treating script choice as foundational vs. instrumental; examination of whether subsequent states or movements have contested the Latin-script choice as a core commitment vs. proposing practical alternatives; evaluation of symbolic weight assigned to Latin script in national identity mythology.',
    'If kernel: the reform instantiates a reading of Turkish identity that forecloses or influences alternative readings (Ottoman, Islamic identity); CS-structure classification applies. If instrumental: the reform is a pure extraction mechanism using administrative cover; no CS-structure needed. The answer determines whether to author cs_structure and, if so, what kernel_codification and authority_grounding apply.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alphabetic_kernel_codification, conceptual, 'Whether alphabet choice functions as commitment-system kernel or technical implementation').

omega_variable(
    suppression_mechanism_coercion_vs_naturalization,
    'Is the measured suppression (0.72) primarily active state coercion (enforcement machinery, legal prohibitions, economic penalties) or internalized acceptance (agents come to see Latin script as natural/inevitable and Arabic script as backward)?',
    'Post-suppression-apparatus-failure observation: if coercive apparatus were removed, would Arabic-script literacy revive? Measurement of internal resistance (underground Arabic schools, persistent private use) during the reform period. Long-term survey of attitudes toward the reform among descendants of suppressed communities: is opposition structural or primarily historical memory?',
    'If coercion dominates: suppression reflects state enforcement capacity (external mechanism); constraint would relax with enforcement relaxation. If naturalization dominates: suppression is partially internalized (agents carry the constraint without external enforcement); constraint persists even after enforcement apparatus weakens. High naturalization proportion increases effective suppression magnitude.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_coercion_vs_naturalization, empirical, 'Whether suppression operates through active coercion or internalized acceptance').

omega_variable(
    beneficiary_capture_vs_intentional_coordination,
    'Did the state deliberately design the alphabet reform as a coordination mechanism for administrative efficiency, or did state actors rationalize extractive ideological imposition as ''administrative necessity'' after the fact?',
    'Temporal analysis: if administrative benefit was primary motivation, efficiency gains should precede ideology justification in archival record; if ideology was primary, justifications should precede efficiency claims. Counterfactual analysis: were there alternative mechanisms for achieving the stated administrative goals (e.g., transliteration, parallel-script administration) that were rejected? If rejected, on what grounds (technical or ideological)?',
    'If coordination primary: Tangled Rope classification is justified (genuine + extraction). If ideology primary: Snare classification is more accurate (extraction using efficiency as cover). Determines whether beneficiary classification (state_administrative_apparatus) is appropriately primary or whether kemalist_modernization_ideology should dominate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_vs_intentional_coordination, empirical, 'Whether alphabet reform was designed for coordination or rationalized as such post-hoc').

omega_variable(
    false_summit_state_necessity,
    'Is the mountain perspective (linguistic unification as natural law of state formation) a genuine natural law or a false summit concealing a contingent choice backed by political will?',
    'Comparative state analysis: do all surviving states require linguistic homogeneity, or do functionally successful multi-script states exist? Historical counterfactual: what would Ottoman-Turkish state formation have required if Latin script had not been available? Can administrative capacity expand through other mechanisms (transliteration, diglossia, institutional pluralism)?',
    'If genuine natural law: mountain classification is correct; alphabet reform is immutable requirement. If false summit: state capacity expansion via alphabet reform is a contingent institutional choice that could have been made differently; the constraint becomes re-frameable as extractive ideology in retrospect. Determines whether the analytical perspective''s mountain classification passes engine validation or triggers false_summit_mountain signature.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_state_necessity, conceptual, 'Whether linguistic unification is immutable state necessity or contingent political choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_capacity_expansion, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sce_tr_t0, state_capacity_expansion, theater_ratio, 0, 0.35).
narrative_ontology:measurement(sce_tr_t2, state_capacity_expansion, theater_ratio, 2, 0.48).
narrative_ontology:measurement(sce_tr_t5, state_capacity_expansion, theater_ratio, 5, 0.58).
narrative_ontology:measurement(sce_tr_t10, state_capacity_expansion, theater_ratio, 10, 0.6).
narrative_ontology:measurement(sce_tr_t20, state_capacity_expansion, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(sce_be_t0, state_capacity_expansion, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(sce_be_t2, state_capacity_expansion, base_extractiveness, 2, 0.52).
narrative_ontology:measurement(sce_be_t5, state_capacity_expansion, base_extractiveness, 5, 0.68).
narrative_ontology:measurement(sce_be_t10, state_capacity_expansion, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(sce_be_t20, state_capacity_expansion, base_extractiveness, 20, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(sce_su_t0, state_capacity_expansion, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(sce_su_t1, state_capacity_expansion, suppression_requirement, 1, 0.72).
narrative_ontology:measurement(sce_su_t3, state_capacity_expansion, suppression_requirement, 3, 0.75).
narrative_ontology:measurement(sce_su_t7, state_capacity_expansion, suppression_requirement, 7, 0.68).
narrative_ontology:measurement(sce_su_t15, state_capacity_expansion, suppression_requirement, 15, 0.45).
narrative_ontology:measurement(sce_su_t30, state_capacity_expansion, suppression_requirement, 30, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_capacity_expansion, information_standard).
narrative_ontology:boltzmann_floor_override(state_capacity_expansion, 0.05).
narrative_ontology:affects_constraint(state_capacity_expansion, ottoman_institutional_continuity).
narrative_ontology:affects_constraint(state_capacity_expansion, islamic_religious_authority_suppression).
narrative_ontology:affects_constraint(state_capacity_expansion, national_standardization_pedagogy).

% DUAL FORMULATION NOTE:
% The alphabet reform is upstream of multiple institutional constraints: it enables national standardization pedagogy (a downstream coordination constraint that uses Latin script as the kernel), it conflicts with Ottoman institutional continuity (a degraded institution competing for legitimacy), and it suppresses Islamic religious authority (a victim constraint with its own extraction structure). The alphabet reform story decomposes the language-policy domain into (1) the reform-as-coordination-plus-extraction (this story) and (2) the reform-as-kernel-reading-installation (CS structure analysis). These are not separate constraints but separate analytical frames on the same constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
