% ============================================================================
% CONSTRAINT STORY: orthographic_kernel_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_kernel_flat_control, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: orthographic_kernel_flat_control
 *   human_readable: Orthographic Standard for Written Turkish
 *   domain: political_linguistics/state_formation/commitment_systems
 *
 * SUMMARY:
 *   The 1928 Turkish script reform replaced Ottoman Turkish (Arabic script
 *   with Persian and Turkish modifications) with a Latin-based alphabet as
 *   part of Mustafa Kemal Atatürk's modernization program. The reform was
 *   implemented rapidly — the new script became mandatory for all official
 *   documents, education, and publishing within months. This orthographic
 *   standard now grounds all legitimate literacy in Turkey: state
 *   documentation, legal proceedings, educational credentials, and cultural
 *   transmission all operate through the Latin script. The constraint
 *   exhibits tangled rope structure: it solves genuine coordination problems
 *   (mass literacy rose from ~10% in 1928 to >95% by 1980s, standardized
 *   education became scalable, legal documentation achieved uniformity) while
 *   simultaneously extracting substantial costs (rendered existing literacy
 *   obsolete, severed access to Ottoman textual heritage for subsequent
 *   generations, suppressed regional phonological variation). The 1928
 *   generation experienced maximum extraction — adults literate in Ottoman
 *   script faced functional illiteracy overnight with no exit option.
 *   Contemporary extraction is lower but persistent: access to pre-1928 texts
 *   requires specialized training, regional dialects face standardization
 *   pressure, and the Turkish Language Association maintains prescriptive
 *   authority that has partly atrophied into theatrical rule-making. The
 *   measurements show declining extractiveness and suppression over the
 *   98-year interval as the reform's initial shock dissipated and the Latin
 *   script became naturalized, while theater_ratio increased modestly as the
 *   TDK's prescriptive function shifted from coordination to institutional
 *   legitimacy maintenance.
 *
 * KEY AGENTS:
 *   - State Administrative Apparatus: Primary beneficiary (institutional/arbitrage) — captures coordination gains from standardized documentation and education without bearing transition costs
 *   - Turkish Language Association (TDK): Institutional beneficiary (institutional/constrained) — maintains prescriptive authority; contemporary function partly theatrical
 *   - Standardized Education System: Beneficiary (institutional/mobile) — Latin script enabled mass literacy infrastructure and curriculum standardization
 *   - Publishing Industry: Beneficiary (powerful/mobile) — standardized orthography reduced production costs and enabled national market
 *   - Arabic-Script Literate Population (1928): Primary victim (powerless/trapped) — lifetime literacy investment destroyed by reform with no exit option
 *   - Regional Dialect Speakers: Secondary victim (moderate/constrained) — face suppression of phonological variation through standardization pressure
 *   - Ottoman Textual Heritage Access: Victim (powerless/trapped) — subsequent generations severed from pre-1928 texts without specialized training
 *   - Reformist Linguistic Coalition (1920s-1930s): Organized agents (organized/mobile) — implemented reform as transitional infrastructure for broader modernization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_kernel_flat_control, 0.32).
domain_priors:suppression_score(orthographic_kernel_flat_control, 0.48).
domain_priors:theater_ratio(orthographic_kernel_flat_control, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_kernel_flat_control, extractiveness, 0.32).
narrative_ontology:constraint_metric(orthographic_kernel_flat_control, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(orthographic_kernel_flat_control, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_kernel_flat_control, tangled_rope).
narrative_ontology:human_readable(orthographic_kernel_flat_control, "Orthographic Standard for Written Turkish").
narrative_ontology:topic_domain(orthographic_kernel_flat_control, "political_linguistics/state_formation/commitment_systems").

domain_priors:requires_active_enforcement(orthographic_kernel_flat_control).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_kernel_flat_control, 'e70752c6-4c70-4ea8-b3a9-d5d484bf4d5b').
narrative_ontology:cs_kernel_codification('e70752c6-4c70-4ea8-b3a9-d5d484bf4d5b', formalized).
narrative_ontology:cs_authority_grounding('e70752c6-4c70-4ea8-b3a9-d5d484bf4d5b', lineage).
narrative_ontology:cs_interpretation_layer_present('e70752c6-4c70-4ea8-b3a9-d5d484bf4d5b').
narrative_ontology:cs_created_at('e70752c6-4c70-4ea8-b3a9-d5d484bf4d5b', '').

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(orthographic_kernel_flat_control, orthographic_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_kernel_flat_control, state_administrative_apparatus).
narrative_ontology:constraint_beneficiary(orthographic_kernel_flat_control, turkish_language_association).
narrative_ontology:constraint_beneficiary(orthographic_kernel_flat_control, standardized_education_system).
narrative_ontology:constraint_beneficiary(orthographic_kernel_flat_control, publishing_industry).
narrative_ontology:constraint_victim(orthographic_kernel_flat_control, arabic_script_literate_population).
narrative_ontology:constraint_victim(orthographic_kernel_flat_control, regional_dialect_speakers).
narrative_ontology:constraint_victim(orthographic_kernel_flat_control, ottoman_textual_heritage_access).
narrative_ontology:constraint_vindicates(orthographic_kernel_flat_control, linguistic_modernization_doctrine).
narrative_ontology:constraint_vindicates(orthographic_kernel_flat_control, national_unity_through_standardization).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The orthographic standard solves the coordination problem of enabling mass literacy, standardized education, uniform legal documentation, and scalable publishing infrastructure. Without a fixed script system, these functions would face transaction costs from orthographic variation and incompatible literacy training.
% TRANSFER_FUNCTION: The standard transfers coordination gains (literacy access, educational credentials, state employment eligibility, publishing market access) to those who adopt the Latin script, and imposes costs (literacy obsolescence, heritage access loss, dialect suppression) on those whose prior literacy or linguistic variation is incompatible with the standard. The 1928 reform transferred literacy capital from the Arabic-script literate generation to the Latin-script literate generation.
% ABSENT_VOICES: The Arabic-script literate population in 1928 had no institutional representation in the reform process — the decision was made by a modernizing state elite without consultation of those whose literacy would be destroyed. Regional dialect speakers have limited representation in orthographic standardization — the Turkish Language Association is dominated by Istanbul Turkish speakers. Ottoman heritage scholars and historians are a small professional minority with limited influence on orthographic policy. These absent voices would object to the heritage severance and dialect suppression but were not in the room when the standard was established or when the TDK's prescriptive authority was institutionalized.
% DISAPPEARANCE_RATIONALE: If the orthographic standard disappeared overnight, the world would rearrange substantially: state documentation would face coordination failure across incompatible scripts, educational credentials would lose standardization, legal proceedings would require translation infrastructure, publishing would fragment into script-specific markets, and mass literacy would face transaction costs from orthographic variation. The coordination function is genuine and structural — arrangements depend on the standard's existence. However, the specific choice of Latin script (versus an alternative standardized script) is more contingent — a different script choice would rearrange the beneficiary/victim structure but would not eliminate the coordination function.
% FOUNDING_PROBLEM: The founding problem was the low literacy rate in the late Ottoman period (~10% in 1920s) combined with the perceived incompatibility between Arabic script and Turkish phonology, and the political goal of Westernization and break from Ottoman/Islamic heritage. The reformers framed the problem as: Arabic script is inadequate for Turkish sounds, prevents mass literacy, and symbolically ties Turkey to a backward past. The Latin script was presented as both technically superior (better phonological fit) and politically necessary (signal of modernization and European orientation).
% FOUNDING_PROBLEM_CORROBORATION: The technical inadequacy claim (Arabic script cannot represent Turkish phonology) is contested by linguists — multiple Turkic languages used Arabic script successfully, and proposed Ottoman script reforms in the 1910s-1920s addressed phonological issues without script change. The mass literacy goal is corroborated by literacy statistics (rose from ~10% in 1928 to >95% by 1980s), but whether this required script change versus orthographic reform within Arabic script is disputed. The political goal (Westernization, heritage break) is corroborated by the broader Kemalist reform program (secularization, legal code adoption from Europe, surname law, hat law) which explicitly aimed to sever Ottoman/Islamic continuity. Corroborating sources: Turkish education ministry literacy statistics (beneficiary source, but data is independently verifiable); comparative Turkic linguistics literature on Arabic script adaptations (neutral scholarly source); historical analysis of Kemalist reforms (mixed — some scholars emphasize modernization necessity, others emphasize political rupture as primary goal).
narrative_ontology:disappearance_verdict(orthographic_kernel_flat_control, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_kernel_flat_control, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ARABIC-SCRIPT LITERATE POPULATION (SNARE) — The 1928 script reform rendered an entire generation's literacy obsolete overnight. Adults literate in Ottoman Turkish faced functional illiteracy in the new Latin script with no exit option — state documentation, legal proceedings, and public communication all shifted to the new orthography. Maximum extraction: lifetime literacy investment destroyed by administrative fiat.
constraint_indexing:constraint_classification(orthographic_kernel_flat_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REGIONAL DIALECT SPEAKERS (TANGLED ROPE) — The standardized orthography coordinates national communication and enables participation in state institutions, but also suppresses regional phonological variation and imposes Istanbul Turkish as the legitimate form. Constrained by educational requirements and state employment gates, but benefits from access to standardized literacy infrastructure. Mixed coordination and extraction.
constraint_indexing:constraint_classification(orthographic_kernel_flat_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: STATE ADMINISTRATIVE APPARATUS (ROPE) — The orthographic standard solves genuine coordination problems: uniform documentation, legal clarity, educational scalability, and administrative efficiency. The state apparatus benefits from standardization without bearing its costs — it sets the standard and collects the coordination gains. Net beneficiary with arbitrage-level exit options.
constraint_indexing:constraint_classification(orthographic_kernel_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REFORMIST LINGUISTIC COALITION (SCAFFOLD) — The organized coalition that implemented the 1928 reform saw the Latin script as a temporary bridge to full linguistic modernization, with the expectation that further reforms (vocabulary purification, grammatical simplification) would follow. The orthographic shift was justified as transitional infrastructure for a broader transformation. The coalition had mobile exit options — intellectuals and state elites could navigate both scripts.
constraint_indexing:constraint_classification(orthographic_kernel_flat_control, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: TURKISH LANGUAGE ASSOCIATION (PITON) — The contemporary institutional guardian of orthographic standards maintains elaborate prescriptive rules (spelling conventions, loanword adaptation protocols, neologism approval) whose primary function has shifted from coordination to institutional legitimacy maintenance. Much of the regulatory apparatus is theatrical — actual written Turkish evolves through usage while the TDK issues pronouncements that are selectively followed. The institution persists through inertia and state backing rather than functional necessity.
constraint_indexing:constraint_classification(orthographic_kernel_flat_control, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — The orthographic standard exhibits both genuine coordination function (enables mass literacy, standardized education, legal documentation) and substantial extraction (destroyed existing literacy, suppressed regional variation, severed access to Ottoman textual heritage). The 1928 reform was not a natural evolution but a state-imposed discontinuity with identifiable beneficiaries and victims. The constraint requires active enforcement through education policy, state employment requirements, and publishing standards. Tangled rope classification reflects the irreducible hybrid structure.
constraint_indexing:constraint_classification(orthographic_kernel_flat_control, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_kernel_flat_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(orthographic_kernel_flat_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(orthographic_kernel_flat_control, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(orthographic_kernel_flat_control, TR),
    TR >= 0.70.

:- end_tests(orthographic_kernel_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32 contemporary, 0.68 at 1928): The initial extraction was severe — an entire generation's literacy was rendered obsolete, and access to Ottoman heritage was severed for all subsequent generations. Contemporary extraction is moderate: the coordination function is genuine (mass literacy, standardized education, legal clarity), but costs persist (heritage access requires specialized training, regional variation faces suppression, TDK prescriptive apparatus extracts institutional rents). The declining trajectory reflects that the reform's shock dissipated as the Latin script naturalized and the Arabic-literate generation died. Suppression (0.48 contemporary, 0.75 at 1928): High initial suppression through mandatory adoption in all state functions, education, and publishing. Contemporary suppression is moderate — the standard is enforced through educational requirements, state employment gates, and publishing norms, but digital tools and private communication allow some flexibility. The declining trajectory reflects normalization: what required active enforcement in 1928 is now largely internalized. Theater ratio (0.35 contemporary, 0.15 at 1928): Low initial theater — the 1928 reform was functional coordination with minimal performative content. Theater increased modestly as the TDK's prescriptive function shifted from solving real coordination problems (establishing the standard) to maintaining institutional legitimacy (issuing rulings on loanword spelling and neologisms that are selectively followed). Contemporary theater is moderate — much TDK activity is performative, but the core orthographic standard remains functional.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how the same orthographic standard appears as snare, tangled rope, rope, scaffold, or piton depending on the observer's structural position and time horizon. The 1928 Arabic-script literate generation experienced pure extraction (snare) — their literacy was destroyed with no exit and no benefit. Regional dialect speakers experience mixed coordination and extraction (tangled rope) — standardization enables participation but suppresses variation. The state apparatus experiences pure coordination (rope) — it sets the standard and captures the gains. The 1920s reformist coalition saw temporary infrastructure (scaffold) — the script change was justified as transitional. The contemporary TDK exhibits degraded function (piton) — prescriptive authority maintained theatrically. The analytical observer sees irreducible hybrid structure (tangled rope) — genuine coordination function coexists with substantial extraction, and neither can be eliminated without destroying the other. The perspectival gap is not a measurement error but the constraint's actual structure: it IS simultaneously coordination and extraction, depending on where you sit.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. The state administrative apparatus is a primary beneficiary with arbitrage exit — it experiences the constraint as pure coordination (low d, negative chi). The Arabic-script literate population (1928) was a primary victim with trapped exit — they experienced maximum extraction (high d, high chi). Regional dialect speakers are both victims (suppression of variation) and beneficiaries (access to standardized literacy) with constrained exit — they experience moderate extraction (mid-range d, moderate chi). The TDK is a beneficiary with constrained exit in the contemporary period — it collects institutional rents but is constrained by the standard's naturalization (low-to-moderate d). The reformist coalition had mobile exit options and saw themselves as coordination agents — low d. The analytical observer has analytical exit and sees the hybrid structure — moderate d reflecting the irreducible extraction component. The directionality computation captures that the same constraint extracts from some agents while coordinating for others, and the perspectival gap in classification follows from these different structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The orthographic standard does not exhibit mandatrophy in the classical sense — its coordination function remains live (mass literacy, legal documentation, educational infrastructure all depend on standardization). However, the TDK's prescriptive apparatus shows partial mandatrophy: the institution's original mandate (establishing and stabilizing the new orthography) has been fulfilled, but the institution persists by expanding into prescriptive rule-making (loanword spelling, neologism approval) whose function is partly theatrical. The piton classification for the TDK perspective captures this: the prescriptive layer is maintained through institutional inertia rather than functional necessity. The core orthographic standard itself remains functional — it is a tangled rope, not a piton, because the coordination and extraction are both structurally active. The mandatrophy is localized to the institutional guardian layer, not the constraint itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    script_choice_contingency,
    'Was the Latin script adoption in 1928 a response to genuine technical limitations of Arabic script for Turkish phonology, or primarily a political signal of Westernization and break from Ottoman/Islamic heritage?',
    'Comparative analysis of Arabic script adaptations for other Turkic languages (Uyghur, Kazakh pre-Soviet); assessment of proposed Ottoman script reforms (1910s-1920s) that were rejected in favor of Latin adoption; correlation between script reform and other secularization policies.',
    'If technical: coordination function dominates, extraction is side effect. If political: extraction (heritage severance, identity transformation) was primary goal, coordination was cover story. Changes weight of beneficiary vs victim costs in the tangled rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(script_choice_contingency, conceptual, 'Whether script choice was technically motivated or politically motivated').

omega_variable(
    heritage_access_threshold,
    'At what point does orthographic discontinuity constitute extraction from cultural heritage access versus legitimate modernization cost?',
    'Measurement of Ottoman Turkish literacy rates among contemporary Turkish citizens; availability and usage of transliteration tools; educational curriculum time allocated to Ottoman script instruction; accessibility of historical documents.',
    'If heritage access remains high (>30% can read Ottoman texts): discontinuity is coordination cost. If access collapsed (<5%): discontinuity is extractive severance. Determines whether ''victims'' category includes only the 1928 generation or extends to all subsequent generations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(heritage_access_threshold, empirical, 'Threshold for heritage access loss as extraction vs coordination cost').

omega_variable(
    dialect_suppression_mechanism,
    'Does the orthographic standard suppress regional dialects through inherent phonological bias (Istanbul Turkish phonology encoded in spelling), or through institutional enforcement (education policy, media standards) that could operate independently of the script choice?',
    'Comparison with other standardized orthographies (Spanish, Arabic, Chinese) and their dialect suppression patterns; analysis of whether Latin script inherently favors Istanbul phonology or whether suppression operates through institutional layer; assessment of pre-1928 Ottoman orthography''s relationship to regional variation.',
    'If inherent: script choice itself is extractive mechanism. If institutional: extraction operates through enforcement layer, not orthography. Changes assessment of whether alternative script choices would have reduced extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dialect_suppression_mechanism, empirical, 'Whether dialect suppression is inherent to script or institutional').

omega_variable(
    tdk_functional_atrophy,
    'Has the Turkish Language Association''s prescriptive authority genuinely atrophied (piton), or does it retain functional coordination power through educational and state employment gates?',
    'Compliance measurement: correlation between TDK rulings and actual written usage in media, academic publishing, state documents; tracking of neologism adoption rates; assessment of whether TDK rulings predict or follow usage changes.',
    'If atrophied: piton classification confirmed, theater_ratio should be higher. If functional: TDK retains coordination power, institutional perspective should be rope or tangled_rope rather than piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tdk_functional_atrophy, empirical, 'Whether TDK prescriptive authority is functional or theatrical').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_kernel_flat_control, 0, 98).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ortho_flat_theater_1928, orthographic_kernel_flat_control, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ortho_flat_theater_1948, orthographic_kernel_flat_control, theater_ratio, 20, 0.22).
narrative_ontology:measurement(ortho_flat_theater_1968, orthographic_kernel_flat_control, theater_ratio, 40, 0.28).
narrative_ontology:measurement(ortho_flat_theater_1988, orthographic_kernel_flat_control, theater_ratio, 60, 0.32).
narrative_ontology:measurement(ortho_flat_theater_2008, orthographic_kernel_flat_control, theater_ratio, 80, 0.35).
narrative_ontology:measurement(ortho_flat_theater_2026, orthographic_kernel_flat_control, theater_ratio, 98, 0.35).

% Extraction over time
narrative_ontology:measurement(ortho_flat_extract_1928, orthographic_kernel_flat_control, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(ortho_flat_extract_1948, orthographic_kernel_flat_control, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(ortho_flat_extract_1968, orthographic_kernel_flat_control, base_extractiveness, 40, 0.41).
narrative_ontology:measurement(ortho_flat_extract_1988, orthographic_kernel_flat_control, base_extractiveness, 60, 0.35).
narrative_ontology:measurement(ortho_flat_extract_2008, orthographic_kernel_flat_control, base_extractiveness, 80, 0.33).
narrative_ontology:measurement(ortho_flat_extract_2026, orthographic_kernel_flat_control, base_extractiveness, 98, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(ortho_flat_suppress_1928, orthographic_kernel_flat_control, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(ortho_flat_suppress_1948, orthographic_kernel_flat_control, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(ortho_flat_suppress_1968, orthographic_kernel_flat_control, suppression_requirement, 40, 0.58).
narrative_ontology:measurement(ortho_flat_suppress_1988, orthographic_kernel_flat_control, suppression_requirement, 60, 0.52).
narrative_ontology:measurement(ortho_flat_suppress_2008, orthographic_kernel_flat_control, suppression_requirement, 80, 0.48).
narrative_ontology:measurement(ortho_flat_suppress_2026, orthographic_kernel_flat_control, suppression_requirement, 98, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_kernel_flat_control, information_standard).

% DUAL FORMULATION NOTE:
% This is the flat construction of the Turkish orthographic standard as a single constraint. The substrate contains contestation (was the Latin script choice technically or politically motivated? does heritage severance constitute extraction or coordination cost?) but that contestation is routed through perspectival disagreement and omega variables rather than decomposition into separate readings. A reading-based decomposition would split this into distinct constraints for different framings of the script choice, each with its own epsilon and beneficiary structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
