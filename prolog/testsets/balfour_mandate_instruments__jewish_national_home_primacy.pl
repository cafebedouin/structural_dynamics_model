% ============================================================================
% CONSTRAINT STORY: balfour_mandate_instruments__jewish_national_home_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_balfour_mandate_instruments__jewish_national_home_primacy, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
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
 *   constraint_id: balfour_mandate_instruments__jewish_national_home_primacy
 *   human_readable: Balfour Mandate Instruments: Jewish National Home Primacy Reading
 *   domain: international_law/colonial_administration/state_formation
 *
 * SUMMARY:
 *   The Balfour Declaration (1917) and subsequent League of Nations Mandate
 *   (1920) established a framework for British administration of Palestine
 *   with an explicit commitment to facilitate the establishment of a 'Jewish
 *   national home.' This constraint story models ONE reading of that
 *   contested kernel: the interpretation that 'national home' entails
 *   demographic transformation through facilitated immigration, land purchase
 *   from Arab owners by Jewish purchasers, and institutional supremacy of
 *   Jewish governance structures over Arab political representation. Under
 *   this reading, the Mandate instruments (especially Article 4, granting
 *   quasi-governmental status to the Jewish Agency) are interpreted as
 *   affirmatively requiring systematic facilitation of Jewish settlement and
 *   land acquisition. The extractiveness (0.62) reflects the substantial
 *   asymmetry: beneficiaries (Zionist institutions, Jewish migrants) gain
 *   coordinated access to land, immigration, and institutional autonomy;
 *   victims (Palestinian Arab landholders, peasantry, and political
 *   leadership) face dispossession through legal mechanisms, demographic
 *   subordination, and systematic exclusion from political structures
 *   nominally governing the territory. The rising extractiveness over the
 *   28-year interval (1920-1948) captures the acceleration of land alienation
 *   and demographic transformation as Jewish population expanded from ~80,000
 *   (1920) to ~650,000 (1948) and Arab-held land contracted from ~95% to ~45%
 *   through sales to Jewish purchasers and Mandate-facilitated acquisition.
 *   This reading coexists with two sibling readings:
 *   dual_obligation_indigenous_rights (emphasizing the Mandate's obligation
 *   to Palestinian Arabs and arguing for dual sovereignty) and
 *   mandatory_interpretive_discretion (arguing that the Mandate's language
 *   permits but does not require the demographic policies implemented). The
 *   kernel (the Balfour Declaration and Mandate text itself) is fixed, but
 *   interpretations diverge sharply on whether the text's language logically
 *   entails, merely permits, or affirmatively constrains the demographic and
 *   institutional asymmetry that emerged.
 *
 * KEY AGENTS:
 *   - Zionist institutions (Jewish Agency, Zionist Organization, cooperative land-purchase associations): Primary beneficiary (institutional/arbitrage) — gains quasi-governmental status, control over immigration and settlement, facilitated access to land purchases
 *   - Jewish migrants and settlers: Primary beneficiary (powerful/mobile to arbitrage over time) — gain access to land, employment, political voice, and community institution-building
 *   - Palestinian Arab landholders and village notables: Primary victim (powerful initially/constrained) — face land alienation through sales pressured by economic factors and legal mechanisms; political influence systematically diminished
 *   - Palestinian Arab peasantry: Primary victim (powerless/trapped) — face displacement through land sales by landlords, economic pressure, and lack of political voice in Mandate institutions
 *   - Palestinian Arab political leadership (Supreme Muslim Council, Arab Higher Committee): Secondary victim (organized/constrained) — excluded from Jewish Agency-equivalent institutional status; limited to consultative roles in Mandate administration
 *   - British High Commissioner: Institutional extractor (institutional/arbitrage) — maintains colonial administrative rent and strategic control; enforces suppression through security apparatus and immigration quotas (nominally)
 *   - League of Nations Permanent Mandates Commission: Analytical observer at remove (analytical/analytical at civilizational scale) — reviews Mandatory reports but lacks enforcement capacity; debates whether Mandate implementations comply with preamble language
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balfour_mandate_instruments__jewish_national_home_primacy, 0.62).
domain_priors:suppression_score(balfour_mandate_instruments__jewish_national_home_primacy, 0.68).
domain_priors:theater_ratio(balfour_mandate_instruments__jewish_national_home_primacy, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, extractiveness, 0.62).
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__jewish_national_home_primacy, tangled_rope).
narrative_ontology:human_readable(balfour_mandate_instruments__jewish_national_home_primacy, "Balfour Mandate Instruments: Jewish National Home Primacy Reading").
narrative_ontology:topic_domain(balfour_mandate_instruments__jewish_national_home_primacy, "international_law/colonial_administration/state_formation").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__jewish_national_home_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__jewish_national_home_primacy, '8337b278-12c3-4040-8e52-3243fb56f13d').
narrative_ontology:cs_kernel_codification('8337b278-12c3-4040-8e52-3243fb56f13d', formalized).
narrative_ontology:cs_authority_grounding('8337b278-12c3-4040-8e52-3243fb56f13d', lineage).
narrative_ontology:cs_interpretation_layer_present('8337b278-12c3-4040-8e52-3243fb56f13d').
narrative_ontology:cs_reading_relation('8337b278-12c3-4040-8e52-3243fb56f13d', balfour_mandate_dual_obligation_indigenous_rights, coexists_with).
narrative_ontology:cs_reading_relation('8337b278-12c3-4040-8e52-3243fb56f13d', balfour_mandate_mandatory_interpretive_discretion, influences).
narrative_ontology:cs_axiom('8337b278-12c3-4040-8e52-3243fb56f13d', foundational, national_home_entails_demographic_transformation).
narrative_ontology:cs_axiom_status(national_home_entails_demographic_transformation, holdable).
narrative_ontology:cs_axiom_grounding('8337b278-12c3-4040-8e52-3243fb56f13d', national_home_entails_demographic_transformation, deontological).
narrative_ontology:cs_axiom('8337b278-12c3-4040-8e52-3243fb56f13d', foundational, article_4_agency_affirmative_obligation).
narrative_ontology:cs_axiom_status(article_4_agency_affirmative_obligation, holdable).
narrative_ontology:cs_axiom_grounding('8337b278-12c3-4040-8e52-3243fb56f13d', article_4_agency_affirmative_obligation, empirically_contingent).
narrative_ontology:cs_reference_frame('8337b278-12c3-4040-8e52-3243fb56f13d', jewish_agency_institutional_primacy_framework).
narrative_ontology:cs_drift_state('8337b278-12c3-4040-8e52-3243fb56f13d', contemporary_post_mandate_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8337b278-12c3-4040-8e52-3243fb56f13d', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(balfour_mandate_instruments__jewish_national_home_primacy, balfour_mandate_instruments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__jewish_national_home_primacy, zionist_institutions).
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__jewish_national_home_primacy, jewish_migrants).
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__jewish_national_home_primacy, jewish_land_purchasers).
narrative_ontology:constraint_victim(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_landholders).
narrative_ontology:constraint_victim(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_political_leadership).
narrative_ontology:constraint_victim(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_peasantry).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PALESTINIAN ARAB PEASANTRY (SNARE) — Trapped within the territorial and administrative framework established by the Mandate. Land alienation mechanisms (sales facilitated by Mandate authorities to Jewish purchasers at asymmetric terms) operate with full suppression: exit requires leaving ancestral territory. Peasant cultivators face dispossession through legal mechanisms enforced by Mandate administration. No political representation in the institutions that govern land policy. Maximum experienced extraction — no exit, no voice, no alternatives.
constraint_indexing:constraint_classification(balfour_mandate_instruments__jewish_national_home_primacy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: PALESTINIAN ARAB POLITICAL LEADERSHIP (TANGLED ROPE) — Constrained by Mandate administrative structure but retains some capacity to organize petitions, protests, and communal institutions. The Mandate permits limited Arab municipal bodies and cultural organizations, creating a genuine coordination mechanism for Arab community self-governance alongside the asymmetric extraction (demographic and political subordination to Jewish institutional primacy). Extraction is substantial but not total — leadership has agency to organize and resist, though success is structurally constrained.
constraint_indexing:constraint_classification(balfour_mandate_instruments__jewish_national_home_primacy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ZIONIST INSTITUTIONS / JEWISH AGENCY (ROPE) — The Mandate's Article 4 grants the Jewish Agency quasi-governmental status, enabling land purchase, immigration facilitation, and institutional autonomy. The constraint functions as coordination: the Jewish Agency coordinates arrival, settlement, institution-building, and political organization. The beneficiary experiences the Mandate framework as enabling coordination rather than as extraction. This perspective sees the mechanism as solving the collective action problem of establishing a Jewish presence in Palestine — low experienced extraction because the institutional beneficiary has been granted agency within the system.
constraint_indexing:constraint_classification(balfour_mandate_instruments__jewish_national_home_primacy, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: LEAGUE OF NATIONS MANDATORY ADMINISTRATION (PITON) — The Mandate framework itself becomes performative theater over time. The League of Nations claims to administer the territory in trust for 'the peoples inhabiting [it]' (Mandate preamble), yet the primary operational effect is enabling asymmetric institutional development favoring Jewish institutions. The administrative apparatus of monitoring, reporting, and nominal neutrality persists despite the structural non-neutrality. By the 1930s, Mandate administration is substantially theatrical — maintaining the form of trusteeship while the substance is implementation of Jewish national home primacy. Theater_ratio is moderate (0.55) because the Mandate retains some genuine coordination functions (dispute resolution, infrastructure, basic administration) alongside the performative claims of balance.
constraint_indexing:constraint_classification(balfour_mandate_instruments__jewish_national_home_primacy, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: BRITISH COLONIAL ADMINISTRATION (TANGLED ROPE) — Britain exercises actual territorial authority while claiming mandatory neutrality. The High Commissioner enforces security, controls immigration quotas (nominally), and manages land sales regulation. Britain benefits from the colonial administrative rent and strategic position (Suez, regional influence). Simultaneously, Britain faces genuine coordination problems: maintaining order between communities with irreconcilable territorial claims, managing Jewish immigration pressure against Arab resistance, balancing Zionist institutional demands against League of Nations mandate language. This creates a hybrid: actual extraction (colonial rent, strategic control) combined with genuine but constrained coordination function.
constraint_indexing:constraint_classification(balfour_mandate_instruments__jewish_national_home_primacy, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational perspective, demographic transformation through state-sponsored settlement is a structural feature of colonial expansion: the mechanism (land law, immigration policy, institutional privileging) appears as an inevitable expression of the colonial logic itself — population replacement through legal-administrative means. This perspective risks naturalizing what is actually a specific reading of the Mandate's institutional structure. The engine will identify this as a false summit: the 'inevitability' of demographic transformation is not a law of nature but a particular interpretation of the Mandate's terms coupled with specific policy choices.
constraint_indexing:constraint_classification(balfour_mandate_instruments__jewish_national_home_primacy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(balfour_mandate_instruments__jewish_national_home_primacy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(balfour_mandate_instruments__jewish_national_home_primacy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(balfour_mandate_instruments__jewish_national_home_primacy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(balfour_mandate_instruments__jewish_national_home_primacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(balfour_mandate_instruments__jewish_national_home_primacy, TR),
    TR >= 0.70.

:- end_tests(balfour_mandate_instruments__jewish_national_home_primacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): This reading interprets the Mandate as requiring affirmative facilitation of Jewish settlement and land purchase. The extraction coefficient reflects substantive asymmetry: beneficiaries gain access to coordinated institutional capacity (Jewish Agency), preferential immigration quotas, and facilitated land acquisition; victims face dispossession and political subordination. The coefficient is not at snare level (0.72+) because the Mandate provides genuine coordination functions (dispute resolution, basic administration) that serve both communities, and some Arab institutional capacity exists (local councils, communal organizations). The rising trajectory (0.35 → 0.68 over 28 years) reflects accelerating land alienation and demographic transformation as the Jewish Agency's institutional capacity matured and consolidated. Suppression (0.68): The mechanisms of suppression escalate over the interval. Initial suppression (0.42) reflected limited enforcement capacity and incomplete Mandate institutional apparatus. Mid-interval suppression (0.55) reflects maturation of British security forces, immigration quota enforcement, and legal land-purchase frameworks. Late-interval suppression (0.68) reflects intensified security measures, restrictions on Arab political organizing, and systematic enforcement of land-alienation mechanisms. By 1947-1948, armed suppression becomes explicit. Theater ratio (0.55): Moderate-low. The Mandate framework performs neutrality (the preamble claims trusteeship for 'the peoples'; nominally equal treatment in League reports), yet the operational substance is affirmative facilitation of Jewish institutional development. Theater is not high because the mechanisms are relatively transparent and explicitly declared (the Jewish Agency is openly granted quasi-governmental status; immigration quotas are publicly debated). Theater increases slightly over time (0.48 → 0.57) as the performative gap between preamble language and operational asymmetry widens. Claimed type (tangled_rope): Reflects genuine coordination functions (Jewish Agency coordinating settlement, basic territorial administration functioning) combined with substantive extraction (land alienation, political subordination, demographic transformation asymmetry). The constraint does not classify as pure snare because the beneficiary's coordination is real and operationally effective; does not classify as rope because the victim's suppression and displacement are structural, not peripheral.
 *
 * PERSPECTIVAL GAP:
 *   Massive perspectival divergence across contexts. The Zionist institutions see rope (coordination enabling settlement and institution-building). The Palestinian peasantry sees snare (trapped, dispossessed, unrepresented). The British administration sees tangled rope (balancing colonial rent extraction with genuine order-maintenance coordination). The League of Nations observer sees an approaching false summit — the mountain perspective that views demographic transformation as an inevitable expression of colonial logic. The Palestinian political leadership sees tangled rope (constrained by Mandate structure but retaining organizational capacity). The analytical observer risks mountain by naturalizing contingent policy choices as inherent legal requirements. This perspectival structure is the diagnostic signature of a contested kernel where readings have not been disambiguated: the same base structure appears as coordination (rope), extraction (snare), mixed (tangled rope), and naturalized inevitability (false summit) depending on who is measuring and from what structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values derive from the structural position of each agent relative to the extraction flow. Beneficiaries (Jewish Agency, migrants) occupy the extraction sink — land transfers flow toward them, immigration quotas favor them, institutional autonomy flows to their structures. They experience low or negative d (0.05-0.20), producing low or negative chi via f(d). Institutional beneficiaries with arbitrage options (British High Commissioner) occupy a middle position — they extract administrative rent but also face constraints from competing claims; d~0.30-0.40, moderate chi. Victims with political capacity (Palestinian Arab leadership, organized at regional scale) experience constrained exit and moderate power; d~0.55-0.65, moderate-high chi. Victims without exit capacity (peasantry, landholders under economic pressure) experience trapped/identity-locked exit; d~0.85-0.95, maximum experienced extraction. The analytical observer at civilizational scale risks d~0.70 (sees structural asymmetry globally) but the risk is that this context naturalizes the asymmetry as inevitable rather than recognizing it as a specific interpretation of contestable Mandate language.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING CONSTRAINT: This story resolves mandatrophy by clarifying that the constraint is not 'what type is correct?' but 'which reading of the Mandate kernel are we examining?' The jewish_national_home_primacy reading interprets the Mandate as requiring/entailing demographic transformation and institutional supremacy. The dual_obligation_indigenous_rights reading interprets the same kernel text as requiring equal political rights and protection of Palestinian Arab interests. The mandatory_interpretive_discretion reading interprets the Mandate as permitting multiple policies within its framework. None of these readings is objectively 'correct' — they are different interpretations of an ambiguous, contested kernel. The mandate framework itself (the Balfour text, the League language) is the fixed kernel; the readings are the interpretations laid atop it. Mandatrophy resolution requires recognizing that the classification (tangled_rope, snare, rope) depends on which reading you adopt, not on discovering an external ground truth. The false summit (mountain perspective) emerges when observers naturalize this reading-dependent structure as an inevitable law of colonial settlement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandate_preamble_interpretation,
    'Does the Mandate preamble''s language about ''national home'' logically entail demographic transformation and institutional supremacy, or does it permit dual obligation to indigenous political rights?',
    'Textual analysis of Mandate preamble (Article 1-4) and contemporaneous League of Nations authoritative interpretation; examination of how different mandate holders (France, Belgium, Italy) interpreted similar ''sacred trust'' language in non-Zionist contexts; cross-comparison with dual-obligation reading of the same text',
    'If ''national home'' language entails subordination of indigenous political structure: this reading (jewish_national_home_primacy) and the dual_obligation_indigenous_rights reading logically foreclose each other within a single interpretive framework. If ''national home'' permits multiple interpretations: readings coexist as competing readings of ambiguous kernel, not as foreclosing alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_preamble_interpretation, conceptual, 'Whether ''national home'' language logically entails demographic/institutional supremacy or permits dual obligations').

omega_variable(
    article_4_agency_scope,
    'Does granting the Jewish Agency quasi-governmental status (Article 4) require affirmative facilitation of land purchase and immigration, or merely permit it without restricting alternative policies?',
    'Textual analysis of Article 4 language (''The Administering Authority... shall facilitate... the establishment of the national home''); historical record of policy choices by High Commissioners (some imposed strict immigration quotas, others facilitated higher immigration); comparison with how Article 4 was applied in practice across different administrations (1920-1948)',
    'If Article 4 mandates facilitation: this reading''s extraction mechanism (0.62) is structurally required by the Mandate itself — jewish_national_home_primacy is the canonical interpretation. If Article 4 merely permits: high-extraction interpretation is one possible policy choice, not a logical requirement of the Mandate language.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_4_agency_scope, empirical, 'Whether Article 4 mandates or merely permits facilitation of Jewish settlement').

omega_variable(
    land_sales_mechanism_contingency,
    'Were the asymmetric land sales mechanisms (Jewish purchasers acquiring Arab-held land at terms that accelerated Arab displacement) a necessary consequence of the Mandate''s institutional structure, or were they contingent policy choices by Mandate administrators?',
    'Historical analysis of land purchase patterns; examination of Mandatory land laws and regulations; comparison with alternative land policies that could have been implemented under the same Mandate framework (e.g., communal land tenure, restricted alienation, graduated purchase terms); evidence of debates within League of Nations about whether Article 4 required or merely permitted these mechanisms',
    'If necessary: this reading''s suppression mechanism (0.68) and beneficiary/victim structure follow directly from the Mandate framework itself. If contingent: the high extraction represents one possible interpretation, and different policies (supporting dual land tenure, restricting alienation) could have been pursued under the same Mandate',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(land_sales_mechanism_contingency, empirical, 'Whether land alienation mechanisms were Mandate-required or contingent policy choice').

omega_variable(
    mandatory_interpretive_discretion_scope,
    'What interpretive discretion did the Mandate framework itself grant to its administrators, and were there feasible alternative policy regimes that would have satisfied the Mandate''s literal language while achieving different demographic/institutional outcomes?',
    'Textual analysis of Mandate discretionary language; examination of policy debates within Mandatory administration and League of Nations; comparative analysis with Mandate implementations in Iraq, Transjordan, Syria/Lebanon; identification of specific policy choices (quota levels, land purchase restrictions, Arab representation ratios) that could have been different within the same Mandate framework',
    'If discretion was high: mandatory_interpretive_discretion reading (sibling) is empirically substantiated — the jewish_national_home_primacy reading is one interpretation among feasible alternatives. If discretion was low: this reading is more tightly constrained by the Mandate itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatory_interpretive_discretion_scope, conceptual, 'Scope of interpretive discretion available to Mandate administrators').

omega_variable(
    false_summit_mandate_naturalization,
    'Does this reading risk naturalizing contingent institutional arrangements (demographic transformation, institutional asymmetry) as inherent legal requirements, thereby masking the actual policy choices made within the Mandate framework?',
    'Comparison of this reading with mandatory_interpretive_discretion reading; examination of League of Nations language about the Mandate''s ''sacred trust'' and whether that language entails the specific demographic/institutional outcomes or merely the institutional framework within which outcomes emerge; post-hoc analysis of whether alternative policy regimes under the same Mandate language would have produced different demographic structures',
    'If this reading naturalizes contingent choices: the mountain perspective (analytical observer) is a false summit — the ''inevitability'' of demographic transformation is not a law of the Mandate but a specific interpretation. If the Mandate language tightly constrains outcomes: the reading accurately captures structural requirements rather than naturalizing contingencies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_mandate_naturalization, conceptual, 'Whether reading naturalizes contingent policy choices as Mandate imperatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__jewish_national_home_primacy, 0, 28).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balfour_primacy_theater_t0, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 0, 0.48).
narrative_ontology:measurement(balfour_primacy_theater_t8, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 8, 0.52).
narrative_ontology:measurement(balfour_primacy_theater_t15, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 15, 0.55).
narrative_ontology:measurement(balfour_primacy_theater_t28, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 28, 0.57).

% Extraction over time
narrative_ontology:measurement(balfour_primacy_extract_t0, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(balfour_primacy_extract_t8, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(balfour_primacy_extract_t15, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(balfour_primacy_extract_t28, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 28, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(balfour_primacy_suppress_t0, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(balfour_primacy_suppress_t8, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(balfour_primacy_suppress_t15, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 15, 0.64).
narrative_ontology:measurement(balfour_primacy_suppress_t28, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 28, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__jewish_national_home_primacy, resource_allocation).
narrative_ontology:affects_constraint(balfour_mandate_instruments__jewish_national_home_primacy, balfour_mandate_dual_obligation_indigenous_rights).
narrative_ontology:affects_constraint(balfour_mandate_instruments__jewish_national_home_primacy, balfour_mandate_mandatory_interpretive_discretion).
narrative_ontology:affects_constraint(balfour_mandate_instruments__jewish_national_home_primacy, palestine_partition_un_resolution_181).
narrative_ontology:affects_constraint(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_refugee_classification_1948).

% DUAL FORMULATION NOTE:
% The Balfour Mandate instruments kernel decomposes into three empirically distinct readings with different extractiveness values and beneficiary/victim structures: jewish_national_home_primacy (ε=0.62, tangled_rope), dual_obligation_indigenous_rights (ε=0.42, tangled_rope with lower asymmetry), mandatory_interpretive_discretion (ε=0.35-0.55 depending on policy regime, rope-to-tangled_rope range). Each reading models a coherent interpretation of the same Mandate text. The family is linked because downstream constraints (UN Partition, refugee classification) inherit the ambiguity about which reading of the Mandate was authoritative. This reading (jewish_national_home_primacy) is the interpretation that drove the historical Mandatory policies and is empirically upstream of the demographic transformation by 1948.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(balfour_mandate_instruments__jewish_national_home_primacy, institutional, 0.35).
constraint_indexing:directionality_override(balfour_mandate_instruments__jewish_national_home_primacy, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
