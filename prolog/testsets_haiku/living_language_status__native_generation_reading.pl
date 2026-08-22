% ============================================================================
% CONSTRAINT STORY: living_language_status__native_generation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_living_language_status__native_generation_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: living_language_status__native_generation_reading
 *   human_readable: Living Language Status — Native Generation Reading
 *   domain: sociolinguistics/nationalism/religious studies
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested kernel 'living
 *   language status.' The reading under instantiation here is the NATIVE
 *   GENERATION READING: a language is living only if native speakers transmit
 *   it generationally as a mother tongue in daily life; liturgical recitation
 *   is preservation of a corpse, not vitality. This reading has institutional
 *   power in academic linguistics, state language planning, and international
 *   language preservation organizations. It benefits secular nationalist
 *   movements and state planners by legitimizing resource concentration on
 *   languages with demonstrable native-transmission cohorts. It extracts from
 *   communities whose linguistic transmission happens primarily through
 *   liturgical, literary, or diaspora channels by framing such transmission
 *   as non-vitality. The constraint is classified as a TANGLED ROPE because
 *   it combines genuine coordination (solving the resource-prioritization
 *   problem of which languages deserve urgent support) with asymmetric
 *   extraction (imposing a definitional frame that marginalizes communities
 *   whose linguistic practice does not fit the native-generation model). The
 *   reading is alive and institutionally powerful; sibling readings
 *   (liturgical preservation, literary continuity) coexist as competing
 *   definitions held by different communities.
 *
 * KEY AGENTS:
 *   - Secular nationalist linguists (institutional agenda-setter): Control academic definitions, funding allocation, conference prestige. Benefit from the native-generation criterion as it aligns language vitality with nation-state projects.
 *   - State language planners (institutional beneficiary): Use the criterion to justify resource concentration on native-transmission-focused languages aligned with state identity projects.
 *   - Liturgical-only communities (powerless victim, identity-locked): Transmit languages through religious practice and sacred texts; classified under this reading as preserving 'corpses,' not living languages. Identity fusion with religious tradition creates locked exit.
 *   - Diasporic heritage speakers (moderate victim with partial benefit): Transmit natively but in diaspora contexts; status ambiguous under the criterion's requirement of 'daily life' native transmission.
 *   - Literary/intellectual communities (organized but excluded): Produce new work in languages outside the native-transmission paradigm; excluded from the institutions that define vitality.
 *   - Descriptive linguists (institutional observer): Empirically document language transmission patterns and vitality measures; offer alternative frameworks.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__native_generation_reading, 0.58).
domain_priors:suppression_score(living_language_status__native_generation_reading, 0.67).
domain_priors:theater_ratio(living_language_status__native_generation_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, suppression_requirement, 0.67).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__native_generation_reading, tangled_rope).
narrative_ontology:human_readable(living_language_status__native_generation_reading, "Living Language Status — Native Generation Reading").
narrative_ontology:topic_domain(living_language_status__native_generation_reading, "sociolinguistics/nationalism/religious studies").

domain_priors:requires_active_enforcement(living_language_status__native_generation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__native_generation_reading, '951e7d90-4484-4cda-8e37-8f8cc8899b34').
narrative_ontology:cs_kernel_codification('951e7d90-4484-4cda-8e37-8f8cc8899b34', distributed).
narrative_ontology:cs_authority_grounding('951e7d90-4484-4cda-8e37-8f8cc8899b34', extraction).
narrative_ontology:cs_interpretation_layer_present('951e7d90-4484-4cda-8e37-8f8cc8899b34').
narrative_ontology:cs_reading_relation('951e7d90-4484-4cda-8e37-8f8cc8899b34', living_language_status__lithurgical_preservation_reading, coexists_with).
narrative_ontology:cs_reading_relation('951e7d90-4484-4cda-8e37-8f8cc8899b34', living_language_status__literary_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('951e7d90-4484-4cda-8e37-8f8cc8899b34', foundational, native_transmission_sole_vitality_marker).
narrative_ontology:cs_axiom_status(native_transmission_sole_vitality_marker, holdable).
narrative_ontology:cs_axiom_grounding('951e7d90-4484-4cda-8e37-8f8cc8899b34', native_transmission_sole_vitality_marker, empirically_contingent).
narrative_ontology:cs_axiom('951e7d90-4484-4cda-8e37-8f8cc8899b34', secondary, secular_demographic_criterion_privileged).
narrative_ontology:cs_axiom_status(secular_demographic_criterion_privileged, holdable).
narrative_ontology:cs_axiom_grounding('951e7d90-4484-4cda-8e37-8f8cc8899b34', secular_demographic_criterion_privileged, instrumental).
narrative_ontology:cs_reference_frame('951e7d90-4484-4cda-8e37-8f8cc8899b34', native_speaker_generational_vitality).
narrative_ontology:cs_drift_state('951e7d90-4484-4cda-8e37-8f8cc8899b34', contemporary_internet_diaspora_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('951e7d90-4484-4cda-8e37-8f8cc8899b34', '').
narrative_ontology:cs_kernel_id(living_language_status__native_generation_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__native_generation_reading, secular_nationalist_linguists).
narrative_ontology:constraint_beneficiary(living_language_status__native_generation_reading, state_language_planners).
narrative_ontology:constraint_victim(living_language_status__native_generation_reading, liturgical_only_communities).
narrative_ontology:constraint_victim(living_language_status__native_generation_reading, diasporic_heritage_speakers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(living_language_status__native_generation_reading, diasporic_heritage_speakers).
narrative_ontology:constraint_victim(living_language_status__native_generation_reading, lithurgical_only_communities).
narrative_ontology:constraint_vindicates(living_language_status__native_generation_reading, linguistic_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(living_language_status__native_generation_reading, native_speaker_vitality_criterion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define and enforce the criterion that a language is 'living' only through native generational transmission. Control academic publications, grant funding, conference proceedings, and institutional prestige ladders. Define which languages qualify as 'living' and which are relegated to 'lithurgical preservation' or 'cultural artifact.' Benefit from this definitional power by aligning language vitality with nation-state projects and secular modernization narratives. Their authority derives from institutional positions in universities, language academies, and ministries of culture.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, secular_nationalist_linguists, agenda_setter,
    institutional, generational, arbitrage, national).

% Use the native-generation criterion to justify state funding and infrastructure for languages aligned with national identity (often the dominant language of the state, or languages the state seeks to promote). Resources flow to schools, broadcast media, and public institutions where native transmission is demonstrable. The criterion legitimizes resource concentration on languages with active child-rearing cohorts and marginalizes languages whose communities lack state power to support native transmission.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, state_language_planners, beneficiary,
    institutional, generational, arbitrage, national).

% Transmit a language primarily through sacred texts, ritual recitation, and study with religious elders — the historical norm for many minority and diaspora communities. Under this reading's criterion, their transmission is classified as 'preservation of a corpse,' not vitality. They are structurally denied recognition as 'living speakers' by the definitional apparatus, which creates downstream consequences: ineligibility for language-preservation funding, absence from official language lists, exclusion from educational curricula, and stigmatization of their children's linguistic competence. Identity fusion with the religious tradition makes exit from the language unthinkable; identity fusion with the language itself makes accepting the 'corpse' label psychologically costly.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, lithurgical_only_communities, payer,
    powerless, biographical, identity_locked, local).

% Maintain a heritage language in diaspora through household transmission, community education, and cultural institutions — often partially through religious/cultural organizations. They transmit natively to their children but in a context where the dominant state language is the primary medium of economic and social life. The criterion's requirement of native generational transmission in a 'daily life' context (often implicitly assumed to mean a geographically concentrated native community) creates structural ambiguity about their status: do diaspora children count as 'native speakers' if they speak the heritage language at home but the dominant language at school and work? The reading often implicitly frames diaspora transmission as insufficiently robust — a benefit (recognition as native speakers) coupled with a cost (precarity and marginalization in the wider society).
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, diasporic_heritage_speakers, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(living_language_status__native_generation_reading, diasporic_heritage_speakers, beneficiary).

% Produce new literature, scholarship, journalism, and intellectual work in languages that may lack large native-speaker cohorts but remain productive media for thought and expression. They would argue that literary vitality and productivity are the relevant measure; they are excluded from the conversation that frames such work as secondary to native generational transmission. Their voice would contest the reading's definition but is systematically marginalized in the institutions that enforce it.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, literary_intellectual_communities, excluded,
    organized, generational, constrained, global).

% Study language vitality using empirical criteria: speaker populations, transmission patterns, morphosyntactic complexity, domains of use, corpus age distribution. They take a methodologically neutral stance and document the criterion this reading instantiates, but they also document alternative readings and their empirical consequences. They serve as the seat from which the constraint structure is transparently visible.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, academic_linguists_descriptive, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(living_language_status__native_generation_reading, secular_nationalist_linguists).
narrative_ontology:fixing_cost_class(living_language_status__native_generation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified, measurable criterion for identifying which languages require urgent preservation resources and state support, enabling governments and international organizations to allocate limited language-preservation funding to languages with demonstrable native-transmission cohorts rather than dispersing resources across historical claims. Solves the collective-action problem of prioritization: 'living language status' operationalizes what counts as a preservation target.
% TRANSFER_FUNCTION: Moves institutional prestige, funding, educational resources, media infrastructure, and official recognition from languages classified as 'lithurgical' or 'literary-only' to languages that satisfy the native-generation criterion. Transfers decisional power over language classification to institutional linguists and state planners who control the criterion's application. Transfers stigma to communities whose linguistic transmission does not fit the native-generational model.
% ABSENT_VOICES: Communities whose languages are transmitted through literary, lithurgical, or diaspora networks are structurally excluded from the institutions (universities, academies, ministries) that define and apply the criterion. Speakers of such languages would dispute the reading's definition but are kept outside the definitional apparatus. Also excluded: theoretical linguists who argue that language vitality should be measured by productivity, complexity, or corpus age rather than native-speaker demographics.
% DISAPPEARANCE_RATIONALE: If this criterion disappeared, institutions would reallocate resources from native-transmission-focused revitalization toward literary, diaspora, and lithurgical language communities; academic hierarchy would reorganize around alternative vitality measures; governments would lose a framework for privileging certain languages over others; and the stigmatization of non-generational transmission would lift. Languages currently classified as 'dead' or 'preserved corpora' would re-enter policy conversations as potential subjects of revitalization if the native-generation criterion were removed.
% FOUNDING_PROBLEM: In the mid-20th century, rapid language extinction in colonized regions and minority populations created urgency around language preservation. Linguists and nationalist movements needed a criterion to distinguish languages worth urgent intervention from historical relics. The native-generation criterion provided clarity: if children still speak the language at home, the community has the internal resource to transmit it; if transmission had already broken, external intervention faced structural barriers.
% FOUNDING_PROBLEM_CORROBORATION: State language planners and academic linguists in nationalist-aligned institutions attest the founding problem is still live and urgent — language extinction is real and accelerating. Descriptive linguists and scholars of diaspora communities attest the founding problem was real but the criterion has outlived its analytical utility — many languages with broken native transmission are intellectually productive or religiously vital, and the criterion now functions primarily as a mechanism to justify resource allocation rather than measuring actual linguistic vitality. Literary and lithurgical communities attest the criterion's application has become stigmatizing rather than preserving.
narrative_ontology:disappearance_verdict(living_language_status__native_generation_reading, world_rearranges).
narrative_ontology:founding_problem_status(living_language_status__native_generation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(living_language_status__native_generation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(living_language_status__native_generation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(living_language_status__native_generation_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(living_language_status__native_generation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(living_language_status__native_generation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(living_language_status__native_generation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.42 to 0.58 over the interval, with steepest growth in the first 15 years and plateau thereafter. This reflects the institutionalization of the native-generation criterion: initial adoption in academic circles (0–5 years), rapid incorporation into state language policy and funding mechanisms (5–15 years), and stabilization once the criterion becomes embedded in institutional infrastructure (15–35 years). Suppression requirement rises from 0.52 to 0.67 and plateaus similarly: maintaining the criterion requires ongoing institutional enforcement — defending funding models, enforcing classification decisions, managing challenges from alternative readings. Theater ratio rises from 0.28 to 0.41 and plateaus: the coordination function (prioritizing extinction-threat languages) is real but a growing share of institutional effort is devoted to defending the definitional boundary against challenged from literary and liturgical communities. Accessibility collapse is high (0.72) because once the native-generation criterion is institutionalized, alternative measures of language vitality become difficult to invoke in policy contexts — the criterion crowds out other frameworks. Resistance is moderate (0.58) because communities whose languages fall outside the criterion actively contest the reading, but they lack the institutional power to impose alternative definitions. The beneficiary and victim seats compute differently: from the institutional agenda-setter's seat, the arrangement solves a real coordination problem and is justified; from the liturgical community's seat, the same structure is experienced as definitional exclusion and resource denial.
 *
 * PERSPECTIVAL GAP:
 *   The secular nationalist linguist seat experiences this constraint as rational, evidence-based prioritization: children speaking a language natively are the best indicator that the language will persist without intervention. The liturgical community seat experiences the same structure as delegitimization: their transmission is declared non-vital precisely because it does not conform to a secular, demographic model of language use. The state planner seat experiences it as a tool for aligning language vitality with national identity projects. The descriptive linguist (observer seat) sees the structural asymmetry: the criterion's scientific framing masks a normative claim about what counts as legitimate language transmission. The engine computes per-seat classifications from these structural differences; the authored metrics reflect the constraint's operation from the reading's own standpoint (not a neutral standpoint, but the native-generation reading's standpoint).
 *
 * DIRECTIONALITY LOGIC:
 *   Secular nationalist linguists are beneficiaries (d near 0.2): they control the criterion, benefit from institutional prestige, and face low exit friction — they can shift to alternative vitality measures if needed. State language planners are beneficiaries (d near 0.15): they use the criterion to justify existing resource allocation decisions; shifting would require political negotiation but is strategically possible. Liturgical communities are victims (d near 0.85): the criterion denies them classification as 'living speakers,' triggering resource denial, educational exclusion, and stigma; their exit options are severely constrained by identity fusion with both the religious tradition and the language itself; if they exit the religious practice, they exit the language; if they exit the language, they exit the religious tradition. Diasporic heritage speakers occupy an intermediate position (d near 0.55): they transmit natively but in diaspora contexts, and the criterion's implicit framing of 'daily life' transmission as occurring in geographically concentrated native communities creates structural ambiguity about their status. No directionality override is needed: the derivation from beneficiary/victim declarations and exit constraints produces accurate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — language extinction in colonized and minority regions — was live and urgent when the native-generation criterion was adopted (roughly mid-20th century). By the 21st century, the founding problem has partially died: global internet connectivity, literary production, and diaspora communities have changed the landscape of language transmission; languages once thought extinct are being revitalized through non-native pathways; the criterion's framing as the only measure of vitality has become increasingly contested. Yet the constraint persists, and institutional investment in maintaining it has grown. This is a classic mandatrophy candidate: the founding problem is dead for many constituencies (literary and liturgical communities), contested for others (state planners in multilingual regions), and kept artificially 'live' by institutional interests in maintaining the native-generation criterion as the authoritative measure. The constraint does not show the classic piton profile (low extraction, high theater, diffuse cost) because the extraction remains substantial and asymmetric — it is not a theatrical remnant but an actively maintained apparatus. However, mandatrophy is structural: the criterion persists not because the founding problem demands it but because institutions have layered new functions (nationalism, resource allocation legitimacy) onto the original preservation rationale, and those new functions are extractive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    native_transmission_necessity,
    'Is native generational transmission a necessary and sufficient condition for language vitality, or is it one sufficient condition among several?',
    'Longitudinal study of languages with broken native transmission (literary or liturgical languages) that are intellectually productive, used in new domains, and transmitted effectively through non-familial pathways (formal education, community institutions, digital media). If such languages demonstrate sustained vitality without native generational transmission, the criterion is sufficient but not necessary.',
    'If native transmission is not necessary, the constraint''s beneficiary/victim structure changes: communities with literary or liturgical transmission would no longer be classified as ''dead'' or ''preserved''; institutional resources would reallocate; mandatrophy would become acute.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(native_transmission_necessity, empirical, 'Whether native transmission is truly the only measure of language vitality or one measure among others.').

omega_variable(
    daily_life_context,
    'What counts as ''daily life'' transmission? Does diaspora transmission of heritage languages at home count as ''daily life''? Does transmission through digital media count?',
    'Clarify the criterion''s operational definition in institutional policy documents and academic application. Test consistency across cases: does a diaspora child who speaks a heritage language at home and an immigrant language at school count as a native speaker? Does a speaker who uses a language primarily through social media and online communities count as ''daily life''?',
    'If ''daily life'' is narrowly interpreted (geographically concentrated, face-to-face, in pre-digital contexts), diaspora and digital communities are structurally excluded and victimized further. If broadly interpreted, the criterion loses definitional force. The scope of suppression depends on this clarification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(daily_life_context, conceptual, 'Operational definition of ''daily life'' transmission and its scope across different contexts.').

omega_variable(
    secular_nationalist_framing,
    'To what extent does the native-generation criterion reflect a secular, demographically-oriented model of language that is culturally specific to nationalist and modernizing ideologies, rather than a universal measure of linguistic vitality?',
    'Genealogical analysis tracing the criterion''s emergence in 20th-century nationalist movements and modernization theory. Cross-cultural comparison of non-Western, pre-nationalist frameworks for defining language vitality (e.g., religious, performative, intellectual criteria). Assess whether the criterion''s institutional dominance reflects its explanatory power or its alignment with dominant power structures.',
    'If the criterion is revealed as culturally specific rather than universal, its claim to objectivity is undermined; legitimacy would shift from ''scientific accuracy'' to ''one framework among others chosen by powerful institutions.'' This would enable alternative readings to claim equal legitimacy and redistribute resources accordingly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(secular_nationalist_framing, conceptual, 'Whether the native-generation criterion is culturally universal or specific to secular-nationalist frameworks.').

omega_variable(
    lithurgical_transmission_internalization,
    'To what extent is the suppression of liturgical communities structural (policy exclusion, resource denial) versus internalized (communities have internalized the ''corpse'' framing and experience shame about their own transmission mode)?',
    'Post-policy shift suppression trajectory: if liturgical communities are given equal resource access and institutional recognition, do they recover transmission confidence and intergenerational transmission rates, or has suppression become internalized (independent of the institutional frame)?',
    'If suppression is primarily internalized, the constraint''s effective suppression is higher than authored metrics suggest — the structural machinery could be removed but the psychological devaluation would persist. This would indicate deep identity-fusion and would require longer timescales for suppression recovery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lithurgical_transmission_internalization, empirical, 'Structural vs. internalized suppression in lithurgical communities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__native_generation_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(livi_tr_t0, living_language_status__native_generation_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(livi_tr_t0, observed).
narrative_ontology:measurement(livi_tr_t5, living_language_status__native_generation_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(livi_tr_t5, observed).
narrative_ontology:measurement(livi_tr_t10, living_language_status__native_generation_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(livi_tr_t10, observed).
narrative_ontology:measurement(livi_tr_t15, living_language_status__native_generation_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(livi_tr_t15, observed).
narrative_ontology:measurement(livi_tr_t25, living_language_status__native_generation_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(livi_tr_t25, observed).
narrative_ontology:measurement(livi_tr_t35, living_language_status__native_generation_reading, theater_ratio, 35, 0.41).
narrative_ontology:measurement_basis(livi_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(livi_be_t0, living_language_status__native_generation_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(livi_be_t0, observed).
narrative_ontology:measurement(livi_be_t5, living_language_status__native_generation_reading, base_extractiveness, 5, 0.47).
narrative_ontology:measurement_basis(livi_be_t5, observed).
narrative_ontology:measurement(livi_be_t10, living_language_status__native_generation_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(livi_be_t10, observed).
narrative_ontology:measurement(livi_be_t15, living_language_status__native_generation_reading, base_extractiveness, 15, 0.55).
narrative_ontology:measurement_basis(livi_be_t15, observed).
narrative_ontology:measurement(livi_be_t25, living_language_status__native_generation_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement_basis(livi_be_t25, observed).
narrative_ontology:measurement(livi_be_t35, living_language_status__native_generation_reading, base_extractiveness, 35, 0.58).
narrative_ontology:measurement_basis(livi_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(livi_su_t0, living_language_status__native_generation_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(livi_su_t0, observed).
narrative_ontology:measurement(livi_su_t5, living_language_status__native_generation_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement_basis(livi_su_t5, observed).
narrative_ontology:measurement(livi_su_t10, living_language_status__native_generation_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement_basis(livi_su_t10, observed).
narrative_ontology:measurement(livi_su_t15, living_language_status__native_generation_reading, suppression_requirement, 15, 0.65).
narrative_ontology:measurement_basis(livi_su_t15, observed).
narrative_ontology:measurement(livi_su_t25, living_language_status__native_generation_reading, suppression_requirement, 25, 0.67).
narrative_ontology:measurement_basis(livi_su_t25, observed).
narrative_ontology:measurement(livi_su_t35, living_language_status__native_generation_reading, suppression_requirement, 35, 0.67).
narrative_ontology:measurement_basis(livi_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_language_status__native_generation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(living_language_status__native_generation_reading, 0.12).
narrative_ontology:affects_constraint(living_language_status__native_generation_reading, living_language_status__lithurgical_preservation_reading).
narrative_ontology:affects_constraint(living_language_status__native_generation_reading, living_language_status__literary_continuity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel 'living_language_status.' Sibling constraints (lithurgical_preservation_reading, literary_continuity_reading) instantiate alternative readings of the same kernel. They are NOT measurements of the same constraint from different angles — they are fundamentally different definitions of linguistic vitality. Each reading has its own ε, beneficiary/victim structure, and type. The network links document the kernel family structure: the native-generation reading influences both sibling readings by imposing definitional pressure; the lithurgical and literary readings coexist as competing frameworks. All three share the same kernel but instantiate different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
