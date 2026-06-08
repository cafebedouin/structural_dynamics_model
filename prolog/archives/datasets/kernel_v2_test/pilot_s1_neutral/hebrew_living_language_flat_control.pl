% ============================================================================
% CONSTRAINT STORY: hebrew_living_language_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hebrew_living_language_flat_control
 *   human_readable: Hebrew as a Living Language Commitment
 *   domain: sociolinguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   The commitment 'Hebrew is a living language' represents a foundational
 *   claim about linguistic vitality that operated under radically different
 *   operational definitions across two millennia of Jewish diaspora and
 *   modern Israeli history. This constraint exhibits a peculiar structure: it
 *   is simultaneously a genuine coordination mechanism (solving the problem
 *   of inter-community communication across diaspora languages), an
 *   institutional extraction device (suppressing competing languages and
 *   marginalizing non-Hebrew-speaking communities), a false natural law
 *   (framing as inevitable what is actually constructed), and an increasingly
 *   performative institutional ritual (theater persists after functional
 *   necessity declines). The constraint emerged from the early Zionist
 *   movement and Eliezer Ben-Yehuda's revival efforts in late 19th-century
 *   Palestine, was institutionalized through Jewish education and British
 *   Mandate administration in the 1920s-1940s, and became a state-level
 *   enforced commitment after Israeli independence in 1948. The
 *   extractiveness trajectory shows an initial rise (1920-1950, from 0.15 to
 *   0.40) as institutional enforcement intensified, with a slight decline in
 *   contemporary period (0.40 to 0.38) as acquisition became naturalized and
 *   enforcement shifted to cultural rather than coercive mechanisms. The
 *   suppression requirement peaked (0.68) around 1960-1970 when
 *   second-language learner populations were largest and Hebrew dominance was
 *   still contested; it has declined slightly (0.62) as native-speaker
 *   cohorts emerged but remains substantial due to persistent treatment of
 *   alternative languages as subcultural markers. Theater ratio shows
 *   sustained elevation (0.58), indicating that significant performative
 *   content persists alongside genuine coordination function.
 *
 * KEY AGENTS:
 *   - Yiddish-speaking diaspora Jews: Primary victim (powerless/trapped) — bears cost of reframing their living language as spiritually incomplete; no exit from identity frame without abandoning community
 *   - Arabic-speaking Palestinian communities: Primary victim (powerless/trapped) — displaced from institutional and economic participation in emerging Hebrew-dominant order; no geographic exit options
 *   - Hebrew-learning immigrants and second-language speakers: Secondary victim (moderate/constrained) — face high learning barriers but benefit from coordination function; can exit through rejection of Israeli identity
 *   - Hebrew revival institutions and Zionist movement leaders: Primary beneficiary (institutional/arbitrage) — capture institutional prestige, cultural authority, and state-level enforcement power; can exit at any time (authored the constraint)
 *   - Israeli state and public education system: Organized enforcer (organized/constrained) — benefits from linguistic standardization and national coherence narrative; constrained by need to manage linguistic diversity and enforce suppression continuously
 *   - Academy of the Hebrew Language: Secondary institutional actor (institutional/arbitrage) — exercises performative authority over neologism and orthography; functionally degraded as constraint becomes self-sustaining
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — at risk of naturalizing institutional arrangement as property of language itself; false summit candidate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language_flat_control, 0.38).
domain_priors:suppression_score(hebrew_living_language_flat_control, 0.62).
domain_priors:theater_ratio(hebrew_living_language_flat_control, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language_flat_control, extractiveness, 0.38).
narrative_ontology:constraint_metric(hebrew_living_language_flat_control, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(hebrew_living_language_flat_control, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language_flat_control, tangled_rope).
narrative_ontology:human_readable(hebrew_living_language_flat_control, "Hebrew as a Living Language Commitment").
narrative_ontology:topic_domain(hebrew_living_language_flat_control, "sociolinguistics/language_revitalization/commitment_systems").

domain_priors:requires_active_enforcement(hebrew_living_language_flat_control).

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(hebrew_living_language_flat_control, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language_flat_control, hebrew_revival_institutions).
narrative_ontology:constraint_beneficiary(hebrew_living_language_flat_control, zionist_movement).
narrative_ontology:constraint_beneficiary(hebrew_living_language_flat_control, israeli_state).
narrative_ontology:constraint_victim(hebrew_living_language_flat_control, yiddish_speakers).
narrative_ontology:constraint_victim(hebrew_living_language_flat_control, arabic_speakers_in_mandate_palestine).
narrative_ontology:constraint_victim(hebrew_living_language_flat_control, diaspora_hebrew_reading_communities).
narrative_ontology:constraint_victim(hebrew_living_language_flat_control, linguistic_plurality).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_living_language_flat_control, hebrew_learning_immigrants).
narrative_ontology:constraint_victim(hebrew_living_language_flat_control, yiddish_speaking_diaspora).
narrative_ontology:constraint_victim(hebrew_living_language_flat_control, arabic_speaking_palestinians).
narrative_ontology:constraint_victim(hebrew_living_language_flat_control, hebrew_learning_immigrants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Majority language for Eastern European Jewish communities through 19th-20th centuries. Within the framework of 'Hebrew is living language' commitment, Yiddish becomes marked as parochial, spiritually incomplete, insufficiently Jewish. Speakers face identity pressure to adopt Hebrew despite Yiddish being their genuine mother tongue and community vernacular. Cannot exit without abandoning Jewish identity as traditionally understood by Zionist movement. Language carries identity — rejecting Yiddish means rejecting immigrant parents and community belonging.
narrative_ontology:constraint_stakeholder(hebrew_living_language_flat_control, yiddish_speaking_diaspora, payer,
    powerless, biographical, identity_locked, global).

% Majority language in Mandate Palestine and Jordan, Syria, Lebanon. As Hebrew is institutionalized through British Mandate administration and later Israeli state apparatus, Arabic speakers are progressively excluded from economic opportunity in Jewish-dominated sectors, administration, education in Hebrew-language schools. Linguistic exclusion becomes economic and civic exclusion. Cannot exit geographically (occupied territory) or linguistically (Arabic is their native language). No alternatives for participation in emerging Hebrew-dominant institutional order.
narrative_ontology:constraint_stakeholder(hebrew_living_language_flat_control, arabic_speaking_palestinians, payer,
    powerless, biographical, trapped, regional).

% Jewish immigrants from diverse diaspora communities (Yemen, Morocco, Poland, Iraq, etc.) arrive in Palestine/Israel speaking native languages other than Hebrew. Face requirement to acquire Hebrew for employment, education, civic participation. Learning curve is steep (foreign language, cultural context unfamiliar). Bear cost of acquisition, psychological strain of linguistic displacement. Benefit from shared language enabling communication across diaspora communities otherwise linguistically isolated. Can exit through not immigrating or through assimilation costs (psychological identity shift). Exit is possible but costly.
narrative_ontology:constraint_stakeholder(hebrew_living_language_flat_control, hebrew_learning_immigrants, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(hebrew_living_language_flat_control, hebrew_learning_immigrants, beneficiary).

% Eliezer Ben-Yehuda and the Academy of the Hebrew Language author and enforce the commitment 'Hebrew is living language.' Receive institutional prestige, cultural authority, funding for language work. Set educational policy, approve neologisms, establish standards. Directly benefit from enforcement — the more the commitment is institutionalized, the more authority accrues to the institutions that maintain it. Can exit at any time by ceasing enforcement; the constraint is theirs to relax or abandon. Authored it; can unauthor it.
narrative_ontology:constraint_stakeholder(hebrew_living_language_flat_control, hebrew_revival_institutions, agenda_setter,
    institutional, biographical, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(hebrew_living_language_flat_control, hebrew_revival_institutions, beneficiary).

% Post-1948 Israeli state enforces 'Hebrew is living language' through mandatory education, state administration, employment policy, cultural institutions. Benefits from linguistic standardization (efficient public administration, national narrative cohesion, assertion of sovereignty). Required to continuously reinforce the commitment through education and suppression of alternatives — theater is necessary because enforcement is not self-sustaining without institutional work. Constrained by need to manage linguistic diversity (Mizrahi, Russian-speaking, Arabic-speaking populations). Can theoretically exit but would require political cost (revision of national identity narrative).
narrative_ontology:constraint_stakeholder(hebrew_living_language_flat_control, israeli_state_education, agenda_setter,
    organized, generational, constrained, national).

% Formal institution charged with standardizing Hebrew, approving neologisms, maintaining language purity. Original function (creating vocabulary for modern referents in a liturgical language) was genuine and necessary work. Current function is largely ceremonial — native speakers acquire Hebrew without academy guidance; neologism approval has minimal real effect on actual language use. Institution persists through inertia and formal authority. Theater ratio high at this node — much of the institution's activity is performative assertion of authority over a language that now acquires itself. Can exit by dissolving; authority is an artifact of institutional structure.
narrative_ontology:constraint_stakeholder(hebrew_living_language_flat_control, academy_of_hebrew_language, agenda_setter,
    institutional, civilizational, arbitrage, global).

% External analytical position examining the constraint from linguistic universality perspective. At risk of naturalizing institutional commitment as property of language itself: 'living languages emerge from speaker communities,' 'language vitality is inherent to speech communities.' This framing makes the constraint appear as natural law when it is actually constructed institutional arrangement with identifiable beneficiaries, suppressions, and enforcement apparatus. Analytical position is itself positioned — it views from outside the commitment frame and may not recognize its own position-dependence.
narrative_ontology:constraint_stakeholder(hebrew_living_language_flat_control, analytical_observer, observer,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(hebrew_living_language_flat_control, analytical_observer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solving the diaspora-unity problem: Jewish communities from different geographic regions spoke mutually unintelligible languages (Yiddish, Ladino, Arabic, Amharic, etc.). Hebrew as a shared language enables communication, cultural identification, and collective action across these fragmented communities. Genuine coordination problem: no shared language existed naturally; multiple languages were equally legitimate within diaspora tradition.
% TRANSFER_FUNCTION: Authority and legitimacy flow from suppressed-language communities (Yiddish, Arabic speakers) to Hebrew institutions and the Israeli state (in the form of compliance with language policy, acceptance of Hebrew dominance in administration/education, suppression of children's native-language instruction). Economic participation flows selectively toward Hebrew speakers in Jewish institutions. Cultural prestige flows toward Hebrew and Zionist institutions; away from Yiddish (reframed as parochial) and Arabic (reframed as foreign). Identity work flows toward Hebrew assimilation; away from alternative language communities.
% ABSENT_VOICES: Yiddish-speaking cultural authorities (who saw Yiddish as equally Jewish and linguistically sophisticated) are excluded from the conversation framing Hebrew as 'the' Jewish language. Palestinian Arab intellectuals and language authorities are excluded entirely — their objections to Hebrew imposition are treated as external rather than as legitimate alternative linguistic perspectives. Pre-Zionist Jewish communities that maintained plural-language practices (Ottoman, North African, Sephardic communities comfortable with Ladino, Arabic, Hebrew coexistence) are excluded from the conversation.
% DISAPPEARANCE_RATIONALE: From the beneficiary perspective (Zionist institutions, Israeli state): if the commitment disappeared, Hebrew language would persist but would lose formal institutional backing and enforcement infrastructure. The state would lose its mechanism for linguistic standardization and national identity assertion. From the trapped perspective (Yiddish, Arabic speakers): if the commitment disappeared, their languages could return to educational and administrative domains currently closed to them. The world rearranges toward linguistic plurality. From the analytical perspective: if the commitment disappeared, Hebrew would continue as a living language (native speaker acquisition is now self-sustaining) — but the constraint's disappearance would change which arrangements depend on it. Some agents depend on Hebrew dominance (Israeli state, Hebrew institutions); others would benefit from its relaxation (Palestinian communities, diaspora Yiddish revival).
% FOUNDING_PROBLEM: Communication across Jewish diaspora languages. In the late 19th-early 20th century, Jewish communities from Yemen, Morocco, Poland, Iraq, Turkey, and elsewhere spoke native languages that were mutually unintelligible. Hebrew offered a solution: a historically Jewish language that no community owned exclusively, enabling neutral common ground. The founding problem is genuine and was real.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by: Ben-Yehuda's own writings describing diaspora linguistic fragmentation; early Zionist movement documents (1890s-1920s) identifying language as barrier to collective settlement; British Mandate education reports showing linguistic heterogeneity in Jewish immigrant population. The problem WAS live through 1960s (native speakers still acquiring Hebrew as second language). By 1970s, Israeli-born generations were native Hebrew speakers; the problem of inter-community communication across diaspora languages was functionally solved. Contemporary corroboration: Israeli linguists acknowledge that native acquisition is now self-sustaining; the state has no need to enforce Hebrew acquisition because children acquire it naturally. Enforcement continues despite functional accomplishment, indicating mandatrophy.
narrative_ontology:disappearance_verdict(hebrew_living_language_flat_control, contested).
narrative_ontology:founding_problem_status(hebrew_living_language_flat_control, dead).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: YIDDISH-SPEAKING DIASPORA (SNARE) — Trapped within a commitment system that reframes their living language (Yiddish, the actual vernacular of most Jewish communities through the 19th-20th centuries) as spiritually incomplete or linguistically deficient. The constraint suppresses alternatives: speaking Yiddish becomes marked as non-Zionist, parochial, or insufficiently Jewish. No exit from the identity frame without abandoning community belonging. Bears full cost of the 'Hebrew must be living' commitment without benefit.
constraint_indexing:constraint_classification(hebrew_living_language_flat_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: ARABIC-SPEAKING PALESTINIAN COMMUNITIES (SNARE) — Trapped by the implementation of 'Hebrew as living language' in institutional contexts (schools, administration, commerce under British Mandate and later Israeli sovereignty). The constraint suppresses Arabic as a legitimate language of daily civic life, displaces Arabic speakers from economic and educational opportunity in the emerging Hebrew-dominant order. No alternatives within the territory; no exit from the constraint without geographic displacement.
constraint_indexing:constraint_classification(hebrew_living_language_flat_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: HEBREW-LEARNING IMMIGRANTS (TANGLED ROPE) — Face high barriers to linguistic assimilation (years of intensive study, cultural learning, reorienting identity) but also benefit from the coordination function: Hebrew as a shared lingua franca enables communication across diaspora communities with different mother tongues. Constrained by psychological cost and time investment; benefit from collective coordination. Significant but not maximal extraction — agency exists through language choice and pace of acquisition.
constraint_indexing:constraint_classification(hebrew_living_language_flat_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: HEBREW REVIVAL INSTITUTIONS (ROPE) — Beneficiaries with substantial agency and arbitrage options. Eliezer Ben-Yehuda and the Academy of the Hebrew Language saw the constraint as solving a genuine collective-action problem: enabling Jewish unity across diaspora languages. Net beneficiaries from institutional prestige, publication authority, and cultural influence. Experience the constraint as coordination and cultural renewal. Low experienced extraction because they can exit the framework at any time (they authored it).
constraint_indexing:constraint_classification(hebrew_living_language_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: ISRAELI STATE / PUBLIC EDUCATION (TANGLED ROPE) — Organized agent enforcing the commitment through schooling, civil administration, and cultural policy. Benefits from linguistic standardization (administrative efficiency, national cohesion narrative, assertion of sovereignty). Constrained by need to manage linguistic diversity (Russian, Arabic, Amharic speaker populations) and perpetual need for enforcement (theater ratio remains high — constant reinforcement required to maintain dominance). Genuine coordination function (public education requires a shared language) layered with extraction (suppression of alternatives).
constraint_indexing:constraint_classification(hebrew_living_language_flat_control, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ACADEMY OF HEBREW LANGUAGE (PITON) — Original function (reviving a liturgical/literary language into a living vernacular) was achieved by early-to-mid 20th century. Contemporary role is largely performative: neologism approval, orthographic standardization, and ceremonial authority. The institution persists through inertia and institutional legitimacy rather than because the constraint requires active intervention. Functionally degraded — native speakers now acquire Hebrew without academy guidance; the constraint enforces itself through education and social norms. Theater ratio reflects this: much of the 'living language' work is now performance of authority rather than functional language revitalization.
constraint_indexing:constraint_classification(hebrew_living_language_flat_control, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a linguistic universality perspective, language vitality is a property of speaker communities, not of commitments. Either Hebrew was genuinely adopted as a living vernacular by sufficient speakers with sufficient daily use (in which case it IS living, emergent fact) or it was not (in which case labeling it 'living' is performative). The constraint appears to collapse into either tautology (Hebrew is living because enough people speak it, in which case the constraint is unnecessary) or false claim (Hebrew is not actually used daily as a vernacular, in which case the constraint cannot make it so). However, this perspective ignores the structural work the commitment performs: the false natural law framing naturalizes what is actually a constructed institutional arrangement.
constraint_indexing:constraint_classification(hebrew_living_language_flat_control, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_living_language_flat_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hebrew_living_language_flat_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hebrew_living_language_flat_control, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(hebrew_living_language_flat_control, TR),
    TR >= 0.70.

:- end_tests(hebrew_living_language_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts from trapped agents (Yiddish and Arabic speakers) but coordinates genuine communication across diaspora. The extraction is not maximal because the coordination function is real — immigrants genuinely benefit from shared language access, and the psychological benefit to Hebrew speakers (cultural renewal, national identity) is not entirely illegitimate extraction. The 30-year upward trend (0.15 to 0.40) reflects intensifying institutional enforcement; slight decline in final period (0.40 to 0.38) reflects naturalization of native acquisition, reducing need for active enforcement. Suppression (0.62): Moderate-high. Multi-layered suppression: institutional barriers (education policy, administrative language), economic barriers (employment in Hebrew-dominant sectors), identity barriers (Yiddish marked as parochial/diaspora, Arabic marked as foreign), and internalized shame (children refusing parents' languages). Peak suppression (0.68 at t=20) corresponds to post-independence state building and mandatory education; current level (0.62) reflects persistent cultural suppression despite some policy relaxation (recognition of Arabic as minority language, Yiddish cultural revival in diaspora). Theater ratio (0.58): Moderate-high. Significant performative content: Ben-Yehuda's linguistic neologism work was partly functional (creating vocabulary for modern referents in a language that had centuries of liturgical use without daily vernacular) and partly performative (establishing authority of the revival project). Contemporary theater reflects institutional inertia — the Academy's role is now mostly ceremonial; native acquisition is naturalized; extensive curricula and state resources devoted to 'teaching Hebrew to speakers who already speak it' signal theater persistence. The slight decline from 0.62 to 0.58 suggests theater is moderate rather than high, reflecting that the constraint does maintain real coordination function even as performative elements remain.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates dramatic perspectival divergence from a single base property set. The Yiddish-speaking diaspora and Palestinian communities see pure extraction (snare) — the constraint suppresses their languages with no offsetting coordination benefit for them. Hebrew-learning immigrants see mixed coordination and extraction (tangled-rope) — genuine lingua franca benefit layered with high acquisition barriers. Zionist institutional beneficiaries see coordination (rope) — solving the diaspora-unity problem. The Israeli state sees active enforcement with real coordination benefit (tangled-rope) — linguistic standardization for public education and administration, but perpetual suppression work required. The Academy sees its own institution as degraded (piton) — once functional (neologism creation) now performative (authority over a language that acquires itself naturally). The analytical observer risks seeing natural law (mountain) — 'living languages emerge from speaker communities' — but this naturalizes what is actually constructed institutional commitment with identifiable beneficiaries and suppressions. The perspectival gap reveals that all six types are simultaneously true: the constraint appears radically different depending on whether you occupy the position of a suppressed language community, a beneficiary institution, or an analytical observer who has not examined their own position-dependence.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from the structural relationship between agent and constraint: beneficiaries and those with arbitrage-exit options occupy d near 0.0 (low experienced extraction); trapped agents with no alternatives occupy d near 1.0 (maximum experienced extraction); moderate agents with constrained options occupy d in the 0.3-0.7 range. Zionist institutions and the Israeli state, as authored beneficiaries with institutional arbitrage options (they can exit the commitment at any time, having authored it), experience low d and thus low or negative effective extraction (the constraint subsidizes them through institutional prestige and enforcement power). Yiddish speakers and Palestinian communities, as trapped agents with no linguistic or geographic alternatives, experience high d and maximum extraction. Second-language learners experience moderate d (they can acquire Hebrew or resist it, but both choices carry cost). The engine derives these d values from the beneficiary/victim declarations and exit options; no manual adjustment is required except where the automatic derivation fails to capture a genuine structural relationship (no directionality overrides are authored for this constraint because the structural relationships are clear).
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is the divergence between original mandate and current function. The original mandate of 'Hebrew is a living language' was to solve a genuine structural problem: Jewish communities from different geographic regions spoke mutually unintelligible languages (Yiddish, Ladino, Arabic, etc.) and needed a shared language to maintain unity and enable migration/settlement in Palestine. This mandate WAS functionally satisfied by the 1960s: Hebrew became the native language of Israeli-born children and acquired fluency among immigrant populations. However, the commitment persists at full institutional strength (0.62 suppression, 0.38 extractiveness) despite functional accomplishment. The mandatrophy is not resolved — the state and educational institutions continue enforcing Hebrew dominance, continue suppressing Arabic in Palestinian communities and Yiddish in diaspora discourse, and continue investing resources in 'teaching Hebrew' to native speakers. This reflects what the piton and theater metrics show: functional core (coordination across diaspora language barriers) has atrophied, but performative core (symbolic assertion of Hebrew dominance, institutional theater of language authority, identity enforcement) persists. The constraint should have transitioned to a Scaffold classification (temporary coordination with sunset) but instead transitioned to Piton (degraded function, performative persistence). The mandatrophy is partially acknowledged in Israeli discourse (debate about Arabic language rights, revival of Yiddish cultural heritage) but not structurally resolved — the institutional enforcement apparatus remains active.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operational_definition_ambiguity,
    'What operational definition of ''living language'' was held by different communities across the two-millennium interval, and how did these definitions diverge?',
    'Textual analysis of contemporary sources (Ben-Yehuda writings, Palestinian Arabic-language newspapers, Yiddish-language journals, British Mandate education reports) revealing implicit definitions of ''living'' (daily vernacular use vs. liturgical fluency vs. institutional availability vs. native-speaker acquisition)',
    'If ''living'' meant native-speaker acquisition: constraint was achieved by 1950s, becomes piton afterward. If ''living'' meant institutional dominance: constraint remains active and extractive. If ''living'' meant psychological identity fusion: constraint persists even when structural acquisition drops, becomes identity_locked mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(operational_definition_ambiguity, empirical, 'Operational definition of ''living language'' across communities and time').

omega_variable(
    beneficiary_naturalness_ambiguity,
    'Is the identification of Hebrew revival institutions and Zionist leadership as beneficiaries a natural structural fact, or a reading imposed by viewing the constraint from outside the movement?',
    'First-person testimony from Ben-Yehuda circle, contemporary Jewish Agency documents, versus external analysis. Does the movement''s own narrative attribute benefit to itself, or is the benefit inferred only by external observers?',
    'If self-ascribed: beneficiary identification is robust and constraint is clearly tangled-rope with identifiable extraction. If observer-inferred: the constraint may be perceived as pure coordination (rope) from within the movement, and the extraction is perspectival (visible only to trapped agents). Changes whether FSM triggers and whether constraint is misnamed in movement discourse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_naturalness_ambiguity, conceptual, 'Whether beneficiaries are naturally identified or observer-ascribed').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.62) primarily structural (institutional barriers to Yiddish and Arabic use) or internalized (psychological rejection of non-Hebrew identity)?',
    'Longitudinal suppression trajectory post-1948 (structural removal in civil sphere) versus identity persistence (continued psychological valuation of Hebrew over other languages among Israeli-born generations). If suppression persists after structural barriers drop, mechanism is internalized.',
    'If structural: constraints can be relaxed through policy change; suppression floor is economic and administrative. If internalized: constraints persist despite policy change; identity-locked mechanisms dominate. Affects classification granularity: partially identity_locked victims would show different exit options than purely trapped ones.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural vs. internalized suppression mechanism').

omega_variable(
    false_summit_naturalness,
    'Is the mountain perspective''s claim that ''language vitality is an emergent fact'' genuinely a natural law, or does it naturalize the constructed commitment?',
    'Counterfactual analysis: if the Hebrew revival commitment had never been made and Yiddish had become the institutional lingua franca instead, would the ''natural law'' perspective still hold (yes = genuine natural law; no = the law is observing-position-dependent, hence not natural). Examine whether the analytical perspective''s framing would apply equally to a Yiddish-dominated alternative.',
    'If natural law: mountain classification stands, constraint is emergence property. If naturalization: mountain is false summit, constraint is institutional arrangement with identifiable beneficiaries, FSM applies. Affects whether constraint is treated as inevitable or revisable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_naturalness, conceptual, 'Whether the ''living language is natural fact'' perspective naturalizes an institutional arrangement').

omega_variable(
    mandate_palestine_temporal_window,
    'During the British Mandate period (1920-1948), what percentage of Palestinian Arabs adopted Hebrew as a second language, and what barriers prevented broader adoption?',
    'Census data on linguistic competence, school enrollment records, occupational statistics in mixed Jewish-Arab workforce. Comparison with adoption rates in other multilingual colonial contexts (India-English, Algeria-French). Distinction between administrative adoption (required for interaction with Jewish institutions) versus identity adoption.',
    'If barriers were primarily economic/institutional (removable): constraint is tangled-rope for Palestinians with path to exit through education. If barriers were identity-based (permanent): constraint is snare for Palestinian communities with no exit. Affects whether the victims designation is robust or overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_palestine_temporal_window, empirical, 'Linguistic adoption barriers and patterns in Mandate Palestine').

omega_variable(
    yiddish_suppression_internalization_timeline,
    'At what generational point did Yiddish suppression become internalized (children refusing to speak it to parents) rather than externally imposed?',
    'Oral history and family narrative analysis from Ashkenazi Jewish communities; correlation between onset of school-enforced Hebrew-only policies and self-reported language shame in home contexts. Comparison of immigrants'' retention rates (external suppression removed) versus Israeli-born rates (internalized suppression).',
    'If internalization occurred early (1940s-1950s): the current generation experiences identity_locked exit (cannot speak Yiddish without violating identity frame). If late (1970s onward): earlier generations have constrained rather than identity-locked options. Affects whether Yiddish is truly ''suppressed out of the system'' (piton trajectory) or remains as internalized trauma (snare persistence).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(yiddish_suppression_internalization_timeline, empirical, 'Timeline of Yiddish suppression internalization').

omega_variable(
    theater_ratio_trend_direction,
    'Is the theater ratio (0.58) currently rising, stable, or falling? Has the commitment''s performative content increased as its structural necessity decreased?',
    'Measurement of enforcement intensity: hours of Hebrew-only education, regulatory weight of Academy decisions, media content celebrating Hebrew revival. Comparison with naturalness of acquisition (native speakers learning without institutional pressure). Piton detection: if theater is rising while structural necessity falls, constraint is degrading.',
    'If rising: constraint is becoming piton, functional core has atrophied. If falling: constraint maintains real coordination function and snare/tangled-rope classifications are stable. If stable: functional and performative components remain balanced. Affects whether the commitment is self-sustaining or requires institutional theater to persist.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theater_ratio_trend_direction, empirical, 'Trajectory of theater ratio (performative vs. functional content)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language_flat_control, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebrew_living_tr_t0, hebrew_living_language_flat_control, theater_ratio, 0, 0.35).
narrative_ontology:measurement(hebrew_living_tr_t10, hebrew_living_language_flat_control, theater_ratio, 10, 0.48).
narrative_ontology:measurement(hebrew_living_tr_t20, hebrew_living_language_flat_control, theater_ratio, 20, 0.62).
narrative_ontology:measurement(hebrew_living_tr_t30, hebrew_living_language_flat_control, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(hebrew_living_be_t0, hebrew_living_language_flat_control, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(hebrew_living_be_t10, hebrew_living_language_flat_control, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(hebrew_living_be_t20, hebrew_living_language_flat_control, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(hebrew_living_be_t30, hebrew_living_language_flat_control, base_extractiveness, 30, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(hebrew_living_su_t0, hebrew_living_language_flat_control, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(hebrew_living_su_t10, hebrew_living_language_flat_control, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(hebrew_living_su_t20, hebrew_living_language_flat_control, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(hebrew_living_su_t30, hebrew_living_language_flat_control, suppression_requirement, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language_flat_control, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_living_language_flat_control, 0.12).
narrative_ontology:affects_constraint(hebrew_living_language_flat_control, yiddish_language_suppression).
narrative_ontology:affects_constraint(hebrew_living_language_flat_control, palestinian_arabic_displacement).
narrative_ontology:affects_constraint(hebrew_living_language_flat_control, israeli_national_identity_fusion).
narrative_ontology:affects_constraint(hebrew_living_language_flat_control, diaspora_hebrew_reading_tradition).

% DUAL FORMULATION NOTE:
% This flat control story models the commitment as a single constraint with perspectival disagreement about its nature. Decomposition into separate stories (Hebrew coordination function, Yiddish suppression mechanism, Palestinian displacement, identity fusion) would apply the ε-invariance principle — each sub-constraint has distinct measurement properties and persistence mechanisms. The network links indicate how this commitment affects and is affected by related linguistic constraints in the ecology.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
