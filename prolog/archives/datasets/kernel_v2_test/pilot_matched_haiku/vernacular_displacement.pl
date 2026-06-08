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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Vernacular Displacement: Yiddish Suppression in Hebrew Revitalization
 *   domain: sociolinguistics/language_revitalization/cultural_policy
 *
 * SUMMARY:
 *   Vernacular displacement in Hebrew revitalization represents a structural
 *   constraint where a genuine coordination problem (integrating diverse
 *   immigrant populations into a single polity) was solved through active
 *   suppression of competing vernaculars, particularly Yiddish. The
 *   constraint exhibits snare characteristics from the perspective of Yiddish
 *   speakers and diaspora heritage communities, who bear extraction costs
 *   without meaningful exit options or beneficiary status. The Hebrew
 *   monolingualism project experienced the constraint as coordination (rope
 *   perspective), solving the legitimate problem of national linguistic
 *   unity. The constraint is downstream of the native-daily reading of the
 *   kernel 'Hebrew is a living language' — a reading that required
 *   suppression of the liturgical reading (Hebrew as living through ritual)
 *   and active marginalization of Yiddish as a competing vernacular. The
 *   measurement trajectory shows rising extractiveness (0.45 → 0.62) over the
 *   first two decades as institutional suppression intensified, followed by
 *   modest decline (0.62) as diaspora revitalization movements and changing
 *   global attitudes toward linguistic diversity created alternative pathways
 *   for Yiddish transmission. Suppression requirement declined over time
 *   (0.75 → 0.58) as the constraint became normalized and internalized,
 *   reducing the need for active enforcement. Theater ratio increased (0.35 →
 *   0.62) as Yiddish preservation shifted from living vernacular to heritage
 *   artifact, indicating degradation of the constraint's functional role.
 *
 * KEY AGENTS:
 *   - Yiddish speakers (immigrant generation): Primary victims (powerless/trapped) — face institutional penalties, educational exclusion, social stigma; no exit options
 *   - Yiddish cultural continuity (diaspora heritage): Primary victim (powerless/identity_locked) — identity-fused with diaspora identity; exit would require abandoning heritage identity
 *   - Hebrew monolingualism project (state apparatus): Primary beneficiary (institutional/arbitrage) — benefits from linguistic unity; has agency and exit options
 *   - Bilingual intellectuals: Secondary victims (moderate/constrained) — constrained by career incentives; experience mixed coordination and extraction
 *   - Yiddish institutional remnant (museums, archives): Degraded structure (institutional/constrained) — maintained through inertia; functionally peripheral
 *   - Diaspora language revitalization movement: Organized agents (organized/mobile) — see constraint as temporary; building alternative pathways through digital networks and diaspora communities
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — risks naturalizing policy choice as immutable law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vernacular_displacement, 0.62).
domain_priors:suppression_score(vernacular_displacement, 0.68).
domain_priors:theater_ratio(vernacular_displacement, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vernacular_displacement, extractiveness, 0.62).
narrative_ontology:constraint_metric(vernacular_displacement, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(vernacular_displacement, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vernacular_displacement, snare).
narrative_ontology:human_readable(vernacular_displacement, "Vernacular Displacement: Yiddish Suppression in Hebrew Revitalization").
narrative_ontology:topic_domain(vernacular_displacement, "sociolinguistics/language_revitalization/cultural_policy").

domain_priors:requires_active_enforcement(vernacular_displacement).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vernacular_displacement, 'fb59c52b-8b6a-4d91-a75b-bf240e13f803').
narrative_ontology:cs_kernel_codification('fb59c52b-8b6a-4d91-a75b-bf240e13f803', fixed_text).
narrative_ontology:cs_authority_grounding('fb59c52b-8b6a-4d91-a75b-bf240e13f803', extraction).
narrative_ontology:cs_interpretation_layer_present('fb59c52b-8b6a-4d91-a75b-bf240e13f803').
narrative_ontology:cs_reading_relation('fb59c52b-8b6a-4d91-a75b-bf240e13f803', vernacular_displacement__hebrew_liturgical_continuity, forecloses).
narrative_ontology:cs_reading_relation('fb59c52b-8b6a-4d91-a75b-bf240e13f803', vernacular_displacement__hebrew_continuity_narrative, influences).
narrative_ontology:cs_axiom('fb59c52b-8b6a-4d91-a75b-bf240e13f803', foundational, hebrew_living_through_vernacular_generation).
narrative_ontology:cs_axiom_status(hebrew_living_through_vernacular_generation, holdable).
narrative_ontology:cs_axiom_grounding('fb59c52b-8b6a-4d91-a75b-bf240e13f803', hebrew_living_through_vernacular_generation, empirically_contingent).
narrative_ontology:cs_axiom('fb59c52b-8b6a-4d91-a75b-bf240e13f803', secondary, yiddish_incompatible_with_hebrew_vitality).
narrative_ontology:cs_axiom_status(yiddish_incompatible_with_hebrew_vitality, holdable).
narrative_ontology:cs_axiom_grounding('fb59c52b-8b6a-4d91-a75b-bf240e13f803', yiddish_incompatible_with_hebrew_vitality, instrumental).
narrative_ontology:cs_reference_frame('fb59c52b-8b6a-4d91-a75b-bf240e13f803', hebrew_as_dormant_until_vernacular_reconstruction).
narrative_ontology:cs_drift_state('fb59c52b-8b6a-4d91-a75b-bf240e13f803', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fb59c52b-8b6a-4d91-a75b-bf240e13f803', '2026-02-26T14:32:00Z').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vernacular_displacement, hebrew_monolingualism_project).
narrative_ontology:constraint_beneficiary(vernacular_displacement, zionist_state_apparatus).
narrative_ontology:constraint_victim(vernacular_displacement, yiddish_cultural_continuity).
narrative_ontology:constraint_victim(vernacular_displacement, diaspora_linguistic_heritage).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vernacular_displacement, bilingual_intellectuals).
narrative_ontology:constraint_beneficiary(vernacular_displacement, diaspora_language_revitalization_movement).
narrative_ontology:constraint_victim(vernacular_displacement, yiddish_speakers_immigrant_generation).
narrative_ontology:constraint_victim(vernacular_displacement, bilingual_intellectuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Native Yiddish speakers arriving in Palestine/Israel face institutional penalties for using their native language. Schools restrict Yiddish instruction and penalize its use. Public discourse and media marginalize Yiddish as 'diaspora language.' Social mobility requires Hebrew fluency. No exit: cannot unlearn Yiddish, cannot maintain it without cost, cannot transmit to children without institutional resistance. The constraint forces linguistic assimilation while denying legitimacy to the source language.
narrative_ontology:constraint_stakeholder(vernacular_displacement, yiddish_speakers_immigrant_generation, payer,
    powerless, biographical, trapped, national).

% Diaspora linguistic heritage becomes delegitimized as 'exile language' and 'backward.' Second-generation speakers internalize the framing that Hebrew is authentic and forward-looking. Yiddish cultural transmission declines as speakers adopt the state's delegitimizing narrative. The binding is cognitive (identity fusion with diaspora identity) rather than purely material — abandoning Yiddish requires abandoning diaspora identity itself.
narrative_ontology:constraint_stakeholder(vernacular_displacement, yiddish_cultural_continuity, payer,
    powerless, generational, identity_locked, national).

% State apparatus and language planners implement policies to establish Hebrew as the sole national language. Educational institutions, media, and public discourse are organized around Hebrew monolingualism. The project benefits from linguistic unity enabling state administration, military integration, and national identity formation. The apparatus has agency and can adjust policy; it perceives the constraint as solving a genuine coordination problem.
narrative_ontology:constraint_stakeholder(vernacular_displacement, hebrew_monolingualism_project, agenda_setter,
    institutional, immediate, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(vernacular_displacement, hebrew_monolingualism_project, beneficiary).

% Intellectuals and professionals maintain Yiddish competence but face career incentives favoring Hebrew fluency. Hebrew enables participation in state institutions, academia, and public discourse. Yiddish fluency carries cultural and familial value but is a liability in professional contexts. The constraint coordinates linguistic unity (enabling their participation in Hebrew-medium institutions) while extracting from their maintenance of heritage language competence (career penalties, social marginalization).
narrative_ontology:constraint_stakeholder(vernacular_displacement, bilingual_intellectuals, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(vernacular_displacement, bilingual_intellectuals, beneficiary).

% Museums, archives, and academic programs preserve Yiddish as cultural heritage. These institutions maintain Yiddish through inertia and cultural guilt rather than active functional use. Yiddish is preserved as artifact and memory, not as living vernacular. The institutional remnant is constrained by limited funding and peripheral status in the broader cultural landscape.
narrative_ontology:constraint_stakeholder(vernacular_displacement, yiddish_institutional_remnant, observer,
    institutional, generational, constrained, national).

% Yiddish cultural organizations, academic networks, and diaspora communities outside Israel build alternative pathways for Yiddish transmission and cultural continuity. Digital communication enables diaspora connection and cultural exchange. Global attitudes toward linguistic diversity are shifting, creating legitimacy for multilingualism. The movement has agency and perceives an exit path: Yiddish can be revitalized outside the state apparatus through community networks and digital platforms.
narrative_ontology:constraint_stakeholder(vernacular_displacement, diaspora_language_revitalization_movement, beneficiary,
    organized, generational, mobile, global).

% From a civilizational perspective, language shift appears as an immutable feature of demographic and economic change. Dominant languages naturally displace minority languages through economic incentives and social mobility. This perspective risks naturalizing what is actually a state-enforced policy choice with identifiable beneficiaries and victims.
narrative_ontology:constraint_stakeholder(vernacular_displacement, analytical_observer, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(vernacular_displacement, analytical_observer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Integration of diverse immigrant populations into a single polity through shared linguistic medium. Hebrew as the common language enables state administration, military integration, education, and national identity formation. The coordination problem is genuine: how to create linguistic unity among speakers of dozens of languages arriving from different diaspora communities.
% TRANSFER_FUNCTION: The constraint transfers linguistic legitimacy from Yiddish to Hebrew. Yiddish speakers lose the ability to transmit their native language to children, participate in public discourse in their native language, and maintain cultural continuity through linguistic practice. The state apparatus gains unified linguistic control and national identity cohesion. Diaspora heritage communities lose cultural continuity and linguistic transmission pathways.
% ABSENT_VOICES: Yiddish speakers who emigrated to other countries (Americas, Western Europe) and maintained Yiddish without state suppression. These communities could testify to whether Yiddish decline is natural language shift or enforced suppression. Also absent: rabbinic authorities who maintained the liturgical reading of 'Hebrew as living language' and could have negotiated coexistence of Hebrew and Yiddish. Also absent: Yiddish intellectuals and cultural leaders whose counter-narrative about Hebrew as constructed language (rather than restored) was suppressed.
% DISAPPEARANCE_RATIONALE: If the vernacular displacement constraint disappeared overnight, the world would rearrange: Yiddish transmission would resume in families and communities; diaspora heritage would be revitalized; bilingual education would become possible; the state apparatus would need to develop alternative mechanisms for linguistic integration (possibly multilingual policy). The constraint is not a natural fact but a policy choice with structural consequences.
% FOUNDING_PROBLEM: How to integrate diverse immigrant populations (speaking dozens of languages) into a single polity and create national identity cohesion through shared linguistic medium.
% FOUNDING_PROBLEM_CORROBORATION: By the 1960s-1970s, Hebrew was firmly established as the dominant language of state administration, education, and public discourse. The coordination problem was solved: linguistic unity was achieved. However, the suppression mechanism persists. Corroboration from: (1) demographic data showing Hebrew dominance by second generation; (2) state education statistics showing Hebrew as primary language of instruction; (3) public discourse analysis showing Hebrew as lingua franca. The founding problem is dead, but the constraint persists through institutional inertia and cultural guilt.
narrative_ontology:disappearance_verdict(vernacular_displacement, world_rearranges).
narrative_ontology:founding_problem_status(vernacular_displacement, dead).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: YIDDISH SPEAKER (SNARE) — Trapped by institutional policy, educational exclusion, and social stigma. Native language becomes liability in public sphere. No exit: cannot unlearn Yiddish, cannot maintain it without social cost, cannot transmit to children without resistance. Maximum extraction — the constraint forces linguistic assimilation while denying the legitimacy of the source language.
constraint_indexing:constraint_classification(vernacular_displacement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: YIDDISH CULTURAL CONTINUITY (SNARE) — Identity-locked victim. The constraint is not merely external suppression but internalized delegitimation: Yiddish becomes marked as 'diaspora language,' 'exile language,' 'backward.' Second-generation speakers internalize the framing that Hebrew is the authentic, forward-looking choice. The binding is cognitive (identity fusion with diaspora identity) rather than purely material, but the exit is functionally impossible because abandoning Yiddish requires abandoning diaspora identity itself.
constraint_indexing:constraint_classification(vernacular_displacement, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: BILINGUAL INTELLECTUAL (TANGLED ROPE) — Constrained by career incentives and social mobility. Hebrew fluency enables participation in state institutions, academia, and public discourse. Yiddish fluency is a liability in professional contexts but carries cultural and familial value. The constraint coordinates linguistic unity (genuine coordination function) while extracting from those who maintain Yiddish competence (career penalties, social marginalization). Mixed experience: some benefit from the coordination (access to Hebrew-medium institutions) alongside extraction (pressure to abandon heritage language).
constraint_indexing:constraint_classification(vernacular_displacement, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: HEBREW MONOLINGUALISM PROJECT (ROPE) — Institutional beneficiary with arbitrage options. Experiences the constraint as coordination: unified national language enables state administration, education, military integration, and national identity formation. The project has agency and can adjust policy; it perceives the constraint as solving a genuine coordination problem (how to integrate diverse immigrant populations into a single polity). Net beneficiary — extraction flows toward this agent.
constraint_indexing:constraint_classification(vernacular_displacement, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: YIDDISH INSTITUTIONAL REMNANT (PITON) — Degraded institutional structure maintained through inertia and nostalgia. Yiddish museums, archives, and academic programs exist but are theatrically preserved rather than functionally vital. The constraint persists through institutional memory and cultural guilt rather than active enforcement. Theater ratio is moderate (0.55) because the preservation is genuine but peripheral — Yiddish is maintained as heritage artifact, not as living vernacular.
constraint_indexing:constraint_classification(vernacular_displacement, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: DIASPORA LANGUAGE REVITALIZATION MOVEMENT (SCAFFOLD) — Organized agents (Yiddish cultural organizations, academic networks, diaspora communities outside Israel) see the constraint as a temporary institutional arrangement with a sunset. Digital communication, diaspora networks, and academic interest in multilingualism are creating alternative pathways for Yiddish transmission and cultural continuity. The movement has agency and perceives an exit path: Yiddish can be revitalized outside the state apparatus through community networks and digital media. Sunset logic: as global attitudes toward linguistic diversity shift and digital platforms enable diaspora connection, the state monopoly on language legitimacy weakens.
constraint_indexing:constraint_classification(vernacular_displacement, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, language shift is an immutable feature of demographic and economic change: dominant languages naturally displace minority languages through economic incentives and social mobility. This perspective sees vernacular displacement as a natural law of sociolinguistics, not as a contingent institutional arrangement. However, the structural data contradicts this classification — the engine will compute this as a false summit, revealing that 'natural language shift' naturalizes what is actually a state-enforced policy choice with identifiable beneficiaries and victims.
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
 *   Extractiveness (0.62): Moderate-high. The constraint extracts from Yiddish speakers through institutional penalties (school restrictions, media exclusion, social stigma) and from diaspora heritage communities through delegitimation of their cultural continuity. The extraction is not total because some bilingual intellectuals benefit from Hebrew coordination while maintaining Yiddish competence, and diaspora communities outside the state apparatus can maintain Yiddish without direct suppression. The measurement trajectory (0.45 → 0.68 → 0.62) reflects intensifying suppression during state consolidation (1920s-1950s), peak extraction as institutional policies matured, followed by modest decline as diaspora revitalization and changing global attitudes created alternative pathways. Suppression (0.68): High. Institutional policies actively penalized Yiddish use in schools, media, and public discourse. Social stigma delegitimized Yiddish as 'diaspora language' and 'exile language.' Economic incentives favored Hebrew fluency for social mobility and state participation. However, suppression was not absolute — Yiddish persisted in private domains, diaspora communities, and cultural institutions. The measurement trajectory (0.75 → 0.58) reflects declining suppression requirement as the constraint became normalized and internalized, reducing the need for active enforcement. Theater ratio (0.55): Moderate. The constraint has both functional and performative elements. The functional element is genuine coordination around linguistic unity for state administration, education, and national identity. The performative element is the theatrical preservation of Yiddish as heritage artifact (museums, archives, academic programs) while maintaining suppression of its living use. The rising trajectory (0.35 → 0.62) reflects increasing theatricality as Yiddish shifted from living vernacular to cultural memory.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full range of DR classification from a single set of base properties. Yiddish speakers see pure extraction (Snare) — institutional suppression with no coordination benefit. The state apparatus sees coordination (Rope) — linguistic unity enabling national integration. Bilingual intellectuals see mixed coordination and extraction (Tangled Rope) — Hebrew coordination alongside Yiddish suppression. Diaspora revitalization movements see a temporary problem with a sunset (Scaffold) — alternative pathways through digital networks and diaspora communities. The institutional remnant sees a degraded ritual (Piton) — Yiddish preserved theatrically but functionally peripheral. The analytical observer risks seeing an immutable natural law (Mountain) — language shift as inherent to modernization — but the structural data reveals this as a false summit: the constraint is a policy choice with identifiable beneficiaries (state apparatus) and victims (Yiddish speakers and diaspora heritage).
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value (d) is determined by the agent's structural position relative to the constraint. Yiddish speakers are trapped victims with no exit options — maximum d (approaching 1.0) — producing maximum experienced extraction. Diaspora heritage communities are identity-locked victims — high d but with cognitive rather than material binding — producing high extraction that persists even after material barriers are removed. The state apparatus is an institutional beneficiary with arbitrage options — low d (approaching 0.0) — producing negative effective extraction (subsidy). Bilingual intellectuals are constrained victims with some benefit from coordination — moderate d (around 0.5) — producing moderate extraction. The diaspora revitalization movement is organized with mobile exit options — low d — producing low extraction. The analytical observer occupies the universal/civilizational context where the constraint appears as natural law, but the false summit detector identifies this as naturalization of a contingent policy choice.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits mandatrophy: the original mandate (integrate diverse immigrant populations into a single polity through linguistic unity) has been achieved, but the suppression mechanism persists. The measurement trajectory shows that suppression requirement declined (0.75 → 0.58) as the constraint became normalized and internalized, yet extractiveness remained elevated (0.62) because the institutional penalties and social stigma persist even after the coordination problem is solved. The theater ratio increased (0.35 → 0.62) as Yiddish shifted from living vernacular to heritage artifact, indicating that the constraint's functional role has atrophied while its performative role has expanded. The constraint is now maintained through institutional inertia and cultural guilt rather than active enforcement — a classic piton signature. However, the snare classification remains accurate because the constraint continues to extract from Yiddish speakers and diaspora heritage communities despite the mandate being fulfilled. The mandatrophy is not resolved because the state apparatus continues to benefit from linguistic unity and has no incentive to dismantle the suppression mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_enforced_shift,
    'Is the observed Yiddish decline a natural consequence of economic incentives and demographic change, or a result of active state suppression and institutional policy?',
    'Comparative analysis: language shift patterns in communities with and without state suppression policies; historical counterfactual analysis of what Yiddish transmission rates would be without institutional penalties; examination of diaspora communities where Yiddish faced economic pressure but not state suppression.',
    'If natural: mountain classification confirmed — language shift is inherent to modernization. If enforced: snare classification confirmed — the constraint is a policy choice with identifiable beneficiaries and victims. If mixed: tangled_rope classification more accurate — genuine coordination function (national unity) alongside extraction (suppression of heritage language).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_vs_enforced_shift, empirical, 'Whether Yiddish decline is natural shift or enforced suppression').

omega_variable(
    identity_lock_mechanism,
    'To what extent is Yiddish speaker exit constrained by material barriers (institutional policy, economic incentives) versus internalized identity frames (diaspora identity, shame, delegitimation)?',
    'Ethnographic analysis of speaker narratives; comparison of exit patterns across generations; examination of diaspora communities where Yiddish speakers face economic pressure but not institutional suppression; post-suppression trajectory analysis (do speakers maintain Yiddish after institutional barriers are removed?).',
    'If primarily material: trapped classification more accurate. If primarily internalized: identity_locked classification more accurate. If mixed: the suppression mechanism is more durable because it operates at both structural and cognitive levels.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Material vs. internalized mechanisms of Yiddish speaker constraint').

omega_variable(
    kernel_reading_foreclosure,
    'Do the liturgical reading (''Hebrew as living through ritual'') and native-daily reading (''Hebrew as living through vernacular reconstruction'') represent genuinely incompatible frameworks, or can they coexist within a single commitment system?',
    'Historical analysis of rabbinic responses to Zionist language planning; examination of whether liturgical authorities explicitly rejected the native-daily reading or merely occupied different institutional domains; analysis of contemporary Orthodox communities that maintain both ritual Hebrew and Yiddish vernacular.',
    'If foreclosed: the readings are mutually exclusive — the native-daily reading required suppression of the liturgical reading''s authority. If coexistent: the readings occupy different domains and the suppression of Yiddish is not a necessary consequence of Hebrew revival. If influenced: the native-daily reading created structural pressure on the liturgical reading without logically foreclosing it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether kernel readings foreclose or coexist').

omega_variable(
    state_apparatus_extraction,
    'Does the Hebrew monolingualism project extract benefit from Yiddish suppression, or is suppression merely a side effect of coordination around linguistic unity?',
    'Analysis of state policies: were Yiddish-specific penalties (school restrictions, media exclusion, social stigma) necessary for Hebrew coordination, or were they chosen to maximize extraction? Comparison with multilingual nation-states that achieved linguistic coordination without suppressing minority languages. Examination of whether state apparatus actively enforced Yiddish suppression or merely failed to protect it.',
    'If extraction is primary: snare classification confirmed. If coordination is primary and suppression is side effect: tangled_rope classification more accurate. If suppression is incidental: rope classification more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_apparatus_extraction, empirical, 'Whether state apparatus actively extracts from Yiddish suppression').

omega_variable(
    false_summit_mountain,
    'Is the analytical observer''s mountain classification (language shift as natural law) a genuine natural law or a false summit that naturalizes a contingent policy choice?',
    'Examination of whether language shift occurs at similar rates in communities with and without state suppression; analysis of whether the ''natural law'' framing serves the interests of the state apparatus; investigation of whether the mountain classification would hold if the state had chosen multilingual policy instead.',
    'If genuine natural law: mountain classification stands. If false summit: the constraint is snare or tangled_rope, and the ''natural law'' framing is a cover story for policy choice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_mountain, empirical, 'Whether language shift is natural law or false summit').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vernacular_displacement, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vern_disp_tr_t0, vernacular_displacement, theater_ratio, 0, 0.35).
narrative_ontology:measurement(vern_disp_tr_t10, vernacular_displacement, theater_ratio, 10, 0.45).
narrative_ontology:measurement(vern_disp_tr_t20, vernacular_displacement, theater_ratio, 20, 0.55).
narrative_ontology:measurement(vern_disp_tr_t30, vernacular_displacement, theater_ratio, 30, 0.62).

% Extraction over time
narrative_ontology:measurement(vern_disp_be_t0, vernacular_displacement, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(vern_disp_be_t10, vernacular_displacement, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(vern_disp_be_t20, vernacular_displacement, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(vern_disp_be_t30, vernacular_displacement, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(vern_disp_su_t0, vernacular_displacement, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(vern_disp_su_t10, vernacular_displacement, suppression_requirement, 10, 0.72).
narrative_ontology:measurement(vern_disp_su_t20, vernacular_displacement, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(vern_disp_su_t30, vernacular_displacement, suppression_requirement, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vernacular_displacement, identity_coordination).
narrative_ontology:affects_constraint(vernacular_displacement, native_daily_reading).
narrative_ontology:affects_constraint(vernacular_displacement, hebrew_liturgical_continuity).
narrative_ontology:affects_constraint(vernacular_displacement, diaspora_identity_formation).

% DUAL FORMULATION NOTE:
% Vernacular displacement is downstream of the native-daily reading of the kernel 'Hebrew is a living language.' The native-daily reading (tangled_rope: genuine coordination around linguistic unity alongside extraction from Yiddish speakers) generates the vernacular displacement constraint (snare: pure extraction from Yiddish speakers and diaspora heritage). The two constraints are linked by causal dependency: the native-daily reading's dominance required suppression of competing vernaculars. Separate stories enable independent measurement of extractiveness at each level: the native-daily reading has moderate extractiveness (coordination function is genuine); vernacular displacement has higher extractiveness (extraction is primary, coordination is secondary).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vernacular_displacement, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
