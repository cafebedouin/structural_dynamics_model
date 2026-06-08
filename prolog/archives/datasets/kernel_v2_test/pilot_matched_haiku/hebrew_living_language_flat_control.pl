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
    narrative_ontology:constraint_vindicates/2,
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
    narrative_ontology:flat_control_of/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hebrew_living_language_flat_control
 *   human_readable: Hebrew as a Living Language: Commitment and Operational Definition
 *   domain: sociolinguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   The commitment 'Hebrew is a living language' represents a
 *   two-millennia-long contestation over what counts as linguistic vitality.
 *   For roughly 1,800 years (70 CE to 1880s), Hebrew existed primarily as a
 *   liturgical and literary language, spoken natively by no one but
 *   maintained through religious practice and scholarly tradition. The
 *   Zionist movement of the late 19th century undertook an unprecedented
 *   linguistic revival, claiming that Hebrew could and should become a
 *   living, native language for a new political community. This commitment
 *   was enforced through institutional gatekeeping (the Hebrew Language
 *   Academy, founded 1889), educational mandates, and political authority.
 *   Different communities held fundamentally different operational
 *   definitions of what 'living' meant: for the Academy, it meant conformity
 *   to prescribed standards; for diaspora speakers, it meant actual usage
 *   patterns; for Palestinian Arabs, it meant a language imposed on shared
 *   linguistic space; for Israeli native speakers born after 1948, it meant
 *   their native tongue. The constraint exhibits all six DR types from
 *   different perspectives because the commitment's operational definition
 *   was never settled — it was enforced through institutional power rather
 *   than consensus. The theater_ratio trajectory (0.35 → 0.61) shows the
 *   constraint degrading from functional necessity (early revival period)
 *   toward theatrical maintenance (contemporary Academy). The extractiveness
 *   trajectory (0.68 → 0.52) shows the constraint's extraction mechanism
 *   weakening as Hebrew achieved genuine native-speaker status, but the
 *   constraint persists because the Academy's institutional authority depends
 *   on the claim remaining contested.
 *
 * KEY AGENTS:
 *   - Hebrew Language Academy: Institutional beneficiary (institutional/arbitrage) — controls operational definition of 'living Hebrew'; benefits from gatekeeping authority and institutional legitimacy
 *   - Zionist Movement and Israeli State: Institutional beneficiary (institutional/arbitrage) — benefits from linguistic unity and national identity coordination; uses the commitment to enforce political integration
 *   - Diaspora Hebrew Speakers: Primary victim (powerless/trapped) — their actual linguistic practices excluded from the definition; no voice in defining the commitment they are bound by
 *   - Palestinian Arabic Speakers: Secondary victim (moderate/constrained) — constrained by Hebrew revitalization in shared linguistic space; marginalized in their own territory
 *   - Israeli Native Speakers: Organized agents (organized/constrained) — born into Hebrew as native language; coordinated by the commitment but also constrained by prescriptive standards they did not choose
 *   - Linguistic Authenticity Claims: Victim (non-agent) — the abstract commitment to linguistic authenticity is violated by the constructed nature of the revival; vindicated propositions (continuity doctrine, national sovereignty) benefit from the constraint's enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language_flat_control, 0.52).
domain_priors:suppression_score(hebrew_living_language_flat_control, 0.48).
domain_priors:theater_ratio(hebrew_living_language_flat_control, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language_flat_control, extractiveness, 0.52).
narrative_ontology:constraint_metric(hebrew_living_language_flat_control, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(hebrew_living_language_flat_control, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language_flat_control, tangled_rope).
narrative_ontology:human_readable(hebrew_living_language_flat_control, "Hebrew as a Living Language: Commitment and Operational Definition").
narrative_ontology:topic_domain(hebrew_living_language_flat_control, "sociolinguistics/language_revitalization/commitment_systems").

domain_priors:requires_active_enforcement(hebrew_living_language_flat_control).

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(hebrew_living_language_flat_control, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language_flat_control, hebrew_revival_movement).
narrative_ontology:constraint_beneficiary(hebrew_living_language_flat_control, israeli_state_institutions).
narrative_ontology:constraint_beneficiary(hebrew_living_language_flat_control, hebrew_language_academy).
narrative_ontology:constraint_victim(hebrew_living_language_flat_control, diaspora_hebrew_speakers).
narrative_ontology:constraint_victim(hebrew_living_language_flat_control, linguistic_authenticity_claims).
narrative_ontology:constraint_victim(hebrew_living_language_flat_control, alternative_hebrew_definitions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_living_language_flat_control, israeli_native_speakers).
narrative_ontology:constraint_victim(hebrew_living_language_flat_control, palestinian_arabic_speakers).
narrative_ontology:constraint_victim(hebrew_living_language_flat_control, israeli_native_speakers).
narrative_ontology:constraint_vindicates(hebrew_living_language_flat_control, hebrew_linguistic_continuity_doctrine).
narrative_ontology:constraint_vindicates(hebrew_living_language_flat_control, national_language_sovereignty_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the operational definition of 'living Hebrew' through prescriptive standards, vocabulary approval, and institutional authority. Benefits from gatekeeping authority and institutional legitimacy. Can shift definitions without losing institutional position. Primary beneficiary of the constraint.
narrative_ontology:constraint_stakeholder(hebrew_living_language_flat_control, hebrew_language_academy, agenda_setter,
    institutional, immediate, arbitrage, national).

% Uses the commitment to enforce linguistic unity and national identity. Benefits from coordination of a shared language for a new political community. Can shift linguistic policies without losing state authority. Primary beneficiary alongside the Academy.
narrative_ontology:constraint_stakeholder(hebrew_living_language_flat_control, zionist_movement_israeli_state, agenda_setter,
    institutional, generational, arbitrage, national).

% Their actual linguistic practices (Yiddish-influenced, Ladino-influenced, conversational) are excluded from the definition of 'living Hebrew'. No voice in defining the commitment they are bound by. Cannot exit without abandoning linguistic identity. Bears the cost of delegitimization and exclusion.
narrative_ontology:constraint_stakeholder(hebrew_living_language_flat_control, diaspora_hebrew_speakers, payer,
    powerless, biographical, trapped, global).

% Constrained by Hebrew revitalization in shared linguistic space. Marginalized in their own territory as Hebrew becomes the dominant language. Cannot fully exit without leaving the territory. Bears the cost of linguistic marginalization and displacement.
narrative_ontology:constraint_stakeholder(hebrew_living_language_flat_control, palestinian_arabic_speakers, payer,
    moderate, biographical, constrained, national).

% Born into Hebrew as native language. Benefit from linguistic coordination and national identity. But also constrained by prescriptive standards and Academy gatekeeping that diverge from their actual usage. Cannot exit Hebrew without exiting Israeli national identity. Mixed experience of coordination and extraction.
narrative_ontology:constraint_stakeholder(hebrew_living_language_flat_control, israeli_native_speakers, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(hebrew_living_language_flat_control, israeli_native_speakers, payer).

% The abstract commitment to linguistic authenticity is violated by the constructed nature of the Hebrew revival. The constraint's operation vindicates the doctrine of linguistic continuity (Hebrew as continuous from biblical times) while simultaneously revealing it as a constructed institutional achievement. Non-agent: a vindicated proposition that collects no rents.
narrative_ontology:constraint_stakeholder(hebrew_living_language_flat_control, linguistic_authenticity_doctrine, payer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(hebrew_living_language_flat_control, linguistic_authenticity_doctrine).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishing a shared language for a new political community (Palestine/Israel) and coordinating linguistic identity across diaspora and native speakers. The genuine coordination problem: how to create linguistic unity when the population speaks multiple languages and Hebrew has no native speakers.
% TRANSFER_FUNCTION: The constraint transfers linguistic legitimacy from diaspora speakers and Palestinian speakers to the Academy and Israeli state. It moves authority over language definition from distributed communities to centralized institutional gatekeeping. It transfers national identity from diaspora Jewish identity to Israeli Hebrew-based identity.
% ABSENT_VOICES: Diaspora Hebrew speakers are partially excluded from the conversation — their linguistic practices are not recognized as valid evidence of Hebrew vitality. Palestinian speakers are largely absent from the conversation about Hebrew revitalization, despite being constrained by it. Linguistic minorities within Israel (Mizrahi speakers, immigrant communities) have limited voice in defining 'living Hebrew'. Alternative language revitalization movements (Yiddish, Ladino) are absent from the conversation about what counts as linguistic vitality.
% DISAPPEARANCE_RATIONALE: If the commitment 'Hebrew is a living language' had not been enforced, the world would have rearranged significantly. Hebrew would likely have remained a liturgical and literary language, not a native language. The political formation of Israel would have proceeded differently — possibly with Arabic as the dominant language, or with linguistic pluralism rather than Hebrew-based national identity. The diaspora Jewish communities would have maintained their own linguistic practices (Yiddish, Ladino, etc.) without the pressure to adopt Hebrew. Palestinian speakers would not have been marginalized in their own territory. The constraint's disappearance would have changed the linguistic landscape of the Middle East and the nature of Israeli national identity.
% FOUNDING_PROBLEM: The founding problem was the absence of a native-speaker base for Hebrew in the late 19th century. The Zionist movement needed a shared language for a new political community, but Hebrew had no native speakers and was not widely spoken in daily life. The commitment 'Hebrew is a living language' was a claim that Hebrew could and should become a living, native language through institutional revival and educational mandates.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by historical records: Hebrew had no native speakers in 1880s Palestine. By the 1950s-1970s, Hebrew had achieved stable native-speaker status and became the primary language of Israeli society. Multiple sources corroborate this: linguistic historians (Rabin, Fellman), demographic data on language acquisition, educational records showing Hebrew as the primary language of Israeli schools. The problem that motivated the constraint has been solved — Hebrew is now genuinely a living language spoken natively by millions. However, the Academy and Israeli state institutions continue to enforce prescriptive standards and gatekeeping authority, suggesting the constraint persists for reasons beyond the original mandate.
narrative_ontology:disappearance_verdict(hebrew_living_language_flat_control, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_living_language_flat_control, dead).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DIASPORA HEBREW SPEAKERS (SNARE) — Trapped in a commitment whose operational definition they cannot control. The claim 'Hebrew is living' was enforced through institutional gatekeeping (Hebrew language academies, Zionist organizations) that defined what counted as 'living' Hebrew. Diaspora speakers' actual usage patterns were systematically excluded from the definition. No exit option: accepting the commitment meant accepting their own linguistic practice as inauthentic or dead. Maximum extraction — the constraint extracts legitimacy from diaspora speakers while denying them voice in defining the very commitment they are bound by.
constraint_indexing:constraint_classification(hebrew_living_language_flat_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PALESTINIAN ARABIC SPEAKERS (TANGLED ROPE) — Constrained by the institutional enforcement of Hebrew revitalization in shared linguistic space. The commitment 'Hebrew is living' carried implicit coordination function (establishing a shared language for a new political community) alongside asymmetric extraction (Hebrew revitalization was funded and mandated while Arabic was marginalized). Constrained exit: could not fully exit the linguistic space without leaving the territory, but also could not participate in defining what 'living Hebrew' meant. Mixed experience of coordination and extraction.
constraint_indexing:constraint_classification(hebrew_living_language_flat_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HEBREW LANGUAGE ACADEMY AND ZIONIST INSTITUTIONS (ROPE) — Primary beneficiary with arbitrage options. These institutions controlled the operational definition of 'living Hebrew' and benefited from the commitment through institutional authority, funding, and political legitimacy. The constraint solved a genuine coordination problem: establishing a shared language for a new political community. The institutions experienced the commitment as pure coordination — they were solving the problem of linguistic unity. Arbitrage exit: could shift definitions, adopt new vocabulary, or modify standards without losing institutional position.
constraint_indexing:constraint_classification(hebrew_living_language_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ISRAELI NATIVE SPEAKERS (TANGLED ROPE) — Organized agents born into Hebrew as a native language. The commitment 'Hebrew is living' coordinated their linguistic socialization and national identity. But the constraint also extracted from them: the operational definition of 'living Hebrew' was controlled by the Academy, not by native speakers. They were constrained by prescriptive standards that diverged from their actual usage. Constrained exit: could not exit Hebrew without exiting Israeli national identity, but could negotiate the boundaries of acceptable usage through generational linguistic drift.
constraint_indexing:constraint_classification(hebrew_living_language_flat_control, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONTEMPORARY HEBREW LANGUAGE ACADEMY (PITON) — The Academy's primary function was to define and enforce 'living Hebrew' during the revitalization period (1880s-1950s). That function has substantially atrophied: Hebrew is now genuinely spoken natively by millions, and the Academy's prescriptive authority is largely theatrical. The institution persists through inertia and symbolic authority, maintaining the commitment through performative gatekeeping (approving new vocabulary, issuing usage guidelines) that has minimal functional impact on actual language use. Theater ratio (0.61) reflects this degradation: much of the Academy's activity is now ceremonial rather than functionally necessary for language vitality.
constraint_indexing:constraint_classification(hebrew_living_language_flat_control, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the claim 'Hebrew is a living language' might appear as a natural fact: languages are either spoken natively or they are not. Once Hebrew achieved native-speaker status in Palestine/Israel, the claim became descriptively true regardless of institutional enforcement. This perspective risks naturalizing what was actually a contingent institutional achievement. The engine's false summit detector will identify this as a false summit: the 'naturalness' of Hebrew's vitality obscures the constructed nature of the commitment and the extraction mechanisms that enforced it.
constraint_indexing:constraint_classification(hebrew_living_language_flat_control, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

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
 *   Extractiveness (0.52): Moderate. The constraint exhibits genuine coordination function (establishing a shared language for a new political community) alongside asymmetric extraction (controlling who gets to define what counts as 'living'). The extractiveness is not as high as a pure Snare (0.75+) because the coordination function is real and benefits multiple parties. The value reflects that the constraint's primary function is coordination, but the distribution of benefits is asymmetric — the Academy and Israeli state capture disproportionate authority. Suppression (0.48): Moderate. Significant barriers to exit include institutional gatekeeping, educational mandates, and political authority. But suppression is not total — diaspora speakers could maintain their own Hebrew practices (though delegitimized), and Palestinian speakers could maintain Arabic (though marginalized). The trajectory shows suppression declining over time as Hebrew achieved native-speaker status and the need for institutional enforcement decreased. Theater ratio (0.61): Moderate-high. The Academy's contemporary role is substantially performative — approving new vocabulary, issuing usage guidelines, maintaining prescriptive standards that have minimal functional impact on actual language use. The trajectory shows theater increasing over time as the constraint's functional necessity declined but institutional maintenance persisted. This is the diagnostic signature of a Piton: the constraint is maintained through inertia and symbolic authority rather than functional necessity.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same commitment produces radically different classifications depending on the observer's structural position. The Academy sees pure coordination (Rope) — they are solving the problem of linguistic unity. Diaspora speakers see pure extraction (Snare) — their linguistic practices are excluded and delegitimized. Israeli native speakers see mixed coordination and extraction (Tangled Rope) — they benefit from linguistic unity but are constrained by prescriptive standards. Palestinian speakers see extraction with coordination cover (Tangled Rope) — the commitment coordinates a new political community but extracts from them. The contemporary Academy sees its own degraded ritual (Piton) — the institution persists through inertia, not function. The analytical observer risks seeing a natural fact (Mountain) — Hebrew is now genuinely spoken natively — but the structural data reveals this as a false summit: the 'naturalness' obscures the constructed nature of the commitment and the extraction mechanisms that enforced it. The perspectival gap is not a measurement error — it is the constraint's defining feature. The commitment 'Hebrew is a living language' means fundamentally different things to different communities because the operational definition was never settled; it was enforced through institutional power.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's structural position relative to the constraint. The Academy and Israeli state are beneficiaries with arbitrage options (d ≈ 0.1-0.2): they control the definition and can shift it without losing institutional position. Diaspora speakers are victims with trapped exit (d ≈ 0.9): they cannot exit the commitment without abandoning their linguistic identity, and they have no voice in defining it. Israeli native speakers are organized agents with constrained exit (d ≈ 0.6): they benefit from linguistic coordination but are constrained by prescriptive standards. Palestinian speakers are moderate agents with constrained exit (d ≈ 0.7): they are marginalized in the linguistic space but cannot fully exit without leaving the territory. The analytical observer has analytical exit (d ≈ 0.5): they can observe the constraint from outside but risk naturalizing its constructed nature. The directionality values feed into the engine's effective extraction computation (χ), which scales extractiveness by directionality and scope. High-d agents (victims) experience amplified extraction; low-d agents (beneficiaries) experience damped or negative extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits classic mandatrophy dynamics: the founding mandate was to establish Hebrew as a living language for a new political community. This mandate was substantially fulfilled by the 1950s-1970s, when Hebrew achieved stable native-speaker status and became the primary language of Israeli society. However, the constraint persists beyond the fulfillment of its mandate. The Hebrew Language Academy continues to enforce prescriptive standards and gatekeeping authority, but these functions are now largely theatrical — they have minimal functional impact on actual language use. The theater_ratio trajectory (0.35 → 0.61) shows this degradation: the constraint's functional necessity has declined while its theatrical maintenance has increased. The extractiveness trajectory (0.68 → 0.52) shows the constraint's extraction mechanism weakening as Hebrew achieved genuine vitality, but the constraint persists because the Academy's institutional authority depends on the claim remaining contested. The suppression_requirement trajectory (0.72 → 0.38) shows enforcement becoming less necessary as Hebrew became genuinely native. The constraint has not been formally sunset — the Academy still exists and still claims authority — but it has degraded toward Piton status. The mandatrophy is not fully resolved because the constraint's persistence serves institutional interests (the Academy's authority, the state's linguistic nationalism) even though the founding mandate has been fulfilled. A genuine resolution would require either (a) formal sunset of the Academy's gatekeeping authority, or (b) explicit acknowledgment that the constraint's function has changed from establishing vitality to maintaining prescriptive standards.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operational_definition_contestation,
    'What counts as ''living'' in ''Hebrew is a living language''? Is it native-speaker status, daily usage, institutional recognition, or something else?',
    'Historical analysis of how different communities defined ''living Hebrew'' across time periods; comparison of Academy definitions vs. actual usage patterns; examination of which linguistic practices were included/excluded from the definition',
    'If ''living'' = native-speaker status: the commitment became true around 1950s and the constraint''s extraction mechanism should have dissolved. If ''living'' = institutional recognition: the commitment remains contested and the Academy''s gatekeeping persists. If ''living'' = daily usage: diaspora Hebrew speakers'' practices should have counted, changing the victim/beneficiary structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(operational_definition_contestation, conceptual, 'Contested operational definition of linguistic vitality').

omega_variable(
    extraction_vs_coordination_boundary,
    'How much of the constraint''s function was genuine coordination (establishing a shared language) versus extraction (controlling linguistic legitimacy)?',
    'Comparative analysis: did Hebrew revitalization require the specific institutional gatekeeping mechanisms used, or could coordination have been achieved through less extractive means? Counterfactual: what would have happened if diaspora speakers'' Hebrew had been included in the definition of ''living''?',
    'If primarily coordination: the constraint is legitimately Rope/Tangled Rope and the extraction is a side effect of necessary enforcement. If primarily extraction: the constraint is legitimately Snare and the coordination function is cover. The boundary determines whether the Academy''s authority was justified or parasitic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_boundary, empirical, 'Proportion of coordination vs. extraction in the constraint''s operation').

omega_variable(
    false_summit_naturalization,
    'Is the contemporary claim ''Hebrew is a living language'' a natural fact or a constructed institutional achievement that benefits from appearing natural?',
    'Counterfactual history: would Hebrew have achieved native-speaker status without the institutional enforcement mechanisms? Analysis of alternative language revitalization movements and their success/failure rates. Examination of whether the Academy''s continued existence depends on the claim remaining contested.',
    'If natural fact: the constraint should reclassify as Mountain and the Academy''s role becomes merely descriptive. If constructed achievement: the constraint remains Tangled Rope/Snare and the Academy''s gatekeeping is essential to maintaining the beneficiary structure. The false summit detector will flag this as a candidate for reclassification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_naturalization, conceptual, 'Whether linguistic vitality is natural or constructed').

omega_variable(
    diaspora_hebrew_legitimacy,
    'Should diaspora Hebrew speakers'' actual linguistic practices have counted as evidence that Hebrew was ''living'' before the Zionist revival?',
    'Linguistic analysis of diaspora Hebrew usage patterns (Yiddish-influenced, Ladino-influenced, liturgical, conversational); comparison with Academy''s definition of acceptable Hebrew; examination of whether diaspora practices were deliberately excluded or simply not recognized',
    'If diaspora Hebrew should have counted: the constraint''s victim set expands and the extraction mechanism is more severe (active suppression of legitimate linguistic practice). If diaspora Hebrew was legitimately different: the constraint''s classification remains stable but the justification for exclusion becomes clearer.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(diaspora_hebrew_legitimacy, empirical, 'Status of diaspora Hebrew in the definition of linguistic vitality').

omega_variable(
    mandatrophy_status,
    'Has the founding mandate of the constraint (establishing Hebrew as a living language) been fulfilled, and if so, should the constraint persist?',
    'Temporal analysis: when did Hebrew achieve stable native-speaker status? When did the Academy''s gatekeeping become functionally unnecessary? Examination of whether the constraint''s persistence serves the original mandate or has become inertial.',
    'If mandate fulfilled (1950s-1970s): the constraint should have sunset and the Academy''s continued enforcement is theatrical (Piton). If mandate ongoing: the constraint remains functionally necessary (Tangled Rope). The theater_ratio trajectory will show whether the constraint is degrading toward Piton status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_status, empirical, 'Whether the founding mandate has been fulfilled').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language_flat_control, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebrew_living_theater_1880s, hebrew_living_language_flat_control, theater_ratio, 0, 0.35).
narrative_ontology:measurement(hebrew_living_theater_1920s, hebrew_living_language_flat_control, theater_ratio, 20, 0.42).
narrative_ontology:measurement(hebrew_living_theater_1948, hebrew_living_language_flat_control, theater_ratio, 40, 0.48).
narrative_ontology:measurement(hebrew_living_theater_1970s, hebrew_living_language_flat_control, theater_ratio, 60, 0.58).
narrative_ontology:measurement(hebrew_living_theater_2000s, hebrew_living_language_flat_control, theater_ratio, 80, 0.61).

% Extraction over time
narrative_ontology:measurement(hebrew_living_extractiveness_1880s, hebrew_living_language_flat_control, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(hebrew_living_extractiveness_1920s, hebrew_living_language_flat_control, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(hebrew_living_extractiveness_1948, hebrew_living_language_flat_control, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(hebrew_living_extractiveness_1970s, hebrew_living_language_flat_control, base_extractiveness, 60, 0.48).
narrative_ontology:measurement(hebrew_living_extractiveness_2000s, hebrew_living_language_flat_control, base_extractiveness, 80, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(hebrew_living_suppression_1880s, hebrew_living_language_flat_control, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(hebrew_living_suppression_1920s, hebrew_living_language_flat_control, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(hebrew_living_suppression_1948, hebrew_living_language_flat_control, suppression_requirement, 40, 0.58).
narrative_ontology:measurement(hebrew_living_suppression_1970s, hebrew_living_language_flat_control, suppression_requirement, 60, 0.42).
narrative_ontology:measurement(hebrew_living_suppression_2000s, hebrew_living_language_flat_control, suppression_requirement, 80, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language_flat_control, identity_coordination).
narrative_ontology:affects_constraint(hebrew_living_language_flat_control, yiddish_linguistic_marginalization).
narrative_ontology:affects_constraint(hebrew_living_language_flat_control, palestinian_arabic_displacement).
narrative_ontology:affects_constraint(hebrew_living_language_flat_control, israeli_national_identity_formation).

% DUAL FORMULATION NOTE:
% The commitment 'Hebrew is a living language' is a single constraint in this flat construction. A kernel-reading decomposition would separate the Academy's definition (formalized, lineage-grounded) from diaspora speakers' definition (distributed, practice-grounded) and Israeli native speakers' definition (implicit, practice-grounded). Each reading would have different axioms and reference frames. The flat construction treats these as perspectival disagreements within a single constraint rather than as separate readings of a contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hebrew_living_language_flat_control, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
