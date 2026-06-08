% ============================================================================
% CONSTRAINT STORY: vernacular_displacement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-09
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   human_readable: Vernacular Displacement in Hebrew Language Revival
 *   domain: sociolinguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   The Hebrew language revival in Mandatory Palestine and early Israel
 *   required the displacement of Yiddish as the dominant Ashkenazi
 *   vernacular. While Hebrew served a genuine coordination function (enabling
 *   communication across immigrant communities from dozens of linguistic
 *   backgrounds), the costs of language shift were asymmetrically borne by
 *   Yiddish-speaking communities. Institutional policies penalized Yiddish
 *   use in schools, civic participation, and employment. Social stigma
 *   campaigns framed Yiddish as 'jargon' of the diaspora, incompatible with
 *   modern nationhood. The constraint exhibits rising extractiveness and
 *   suppression through the state-building period (1920s-1950s), peaking
 *   during the early statehood years when enforcement was most intense, then
 *   declining as Hebrew became demographically dominant and active
 *   suppression became less necessary. Theater ratio rises over the interval
 *   as the coordination justification (linguistic unity for state survival)
 *   becomes increasingly performative — by the 1960s, Hebrew dominance was
 *   secure, but anti-Yiddish stigma persisted beyond functional necessity.
 *   The constraint is downstream of the native-daily reading of the 'Hebrew
 *   as living language' kernel: the commitment that only generative
 *   vernacular use constitutes linguistic life required suppressing competing
 *   vernaculars to succeed.
 *
 * KEY AGENTS:
 *   - Yiddish-Speaking Immigrants: Primary victims (powerless/trapped) — bore costs of language shift through institutional penalties, social stigma, and cultural erasure; could not exit without abandoning Zionist project
 *   - Yiddish Cultural Continuity: Abstract victim (powerless/identity_locked) — the collective good of Yiddish literary tradition and diasporic identity; identity-locked because preservation requires maintaining Yiddish as living vernacular, not just archival object
 *   - Hebrew Monolingualism Project: Primary beneficiary (institutional/arbitrage) — state-building apparatus, language planning authorities, Zionist cultural institutions; captured institutional resources and cultural prestige
 *   - Bilingual Educators: Mixed position (moderate/constrained) — experienced both coordination benefits (Hebrew enabled cross-community teaching) and extraction (professional requirement to suppress Yiddish, loss of pedagogical richness)
 *   - Haredi Community: Organized resistance (organized/constrained) — maintained Yiddish as internal vernacular while participating in Hebrew civic life; partial exit through parallel institutions
 *   - Diaspora Yiddish Revival Movement: Scaffold perspective (organized/mobile) — sees constraint as temporary; geographic exit and organizational capacity enable Yiddish cultural work outside suppression zone
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vernacular_displacement, 0.62).
domain_priors:suppression_score(vernacular_displacement, 0.73).
domain_priors:theater_ratio(vernacular_displacement, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vernacular_displacement, extractiveness, 0.62).
narrative_ontology:constraint_metric(vernacular_displacement, suppression_requirement, 0.73).
narrative_ontology:constraint_metric(vernacular_displacement, theater_ratio, 0.41).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vernacular_displacement, snare).
narrative_ontology:human_readable(vernacular_displacement, "Vernacular Displacement in Hebrew Language Revival").
narrative_ontology:topic_domain(vernacular_displacement, "sociolinguistics/language_revitalization/commitment_systems").

domain_priors:requires_active_enforcement(vernacular_displacement).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vernacular_displacement, 'a1376b1a-6bc6-442c-9a2b-66b13c104952').
narrative_ontology:cs_kernel_codification('a1376b1a-6bc6-442c-9a2b-66b13c104952', distributed).
narrative_ontology:cs_authority_grounding('a1376b1a-6bc6-442c-9a2b-66b13c104952', extraction).
narrative_ontology:cs_reading_relation('a1376b1a-6bc6-442c-9a2b-66b13c104952', vernacular_displacement__liturgical_reading, forecloses).
narrative_ontology:cs_reading_relation('a1376b1a-6bc6-442c-9a2b-66b13c104952', vernacular_displacement__continuity_narrative_reading, influences).
narrative_ontology:cs_axiom('a1376b1a-6bc6-442c-9a2b-66b13c104952', foundational, generative_vernacular_use_required_for_linguistic_life).
narrative_ontology:cs_axiom_status(generative_vernacular_use_required_for_linguistic_life, holdable).
narrative_ontology:cs_axiom_grounding('a1376b1a-6bc6-442c-9a2b-66b13c104952', generative_vernacular_use_required_for_linguistic_life, conventional).
narrative_ontology:cs_axiom('a1376b1a-6bc6-442c-9a2b-66b13c104952', secondary, monolingualism_required_for_national_cohesion).
narrative_ontology:cs_axiom_status(monolingualism_required_for_national_cohesion, holdable).
narrative_ontology:cs_axiom_grounding('a1376b1a-6bc6-442c-9a2b-66b13c104952', monolingualism_required_for_national_cohesion, empirically_contingent).
narrative_ontology:cs_reference_frame('a1376b1a-6bc6-442c-9a2b-66b13c104952', liturgical_continuity_baseline).
narrative_ontology:cs_drift_state('a1376b1a-6bc6-442c-9a2b-66b13c104952', post_state_establishment, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('a1376b1a-6bc6-442c-9a2b-66b13c104952', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vernacular_displacement, hebrew_monolingualism_project).
narrative_ontology:constraint_beneficiary(vernacular_displacement, state_building_apparatus).
narrative_ontology:constraint_beneficiary(vernacular_displacement, zionist_language_planners).
narrative_ontology:constraint_victim(vernacular_displacement, yiddish_cultural_continuity).
narrative_ontology:constraint_victim(vernacular_displacement, yiddish_speaking_communities).
narrative_ontology:constraint_victim(vernacular_displacement, multilingual_diaspora_identity).
narrative_ontology:constraint_vindicates(vernacular_displacement, linguistic_unity_prerequisite_for_nationhood).
narrative_ontology:constraint_vindicates(vernacular_displacement, vernacular_monolingualism_as_modernity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: YIDDISH-SPEAKING IMMIGRANT (SNARE) — Trapped by geographic migration, economic dependency, and institutional penalties. Cannot exit the Hebrew-dominant environment without abandoning the Zionist project entirely. Experiences maximum extraction: language shift imposed through school policy, employment requirements, civic participation barriers, and social stigma. The coordination story (national unity requires shared language) is cover for cultural erasure.
constraint_indexing:constraint_classification(vernacular_displacement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: BILINGUAL EDUCATOR (TANGLED ROPE) — Constrained by institutional policy but possesses linguistic capital in both languages. Benefits from the coordination function (Hebrew enables cross-community communication in a multilingual immigrant society) while bearing costs (professional requirement to suppress Yiddish in classroom, loss of pedagogical richness from multilingual instruction). Mixed experience: genuine coordination need exists alongside asymmetric extraction.
constraint_indexing:constraint_classification(vernacular_displacement, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LANGUAGE PLANNING AUTHORITY (ROPE) — Primary beneficiary with arbitrage-level exit options. Experiences the constraint as pure coordination: Hebrew revival solves the genuine problem of linguistic fragmentation across immigrant communities from dozens of countries. Extraction runs toward this agent (institutional authority, cultural prestige, state resources) rather than away. The Yiddish displacement is invisible from this position or rationalized as necessary cost of modernization.
constraint_indexing:constraint_classification(vernacular_displacement, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: YIDDISH CULTURAL CONTINUITY (SNARE, IDENTITY-LOCKED) — The abstract collective good of Yiddish literary tradition, folk culture, and diasporic identity. Identity-locked rather than materially trapped: Yiddish speakers could learn Hebrew, but doing so requires abandoning the identity constituted through Yiddish (the mame-loshn, the Ashkenazi cultural world, the counter-Zionist or non-Zionist diaspora frameworks). Experiences the constraint as pure extraction with no coordination benefit — the 'shared language' does not preserve what Yiddish carried.
constraint_indexing:constraint_classification(vernacular_displacement, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 5: HAREDI COMMUNITY (TANGLED ROPE) — Organized resistance with constrained exit. Maintained Yiddish as internal vernacular while participating in Hebrew civic life, creating a parallel linguistic economy. Benefits from Hebrew coordination (state services, economic participation) while bearing costs (cultural pressure, educational policy conflicts, stigmatization of Yiddish maintenance). The constraint is both coordination mechanism and extraction — the community's organization allows partial exit but not full escape.
constraint_indexing:constraint_classification(vernacular_displacement, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: DIASPORA YIDDISH REVIVAL MOVEMENT (SCAFFOLD) — Organized agents outside Israel (YIVO, Yiddish cultural organizations, academic programs) see the constraint as temporary. The initial suppression was a contingent political choice, not a permanent linguistic law. As Hebrew stabilizes and the state-building emergency recedes, space opens for Yiddish cultural revival without threatening national cohesion. Sunset logic: the constraint's justification (linguistic unity for state survival) weakens as the state matures. Low effective extraction due to geographic exit and organizational capacity.
constraint_indexing:constraint_classification(vernacular_displacement, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Recognizes both the genuine coordination function (immigrant linguistic integration, cross-community communication, state-building in a fragmented population) and the asymmetric extraction (costs borne by Yiddish speakers, cultural erasure, loss of diasporic linguistic diversity). The constraint required active enforcement (school policy, employment requirements, social stigma campaigns) and produced identifiable victims. Not a mountain (language shift is not a natural law) and not pure rope (the costs were not symmetrically distributed). Tangled rope: coordination and extraction are structurally inseparable.
constraint_indexing:constraint_classification(vernacular_displacement, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

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

:- end_tests(vernacular_displacement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): Substantial. The coordination function (immigrant linguistic integration) was real, but the costs were asymmetrically distributed. Yiddish speakers bore cultural erasure, loss of literary tradition, and severance from diasporic identity. The extraction increased through the state-building period as enforcement intensified, peaked in the early statehood years (0.72 at T=30), then declined as Hebrew became demographically dominant and active suppression became less necessary. The final value (0.62) reflects that extraction persists through internalized stigma and institutional path dependency even after peak enforcement. Suppression (0.73): High. Institutional penalties (school policy requiring Hebrew, employment discrimination, civic participation barriers) combined with social stigma campaigns. Suppression trajectory mirrors extractiveness: rose through the state-building period (0.82 at T=30), then declined as Hebrew dominance became self-sustaining. The final value reflects that suppression remains substantial through path dependency and internalized norms, even though active enforcement has weakened. Theater ratio (0.41): Moderate. The coordination justification (linguistic unity for state survival) was genuine during the fragile state-building period but became increasingly performative as Hebrew dominance was secured. By the 1960s-1970s, anti-Yiddish stigma persisted beyond functional necessity — the 'threat' to Hebrew was theatrical. Theater ratio rises over the interval (0.25 to 0.45, then stabilizes at 0.41) as the gap between claimed justification and actual necessity widened.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates extreme perspectival divergence. The language planning authority (institutional/arbitrage) experiences pure coordination (rope): Hebrew revival solves the genuine problem of linguistic fragmentation, and Yiddish displacement is invisible or rationalized as necessary modernization cost. The Yiddish-speaking immigrant (powerless/trapped) experiences pure extraction (snare): language shift is imposed through institutional penalties and social stigma with no exit option except abandoning the Zionist project entirely. The bilingual educator (moderate/constrained) experiences mixed coordination and extraction (tangled rope): benefits from Hebrew's cross-community communication function while bearing professional costs of Yiddish suppression. The Haredi community (organized/constrained) also sees tangled rope but with partial exit through parallel institutions. The diaspora revival movement (organized/mobile) sees scaffold: the constraint is temporary, justified only by state-building emergency, with sunset logic as Hebrew stabilizes. The analytical observer recognizes the constraint as tangled rope at the civilizational scale: genuine coordination function inseparable from asymmetric extraction. The gap between rope (beneficiary view) and snare (victim view) is the core measurement — the same structural phenomenon appears as solution or oppression depending on position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. The hebrew_monolingualism_project and state_building_apparatus are declared beneficiaries — they capture institutional authority, cultural prestige, and state resources from Hebrew's success. The language planning authority (institutional/arbitrage) is a beneficiary with maximum exit options, producing low d and low/negative chi (experiences the constraint as coordination). The yiddish_cultural_continuity and yiddish_speaking_communities are declared victims — they bear the costs of language shift (cultural erasure, loss of literary tradition, severance from diasporic identity). The Yiddish-speaking immigrant (powerless/trapped) is a victim with zero exit options, producing high d and maximum chi (experiences pure extraction). The bilingual educator (moderate/constrained) is in both beneficiary and victim sets (benefits from Hebrew coordination, bears costs of Yiddish suppression), producing intermediate d and moderate chi (tangled rope experience). The Haredi community (organized/constrained) is primarily victim but with organizational capacity that reduces effective extraction. The diaspora revival movement (organized/mobile) is victim by solidarity but with geographic exit that produces low chi (scaffold experience). No directionality overrides are needed — the structural derivation from beneficiary/victim + exit captures the actual relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that coordination and extraction are structurally inseparable in language shift contexts. The coordination function (Hebrew enabled communication across dozens of immigrant linguistic backgrounds) was genuine — this was not pure theater. But the costs were asymmetrically distributed: Yiddish speakers bore cultural erasure while Hebrew monolingualism advocates captured institutional authority. The tangled rope classification (from the analytical perspective) reflects this inseparability: you cannot have the coordination benefit without the extraction cost in this structural configuration. The snare classification (from the victim perspective) is also valid — from the position of the Yiddish-speaking immigrant with no exit, the coordination story is cover for cultural erasure. Both classifications are true from their respective positions. The mandatrophy is resolved by recognizing that 'coordination' and 'extraction' are not mutually exclusive categories but perspectival readings of the same structural flow. The constraint is coordination from the beneficiary seat and extraction from the victim seat, and both readings are empirically grounded.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    monolingualism_necessity,
    'Was Hebrew monolingualism structurally necessary for Israeli state-building, or was it a contingent policy choice that could have accommodated Yiddish-Hebrew bilingualism?',
    'Comparative analysis of multilingual state-building projects (Switzerland, Belgium, India, Singapore); counterfactual modeling of Hebrew-Yiddish official bilingualism; examination of Haredi community''s parallel linguistic economy as existence proof of bilingual viability',
    'If structurally necessary: constraint reclassifies toward rope (coordination with unavoidable costs). If contingent choice: constraint remains snare (extraction rationalized by false necessity claim).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(monolingualism_necessity, empirical, 'Whether monolingualism was necessary or contingent for state-building').

omega_variable(
    yiddish_vitality_counterfactual,
    'Would Yiddish have declined in Israel even without active suppression, due to demographic mixing and economic incentives for Hebrew adoption?',
    'Comparison with Yiddish decline trajectories in other immigrant destinations (USA, Argentina, South Africa) where no state suppression occurred; analysis of Haredi community maintenance as counterfactual; examination of language shift timelines in suppression vs non-suppression contexts',
    'If decline was inevitable: suppression was theater (piton classification for enforcement apparatus). If suppression was causal: extraction was real and substantial (snare classification confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(yiddish_vitality_counterfactual, empirical, 'Whether Yiddish decline required active suppression or would have occurred naturally').

omega_variable(
    cultural_loss_measurement,
    'How do we measure the cultural loss from Yiddish displacement against the coordination gains from Hebrew unification?',
    'No resolution mechanism — this is a values question about incommensurable goods. Different frameworks (nationalist, diasporist, liberal-pluralist, linguistic-diversity) produce different weightings.',
    'Affects whether the constraint''s net effect is classified as coordination-with-costs (tangled rope) or extraction-with-coordination-cover (snare). The classification depends on the observer''s normative framework for weighing cultural preservation against state-building.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cultural_loss_measurement, preference, 'Incommensurability of cultural loss vs coordination gains').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (institutional penalties, resource denial) or internalized (Yiddish speakers adopted Hebrew-supremacy framing)?',
    'Post-migration suppression trajectory: if Yiddish maintenance remained stigmatized after institutional penalties were removed (1980s-present), reclassify as partially internalized. Examination of second-generation language attitudes: did stigma persist beyond policy enforcement?',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — targets carry the suppression with them after policy changes. If purely structural, suppression should decline as enforcement weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Whether suppression was structural or internalized').

omega_variable(
    cs_framing_under_determination,
    'Is the kernel ''Hebrew as living language'' or ''Hebrew as national vernacular''? The native-daily reading could be framed as either: (1) a reading of the ''living language'' kernel that forecloses the liturgical reading, or (2) a distinct kernel (''national vernacular'') that coexists with the liturgical kernel but serves a different commitment.',
    'Historical discourse analysis: did Zionist language planners present Hebrew revival as redefining what ''living'' means (reading of existing kernel) or as a new commitment orthogonal to liturgical use (new kernel)? Examination of whether the two commitments were held simultaneously by the same actors or by different communities.',
    'Framing (1) produces a forecloses relation and axiom contradiction (symbolic vs generative definitions of ''living''). Framing (2) produces coexists_with relation and no axiom contradiction (liturgical and vernacular are separate commitments). The choice affects cs_structure.reading_relations and axiom status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_under_determination, conceptual, 'Whether native-daily reading is a reading of ''living language'' kernel or a distinct kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vernacular_displacement, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vern_disp_theater_1920, vernacular_displacement, theater_ratio, 0, 0.25).
narrative_ontology:measurement(vern_disp_theater_1930, vernacular_displacement, theater_ratio, 10, 0.32).
narrative_ontology:measurement(vern_disp_theater_1940, vernacular_displacement, theater_ratio, 20, 0.38).
narrative_ontology:measurement(vern_disp_theater_1950, vernacular_displacement, theater_ratio, 30, 0.45).
narrative_ontology:measurement(vern_disp_theater_1960, vernacular_displacement, theater_ratio, 40, 0.43).
narrative_ontology:measurement(vern_disp_theater_1970, vernacular_displacement, theater_ratio, 50, 0.41).

% Extraction over time
narrative_ontology:measurement(vern_disp_extract_1920, vernacular_displacement, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(vern_disp_extract_1930, vernacular_displacement, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(vern_disp_extract_1940, vernacular_displacement, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(vern_disp_extract_1950, vernacular_displacement, base_extractiveness, 30, 0.72).
narrative_ontology:measurement(vern_disp_extract_1960, vernacular_displacement, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(vern_disp_extract_1970, vernacular_displacement, base_extractiveness, 50, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(vern_disp_suppress_1920, vernacular_displacement, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(vern_disp_suppress_1930, vernacular_displacement, suppression_requirement, 10, 0.63).
narrative_ontology:measurement(vern_disp_suppress_1940, vernacular_displacement, suppression_requirement, 20, 0.78).
narrative_ontology:measurement(vern_disp_suppress_1950, vernacular_displacement, suppression_requirement, 30, 0.82).
narrative_ontology:measurement(vern_disp_suppress_1960, vernacular_displacement, suppression_requirement, 40, 0.71).
narrative_ontology:measurement(vern_disp_suppress_1970, vernacular_displacement, suppression_requirement, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vernacular_displacement, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is downstream of native_daily_reading (the commitment that Hebrew was dormant until vernacular reconstruction). The upstream constraint has its own extractiveness reflecting the costs of the revival project itself; this constraint has its own extractiveness reflecting the specific costs of Yiddish displacement. They are structurally distinct: native_daily_reading could have succeeded without suppressing Yiddish (through bilingual policy), but the actual historical implementation chose monolingualism. The vernacular_displacement constraint is the cost of that choice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
