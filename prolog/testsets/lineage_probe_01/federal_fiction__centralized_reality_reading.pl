% ============================================================================
% CONSTRAINT STORY: federal_fiction__centralized_reality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federal_fiction__centralized_reality_reading, []).

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
 *   constraint_id: federal_fiction__centralized_reality_reading
 *   human_readable: Federal Fiction / Centralized Reality: One Pipe, One Plan, Decorative Borders
 *   domain: legal/political/doctrinal
 *
 * SUMMARY:
 *   The Soviet federal system presents a canonical instance of decorative
 *   institutional form masking unitary command control. The USSR was
 *   constituted as a Union of fifteen Soviet Socialist Republics, each with
 *   nominal sovereignty, territorial integrity, and a constitutional right of
 *   secession (Article 17). Yet behind this federal map ran a single
 *   administrative apparatus: all-union ministries hierarchically organized
 *   by industrial sector (coal, steel, chemicals, agriculture), answering to
 *   Moscow; a unified central plan allocating resources without regard for
 *   republic-level autonomy; and a Communist Party structure that was
 *   centralized vertically, not federal. From the perspective of the
 *   centralized reality reading, the federation was pure theater—decorative
 *   borders that conveyed legitimacy to Moscow's control while preventing the
 *   coordination costs of overt conquest. The republic-level cadres,
 *   nominally governors of sovereign republics, were executing Moscow's
 *   orders. Republic economies were specialized in all-union plan roles
 *   (Ukraine for steel and coal, Kazakhstan for minerals, Central Asia for
 *   cotton) without control over their own resource allocation. National
 *   movements claiming the secession right faced suppression as
 *   counter-revolutionary agitation. This constraint story instantiates ONE
 *   reading of the contested kernel 'federal_fiction'—the reading that
 *   emphasizes how the federal form enabled extraction and suppression while
 *   providing legitimacy. Other readings (nationality_form_reading,
 *   secession_dead_letter_reading) see different structural features of the
 *   same institutional arrangement.
 *
 * KEY AGENTS:
 *   - Central Moscow Apparatus and All-Union Ministries: Primary beneficiary (institutional/arbitrage) — controls resource allocation, implements central plan, extracts surplus from republics without managing local resistance
 *   - Communist Party Apparatus: Secondary beneficiary (institutional/arbitrage) — maintains vertical Party discipline that bypasses federal structure; uses theater to avoid exposing unitary reality
 *   - Republic-Level Cadres and Economic Managers: Primary victim (powerless/trapped) — formally sovereign but functionally controlled; execute Moscow orders without autonomy
 *   - Republic-Level National Movements and Cultural Elites: Secondary victim (organized/constrained) — benefit from federal form's identity institutions (language, national cadres) but suffer suppression of actual autonomy and nationalist expression
 *   - Larger Republics (Ukraine, Byelorussia): Tertiary actor (powerful/constrained) — more institutional weight in all-union system; negotiate within constraints rather than experience pure suppression
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional choice as structural necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federal_fiction__centralized_reality_reading, 0.68).
domain_priors:suppression_score(federal_fiction__centralized_reality_reading, 0.82).
domain_priors:theater_ratio(federal_fiction__centralized_reality_reading, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federal_fiction__centralized_reality_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(federal_fiction__centralized_reality_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(federal_fiction__centralized_reality_reading, theater_ratio, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federal_fiction__centralized_reality_reading, snare).
narrative_ontology:human_readable(federal_fiction__centralized_reality_reading, "Federal Fiction / Centralized Reality: One Pipe, One Plan, Decorative Borders").
narrative_ontology:topic_domain(federal_fiction__centralized_reality_reading, "legal/political/doctrinal").

domain_priors:requires_active_enforcement(federal_fiction__centralized_reality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federal_fiction__centralized_reality_reading, 'ae2ef53a-0ec7-467b-9d7d-955af575d550').
narrative_ontology:cs_kernel_codification('ae2ef53a-0ec7-467b-9d7d-955af575d550', formalized).
narrative_ontology:cs_authority_grounding('ae2ef53a-0ec7-467b-9d7d-955af575d550', extraction).
narrative_ontology:cs_interpretation_layer_present('ae2ef53a-0ec7-467b-9d7d-955af575d550').
narrative_ontology:cs_reading_relation('ae2ef53a-0ec7-467b-9d7d-955af575d550', federal_fiction__nationality_form_reading, coexists_with).
narrative_ontology:cs_reading_relation('ae2ef53a-0ec7-467b-9d7d-955af575d550', federal_fiction__secession_dead_letter_reading, influences).
narrative_ontology:cs_axiom('ae2ef53a-0ec7-467b-9d7d-955af575d550', foundational, federation_is_decorative_theater).
narrative_ontology:cs_axiom_status(federation_is_decorative_theater, holdable).
narrative_ontology:cs_axiom_grounding('ae2ef53a-0ec7-467b-9d7d-955af575d550', federation_is_decorative_theater, empirically_contingent).
narrative_ontology:cs_axiom('ae2ef53a-0ec7-467b-9d7d-955af575d550', foundational, central_apparatus_extracts_via_institutional_mediation).
narrative_ontology:cs_axiom_status(central_apparatus_extracts_via_institutional_mediation, holdable).
narrative_ontology:cs_axiom_grounding('ae2ef53a-0ec7-467b-9d7d-955af575d550', central_apparatus_extracts_via_institutional_mediation, empirically_contingent).
narrative_ontology:cs_reference_frame('ae2ef53a-0ec7-467b-9d7d-955af575d550', unitary_command_economy_framework).
narrative_ontology:cs_drift_state('ae2ef53a-0ec7-467b-9d7d-955af575d550', late_soviet_period, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ae2ef53a-0ec7-467b-9d7d-955af575d550', '').
narrative_ontology:cs_kernel_id(federal_fiction__centralized_reality_reading, federal_fiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federal_fiction__centralized_reality_reading, central_moscow_apparatus).
narrative_ontology:constraint_beneficiary(federal_fiction__centralized_reality_reading, all_union_ministries).
narrative_ontology:constraint_beneficiary(federal_fiction__centralized_reality_reading, communist_party_apparatus).
narrative_ontology:constraint_victim(federal_fiction__centralized_reality_reading, republic_level_autonomy).
narrative_ontology:constraint_victim(federal_fiction__centralized_reality_reading, republic_level_priority_setting).
narrative_ontology:constraint_victim(federal_fiction__centralized_reality_reading, national_cadre_independence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REPUBLIC-LEVEL CADRE AND ECONOMY (SNARE) — Trapped within the federal structure with no exit capacity. The republic's economy, governance, and national cadre are formally sovereign but functionally controlled by all-union ministries and the central plan. Extraction is maximal: the appearance of autonomy (decorative borders, republican constitutions, titular languages) provides legitimacy to Moscow's control, but the cadre cannot exercise genuine economic decision-making. The theater of federalism IS the suppression mechanism.
constraint_indexing:constraint_classification(federal_fiction__centralized_reality_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: REPUBLIC-BASED NATIONAL MOVEMENTS / ORGANIZED OPPOSITION (TANGLED ROPE) — Organized actors within the republic (cultural elites, national cadres, nationalist intellectuals) benefit from some coordination functions the federal form provides: language standardization, cultural institutions, titular national status in law. But they also experience severe extraction: their priorities are overridden by the central plan, national movements are suppressed as 'bourgeois nationalism,' and advocating secession rights becomes counter-revolutionary agitation. The constraint provides real but severely constrained coordination alongside asymmetric extraction.
constraint_indexing:constraint_classification(federal_fiction__centralized_reality_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: CENTRAL MOSCOW APPARATUS / ALL-UNION MINISTRIES (ROPE) — Benefits directly from the federal fiction: the central plan is implemented through the decorative federal structure without needing to maintain expensive repressive apparatus on the ground. The ministries coordinate economic allocation without republics having autonomy to resist. The federal form enables 'soft' coordination — the ministry issues orders through the republic's formal government, which carries legitimacy through the national form. Low experienced extraction because the beneficiary perceives the constraint as coordinating their essential function.
constraint_indexing:constraint_classification(federal_fiction__centralized_reality_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: COMMUNIST PARTY APPARATUS (PITON) — The Party apparatus maintains the federal fiction through increasingly theatrical performance. By the 1980s, the federal form is substantially degraded: republic-level party secretaries are Moscow appointees, the All-Union Congress of Soviets is a rubber stamp, and the only active political structure is the Party hierarchy (which is centralized, not federal). The Party sees its own federal apparatus as inert — it persists through institutional momentum and because dismantling it would expose the unitary reality too directly. Theater ratio is very high because the federal theater masks the real locus of power (the Party center).
constraint_indexing:constraint_classification(federal_fiction__centralized_reality_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universalizing analytical perspective, federal forms always require a unitary administrative apparatus to function at continental scale. The appearance of federation is inevitable theater: coordinating 15 republics without a central command pipeline would require complex negotiation, and the cost-benefit analysis favors centralized hierarchy. This perspective sees the federal fiction as structurally necessary, not contingent — an immutable property of managing empire at scale. However, this reading naturalizes what is actually a political choice enforced by the Party. The engine will detect this as a false summit.
constraint_indexing:constraint_classification(federal_fiction__centralized_reality_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: UKRAINE AND SLAVIC REPUBLICS / NEGOTIATING MINORITIES (TANGLED ROPE) — The larger republics (Ukraine, Byelorussia) have more institutional weight in the all-union ministry system and can negotiate for resource allocation and cadre placement. They experience the constraint as mixed: genuine coordination of all-union economic priorities occurs through ministries where they have representatives, but their autonomy is still suppressed and their specific national interests are overridden. Less severe extraction than smaller republics because they have more organizational power to absorb or negotiate within the system.
constraint_indexing:constraint_classification(federal_fiction__centralized_reality_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federal_fiction__centralized_reality_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(federal_fiction__centralized_reality_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(federal_fiction__centralized_reality_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(federal_fiction__centralized_reality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(federal_fiction__centralized_reality_reading, TR),
    TR >= 0.70.

:- end_tests(federal_fiction__centralized_reality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The central plan systematically allocates republic resources without republic consent, and republic-level actors cannot refuse participation or reallocate their economies toward local priorities. This is extraction—Moscow's apparatus benefits from resource flows and labor specialization that republic populations could have directed differently. The value reflects strong asymmetry: republics bear the costs of all-union priorities (Central Asian cotton monoculture, Siberian resource extraction, Ukrainian coal dependency) while the center captures the allocation benefit and political control. The 68% value (rather than the maximum 0.90+) reflects that the extraction is not achieved through raw violence but through institutional mediation that provides some secondary benefits to republic actors (cadre positions, industrial development) and maintains the illusion of negotiation within the system. Suppression (0.82): High. Multiple mechanisms suppress exit and autonomy: Article 17 secession right is a dead letter (advocating its use is counter-revolutionary); national movements are suppressed as bourgeois nationalism; republic governments are Moscow-appointed or Moscow-controlled; the Party apparatus is centralized not federal; repression targets attempts at economic or political independence. The suppression is structural (embedded in the institutional form) and ideological (enforcement through Party discipline and law). Theater ratio (0.85): Very high and increasing over the 20-year measurement interval. Early in the period (1950s-1960s), the federal form retained some functional role—republic-level planning commissions had input, cultural autonomy in non-political domains was tolerated, and cadre advancement could operate through republic-level positions. By the late 1970s-1980s, the federal structure is almost entirely theater: the All-Union Congress of Soviets is a rubber-stamp body, republic legislatures pass pre-written laws, and all real authority runs through the Party hierarchy. The rising theater_ratio reflects that the extraction mechanism was increasingly exposed and required more theatrical maintenance—more elaborate ceremonies of federal decision-making, more prominent display of republic institutions, more elaborate justification in doctrinal texts. The engine's measurement system flags this as a sign of a constraint approaching degradation (Piton territory) or undergoing intensification of suppression to maintain the fiction.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates radical perspectival divergence driven by position within the extraction system. The central apparatus sees coordination (Rope)—the federal form enables efficient dispatch of economic commands. The republic-level victim sees pure extraction (Snare)—their economy is looted without consent. Organized republic-level actors see mixed coordination and extraction (Tangled Rope)—the federal form provides identity institutions (language, nationality, cadre positions) that they value, but these are purchased at the cost of economic autonomy. The Communist Party apparatus sees degraded theater (Piton)—by the 1980s, the federal machinery is so elaborately performative that Party insiders treat it as spectacle. The analytical observer from a civilizational view risks seeing a natural law (Mountain)—empires must centralize, federal forms are structurally theatrical. The engine's false summit detection should trigger on the mountain classification because beneficiaries (central apparatus, Party) are explicitly declared.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's structural relationship to this specific constraint. Central apparatus: beneficiary + arbitrage exit = very low d (negative f(d)) = they experience minimal extraction. Republic cadres: victim + trapped exit = very high d (f(d) ≈ 1.42) = maximum experienced extractiveness. Organized national movements: victim + constrained exit (can organize within limits but cannot exit) = high-moderate d = moderate-to-high experienced extraction. Party apparatus: beneficiary + arbitrage = low d but the perspective is degraded (Piton) because the beneficiary's own functional purpose has atrophied. The gap in d values across perspectives is pronounced because the same structural constraint produces radically different exit capacities for different agents.
 *
 * MANDATROPHY ANALYSIS:
 *   COMMITTER FRAME: The mandatrophy in this domain is resolved by recognizing that the federal fiction is ONE READING of the kernel 'federal_fiction'. The mandatrophy question is not 'was the federation real or fake?' but 'which aspect of the federal structure do you emphasize in your reading of it?' This reading (centralized_reality) emphasizes extraction and theater. The nationality_form_reading would emphasize identity-constituting functions. The secession_dead_letter_reading would emphasize the paradoxical legal status of the exit right. All three are defensible readings of the same institutional structure, grounded in different evidentiary records and different normative priorities. The analytical perspective's mountain classification is a false summit—it is not a natural law that federations are theater, but a perspectival choice that follows from this particular reading of the kernel. The engine should detect and flag the false summit (beneficiaries declared, mountain claimed) and route the disagreement to the omega variables, where the committer-frame ambiguity is documented.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    federal_form_functionality,
    'Did the federal form perform any genuine coordinative function that could not have been achieved through explicit centralization?',
    'Counterfactual analysis of administrative efficiency with vs. without federal structure; historical comparison to non-federal multi-ethnic empires (Ottoman, Chinese); examination of resource allocation patterns to determine if federal layer added information or legitimacy value',
    'If no genuine function: confirms Snare classification across most perspectives. If modest function: suggests Tangled Rope (coordination + extraction) at republic level. If substantial function: would require upward revision of beneficiary experiences and potential reclassification to Rope for republic-level actors.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(federal_form_functionality, empirical, 'Whether federal structure provided genuine coordinative value beyond theatrical legitimacy').

omega_variable(
    extraction_measurement_ambiguity,
    'How much of the republic-level economic cost was extraction (resources transferred to center that could have gone to republic) versus coordination overhead (genuine administrative cost of the all-union system)?',
    'Input-output accounting of resource flows: inter-republic transfers, all-union ministry budgets, investment allocation by republic; comparison of republic economic growth rates with counterfactual single-nation development paths',
    'If most cost is overhead: extractiveness should be reduced to 0.40-0.45 (Tangled Rope). If most cost is extraction: extractiveness confirmed at 0.68+ (Snare). Measurement determines whether the central apparatus was optimizing for all-union goals or extracting surplus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_measurement_ambiguity, empirical, 'Proportion of republic economic cost that was extraction vs. coordination overhead').

omega_variable(
    federated_form_enabling_secession,
    'Would the constitutional form of federation (with Article 17 secession right) have remained intellectually available for deployment if the actual practice were not so clearly suppressive?',
    'Historical counterfactual: If all-union ministries had permitted republic economic autonomy in limited domains (e.g., light industry, consumer goods, agriculture marketing), would the secession right have remained a dead letter or become a potential negotiating lever? Examination of periods (1920s NEP) when republic autonomy increased and secession rhetoric.',
    'If the form could have enabled exit with different policies: the constraint is contingent political choice (Snare/Tangled Rope). If the form is inherently theater regardless of substance: the constraint is structural (validates Mountain perspective but as false summit). The omega identifies whether the federal reading depends on centralized suppression or would persist under any federal structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federated_form_enabling_secession, conceptual, 'Whether federal form would enable exit if suppression were reduced').

omega_variable(
    reading_committer_frame_natural_law,
    'Is this reading grounding the federal fiction in a natural law claim about empire (empires must centralize, federal forms are theater), or in a contingent institutional choice that could be reversed?',
    'The analytical observer''s perspective frames centralization as structurally necessary — but this is a committer-axis choice, not an empirical fact. If the reading claims natural law (mountain perspective valid), the burden is to show no alternative to centralization exists. If the reading describes contingent institutional design, the analytic is tangled_rope/snare. The engine detects false summits; this omega documents the analytical position''s own methodological choice.',
    'False summit detection: if beneficiaries exist (confirmed: central apparatus), the mountain classification should trigger FSM reclassification. This omega documents that the natural law framing is a perspectival choice available to the analytical observer, not a structural necessity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_committer_frame_natural_law, conceptual, 'Natural law vs. contingent choice framing in the analytical mountain perspective').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federal_fiction__centralized_reality_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fed_central_tr_t0, federal_fiction__centralized_reality_reading, theater_ratio, 0, 0.68).
narrative_ontology:measurement(fed_central_tr_t10, federal_fiction__centralized_reality_reading, theater_ratio, 10, 0.78).
narrative_ontology:measurement(fed_central_tr_t20, federal_fiction__centralized_reality_reading, theater_ratio, 20, 0.85).

% Extraction over time
narrative_ontology:measurement(fed_central_be_t0, federal_fiction__centralized_reality_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(fed_central_be_t10, federal_fiction__centralized_reality_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(fed_central_be_t20, federal_fiction__centralized_reality_reading, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fed_central_su_t0, federal_fiction__centralized_reality_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(fed_central_su_t10, federal_fiction__centralized_reality_reading, suppression_requirement, 10, 0.78).
narrative_ontology:measurement(fed_central_su_t20, federal_fiction__centralized_reality_reading, suppression_requirement, 20, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federal_fiction__centralized_reality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(federal_fiction__centralized_reality_reading, federal_fiction__nationality_form_reading).
narrative_ontology:affects_constraint(federal_fiction__centralized_reality_reading, federal_fiction__secession_dead_letter_reading).
narrative_ontology:affects_constraint(federal_fiction__centralized_reality_reading, article_seventeen_secession_right).
narrative_ontology:affects_constraint(federal_fiction__centralized_reality_reading, all_union_ministry_command_hierarchy).
narrative_ontology:affects_constraint(federal_fiction__centralized_reality_reading, communist_party_centralism).

% DUAL FORMULATION NOTE:
% The federal_fiction kernel decomposes into at least three distinct constraint stories, one per reading. This story (centralized_reality_reading) models the federation as extractive theater; ε=0.68, Snare from republic perspective. The nationality_form_reading would model federation as identity-constitutive, likely lower ε and Tangled Rope. The secession_dead_letter_reading would model the paradox of a nominal right whose invocation is a crime, likely different ε and different victim set. Each reading has its own ε-invariant structure and should be authored separately. They are linked via network.affects_constraints to show family relationships.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
