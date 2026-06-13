% ============================================================================
% CONSTRAINT STORY: jewish_territorial_claim__political_zionism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_territorial_claim__political_zionism_reading, []).

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
 *   constraint_id: jewish_territorial_claim__political_zionism_reading
 *   human_readable: Political Zionism: Jewish Statehood Through Territorial Sovereignty and Demographic Majority
 *   domain: political/historical/settler-colonial
 *
 * SUMMARY:
 *   Political Zionism as a movement grounds Jewish statehood in the necessity
 *   of territorial sovereignty as the sole solution to European antisemitism
 *   and the historical 'Jewish Question.' This reading prioritizes
 *   state-building and demographic majority over cultural or spiritual
 *   renewal (distinguishing it from cultural zionism) and frames Arab
 *   residents of Palestine as a demographic obstacle rather than as a
 *   population with parallel claims. The constraint requires continuous
 *   enforcement: immigration law favoring Jews, land-purchase and legal
 *   frameworks preventing Arab property acquisition, settlement expansion
 *   into territory inhabited by Arabs, and military suppression of
 *   resistance. The political zionist reading treats the 'transfer' of Arab
 *   population (through displacement or exclusion) as a necessary mechanism
 *   to achieve the Jewish majority that makes statehood viable and secure.
 *   This is a kernel reading: the contested kernel is the Jewish territorial
 *   claim in Palestine; this reading instantiates one coherent framing (state
 *   first, majority necessary, transfer acceptable); sibling readings
 *   (cultural, labor, revisionist) instantiate different priorities and
 *   mechanisms within the same territorial and historical field.
 *
 * KEY AGENTS:
 *   - jewish_european_diaspora_persecuted: powerless, trapped, seeking refuge and security through sovereign statehood; experience Palestinian presence as obstacle
 *   - zionist_state_apparatus: institutional, agenda-setter, enforces Jewish demographic majority through immigration law, settlement, legal redefinition of property
 *   - palestinian_arab_residents: powerless, trapped, bear direct cost of land expropriation, displacement, military occupation, political exclusion
 *   - palestinian_arab_diaspora: powerless, trapped, barred from return; excluded from conversation despite bearing consequences
 *   - british_mandate_authority: institutional, administered the territory but failed to enforce Arab rights protections; enabled zionist infrastructure-building
 *   - liberal_zionist_revisors: moderate, structurally excluded from political zionist agenda; envision binational state or cultural autonomy
 *   - international_powers_endorsing: powerful, benefit from Jewish state as strategic ally and solution to European 'Jewish Question'
 *   - arab_nationalist_states: powerful, excluded from zionist agenda; oppose on grounds of Palestinian self-determination
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__political_zionism_reading, 0.78).
domain_priors:suppression_score(jewish_territorial_claim__political_zionism_reading, 0.81).
domain_priors:theater_ratio(jewish_territorial_claim__political_zionism_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__political_zionism_reading, tangled_rope).
narrative_ontology:human_readable(jewish_territorial_claim__political_zionism_reading, "Political Zionism: Jewish Statehood Through Territorial Sovereignty and Demographic Majority").
narrative_ontology:topic_domain(jewish_territorial_claim__political_zionism_reading, "political/historical/settler-colonial").

domain_priors:requires_active_enforcement(jewish_territorial_claim__political_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__political_zionism_reading, '56e58001-4771-46a9-8b55-e54bc0be8f7a').
narrative_ontology:cs_kernel_codification('56e58001-4771-46a9-8b55-e54bc0be8f7a', formalized).
narrative_ontology:cs_authority_grounding('56e58001-4771-46a9-8b55-e54bc0be8f7a', extraction).
narrative_ontology:cs_interpretation_layer_present('56e58001-4771-46a9-8b55-e54bc0be8f7a').
narrative_ontology:cs_reading_relation('56e58001-4771-46a9-8b55-e54bc0be8f7a', jewish_territorial_claim__cultural_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('56e58001-4771-46a9-8b55-e54bc0be8f7a', jewish_territorial_claim__labor_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('56e58001-4771-46a9-8b55-e54bc0be8f7a', jewish_territorial_claim__revisionist_zionism_reading, influences).
narrative_ontology:cs_axiom('56e58001-4771-46a9-8b55-e54bc0be8f7a', foundational, jewish_statehood_requires_demographic_majority).
narrative_ontology:cs_axiom_status(jewish_statehood_requires_demographic_majority, holdable).
narrative_ontology:cs_axiom_grounding('56e58001-4771-46a9-8b55-e54bc0be8f7a', jewish_statehood_requires_demographic_majority, empirically_contingent).
narrative_ontology:cs_axiom('56e58001-4771-46a9-8b55-e54bc0be8f7a', foundational, arab_population_obstacle_to_jewish_security).
narrative_ontology:cs_axiom_status(arab_population_obstacle_to_jewish_security, holdable).
narrative_ontology:cs_axiom_grounding('56e58001-4771-46a9-8b55-e54bc0be8f7a', arab_population_obstacle_to_jewish_security, empirically_contingent).
narrative_ontology:cs_reference_frame('56e58001-4771-46a9-8b55-e54bc0be8f7a', jewish_diaspora_persecuted_european).
narrative_ontology:cs_drift_state('56e58001-4771-46a9-8b55-e54bc0be8f7a', established_state_1950, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('56e58001-4771-46a9-8b55-e54bc0be8f7a', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__political_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, jewish_european_diaspora_persecuted).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, zionist_state_apparatus).
narrative_ontology:constraint_victim(jewish_territorial_claim__political_zionism_reading, palestinian_arab_residents).
narrative_ontology:constraint_victim(jewish_territorial_claim__political_zionism_reading, palestinian_arab_diaspora).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__political_zionism_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(jewish_territorial_claim__political_zionism_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_territorial_claim__political_zionism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_territorial_claim__political_zionism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_territorial_claim__political_zionism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.15 (1880, early settlement phase with minimal institutional power) to 0.78 (1950, established state with legal monopoly on territory and demographic control). The rise tracks the constraint's capacity to enforce: as zionist institutions consolidated, as the Mandate enabled legal and administrative infrastructure, as immigration accelerated post-Holocaust, the mechanism shifted from voluntary settlement and land purchase to legally codified exclusion and state apparatus control. Suppression requirement rises similarly (0.2 to 0.81), indicating that the constraint's persistence increasingly depends on active enforcement: Palestinian resistance grows as displacement intensifies; suppression must harden to maintain the demographic balance. Theater ratio rises more slowly (0.05 to 0.42), reflecting that the coordination function (refugee absorption, state-building) remains partly genuine even as extraction intensifies—but the performative dimension grows as the constraint's stated purpose (Jewish security) becomes decoupled from its mechanism (permanent occupation and demographic dominance). The coercion grid shows the suppression and stakes inflation hitting hardest at the structural level (system-level arrangement of territory and population), with individual and class resistance growing as the consequences of displacement and occupation accumulate. All measurements are authored on a single time grid (1880, 1900, 1920, 1935, 1945, 1950) so every metric is comparable across the interval.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (jewish_european_diaspora) and the agenda-setter seat (zionist_state_apparatus) should compute differently from the payer seats (palestinian_arab_residents, diaspora). From the diaspora's perspective, the constraint is a genuine solution to existential threat—the coordination function is real, the extraction (loss of European home and status) feels preferable to statelessness and persecution, and the transfer is experienced as justified escape. From the zionist state's perspective, the constraint is both coordination (building a viable state) and power-consolidation (controlling territory and demographics); the apparatus benefits from both the coordination and the extraction. From the Palestinian payer seats, the constraint is pure extraction: they bear the direct cost (dispossession, displacement, occupation) without participation in the agenda or benefit from the state apparatus. The engine's per-seat computation will likely show the diaspora and state apparatus seats computing as rope or tangled_rope (genuine coordination with asymmetric benefits), while Palestinian seats compute as snare (coercion masquerading as coordination they never agreed to). This divergence is the story—it is not a defect in the classification.
 *
 * DIRECTIONALITY LOGIC:
 *   The jewish_european_diaspora_persecuted sits near d=0.3 (beneficiary): they receive refuge, security, and statehood from the constraint's operation; they bear a cost (losing European homes, displacement to new territory) but frame it as preferable to persecution. The directionality is beneficiary-leaning because they are not trapped targets of extraction—they are voluntary participants seeking escape. However, the power atom is 'powerless' and time_horizon is generational, which moderates the beneficiary directionality slightly: they cannot exit the constraint once established and depend on institutions they do not fully control. The zionist_state_apparatus sits near d=0.1 (full beneficiary): it collects extraction (state power, territory, demographic control), sets the agenda, has arbitrage-level exit (can admit or exclude populations by law). The palestinian_arab_residents sit near d=0.95 (full target): they bear the direct extraction (land, home, freedom), are powerless and trapped, have no seat at the agenda-setting table. Their directionality is pulled fully toward target by the power asymmetry and trapped exit. The palestinian_arab_diaspora sits near d=0.92 (near-full target): rightless, barred from return by law, with only trapped exit. The british_mandate_authority sits near d=0.5 (symmetric): it enforces the territory's administration and benefits from order but is not the primary extractor; it is the frame within which extraction happens. The liberal_zionist_revisors sit near d=0.65 (mixed): they are partly coordinated into the state-building project but structurally excluded from agenda-setting and treated as obstacles; they have constrained exit but retain some intellectual/political autonomy. International powers sit near d=0.15 (beneficiary): they endorse and benefit from the arrangement (strategic ally, refugee absorption) without bearing its direct cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids collapsing into mandatrophy (founding problem outlived, function atrophied) by continuously redefining its mandate: the founding problem is European antisemitism and diaspora insecurity; by 1950, European antisemitism is discredited and many European Jews have secured refuge in the state. The constraint could logically dissolve: the founding problem is solved. But the institutional apparatus (state, military, settlement bureaucracy) develops extractive interests in permanent control, and redefines the mandate to permanent demographic dominance, territorial expansion, and security justifications that become decoupled from the original problem. The theater ratio's rise to 0.42 indicates this reframing: the constraint's stated function (Jewish security and refuge) increasingly becomes cover for extraction (territorial control, resource appropriation, domination). However, the constraint is not yet pure piton at 1950—the coordination function is still partly live (state institutions genuinely serve diaspora settlement and security), and the extraction is still partly justified by the founding problem's recency. The mandatrophy clock starts ticking after 1950 as the state consolidates and the founding problem recedes further into history; a later reading (1970–2000) would likely show mandatrophy advancing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_scope_and_solution_fit,
    'Is European antisemitism and Jewish persecution a problem that can ONLY be solved by territorial sovereignty in Palestine with a Jewish demographic majority, or could Jewish security be achieved through other mechanisms (international protection, diaspora minority rights guarantees, cultural autonomy without statehood)?',
    'Historical counterfactuals and comparative analysis: Did non-territorial Jewish communities (Diaspora, Autonomous Region schemes, cosmopolitan states) achieve similar security and self-determination outcomes? Did the territorial solution actually prevent recurrence of persecution, or did it create new conflicts? What do Jewish communities in liberal democracies report about their security and self-determination post-WWII?',
    'If other mechanisms could have provided equivalent security, the political zionist reading''s core claim—that statehood with demographic majority is necessary—is weakened, and the extraction of Palestinian territory becomes less justified as a necessary means and more apparent as a choice made among alternatives. If the territorial solution is indeed necessary, the extraction becomes a tragic necessity rather than an elective dominance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_scope_and_solution_fit, empirical, 'Whether Jewish security required specifically territorial statehood or whether other mechanisms were viable.').

omega_variable(
    demographic_majority_as_logical_requirement,
    'Is a Jewish demographic majority structurally necessary for a Jewish state to survive and provide security, or is it a contingent preference of political zionists that conflates security with dominance?',
    'Comparative study of multiethnic and multicultural states: Which institutional designs (proportional representation, federal structures, consociational arrangements, minority rights protections) allow minority-group security and self-determination without requiring demographic majority? What would Israeli statehood with a Palestinian-majority or parity population require institutionally to guarantee Jewish security?',
    'If demographic majority is not logically required for Jewish security (only institutional arrangements that guarantee minority rights and political participation), then the constraint''s extraction—the displacement and dispossession of Palestinians to achieve majority—is a choice made for domination rather than necessity. If demographic majority is logically necessary (majority cannot guarantee minority rights without coercion), then the constraint''s extraction becomes a tragic necessity embedded in the founding problem''s structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_majority_as_logical_requirement, conceptual, 'Whether demographic majority is logically necessary for Jewish state security or a chosen preference.').

omega_variable(
    transfer_mechanism_necessity_and_justice,
    'Is the ''transfer'' of Arab population from Palestine (through displacement, refugee barring, property expropriation) a logically necessary mechanism to achieve a Jewish demographic majority, or is it a chosen mechanism that could be replaced by alternative arrangements (partition, return with power-sharing, voluntary population exchange)?',
    'Demographic modeling: What population ratios would result from open immigration (Jewish) and return (Palestinian) under different scenarios? Would partition into separate territories allow both peoples to constitute majorities in their own spaces without transfer? What do international population transfers in other contexts (Greece-Turkey, India-Pakistan) tell us about whether transfers are necessary or chosen?',
    'If transfer is not logically necessary (partition or power-sharing could achieve the constraint''s aims), then treating Arab displacement as inevitable rather than chosen becomes a falsification—the constraint''s operation relies on naturalizing a political choice as a demographic necessity. If transfer is logically necessary, it remains unjust but is framed as tragic rather than elective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transfer_mechanism_necessity_and_justice, empirical, 'Whether Arab displacement is logically necessary to achieve the constraint''s goals or a chosen mechanism.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of Palestinian resistance and Arab opposition in the constraint''s operation primarily structural (external barriers: legal codes, military force, territorial control) or has it become internalized in Palestinian self-perception and political culture (the constraint is accepted as inevitable, resistance is framed as futile)?',
    'Post-separation analysis: If the constraint''s external enforcement mechanisms were removed (military withdrawal, property rights restored, borders opened to return), would Palestinian resistance and opposition persist and intensify, or has internalized suppression become dominant? What do psychological studies of dispossessed populations and long-term occupation show about the internalization of coercion?',
    'If suppression is primarily structural, the constraint could be reversed or reformed through removal of external mechanisms. If internalized, the constraint''s persistence after external enforcement ends indicates that the emotional and cognitive colonization of Palestinian self-perception is the real extractive mechanism, and the constraint''s effective suppression is higher than the 0.81 structural measure suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of opposition is structural or internalized in Palestinian consciousness.').

omega_variable(
    kernel_reading_foreclosure_or_coexistence,
    'Does the political zionist reading''s core axiom (''Jewish_statehood_requires_demographic_majority'') logically foreclose the cultural zionist and labor zionist readings, or do all three readings remain coherent within a single zionist framework despite their contradictions?',
    'Logical analysis: Can a zionist framework hold that (a) a Jewish state must have a Jewish demographic majority AND (b) a Jewish cultural center without political sovereignty would suffice? Can the same movement pursue both state-building with demographic enforcement AND socialist transformation and ''conquest of labor'' that might rely on Arab worker participation? If yes, they coexist; if no, one forecloses the other.',
    'If the political zionist reading forecloses the cultural and labor readings, the constraint''s type should shift toward a stronger dominance claim. If they coexist, then the institutional dominance of the political zionist reading is a contingent outcome (the movement could have chosen differently) rather than a logical necessity, which opens space for the constraint to be reformed or replaced by an alternative zionist arrangement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_or_coexistence, conceptual, 'Whether political Zionism forecloses or coexists with cultural and labor zionist readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__political_zionism_reading, 1880, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__political_zionism_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(jewish_territorial_claim__political_zionism_reading, 0.18).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, jewish_territorial_claim__labor_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, jewish_territorial_claim__cultural_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, jewish_territorial_claim__revisionist_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, palestinian_national_self_determination_constraint).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, arab_nationalism_territorial_claims).

% DUAL FORMULATION NOTE:
% The jewish_territorial_claim kernel decomposes into four constraint stories, each instantiating a different reading of how Jewish settlement and statehood should relate to territorial, cultural, and political sovereignty. The political_zionism_reading prioritizes state-building and demographic majority; it influences (creates structural upstream pressure on) the revisionist_zionism_reading, which responds by demanding even more aggressive territorial and demographic claims to ensure majority. It coexists with the labor and cultural readings, which remain live alternative visions within the zionist movement but are structurally subordinated by the institutional apparatus following the political zionist priority. The epsilon values differ sharply: cultural_zionism is lower-extraction (seeking spiritual/cultural center without necessarily demographic majority or political sovereignty), labor_zionism has a different extraction target (productive transformation and class revolution), revisionist_zionism has higher extraction (demands of maximalist territory and forced Arab acceptance). This constraint also affects the palestinian_national_self_determination_constraint (they are in zero-sum territorial and political conflict) and arab_nationalism_territorial_claims (which oppose Jewish demographic majority in any territory the Arab world claims as part of its historical sphere).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
