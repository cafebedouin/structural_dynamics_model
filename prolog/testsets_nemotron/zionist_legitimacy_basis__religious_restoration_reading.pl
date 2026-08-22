% ============================================================================
% CONSTRAINT STORY: zionist_legitimacy_basis__religious_restoration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zionist_legitimacy_basis__religious_restoration_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: zionist_legitimacy_basis__religious_restoration_reading
 *   human_readable: Religious Restoration Basis for Zionist Legitimacy (Post-1967)
 *   domain: political_history/nationalism/settler_colonialism_studies
 *
 * SUMMARY:
 *   Post-1967 religious Zionism reinterprets the Six-Day War conquest of
 *   biblical Judea and Samaria as divine intervention initiating the
 *   messianic process. The constraint is the claim that Jewish settlement of
 *   the entire Land of Israel is a religious obligation that overrides
 *   secular political considerations, international law, and Palestinian
 *   rights. The arrangement extracts land, resources, and political autonomy
 *   from Palestinians while coordinating Jewish settlers around a messianic
 *   project. Beneficiaries include the settlement movement, state rabbinate,
 *   and messianic parties who gain theological authority, state funding, and
 *   political power. Victims include Palestinians under occupation, secular
 *   Israelis who bear security costs, and dissident religious Jews
 *   marginalized by the dominant theology. The reading coexists with but
 *   structurally pressures the national liberation and settler colonial
 *   readings.
 *
 * KEY AGENTS:
 *   - religious_settlement_movement: Primary beneficiary (institutional/identity_locked) — receives state resources, theological authority, political power
 *   - state_rabbinate_institutions: Primary beneficiary (institutional/identity_locked) — monopoly over religious certification, conversion, marriage; state-funded
 *   - messianic_nationalist_parties: Primary beneficiary (organized/identity_locked) — political representation, coalition leverage, legislative agenda
 *   - palestinian_population_west_bank_gaza: Primary victim (powerless/trapped) — land expropriation, movement restrictions, military rule, no citizenship
 *   - secular_israeli_citizens: Secondary victim (powerful/constrained) — security costs, democratic erosion, military service burden, cultural coercion
 *   - palestinian_citizens_of_israel: Secondary victim (moderate/constrained) — institutional discrimination, demographic anxiety, citizenship hierarchy
 *   - dissident_religious_jews: Excluded victim (moderate/identity_locked) — theological marginalization, communal ostracism for opposing settlement theology
 *   - analytical_observer: Observer (analytical/analytical) — sees full structural dynamics across readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis__religious_restoration_reading, 0.88).
domain_priors:suppression_score(zionist_legitimacy_basis__religious_restoration_reading, 0.82).
domain_priors:theater_ratio(zionist_legitimacy_basis__religious_restoration_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__religious_restoration_reading, tangled_rope).
narrative_ontology:human_readable(zionist_legitimacy_basis__religious_restoration_reading, "Religious Restoration Basis for Zionist Legitimacy (Post-1967)").
narrative_ontology:topic_domain(zionist_legitimacy_basis__religious_restoration_reading, "political_history/nationalism/settler_colonialism_studies").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__religious_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__religious_restoration_reading, 'beb85050-6a11-40a4-9748-215e3974188a').
narrative_ontology:cs_kernel_codification('beb85050-6a11-40a4-9748-215e3974188a', fixed_text).
narrative_ontology:cs_authority_grounding('beb85050-6a11-40a4-9748-215e3974188a', lineage).
narrative_ontology:cs_interpretation_layer_present('beb85050-6a11-40a4-9748-215e3974188a').
narrative_ontology:cs_reading_relation('beb85050-6a11-40a4-9748-215e3974188a', zionist_legitimacy_basis__national_liberation_reading, coexists_with).
narrative_ontology:cs_reading_relation('beb85050-6a11-40a4-9748-215e3974188a', zionist_legitimacy_basis__settler_colonial_reading, coexists_with).
narrative_ontology:cs_axiom('beb85050-6a11-40a4-9748-215e3974188a', foundational, divine_land_promise_eternal).
narrative_ontology:cs_axiom_status(divine_land_promise_eternal, holdable).
narrative_ontology:cs_axiom_grounding('beb85050-6a11-40a4-9748-215e3974188a', divine_land_promise_eternal, theological).
narrative_ontology:cs_axiom('beb85050-6a11-40a4-9748-215e3974188a', foundational, messianic_process_activated_1967).
narrative_ontology:cs_axiom_status(messianic_process_activated_1967, holdable).
narrative_ontology:cs_axiom_grounding('beb85050-6a11-40a4-9748-215e3974188a', messianic_process_activated_1967, theological).
narrative_ontology:cs_axiom('beb85050-6a11-40a4-9748-215e3974188a', secondary, halakhic_obligation_overrides_international_law).
narrative_ontology:cs_axiom_status(halakhic_obligation_overrides_international_law, holdable).
narrative_ontology:cs_axiom_grounding('beb85050-6a11-40a4-9748-215e3974188a', halakhic_obligation_overrides_international_law, deontological).
narrative_ontology:cs_reference_frame('beb85050-6a11-40a4-9748-215e3974188a', biblical_covenantal_framework).
narrative_ontology:cs_drift_state('beb85050-6a11-40a4-9748-215e3974188a', post_1967_occupation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('beb85050-6a11-40a4-9748-215e3974188a', '').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__religious_restoration_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__religious_restoration_reading, religious_settlement_movement).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__religious_restoration_reading, state_rabbinate_institutions).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__religious_restoration_reading, messianic_nationalist_parties).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, palestinian_population_west_bank_gaza).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, secular_israeli_citizens).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, palestinian_citizens_of_israel).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, dissident_religious_jews).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__religious_restoration_reading, divine_land_promise_fulfillment).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__religious_restoration_reading, messianic_redemption_through_settlement).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__religious_restoration_reading, halakhic_territorial_maximalism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organizes and executes settlement expansion in West Bank; receives state funding, infrastructure, military protection; theological authority derives from claiming to fulfill divine mandate; exit would require abandoning core identity as messianic agents.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, religious_settlement_movement, beneficiary,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__religious_restoration_reading, religious_settlement_movement, agenda_setter).

% Holds state monopoly over Jewish religious status (conversion, marriage, divorce, burial); certifies settlement enterprise as religiously mandated; funded by state; theological identity fused with institutional survival.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, state_rabbinate_institutions, beneficiary,
    institutional, generational, identity_locked, national).

% Political vehicles for settlement movement (e.g., Religious Zionism, Otzma Yehudit); leverage coalition politics to advance settlement expansion, judicial reform, annexation; voter base identity-locked to messianic theology.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, messianic_nationalist_parties, agenda_setter,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__religious_restoration_reading, messianic_nationalist_parties, beneficiary).

% Subject to military rule without citizenship; land expropriated for settlements; movement restricted by checkpoints, permits, separation barrier; no political representation in governing authority; resistance met with disproportionate force.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, palestinian_population_west_bank_gaza, payer,
    powerless, generational, trapped, regional).

% Bear security costs (military service, terrorism risk, economic burden); democratic institutions eroded by settlement politics; cultural space constrained by religious coercion; can emigrate or oppose politically but at high personal cost.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, secular_israeli_citizens, payer,
    powerful, biographical, constrained, national).

% Formal citizens but structurally subordinated: land allocation discrimination, demographic surveillance, loyalty tests, exclusion from national ethos; political representation marginalized; exit requires leaving homeland.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, palestinian_citizens_of_israel, payer,
    moderate, generational, constrained, national).

% Religious Jews (e.g., Neturei Karta, liberal Orthodox, Reform/Conservative) who oppose settlement theology; marginalized as 'traitors to the faith'; communal ostracism, loss of religious authority, family rupture; identity fusion makes exit existential.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, dissident_religious_jews, excluded,
    moderate, biographical, identity_locked, national).

% Sees the full structural dynamics across all three kernel readings; no material stake in any reading's victory; evaluates classification from outside the contested commitments.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Jewish settlers around a shared messianic project: settlement as divine command creates collective purpose, mutual obligation, and theological meaning that sustains high-cost collective action (settlement enterprise, military service, political mobilization) without requiring continuous state coercion of participants.
% TRANSFER_FUNCTION: Moves land, water, state resources, and political autonomy from Palestinian population (West Bank/Gaza) to Jewish settlers and Israeli state; moves theological authority and state funding to religious institutions; moves security burden and democratic erosion onto secular Israeli citizens; moves civil equality onto Palestinian citizens of Israel.
% ABSENT_VOICES: Palestinian refugees (1948 and 1967) whose right of return is theologically negated; Mizrahi Jews whose indigenous Middle Eastern identity is erased by Ashkenazi-dominated religious nationalism; global Jewish diaspora voices critical of occupation but excluded from Israeli political discourse; future generations of both peoples who inherit the conflict.
% DISAPPEARANCE_RATIONALE: If the religious restoration reading vanished overnight, the settlement enterprise would lose its primary theological legitimation and motivated base; Israeli politics would lose its most cohesive ideological bloc; the occupation regime would face crisis of legitimacy; Palestinian political horizon would shift. The world would rearrange — but whether toward peace, civil war, or new conflict is contested.
% FOUNDING_PROBLEM: Post-Holocaust Jewish existential insecurity and the theological crisis of divine absence; the need to explain Jewish survival and sovereignty as divine redemption rather than historical accident; the religious obligation to settle the Land of Israel as commandment.
% FOUNDING_PROBLEM_CORROBORATION: Beneficiaries (religious settlement movement, state rabbinate) attest the founding problem remains live: security threats persist, redemption incomplete, divine command eternal. Critics (Israeli historians like Tom Segev, Palestinian scholars like Rashid Khalidi, dissident rabbis like David Hartman) attest the founding problem (Jewish survival) is substantially solved — Israel exists as sovereign state — and the arrangement now serves territorial maximalism, not security. International legal consensus (ICJ, UN) corroborates the shift from survival to expansion.
narrative_ontology:disappearance_verdict(zionist_legitimacy_basis__religious_restoration_reading, world_rearranges).
narrative_ontology:founding_problem_status(zionist_legitimacy_basis__religious_restoration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__religious_restoration_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(zionist_legitimacy_basis__religious_restoration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zionist_legitimacy_basis__religious_restoration_reading, 0.88, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zionist_legitimacy_basis__religious_restoration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(zionist_legitimacy_basis__religious_restoration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(zionist_legitimacy_basis__religious_restoration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.88) is very high: the reading legitimizes permanent occupation, settlement expansion, and legal discrimination, transferring land and rights from Palestinians to Jewish settlers. Suppression (0.82) is high: military enforcement, legal restrictions, and theological delegitimization of dissent maintain the arrangement. Theater (0.35) is moderate: genuine messianic belief coordinates settlers, but growing performative adherence as political necessity rather than conviction. Accessibility collapse (0.72) is high: the theological frame makes territorial compromise appear as religious betrayal, collapsing political alternatives. Resistance (0.68) is substantial: Palestinian resistance, Israeli peace movements, international pressure, and internal religious dissent all contest the arrangement. The claimed type tangled_rope reflects genuine messianic coordination for believers combined with asymmetric extraction from Palestinians.
 *
 * PERSPECTIVAL GAP:
 *   From the religious settler seat (beneficiary/agenda_setter), the constraint is experienced as rope — genuine coordination around divine command. From the Palestinian seat (victim), it is experienced as snare — pure extraction enforced by military power. From the secular Israeli seat (secondary victim), it is experienced as tangled_rope — security coordination mixed with ideological extraction. The engine computes this divergence from the structural data: beneficiaries declared as religious_settlement_movement/state_rabbinate/messianic_parties with identity_locked exit; victims declared as palestinian_population/secular_israelis/palestinian_citizens/dissident_religious with trapped/constrained exit.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (religious_settlement_movement, state_rabbinate, messianic_parties) hold institutional power and identity_locked exit — they cannot exit the theological frame without losing their structural position, so d is low (~0.15-0.25) and they experience subsidy. Victims: palestinian_population is powerless/trapped (d ~0.95); secular_israelis are powerful/constrained (d ~0.65 — they bear costs but have some exit via emigration or political opposition); palestinian_citizens_of_israel are moderate/constrained (d ~0.75); dissident_religious_jews are moderate/identity_locked (d ~0.7 — theological identity fused with communal belonging). The theological mandate overrides secular law for beneficiaries but is imposed as suppression on victims.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Jewish existential security after Holocaust) is contested — beneficiaries claim it remains live, critics argue it is dead (state exists, security achieved) or transformed (security now requires Palestinian rights). The arrangement persists by fusing the original security mandate with an expanding theological mandate that has no natural terminus (messianic completion is indefinitely deferred). This mandatrophy is unresolved: the coordination function (Jewish survival) has been achieved, but the extraction function (territorial maximalism) has expanded and cannot be satisfied without permanent subjugation of another people. The tangled_rope classification captures this: genuine coordination (Jewish sovereignty, security) fused with asymmetric extraction (settlement enterprise).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_of_contested_kernel,
    'This constraint is one reading (religious_restoration_reading) of the contested kernel zionist_legitimacy_basis. How does the structural classification change when the same historical arrangement is read through national_liberation_reading or settler_colonial_reading?',
    'Author separate constraint stories for each reading per ε-invariance principle; compare computed types across readings. The kernel structure is irreducible ambiguity, not measurement error.',
    'If national_liberation_reading computes as rope and settler_colonial_reading computes as snare, the kernel itself demonstrates reading-dependent classification — confirming that ''Zionism'' is a label covering structurally distinct claims, not one constraint with multiple perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_of_contested_kernel, conceptual, 'Kernel reading multiplicity and ε-invariance across readings').

omega_variable(
    divine_mandate_vs_secular_law,
    'Does the theological mandate (divine promise, messianic process) function as a genuine coordination mechanism for believers, or as an extraction cover that suppresses secular legal constraints and Palestinian rights?',
    'Track whether religious settlers accept adverse rulings from Israeli secular courts on land issues (coordination) or reject them as illegitimate (extraction cover). Measure compliance divergence between religious and secular legal orders.',
    'If theological mandate coordinates behavior independently of state enforcement → genuine coordination function (rope component). If it only operates through state enforcement and collapses without it → extraction cover (snare component). Determines whether tangled_rope classification holds or degrades to snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_mandate_vs_secular_law, empirical, 'Theological mandate as coordination vs. extraction mechanism').

omega_variable(
    post_1967_shift_magnitude,
    'How much did the 1967 conquest of the West Bank transform religious Zionism from passive messianic waiting to active territorial maximalism? Was the shift doctrinal or opportunistic?',
    'Compare pre-1967 and post-1967 rabbinic literature, settlement rhetoric, and institutional behavior. Look for doctrinal innovation vs. selective citation of existing sources.',
    'If doctrinal innovation → the reading is a constructed response to new political facts (supports high extraction). If continuity with pre-1967 sources → the reading reveals latent structure activated by circumstance (supports coordination function).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_1967_shift_magnitude, empirical, '1967 as doctrinal rupture vs. activation in religious Zionist theology').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis__religious_restoration_reading, 1967, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zion_tr_t1967, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 1967, 0.15).
narrative_ontology:measurement(zion_tr_t1977, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 1977, 0.22).
narrative_ontology:measurement(zion_tr_t1987, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 1987, 0.28).
narrative_ontology:measurement(zion_tr_t1993, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 1993, 0.31).
narrative_ontology:measurement(zion_tr_t2000, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 2000, 0.33).
narrative_ontology:measurement(zion_tr_t2010, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 2010, 0.34).
narrative_ontology:measurement(zion_tr_t2023, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 2023, 0.35).

% Extraction over time
narrative_ontology:measurement(zion_be_t1967, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 1967, 0.55).
narrative_ontology:measurement(zion_be_t1977, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 1977, 0.68).
narrative_ontology:measurement(zion_be_t1987, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 1987, 0.75).
narrative_ontology:measurement(zion_be_t1993, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 1993, 0.78).
narrative_ontology:measurement(zion_be_t2000, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 2000, 0.82).
narrative_ontology:measurement(zion_be_t2010, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 2010, 0.85).
narrative_ontology:measurement(zion_be_t2023, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 2023, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(zion_su_t1967, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 1967, 0.45).
narrative_ontology:measurement(zion_su_t1977, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 1977, 0.58).
narrative_ontology:measurement(zion_su_t1987, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 1987, 0.68).
narrative_ontology:measurement(zion_su_t1993, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 1993, 0.72).
narrative_ontology:measurement(zion_su_t2000, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 2000, 0.76).
narrative_ontology:measurement(zion_su_t2010, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 2010, 0.79).
narrative_ontology:measurement(zion_su_t2023, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 2023, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis__religious_restoration_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(zionist_legitimacy_basis__religious_restoration_reading, 0.12).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, zionist_legitimacy_basis__national_liberation_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, zionist_legitimacy_basis__settler_colonial_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, occupation_regime_west_bank).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, settlement_enterprise_funding).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, israeli_democratic_erosion).

% DUAL FORMULATION NOTE:
% Part of the zionist_legitimacy_basis constraint family. This reading (religious_restoration) provides the theological engine for territorial maximalism that the settler_colonial_reading identifies as extraction cover and the national_liberation_reading must either incorporate or reject. The three readings share the same historical referent (Zionist project post-1967) but author different ε values and beneficiary/victim structures per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(zionist_legitimacy_basis__religious_restoration_reading, institutional, 0.15).
constraint_indexing:directionality_override(zionist_legitimacy_basis__religious_restoration_reading, powerless, 0.95).
constraint_indexing:directionality_override(zionist_legitimacy_basis__religious_restoration_reading, powerful, 0.65).
constraint_indexing:directionality_override(zionist_legitimacy_basis__religious_restoration_reading, moderate, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
