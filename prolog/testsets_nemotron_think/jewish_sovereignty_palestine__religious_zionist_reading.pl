% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__religious_zionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__religious_zionist_reading, []).

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
 *   constraint_id: jewish_sovereignty_palestine__religious_zionist_reading
 *   human_readable: Divine Promise of Eretz Yisrael as Inalienable Territorial Claim
 *   domain: political_philosophy/nationalism_studies/postcolonial_theology
 *
 * SUMMARY:
 *   The religious Zionist reading of Jewish sovereignty in Palestine treats
 *   the divine promise to Abraham (Genesis 12, 15, 17) as an eternal,
 *   inalienable title deed to the entire Land of Israel (Eretz Yisrael).
 *   Statehood is not merely a political achievement but a theological
 *   milestone — the beginning of redemption (atchalta de'geulah). This
 *   reading became politically dominant after 1967, when the Six-Day War
 *   placed the biblical heartland (Judea/Samaria) under Israeli control,
 *   energizing the settlement movement (Gush Emunim) and progressively
 *   capturing state institutions. The constraint operates by rendering the
 *   land non-negotiable: partition is not a policy choice but a theological
 *   impossibility. Palestinians are not parties to the covenant and therefore
 *   bear the extraction (land, rights, sovereignty) without standing in the
 *   constraint's logic. The coordination function (Jewish return, defense,
 *   communal cohesion) is real but subordinated to maximalist extraction —
 *   the system suppresses the very coordination (partition, shared
 *   sovereignty) that would resolve the conflict.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__religious_zionist_reading, 0.85).
domain_priors:suppression_score(jewish_sovereignty_palestine__religious_zionist_reading, 0.82).
domain_priors:theater_ratio(jewish_sovereignty_palestine__religious_zionist_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, accessibility_collapse, 0.83).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__religious_zionist_reading, snare).
narrative_ontology:human_readable(jewish_sovereignty_palestine__religious_zionist_reading, "Divine Promise of Eretz Yisrael as Inalienable Territorial Claim").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__religious_zionist_reading, "political_philosophy/nationalism_studies/postcolonial_theology").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__religious_zionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__religious_zionist_reading, 'e7dce493-d28b-4564-a2c4-c06da9b96cbf').
narrative_ontology:cs_kernel_codification('e7dce493-d28b-4564-a2c4-c06da9b96cbf', fixed_text).
narrative_ontology:cs_authority_grounding('e7dce493-d28b-4564-a2c4-c06da9b96cbf', lineage).
narrative_ontology:cs_interpretation_layer_present('e7dce493-d28b-4564-a2c4-c06da9b96cbf').
narrative_ontology:cs_reading_relation('e7dce493-d28b-4564-a2c4-c06da9b96cbf', jewish_sovereignty_palestine__liberal_nationalist_reading, forecloses).
narrative_ontology:cs_reading_relation('e7dce493-d28b-4564-a2c4-c06da9b96cbf', jewish_sovereignty_palestine__cultural_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('e7dce493-d28b-4564-a2c4-c06da9b96cbf', jewish_sovereignty_palestine__post_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('e7dce493-d28b-4564-a2c4-c06da9b96cbf', jewish_sovereignty_palestine__settler_colonial_reading, influences).
narrative_ontology:cs_axiom('e7dce493-d28b-4564-a2c4-c06da9b96cbf', foundational, divine_promise_grounds_inalienable_title).
narrative_ontology:cs_axiom_status(divine_promise_grounds_inalienable_title, holdable).
narrative_ontology:cs_axiom_grounding('e7dce493-d28b-4564-a2c4-c06da9b96cbf', divine_promise_grounds_inalienable_title, theological).
narrative_ontology:cs_axiom('e7dce493-d28b-4564-a2c4-c06da9b96cbf', foundational, statehood_is_theological_fulfillment).
narrative_ontology:cs_axiom_status(statehood_is_theological_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('e7dce493-d28b-4564-a2c4-c06da9b96cbf', statehood_is_theological_fulfillment, theological).
narrative_ontology:cs_axiom('e7dce493-d28b-4564-a2c4-c06da9b96cbf', secondary, partition_is_illegitimate).
narrative_ontology:cs_axiom_status(partition_is_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('e7dce493-d28b-4564-a2c4-c06da9b96cbf', partition_is_illegitimate, theological).
narrative_ontology:cs_reference_frame('e7dce493-d28b-4564-a2c4-c06da9b96cbf', divine_covenant_title).
narrative_ontology:cs_drift_state('e7dce493-d28b-4564-a2c4-c06da9b96cbf', contemporary_settler_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('e7dce493-d28b-4564-a2c4-c06da9b96cbf', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__religious_zionist_reading, jewish_covenant_community).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__religious_zionist_reading, religious_zionist_settlers).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__religious_zionist_reading, israeli_state_apparatus).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_people).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_residents_west_bank_gaza).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__religious_zionist_reading, divine_promise_to_abraham).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__religious_zionist_reading, jewish_people_as_covenant_community).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__religious_zionist_reading, theological_fulfillment_through_sovereignty).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__religious_zionist_reading, inalienability_of_eretz_yisrael).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The collective subject of the divine covenant; rabbinic and political leadership articulates the theological mandate, sets the agenda for settlement and sovereignty, and receives the primary spiritual benefit of covenant fulfillment. Exit from this identity would constitute apostasy or existential betrayal of the covenant itself.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, jewish_covenant_community, agenda_setter,
    institutional, civilizational, identity_locked, universal).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__religious_zionist_reading, jewish_covenant_community, beneficiary).

% Materially inhabit and expand the claim through settlement; receive state subsidies, legal protection, and ideological validation. Their personal identity fuses with the theological project — leaving the settlements means abandoning their self-concept as agents of redemption. They drive the territorial maximalist agenda from below.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, religious_zionist_settlers, beneficiary,
    organized, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__religious_zionist_reading, religious_zionist_settlers, agenda_setter).

% Provides the military, legal, and administrative enforcement machinery (military rule in West Bank, settlement planning, land expropriation laws, IDF protection). While formally a secular sovereign, the state apparatus has been progressively captured by the theological agenda — key ministries, the judiciary, and the IDF rabbinate now operate within the religious Zionist frame. Could theoretically change course but at prohibitive political cost.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, israeli_state_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Bear the material extraction: land confiscation, displacement, military occupation, denial of political rights, resource diversion (water, movement, building permits). Their national claim is rendered invisible by the theological framework — they are not parties to the covenant and therefore have no standing in the constraint's calculus. No exit: they cannot leave the land, cannot vote in the sovereign power controlling it, and cannot access the international legal order effectively.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_people, payer,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_people, excluded).

% Experience the constraint daily: checkpoints, permit regimes, settlement expansion onto private land, home demolitions, dual legal systems (military law for Palestinians, civil law for settlers). Their individual lives are structured by a constraint that does not recognize them as rights-bearing subjects. Exit is physically prevented (movement restrictions) and politically blocked (no citizenship path).
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_residents_west_bank_gaza, payer,
    powerless, biographical, trapped, local).

% Israeli Jews who accept Jewish self-determination but reject theological maximalism and occupation. They are marginalized within the Israeli polity — their parties shrink, their discourse is delegitimized as 'defeatist,' and their vision of partition is treated as heresy against the divine promise. They remain inside the polity but their voice is structurally suppressed by the constraint's logic.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, liberal_zionist_dissenters, excluded,
    moderate, biographical, constrained, national).

% UN, ICJ, ICC, and human rights bodies document violations (settlements as war crimes, apartheid findings) but lack enforcement power against a nuclear-armed state with great-power patronage. Their resolutions name the constraint's extraction but cannot alter its operation — they observe and record without effective leverage.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, international_legal_order, observer,
    institutional, generational, analytical, global).

% PA/PLO leadership seeks negotiated two-state solution based on international law. The constraint's theology renders them non-partners — no divine mandate, no covenantal standing. They are excluded from the only framework that matters (the divine promise) and their diplomatic efforts are structurally nullified by the constraint's non-negotiability.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_leadership, excluded,
    moderate, biographical, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Jewish collective return, settlement, and defense of the land as a unified covenant community under divine mandate — solves the problem of organizing a dispersed people around a singular territorial-theological mission.
% TRANSFER_FUNCTION: Moves land, water, movement rights, and political sovereignty from Palestinian presence to Jewish control under theological justification; moves the obligation to settle and defend from divine command to human action, converting theological debt into material acquisition.
% ABSENT_VOICES: Palestinian collective national voice, liberal Zionist dissenters within Israel, international legal frameworks recognizing Palestinian self-determination — excluded by the theological framework that renders the land non-negotiable and the Palestinian claim ontologically void.
% DISAPPEARANCE_RATIONALE: If the divine promise constraint vanished, the theological justification for maximalist territorial claim would collapse, enabling partition negotiations, Palestinian statehood, and a civic rather than ethnic-theological basis for Israeli polity. The settlement enterprise, military occupation architecture, and land regime would lose their legitimating core.
% FOUNDING_PROBLEM: The existential condition of Jewish exile and powerlessness; the need to fulfill divine mandate to return to and possess the promised land as precondition for messianic redemption.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by classical Jewish liturgy and rabbinic tradition across millennia (daily prayers for return to Zion, Talmudic land-centered commandments); contested by liberal Zionist and post-Zionist readings who argue the founding problem of existential exile was resolved by 1948 statehood and that continued maximalism creates new injustices.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__religious_zionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__religious_zionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__religious_zionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__religious_zionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__religious_zionist_reading, 0.85, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__religious_zionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_sovereignty_palestine__religious_zionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_sovereignty_palestine__religious_zionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.85) because the constraint claims 100% of the territory for one party and structurally denies the other party's claim — the transfer is total and uncompensated. Suppression is high (0.82) because the constraint's persistence depends on active military enforcement, legal erasure of Palestinian property rights, and ideological suppression of alternatives (partition, binationalism). Theater ratio is moderate (0.38): the theological discourse is sincere for many adherents, but the settlement enterprise's bureaucratic, legal, and military machinery performs a functional extraction that exceeds ritual observance. Accessibility collapse is high (0.83) because the divine-title frame renders alternatives (two-state, equality) cognitively unavailable to adherents — they are not 'options' but heresies. Resistance is high (0.71) from Palestinian national movement, international law, and internal Israeli dissent, but the constraint's identity-locked adherents treat resistance as validation (birth pangs of redemption).
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter/beneficiary seats (covenant community, settlers), the constraint is experienced as sacred coordination — the fulfillment of history's deepest meaning. From the payer seats (Palestinians), it is experienced as totalizing extraction backed by overwhelming force. The engine computes this divergence from the structural data: identity_locked exit for beneficiaries amplifies their subsidy perception; trapped exit for payers amplifies their extraction perception. The same military checkpoint is 'security coordination' to one seat and 'suppression mechanism' to the other. The constraint's theology explicitly denies the payer seat's standing, which the engine reads as structural suppression of voice.
 *
 * DIRECTIONALITY LOGIC:
 *   The Jewish covenant community (agenda_setter/beneficiary) sits at d ≈ 0.05 — full beneficiary, identity-locked, the constraint subsidizes their cosmic status. Religious Zionist settlers (beneficiary/agenda_setter) at d ≈ 0.15 — material beneficiaries with identity-locked exit. Israeli state apparatus (agenda_setter) at d ≈ 0.35 — administers the constraint, partly captured, could change course but at prohibitive cost (constrained exit). Palestinians (payer/excluded) at d ≈ 0.95 — full targets, trapped, bear total extraction with no voice. Liberal Zionist dissenters (excluded) at d ≈ 0.6 — bear political marginalization for opposing the constraint. International legal order (observer) at d ≈ 0.0 — analytical seat. The derivation chain from beneficiary/victim declarations + identity_locked exit produces these directionalities automatically.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (exile, powerlessness) was arguably resolved by 1948 statehood — but this reading declares it 'live' because redemption requires the whole land. The mandate has not atrophied; it has intensified. The constraint is not a piton (inertial remnant) but an actively expanding snare: extraction increases yearly (settlement growth), suppression hardens (legal annexation moves), and the theological cover story remains the primary legitimating discourse. No concentrated beneficiary 'profits' in conventional terms — the gain is theological and existential — but the material gains (land, water, subsidies) flow to settlers and state. The fixing cost is prohibitive because the constraint's logic makes compromise a metaphysical error.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the jewish_sovereignty_palestine kernel, or does it collapse into the kernel itself?',
    'Compare structural metrics (ε, suppression, beneficiary/victim sets) across all five declared readings. If readings produce divergent χ profiles for the same agents, they are distinct constraints. If they converge, the kernel is not contested but monolithic.',
    'If distinct, the kernel is a genuine contested commitment with multiple ε-invariant constraints. If not, the ''kernel'' framing is analytical error — there is only one constraint with multiple rhetorical framings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel decomposition into five readings holds structurally.').

omega_variable(
    theological_coordination_vs_extraction,
    'Does the divine promise frame provide genuine coordination (Jewish survival, communal cohesion) that is separable from the territorial extraction, or is the coordination function entirely constituted by the extraction?',
    'Counterfactual: if the territorial maximalism were removed (e.g., acceptance of 1967 lines), would the religious Zionist community retain its cohesion and sense of covenant fulfillment? Historical test: religious Zionist factions that accepted partition (Mizrachi, 1947) vs. those that rejected it (Gush Emunim, 1967+).',
    'If coordination is separable, the constraint is a tangled_rope (genuine coordination + asymmetric extraction). If inseparable, it is a pure snare — the theology is the extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_coordination_vs_extraction, empirical, 'Whether the constraint''s coordination function is structurally independent of its extraction.').

omega_variable(
    palestinian_absence_structural_or_rhetorical,
    'Is the Palestinian absence from the beneficiary/victim calculus a structural feature of the constraint (theology renders them invisible) or a rhetorical choice by current leadership?',
    'Analyze classical religious Zionist texts (Kook, Kook, Gush Emunim founders) vs. contemporary settler rhetoric. If classical sources already exclude Palestinian collective rights theologically, the absence is structural. If classical sources allow for ''ger toshav'' (resident alien) protections but contemporary actors ignore them, the absence is rhetorical escalation.',
    'If structural, the snare classification is inherent to the reading''s logic. If rhetorical, a theological reform movement could theoretically reintegrate Palestinian rights without abandoning the covenant frame — making the constraint reformable rather than foreclosed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(palestinian_absence_structural_or_rhetorical, conceptual, 'Whether Palestinian exclusion is built into the theological logic or a contingent political choice.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (military occupation, legal erasure) or partially internalized (Palestinian leadership''s collaboration with occupation via security coordination, Palestinian public''s resignation)?',
    'Post-exit suppression trajectory: if PA security coordination and Palestinian public acquiescence persist even when structural pressure eases (e.g., during Oslo), the internalized component is significant. Compare First Intifada (low internalization, high structural suppression) vs. post-Oslo period (structural suppression continues but internalized quiescence grows).',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them. This would increase χ for Palestinian seats beyond the structural suppression metric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for Palestinian payer seats.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__religious_zionist_reading, 0, 57).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jsp_rzr_tr_t0, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(jsp_rzr_tr_t10, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(jsp_rzr_tr_t20, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement(jsp_rzr_tr_t30, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(jsp_rzr_tr_t40, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 40, 0.37).
narrative_ontology:measurement(jsp_rzr_tr_t57, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 57, 0.38).

% Extraction over time
narrative_ontology:measurement(jsp_rzr_be_t0, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(jsp_rzr_be_t10, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(jsp_rzr_be_t20, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(jsp_rzr_be_t30, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 30, 0.75).
narrative_ontology:measurement(jsp_rzr_be_t40, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 40, 0.81).
narrative_ontology:measurement(jsp_rzr_be_t57, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 57, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(jsp_rzr_su_t0, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(jsp_rzr_su_t10, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(jsp_rzr_su_t20, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(jsp_rzr_su_t30, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 30, 0.76).
narrative_ontology:measurement(jsp_rzr_su_t40, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 40, 0.8).
narrative_ontology:measurement(jsp_rzr_su_t57, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 57, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__religious_zionist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_sovereignty_palestine__religious_zionist_reading, 0.08).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine__cultural_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine__post_zionist_reading).

% DUAL FORMULATION NOTE:
% This constraint and its four siblings form the jewish_sovereignty_palestine constraint family. Each reading instantiates a different ε-invariant constraint from the same kernel. The religious_zionist_reading has the highest ε (0.85) because its divine-title premise forecloses compromise; the liberal_nationalist_reading has lower ε (accepts partition); the cultural_zionist_reading lower still (no sovereignty requirement); the post_zionist_reading inverts the beneficiary/victim calculus; the settler_colonial_reading is an external analytical constraint with its own ε measuring the displacement regime's extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_sovereignty_palestine__religious_zionist_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
