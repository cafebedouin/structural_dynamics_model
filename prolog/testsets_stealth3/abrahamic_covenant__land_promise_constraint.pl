% ============================================================================
% CONSTRAINT STORY: abrahamic_covenant__land_promise_constraint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_abrahamic_covenant__land_promise_constraint, []).

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
 *   constraint_id: abrahamic_covenant__land_promise_constraint
 *   human_readable: Abrahamic Covenant Territorial-Grant Reading (Land Promise as Presently Binding Title)
 *   domain: religious/institutional/geopolitical
 *
 * SUMMARY:
 *   This story authors ONE reading of the abrahamic_covenant kernel: the
 *   claim that the Genesis territorial grant (Land of Canaan) is presently
 *   binding and confers enduring title, as that claim operates in the
 *   standing contemporary arrangement — the administered territory between
 *   the Jordan and the Mediterranean, the settlement enterprise it warrants,
 *   and the displacement structure it legitimates. The ε referent is that
 *   standing arrangement, not any endorsed alternative. The internal disputes
 *   named in the scenario label — whether the promise is conditional or
 *   fulfilled rather than ongoing — are routed to an omega variable, not
 *   averaged into this constraint; the transmission-line disputes
 *   (Isaac-exclusive vs. Ishmael-inclusive) are sibling stories linked
 *   through the network. The manifest's expected structural delta (high
 *   extraction, displaced populations as targets, a state actor capturing
 *   territorial legitimacy) is treated as a hypothesis the structural data
 *   tests, not a conclusion the metrics were tuned to produce. KEY AGENTS (by
 *   structural relationship): - israeli_state_institutions: Agenda-setting
 *   beneficiary (institutional/constrained) — administers and enforces;
 *   collects sovereignty and legitimacy - religious_settler_movement: Direct
 *   beneficiary (organized/identity_locked) — receives land, subsidy,
 *   protection; mission-fused - rabbinic_interpretive_establishment:
 *   Interpretive-layer beneficiary (institutional/identity_locked) — collects
 *   interpretive authority - west_bank_palestinian_residents: Primary target
 *   (moderate/trapped) - east_jerusalem_palestinian_residents: Target
 *   (moderate/constrained) - palestinian_refugee_diaspora: Target
 *   (powerless/trapped) - non_restorationist_jewish_denominations: Excluded
 *   voice (organized/constrained) - international_legal_institutions:
 *   Analytical observer (institutional/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(abrahamic_covenant__land_promise_constraint, 0.86).
domain_priors:suppression_score(abrahamic_covenant__land_promise_constraint, 0.82).
domain_priors:theater_ratio(abrahamic_covenant__land_promise_constraint, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, extractiveness, 0.86).
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(abrahamic_covenant__land_promise_constraint, snare).
narrative_ontology:human_readable(abrahamic_covenant__land_promise_constraint, "Abrahamic Covenant Territorial-Grant Reading (Land Promise as Presently Binding Title)").
narrative_ontology:topic_domain(abrahamic_covenant__land_promise_constraint, "religious/institutional/geopolitical").

domain_priors:requires_active_enforcement(abrahamic_covenant__land_promise_constraint).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(abrahamic_covenant__land_promise_constraint, '35ea84aa-11e1-48d7-8df8-38e46c6c8295').
narrative_ontology:cs_kernel_codification('35ea84aa-11e1-48d7-8df8-38e46c6c8295', fixed_text).
narrative_ontology:cs_authority_grounding('35ea84aa-11e1-48d7-8df8-38e46c6c8295', lineage).
narrative_ontology:cs_interpretation_layer_present('35ea84aa-11e1-48d7-8df8-38e46c6c8295').
narrative_ontology:cs_reading_relation('35ea84aa-11e1-48d7-8df8-38e46c6c8295', abrahamic_covenant__isaac_covenant_reading, influences).
narrative_ontology:cs_reading_relation('35ea84aa-11e1-48d7-8df8-38e46c6c8295', abrahamic_covenant__ishmael_covenant_reading, influences).
narrative_ontology:cs_axiom('35ea84aa-11e1-48d7-8df8-38e46c6c8295', foundational, presently_binding_territorial_grant).
narrative_ontology:cs_axiom_status(presently_binding_territorial_grant, holdable).
narrative_ontology:cs_axiom_grounding('35ea84aa-11e1-48d7-8df8-38e46c6c8295', presently_binding_territorial_grant, theological).
narrative_ontology:cs_axiom('35ea84aa-11e1-48d7-8df8-38e46c6c8295', secondary, exile_does_not_void_title).
narrative_ontology:cs_axiom_status(exile_does_not_void_title, holdable).
narrative_ontology:cs_axiom_grounding('35ea84aa-11e1-48d7-8df8-38e46c6c8295', exile_does_not_void_title, theological).
narrative_ontology:cs_reference_frame('35ea84aa-11e1-48d7-8df8-38e46c6c8295', everlasting_unconditional_land_grant).
narrative_ontology:cs_drift_state('35ea84aa-11e1-48d7-8df8-38e46c6c8295', contemporary_international_law_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('35ea84aa-11e1-48d7-8df8-38e46c6c8295', '').
narrative_ontology:cs_kernel_id(abrahamic_covenant__land_promise_constraint, abrahamic_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(abrahamic_covenant__land_promise_constraint, israeli_state_institutions).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__land_promise_constraint, religious_settler_movement).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__land_promise_constraint, rabbinic_interpretive_establishment).
narrative_ontology:constraint_victim(abrahamic_covenant__land_promise_constraint, west_bank_palestinian_residents).
narrative_ontology:constraint_victim(abrahamic_covenant__land_promise_constraint, east_jerusalem_palestinian_residents).
narrative_ontology:constraint_victim(abrahamic_covenant__land_promise_constraint, palestinian_refugee_diaspora).
narrative_ontology:constraint_vindicates(abrahamic_covenant__land_promise_constraint, election_theology).
narrative_ontology:constraint_vindicates(abrahamic_covenant__land_promise_constraint, settlement_as_redemption_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the territory between the Jordan and the Mediterranean, running the planning, land-registration, permit, and military-order systems that govern who may build, reside, farm, and move. Founding documents and official rhetoric invoke the ancestral promise in describing the land's significance. Sovereignty, state land reserves, and the legitimacy that flows from the reading accrue to these institutions, along with the costs of policing the arrangement and of sustained international friction.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, israeli_state_institutions, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(abrahamic_covenant__land_promise_constraint, israeli_state_institutions, beneficiary).

% Builds and inhabits communities across the West Bank hill country on the understanding that dwelling there fulfills an ancestral command. Receives housing subsidies, bypass-road infrastructure, and military protection; schooling, youth movements, and family life are organized around the redemptive mission. Leaving would mean abandoning homes, communities, and the meaning structure that organizes their lives.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, religious_settler_movement, beneficiary,
    organized, generational, identity_locked, regional).

% Maintains the chain of transmission through which the covenant texts are read, issues rulings on questions of land, sale, and dwelling, and trains much of the cadre that staffs the settlement movement's religious institutions. Its standing depends on the texts remaining authoritative and on its own position as authorized interpreter.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, rabbinic_interpretive_establishment, beneficiary,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(abrahamic_covenant__land_promise_constraint, rabbinic_interpretive_establishment, agenda_setter).

% Farm, build, and travel under a military and civil administration they did not elect. Land requisitions, settlement expansion, permit denials, and unequal water and road allocation steadily shrink what they control. Leaving the area means forfeiting livelihood, land, and family holdings; staying means living under expanding restrictions.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, west_bank_palestinian_residents, payer,
    moderate, generational, trapped, regional).

% Hold revocable residency rather than citizenship in the city where they live. House demolitions, residency-revocation proceedings, and ring-of-settlement encirclement press on their neighborhoods. Relocating abroad risks losing residency status; moving within the city is bounded by the separation barrier and the permit system.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, east_jerusalem_palestinian_residents, payer,
    moderate, biographical, constrained, regional).

% Descendants of those displaced in 1948 and afterward live in camps and host-country citizenship limbo across the Levant and beyond, many holding deeds and keys to homes they cannot reach. Return is blocked by the sovereignty arrangements the promise reading underwrites; third-country resettlement is partial and frequently precarious.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, palestinian_refugee_diaspora, payer,
    powerless, generational, trapped, continental).

% Hold long-standing readings of the promise as spiritual, conditional, or concluded — historic denominational platforms rejected restorationist politics outright, and other currents subordinate land to ethics. They have no seat in the institutions that determine the operative reading and watch the tradition's center of gravity move away from them.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, non_restorationist_jewish_denominations, excluded,
    organized, biographical, constrained, global).

% Assess the arrangement against treaty law and Security Council resolutions, issue advisory opinions and rulings on the barrier, settlements, and the occupation's legality, and record state responses. Their determinations reshape the diplomatic environment but carry no enforcement arm of their own.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, international_legal_institutions, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(abrahamic_covenant__land_promise_constraint, israeli_state_institutions).
narrative_ontology:fixing_cost_class(abrahamic_covenant__land_promise_constraint, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Sustains a transgenerational peoplehood around a shared scriptural warrant for belonging to a specific land: it coordinates memory, calendar, liturgy, and migration decisions across centuries and continents, and in the modern period coordinates coalition politics between religious and nationalist constituencies inside a single state project.
% TRANSFER_FUNCTION: Moves land, residence rights, building permits, water shares, and governing capacity from inhabitants outside the covenant line toward the covenant-line polity and its settlement vanguard; separately, it moves legitimacy — narrative capital with diaspora supporters and voters — to the state institutions that invoke the reading.
% ABSENT_VOICES: The displaced and occupied sit outside the interpretive process that produced the operative reading: refugee communities barred from return, residents governed by military order without a vote over those orders, and the Muslim and Christian custodians of rival attachments to the same geography. Non-restorationist Jewish denominations are inside the textual tradition but outside the operative interpretation. They speak from courtrooms, camps, and minority platforms rather than from the tables where the reading is administered.
% DISAPPEARANCE_RATIONALE: If the ongoing-grant reading ceased overnight to bind anyone, territorial claims would stand on international law, documented history, and demography alone; the settlement enterprise loses its scriptural warrant and much of its recruitment and funding logic; governing coalitions built around the religious blocs would re-form; rival Abrahamic claims lose their principal foil. The conflict's theological engine drops out, leaving a hard but ordinary territorial dispute with a different structure.
% FOUNDING_PROBLEM: A landless, dispersed people needed a portable warrant for identity and endurance: the promise answered dispersion with a destination and bound scattered communities to one geography and one continuing story.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Judaism working outside any beneficiary seat document the promise's diaspora-identity function; Palestinian historiography and international legal scholarship attest the modern shift toward a live-title instrument; non-restorationist Jewish platforms corroborate that the tradition contains settled non-political readings of the same verses. No source outside the benefiting parties attests that present-tense divine title to the territory is fact — outside corroboration uniformly supports the identity-history reading and disputes the live-title one.
narrative_ontology:disappearance_verdict(abrahamic_covenant__land_promise_constraint, world_rearranges).
narrative_ontology:founding_problem_status(abrahamic_covenant__land_promise_constraint, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(abrahamic_covenant__land_promise_constraint, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(abrahamic_covenant__land_promise_constraint, 'none', 1).
narrative_ontology:epsilon_provenance(abrahamic_covenant__land_promise_constraint, 0.86, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(abrahamic_covenant__land_promise_constraint_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(abrahamic_covenant__land_promise_constraint, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(abrahamic_covenant__land_promise_constraint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Time indexing assumption: T0 corresponds to 1967 (the territories come under Israeli control and the land-promise reading acquires a large operational surface); T59 corresponds to 2026. Extractiveness is authored high (0.86 at interval end) because the arrangement continuously transfers land, residence rights, water, and governing capacity away from a population of millions with no compensating flow back; the series ratchets upward through settlement acceleration after the 1977 political turn, Oslo-era parallel expansion, barrier and permit-regime consolidation, normalization of annexation discourse, and wartime displacement after 2023. Suppression is authored as a raw structural property (unscaled by power or scope): the arrangement persists through military order, administrative detention, a maturing permit and barrier system, and the marginalization of alternative readings inside and outside the tradition — hence the rising suppression_requirement series tracking enforcement-capacity buildup, which is exactly the dynamic this story traces. Theater_ratio is moderate and rising (0.24 → 0.42): much of the devotional and archaeological activity is sincere, but a growing share of maintenance effort is performative legitimation (site symbolism, anniversary ritual, antiquity-as-title campaigns) relative to the arrangement's functional governance content. Accessibility_collapse is moderate (0.55): alternatives — conditional readings, fulfilled/spent readings, negotiated partition, binational frames — remain articulable and periodically negotiated, but each faces heavy institutional friction. Resistance is high (0.72): armed, civic, legal, boycott-based, and interstate pressure, plus internal dissent. Claim and metrics are independent authored facts: claimed_type is snare because the modern operative form's coordination content for anyone outside the extracting coalition is thin — the identity-coordination story functions largely as cover — while the metrics describe observed operation; where the engine computes a divergent type from these data, that divergence is the measurement the corpus exists to take. The series run on one shared seven-point grid; the trajectory is a ratchet with punctuated jumps rather than a regular cycle, so no extended oscillation grid is required.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute very differently. From the religious_settler_movement seat, the arrangement presents as sacred obligation and belonging — a calling with real costs willingly borne — which computes closer to identity coordination than extraction. From the israeli_state_institutions seat it presents as a legitimacy asset worth its enforcement and diplomatic costs. From the trapped payer seats — occupied residents and the refugee diaspora — the same structure presents as unambiguous extraction with no exit. The rabbinic_interpretive_establishment seat experiences interpretive sovereignty: the reading is its institutional product and source of standing. The excluded denominations experience the constraint as silencing within their own tradition. The observer seat registers contested legality without bearing any of its costs. The engine derives these divergences from the structural data; nothing in the authored claim adjudicates among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit near the d≈0 end: the state institutions collect sovereignty and legitimacy while bearing enforcement costs (slightly above pure beneficiary, but strongly net-subsidized); settler communities receive land, subsidy, and meaning, and their identity_locked exit amplifies their investment in the arrangement's persistence; the rabbinic establishment collects interpretive authority. Targets sit near the d≈1 end: the refugee diaspora is the most fully locked (no reachable home, no host-state integration, no seat anywhere) and therefore computes nearest the full-target pole; West Bank and East Jerusalem residents are heavily extracted-from with trapped-to-constrained exit. Suppression is declared as a structural property and deliberately not adjusted for scope; effective extraction is left entirely to the engine's scaling over directionality and the national/regional scopes carried by the stakeholders.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — giving a dispersed, landless people a portable warrant for identity and endurance — was substantially solved by sovereign statehood in 1948; the arrangement nonetheless persists under a successor mandate (legitimating ongoing control of territory inhabited by another people), which is the classic mandate-outlived-function signature, recorded in base_properties.mandatrophy_resolved while the founding_problem_status stays contested because the beneficiary tradition disputes that the exile-problem is closed. The mandatrophy lens prevents two mislabelings: it blocks a rope reading (the coordination function is real but historically located in the diaspora era, not in the present title-enforcement arrangement this story's referent fixes), and it blocks a piton reading (nothing here is vestigial — the arrangement is actively maintained and expanding). The divine-decree rhetoric surrounding the reading is mountain-shaped cover for a constructed, enforced arrangement; the omega variables carry that ambiguity rather than the classification resolving it by fiat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint is the land_promise_constraint reading of the abrahamic_covenant kernel: the grantee clause is the locus of the kernel contest — the isaac_covenant_reading transmits the covenant exclusively through Isaac and excludes Ishmael, while the ishmael_covenant_reading continues it through Ishmael to Muhammad inclusively; this reading fixes the OBJECT of the promise (territory) and its present bindingness. How do the readings partition the same verses?',
    'Comparative-theological tracing of which clause each tradition loads (grantee, object, duration) and of the institutional sites where each reading is administered; corpus decomposition into the sibling stories already linked in the network.',
    'Adopting the inclusive-transmission reading dissolves the exclusivity that powers the modern title conflict and collapses this reading''s extraction; the exclusive readings are what make the territorial grant zero-sum.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Kernel-membership omega: one covenant, three live readings, disagreement located in the transmission and duration clauses.').

omega_variable(
    promise_duration_status,
    'Within the tradition itself, is the territorial promise conditional (Deuteronomically conditioned on conduct), fulfilled-and-concluded, or presently binding? The scenario label names all three positions as live.',
    'Textual-tradition analysis of which variant commands operative allegiance in the institutions that administer the arrangement (court rulings, settlement-movement curricula, state ceremonial usage), as opposed to positions held only academically.',
    'If the conditional or concluded variant is the operative one, current-title claims lose their warrant and ε collapses toward the diaspora-era baseline; the high-extraction structure depends entirely on the ongoing variant holding institutional force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(promise_duration_status, conceptual, 'Internal-variant omega: conditionality versus fulfillment versus ongoing bindingness of the grant.').

omega_variable(
    divine_decree_vs_political_instrument,
    'Is the land grant a theological reality that would bind regardless of enforcement, or a political instrument maintained by coercion and dressed in decree language?',
    'Counterfactual enforcement-withdrawal comparison: observe the claim''s vitality and behavioral force when decoupled from state power (the diaspora-era function) against its state-era operation; measure whether commitment survives the removal of the material returns.',
    'If purely instrumental, the snare classification stands with the decree language confirmed as cover; if sincere commitment independent of enforcement is substantial, part of the structure is better modeled as identity coordination and the effective extraction attributable to the reading proper shrinks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_decree_vs_political_instrument, conceptual, 'Naturalness omega: mountain-shaped divine-decree rhetoric versus constructed enforced arrangement.').

omega_variable(
    target_coalition_capacity,
    'Can the targeted populations'' collective action — litigation, civic resistance, boycott movements, interstate recognition campaigns — materially alter the arrangement, or is the coercion-exit asymmetry effectively total?',
    'Track the downstream effect of advisory opinions and rulings, recognition votes, and campaign pressure on settlement rates, demolition rates, and coalition stability over successive measurement windows.',
    'If coalition capacity proves effective, the resistance metric rises and the structure trends toward negotiated coordination rather than pure extraction; if it stays ineffective, the snare reading is reinforced and the trapped-target seats'' extraction remains uncompensated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(target_coalition_capacity, empirical, 'Coalition-power omega for a multi-victim snare: whether aggregate target-side power can bend the arrangement.').

omega_variable(
    era_referent_decomposition,
    'Does the covenant reading''s extraction differ across eras enough to constitute two distinct constraints — the diaspora-era identity-endurance function versus the sovereign-era title function this story authors?',
    'Corpus decomposition: author a separate story fixing the diaspora-era referent (promise sustaining dispersed communities without territorial enforcement) and compare classifications; this story keeps the sovereign-era referent fixed.',
    'If separated, this story retains its high-ε sovereign-era profile while the diaspora-era story plausibly classifies as rope-like identity coordination; keeping them merged would average a genuine coordination function into an extractive arrangement and blur both.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(era_referent_decomposition, conceptual, 'Decomposition omega guarding ε-invariance across the reading''s historical phases.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(abrahamic_covenant__land_promise_constraint, 0, 59).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(abra_tr_t0, abrahamic_covenant__land_promise_constraint, theater_ratio, 0, 0.24).
narrative_ontology:measurement(abra_tr_t10, abrahamic_covenant__land_promise_constraint, theater_ratio, 10, 0.27).
narrative_ontology:measurement(abra_tr_t20, abrahamic_covenant__land_promise_constraint, theater_ratio, 20, 0.31).
narrative_ontology:measurement(abra_tr_t30, abrahamic_covenant__land_promise_constraint, theater_ratio, 30, 0.33).
narrative_ontology:measurement(abra_tr_t40, abrahamic_covenant__land_promise_constraint, theater_ratio, 40, 0.36).
narrative_ontology:measurement(abra_tr_t50, abrahamic_covenant__land_promise_constraint, theater_ratio, 50, 0.39).
narrative_ontology:measurement(abra_tr_t59, abrahamic_covenant__land_promise_constraint, theater_ratio, 59, 0.42).

% Extraction over time
narrative_ontology:measurement(abra_be_t0, abrahamic_covenant__land_promise_constraint, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(abra_be_t10, abrahamic_covenant__land_promise_constraint, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(abra_be_t20, abrahamic_covenant__land_promise_constraint, base_extractiveness, 20, 0.73).
narrative_ontology:measurement(abra_be_t30, abrahamic_covenant__land_promise_constraint, base_extractiveness, 30, 0.76).
narrative_ontology:measurement(abra_be_t40, abrahamic_covenant__land_promise_constraint, base_extractiveness, 40, 0.8).
narrative_ontology:measurement(abra_be_t50, abrahamic_covenant__land_promise_constraint, base_extractiveness, 50, 0.83).
narrative_ontology:measurement(abra_be_t59, abrahamic_covenant__land_promise_constraint, base_extractiveness, 59, 0.86).

% Suppression requirement over time
narrative_ontology:measurement(abra_su_t0, abrahamic_covenant__land_promise_constraint, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(abra_su_t10, abrahamic_covenant__land_promise_constraint, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(abra_su_t20, abrahamic_covenant__land_promise_constraint, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(abra_su_t30, abrahamic_covenant__land_promise_constraint, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(abra_su_t40, abrahamic_covenant__land_promise_constraint, suppression_requirement, 40, 0.74).
narrative_ontology:measurement(abra_su_t50, abrahamic_covenant__land_promise_constraint, suppression_requirement, 50, 0.78).
narrative_ontology:measurement(abra_su_t59, abrahamic_covenant__land_promise_constraint, suppression_requirement, 59, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(abrahamic_covenant__land_promise_constraint, identity_coordination).
narrative_ontology:affects_constraint(abrahamic_covenant__land_promise_constraint, isaac_covenant_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__land_promise_constraint, ishmael_covenant_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'Abrahamic covenant promise.' The label conflates three structurally distinct claims: (1) WHO inherits the covenant — the isaac_covenant_reading and ishmael_covenant_reading siblings, which disagree on the transmission clause; (2) WHAT is promised and WHETHER IT BINDS NOW — this story (object-and-duration axis, sovereign-era referent, high ε); (3) the diaspora-era identity-endurance function, flagged for separate authorship by the era_referent_decomposition omega. Each member carries its own ε, beneficiaries, and victims; they are linked rather than merged because the upstream exclusivist genealogy (Isaac reading) supplies the warrant this reading consumes, while the inclusive reading competes as a rival title basis — the upstream claim is routinely cited as evidence for the downstream territorial claim, which is why the edges run through this node.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
