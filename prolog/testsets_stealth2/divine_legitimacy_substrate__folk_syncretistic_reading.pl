% ============================================================================
% CONSTRAINT STORY: divine_legitimacy_substrate__folk_syncretistic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_legitimacy_substrate_folk_syncretistic_reading, []).

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
 *   constraint_id: divine_legitimacy_substrate__folk_syncretistic_reading
 *   human_readable: Household-Village Ritual Substrate of Divine Legitimacy (Folk Syncretistic Reading)
 *   domain: religious/political-economic (New Kingdom Egypt)
 *
 * SUMMARY:
 *   In New Kingdom Egypt, divine legitimacy reaches ordinary people through
 *   what happens in courtyards and on village feast days: household shrines,
 *   ancestor remembrance, protective amulets, pragmatic recourse to whichever
 *   deity addresses the trouble at hand. This story authors ONE reading of
 *   the divine_legitimacy_substrate kernel — the folk syncretistic reading,
 *   in which legitimacy flows through household and village practice itself,
 *   with pharaoh and priesthood as distant elites whom the practice neither
 *   requires nor obeys. The sibling readings (amun_polytheistic_reading:
 *   legitimacy through established priestly interpretation;
 *   atenist_monotheistic_reading: legitimacy solely through pharaonic
 *   revelation of Aten) are separate constraints in separate files with their
 *   own epsilon values and beneficiary structures; they are not averaged into
 *   this one. Epsilon's referent here is the standing arrangement under
 *   contest — the household/village practice complex as it actually operated
 *   — assessed by this reading's own lights, in which practice-based
 *   legitimacy is genuine and good. KEY AGENTS (by structural relationship):
 *   - village_households: Primary beneficiary and substrate
 *   (organized/identity_locked) — enact and transmit the practice -
 *   household_heads: Agenda-setter at household scale
 *   (moderate/identity_locked) — conduct daily rite, adapt pragmatically -
 *   local_ritual_specialists: Secondary beneficiary (moderate/constrained) —
 *   collect fees and offerings for services rendered -
 *   state_temple_establishment: Incidental beneficiary, distant elite
 *   (institutional/arbitrage) - pharaonic_state: Incidental beneficiary,
 *   distant elite (institutional/arbitrage) — attempted top-down replacement
 *   failed - landless_laborers: Excluded voice (powerless/trapped) — bear
 *   festival burdens without standing - scribal_recorders: Analytical
 *   observer (moderate/analytical) — record elite religion; folk practice
 *   enters the archive only accidentally
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_legitimacy_substrate__folk_syncretistic_reading, 0.28).
domain_priors:suppression_score(divine_legitimacy_substrate__folk_syncretistic_reading, 0.13).
domain_priors:theater_ratio(divine_legitimacy_substrate__folk_syncretistic_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 0.13).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_legitimacy_substrate__folk_syncretistic_reading, rope).
narrative_ontology:human_readable(divine_legitimacy_substrate__folk_syncretistic_reading, "Household-Village Ritual Substrate of Divine Legitimacy (Folk Syncretistic Reading)").
narrative_ontology:topic_domain(divine_legitimacy_substrate__folk_syncretistic_reading, "religious/political-economic (New Kingdom Egypt)").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__folk_syncretistic_reading, 'c450ab6d-3c3c-42e6-8d90-1e82d6dd0c12').
narrative_ontology:cs_kernel_codification('c450ab6d-3c3c-42e6-8d90-1e82d6dd0c12', distributed).
narrative_ontology:cs_authority_grounding('c450ab6d-3c3c-42e6-8d90-1e82d6dd0c12', practice).
narrative_ontology:cs_interpretation_layer_present('c450ab6d-3c3c-42e6-8d90-1e82d6dd0c12').
narrative_ontology:cs_reading_relation('c450ab6d-3c3c-42e6-8d90-1e82d6dd0c12', divine_legitimacy_substrate__amun_polytheistic_reading, coexists_with).
narrative_ontology:cs_reading_relation('c450ab6d-3c3c-42e6-8d90-1e82d6dd0c12', divine_legitimacy_substrate__atenist_monotheistic_reading, forecloses).
narrative_ontology:cs_axiom('c450ab6d-3c3c-42e6-8d90-1e82d6dd0c12', foundational, plural_devotion_legitimate).
narrative_ontology:cs_axiom_status(plural_devotion_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('c450ab6d-3c3c-42e6-8d90-1e82d6dd0c12', plural_devotion_legitimate, conventional).
narrative_ontology:cs_axiom('c450ab6d-3c3c-42e6-8d90-1e82d6dd0c12', foundational, rite_efficacy_independent_of_office).
narrative_ontology:cs_axiom_status(rite_efficacy_independent_of_office, holdable).
narrative_ontology:cs_axiom_grounding('c450ab6d-3c3c-42e6-8d90-1e82d6dd0c12', rite_efficacy_independent_of_office, instrumental).
narrative_ontology:cs_reference_frame('c450ab6d-3c3c-42e6-8d90-1e82d6dd0c12', domestic_practice_primacy).
narrative_ontology:cs_drift_state('c450ab6d-3c3c-42e6-8d90-1e82d6dd0c12', post_amarna_restoration, gap(revival_pressure, minor, false)).
narrative_ontology:cs_created_at('c450ab6d-3c3c-42e6-8d90-1e82d6dd0c12', '').
narrative_ontology:cs_kernel_id(divine_legitimacy_substrate__folk_syncretistic_reading, divine_legitimacy_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__folk_syncretistic_reading, village_households).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__folk_syncretistic_reading, household_heads).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__folk_syncretistic_reading, local_ritual_specialists).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__folk_syncretistic_reading, state_temple_establishment).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__folk_syncretistic_reading, pharaonic_state).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Families across a village enact the practice that constitutes their religious world: daily offerings at household shrines, ancestor remembrance, amulets for childbirth and protection, participation in festival days that mark the agricultural round. They give small material contributions and receive cohesion, lifecycle structure, and a working sense of divine order. Leaving the practice would mean leaving the community's shared time and their own inherited identity; there is nowhere else for it to happen.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, village_households, beneficiary,
    organized, generational, identity_locked, local).

% Senior household members conduct the daily rite, decide which deity to approach for which trouble, add or drop practices as need arises, and teach the next generation. Authority here is exercised by doing: no one appoints them, no council reviews them, and their adaptations spread by imitation rather than decree. They also hold the tradition they administer, having received it from their own parents.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, household_heads, agenda_setter,
    moderate, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__folk_syncretistic_reading, household_heads, beneficiary).

% Part-time village priests, festival leaders, amulet makers, and mortuary workers who provide services households request: officiating at rites of passage, supplying protective objects, preparing burials. They receive offerings, fees, and festival shares in return. They have no power to redefine what the practice requires; demand comes from the households, and their standing depends on serving it. Their livelihood is tied to the locality they serve.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, local_ritual_specialists, beneficiary,
    moderate, biographical, constrained, local).

% The great temple hierarchies administer a theology and a festival circuit that villages know from afar. They draw personnel and devotional traffic from the same population that practices at home, and their processions and oracles travel to the people rather than the reverse. They cannot dictate household observance; when popular devotion moves, the temples accommodate it. Their legitimacy borrows plausibility from a divine order continuously enacted in thousands of courtyards they do not control.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, state_temple_establishment, beneficiary,
    institutional, generational, arbitrage, national).

% Crown and court claim divine sanction for rule and fund state festivals that feed popular observance. The lived plausibility of divine order in village life is the currency their legitimacy claims spend, yet the practice that mints it sits beyond administrative reach. One reign attempted to replace the entire arrangement with a single exclusive cult dispensed from the palace; the villages kept their shrines, waited the reign out, and resumed openly afterward. The state pays for cult infrastructure and absorbs the lesson.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, pharaonic_state, beneficiary,
    institutional, generational, arbitrage, national).

% Estate-dependent workers join the festivals, need the same protections for birth and burial, and contribute what they can, but hold no household standing in the village: they do not help schedule the festival calendar, organize collections, or decide which rites a crisis warrants. Their interests surface only indirectly, through patrons or neighbors. If they objected to contribution expectations unaccompanied by voice, there is no seat from which to object.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, landless_laborers, excluded,
    powerless, biographical, trapped, local).

% Literate officials trained in temple schools who compose the surviving record: state theology, temple inventories, royal inscriptions. Household practice enters their documents only accidentally, in letters, wisdom sayings, and the occasional complaint. They see the elite religious world clearly and the village religious world obliquely, and everything later analysts know passes through that asymmetry.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, scribal_recorders, observer,
    moderate, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_legitimacy_substrate__folk_syncretistic_reading, local_ritual_specialists).
narrative_ontology:fixing_cost_class(divine_legitimacy_substrate__folk_syncretistic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates village religious life around shared sacred time and domestic protection: festival calendars synchronize agricultural and communal rhythms, household rites standardize responses to birth, illness, death, and misfortune, and deity-and-ancestor propitiation sustains kinship obligation networks. It solves, at negligible organizational overhead, the problem of maintaining access to divine order without waiting on distant temple service.
% TRANSFER_FUNCTION: Moves small material flows — grain offerings, festival contributions, amulet and mortuary-service fees — from households to local specialists and communal festival funds. It also moves assurance upward implicitly: elite legitimacy claims borrow plausibility from a divine order continuously enacted in village practice, though no goods travel to elites through this arrangement itself.
% ABSENT_VOICES: Landless laborers attached to estates share festival burdens and protection needs but hold no household standing in scheduling or organization; their objection would target contribution expectations decoupled from voice. Women, though principal practitioners of the domestic cult, are nearly unnamed in surviving texts — an archival absence rather than a practical one. Systematic theologians had no seat at village level; their categories simply never arrived.
% DISAPPEARANCE_RATIONALE: Village calendars, lifecycle transitions, and kinship obligations were timed and structured by the practice; specialists' livelihoods, the festival economy, and the plausibility conditions underlying elite legitimacy claims all rest on it. The Amarna episode shows the direction of dependence: when the palace attempted to suppress the arrangement and substitute its own, the villages persisted without organization or leadership, and the world rearranged around their persistence rather than around the reform.
% FOUNDING_PROBLEM: Making divine power locally actionable: securing protection for birth, harvest, illness, and death when temple intermediaries are distant, their services costly, and their calendars indifferent to village need.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set by material culture and by adversarial history: domestic shrine assemblages and Bes, Taweret, and Hathor amulets appear in excavated houses across status levels (Deir el-Medina, the Amarna workmen's village, provincial towns); wisdom texts and personal letters attest household-scale piety in elite-authored sources that had no reason to flatter it; and the practice's persistence through the Amarna interlude — when elite support was withdrawn and conformity was enforced — demonstrates the founding problem was being answered at the household level, not granted from above.
narrative_ontology:disappearance_verdict(divine_legitimacy_substrate__folk_syncretistic_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_legitimacy_substrate__folk_syncretistic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_legitimacy_substrate__folk_syncretistic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(divine_legitimacy_substrate__folk_syncretistic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_legitimacy_substrate__folk_syncretistic_reading, 0.28, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_legitimacy_substrate__folk_syncretistic_reading_tests).
:- end_tests(divine_legitimacy_substrate__folk_syncretistic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-to-modest (0.28 at interval end): the material flows are small, largely fee-for-service, and matched by goods received (protection rites, lifecycle structure, festival redistribution). Suppression is very low (0.13) and is authored as a raw structural property, unscaled by power or scope: syncretism absorbs rather than excludes, so the arrangement needs no enforcement machinery to persist — its rival needed one and died for it. Theater is low (0.20): the rites are functional for their participants, not performances maintaining an empty shell. Accessibility collapse is low (0.30): knowing how the arrangement works does not close alternatives, because the practice itself flexes — deities are added, dropped, and merged as need dictates. Resistance is low (0.15): little organized opposition exists because little coercion generates it; the notable resistance event ran the other way (villages passively resisting the Atenist reform). The temporal series run on ONE shared grid (t = 0, 5, 10, 15, 18, 22, 26, 30; the interval models the three decades bracketing the Amarna reform, with t=15–18 corresponding to the height of enforced Atenism), so every tracked metric is authored at every examined point. The series show a single externally-driven shock cycle, not an oscillating extraction mechanism: extractiveness, theater, and suppression all rise together during the reform window (covert practice carried risk and cost; outward conformity inflated the performative share; discretion required internal social policing), then decay together after the restoration. The cycle's driver is royal policy, not intermittent reinforcement by the arrangement itself — the arrangement's own baseline is flat, and that flatness is the diagnostic finding.
 *
 * PERSPECTIVAL GAP:
 *   From the elite seats the same religious world reads as flowing downward — legitimacy conferred on villages from temple and throne. From the practitioner seats it reads as made at home and merely borrowed by elites. The engine computes per-seat classifications from the structural data, and the seats should diverge: institutional seats carry arbitrage exit and incidental benefit (they can reframe doctrine around whatever the villages do); practitioner seats carry identity_locked exit and near-symmetric exchange (small gifts out, protection and cohesion in). The Amarna episode is the natural experiment that exposes the gap: when the elite reading attempted to become the only reading, the practitioner seats' unorganized persistence revealed where the authority actually resided — and the elite seats subsequently arbitraged back toward accommodation.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations map onto real structure. village_households and household_heads sit near the beneficiary pole: they give small offerings and receive cohesion, protection, and lifecycle structure — net gain, low d. local_ritual_specialists also sit low: they receive fees and shares but render requested services and cannot steer the practice. The two institutional seats are the subtlety. Deriving d from their beneficiary listing alone would place them near full subsidy (d approximately 0.1), but their gain is incidental legitimation drawn from a substrate they cannot administer, and they bear real costs: festival funding, cult upkeep, and — in the Amarna window — the fiscal and political weight of a reform the villages declined to adopt. A single directionality override at the institutional power atom (d = 0.30) corrects both seats: subsidized, but far short of capture, matching the declared structural delta that pharaoh and priesthood are alike treated as distant elites. No override is needed at the practitioner seats, where beneficiary-plus-exit derivation already lands correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   Two mislabels are guarded against. First, piton: the arrangement is old, diffuse, and leaderless, which invites an inertial-survival reading — but its function is demonstrably live (Q5 verdict world_rearranges; theater_ratio 0.20 and falling after the shock), and the piton cost-asymmetry test fails for a structural reason: there is no administrator who could change it more cheaply than keeping it, because there is no administrator. Second, snare: elite readings historically cast folk practice as error or credulity, and a hostile analyst could dress the specialist fees as exploitation — but no seat bears asymmetric extraction, the largest material flows are payment for requested services, and the arrangement persisted through an armed attempt to destroy it without ever deploying enforcement of its own. Claiming rope keeps the coordination function foregrounded while the omegas hold open the specialist-capture question that would legitimately move the computed type toward tangled_rope if the evidence ever showed rents above cost.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contingency,
    'This story instantiates one reading (folk_syncretistic) of the divine_legitimacy_substrate kernel; the sibling readings assign the same religious world entirely different beneficiary structures — priesthood-captured in the Amun reading, pharaoh-monopolized in the Atenist reading. Is the folk reading''s diffuse, unclear beneficiary structure a property of the arrangement itself, or an artifact of which seat the analyst occupies?',
    'Cross-reading comparison across the three constraint files: convergence on low extraction at the practitioner seat across all readings would confirm substrate diffuseness; divergence confined to the elite seats would locate the contest exactly where the siblings place it.',
    'If the substrate turns out to be administered somewhere (a temple branch, a festival association with dues power, a customary council), this reading''s type moves from rope toward tangled_rope and the beneficiary structure clarifies; if diffuseness holds, the elite readings stand as competing overlays on an unadministered base rather than descriptions of it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contingency, conceptual, 'Whether the folk reading''s unclear beneficiary structure is real or seat-relative.').

omega_variable(
    specialist_capture_ambiguity,
    'Do local ritual specialists merely recoup service costs (amulets, mortuary work, festival officiating) or capture returns above cost from household demand that cannot easily substitute?',
    'Administrative ostraca and accounts (Deir el-Medina work records, temple granary ledgers) pricing specialist services against comparable non-ritual labor; comparison of amulet and burial-goods prices across villages with and without resident specialists.',
    'Returns above cost would raise effective extraction borne at the household side and move the computed type toward tangled_rope; cost recoupment supports the rope claim and the fee-for-service reading of the receipt surface.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(specialist_capture_ambiguity, empirical, 'Whether specialist fees track service cost or rent.').

omega_variable(
    elite_source_bias,
    'Nearly all surviving texts were produced by the scribal elite, whose institutions this reading treats as distant; household practice is reconstructed mainly from archaeology. Does the authored picture describe the arrangement as it operated, or as the surviving filter permits?',
    'Continued synthesis of domestic assemblages, votive deposits, and workmen''s-village evidence against the textual claims; weighting settlement archaeology over temple archaeology when the two diverge.',
    'A severe filter could mean extractiveness and suppression are understated (covert conflict leaves few traces) or overstated (visible assemblages skew toward wealthier households); the beneficiary structure itself could shift if practice differed by class more than the current picture allows.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(elite_source_bias, empirical, 'Evidentiary bias toward elite-produced sources in reconstructing folk practice.').

omega_variable(
    absorption_vs_erasure,
    'The low suppression score rests on syncretism''s apparent absorptiveness — new deities and rites folded in rather than excluded. But absorption and erasure leave different traces: did the practice incorporate rivals willingly, or did resistant alternatives simply fail to survive in the record?',
    'Trace foreign and novel cult elements (Syrian, Nubian, Levantine deities) in domestic assemblages across the interval; distinguish contested adoption (new elements appearing first in marginal households) from uniform uptake.',
    'Genuine absorptiveness supports low suppression and the rope claim; erasure-driven apparent absorptiveness would raise suppression and reveal a harder edge to the arrangement than the metrics currently show.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(absorption_vs_erasure, conceptual, 'Whether syncretistic absorption reflects openness or survivorship bias.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_legitimacy_substrate__folk_syncretistic_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(folk_syncretic_tr_t0, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(folk_syncretic_tr_t5, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 5, 0.19).
narrative_ontology:measurement(folk_syncretic_tr_t10, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(folk_syncretic_tr_t15, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 15, 0.42).
narrative_ontology:measurement(folk_syncretic_tr_t18, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 18, 0.38).
narrative_ontology:measurement(folk_syncretic_tr_t22, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 22, 0.26).
narrative_ontology:measurement(folk_syncretic_tr_t26, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 26, 0.22).
narrative_ontology:measurement(folk_syncretic_tr_t30, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 30, 0.2).

% Extraction over time
narrative_ontology:measurement(folk_syncretic_be_t0, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 0, 0.24).
narrative_ontology:measurement(folk_syncretic_be_t5, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 5, 0.26).
narrative_ontology:measurement(folk_syncretic_be_t10, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 10, 0.27).
narrative_ontology:measurement(folk_syncretic_be_t15, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 15, 0.34).
narrative_ontology:measurement(folk_syncretic_be_t18, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 18, 0.33).
narrative_ontology:measurement(folk_syncretic_be_t22, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 22, 0.3).
narrative_ontology:measurement(folk_syncretic_be_t26, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 26, 0.29).
narrative_ontology:measurement(folk_syncretic_be_t30, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 30, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(folk_syncretic_su_t0, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(folk_syncretic_su_t5, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 5, 0.12).
narrative_ontology:measurement(folk_syncretic_su_t10, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 10, 0.13).
narrative_ontology:measurement(folk_syncretic_su_t15, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 15, 0.22).
narrative_ontology:measurement(folk_syncretic_su_t18, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 18, 0.2).
narrative_ontology:measurement(folk_syncretic_su_t22, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 22, 0.14).
narrative_ontology:measurement(folk_syncretic_su_t26, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 26, 0.13).
narrative_ontology:measurement(folk_syncretic_su_t30, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 30, 0.13).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_legitimacy_substrate__folk_syncretistic_reading, attachment_coordination).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__folk_syncretistic_reading, amun_polytheistic_reading).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__folk_syncretistic_reading, atenist_monotheistic_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the divine_legitimacy_substrate kernel. The colloquial label 'how divine legitimacy flowed in New Kingdom Egypt' covers three structurally distinct claims with distinct epsilon values and beneficiary structures: this folk reading (legitimacy through unmediated household/village practice; low extraction, diffuse benefit), amun_polytheistic_reading (legitimacy through priestly interpretive office; institutional rents ride on the mediation claim), and atenist_monotheistic_reading (legitimacy solely through royal revelation; totalizing, coerced, historically brief). The folk substrate is upstream of both elite readings in the dependency sense: each presupposes a world in which divine legitimacy is a meaningful currency, and that currency is minted in village practice. The Amarna episode is the family's natural experiment — the Atenist reading attempted to sever the dependency, could not, and the family re-equilibrated with the folk substrate intact. Each file links the other two via affects_constraints; epsilon values are authored independently per file and must not be reconciled.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(divine_legitimacy_substrate__folk_syncretistic_reading, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
