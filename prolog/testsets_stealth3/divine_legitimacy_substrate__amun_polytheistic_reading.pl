% ============================================================================
% CONSTRAINT STORY: divine_legitimacy_substrate__amun_polytheistic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_legitimacy_substrate__amun_polytheistic_reading, []).

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
 *   constraint_id: divine_legitimacy_substrate__amun_polytheistic_reading
 *   human_readable: Amun Priestly Validation Gate on Divine Kingship
 *   domain: religious/political/economic (ancient Near East)
 *
 * SUMMARY:
 *   After the expulsion of the Hyksos, the Theban war coalition's patron
 *   deity Amun was fused with the sun god as Amun-Ra and installed as chief
 *   of a broad, hierarchically ordered pantheon. Access to legitimate divine
 *   sanction — for accession, campaign, appointment, and judgment — ran
 *   exclusively through established priestly interpretation: college-trained
 *   lector priests posed questions before the god's barque, read its
 *   responses, and issued oracular ratification. Kings paid for validation in
 *   the coin the temples could bank permanently: land grants, tax exemptions,
 *   captive laborers, and construction. The arrangement accommodated regional
 *   variation (local gods continued under Amun's primacy, recast as
 *   manifestations of one divine order), which is precisely what kept
 *   alternatives from collapsing entirely. Across the interval mapped here
 *   (T0 approximately the accession of Ahmose I, ca. 1550 BCE; T30
 *   approximately the death of Ramesses XI, ca. 1070 BCE), the validation
 *   function remained real while the endowment ratchet compounded: by
 *   interval end the Amun complex at Karnak was the largest landholder in the
 *   country, exempt from crown assessment, and the High Priest of Amun
 *   commanded garrison and fleet at Thebes as a rival center of rule. KEY
 *   AGENTS (by structural relationship): - amun_priesthood: Primary agenda
 *   setter (institutional/constrained) — runs interpretive validation, staffs
 *   the largest administrative complex, ratifies appointments -
 *   theban_amun_estate: Primary beneficiary (institutional/constrained) —
 *   accumulates land, grain, labor, and exemptions; receives the transfer
 *   stream - pharaonic_monarch: Mixed seat (powerful/identity_locked) —
 *   declared victim paying legitimacy-price, secondary beneficiary receiving
 *   validation - royal_treasury: Fiscal payer (institutional/constrained) —
 *   loses assessable base to exemptions and grants -
 *   rural_producer_households: Burden-bearing payer with insurance offset
 *   (powerless/trapped) - regional_cult_establishments: Accommodated
 *   secondary beneficiaries (organized/constrained, regional) -
 *   subordinated_cult_priesthoods: Payer with retained endowments
 *   (organized/constrained, regional) - gods_wife_of_amun: Dual-positioned
 *   royal-temple hinge (powerful/identity_locked) -
 *   village_ritual_specialists: Excluded pragmatic practitioners
 *   (powerless/mobile, local)
 *
 * KEY AGENTS:
 *   - amun_priesthood: agenda setter (institutional/constrained) — interpretive validation authority
 *   - theban_amun_estate: primary beneficiary (institutional/constrained) — accumulating endowment economy
 *   - pharaonic_monarch: payer with secondary beneficiary position (powerful/identity_locked)
 *   - royal_treasury: fiscal payer (institutional/constrained)
 *   - rural_producer_households: payer with secondary beneficiary position (powerless/trapped)
 *   - regional_cult_establishments: secondary beneficiaries (organized/constrained, regional)
 *   - subordinated_cult_priesthoods: payer with retained endowments (organized/constrained, regional)
 *   - gods_wife_of_amun: beneficiary with payer secondary role (powerful/identity_locked)
 *   - village_ritual_specialists: excluded pragmatic practitioners (powerless/mobile, local)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_legitimacy_substrate__amun_polytheistic_reading, 0.68).
domain_priors:suppression_score(divine_legitimacy_substrate__amun_polytheistic_reading, 0.55).
domain_priors:theater_ratio(divine_legitimacy_substrate__amun_polytheistic_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_legitimacy_substrate__amun_polytheistic_reading, tangled_rope).
narrative_ontology:human_readable(divine_legitimacy_substrate__amun_polytheistic_reading, "Amun Priestly Validation Gate on Divine Kingship").
narrative_ontology:topic_domain(divine_legitimacy_substrate__amun_polytheistic_reading, "religious/political/economic (ancient Near East)").

domain_priors:requires_active_enforcement(divine_legitimacy_substrate__amun_polytheistic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__amun_polytheistic_reading, 'a80d38b0-6e41-4bc1-bda0-7a43d8f3318a').
narrative_ontology:cs_kernel_codification('a80d38b0-6e41-4bc1-bda0-7a43d8f3318a', distributed).
narrative_ontology:cs_authority_grounding('a80d38b0-6e41-4bc1-bda0-7a43d8f3318a', practice).
narrative_ontology:cs_interpretation_layer_present('a80d38b0-6e41-4bc1-bda0-7a43d8f3318a').
narrative_ontology:cs_reading_relation('a80d38b0-6e41-4bc1-bda0-7a43d8f3318a', divine_legitimacy_substrate__atenist_monotheistic_reading, forecloses).
narrative_ontology:cs_reading_relation('a80d38b0-6e41-4bc1-bda0-7a43d8f3318a', divine_legitimacy_substrate__folk_syncretistic_reading, coexists_with).
narrative_ontology:cs_axiom('a80d38b0-6e41-4bc1-bda0-7a43d8f3318a', foundational, many_gods_real_and_hierarchically_ordered).
narrative_ontology:cs_axiom_status(many_gods_real_and_hierarchically_ordered, holdable).
narrative_ontology:cs_axiom_grounding('a80d38b0-6e41-4bc1-bda0-7a43d8f3318a', many_gods_real_and_hierarchically_ordered, theological).
narrative_ontology:cs_axiom('a80d38b0-6e41-4bc1-bda0-7a43d8f3318a', foundational, legitimacy_requires_priestly_mediation).
narrative_ontology:cs_axiom_status(legitimacy_requires_priestly_mediation, holdable).
narrative_ontology:cs_axiom_grounding('a80d38b0-6e41-4bc1-bda0-7a43d8f3318a', legitimacy_requires_priestly_mediation, conventional).
narrative_ontology:cs_axiom('a80d38b0-6e41-4bc1-bda0-7a43d8f3318a', secondary, regional_cults_are_manifestations_of_the_amun_order).
narrative_ontology:cs_axiom_status(regional_cults_are_manifestations_of_the_amun_order, holdable).
narrative_ontology:cs_axiom_grounding('a80d38b0-6e41-4bc1-bda0-7a43d8f3318a', regional_cults_are_manifestations_of_the_amun_order, theological).
narrative_ontology:cs_reference_frame('a80d38b0-6e41-4bc1-bda0-7a43d8f3318a', amun_supremacy_with_college_mediation).
narrative_ontology:cs_drift_state('a80d38b0-6e41-4bc1-bda0-7a43d8f3318a', late_ramesside_administrative_crisis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a80d38b0-6e41-4bc1-bda0-7a43d8f3318a', '').
narrative_ontology:cs_kernel_id(divine_legitimacy_substrate__amun_polytheistic_reading, divine_legitimacy_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, amun_priesthood).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, theban_amun_estate).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, regional_cult_establishments).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, royal_treasury).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, rural_producer_households).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, subordinated_cult_priesthoods).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, pharaonic_monarch).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, rural_producer_households).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, subordinated_cult_priesthoods).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, gods_wife_of_amun).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, pharaonic_monarch).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, gods_wife_of_amun).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the divine will through oracular consultation, validates accessions, campaigns, appointments, and judgments, and maintains the festival calendar and daily liturgy. Recruits through hereditary scribal and priestly lines; leaving the college forfeits office, stipend, and standing. By interval end the senior college ratifies appointments and adjudicates theft cases by procession, and its high priest commands troops and river craft at Thebes.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, amun_priesthood, agenda_setter,
    institutional, generational, constrained, national).

% Holds granaries, herds, vineyards, workshops, ships, fields, and dependent laborers accumulated through successive royal land grants and tax exemptions. Converts each reign's legitimacy purchases into permanent endowment; by interval end it is the largest single landholding in the country, with income exempt from crown assessment and administered by the same personnel who run the validation apparatus.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, theban_amun_estate, beneficiary,
    institutional, generational, constrained, national).

% Reigns as the god's son by right, but every accession, coronation jubilee, campaign launch, and major appointment requires oracular ratification delivered through the colleges. Pays for validation with land grants, tax exemptions, and laborer allocations that permanently shrink the assessable crown domain, and receives in return the legitimation that makes reign possible at all. The office's identity is constituted by the validation it submits to: the one incumbent who attempted to re-found royal legitimacy outside the colleges was struck from the monuments after his death, and the office returned to the colleges' gate.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, pharaonic_monarch, payer,
    powerful, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__amun_polytheistic_reading, pharaonic_monarch, beneficiary).

% Administers crown revenue, corvee schedules, and grain assessment through scribal bureaus. Every new exemption and endowment removes land and laborers from its assessable base forever; by the late Ramesside phase its ledgers show grain shortages coexisting with full temple storehouses. Its recourse is limited to commissioning investigations of precinct corruption, which document losses without recovering the base.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, royal_treasury, payer,
    institutional, generational, trapped, national).

% Farm temple and crown fields, deliver corvee labor for temple construction, and pay tithes and offerings assessed through temple administration. Receive in return festival rations, amuletic and funerary services, granary relief in bad flood years, and a ritual calendar that organizes the agricultural year. Movement off the land is bounded by corvee registration and the geography of the valley; obligations are recorded in ration and tax ledgers kept by the same scribes who administer the temples.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, rural_producer_households, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__amun_polytheistic_reading, rural_producer_households, beneficiary).

% Provincial temples — Ptah's house at Memphis, the sun temple at Heliopolis, Osiris's precinct at Abydos, and the nome gods — keep their cults, feasts, endowments, and pilgrim traffic provided they acknowledge Amun-Ra's primacy and route high theology through Thebes. Their local deities are recast as forms of the one divine order, which preserves their standing while binding it to the senior college's approval. They supply priests and scribes to the wider system and receive residual royal patronage.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, regional_cult_establishments, beneficiary,
    organized, generational, constrained, regional).

% Clergies of the old centers keep their offices, titles, and endowed incomes but cede theological primacy and first claim on royal patronage to Thebes. Their doctrines are harmonized into the Amun-centered cosmology whether or not their own traditions ranked their gods first, and dissent from the ordering costs preferment and posting. Retained endowments give them a durable stake in the system's continuation despite the subordination.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, subordinated_cult_priesthoods, payer,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__amun_polytheistic_reading, subordinated_cult_priesthoods, beneficiary).

% A royal woman installed as the god's consort, holding dedicated estates and performing rites that bind palace to temple. The office requires celibacy and passes by adoption rather than birth, so she surrenders marriage and lineage for estate income and standing at the hinge between the two power centers. Her position depends entirely on the validation structure she helps stage; she cannot hold the office and oppose the apparatus that defines it.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, gods_wife_of_amun, beneficiary,
    powerful, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__amun_polytheistic_reading, gods_wife_of_amun, payer).

% Local mediums, festival organizers, healers, and amulet sellers conduct pragmatic plural practice in villages throughout the valley. Households rely on them for everyday protection and negotiation with the unseen, but their practice carries no validating authority for anyone above the village, enters no canonical record, and earns no preferment. They move and adapt freely; what they lack is a seat at the interpretive table whose decisions bind the kingdom.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, village_ritual_specialists, excluded,
    powerless, biographical, mobile, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_legitimacy_substrate__amun_polytheistic_reading, theban_amun_estate).
narrative_ontology:fixing_cost_class(divine_legitimacy_substrate__amun_polytheistic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a shared sacred order across a reunified kingdom: synchronizes the festival and flood-cycle calendar, arbitrates succession through oracular validation so royal deaths do not automatically become wars, stores and redistributes grain against famine, trains the scribal class, and adjudicates disputes through temple courts. Stated without evaluation of how the costs are distributed.
% TRANSFER_FUNCTION: Moves surplus — grain, cattle, labor-days, and land tenure — from rural producer households and the royal fisc to temple estates, overwhelmingly the Amun complex, via offerings, tithes, corvee, and perpetual tax-exempt endowments; and moves validated authority downward from oracular pronouncement to king, officials, and judges.
% ABSENT_VOICES: Village ritual specialists whose pragmatic pluralism the canon never credits would object that binding legitimacy to college interpretation starves working religion of standing; they are present everywhere in the villages and nowhere in the record. Rural households appear only as ration-line entries and corvee quotas — their objections surface solely as the rare organized work stoppage. Subordinated clergy at Heliopolis and Memphis accept the ordering in public doctrine while their own traditions ranked their gods differently; their dissent survives only as harmonization friction.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, successions would lose their arbiter and resolve by force among claimants; the synchronized festival and flood-calendar would fragment into local cycles; the redistribution and famine-relief economy would collapse into local hoarding while the crown briefly recovered its assessment base; regional cults would re-anchor around their own patrons without the Theban umbrella; household practice would continue largely unchanged, since it never depended on the colleges' gate. The elite and fiscal layers rearrange; the substratum adapts.
% FOUNDING_PROBLEM: Bind a newly reunified kingdom to a single sacred order and secure peaceful succession: reward the Theban war coalition by elevating its patron god to primacy, and institutionalize an interpretive authority capable of arbitrating legitimacy beyond the lifespan of any individual reign or dynasty.
% FOUNDING_PROBLEM_CORROBORATION: The colleges attest the problem is live and that only enlarging the sacred establishment meets it. Sources outside the benefiting parties dispute the arrangement's continuing fitness: the crown's own investigation records (the tomb-robbery commissions of the late Ramesside era) document precinct corruption and lost revenue; the ration-strike ledger from the Theban workmen's community records labor withheld because the state that fed its workers stood empty beside full temple granaries; and foreign diplomatic correspondence of the period treats the ideology as an instrument to be managed rather than a reality to be honored. No corroborating voice from outside the beneficiary set attests that the founding problem was still being solved by the arrangement as constituted at interval end.
narrative_ontology:disappearance_verdict(divine_legitimacy_substrate__amun_polytheistic_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_legitimacy_substrate__amun_polytheistic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_legitimacy_substrate__amun_polytheistic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(divine_legitimacy_substrate__amun_polytheistic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_legitimacy_substrate__amun_polytheistic_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_legitimacy_substrate__amun_polytheistic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_legitimacy_substrate__amun_polytheistic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_legitimacy_substrate__amun_polytheistic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.68 at interval end) because the transfer stream was decoupled from service rendered: exemption and endowment compounded regardless of the validation workload, and by the late Ramesside phase the estate's income dwarfed any plausible cost of ritual maintenance. Suppression (0.55) is authored as a raw structural property — it is NOT scaled by power or scope in the engine's computation — reflecting real but bounded coercive machinery: restoration-era orthodoxy policing after the Amarna rupture, oracle protocol enforced on officials, and by interval end a garrisoned high priesthood; regional variation was tolerated, so suppression never approached total. Theater_ratio (0.42) is moderate-high: the daily liturgy, festival calendar, and granary operations were functionally load-bearing, while a growing share of oracular activity ratified decisions already taken — the processions deciding appointments and theft trials by the god's nod are the documented signature of that shift. Accessibility_collapse (0.52) is middling because the reading deliberately accommodated regional and household alternatives inside the umbrella: exit was foreclosed at the kingship layer (no incumbent could reign without validation) but remained open beneath it. Resistance (0.54) reflects documented pushback: the first recorded labor strike (the Deir el-Medina workmen's sit-down over unpaid rations while temple granaries stood full), the crown's tomb-robbery commissions documenting precinct corruption, and finally the high priesthood's assumption of royal style at Thebes. All three temporal series run on one shared seven-point grid (T0, 5, 10, 15, 20, 25, 30) so every metric is authored at every examined time point; the trajectories are monotonic, modeling accumulation rather than oscillation. Anchoring assumptions stated openly: T0-T5 corresponds to the early 18th Dynasty consolidation, T10 to imperial spoil-flow under Thutmose III, T15 to the Amarna rupture and restoration, T20 to the peak endowment era of Ramesses II, T25 to the Medinet Habu donation inscriptions coinciding with the Year-29 strike, and T30 to the late Ramesside administrative collapse.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the college seat, the arrangement IS the cosmos working: daily rites maintained the ordered world, and validation was a service the colleges performed, not a toll they levied. From the royal-fiscal seat, the same structure is a ratchet converting sovereignty into perpetual endowment — every grant bought today's legitimacy with tomorrow's revenue base. From the rural household seat, the system mixes corvee and tithe against granary relief and a festival calendar that structured the year; the omega on insurance-versus-burden marks this as genuinely unresolved. On coalition capacity: the powerless payer class was not without collective leverage — the Deir el-Medina strike shows organized work-stoppage extracting concessions from the state at the moment ration delivery failed — but that coalition capacity remained localized to state-employed specialist communities and did not generalize across the corvee-bound peasantry. The engine computes per-seat types from the structural declarations; this commentary explains why divergence is expected rather than adjudicating it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: the priesthood and the estate sit at the subsidy end (the arrangement channels resources toward them), regional establishments somewhat higher but still beneficiary-side. Victim declarations drive high directionality: rural households derive a high target value tempered by their declared secondary beneficiary position (insurance goods); the royal treasury is the purest fiscal target — institutional power, but no exit, since refusing the next grant means contesting the next succession without an arbiter. The pharaoh's seat is deliberately mixed: declared victim (pays the legitimacy-price), secondary beneficiary (receives validation), and identity-locked, because the office is constituted by the validation it submits to — the one ruler who attempted to re-found the office outside the colleges was erased from the monuments posthumously, which is the strongest available evidence that exit from this seat destroys the seat. Subordinated clergy derive high-moderate directionality tempered by retained endowments. No directionality_overrides are authored: the derivation chain produces the right qualitative structure from the beneficiary/victim data alone, and the override mechanism acts at power-atom granularity, which would misprice the deliberately mixed seats (an override on 'powerful' would drag the pure-payer treasury along with the mixed monarch).
 *
 * MANDATROPHY ANALYSIS:
 *   The classification discipline cuts both ways here. Reading the arrangement as pure extraction (snare) would erase the real coordination it performed: succession arbitration that prevented every royal death from becoming a war, a synchronized festival and flood-cycle calendar, grain storage and famine relief, scribal training, and dispute adjudication through temple courts — functions documented doing work throughout the interval. Reading it as pure coordination (rope) would erase the ratchet: exemptions that never expired, grants sized to crises rather than service costs, and a validation apparatus that increasingly ratified rather than decided. Tangled rope holds both facts in one structure. The Mandatrophy question — whether the founding mandate outlived its function — is routed to the R5 fields rather than keyed to any metric: the founding problem (bind the kingdom under a shared sacred order and arbitrate succession) remained live at interval end, but the parties dispute whether the arrangement still solved it, and the theater_ratio trajectory documents the function thinning. If the oracle_precommitment_rate omega resolves high, the late-phase arrangement drifts toward theatrical maintenance of a validation function that no longer governs — the signature the piton category watches for.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the kernel divine_legitimacy_substrate — specifically amun_polytheistic_reading. What would change structurally if a sibling reading (atenist_monotheistic_reading or folk_syncretistic_reading) were instantiated as the standing arrangement?',
    'Comparative classification across the three sibling stories once generated: the Atenist reading relocates interpretive authority wholly into the royal person (victim set flips to include the colleges; epsilon rises sharply for clerical seats); the folk reading shifts the referent to household practice (elite extraction largely leaves the picture). The disagreement is located in WHO interprets: college mediation versus royal revelation versus household pragmatism.',
    'If a sibling reading became the standing arrangement, this story''s beneficiary and victim declarations invert or dissolve; the current classification holds only for the college-mediated polytheistic arrangement described here.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer-frame routing: this story is one indexed reading of a contested kernel, not the kernel itself.').

omega_variable(
    estate_expansion_mechanism,
    'Was the growth of the Amun estate driven by a legitimacy-price feedback (each king purchasing validation with land, raising the price for successors) or by administrative capture (temple administrators diverting crown revenue under cover of piety)?',
    'Compare endowment rates under strong versus weak monarchs across the interval: if weak kings granted disproportionately more, capture dominates; if granting tracked genuine crises of succession uniformly, feedback pricing dominates.',
    'Feedback pricing supports the tangled_rope reading (coordination with a compounding coordination cost); confirmed capture pushes the payer seats toward snare-flavored computation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(estate_expansion_mechanism, empirical, 'Whether estate growth reflects purchased legitimacy or diverted revenue.').

omega_variable(
    oracle_precommitment_rate,
    'What fraction of oracular decisions were genuine adjudication versus staged ratification of decisions already taken by officials?',
    'Cross-check published oracular decrees against independent administrative outcomes; identify processions where the god''s motion contradicted known prior intent versus cases where the query was framed so only one answer was available.',
    'A high pre-commitment rate raises the effective theater_ratio at the validation layer and flags drift toward theatrical maintenance of a validation function that no longer decides anything.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oracle_precommitment_rate, empirical, 'Genuine oracle adjudication versus staged ratification.').

omega_variable(
    peasant_insurance_vs_burden,
    'Did rural producer households experience the temple system net as burden (corvee, tithes, offerings) or net as insurance (granary famine relief, festival rations, funerary services)?',
    'Settlement archaeology and ration-ledger comparison against documented famine-year granary releases; regional variation in corvee assessment versus relief incidence.',
    'If insurance dominates locally, the payer seat''s effective position sits nearer symmetric than the victim declaration alone would derive; the classification survives but seat divergence widens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(peasant_insurance_vs_burden, empirical, 'Net lived position of the rural payer seat.').

omega_variable(
    legitimacy_price_reversibility,
    'Could a sufficiently strong crown have clawed back temple tax exemptions and endowment lands without unmaking its own legitimacy, or was the ratchet irreversible within this reading''s own terms?',
    'Extended-record comparison: later episodes of recentralization (Saite-era restoration) show clawback was possible once a NEW legitimacy source was available, suggesting irreversibility was conditional on the absence of an alternative validator.',
    'If reversible only via alternative validation, fixing_cost is genuinely prohibitive from inside this reading; if reversible by fiat, part of the measured persistence was enforcement laziness rather than structural lock.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_price_reversibility, empirical, 'Whether the endowment ratchet could be unwound from within the reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_legitimacy_substrate__amun_polytheistic_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(divi_tr_t5, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 5, 0.24).
narrative_ontology:measurement(divi_tr_t10, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 10, 0.27).
narrative_ontology:measurement(divi_tr_t15, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 15, 0.31).
narrative_ontology:measurement(divi_tr_t20, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(divi_tr_t25, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 25, 0.39).
narrative_ontology:measurement(divi_tr_t30, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 30, 0.42).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(divi_be_t5, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(divi_be_t10, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 10, 0.47).
narrative_ontology:measurement(divi_be_t15, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 15, 0.53).
narrative_ontology:measurement(divi_be_t20, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(divi_be_t25, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 25, 0.64).
narrative_ontology:measurement(divi_be_t30, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t0, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(divi_su_t5, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 5, 0.37).
narrative_ontology:measurement(divi_su_t10, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(divi_su_t15, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 15, 0.44).
narrative_ontology:measurement(divi_su_t20, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(divi_su_t25, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 25, 0.52).
narrative_ontology:measurement(divi_su_t30, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 30, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_legitimacy_substrate__amun_polytheistic_reading, identity_coordination).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__amun_polytheistic_reading, atenist_monotheistic_reading).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__amun_polytheistic_reading, folk_syncretistic_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial concept 'Egyptian divine legitimacy' decomposes into three structurally distinct constraints sharing one kernel (divine_legitimacy_substrate). This file is the long-standing baseline reading (amun_polytheistic_reading): college-mediated validation, temple-economy beneficiaries, accommodated regional variation. The Atenist sibling inverts the structure — interpretive monopoly collapses into the royal person and the colleges flip into the victim set — which is why the baseline reading functions as its upstream: the Atenist episode is intelligible only as an attack on this arrangement's beneficiary structure, and the post-Amarna restoration (with its orthodoxy policing, visible in the suppression_requirement series) is this constraint reasserting itself against the sibling. The folk sibling operates beneath both as a parallel stratum with negligible elite extraction. Each file carries its own stable epsilon per DP-001; the family links here preserve the decomposition trail.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
