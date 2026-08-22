% ============================================================================
% CONSTRAINT STORY: hagia_sophia_substrate__islamic_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hagia_sophia_substrate__islamic_sovereignty_reading, []).

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
 *   constraint_id: hagia_sophia_substrate__islamic_sovereignty_reading
 *   human_readable: Hagia Sophia Substrate — Islamic Sovereignty Reading
 *   domain: cultural_heritage/sovereignty/religious_authority
 *
 * SUMMARY:
 *   This constraint story captures the Islamic sovereignty reading of the
 *   Hagia Sophia substrate: the site's legitimacy derives from the 1453
 *   Ottoman conquest and Mehmed II's waqf endowment, making it inalienable
 *   Islamic worship space under Turkish state authority. The 2020
 *   reconversion by executive decree (validated by the Council of State's
 *   reversal of the 1934 museum ruling) operationalizes this reading as state
 *   policy. The constraint is a tangled_rope: it coordinates Islamic worship
 *   and Sunni symbolic unity (genuine coordination function) while extracting
 *   access, narrative control, and heritage authority from non-Muslim
 *   visitors, UNESCO, and secularist Turks (asymmetric extraction), all
 *   maintained by active enforcement (prayer-time closures, mosaic coverings,
 *   security perimeters, legal exclusion of rival claimants).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hagia_sophia_substrate__islamic_sovereignty_reading, 0.68).
domain_priors:suppression_score(hagia_sophia_substrate__islamic_sovereignty_reading, 0.62).
domain_priors:theater_ratio(hagia_sophia_substrate__islamic_sovereignty_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_substrate__islamic_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(hagia_sophia_substrate__islamic_sovereignty_reading, "Hagia Sophia Substrate — Islamic Sovereignty Reading").
narrative_ontology:topic_domain(hagia_sophia_substrate__islamic_sovereignty_reading, "cultural_heritage/sovereignty/religious_authority").

domain_priors:requires_active_enforcement(hagia_sophia_substrate__islamic_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hagia_sophia_substrate__islamic_sovereignty_reading, 'b25ceb5a-d692-4faf-a9f4-157a1f33390c').
narrative_ontology:cs_kernel_codification('b25ceb5a-d692-4faf-a9f4-157a1f33390c', formalized).
narrative_ontology:cs_authority_grounding('b25ceb5a-d692-4faf-a9f4-157a1f33390c', lineage).
narrative_ontology:cs_interpretation_layer_present('b25ceb5a-d692-4faf-a9f4-157a1f33390c').
narrative_ontology:cs_reading_relation('b25ceb5a-d692-4faf-a9f4-157a1f33390c', hagia_sophia_substrate__orthodox_restitution_reading, forecloses).
narrative_ontology:cs_reading_relation('b25ceb5a-d692-4faf-a9f4-157a1f33390c', hagia_sophia_substrate__universal_heritage_reading, influences).
narrative_ontology:cs_axiom('b25ceb5a-d692-4faf-a9f4-157a1f33390c', foundational, conquest_waqf_establishes_perpetual_islamic_sovereignty).
narrative_ontology:cs_axiom_status(conquest_waqf_establishes_perpetual_islamic_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('b25ceb5a-d692-4faf-a9f4-157a1f33390c', conquest_waqf_establishes_perpetual_islamic_sovereignty, theological).
narrative_ontology:cs_axiom('b25ceb5a-d692-4faf-a9f4-157a1f33390c', foundational, turkish_state_is_legitimate_successor_to_ottoman_caliphate_waqf_authority).
narrative_ontology:cs_axiom_status(turkish_state_is_legitimate_successor_to_ottoman_caliphate_waqf_authority, holdable).
narrative_ontology:cs_axiom_grounding('b25ceb5a-d692-4faf-a9f4-157a1f33390c', turkish_state_is_legitimate_successor_to_ottoman_caliphate_waqf_authority, conventional).
narrative_ontology:cs_reference_frame('b25ceb5a-d692-4faf-a9f4-157a1f33390c', mehmed_ii_waqf_founding).
narrative_ontology:cs_drift_state('b25ceb5a-d692-4faf-a9f4-157a1f33390c', republican_secularization_1934, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('b25ceb5a-d692-4faf-a9f4-157a1f33390c', '').
narrative_ontology:cs_kernel_id(hagia_sophia_substrate__islamic_sovereignty_reading, hagia_sophia_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, akp_political_coalition).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, turkish_islamic_constituency).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, sunni_ummah_symbolic).
narrative_ontology:constraint_victim(hagia_sophia_substrate__islamic_sovereignty_reading, non_muslim_visitors).
narrative_ontology:constraint_victim(hagia_sophia_substrate__islamic_sovereignty_reading, unesco_regime).
narrative_ontology:constraint_victim(hagia_sophia_substrate__islamic_sovereignty_reading, secularist_turks).
narrative_ontology:constraint_vindicates(hagia_sophia_substrate__islamic_sovereignty_reading, ottoman_conquest_legitimacy_doctrine).
narrative_ontology:constraint_vindicates(hagia_sophia_substrate__islamic_sovereignty_reading, waqf_inalienability_principle).
narrative_ontology:constraint_vindicates(hagia_sophia_substrate__islamic_sovereignty_reading, turkish_state_religious_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The AKP government initiated and executes the 2020 reconversion via executive decree and court ruling, framing it as restoration of sovereign rights. The move consolidates the party's religious-nationalist base, signals leadership of the Sunni world, and converts a secular-era museum into an active symbol of Islamic sovereignty. The coalition controls the enforcement machinery (Diyanet, courts, security) and faces no meaningful domestic exit from the policy.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, akp_political_coalition, agenda_setter,
    institutional, generational, arbitrage, national).

% Millions of Turkish Muslims experience the reconversion as the rightful return of a sacred space. Friday prayers at Hagia Sophia are a lived validation of identity. Exit is identity-locked: opposing the reconversion would fracture communal and religious self-understanding. The constituency receives symbolic and spiritual benefit without bearing enforcement costs.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, turkish_islamic_constituency, beneficiary,
    organized, biographical, identity_locked, national).

% Broader Sunni Muslim publics treat Hagia Sophia's status as a barometer of Islamic civilizational dignity. The reconversion is celebrated across the Muslim world as a symbolic victory. This constituency is not subject to Turkish enforcement; its exit options are arbitrage-grade (it can shift symbolic allegiance without material cost), but the identity resonance makes disengagement unlikely.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, sunni_ummah_symbolic, beneficiary,
    organized, civilizational, arbitrage, global).

% Christian pilgrims, tourists, and art historians face restricted access during prayer times, covering of figurative mosaics, and a worship environment that centers Islamic ritual. The site's universal accessibility — the museum-era promise — is partially withdrawn. Exit is constrained: they can visit at limited times or not at all, but cannot access the site on their own terms.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, non_muslim_visitors, payer,
    powerless, immediate, constrained, global).

% UNESCO's World Heritage framework relies on state parties preserving outstanding universal value (OUV) through agreed management plans. Turkey's unilateral reconversion without prior consultation breaches the 1972 Convention's spirit and the 2019 Committee decision urging dialogue. UNESCO's authority to govern heritage is eroded; its exit is constrained — it can list the site as 'in danger' or delist, but lacks enforcement leverage against a sovereign state.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, unesco_regime, payer,
    institutional, generational, constrained, global).

% Turkish citizens who identify with the Republic's secular founding principle experience the reconversion as an ideological defeat — the reversal of Atatürk's 1934 museum decree, which symbolized a modern, pluralist civic identity. Their exit is identity-locked: leaving the country or abandoning secular identity are the only ways to escape the symbolic loss. They bear the cost of a redefined national narrative without recourse.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, secularist_turks, payer,
    organized, biographical, identity_locked, national).

% The Ecumenical Patriarchate of Constantinople claims spiritual continuity with the Byzantine cathedral. It has no legal standing in Turkish administrative proceedings and was not consulted on the reconversion. Its exit is trapped: it cannot relocate its historical claim, cannot enforce access, and operates under Turkish state surveillance. It would object to Islamic exclusivity but is structurally excluded from the decision.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, orthodox_patriarchate, excluded,
    moderate, generational, trapped, regional).

% Academics and conservation professionals monitor the site's physical condition, mosaic preservation, and management transparency. They hold no decision power but produce the epistemic record UNESCO and others rely on. Their analytical exit is unconstrained — they can publish, dissent, or withdraw attention without personal cost.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, international_heritage_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a shared Islamic worship space for Turkish Muslims and a global symbolic anchor for Sunni identity, resolving the ambiguity of a contested site by locking it into a single sovereign-religious frame backed by state enforcement.
% TRANSFER_FUNCTION: Transfers control of access, narrative, and symbolic capital from a pluralist museum framework (UNESCO-managed, secular-state administered) to an exclusive Islamic worship framework (Turkish state + Diyanet administered). The transfer moves authority from an international heritage regime to a national-religious sovereignty claim.
% ABSENT_VOICES: The Orthodox Patriarchate and global Orthodox communion are structurally excluded — they would claim continuity with the Byzantine cathedral and demand shared custody or neutrality. Armenian and other Eastern Christian communities with historical ties to the site are similarly absent. Their exclusion is maintained by Turkish state sovereignty and the waqf legal frame that treats the site as inalienable Islamic endowment.
% DISAPPEARANCE_RATIONALE: If the Islamic sovereignty reading vanished overnight, the site would revert to a contested status requiring new adjudication: museum, shared worship, or international administration. The Turkish state would lose a flagship sovereignty symbol; the AKP would lose a core identity deliverable; UNESCO would regain jurisdiction; non-Muslim visitors would regain full access. The world would rearrange around a new settlement.
% FOUNDING_PROBLEM: The 1453 conquest established Ottoman-Islamic sovereignty over Constantinople; the waqf endowment by Mehmed II locked the site into Islamic worship in perpetuity. The 1934 museum conversion by the secular Republic violated that founding endowment and the conquest's symbolic legacy. The reconversion restores the original founding frame.
% FOUNDING_PROBLEM_CORROBORATION: The waqf deed and conquest narrative are attested by Ottoman archival records and continuous Islamic jurisprudence (recognized by Diyanet and mainstream Sunni scholarship). The secularist counter-narrative (Atatürk's 1934 decree as modernizing act) is attested by Republican archives and CHP tradition. Both sides have institutional corroboration; the founding problem is contested, not settled.
narrative_ontology:disappearance_verdict(hagia_sophia_substrate__islamic_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(hagia_sophia_substrate__islamic_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hagia_sophia_substrate__islamic_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(hagia_sophia_substrate__islamic_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hagia_sophia_substrate__islamic_sovereignty_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hagia_sophia_substrate__islamic_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hagia_sophia_substrate__islamic_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hagia_sophia_substrate__islamic_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68) reflects the transfer of a globally significant heritage asset from pluralist management to exclusive religious-nationalist control — the commission is not monetary but symbolic and jurisdictional. Suppression (0.62) captures the active enforcement needed to maintain exclusivity: prayer-time closures, mosaic covering, legal barriers to UNESCO oversight, and the structural exclusion of the Patriarchate. Theater ratio (0.42) is significant: the 'restoration of worship' framing performs piety while the political consolidation function runs underneath; the site's physical conservation (a genuine coordination need) is subordinated to the sovereignty signal. Accessibility collapse (0.58) is moderate — alternatives (virtual access, limited visiting hours, other Byzantine sites) persist but the prime experience is foreclosed. Resistance (0.71) is high: UNESCO condemnation, Orthodox protests, secularist opposition, and scholarly critique constitute sustained pushback.
 *
 * PERSPECTIVAL GAP:
 *   From the AKP/Diyanet seat, the constraint is a restored rope — genuine coordination of worship and identity, minimal extraction (the waqf is the original deal). From non-Muslim visitor and UNESCO seats, it is a snare — extraction of access and heritage authority under cover of sovereignty. From secularist Turks, it is a snare with identity-locked extraction. The engine computes this seat divergence from the authored power/exit/role data; the claimed_type (tangled_rope) reflects the authoring seat's structural judgment that both coordination and extraction are real and non-reducible.
 *
 * DIRECTIONALITY LOGIC:
 *   The AKP coalition is the agenda-setter with arbitrage-grade exit (it could reverse course but pays high political cost). The Turkish Islamic constituency is identity-locked beneficiary — exit fractures religious self-concept. The Sunni ummah is symbolic beneficiary with arbitrage exit. Non-Muslim visitors are constrained payers (limited access, no voice). UNESCO is constrained institutional payer (authority eroded, limited leverage). Secularist Turks are identity-locked payers (ideological defeat, no domestic recourse). The Orthodox Patriarchate is trapped excluded (spiritual claim with no legal standing). Directionality derives from these structural positions: the constraint subsidizes identity-locked beneficiaries and extracts from constrained/trapped payers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1453 conquest + waqf inalienability) remains live per this reading — the endowment's terms are perpetual in Islamic law. The 1934 museum conversion is the mandatrophic event: a secular state repurposed an Islamic endowment for a civic function the endowment never authorized. The reconversion resolves that mandatrophy by restoring the original function. However, the constraint now serves a secondary political consolidation function (AKP identity delivery) that the original waqf did not anticipate — a new extraction layer atop the restored coordination. This dual character (restored original function + novel political extraction) is why tangled_rope fits: the coordination is genuine (worship happens, identity coheres) and the extraction is real (access restricted, UNESCO excluded, secularists defeated).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    waqf_perpetuity_vs_sovereign_modification,
    'Does the waqf''s perpetual inalienability under Islamic law bind the Turkish state (as successor to the Ottoman caliphate) against its own sovereign power to modify the endowment''s terms?',
    'Comparative analysis of Ottoman waqf law, Republican-era secularization of waqfs (1924 Law on Unification of Education, 2011 General Directorate of Foundations reforms), and contemporary Turkish constitutional court jurisprudence on waqf mutability.',
    'If the waqf binds the state, the reconversion is legally compelled (low extraction, coordination-dominant). If the state can modify the waqf, the reconversion is a sovereign choice using the waqf as cover (high extraction, snare-leaning).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(waqf_perpetuity_vs_sovereign_modification, conceptual, 'Whether the founding endowment constrains the modern state or the state reinterprets the endowment.').

omega_variable(
    mosaic_preservation_under_islamic_worship,
    'Can Byzantine figurative mosaics be permanently preserved under active Islamic worship practice, given the theological prohibition on figural representation in prayer spaces?',
    'Long-term monitoring of mosaic condition under the current covering/uncovering regime; comparative study of other converted Byzantine churches (e.g., Chora Church, Fethiye Mosque) where mosaics remain covered or are at risk.',
    'If preservation fails, the constraint extracts irreversible cultural loss from humanity (UNESCO OUV criterion) — extraction becomes destructive, not just allocative. If preservation succeeds, the coordination function includes heritage stewardship.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mosaic_preservation_under_islamic_worship, empirical, 'Whether the constraint''s physical stewardship matches its heritage obligations.').

omega_variable(
    akp_constituency_identity_lock_depth,
    'Is the Turkish Islamic constituency''s identity lock to this constraint theological (waqf inalienability as divine command) or political (AKP deliverable as proof of movement victory)?',
    'Sociological study of constituency discourse: does opposition to reconversion correlate with theological dissent or political opposition? Track fatwa authority and grassroots religious opinion vs. party loyalty signals.',
    'If theological, the identity lock is deep and the constraint is stable across political cycles. If political, the lock may fracture if AKP loses power or rebrands — the constraint could become a piton or scaffold.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(akp_constituency_identity_lock_depth, empirical, 'Depth and source of the beneficiary constituency''s identity lock.').

omega_variable(
    unesco_enforcement_leverage,
    'Does UNESCO retain any effective leverage over Turkish state behavior at World Heritage sites, or has the Hagia Sophia precedent established that sovereignty claims trump Convention obligations?',
    'Track UNESCO Committee decisions on Hagia Sophia (2021, 2022, 2023, 2024 sessions), Turkish compliance with reporting requests, and any tangible consequences (funding, technical assistance, diplomatic pressure). Compare with other ''in danger'' listings.',
    'If leverage is nil, UNESCO is a trapped payer with no exit — the constraint extracts its authority costlessly. If leverage persists, UNESCO remains a constrained but active institutional counterweight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unesco_enforcement_leverage, empirical, 'Whether the international heritage regime has teeth against sovereign religious claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_substrate__islamic_sovereignty_reading, 1453, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hagia_sophia_substrate__islamic_sovereignty_reading_tr_t1453, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 1453, 0.05).
narrative_ontology:measurement(hagia_sophia_substrate__islamic_sovereignty_reading_tr_t1934, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 1934, 0.1).
narrative_ontology:measurement(hagia_sophia_substrate__islamic_sovereignty_reading_tr_t1985, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 1985, 0.15).
narrative_ontology:measurement(hagia_sophia_substrate__islamic_sovereignty_reading_tr_t2013, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 2013, 0.22).
narrative_ontology:measurement(hagia_sophia_substrate__islamic_sovereignty_reading_tr_t2020, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 2020, 0.38).
narrative_ontology:measurement(hagia_sophia_substrate__islamic_sovereignty_reading_tr_t2024, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(hagia_sophia_substrate__islamic_sovereignty_reading_be_t1453, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 1453, 0.15).
narrative_ontology:measurement(hagia_sophia_substrate__islamic_sovereignty_reading_be_t1934, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 1934, 0.05).
narrative_ontology:measurement(hagia_sophia_substrate__islamic_sovereignty_reading_be_t1985, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 1985, 0.12).
narrative_ontology:measurement(hagia_sophia_substrate__islamic_sovereignty_reading_be_t2013, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 2013, 0.28).
narrative_ontology:measurement(hagia_sophia_substrate__islamic_sovereignty_reading_be_t2020, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 2020, 0.62).
narrative_ontology:measurement(hagia_sophia_substrate__islamic_sovereignty_reading_be_t2024, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(hagia_sophia_substrate__islamic_sovereignty_reading_su_t1453, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 1453, 0.2).
narrative_ontology:measurement(hagia_sophia_substrate__islamic_sovereignty_reading_su_t1934, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 1934, 0.1).
narrative_ontology:measurement(hagia_sophia_substrate__islamic_sovereignty_reading_su_t1985, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 1985, 0.15).
narrative_ontology:measurement(hagia_sophia_substrate__islamic_sovereignty_reading_su_t2013, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 2013, 0.35).
narrative_ontology:measurement(hagia_sophia_substrate__islamic_sovereignty_reading_su_t2020, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 2020, 0.58).
narrative_ontology:measurement(hagia_sophia_substrate__islamic_sovereignty_reading_su_t2024, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hagia_sophia_substrate__islamic_sovereignty_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hagia_sophia_substrate__islamic_sovereignty_reading, 0.1).
narrative_ontology:affects_constraint(hagia_sophia_substrate__islamic_sovereignty_reading, hagia_sophia_substrate__universal_heritage_reading).
narrative_ontology:affects_constraint(hagia_sophia_substrate__islamic_sovereignty_reading, hagia_sophia_substrate__orthodox_restitution_reading).
narrative_ontology:affects_constraint(hagia_sophia_substrate__islamic_sovereignty_reading, turkish_secularism_substrate__republican_identity).
narrative_ontology:affects_constraint(hagia_sophia_substrate__islamic_sovereignty_reading, unesco_world_heritage_regime__sovereignty_exception).

% DUAL FORMULATION NOTE:
% This reading decomposes the Hagia Sophia substrate kernel alongside universal_heritage_reading and orthodox_restitution_reading. The ε values differ structurally: this reading authors moderate-high extraction (political consolidation + diplomatic friction); universal_heritage_reading would author near-zero ε (pluralist coordination); orthodox_restitution_reading would author moderate ε (restitution claim with weak enforcement). All three share the same physical substrate but instantiate different constraints with different authority groundings and beneficiary/victim sets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hagia_sophia_substrate__islamic_sovereignty_reading, institutional, 0.15).
constraint_indexing:directionality_override(hagia_sophia_substrate__islamic_sovereignty_reading, organized, 0.25).
constraint_indexing:directionality_override(hagia_sophia_substrate__islamic_sovereignty_reading, powerless, 0.85).
constraint_indexing:directionality_override(hagia_sophia_substrate__islamic_sovereignty_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
