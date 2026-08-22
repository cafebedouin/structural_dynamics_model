% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__security_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy__security_necessity_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: territorial_legitimacy__security_necessity_reading
 *   human_readable: Territorial Legitimacy via Security Necessity (1967+ Strategic Depth)
 *   domain: political/territorial
 *
 * SUMMARY:
 *   The security necessity reading of territorial legitimacy frames Israeli
 *   control of the West Bank and Golan Heights as justified by the
 *   military-defensive requirement for strategic depth against existential
 *   threats. This reading grounds legitimacy in necessity rather than
 *   international law or indigenous continuity. The 1967 borders are
 *   presented as indefensible; the territories acquired in 1967 and held
 *   since are presented as non-negotiable security buffer. Palestinian
 *   sovereignty is conditional on demilitarization; settlements are presented
 *   as security presence, not colonization. This story instantiates ONE
 *   READING of the contested territorial_legitimacy kernel — the reading
 *   endorsed by Israeli security doctrine and supported by regional allies.
 *   The sibling readings (partition_reading: UN 181 and state recognition;
 *   indigenous_continuity_reading: Nakba and anti-colonial
 *   self-determination) are OTHER CONSTRAINTS, not variations within this
 *   one.
 *
 * KEY AGENTS:
 *   - israeli_security_establishment: agenda-setter, institutional power, arbitrage exit — sets and enforces the territorial arrangement
 *   - palestinian_residents_west_bank: payer, powerless, trapped — bear the costs of territorial control, excluded from the security conversation
 *   - palestinian_residents_golan: payer, powerless, trapped — same situation as West Bank residents
 *   - israeli_settlers: organized beneficiaries with identity-locked exit — benefit from settlement subsidies and vindication, also serve as security markers
 *   - palestinian_political_authorities: excluded but conditional — recognized only under Israeli security coordination, cannot contest the framework
 *   - regional_military_actors: excluded by design — their threat justifies the arrangement; their exclusion is the intended effect
 *   - us_strategic_alliance: beneficiary, institutional power — supports the doctrine as compatible with regional and counter-terror strategy
 *   - international_law_community: observer, analytical power — divided on whether security necessity overrides partition and occupation law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__security_necessity_reading, 0.82).
domain_priors:suppression_score(territorial_legitimacy__security_necessity_reading, 0.88).
domain_priors:theater_ratio(territorial_legitimacy__security_necessity_reading, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__security_necessity_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy__security_necessity_reading, "Territorial Legitimacy via Security Necessity (1967+ Strategic Depth)").
narrative_ontology:topic_domain(territorial_legitimacy__security_necessity_reading, "political/territorial").

domain_priors:requires_active_enforcement(territorial_legitimacy__security_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__security_necessity_reading, '0d8db52c-3e79-486a-b34f-5093a74f8d4b').
narrative_ontology:cs_kernel_codification('0d8db52c-3e79-486a-b34f-5093a74f8d4b', formalized).
narrative_ontology:cs_authority_grounding('0d8db52c-3e79-486a-b34f-5093a74f8d4b', extraction).
narrative_ontology:cs_interpretation_layer_present('0d8db52c-3e79-486a-b34f-5093a74f8d4b').
narrative_ontology:cs_reading_relation('0d8db52c-3e79-486a-b34f-5093a74f8d4b', territorial_legitimacy__partition_reading, influences).
narrative_ontology:cs_reading_relation('0d8db52c-3e79-486a-b34f-5093a74f8d4b', territorial_legitimacy__indigenous_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('0d8db52c-3e79-486a-b34f-5093a74f8d4b', foundational, defensive_territorial_depth_necessity).
narrative_ontology:cs_axiom_status(defensive_territorial_depth_necessity, holdable).
narrative_ontology:cs_axiom_grounding('0d8db52c-3e79-486a-b34f-5093a74f8d4b', defensive_territorial_depth_necessity, empirically_contingent).
narrative_ontology:cs_axiom('0d8db52c-3e79-486a-b34f-5093a74f8d4b', foundational, security_override_international_occupation_law).
narrative_ontology:cs_axiom_status(security_override_international_occupation_law, holdable).
narrative_ontology:cs_axiom_grounding('0d8db52c-3e79-486a-b34f-5093a74f8d4b', security_override_international_occupation_law, instrumental).
narrative_ontology:cs_reference_frame('0d8db52c-3e79-486a-b34f-5093a74f8d4b', strategic_indefensibility_1967).
narrative_ontology:cs_drift_state('0d8db52c-3e79-486a-b34f-5093a74f8d4b', contemporary_post_peace_agreements, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0d8db52c-3e79-486a-b34f-5093a74f8d4b', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy__security_necessity_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, israeli_security_establishment).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, palestinian_residents_west_bank).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, palestinian_residents_golan).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, israeli_settlers).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, us_strategic_alliance).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, israeli_settlers).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, palestinian_political_authorities).
narrative_ontology:constraint_vindicates(territorial_legitimacy__security_necessity_reading, security_buffer_necessity_doctrine).
narrative_ontology:constraint_vindicates(territorial_legitimacy__security_necessity_reading, defensive_territorial_depth_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains that Israeli control of the West Bank and Golan Heights is essential for defensive depth against military threats from neighboring states and armed groups. Justifies territorial administration, military presence, and settlement activity as security infrastructure. Claims the 1967 boundaries create undefendable borders (narrow strip at waist) and that strategic depth is non-negotiable for survival. Enforces the territorial arrangement through military administration, settlement policy, checkpoint systems, and restrictions on Palestinian armed capability.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, israeli_security_establishment, agenda_setter,
    institutional, generational, arbitrage, regional).

% Live under Israeli military administration and settlement presence justified as security necessity. Subject to movement restrictions, land confiscation for settlements framed as security buffers, military law, and control of water and resources. Cannot exit the territory; cannot participate in the security justifications that govern their circumstance. The 'security' rationale forecloses their voice from the legitimacy conversation — security justifications are presented as technical and non-negotiable rather than contestable.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, palestinian_residents_west_bank, payer,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__security_necessity_reading, palestinian_residents_west_bank, excluded).

% Inhabited the Golan Heights before 1967; remain under Israeli administration and settlement. Experience the same restrictions on movement, land ownership, and resource access as West Bank Palestinians, justified through the same security necessity doctrine. Many were displaced or fled in 1967; those who remain are a small, identity-locked population with no exit and no voice in the security assessments that determine their territory.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, palestinian_residents_golan, payer,
    powerless, biographical, trapped, regional).

% Settle in the West Bank and Golan justified as security presence. Receive subsidies, military protection, and ideological framing that fuses settlement expansion with national security. Are also exposed to security risks and are used as visible markers of territorial control. Their presence both vindicates and depends on the security necessity framing — they are beneficiaries of the doctrine and instruments of its enforcement.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, israeli_settlers, beneficiary,
    organized, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__security_necessity_reading, israeli_settlers, payer).

% Recognized conditionally as governing bodies (Palestinian Authority in West Bank) under Israeli security coordination agreements that bind them to suppress armed resistance and enforce demilitarization. Their legitimacy is conditional on accepting the security framework; their sovereignty is explicitly subordinated to Israeli security requirements. Cannot unilaterally change the security arrangements and are blamed when they fail to enforce demilitarization.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, palestinian_political_authorities, excluded,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__security_necessity_reading, palestinian_political_authorities, payer).

% Syria, Jordan, Hezbollah, and other actors who would challenge Israeli control are kept out by the military arrangements justified as security necessity. The doctrine forecloses their participation — their threat is the constant justification for the territorial control, and their exclusion is the intended effect.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, regional_military_actors, excluded,
    powerful, generational, trapped, regional).

% International law community (UN, human rights bodies, academic scholars) divides on whether security necessity can justify territorial occupation and settlement. The security reading claims uti possidetis and necessity override the 4th Geneva Convention; the partition and indigenous-continuity readings reject this prioritization. Observers lack enforcement power but produce legitimacy verdicts that shape international relations.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, international_law_community, observer,
    institutional, generational, analytical, global).

% Supports Israeli security framing as compatible with US regional interests and anti-terrorism doctrine. Provides military aid contingent on Israeli security partnership. Benefits from Israeli intelligence and military presence in the region. Endorses the security necessity doctrine in international forums.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, us_strategic_alliance, beneficiary,
    institutional, generational, arbitrage, global).

% Are formally excluded from the security arrangements that justify the occupation — their presence would violate the doctrine's core claim (that Israeli territorial control is necessary precisely because neighboring states threaten). Some accept this exclusion through peace agreements (Egypt, Jordan); others remain in formal hostility. All are kept out of the security conversation by the very doctrine that justifies the territorial control.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, arab_state_governments, excluded,
    institutional, generational, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy__security_necessity_reading, israeli_security_establishment).
narrative_ontology:fixing_cost_class(territorial_legitimacy__security_necessity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a military security perimeter that coordinates Israeli defense against external and internal armed threats. Creates a buffer zone claimed to prevent rapid military assault; administers settlements and military presence as forward security infrastructure; coordinates checkpoint and movement systems to prevent infiltration.
% TRANSFER_FUNCTION: Transfers Palestinian land and mobility rights to Israeli security administration and settlement expansion. Palestinians lose control of water resources, movement within and beyond the territory, and land access — the costs are framed as security requirements rather than extraction. Settlers receive subsidized land, security guarantees, and ideological vindication. The Israeli security establishment receives budgetary resources, political authority, and operational control over a large population.
% ABSENT_VOICES: Palestinian residents (powerless, trapped — excluded by the security framing itself, which treats their consent as irrelevant to defensive necessity). Palestinian political authorities (conditional legitimacy — cannot contest the security framework without losing what recognition they have). Regional military actors and Arab state governments (their exclusion is the point of the doctrine — the security necessity claims they are the threat). International law scholars and human rights bodies that reject the security prioritization are present as observers but lack enforcement power.
% DISAPPEARANCE_RATIONALE: If this territorial arrangement and its security justification vanished, Israeli military presence would compress to the 1967 borders, settlements would require either evacuation or separate legitimacy (partition, coexistence, or incorporation arguments), Palestinian residents would regain movement and land access, and the region would reorganize around a different military balance. The disappeared doctrine would be replaced by one of the sibling readings (partition or indigenous-continuity) or a new arrangement entirely.
% FOUNDING_PROBLEM: Arab military threats in 1967 (and historically) created a military situation where the pre-1967 borders were deemed indefensible — a narrow strip at Israel's waist vulnerable to rapid encirclement. The doctrine arose to justify territory seized in the 1967 war as necessary for defensive depth.
% FOUNDING_PROBLEM_CORROBORATION: Israeli military and strategic analysts attest that the 1967 borders were strategically vulnerable and that territorial depth is essential for defense. Palestinian analysts and international law scholars contest this claim: they argue that military capability, alliances, and negotiated demilitarization agreements can provide security without occupation; that the doctrine has become cover for permanent territorial expansion; and that the security threat has been substantially reduced (peace with Egypt and Jordan, Palestinian Authority security coordination) yet the territorial control persists. Independent military analysis and historical documentation confirm the 1967 vulnerability but dispute whether it remains dispositive 55+ years later.
narrative_ontology:disappearance_verdict(territorial_legitimacy__security_necessity_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy__security_necessity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__security_necessity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(territorial_legitimacy__security_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy__security_necessity_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy__security_necessity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy__security_necessity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy__security_necessity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and rising (0.58→0.82 over 56 years) because the doctrine increasingly functions to justify permanent territorial expansion beyond 1967 security lines. Early measurements reflect the genuine tactical vulnerability of the pre-1967 borders; later measurements reflect settled population growth, resource extraction, and permanent administrative control decoupled from any clear security deadline. Theater is substantial and rising (0.35→0.61): military operations and settlement activity are increasingly justified retroactively as security rather than driven by current threat assessment. The rise in theater_ratio signals Goodhart drift — the security justification has become the default cover story rather than a constraint on behavior. Suppression is very high (0.72→0.88) and tightening: maintaining the arrangement requires constant military enforcement, checkpoint systems, movement restrictions, land confiscation, and prevention of Palestinian armed capability. Suppression does not scale down as threat assessment shifts because the doctrine has become the legitimacy framework itself — the arrangement persists because the security narrative persists, not because the founding threat persists.
 *
 * PERSPECTIVAL GAP:
 *   From the Israeli security establishment's seat, the constraint appears as genuine defensive necessity — a Rope or even a Mountain (military reality of the 1967 borders). From the Palestinian residents' seat, the same constraint appears as Snare or pure Tangled Rope — extraction is the primary function, security is the cover story. From the international law observer seat, the constraint is Tangled Rope at best and Snare at worst: coordination (defense) and extraction (territorial control) are simultaneously present, but the extraction component violates the 4th Geneva Convention and expands continually beyond the original 1967 security lines. The engine computes this gap from the power atoms, exit options, and beneficiary/victim structure: institutional power with arbitrage exit (agenda-setter) computes differently from powerless trapped victims. The authored metrics are uniform across all seats because extractiveness and suppression are properties of the constraint's operation, not of seat-relative perception; the engine applies directionality scaling to produce per-seat effective extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The Israeli security establishment is the structural beneficiary: it sets policy, collects resources (budget allocation, operational authority, territory), and controls the narrative that justifies the arrangement. Directionality is near beneficiary (d ≈ 0.05) because this seat experiences the constraint as providing security goods it values. Palestinian residents are the structural targets: they bear the costs (land loss, mobility restriction, administrative subjection) and cannot exit or contest the justification (their security is not the referent — Israeli security is). Directionality is near target (d ≈ 0.95). The gap is deliberate and structural: the same territorial control produces asymmetric effects because it is authored from one seat's security requirements over another's liberty. Palestinian political authorities sit at constrained directionality (d ≈ 0.60): they are recognized but only under security agreements that bind them to Israeli requirements; they benefit from limited governance but bear the cost of enforcing demilitarization against their own population. Israeli settlers occupy identity-locked beneficiary space: they genuinely benefit from settlement subsidies and security provision, but their exit is ideologically fused with the territorial expansion itself — they cannot leave without renouncing the core belief that justifies their presence.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1967 military vulnerability) was genuine and urgent. The problem's status is now contested: Israeli security analysts maintain the threat persists (regional instability, anti-Israel actors, weapons proliferation); Palestinian and international analysts argue the founding problem is substantially solved (peace with Egypt and Jordan, Palestinian Authority security coordination, Hezbollah deterred, Syrian military capacity reduced). The theater_ratio rise (0.35→0.61) and extractiveness plateau (0.79→0.82, flattening at saturation) together suggest mandatrophy: the constraint persists because the security legitimacy narrative persists, not because the founding threat persists. If the founding problem were dead (status=dead) and the disappearance verdict is world_rearranges (settlements, military bases, and administrative control would reorganize), the constraint exhibits mandatrophy — a legitimacy claim that outlived its function. The authorization to control the territory came from the security emergency; the authorization does not automatically transfer to permanent territorial expansion once the emergency is managed. This reading does not resolve mandatrophy (that is not the role of a single constraint) but documents the structure that enables it: the doctrine's flexibility allows security-for-now to become security-forever.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_threat_persistence,
    'Is the 1967 military threat to Israel''s existence still operative and dispositive 55+ years later, or has the threat environment substantially changed such that territorial control is now justified by other interests (resource control, political leverage, settlement ideology) rather than military necessity?',
    'Independent military analysis of regional threat trajectories, declassified Israeli security assessments, and counterfactual modeling of Israeli security under alternative arrangements (demilitarized Palestinian state, negotiated confidence-building measures, arms control agreements). The answer depends on empirical threat assessment, not on doctrine.',
    'If the threat is dead or substantially mitigated, the constraint exhibits mandatrophy: the authorization came from emergency, but the emergency is over. Terminal classification shifts from Tangled Rope (coordination + extraction) toward pure Snare (extraction with security justification as cover). If the threat remains alive, the Tangled Rope classification holds and security necessity legitimizes the extraction component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_threat_persistence, empirical, 'Whether the 1967 security rationale remains operative or has become post-hoc justification for territorial expansion.').

omega_variable(
    security_vs_settlement_decoupling,
    'Are the West Bank settlements and associated Israeli civilian presence structurally necessary for security (they are military infrastructure), or are they separable from the security requirement and driven by other interests (territorial expansion, resource control, ideological claims)?',
    'Comparative analysis: (a) settlements in genuinely necessary military positions (Golan, narrow security buffer zones) vs. (b) settlements deep in Palestinian territory far from any plausible security perimeter. If (b) cannot be justified by security necessity, the constraint exhibits settlement expansion decoupled from the founding security rationale.',
    'If decoupled, the constraint splits into two sub-constraints: defensive security (legitimate under this reading) and permanent territorial expansion (extraction without coordination). The Tangled Rope classification would apply only to the security component; the settlement expansion would be Snare. If coupled, the entire arrangement is security infrastructure and the Tangled Rope classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(security_vs_settlement_decoupling, empirical, 'Whether settlement expansion remains tied to security necessity or has become independent territorial acquisition.').

omega_variable(
    suppression_internalization,
    'Is the high suppression (0.88) structural (military checkpoints, legal restrictions, administrative barriers) or internalized (Palestinian residents believe the security threat justifies restrictions and accept them as legitimate)?',
    'Post-conflict or post-occupation trajectory analysis: do Palestinian residents who exit the constraint (emigration, settlements absorbed into Israel proper) maintain suppression beliefs or do they recover independent political expression? If recovery occurs, the suppression is structural and dissipates with the constraint; if beliefs persist, suppression is partially internalized.',
    'If structural, removing the constraint removes the suppression. If internalized, the suppression persists after the territorial control ends (psychological or ideological internalization). The mechanism affects the terminal state and the cost of transitioning to a different constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Whether measured suppression is structural military control or internalized acceptance of security justification.').

omega_variable(
    competing_kernel_readings,
    'This constraint instantiates ONE reading of the territorial_legitimacy kernel. Do the partition_reading and indigenous_continuity_reading represent genuinely live alternative frameworks within Israeli and Palestinian political discourse, or are they rhetorical positions with no institutional backing?',
    'Historical and contemporary documentation: which readings have been endorsed by Israeli or Palestinian political authorities? Which readings structure actual negotiations or governance? The relation type (forecloses vs. coexists_with vs. influences) depends on whether the readings are genuinely live options for institutional actors or merely abstract alternatives.',
    'If the partition_reading is live and potentially endorsed by Israeli negotiators, the security_reading and partition_reading coexist (different factions hold different readings). If the partition_reading is foreclosed by Israeli institutional commitment to permanent territorial control, it influences but does not foreclose. The indigenous_continuity_reading is almost certainly coexists_with (Palestinian factions hold it; Israeli institutional actors reject it). The coexistence claim depends on whether no single framework could hold multiple readings — which is false; many Israelis and Palestinians explicitly hold partition and security-necessity as compatible (security buffer within a two-state framework).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competing_kernel_readings, conceptual, 'Whether sibling readings are genuinely live institutional positions or rhetorical alternatives outside real political discourse.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__security_necessity_reading, 0, 56).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t0, territorial_legitimacy__security_necessity_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(terr_tr_t0, observed).
narrative_ontology:measurement(terr_tr_t8, territorial_legitimacy__security_necessity_reading, theater_ratio, 8, 0.39).
narrative_ontology:measurement_basis(terr_tr_t8, observed).
narrative_ontology:measurement(terr_tr_t16, territorial_legitimacy__security_necessity_reading, theater_ratio, 16, 0.44).
narrative_ontology:measurement_basis(terr_tr_t16, observed).
narrative_ontology:measurement(terr_tr_t24, territorial_legitimacy__security_necessity_reading, theater_ratio, 24, 0.5).
narrative_ontology:measurement_basis(terr_tr_t24, observed).
narrative_ontology:measurement(terr_tr_t32, territorial_legitimacy__security_necessity_reading, theater_ratio, 32, 0.55).
narrative_ontology:measurement_basis(terr_tr_t32, observed).
narrative_ontology:measurement(terr_tr_t40, territorial_legitimacy__security_necessity_reading, theater_ratio, 40, 0.59).
narrative_ontology:measurement_basis(terr_tr_t40, observed).
narrative_ontology:measurement(terr_tr_t48, territorial_legitimacy__security_necessity_reading, theater_ratio, 48, 0.61).
narrative_ontology:measurement_basis(terr_tr_t48, observed).
narrative_ontology:measurement(terr_tr_t56, territorial_legitimacy__security_necessity_reading, theater_ratio, 56, 0.61).
narrative_ontology:measurement_basis(terr_tr_t56, observed).

% Extraction over time
narrative_ontology:measurement(terr_be_t0, territorial_legitimacy__security_necessity_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(terr_be_t0, observed).
narrative_ontology:measurement(terr_be_t8, territorial_legitimacy__security_necessity_reading, base_extractiveness, 8, 0.64).
narrative_ontology:measurement_basis(terr_be_t8, observed).
narrative_ontology:measurement(terr_be_t16, territorial_legitimacy__security_necessity_reading, base_extractiveness, 16, 0.7).
narrative_ontology:measurement_basis(terr_be_t16, observed).
narrative_ontology:measurement(terr_be_t24, territorial_legitimacy__security_necessity_reading, base_extractiveness, 24, 0.75).
narrative_ontology:measurement_basis(terr_be_t24, observed).
narrative_ontology:measurement(terr_be_t32, territorial_legitimacy__security_necessity_reading, base_extractiveness, 32, 0.79).
narrative_ontology:measurement_basis(terr_be_t32, observed).
narrative_ontology:measurement(terr_be_t40, territorial_legitimacy__security_necessity_reading, base_extractiveness, 40, 0.81).
narrative_ontology:measurement_basis(terr_be_t40, observed).
narrative_ontology:measurement(terr_be_t48, territorial_legitimacy__security_necessity_reading, base_extractiveness, 48, 0.82).
narrative_ontology:measurement_basis(terr_be_t48, observed).
narrative_ontology:measurement(terr_be_t56, territorial_legitimacy__security_necessity_reading, base_extractiveness, 56, 0.82).
narrative_ontology:measurement_basis(terr_be_t56, observed).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t0, territorial_legitimacy__security_necessity_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement_basis(terr_su_t0, observed).
narrative_ontology:measurement(terr_su_t8, territorial_legitimacy__security_necessity_reading, suppression_requirement, 8, 0.76).
narrative_ontology:measurement_basis(terr_su_t8, observed).
narrative_ontology:measurement(terr_su_t16, territorial_legitimacy__security_necessity_reading, suppression_requirement, 16, 0.8).
narrative_ontology:measurement_basis(terr_su_t16, observed).
narrative_ontology:measurement(terr_su_t24, territorial_legitimacy__security_necessity_reading, suppression_requirement, 24, 0.83).
narrative_ontology:measurement_basis(terr_su_t24, observed).
narrative_ontology:measurement(terr_su_t32, territorial_legitimacy__security_necessity_reading, suppression_requirement, 32, 0.85).
narrative_ontology:measurement_basis(terr_su_t32, observed).
narrative_ontology:measurement(terr_su_t40, territorial_legitimacy__security_necessity_reading, suppression_requirement, 40, 0.87).
narrative_ontology:measurement_basis(terr_su_t40, observed).
narrative_ontology:measurement(terr_su_t48, territorial_legitimacy__security_necessity_reading, suppression_requirement, 48, 0.88).
narrative_ontology:measurement_basis(terr_su_t48, observed).
narrative_ontology:measurement(terr_su_t56, territorial_legitimacy__security_necessity_reading, suppression_requirement, 56, 0.88).
narrative_ontology:measurement_basis(terr_su_t56, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy__security_necessity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(territorial_legitimacy__security_necessity_reading, 0.18).
narrative_ontology:affects_constraint(territorial_legitimacy__security_necessity_reading, territorial_legitimacy__partition_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__security_necessity_reading, territorial_legitimacy__indigenous_continuity_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__security_necessity_reading, palestinian_sovereignty__conditional_demilitarization).
narrative_ontology:affects_constraint(territorial_legitimacy__security_necessity_reading, settlement_expansion__security_infrastructure_framing).

% DUAL FORMULATION NOTE:
% The territorial_legitimacy kernel decomposes into three structurally distinct constraints corresponding to three competing readings: security_necessity_reading (this file, high extractiveness, enforcement-mechanism coordination), partition_reading (UN 181 and state recognition, lower extractiveness, resource_allocation coordination), and indigenous_continuity_reading (anti-colonial self-determination, high extractiveness similar to security reading but with different beneficiary structure). Each reading has a different ε, different beneficiary/victim alignment, and different terminal classification. They are NOT variations on a single constraint — they are three constraints linked by a shared kernel. The readings coexist in contemporary discourse (different factions hold different readings) but are structurally incommensurable: no single framework can coherently hold all three simultaneously. The security_reading influences the partition_reading (a negotiated two-state settlement might accept security buffers) but does not foreclose it. The indigenous_continuity_reading coexists with both (Palestinian actors hold it; Israeli actors reject it) but is not foreclosed by either security or partition reading alone.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_legitimacy__security_necessity_reading, powerless, 0.95).
constraint_indexing:directionality_override(territorial_legitimacy__security_necessity_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
