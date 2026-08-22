% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__religious_covenant_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__religious_covenant_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: jewish_self_determination__religious_covenant_reading
 *   human_readable: Religious Covenant Reading of Jewish Territorial Sovereignty
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint story captures the religious_covenant_reading of Jewish
 *   self-determination: the claim that Jewish territorial sovereignty derives
 *   from divine covenant, making it a religious obligation that operates
 *   independently of and hierarchically above secular political frameworks.
 *   The reading is instantiated by the religious Zionist movement, the
 *   settlement enterprise, and the state rabbinate, which together form a
 *   coalition that translates theological claim into state policy. The
 *   constraint presents itself as a mountain (divine command as immutable
 *   natural law) but operates as a tangled rope: the coordination function is
 *   real within the religious community (shared covenantal identity,
 *   collective religious obligation), but the same structure extracts
 *   asymmetrically from secular negotiation frameworks and Palestinian
 *   political claims through state power. The measurement series tracks the
 *   operationalization of this reading from 1948 (state founding, religious
 *   claim subsidiary to secular nationalist framework) through 1967
 *   (territorial expansion activating religious claim to West Bank), 1977
 *   (religious Zionist political ascendancy), 1993 (Oslo process triggering
 *   religious opposition as betrayal of covenant), 2000 (Second Intifada and
 *   collapse of secular peace framework), to 2023 (religious claim as
 *   dominant sovereignty framework).
 *
 * KEY AGENTS:
 *   - religious_zionist_movement: Primary beneficiary (institutional/powerful) — translates divine claim into political program and settlement policy
 *   - settlement_enterprise: Primary beneficiary (organized/powerful) — materializes the religious claim on the ground, creates facts that foreclose secular negotiation
 *   - state_rabbinate: Agenda setter (institutional/generational) — authorizes the religious interpretation, converts theological claim into halakhic obligation for state actors
 *   - secular_negotiation_framework: Primary victim (institutional/biographical) — territorial compromise frameworks (Oslo, two-state) structurally foreclosed by religious claim's absolute character
 *   - palestinian_political_claims: Victim (organized/biographical) — national claims rendered illegible by a sovereignty framework that recognizes only divine title
 *   - secular_israeli_constitutionalism: Victim (institutional/biographical) — democratic/constitutional principles subordinated to religious sovereignty claim
 *   - analytical_observer: Observer (analytical/civilizational) — sees the full structure of claim, enforcement, and extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__religious_covenant_reading, 0.72).
domain_priors:suppression_score(jewish_self_determination__religious_covenant_reading, 0.78).
domain_priors:theater_ratio(jewish_self_determination__religious_covenant_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__religious_covenant_reading, tangled_rope).
narrative_ontology:human_readable(jewish_self_determination__religious_covenant_reading, "Religious Covenant Reading of Jewish Territorial Sovereignty").
narrative_ontology:topic_domain(jewish_self_determination__religious_covenant_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_self_determination__religious_covenant_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__religious_covenant_reading, 'caace6af-819e-4ade-993f-51c0ad2d34f4').
narrative_ontology:cs_kernel_codification('caace6af-819e-4ade-993f-51c0ad2d34f4', fixed_text).
narrative_ontology:cs_authority_grounding('caace6af-819e-4ade-993f-51c0ad2d34f4', lineage).
narrative_ontology:cs_interpretation_layer_present('caace6af-819e-4ade-993f-51c0ad2d34f4').
narrative_ontology:cs_reading_relation('caace6af-819e-4ade-993f-51c0ad2d34f4', jewish_self_determination__liberal_nationalist_reading, influences).
narrative_ontology:cs_reading_relation('caace6af-819e-4ade-993f-51c0ad2d34f4', jewish_self_determination__indigenous_return_reading, influences).
narrative_ontology:cs_reading_relation('caace6af-819e-4ade-993f-51c0ad2d34f4', jewish_self_determination__settler_colonial_reading, forecloses).
narrative_ontology:cs_reading_relation('caace6af-819e-4ade-993f-51c0ad2d34f4', jewish_self_determination__diasporist_reading, coexists_with).
narrative_ontology:cs_axiom('caace6af-819e-4ade-993f-51c0ad2d34f4', foundational, divine_covenant_entails_territorial_sovereignty).
narrative_ontology:cs_axiom_status(divine_covenant_entails_territorial_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('caace6af-819e-4ade-993f-51c0ad2d34f4', divine_covenant_entails_territorial_sovereignty, theological).
narrative_ontology:cs_axiom('caace6af-819e-4ade-993f-51c0ad2d34f4', foundational, religious_obligation_outranks_secular_law).
narrative_ontology:cs_axiom_status(religious_obligation_outranks_secular_law, holdable).
narrative_ontology:cs_axiom_grounding('caace6af-819e-4ade-993f-51c0ad2d34f4', religious_obligation_outranks_secular_law, theological).
narrative_ontology:cs_axiom('caace6af-819e-4ade-993f-51c0ad2d34f4', secondary, land_settlement_as_mitzvah).
narrative_ontology:cs_axiom_status(land_settlement_as_mitzvah, holdable).
narrative_ontology:cs_axiom_grounding('caace6af-819e-4ade-993f-51c0ad2d34f4', land_settlement_as_mitzvah, theological).
narrative_ontology:cs_axiom('caace6af-819e-4ade-993f-51c0ad2d34f4', secondary, compromise_as_covenantal_betrayal).
narrative_ontology:cs_axiom_status(compromise_as_covenantal_betrayal, holdable).
narrative_ontology:cs_axiom_grounding('caace6af-819e-4ade-993f-51c0ad2d34f4', compromise_as_covenantal_betrayal, theological).
narrative_ontology:cs_reference_frame('caace6af-819e-4ade-993f-51c0ad2d34f4', biblical_covenantal_promise).
narrative_ontology:cs_drift_state('caace6af-819e-4ade-993f-51c0ad2d34f4', contemporary_settlement_reality, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('caace6af-819e-4ade-993f-51c0ad2d34f4', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__religious_covenant_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__religious_covenant_reading, religious_zionist_movement).
narrative_ontology:constraint_beneficiary(jewish_self_determination__religious_covenant_reading, settlement_enterprise).
narrative_ontology:constraint_beneficiary(jewish_self_determination__religious_covenant_reading, state_rabbinate).
narrative_ontology:constraint_victim(jewish_self_determination__religious_covenant_reading, secular_negotiation_framework).
narrative_ontology:constraint_victim(jewish_self_determination__religious_covenant_reading, palestinian_political_claims).
narrative_ontology:constraint_victim(jewish_self_determination__religious_covenant_reading, secular_israeli_constitutionalism).
narrative_ontology:constraint_vindicates(jewish_self_determination__religious_covenant_reading, divine_covenant_entails_territorial_sovereignty).
narrative_ontology:constraint_vindicates(jewish_self_determination__religious_covenant_reading, religious_obligation_outranks_secular_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organizes political and settlement activity around the divine covenant claim. Gains: political influence, state funding for settlements, ideological coherence, demographic growth through religious education. Exit requires abandoning core identity — the covenant is constitutive of self-understanding. Constrained by dependence on state power to implement the vision.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, religious_zionist_movement, beneficiary,
    powerful, generational, identity_locked, national).

% Materializes the religious claim through physical settlements in West Bank and East Jerusalem. Gains: land, housing subsidies, state protection, political representation. Creates irreversible facts on ground that foreclose territorial compromise. Exit is constrained by sunk costs (communities, infrastructure) and ideological commitment; leaving means abandoning the divine mission.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, settlement_enterprise, beneficiary,
    organized, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__religious_covenant_reading, settlement_enterprise, agenda_setter).

% Official religious authority of the state; converts divine covenant into halakhic rulings that bind state policy (e.g., prohibition on land cession, military service exemptions, conversion standards). Gains: institutional monopoly over Jewish status, control of marriage/divorce, state funding, legislative influence. Identity-locked: the rabbinate's institutional identity is fused with the covenantal sovereignty claim; reinterpreting would dissolve its authority.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, state_rabbinate, agenda_setter,
    institutional, generational, identity_locked, national).

% The architectural framework for territorial compromise (Oslo Accords, two-state solution, land-for-peace). Bears the cost of foreclosure: the religious claim's absolute character makes negotiation structurally impossible — any concession is framed as betrayal of divine mandate. The framework persists as zombie institution (Quartet, PA security coordination) but has lost operative legitimacy. Exit is constrained: the framework is embedded in international law and Israeli security doctrine, but its political constituency has been eroded by the religious claim's dominance.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, secular_negotiation_framework, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_non_agent(jewish_self_determination__religious_covenant_reading, secular_negotiation_framework).

% Palestinian national movement claiming statehood, sovereignty, and right of return. Bears extraction: land loss to settlements, fragmentation of territory, denial of political agency, subordination to a sovereignty claim that recognizes only Jewish divine title. Exit options are trapped: cannot leave the territory, cannot exit the claim's enforcement (military occupation, permit regime), and the religious claim offers no accommodation — compromise is theologically forbidden.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, palestinian_political_claims, payer,
    organized, generational, trapped, national).

% Democratic, liberal, and constitutional principles within Israel (judicial independence, equality before law, separation of religion/state). Bears extraction: subordination of democratic norms to religious sovereignty claim (nation-state law, rabbinate monopoly, settlement legality). Exit is constrained: emigration is possible but costly; internal resistance (protests, litigation, civil society) faces institutional capture by religious-nationalist coalition. The constraint extracts democratic legitimacy to fund theological sovereignty.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, secular_israeli_constitutionalism, payer,
    institutional, biographical, constrained, national).

% Sees the full structure: a divine covenant claim that operates as mountain within the believing community but as tangled_rope when enforced through state power on non-adherents. Tracks the measurement series, the seat divergences, the kernel family. Does not bear costs or collect benefits from the constraint's operation.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__religious_covenant_reading, settlement_enterprise).
narrative_ontology:fixing_cost_class(jewish_self_determination__religious_covenant_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Jewish religious collective around a shared covenantal mission: settlement of the land as divine obligation, creating a society ordered by halakha, providing existential meaning and demographic continuity for the religious Zionist public.
% TRANSFER_FUNCTION: Moves territory, state resources, political sovereignty, and democratic legitimacy from secular/Palestinian frameworks to the religious Zionist settlement enterprise. The state's monopoly on violence, planning law, military protection, and budget are redirected to serve the covenantal claim. Palestinian land and political agency are extracted as the territorial substrate of the religious project.
% ABSENT_VOICES: Palestinian refugees (excluded by geography and political status), secular Israeli youth facing religious-nationalist conscription, religious anti-Zionists (Haredi non-Zionist and Neturei Karta) who reject the covenantal sovereignty claim as heretical acceleration of messianic time, international legal frameworks that recognize Palestinian rights but lack enforcement power. These voices are structurally excluded: the constraint's logic recognizes no legitimate interlocutor outside the covenant.
% DISAPPEARANCE_RATIONALE: If the religious covenant claim vanished overnight: the settlement enterprise would lose its theological justification (though material interests might persist); the state rabbinate would lose its monopolistic authority; the secular negotiation framework would revive as the only viable political architecture; Palestinian statehood would become negotiable on territorial rather than theological terms; Israeli constitutional democracy would face a different threat landscape. The territorial map, the legal architecture, the demographic balance, and the international legitimacy framework would all rearrange.
% FOUNDING_PROBLEM: Jewish existential survival after the Holocaust and the perceived failure of diaspora existence; the religious interpretation of covenantal promise as requiring territorial sovereignty in the historic land; the need to resolve the theological tension between exile (divine punishment) and return (divine redemption) through human agency aligned with divine will.
% FOUNDING_PROBLEM_CORROBORATION: The religious Zionist movement attests the founding problem is live and eternal (covenant is perpetual, redemption incomplete). Secular Zionist historians (Shapira, Engel) attest the founding problem was the existential crisis of European Jewry, substantially resolved by statehood in 1948. Palestinian historians (Khalidi, Pappé) attest the founding problem was a European colonial project that invented a religious cover. International legal scholars attest the UN partition resolution (1947) addressed a refugee/self-determination problem, not a divine covenant. The religious reading's corroboration comes from within its own tradition; external corroboration supports the shifted-function reading.
narrative_ontology:disappearance_verdict(jewish_self_determination__religious_covenant_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__religious_covenant_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__religious_covenant_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(jewish_self_determination__religious_covenant_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__religious_covenant_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__religious_covenant_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__religious_covenant_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_self_determination__religious_covenant_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.72) reflects the constraint's operational extraction: the religious claim, while internally coherent as absolute obligation, functions through state power to extract territory, resources, and political legitimacy from competing claims. The divine command's 'zero epsilon within the framework' becomes high effective epsilon when enforced on those outside it. Suppression (0.78) is high because the constraint's persistence depends on actively foreclosing alternatives: the secular two-state framework, Palestinian statehood, and Israeli constitutional democracy are not merely disagreed with — they are structurally suppressed through settlement facts, legal architecture, and the framing of compromise as religious betrayal. Theater ratio (0.42) is substantial: the genuine coordination function (religious community cohesion, covenantal identity) coexists with performative invocation of divine mandate to justify state policies that serve material settlement interests. Accessibility collapse (0.65) is moderate-high: within the religious framework alternatives are nearly unimaginable (covenant is absolute), but secular and Palestinian alternatives persist outside it. Resistance (0.71) is high: the constraint meets active resistance from secular Israelis, Palestinian national movement, international law frameworks, and even religious anti-Zionist currents.
 *
 * PERSPECTIVAL GAP:
 *   The religious adherent experiences this as mountain (divine command, zero extraction, no alternative). The secular Israeli experiences it as snare (extraction of democratic norms, suppression of constitutional alternatives). The Palestinian experiences it as snare (extraction of land and political agency, suppression of national claim). The settlement enterprise experiences it as rope (coordination of divine mission, genuine collective action). The engine computes these divergences from the structural data — the claimed_type (tangled_rope) captures the structural reality that the constraint is mountain-for-some and snare-for-others simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (religious_zionist_movement, settlement_enterprise, state_rabbinate) collect: political power, territorial control, institutional authority, material resources from settlement. Their directionality is near 0.0 (full beneficiary). Victims (secular_negotiation_framework, palestinian_political_claims, secular_israeli_constitutionalism) bear costs: foreclosed political options, lost territory, subordinated legal frameworks. Their directionality is near 1.0 (full target). The state_rabbinate as agenda_setter holds power to define the constraint's content — its exit options are 'arbitrage' (can reinterpret covenant) but institutional identity locks it to the sovereignty reading. Secular framework actors have 'constrained' exit (can emigrate, resist internally, but cannot exit the territorial jurisdiction). Palestinian actors are 'trapped' (no exit from the territory or the claim's enforcement).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Jewish survival and covenantal fulfillment) was live in 1948 and remains contested. The religious reading claims the problem is eternal (covenant is perpetual). Secular and Palestinian readings claim the founding problem was solved by statehood (1948) or never justified the extraction (ongoing). The constraint shows mandatrophy signals: the religious obligation has accumulated state power, settlement infrastructure, and legal architecture that now sustain themselves beyond the theological core. The theater ratio rise (0.15→0.42) tracks this: more enforcement energy goes to maintaining the extraction structure than to the covenantal coordination. But the religious core remains live for adherents — not a dead mandate, a metastasized one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'This constraint is one reading of the contested kernel ''jewish_self_determination'' — specifically the religious_covenant_reading. Sibling readings are liberal_nationalist_reading, indigenous_return_reading, settler_colonial_reading, diasporist_reading. What structural elements do these readings share versus where do they diverge?',
    'Comparative analysis of each reading''s beneficiary/victim structure, claimed_type, and epsilon referent. The engine computes per-reading classifications from structural data; divergence is the measurement.',
    'If readings share the same kernel but produce different constraint classifications, the kernel itself is not a single constraint but a family — confirming the ε-invariance principle. The religious_covenant_reading''s claim of mountain (divine immutability) operationalizing as tangled_rope (state entanglement) would be validated by the classification gap.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment system framing: this reading as one member of a kernel family').

omega_variable(
    divine_command_extraction_boundary,
    'Within the religious framework, the divine command is absolute (epsilon ≈ 0). But the framework itself is contested by secular actors, raising effective extraction. Where is the boundary between ''internal coherence of the religious claim'' and ''extraction imposed on those outside the framework''?',
    'Track whether the constraint''s enforcement machinery (state power, settlement expansion, legal architecture) operates only within the voluntary religious community or extends coercively to non-adherents. The secular negotiation framework''s exclusion is the test.',
    'If enforcement stays within the religious community, the constraint may be a genuine mountain for adherents. If state power enforces it on non-adherents, the extraction is real and the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_command_extraction_boundary, conceptual, 'Whether the religious claim''s internal immutability translates to external extraction').

omega_variable(
    settler_colonial_relation,
    'The religious_covenant_reading and settler_colonial_reading make mutually exclusive claims about the same territorial outcome. Does the religious reading foreclose the settler colonial reading, or do they coexist as competing framings held by different parties?',
    'Analyze whether a single framework could hold both: the religious reading says ''divine right makes this decolonization''; the settler colonial reading says ''European project makes this colonization''. They cannot both be true of the same historical process — but different parties hold each as their operative frame.',
    'If forecloses: the readings are logically incompatible within one framework. If coexists_with: they are competing political theologies held by different factions. The engine computes foreclosure from cs_structure.reading_relations; this omega documents the author''s judgment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settler_colonial_relation, conceptual, 'Structural relationship between religious_covenant_reading and settler_colonial_reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__religious_covenant_reading, 1948, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1948, jewish_self_determination__religious_covenant_reading, theater_ratio, 1948, 0.15).
narrative_ontology:measurement(jewi_tr_t1967, jewish_self_determination__religious_covenant_reading, theater_ratio, 1967, 0.22).
narrative_ontology:measurement(jewi_tr_t1977, jewish_self_determination__religious_covenant_reading, theater_ratio, 1977, 0.28).
narrative_ontology:measurement(jewi_tr_t1993, jewish_self_determination__religious_covenant_reading, theater_ratio, 1993, 0.33).
narrative_ontology:measurement(jewi_tr_t2000, jewish_self_determination__religious_covenant_reading, theater_ratio, 2000, 0.38).
narrative_ontology:measurement(jewi_tr_t2023, jewish_self_determination__religious_covenant_reading, theater_ratio, 2023, 0.42).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1948, jewish_self_determination__religious_covenant_reading, base_extractiveness, 1948, 0.35).
narrative_ontology:measurement(jewi_be_t1967, jewish_self_determination__religious_covenant_reading, base_extractiveness, 1967, 0.52).
narrative_ontology:measurement(jewi_be_t1977, jewish_self_determination__religious_covenant_reading, base_extractiveness, 1977, 0.58).
narrative_ontology:measurement(jewi_be_t1993, jewish_self_determination__religious_covenant_reading, base_extractiveness, 1993, 0.62).
narrative_ontology:measurement(jewi_be_t2000, jewish_self_determination__religious_covenant_reading, base_extractiveness, 2000, 0.66).
narrative_ontology:measurement(jewi_be_t2023, jewish_self_determination__religious_covenant_reading, base_extractiveness, 2023, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1948, jewish_self_determination__religious_covenant_reading, suppression_requirement, 1948, 0.45).
narrative_ontology:measurement(jewi_su_t1967, jewish_self_determination__religious_covenant_reading, suppression_requirement, 1967, 0.58).
narrative_ontology:measurement(jewi_su_t1977, jewish_self_determination__religious_covenant_reading, suppression_requirement, 1977, 0.65).
narrative_ontology:measurement(jewi_su_t1993, jewish_self_determination__religious_covenant_reading, suppression_requirement, 1993, 0.68).
narrative_ontology:measurement(jewi_su_t2000, jewish_self_determination__religious_covenant_reading, suppression_requirement, 2000, 0.72).
narrative_ontology:measurement(jewi_su_t2023, jewish_self_determination__religious_covenant_reading, suppression_requirement, 2023, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__religious_covenant_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_self_determination__religious_covenant_reading, 0.1).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__diasporist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, israeli_settlement_enterprise).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, palestinian_national_movement).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, oslo_peace_process).

% DUAL FORMULATION NOTE:
% The jewish_self_determination kernel decomposes into five constraint stories, one per reading. This reading (religious_covenant) claims mountain-type immutability but operationalizes as tangled_rope through state entanglement. The liberal_nationalist_reading likely computes as rope (coordination with some extraction). The indigenous_return_reading may compute as scaffold (transitional decolonization claim). The settler_colonial_reading computes as snare (pure extraction). The diasporist_reading computes as mountain or rope (diaspora survival as natural law or coordination). The network edges capture structural influence: this reading's state power suppresses the secular negotiation framework that the liberal_nationalist and indigenous_return readings depend on; it fuels the settler_colonial reading's evidence base; it forecloses the diasporist reading's political alternative.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_self_determination__religious_covenant_reading, institutional, 0.1).
constraint_indexing:directionality_override(jewish_self_determination__religious_covenant_reading, organized, 0.85).
constraint_indexing:directionality_override(jewish_self_determination__religious_covenant_reading, powerful, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
