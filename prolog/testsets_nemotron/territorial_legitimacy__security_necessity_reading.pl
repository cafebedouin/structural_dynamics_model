% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__security_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-24
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
 *   constraint_id: territorial_legitimacy__security_necessity_reading
 *   human_readable: Territorial Legitimacy via Security Necessity and Defensive Depth
 *   domain: political_theory/international_law/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint story instantiates the security_necessity_reading of the
 *   territorial_legitimacy kernel. It treats Israeli control of the West
 *   Bank, East Jerusalem, and Golan Heights as legitimate security
 *   requirements: the 1967 lines plus strategic depth are the minimal
 *   defensible borders; Palestinian sovereignty is conditional on
 *   demilitarization and security cooperation; settlements are legitimate as
 *   security outposts. The claimed type is tangled_rope — a genuine
 *   coordination function (defensible borders for a state that faced
 *   existential wars) fused with asymmetric extraction (permanent territorial
 *   control, resource appropriation, demographic engineering imposed on a
 *   stateless population). The engine computes per-seat classifications from
 *   the structural data below; the authored metrics describe the constraint's
 *   actual operation over 1967–2024.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__security_necessity_reading, 0.78).
domain_priors:suppression_score(territorial_legitimacy__security_necessity_reading, 0.85).
domain_priors:theater_ratio(territorial_legitimacy__security_necessity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__security_necessity_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy__security_necessity_reading, "Territorial Legitimacy via Security Necessity and Defensive Depth").
narrative_ontology:topic_domain(territorial_legitimacy__security_necessity_reading, "political_theory/international_law/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy__security_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__security_necessity_reading, '478f5cb6-0e35-4cfc-b349-e55508b1e937').
narrative_ontology:cs_kernel_codification('478f5cb6-0e35-4cfc-b349-e55508b1e937', distributed).
narrative_ontology:cs_authority_grounding('478f5cb6-0e35-4cfc-b349-e55508b1e937', extraction).
narrative_ontology:cs_interpretation_layer_present('478f5cb6-0e35-4cfc-b349-e55508b1e937').
narrative_ontology:cs_reading_relation('478f5cb6-0e35-4cfc-b349-e55508b1e937', territorial_legitimacy__partition_reading, coexists_with).
narrative_ontology:cs_reading_relation('478f5cb6-0e35-4cfc-b349-e55508b1e937', territorial_legitimacy__indigenous_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('478f5cb6-0e35-4cfc-b349-e55508b1e937', foundational, strategic_depth_is_existential_for_jewish_survival).
narrative_ontology:cs_axiom_status(strategic_depth_is_existential_for_jewish_survival, holdable).
narrative_ontology:cs_axiom_grounding('478f5cb6-0e35-4cfc-b349-e55508b1e937', strategic_depth_is_existential_for_jewish_survival, instrumental).
narrative_ontology:cs_axiom('478f5cb6-0e35-4cfc-b349-e55508b1e937', foundational, palestinian_sovereignty_conditional_on_demilitarization).
narrative_ontology:cs_axiom_status(palestinian_sovereignty_conditional_on_demilitarization, holdable).
narrative_ontology:cs_axiom_grounding('478f5cb6-0e35-4cfc-b349-e55508b1e937', palestinian_sovereignty_conditional_on_demilitarization, conventional).
narrative_ontology:cs_axiom('478f5cb6-0e35-4cfc-b349-e55508b1e937', secondary, settlements_as_security_assets).
narrative_ontology:cs_axiom_status(settlements_as_security_assets, holdable).
narrative_ontology:cs_axiom_grounding('478f5cb6-0e35-4cfc-b349-e55508b1e937', settlements_as_security_assets, instrumental).
narrative_ontology:cs_reference_frame('478f5cb6-0e35-4cfc-b349-e55508b1e937', pre_1967_indefensible_borders).
narrative_ontology:cs_drift_state('478f5cb6-0e35-4cfc-b349-e55508b1e937', post_oslo_post_second_intifada, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('478f5cb6-0e35-4cfc-b349-e55508b1e937', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy__security_necessity_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, israeli_state).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, israeli_settler_movement).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, israeli_defense_establishment).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, palestinian_population_west_bank).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, palestinian_population_gaza).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, syrian_population_golan).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, palestinian_refugees).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, us_foreign_policy_establishment).
narrative_ontology:constraint_vindicates(territorial_legitimacy__security_necessity_reading, defensive_war_legitimizes_territorial_retention).
narrative_ontology:constraint_vindicates(territorial_legitimacy__security_necessity_reading, strategic_depth_is_existential_for_small_states).
narrative_ontology:constraint_vindicates(territorial_legitimacy__security_necessity_reading, demilitarization_precondition_for_adversary_sovereignty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers military occupation, settlement enterprise, and security coordination in West Bank; annexed Golan Heights and East Jerusalem. Sets the legal framework defining Palestinian autonomy as conditional on security compliance. Collects strategic depth, water resources, and demographic control as primary benefits.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, israeli_state, agenda_setter,
    institutional, generational, arbitrage, regional).

% Operationalizes the security doctrine: determines "strategic depth" requirements, designs the barrier route, authorizes settlement placement as security assets, and controls movement permits. Its institutional mandate and budget expand with the territorial envelope.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, israeli_defense_establishment, beneficiary,
    institutional, generational, arbitrage, regional).

% Physically inhabits the strategic depth, creating facts on the ground that make withdrawal politically costly. Gains subsidized housing, ideological fulfillment, and political leverage. Exit would mean abandoning a messianic/ideological life project — identity-locked.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, israeli_settler_movement, beneficiary,
    organized, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__security_necessity_reading, israeli_settler_movement, agenda_setter).

% Subject to military law, permit regime, land expropriation, settlement expansion, and fragmentation into Areas A/B/C. Bears the daily costs of the security architecture: checkpoints, night raids, demolished homes, restricted agriculture. No exit — stateless, territorially fragmented, economically dependent.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, palestinian_population_west_bank, payer,
    powerless, biographical, trapped, local).

% Under blockade since 2007; subject to repeated military campaigns justified by the same security doctrine. The "strategic depth" logic treats Gaza as a security threat to be contained, not a population with rights. Exit is physically impossible; economic collapse is enforced.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, palestinian_population_gaza, payer,
    powerless, immediate, trapped, local).

% Displaced from Golan Heights in 1967; the territory annexed by Israel in 1981. The strategic depth rationale treats the plateau as non-negotiable high ground. No return, no compensation, no political pathway — the constraint's logic froze their loss as permanent.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, syrian_population_golan, payer,
    powerless, generational, trapped, local).

% Descendants of 1948 and 1967 displacement; their right of return is structurally excluded by the security necessity reading, which treats demographic reversal as an existential threat. They are not parties to any negotiation — the constraint's logic renders their claim illegible.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, palestinian_refugees, excluded,
    powerless, generational, trapped, regional).

% ICJ, UNSC, ICC, and treaty bodies consistently rule the occupation illegal, settlements a war crime, annexation null. Their rulings are structurally ineffective — the constraint's enforcement machinery (US veto, Israeli non-cooperation) neutralizes them. They observe, document, and declare without altering the facts on the ground.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, international_legal_community, observer,
    institutional, civilizational, analytical, universal).

% Provides diplomatic cover, military aid, and veto protection that make the constraint sustainable. Gains a regional ally, intelligence partnership, and domestic political capital. Exit is constrained by domestic politics (evangelical, pro-Israel lobbies) and strategic doctrine — but not identity-locked; policy shifts have occurred (e.g., Reagan 1982, Bush 1991, Obama 2016).
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, us_foreign_policy_establishment, beneficiary,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__security_necessity_reading, us_foreign_policy_establishment, agenda_setter).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a recognized security framework for a state facing repeated existential wars (1948, 1967, 1973) and ongoing asymmetric threats. The 1967 lines plus strategic depth are presented as the minimal territorial envelope for defensible borders, replacing the 1949 armistice lines that proved indefensible.
% TRANSFER_FUNCTION: Moves territorial control, water aquifers, high ground, and demographic majority from the indigenous Palestinian/Syrian population to the Israeli state and settler movement. The transfer is enforced by military law, planning regimes, and the permit system — all justified as security requirements.
% ABSENT_VOICES: Palestinian refugees (right of return excluded as demographic threat), Syrian Golan residents (displaced, no return path), Palestinian citizens of Israel (subject to the same security logic inside 1948 lines), and the international legal community (rulings structurally neutralized). These voices would challenge the security necessity premise, the permanence of the arrangement, and the legitimacy of demographic engineering — but they are not seated at the table where the constraint is authored.
% DISAPPEARANCE_RATIONALE: If the security necessity reading vanished overnight, the legal basis for settlements, annexation, the permit regime, the blockade, and the strategic depth doctrine would collapse. The world would rearrange toward either a partition framework (1967 lines with swaps) or a rights-based framework (equal rights in one state or two sovereign states). The facts on the ground (700k+ settlers, infrastructure, water control) would remain, but their legitimacy would shift from "necessary" to "contested/illegal," triggering a different political physics.
% FOUNDING_PROBLEM: The 1949 armistice lines left Israel 14km wide at its narrowest, with Jerusalem divided, the coastal plain overlooked by the West Bank ridge, and the Galilee overlooked by the Golan. Three existential wars (1948, 1967, 1973) were launched from these territories. The founding problem: how does a small state achieve defensible borders against neighbors who refuse recognition and threaten annihilation?
% FOUNDING_PROBLEM_CORROBORATION: Israeli security establishment and mainstream Zionist historiography attest the founding problem remains live: Iran's axis, Hamas's charter, and October 7 are cited as proof that the existential threat persists. Palestinian, Syrian, and international legal voices attest the founding problem is substantially resolved: Israel is a nuclear-armed regional superpower with peace treaties with Egypt and Jordan, and the "strategic depth" rationale now serves expansion, not survival. No neutral arbiter exists; the contest is structural.
narrative_ontology:disappearance_verdict(territorial_legitimacy__security_necessity_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy__security_necessity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__security_necessity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(territorial_legitimacy__security_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy__security_necessity_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extraction is high (0.78) and rising: the security rationale has expanded from narrow military necessity (1967) to encompass settlement blocs, the Jordan Valley, East Jerusalem, and the Golan — a territorial envelope that maximizes Israeli control while minimizing Palestinian viability. Suppression is very high (0.85): the constraint persists only through active military enforcement (checkpoints, permits, the barrier, blockade, annexation law). Theater ratio is moderate (0.42) and rising: the security coordination function (Area A PA security forces, intelligence sharing) is real but increasingly performs as cover for the extraction envelope. Accessibility collapse (0.68) reflects that alternatives (1967 lines, binational equality, refugee return) are structurally blocked by the constraint's own logic. Resistance (0.72) is high: two intifadas, ongoing diplomatic/legal/BDS campaigns, and the persistence of Palestinian national claims.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter/beneficiary seats (Israeli state, defense establishment, settler movement, US establishment) experience this as a necessary coordination mechanism — the only framework that secures Jewish survival in a hostile region. The payer seats (Palestinians, Syrians) experience it as a snare — a permanent extraction machine that uses security as cover for territorial maximalism. The observer seat (international law) reads it as a clear violation of jus cogens norms. The engine computes these divergent seat types from the same structural data; the divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Israeli state and defense establishment are structural beneficiaries (d near 0.0): they set the rules, collect the territory/resources, and face arbitrage-grade exit (can withdraw unilaterally, as from Sinai 1982, Gaza 2005, Lebanon 2000). Settler movement is beneficiary but identity-locked (d ~0.15): they gain materially and ideologically but cannot exit without abandoning their life project. US establishment is beneficiary with constrained exit (d ~0.25): gains strategic alliance but could shift policy. Palestinian populations (West Bank, Gaza) and Syrian Golan residents are full targets (d near 1.0): they bear the costs, have trapped exit, and the constraint's logic defines their claims as illegitimate. Refugees are excluded (d = 1.0): not even recognized as parties. International legal community is analytical observer (d = 0.5): sees the full structure but cannot affect it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (defensible borders against existential threat) was live in 1967 and remained live through 1973. Since the Egypt peace treaty (1979), Jordan treaty (1994), Israel's nuclear monopoly, and the collapse of conventional Arab military threat, the founding problem has attenuated — but the constraint expanded. The security necessity reading now serves as the legitimating mantle for a settlement enterprise that the defense establishment itself sometimes opposes (e.g., isolated settlements). The constraint has not been resolved; it has been repurposed. Mandatrophy is present but contested: the reading's beneficiaries insist the founding problem persists (Iran, Hamas, October 7); its victims and the international legal community insist the problem is gone and the arrangement is now pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    security_necessity_vs_expansionism,
    'Does the strategic depth doctrine genuinely require the current territorial envelope (settlement blocs, Jordan Valley, East Jerusalem, Golan), or has it become a cover for ideological/religious expansionism?',
    'Compare IDF operational requirements (classified) with settlement map; test whether security objectives can be met by temporary military presence vs. permanent civilian settlement; examine whether settlement placement correlates with security logic or ideological/religious sites.',
    'If the doctrine is cover, the constraint is snare not tangled_rope — the coordination function is pretext. If genuine but excessive, it remains tangled_rope with a wider coordination-extraction gap.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(security_necessity_vs_expansionism, conceptual, 'Whether the security rationale is structurally necessary or a legitimating narrative for territorial maximalism.').

omega_variable(
    demilitarization_feasibility,
    'Is a demilitarized Palestinian state on 1967 lines (with land swaps) a stable equilibrium, or does the security necessity reading structurally require permanent Israeli military control of the Jordan Valley, airspace, and electromagnetic spectrum?',
    'Analyze the Oslo-era security proposals (Clinton Parameters, Olmert 2008, Kerry 2014) — all included long-term Israeli military presence in the Jordan Valley. Test whether any Israeli government has offered full withdrawal to the 1967 line without security reservations.',
    'If demilitarization is structurally impossible under this reading, Palestinian sovereignty is a permanent fiction — the constraint is snare. If a genuine demilitarized sovereignty deal is possible, the coordination function has a realizable endpoint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demilitarization_feasibility, empirical, 'Whether the reading''s own coordination logic admits a terminal non-extractive state.').

omega_variable(
    committer_kernel_framing,
    'This constraint is one reading (security_necessity_reading) of the territorial_legitimacy kernel. The sibling readings (partition_reading, indigenous_continuity_reading) produce different ε, different victims, different types. Where exactly does the structural disagreement locate?',
    'Map the three readings'' beneficiary/victim sets, ε values, and claimed types. The disagreement is not about facts on the ground but about which facts constitute legitimacy: international law (partition), historical habitation (indigenous), or military defensibility (security).',
    'If the kernel is irresolvable (no shared epistemic ground), the three constraints form a permanent constraint family with no synthesis — each reading forecloses the others'' legitimacy claims. If resolvable, a fourth constraint (e.g., two-state with mutual recognition) might emerge.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_framing, conceptual, 'Structural locus of disagreement between the three territorial_legitimacy readings.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the high suppression (0.85) primarily structural (military enforcement, legal barriers, geographic fragmentation) or partially internalized (Palestinian political fragmentation, security coordination by PA, resignation to the permit regime)?',
    'Track suppression trajectory after hypothetical constraint removal: if Palestinian self-governance capacity re-emerges rapidly, suppression was largely structural. If governance vacuum, internalized dependence, or factional collapse persist, internalized component is significant.',
    'If internalized suppression is substantial, the constraint''s effective extraction is higher than the structural measure suggests — the target population carries the suppression internally. This would increase the computed χ for payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the Palestinian payer seats.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__security_necessity_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1967, territorial_legitimacy__security_necessity_reading, theater_ratio, 1967, 0.15).
narrative_ontology:measurement(terr_tr_t1973, territorial_legitimacy__security_necessity_reading, theater_ratio, 1973, 0.18).
narrative_ontology:measurement(terr_tr_t1987, territorial_legitimacy__security_necessity_reading, theater_ratio, 1987, 0.22).
narrative_ontology:measurement(terr_tr_t1993, territorial_legitimacy__security_necessity_reading, theater_ratio, 1993, 0.28).
narrative_ontology:measurement(terr_tr_t2000, territorial_legitimacy__security_necessity_reading, theater_ratio, 2000, 0.33).
narrative_ontology:measurement(terr_tr_t2005, territorial_legitimacy__security_necessity_reading, theater_ratio, 2005, 0.36).
narrative_ontology:measurement(terr_tr_t2014, territorial_legitimacy__security_necessity_reading, theater_ratio, 2014, 0.39).
narrative_ontology:measurement(terr_tr_t2024, territorial_legitimacy__security_necessity_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(terr_be_t1967, territorial_legitimacy__security_necessity_reading, base_extractiveness, 1967, 0.45).
narrative_ontology:measurement(terr_be_t1973, territorial_legitimacy__security_necessity_reading, base_extractiveness, 1973, 0.52).
narrative_ontology:measurement(terr_be_t1987, territorial_legitimacy__security_necessity_reading, base_extractiveness, 1987, 0.58).
narrative_ontology:measurement(terr_be_t1993, territorial_legitimacy__security_necessity_reading, base_extractiveness, 1993, 0.62).
narrative_ontology:measurement(terr_be_t2000, territorial_legitimacy__security_necessity_reading, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement(terr_be_t2005, territorial_legitimacy__security_necessity_reading, base_extractiveness, 2005, 0.71).
narrative_ontology:measurement(terr_be_t2014, territorial_legitimacy__security_necessity_reading, base_extractiveness, 2014, 0.74).
narrative_ontology:measurement(terr_be_t2024, territorial_legitimacy__security_necessity_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1967, territorial_legitimacy__security_necessity_reading, suppression_requirement, 1967, 0.55).
narrative_ontology:measurement(terr_su_t1973, territorial_legitimacy__security_necessity_reading, suppression_requirement, 1973, 0.62).
narrative_ontology:measurement(terr_su_t1987, territorial_legitimacy__security_necessity_reading, suppression_requirement, 1987, 0.71).
narrative_ontology:measurement(terr_su_t1993, territorial_legitimacy__security_necessity_reading, suppression_requirement, 1993, 0.74).
narrative_ontology:measurement(terr_su_t2000, territorial_legitimacy__security_necessity_reading, suppression_requirement, 2000, 0.78).
narrative_ontology:measurement(terr_su_t2005, territorial_legitimacy__security_necessity_reading, suppression_requirement, 2005, 0.81).
narrative_ontology:measurement(terr_su_t2014, territorial_legitimacy__security_necessity_reading, suppression_requirement, 2014, 0.83).
narrative_ontology:measurement(terr_su_t2024, territorial_legitimacy__security_necessity_reading, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy__security_necessity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(territorial_legitimacy__security_necessity_reading, 0.12).
narrative_ontology:affects_constraint(territorial_legitimacy__security_necessity_reading, territorial_legitimacy__partition_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__security_necessity_reading, territorial_legitimacy__indigenous_continuity_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__security_necessity_reading, jerusalem_status_constraint).
narrative_ontology:affects_constraint(territorial_legitimacy__security_necessity_reading, palestinian_refugee_return_constraint).
narrative_ontology:affects_constraint(territorial_legitimacy__security_necessity_reading, golan_annexation_constraint).
narrative_ontology:affects_constraint(territorial_legitimacy__security_necessity_reading, jordan_valley_security_constraint).

% DUAL FORMULATION NOTE:
% This is one of three constraints in the territorial_legitimacy kernel family. The partition_reading (1947/1949 lines, international law) and indigenous_continuity_reading (1948 Nakba, anti-colonial self-determination) are sibling constraints. All three share the same geographic referent but instantiate different ε, different beneficiary/victim structures, and different claimed types. The security_necessity_reading is the currently dominant enforcement constraint; the other two are aspirational/counterfactual constraints that structure resistance and legal discourse.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_legitimacy__security_necessity_reading, institutional, 0.1).
constraint_indexing:directionality_override(territorial_legitimacy__security_necessity_reading, organized, 0.15).
constraint_indexing:directionality_override(territorial_legitimacy__security_necessity_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
