% ============================================================================
% CONSTRAINT STORY: zionist_legitimacy_basis__religious_restoration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: zionist_legitimacy_basis__religious_restoration_reading
 *   human_readable: Religious-Zionist Territorial Mandate (post-1967 messianic reading)
 *   domain: political/religious/settler-colonialism
 *
 * SUMMARY:
 *   This constraint is authored as ONE reading of the contested
 *   zionist_legitimacy_basis kernel: the post-1967 religious-Zionist
 *   interpretation in which territorial control of the West Bank, Gaza, and
 *   other biblical lands is read as fulfillment of divine promise and an
 *   active messianic process (associated with Rabbi Zvi Yehuda Kook and the
 *   Gush Emunim movement and its successors). Under this reading, secular
 *   political considerations — demographic balance, international law,
 *   negotiated peace, security tradeoffs — are subordinate to a religious
 *   obligation to settle and retain the land. This reading coordinates a real
 *   political-religious movement (donor networks, settlement institutions,
 *   coalition politics) while extracting land, infrastructure, and legal
 *   protection from Palestinian and Bedouin residents who have no standing
 *   within the framework that authorizes their displacement. The claimed type
 *   is tangled_rope: it possesses a genuine coordination function (organizing
 *   a mass religious-national movement around shared meaning and political
 *   action) alongside asymmetric extraction sustained by active state
 *   enforcement (military administration, permit regimes, demolition orders)
 *   — not a pure snare, because the coordination function among adherents is
 *   real, and not a rope, because the extraction from non-adherents is
 *   structural and requires ongoing coercive maintenance.
 *
 * KEY AGENTS:
 *   - religious_settler_movement: primary agenda-setter and beneficiary (organized/identity_locked) — treats territorial retention as religious commandment
 *   - west_bank_palestinian_residents: primary target (powerless/trapped) — bears land loss and administrative restriction under the settlement regime
 *   - displaced_bedouin_communities: secondary target (powerless/trapped) — bears repeated relocation under expansion justified theologically
 *   - secular_zionist_and_labor_zionist_institutions: excluded rival tradition (organized/constrained) — the national-liberation reading this constraint displaces in coalition politics
 *   - international_legal_and_diplomatic_bodies: analytical observer (institutional/analytical) — assesses the arrangement against international law, external to the theological framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis__religious_restoration_reading, 0.68).
domain_priors:suppression_score(zionist_legitimacy_basis__religious_restoration_reading, 0.72).
domain_priors:theater_ratio(zionist_legitimacy_basis__religious_restoration_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__religious_restoration_reading, tangled_rope).
narrative_ontology:human_readable(zionist_legitimacy_basis__religious_restoration_reading, "Religious-Zionist Territorial Mandate (post-1967 messianic reading)").
narrative_ontology:topic_domain(zionist_legitimacy_basis__religious_restoration_reading, "political/religious/settler-colonialism").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__religious_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__religious_restoration_reading, 'e4f4175e-c4ab-48c6-bc75-d39487584a56').
narrative_ontology:cs_kernel_codification('e4f4175e-c4ab-48c6-bc75-d39487584a56', fixed_text).
narrative_ontology:cs_authority_grounding('e4f4175e-c4ab-48c6-bc75-d39487584a56', lineage).
narrative_ontology:cs_interpretation_layer_present('e4f4175e-c4ab-48c6-bc75-d39487584a56').
narrative_ontology:cs_reading_relation('e4f4175e-c4ab-48c6-bc75-d39487584a56', zionist_legitimacy_basis__national_liberation_reading, influences).
narrative_ontology:cs_reading_relation('e4f4175e-c4ab-48c6-bc75-d39487584a56', zionist_legitimacy_basis__settler_colonial_reading, coexists_with).
narrative_ontology:cs_axiom('e4f4175e-c4ab-48c6-bc75-d39487584a56', foundational, divine_territorial_mandate_supersedes_secular_negotiation).
narrative_ontology:cs_axiom_status(divine_territorial_mandate_supersedes_secular_negotiation, holdable).
narrative_ontology:cs_axiom_grounding('e4f4175e-c4ab-48c6-bc75-d39487584a56', divine_territorial_mandate_supersedes_secular_negotiation, theological).
narrative_ontology:cs_axiom('e4f4175e-c4ab-48c6-bc75-d39487584a56', foundational, post_1967_conquest_constitutes_messianic_confirmation).
narrative_ontology:cs_axiom_status(post_1967_conquest_constitutes_messianic_confirmation, holdable).
narrative_ontology:cs_axiom_grounding('e4f4175e-c4ab-48c6-bc75-d39487584a56', post_1967_conquest_constitutes_messianic_confirmation, theological).
narrative_ontology:cs_axiom('e4f4175e-c4ab-48c6-bc75-d39487584a56', secondary, territorial_withdrawal_is_religiously_impermissible).
narrative_ontology:cs_axiom_status(territorial_withdrawal_is_religiously_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('e4f4175e-c4ab-48c6-bc75-d39487584a56', territorial_withdrawal_is_religiously_impermissible, deontological).
narrative_ontology:cs_reference_frame('e4f4175e-c4ab-48c6-bc75-d39487584a56', pre_1967_secular_zionist_consensus).
narrative_ontology:cs_drift_state('e4f4175e-c4ab-48c6-bc75-d39487584a56', post_1967_messianic_reinterpretation, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('e4f4175e-c4ab-48c6-bc75-d39487584a56', '').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__religious_restoration_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__religious_restoration_reading, religious_settler_movement).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__religious_restoration_reading, gush_emunim_successor_organizations).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__religious_restoration_reading, settlement_regional_councils).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, west_bank_palestinian_residents).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, secular_israeli_land_use_planners).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, displaced_bedouin_communities).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__religious_restoration_reading, divine_land_grant_doctrine).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__religious_restoration_reading, messianic_process_theology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organizes settlement construction in the West Bank framed as religious commandment (mitzvat yishuv ha'aretz), lobbies government ministries for infrastructure and legal recognition, and treats territorial withdrawal as theologically impermissible. Settlement leaders sit on regional councils that receive state budget allocations and can veto or delay evacuation policy through political coalition leverage. Their identity as a movement is constituted by the settlement project itself; abandoning it is not a policy adjustment but an act experienced as religious betrayal.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, religious_settler_movement, agenda_setter,
    organized, civilizational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__religious_restoration_reading, religious_settler_movement, beneficiary).

% Successor networks to the original Gush Emunim movement provide ideological training, yeshiva funding tied to settlement, and coordinate outpost establishment. They receive donor funding predicated on continued territorial expansion and collect political capital and patronage from allied parties in the Knesset.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, gush_emunim_successor_organizations, beneficiary,
    organized, civilizational, identity_locked, national).

% Administer municipal services, land allocation, and infrastructure budgets for settlements built on the religious-restoration premise. Their institutional survival depends on continued state subsidy justified partly through the religious framing of the land claim.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, settlement_regional_councils, beneficiary,
    institutional, generational, constrained, regional).

% Live under a permit and land-classification regime substantially shaped by settlement expansion justified as fulfilling a divine mandate. Face home demolitions, land expropriation, and checkpoint restrictions tied to areas designated for religious-national settlement. Cannot appeal to the same legitimating framework used against them and have no meaningful exit from the territory or the administrative structure governing it.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, west_bank_palestinian_residents, payer,
    powerless, biographical, trapped, local).

% Communities in areas like the E1 corridor and South Hebron Hills face repeated demolition and relocation orders as settlement expansion proceeds under the theological land-claim rationale. They have no institutional voice in the planning processes that displace them and limited capacity to relocate given restricted land access elsewhere.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, displaced_bedouin_communities, payer,
    powerless, biographical, trapped, local).

% State planning bodies and civil administration officials who must reconcile normal land-use, security, and diplomatic considerations against settlement expansion driven by religious-ideological rather than strategic or economic logic. Their professional planning frameworks are frequently overridden by political decisions made to satisfy the religious-restoration coalition's demands.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, secular_israeli_land_use_planners, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__religious_restoration_reading, secular_israeli_land_use_planners, excluded).

% Historic Zionist institutions grounded in secular national-liberation reasoning find their founding narrative increasingly displaced in public discourse and coalition politics by the religious-messianic framing, particularly regarding territorial compromise. They object that theological maximalism forecloses the two-state and land-for-peace frameworks their tradition considered viable, but hold diminishing coalition leverage relative to religious-nationalist parties.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, secular_zionist_and_labor_zionist_institutions, excluded,
    organized, generational, constrained, national).

% UN bodies, the ICJ, and foreign ministries assess settlement activity against international humanitarian law, treating the religious-restoration justification as legally non-cognizable while noting its political salience within Israeli domestic coalition-building.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, international_legal_and_diplomatic_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(zionist_legitimacy_basis__religious_restoration_reading, religious_settler_movement).
narrative_ontology:fixing_cost_class(zionist_legitimacy_basis__religious_restoration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a religious-nationalist political and settlement movement around a shared theological narrative, mobilizing donor funding, settler recruitment, and coalition votes toward a single, doctrinally justified territorial program that would otherwise fragment across competing security, economic, and diplomatic rationales.
% TRANSFER_FUNCTION: Moves land, water rights, state infrastructure budgets, and legal protection from Palestinian residents and Bedouin communities in the West Bank to the settler movement and its institutional successors, using the theological land-grant claim to legitimate transfers that could not survive purely secular property or security justification.
% ABSENT_VOICES: West Bank Palestinian residents and Bedouin communities subject to the land regime have no standing within the religious-legal framework that authorizes it; secular Zionist institutions objecting to territorial maximalism on national-interest grounds hold shrinking coalition leverage; the sibling national-liberation and settler-colonial readings are excluded by construction from this reading's own account of itself.
% DISAPPEARANCE_RATIONALE: If the religious-restoration justification vanished as a legitimating frame, the settlement enterprise would lose its principal argument against territorial withdrawal, coalition governments would face renewed pressure to negotiate land-for-peace arrangements, and current settlement residents and regional councils would lose the doctrinal basis currently used to resist evacuation, subsidy cuts, or outpost dismantlement — the political and legal terrain of the West Bank would substantially reorganize.
% FOUNDING_PROBLEM: After the 1967 war brought the West Bank, Gaza, East Jerusalem, and the Golan under Israeli control, a theological current within religious Zionism (following Rabbi Zvi Yehuda Kook) reinterpreted the military outcome as divine confirmation that full territorial return to biblical Israel was underway, providing religious meaning and political direction for what secular Zionism had approached as a strategic and demographic question.
% FOUNDING_PROBLEM_CORROBORATION: Religious-Zionist rabbinic authorities and settlement movement leaders attest the messianic process remains live and territorial retention obligatory. Secular Israeli security establishment figures, international legal bodies, and historians of Zionism attest the strategic problem the 1967 conquest posed (borders, demography, international recognition) has been substantially answered through decades of negotiation frameworks and legal rulings, and that the religious framing now function primarily to block negotiated settlement rather than solve an unresolved territorial question; this corroboration comes from outside the religious-restoration movement itself.
narrative_ontology:disappearance_verdict(zionist_legitimacy_basis__religious_restoration_reading, world_rearranges).
narrative_ontology:founding_problem_status(zionist_legitimacy_basis__religious_restoration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__religious_restoration_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(zionist_legitimacy_basis__religious_restoration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zionist_legitimacy_basis__religious_restoration_reading, 0.68, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.68) reflects substantial land, water, and infrastructure transfer sustained by administrative and military enforcement, but not total extraction — the movement genuinely coordinates shared religious meaning and political mobilization among adherents, which is not itself extractive. Suppression (0.72) is high because maintaining the arrangement against Palestinian residents requires active administrative and sometimes military machinery (permits, demolitions, checkpoints) independent of scope or power scaling. Accessibility collapse is moderate (0.4) rather than near-total: alternative Zionist framings (national-liberation, negotiated settlement) remain live in Israeli politics and international discourse, even as their coalition leverage shrinks — this is not a constraint that has fully foreclosed its alternatives, which is why theater_ratio stays moderate (0.3) rather than low: a meaningful share of settlement-adjacent activity is genuinely functional (governance, service provision) rather than purely performative religious maintenance. Resistance (0.75) is high, reflecting sustained Palestinian, international legal, and secular Israeli opposition to the arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   The religious settler movement and its successor organizations sit at the beneficiary end: they set the agenda, receive land and subsidy, and their exit is identity_locked because the settlement project constitutes their religious and communal identity rather than being a policy position they hold. West Bank Palestinian residents and displaced Bedouin communities sit at the full-target end: trapped, powerless, bearing the transfer with no standing in the legitimating framework. Secular Israeli land-use planners occupy an intermediate position — they administer the system but experience its religious-ideological override of their professional judgment as a cost, making them a payer within the state apparatus itself. Secular Zionist institutions are excluded rather than victimized in the same material sense — their loss is political and narrative displacement rather than land loss.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (giving religious-political meaning and direction to the unexpected 1967 territorial outcome) has, by the corroboration of secular Israeli security and international legal observers, been substantially superseded by decades of negotiation frameworks and legal rulings addressing the same territorial questions through secular means — yet the religious-restoration reading persists and has hardened (rising suppression_requirement, rising extractiveness) rather than receding, which is the mandatrophy signature: an arrangement whose original justificatory problem has been substantially answered elsewhere continuing to expand its claims and enforcement machinery. Classifying this as tangled_rope rather than snare prevents mislabeling the movement's genuine internal coordination function (shared meaning, communal life, religious practice) as pure fabricated cover — the coordination is real for adherents — while still registering the asymmetric, enforcement-dependent extraction from non-adherents that a pure rope classification would erase.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    religious_obligation_vs_political_strategy,
    'Is the messianic-process framing a genuine, independently-held theological conviction that would persist absent political utility, or is it substantially adopted/amplified because it provides otherwise-unavailable justification for territorial retention that secular strategic reasoning cannot supply?',
    'Compare religious-Zionist doctrinal commitments and settlement advocacy before and after periods when secular security/demographic arguments for retention weakened (e.g., post-Oslo, post-Gaza disengagement) — if theological intensity tracks political utility rather than independent doctrinal development, the religious framing functions partly as a legitimating cover.',
    'If theologically autonomous, the coordination function is more genuine and the tangled_rope classification''s coordination side is stronger; if politically reactive, the extraction side dominates and a snare reading becomes more defensible from outside the tradition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(religious_obligation_vs_political_strategy, conceptual, 'Whether the religious-restoration doctrine is autonomous conviction or politically reactive legitimation.').

omega_variable(
    kernel_reading_incommensurability,
    'Can the religious_restoration_reading, national_liberation_reading, and settler_colonial_reading be reconciled as complementary partial descriptions of one kernel, or are they genuinely incommensurable framings that different parties will never converge on because they rest on incompatible foundational premises (divine mandate vs. secular self-determination vs. colonial critique)?',
    'This is the committer-frame question the kernel decomposition exists to hold open; no empirical resolution mechanism exists because the disagreement is over which normative framework governs land legitimacy, not over contested facts.',
    'If incommensurable, all three readings must persist as permanently separate constraint stories with no meta-reading that adjudicates between them; if reconcilable, a fourth synthesis story might be warranted describing the overlapping structural features (settlement, displacement, enforcement) independent of legitimating frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether the three kernel readings are reconcilable or genuinely incommensurable.').

omega_variable(
    demographic_trajectory_ambiguity,
    'Does continued settlement expansion under the religious-restoration framework foreclose a viable two-state or shared-sovereignty outcome as a matter of demographic and territorial fact, independent of which legitimating narrative is used?',
    'Track settlement population growth, contiguous land allocation, and infrastructure entrenchment against thresholds identified in prior negotiation frameworks (e.g., Clinton Parameters land-swap ratios) to determine whether physical facts on the ground have passed a point of practical irreversibility.',
    'If demographic/territorial foreclosure has occurred, the ''disappearance_verdict: world_rearranges'' understates the case — even removing the legitimating narrative might not restore the alternative outcomes it currently blocks, since the physical infrastructure would remain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_trajectory_ambiguity, empirical, 'Whether settlement expansion has produced irreversible facts on the ground independent of its justificatory framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis__religious_restoration_reading, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zion_tr_t0, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(zion_tr_t0, observed).
narrative_ontology:measurement(zion_tr_t9, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 9, 0.15).
narrative_ontology:measurement_basis(zion_tr_t9, observed).
narrative_ontology:measurement(zion_tr_t18, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 18, 0.18).
narrative_ontology:measurement_basis(zion_tr_t18, observed).
narrative_ontology:measurement(zion_tr_t27, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 27, 0.22).
narrative_ontology:measurement_basis(zion_tr_t27, observed).
narrative_ontology:measurement(zion_tr_t36, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 36, 0.26).
narrative_ontology:measurement_basis(zion_tr_t36, observed).
narrative_ontology:measurement(zion_tr_t45, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 45, 0.28).
narrative_ontology:measurement_basis(zion_tr_t45, observed).
narrative_ontology:measurement(zion_tr_t55, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 55, 0.3).
narrative_ontology:measurement_basis(zion_tr_t55, observed).

% Extraction over time
narrative_ontology:measurement(zion_be_t0, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(zion_be_t0, observed).
narrative_ontology:measurement(zion_be_t9, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 9, 0.5).
narrative_ontology:measurement_basis(zion_be_t9, observed).
narrative_ontology:measurement(zion_be_t18, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 18, 0.55).
narrative_ontology:measurement_basis(zion_be_t18, observed).
narrative_ontology:measurement(zion_be_t27, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 27, 0.6).
narrative_ontology:measurement_basis(zion_be_t27, observed).
narrative_ontology:measurement(zion_be_t36, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 36, 0.64).
narrative_ontology:measurement_basis(zion_be_t36, observed).
narrative_ontology:measurement(zion_be_t45, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 45, 0.66).
narrative_ontology:measurement_basis(zion_be_t45, observed).
narrative_ontology:measurement(zion_be_t55, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 55, 0.68).
narrative_ontology:measurement_basis(zion_be_t55, observed).

% Suppression requirement over time
narrative_ontology:measurement(zion_su_t0, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(zion_su_t0, observed).
narrative_ontology:measurement(zion_su_t9, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 9, 0.52).
narrative_ontology:measurement_basis(zion_su_t9, observed).
narrative_ontology:measurement(zion_su_t18, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 18, 0.58).
narrative_ontology:measurement_basis(zion_su_t18, observed).
narrative_ontology:measurement(zion_su_t27, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 27, 0.63).
narrative_ontology:measurement_basis(zion_su_t27, observed).
narrative_ontology:measurement(zion_su_t36, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 36, 0.67).
narrative_ontology:measurement_basis(zion_su_t36, observed).
narrative_ontology:measurement(zion_su_t45, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 45, 0.7).
narrative_ontology:measurement_basis(zion_su_t45, observed).
narrative_ontology:measurement(zion_su_t55, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 55, 0.72).
narrative_ontology:measurement_basis(zion_su_t55, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis__religious_restoration_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(zionist_legitimacy_basis__religious_restoration_reading, 0.08).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, zionist_legitimacy_basis__national_liberation_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, zionist_legitimacy_basis__settler_colonial_reading).

% DUAL FORMULATION NOTE:
% Part of a three-member constraint family decomposing the contested zionist_legitimacy_basis kernel per the ε-invariance principle: religious_restoration_reading (this story, tangled_rope, ε=0.68), national_liberation_reading (separate story, expected lower ε reflecting the self-determination/refuge framing), and settler_colonial_reading (separate story, expected higher ε reflecting the colonial-displacement framing). Each reading authors its own ε for the standing arrangement as that reading's own lights assess it; the readings are linked via affects_constraints because they compete for the same legitimating space in Israeli and international discourse and each reading's political success measurably affects the others' institutional standing and resource availability (e.g., religious-restoration coalition strength directly displaces national-liberation framing's influence on Israeli negotiating positions).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
