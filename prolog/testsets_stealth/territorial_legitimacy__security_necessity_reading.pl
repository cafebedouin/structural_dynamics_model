% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__security_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: Territorial Legitimacy via Security Necessity and Defensive Control (Post-1967 Strategic Depth)
 *   domain: political theory/international law/territorial sovereignty
 *
 * SUMMARY:
 *   This story instantiates one reading of the contested
 *   territorial-legitimacy kernel: the claim that control of the West Bank
 *   and Golan Heights acquired in the June 1967 war is legitimate because
 *   defensive necessity requires it. The pre-1967 lines left the country's
 *   coastal center roughly nine miles wide at the waist; on this reading the
 *   heights, the river valley, and the demilitarized approaches supply the
 *   depth, early warning, and reaction time a state facing annihilation-level
 *   threats cannot trade away. Palestinian sovereignty is real but
 *   conditional on demilitarization, and the communities beyond the green
 *   line are framed as a security presence. The story assesses the standing
 *   arrangement — the post-1967 control regime as it actually operates — by
 *   this reading's own lights: epsilon is indexed to what a sincere holder of
 *   the security-necessity premise would measure of this arrangement, not to
 *   what the sibling readings (partition, indigenous continuity) would
 *   measure of the same territory. Those siblings are separate constraints
 *   with separate files. Claim and metrics are authored independently: the
 *   reading claims a tangled_rope (genuine defensive coordination that
 *   concedes asymmetric costs), while the metrics describe the regime's
 *   actual operation.
 *
 * KEY AGENTS:
 *   - - israeli_security_establishment: Agenda setter (institutional/identity_locked) — defines what counts as a security requirement and administers the control regime
 *   - - west_bank_settler_councils: Primary beneficiary (institutional/identity_locked) — receives land, construction, roads, water, and permanence
 *   - - palestinian_west_bank_residents: Primary target (powerless/trapped) — bears movement, land-access, water, and adjudication burdens
 *   - - syrian_golan_displaced: Secondary target (powerless/trapped) — displaced in 1967, restitution never adjudicated
 *   - - israeli_civilian_population: Dual-positioned beneficiary/payer (organized/constrained) — gains buffer protection, pays conscription, casualty risk, and fiscal cost
 *   - - palestinian_authority: Incorporated intermediary (moderate/trapped) — cedes security control, collects conditional revenue and coordination rents
 *   - - international_legal_institutions: Analytical observer (institutional/analytical) — adjudicates legality without enforcement capacity
 *   - - great_power_patron: Beneficiary/observer (powerful/arbitrage) — supplies diplomatic shielding and assistance, collects strategic returns
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__security_necessity_reading, 0.46).
domain_priors:suppression_score(territorial_legitimacy__security_necessity_reading, 0.82).
domain_priors:theater_ratio(territorial_legitimacy__security_necessity_reading, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, extractiveness, 0.46).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__security_necessity_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy__security_necessity_reading, "Territorial Legitimacy via Security Necessity and Defensive Control (Post-1967 Strategic Depth)").
narrative_ontology:topic_domain(territorial_legitimacy__security_necessity_reading, "political theory/international law/territorial sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy__security_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__security_necessity_reading, '284dc5bd-a216-42ac-89cd-c7c059ae85d6').
narrative_ontology:cs_kernel_codification('284dc5bd-a216-42ac-89cd-c7c059ae85d6', formalized).
narrative_ontology:cs_authority_grounding('284dc5bd-a216-42ac-89cd-c7c059ae85d6', practice).
narrative_ontology:cs_interpretation_layer_present('284dc5bd-a216-42ac-89cd-c7c059ae85d6').
narrative_ontology:cs_reading_relation('284dc5bd-a216-42ac-89cd-c7c059ae85d6', territorial_legitimacy__partition_reading, coexists_with).
narrative_ontology:cs_reading_relation('284dc5bd-a216-42ac-89cd-c7c059ae85d6', territorial_legitimacy__indigenous_continuity_reading, influences).
narrative_ontology:cs_axiom('284dc5bd-a216-42ac-89cd-c7c059ae85d6', foundational, survival_trumps_prior_territorial_title).
narrative_ontology:cs_axiom_status(survival_trumps_prior_territorial_title, holdable).
narrative_ontology:cs_axiom_grounding('284dc5bd-a216-42ac-89cd-c7c059ae85d6', survival_trumps_prior_territorial_title, deontological).
narrative_ontology:cs_axiom('284dc5bd-a216-42ac-89cd-c7c059ae85d6', foundational, palestinian_sovereignty_conditional_on_demilitarization).
narrative_ontology:cs_axiom_status(palestinian_sovereignty_conditional_on_demilitarization, holdable).
narrative_ontology:cs_axiom_grounding('284dc5bd-a216-42ac-89cd-c7c059ae85d6', palestinian_sovereignty_conditional_on_demilitarization, instrumental).
narrative_ontology:cs_reference_frame('284dc5bd-a216-42ac-89cd-c7c059ae85d6', defensible_frontiers_strategic_depth).
narrative_ontology:cs_drift_state('284dc5bd-a216-42ac-89cd-c7c059ae85d6', contemporary_post_october_2023, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('284dc5bd-a216-42ac-89cd-c7c059ae85d6', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy__security_necessity_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, israeli_security_establishment).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, west_bank_settler_councils).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, israeli_civilian_population).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, great_power_patron).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, palestinian_west_bank_residents).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, syrian_golan_displaced).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, palestinian_authority).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, israeli_civilian_population).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, palestinian_authority).
narrative_ontology:constraint_vindicates(territorial_legitimacy__security_necessity_reading, defensible_borders_principle).
narrative_ontology:constraint_vindicates(territorial_legitimacy__security_necessity_reading, unsc242_secure_boundaries_formula).
narrative_ontology:constraint_vindicates(territorial_legitimacy__security_necessity_reading, demilitarization_precondition_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Plans and runs the defense posture west of the Jordan River and on the Golan: commands the military government, sets checkpoint and permit policy, designates firing zones and closure areas, and operates intelligence coordination with neighboring forces. Its planning documents define what counts as a security requirement, and its officers testify in court about necessity. Stepping back from the administered territory would mean dissolving the operational domain, career paths, doctrine, and budget lines built around five decades of running it.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, israeli_security_establishment, agenda_setter,
    institutional, generational, identity_locked, national).

% Represent communities established beyond the 1967 lines, many sited on hilltops overlooking approach roads. They receive land allocations, construction approvals, bypass roads, water quotas, and subsidized services through the civil administration, and their elected councils exercise quasi-municipal authority. Leaving would mean abandoning homes, schools, and synagogues built over generations; the 2005 evacuations required physical removal and left lasting political scars that make voluntary departure politically unthinkable for the leadership.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, west_bank_settler_councils, beneficiary,
    institutional, generational, identity_locked, regional).

% Gains the perceived protection of buffer distance, early-warning positions, and the barrier system; pays through universal conscription and extended reserve duty, casualty risk during escalation cycles, and the fiscal cost of administering the territories. Public opinion supports the security rationale while remaining divided on the communities beyond the green line. Individual exit by emigration exists but is socially costly and shifts the burden onto those who remain.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, israeli_civilian_population, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__security_necessity_reading, israeli_civilian_population, payer).

% Live under a permit regime governing movement between towns, access to Jerusalem, and employment inside Israel; farm land adjacent to settlements and firing zones subject to closure orders; draw water under committee-set quotas while nearby settlements consume more per capita. Political expression and much legal recourse run through military courts. Exit means leaving the land entirely — emigration or internal displacement — which forfeits property and any future claim on it.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, palestinian_west_bank_residents, payer,
    powerless, generational, trapped, regional).

% Fled or were driven from the Golan in 1967; roughly 130,000 were displaced and their villages demolished. Those who remained, mostly Druze, were offered Israeli citizenship, which most declined for decades. Property restitution claims have never been adjudicated in any forum. Return to the village sites is barred, and the state they fled to does not recognize the transfer, leaving no exit in either direction.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, syrian_golan_displaced, payer,
    powerless, generational, trapped, regional).

% Administers populated enclaves under a security ceiling negotiated in the 1990s: it fields police for internal order but cedes top-tier security control, and its budget depends on tax revenues collected by Israel and transferred conditionally on continued coordination. Its officials staff the liaison committees where quota and movement rules are implemented. Dissolving the arrangement would cost the governing elite its salaries, prerogatives, and external patronage; continuing it costs legitimacy with its own public.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, palestinian_authority, payer,
    moderate, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__security_necessity_reading, palestinian_authority, beneficiary).

% Adjudicate and opine on the legality of the control regime: the ICJ issued advisory opinions on the barrier in 2004 and on the occupation's lawfulness in 2024, and UN bodies pass recurring resolutions. They command no enforcement capacity of their own and depend on member states for any practical effect.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, international_legal_institutions, observer,
    institutional, civilizational, analytical, global).

% Supplies diplomatic shielding at the Security Council, military assistance, and mediation frameworks, and collects strategic returns: regional leverage, a tested ally, and intelligence cooperation. It alternates between endorsing the security rationale and pressing for construction freezes. Its commitments shift with administrations, and it can redirect support at electoral cadence — an exit option no other seat possesses.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, great_power_patron, beneficiary,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__security_necessity_reading, great_power_patron, observer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy__security_necessity_reading, west_bank_settler_councils).
narrative_ontology:fixing_cost_class(territorial_legitimacy__security_necessity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a working security order across the 1967 lines: a single controlling authority manages the borders, holds early-warning positions on commanding terrain, keeps the area west of the Jordan River demilitarized below the top tier of armament, and runs prevention and intelligence coordination — replacing a patchwork of hostile front lines with one administered zone.
% TRANSFER_FUNCTION: Moves land, water, movement freedom, and ultimate decision authority from Palestinian residents (and the displaced Syrian residents of the Golan) to Israeli state control and the communities beyond the green line; moves security risk away from Israeli population centers and onto Palestinian daily movement and residency.
% ABSENT_VOICES: Holders of the partition reading (full sovereign equality per UN Resolution 181) and the indigenous-continuity reading (right of return, Nakba framing) stand outside the framework this reading recognizes — their legitimacy claims are ruled out by its premises rather than answered within it. The displaced Golan Syrians have no seat in any body administering the Heights. Within the frame, the residents most affected by permit and quota rules sit on the paying side of committees they do not vote in.
% DISAPPEARANCE_RATIONALE: If the doctrine vanished overnight, the legitimacy architecture holding the arrangement together would collapse with it: the communities beyond the green line would lose their authorization frame, the demilitarization conditions would lose their warrant, the liaison and revenue-transfer mechanisms would unravel, and every party would renegotiate position from scratch — the standing order is built on and maintained by this reading's premises.
% FOUNDING_PROBLEM: After June 1967, the state held the West Bank, Gaza, Sinai, and the Golan acquired in a war it experienced as existential; the pre-war lines left the coastal center nine miles wide at the waist, and the founding problem was converting battlefield gains into durable defensive depth pending a peace that would trade land for recognition and secure boundaries.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: Egypt and Jordan attested the security problem was real enough to trade full treaties and recognition for land (1979, 1994), and UNSC Resolution 242's 'secure and recognized boundaries' language presupposed it; independent military historians document the 1967 threat environment. Against continued liveness: the ICJ's 2024 advisory opinion and successive UN resolutions treat the ongoing control as unlawful irrespective of threat claims, and the October 2023 attack breached the depth-and-barrier architecture entirely — corroborating that threats persist while undercutting the doctrine's efficacy claim. No single outside source settles the dispute, which is why the status is contested rather than live or dead.
narrative_ontology:disappearance_verdict(territorial_legitimacy__security_necessity_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy__security_necessity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__security_necessity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(territorial_legitimacy__security_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy__security_necessity_reading, 0.46, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy__security_necessity_reading_tests).
:- end_tests(territorial_legitimacy__security_necessity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.46 from this reading's own lights: the reading sincerely acknowledges that land, water, movement freedom, and ultimate decision authority move from Palestinian residents to Israeli control, but attributes the transfer to necessity rather than rent — hence well above a coordination-cost floor yet far below what a rejectionist reading of the same arrangement would measure. Suppression is 0.82 as a raw structural property: the regime runs on military government, checkpoints, a permit system, administrative detention, and military courts, and it is unscaled by power or scope in the engine's arithmetic — only extractiveness is scaled. Theater ratio is 0.33: the defensive functions are real (early-warning positions, the barrier system, intelligence coordination), but a growing share of activity — hilltop siting, bypass-road networks, construction approvals — exceeds any defensible operational need and is maintained under security language. Accessibility collapse is 0.60: for anyone who grants the security premise, the alternatives (armistice-line withdrawal, unrestricted sovereignty) collapse almost completely, while internationally the alternatives persist and are actively litigated. Resistance is 0.72: two intifadas, sustained litigation, boycott campaigns, recurring UN action, and the 2024 ICJ advisory proceedings. The temporal series run on one shared eight-point grid (years 0-58 of the interval, i.e., 1967-2025). Extractiveness climbs monotonically as facts on the ground accumulate faster than any negotiated reversal. Theater climbs in step as the settlement enterprise outgrows its stated rationale. Suppression is the oscillating series: it ratchets through the first intifada (t16), dips during the Oslo interim (t24) when enforcement relaxed, then hardens through the second intifada and the barrier regime (t32) and stays elevated — an enforcement-intensification trajectory with one negotiated relaxation, not an intermittent-reinforcement cycle; the dip is a side effect of a diplomatic phase, not a designed oscillation.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the agenda-setter seat (security establishment) the arrangement is a defense posture it operates and defines — the coordination function is not a story it tells but a job it does daily. From the settler beneficiary seat it is a subsidy stream and a home. From the resident payer seats the same structure is a permit regime that governs whether a farmer reaches his grove or a patient reaches a hospital. The incorporated intermediary seat (Palestinian Authority) experiences a hybrid: it pays sovereignty and legitimacy at home while collecting revenue flows and governing prerogatives that exist only inside the arrangement. The observer seats see contested legality without bearing any of the costs. Same-level divergence is sharpest between the two Palestinian seats: the Authority (moderate, trapped, dual-roled) has its effective burden dampened by partial incorporation into the arrangement's administration, while the residents (powerless, trapped) carry the full weight with no seat in the committees that set the rules.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive the derivation: the security establishment and settler councils sit near the beneficiary pole (low d), the residents and displaced Golan Syrians near the target pole (high d), and the observers are analytical. Two overrides are declared where the derivation would misread a dual-positioned agent. First, israeli_civilian_population holds the organized power atom; derivation from its beneficiary declaration would place it near the beneficiary pole, but it directly absorbs conscription, casualty risk during every escalation cycle, and the fiscal cost of administration — a mixed position better modeled at d=0.35 than at a near-subsidized value. Second, palestinian_authority holds the moderate atom; derivation from its payer position would place it near the full-target pole, but it collects conditional tax transfers, governing prerogatives, and external patronage that exist only inside the arrangement — d=0.60 models the dampened, dual-positioned reality. No other overrides are needed: the derivation handles the clean poles correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — converting 1967 battlefield gains into durable defensive depth pending a land-for-peace settlement — has been partially overtaken by events: peace treaties with Egypt (1979) and Jordan (1994) eliminated two conventional fronts, and precision-strike warfare arguably devalues shallow-depth arguments, yet the arrangement persists and expands. Classifying the structure as a tangled_rope rather than a snare prevents mislabeling in both directions: the security coordination function is real and independently attested (treaty partners traded recognition for land precisely because the threat was credible), so a pure-extraction reading would erase the parts of the structure that genuinely protect; conversely, a pure-coordination reading would erase the documented asymmetric transfers the reading itself concedes. Because the founding-problem status is contested rather than dead, the mandatrophy question stays open: the mismatch consumer will find status=contested paired with verdict=world_rearranges, which flags neither capture nor obsolescence automatically — the depth-vs-dome omega is the designated path for resolving it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Is defensive necessity the governing source of territorial legitimacy for this territory, or do the partition and indigenous-continuity sources govern instead?',
    'Comparative classification across the three sibling stories of the territorial_legitimacy kernel: whichever reading''s premises the relevant publics, courts, and treaty bodies ultimately ratify determines which epsilon and victim set apply to the same ground.',
    'Under the indigenous-continuity reading the identical control arrangement measures with far higher epsilon and a snare-leaning profile; under the partition reading the victim set widens to include everyone denied recognition-based entitlements after 1967. This story''s tangled_rope verdict holds only within the security-necessity frame.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: this constraint is one of three readings of the territorial_legitimacy kernel; sibling readings instantiate different constraints.').

omega_variable(
    threat_predicate_liveness,
    'Is the empirical threat predicate that grounds defensive necessity still live at the intensity the doctrine assumes?',
    'Independent threat assessment: attack frequencies crossing the 1967 lines, missile and UAV range profiles versus the marginal defensive value of depth, and the October 2023 breach of the depth-and-barrier architecture.',
    'If the predicate weakens, the justification layer thins and the measured costs lose their necessity cover, pushing the computed classification toward snare; if it strengthens, the tangled_rope reading stabilizes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threat_predicate_liveness, empirical, 'Whether the security threats invoked by the reading remain at the magnitude its premises require.').

omega_variable(
    settlement_security_separability,
    'Are the communities beyond the 1967 lines structurally load-bearing for defense (depth, early warning, observation of approach routes), or separable from the defensive function they are authorized by?',
    'Natural experiments: the Sinai evacuation of 1982 and the Gaza disengagement of 2005 — did defensive capability measurably degrade relative to matched baselines after withdrawal?',
    'If separable, the settlement component is a transfer riding on a genuine security core and the theater ratio understates performative activity; if load-bearing, part of the measured cost transfer is the price of the coordination itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settlement_security_separability, empirical, 'Whether the settlement enterprise is functionally coupled to the defensive rationale.').

omega_variable(
    depth_vs_dome_relevance,
    'Does territorial strategic depth retain defensive value in a precision-missile and UAV environment, or has the doctrine''s core military premise been technologically superseded by interception-based defense?',
    'Operational research comparing depth-based and interception-based defense outcomes in recent conflicts with comparable threat profiles.',
    'If depth is superseded, the founding problem is effectively dead and the arrangement persists by inertia — piton dynamics emerge within this reading''s own frame; if depth retains value, the mandate remains live.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(depth_vs_dome_relevance, empirical, 'Technological obsolescence question for the strategic-depth premise.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__security_necessity_reading, 0, 58).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t0, territorial_legitimacy__security_necessity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(terr_tr_t0, observed).
narrative_ontology:measurement(terr_tr_t8, territorial_legitimacy__security_necessity_reading, theater_ratio, 8, 0.14).
narrative_ontology:measurement_basis(terr_tr_t8, observed).
narrative_ontology:measurement(terr_tr_t16, territorial_legitimacy__security_necessity_reading, theater_ratio, 16, 0.18).
narrative_ontology:measurement_basis(terr_tr_t16, observed).
narrative_ontology:measurement(terr_tr_t24, territorial_legitimacy__security_necessity_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement_basis(terr_tr_t24, observed).
narrative_ontology:measurement(terr_tr_t32, territorial_legitimacy__security_necessity_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement_basis(terr_tr_t32, observed).
narrative_ontology:measurement(terr_tr_t40, territorial_legitimacy__security_necessity_reading, theater_ratio, 40, 0.29).
narrative_ontology:measurement_basis(terr_tr_t40, observed).
narrative_ontology:measurement(terr_tr_t48, territorial_legitimacy__security_necessity_reading, theater_ratio, 48, 0.31).
narrative_ontology:measurement_basis(terr_tr_t48, observed).
narrative_ontology:measurement(terr_tr_t58, territorial_legitimacy__security_necessity_reading, theater_ratio, 58, 0.33).
narrative_ontology:measurement_basis(terr_tr_t58, observed).

% Extraction over time
narrative_ontology:measurement(terr_be_t0, territorial_legitimacy__security_necessity_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(terr_be_t0, observed).
narrative_ontology:measurement(terr_be_t8, territorial_legitimacy__security_necessity_reading, base_extractiveness, 8, 0.31).
narrative_ontology:measurement_basis(terr_be_t8, observed).
narrative_ontology:measurement(terr_be_t16, territorial_legitimacy__security_necessity_reading, base_extractiveness, 16, 0.34).
narrative_ontology:measurement_basis(terr_be_t16, observed).
narrative_ontology:measurement(terr_be_t24, territorial_legitimacy__security_necessity_reading, base_extractiveness, 24, 0.37).
narrative_ontology:measurement_basis(terr_be_t24, observed).
narrative_ontology:measurement(terr_be_t32, territorial_legitimacy__security_necessity_reading, base_extractiveness, 32, 0.4).
narrative_ontology:measurement_basis(terr_be_t32, observed).
narrative_ontology:measurement(terr_be_t40, territorial_legitimacy__security_necessity_reading, base_extractiveness, 40, 0.42).
narrative_ontology:measurement_basis(terr_be_t40, observed).
narrative_ontology:measurement(terr_be_t48, territorial_legitimacy__security_necessity_reading, base_extractiveness, 48, 0.44).
narrative_ontology:measurement_basis(terr_be_t48, observed).
narrative_ontology:measurement(terr_be_t58, territorial_legitimacy__security_necessity_reading, base_extractiveness, 58, 0.46).
narrative_ontology:measurement_basis(terr_be_t58, observed).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t0, territorial_legitimacy__security_necessity_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(terr_su_t0, observed).
narrative_ontology:measurement(terr_su_t8, territorial_legitimacy__security_necessity_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement_basis(terr_su_t8, observed).
narrative_ontology:measurement(terr_su_t16, territorial_legitimacy__security_necessity_reading, suppression_requirement, 16, 0.7).
narrative_ontology:measurement_basis(terr_su_t16, observed).
narrative_ontology:measurement(terr_su_t24, territorial_legitimacy__security_necessity_reading, suppression_requirement, 24, 0.66).
narrative_ontology:measurement_basis(terr_su_t24, observed).
narrative_ontology:measurement(terr_su_t32, territorial_legitimacy__security_necessity_reading, suppression_requirement, 32, 0.8).
narrative_ontology:measurement_basis(terr_su_t32, observed).
narrative_ontology:measurement(terr_su_t40, territorial_legitimacy__security_necessity_reading, suppression_requirement, 40, 0.78).
narrative_ontology:measurement_basis(terr_su_t40, observed).
narrative_ontology:measurement(terr_su_t48, territorial_legitimacy__security_necessity_reading, suppression_requirement, 48, 0.79).
narrative_ontology:measurement_basis(terr_su_t48, observed).
narrative_ontology:measurement(terr_su_t58, territorial_legitimacy__security_necessity_reading, suppression_requirement, 58, 0.82).
narrative_ontology:measurement_basis(terr_su_t58, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy__security_necessity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(territorial_legitimacy__security_necessity_reading, territorial_legitimacy__partition_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__security_necessity_reading, territorial_legitimacy__indigenous_continuity_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'territorial legitimacy in Israel/Palestine' decomposes into three structurally distinct constraints per the epsilon-invariance principle: the partition reading (epsilon anchored to recognition-based title and its denial), the indigenous-continuity reading (epsilon anchored to habitation disruption and displacement), and this security-necessity reading (epsilon anchored to the standing control arrangement as measured by defensive-necessity lights). Each has a distinct victim set and failure mode; measuring one with another's observable produces a different constraint, not a different view of the same one. The upstream/downstream structure runs from this reading outward: its facts-on-ground operation changes the demographic and resource conditions under which the indigenous-continuity claim could be implemented, which is why the influences edge points in that direction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_legitimacy__security_necessity_reading, organized, 0.35).
constraint_indexing:directionality_override(territorial_legitimacy__security_necessity_reading, moderate, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
