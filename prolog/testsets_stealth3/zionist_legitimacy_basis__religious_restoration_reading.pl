% ============================================================================
% CONSTRAINT STORY: zionist_legitimacy_basis__religious_restoration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: zionist_legitimacy_basis__religious_restoration_reading
 *   human_readable: Divine-Promise Legitimation of Territorial Maximalism (Religious Zionist Reading, post-1967)
 *   domain: political_history/nationalism/religion_and_state
 *
 * SUMMARY:
 *   After 1967, the religious Zionist stream descending from Rav Abraham
 *   Isaac Kook through his son Zvi Yehuda and the Mercaz Harav yeshiva
 *   reinterpreted the state and the conquered territories as stages of divine
 *   redemption (atchalta d'geulah). This doctrine functions as a legitimacy
 *   basis that converts territorial maximalism from a policy preference into
 *   a covenantal obligation, mobilizing the settlement enterprise,
 *   constraining secular governments, and structuring the lives of everyone
 *   under the arrangement it sustains. IMPORTANT — kernel discipline: this
 *   file instantiates ONE reading (religious_restoration_reading) of the
 *   kernel zionist_legitimacy_basis. The sibling readings
 *   (national_liberation_reading, settler_colonial_reading) are separate
 *   constraints in separate files with their own epsilon values; they are not
 *   described, hedged, or averaged here. Epsilon's referent is the standing
 *   arrangement under contest — the post-1967 settlement and
 *   territorial-control regime — assessed BY THIS READING'S OWN LIGHTS
 *   (OQ-26): from inside the frame the arrangement is covenantal fulfillment,
 *   not taking; the reading acknowledges real costs (Palestinian hardship,
 *   soldier burden, international isolation) but subordinates them
 *   theologically to the redemptive process, yielding a moderate-low epsilon
 *   rather than the high value the settler_colonial_reading would author over
 *   the identical referent.
 *
 * KEY AGENTS:
 *   - west_bank_palestinians: primary target (powerless/trapped) — bears the arrangement's material costs under military administration
 *   - east_jerusalem_palestinians: primary target (powerless/trapped) — residency precarity and demolition exposure
 *   - religious_zionist_settler_community: primary beneficiary (organized/identity_locked) — receives land, subsidies, protection, and meaning; bears casualty and evacuation costs
 *   - national_religious_rabbinic_establishment: agenda setter (institutional/identity_locked) — articulates the mandate and enforces it through rulings and the educational pipeline
 *   - hilltop_youth_vanguard: radicalized beneficiary edge (moderate/immediate) — enforces maximalism ahead of and sometimes against the state
 *   - secular_israeli_political_leadership: dual-positioned payer/beneficiary (institutional/constrained) — loses policy autonomy while drawing symbolic benefit
 *   - israeli_peace_camp: excluded voice (organized/constrained) — favors compromise, progressively marginalized
 *   - palestinian_refugee_diaspora: excluded voice (powerless/trapped) — strongest historical claim, no seat in any tolerated forum
 *   - israeli_defense_establishment: administering agenda setter (institutional/constrained) — runs the military government the doctrine legitimates
 *   - international_legal_bodies: analytical observer (institutional/analytical) — adjudicates legality without enforcement purchase
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis__religious_restoration_reading, 0.38).
domain_priors:suppression_score(zionist_legitimacy_basis__religious_restoration_reading, 0.7).
domain_priors:theater_ratio(zionist_legitimacy_basis__religious_restoration_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, accessibility_collapse, 0.76).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__religious_restoration_reading, tangled_rope).
narrative_ontology:human_readable(zionist_legitimacy_basis__religious_restoration_reading, "Divine-Promise Legitimation of Territorial Maximalism (Religious Zionist Reading, post-1967)").
narrative_ontology:topic_domain(zionist_legitimacy_basis__religious_restoration_reading, "political_history/nationalism/religion_and_state").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__religious_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__religious_restoration_reading, '2303e328-a530-415b-a2ce-a590363c030e').
narrative_ontology:cs_kernel_codification('2303e328-a530-415b-a2ce-a590363c030e', fixed_text).
narrative_ontology:cs_authority_grounding('2303e328-a530-415b-a2ce-a590363c030e', lineage).
narrative_ontology:cs_interpretation_layer_present('2303e328-a530-415b-a2ce-a590363c030e').
narrative_ontology:cs_reading_relation('2303e328-a530-415b-a2ce-a590363c030e', zionist_legitimacy_basis__national_liberation_reading, coexists_with).
narrative_ontology:cs_reading_relation('2303e328-a530-415b-a2ce-a590363c030e', zionist_legitimacy_basis__settler_colonial_reading, forecloses).
narrative_ontology:cs_axiom('2303e328-a530-415b-a2ce-a590363c030e', foundational, land_grant_irrevocable_divine_covenant).
narrative_ontology:cs_axiom_status(land_grant_irrevocable_divine_covenant, holdable).
narrative_ontology:cs_axiom_grounding('2303e328-a530-415b-a2ce-a590363c030e', land_grant_irrevocable_divine_covenant, theological).
narrative_ontology:cs_axiom('2303e328-a530-415b-a2ce-a590363c030e', secondary, statehood_as_messianic_vehicle).
narrative_ontology:cs_axiom_status(statehood_as_messianic_vehicle, holdable).
narrative_ontology:cs_axiom_grounding('2303e328-a530-415b-a2ce-a590363c030e', statehood_as_messianic_vehicle, theological).
narrative_ontology:cs_reference_frame('2303e328-a530-415b-a2ce-a590363c030e', divine_land_grant_messianic_process).
narrative_ontology:cs_drift_state('2303e328-a530-415b-a2ce-a590363c030e', contemporary_post_2023_war, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('2303e328-a530-415b-a2ce-a590363c030e', '').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__religious_restoration_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__religious_restoration_reading, religious_zionist_settler_community).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__religious_restoration_reading, national_religious_rabbinic_establishment).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, west_bank_palestinians).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, east_jerusalem_palestinians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__religious_restoration_reading, secular_israeli_political_leadership).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__religious_restoration_reading, hilltop_youth_vanguard).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, religious_zionist_settler_community).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, secular_israeli_political_leadership).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Lives in communities across Judea, Samaria, and East Jerusalem under the conviction that settling the land fulfills a biblical covenant. Receives state support — housing subsidies, infrastructure, army protection — and supplies disproportionate shares of combat officers and national-religious leadership. Leaving would mean abandoning what members understand as a divine assignment; most cannot imagine themselves elsewhere even as prices, danger, and international criticism rise. They also carry real burdens: casualty risk in elite units, evacuation trauma (Gaza 2005), and pariah status abroad.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, religious_zionist_settler_community, beneficiary,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__religious_restoration_reading, religious_zionist_settler_community, payer).

% Yeshiva heads and senior rabbis in the Mercaz Harav lineage teach that the state's victories are stages of redemption and that ceding land violates divine will. Issues rulings against territorial compromise, runs the educational pipeline (hesder yeshivot, ulpanot) that staffs the settlement movement, and confers or withholds religious legitimacy on political leaders. Their authority rests on the doctrine; revising it would unravel their standing.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, national_religious_rabbinic_establishment, agenda_setter,
    institutional, generational, identity_locked, national).

% Several million people in Areas A, B, and C live under military administration, settlement expansion, checkpoint movement controls, and land expropriation for settlement roads and outposts. Most hold no vote in the state that governs the decisive aspects of their lives. Exit abroad is possible for a few with resources; the majority cannot regularize status, recover confiscated land, or move freely. Political representation is split between rival factions and geographically severed.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, west_bank_palestinians, payer,
    powerless, generational, trapped, regional).

% Holds Israeli permanent-resident cards rather than citizenship; residency can be revoked for prolonged absence or alleged disloyalty. Faces home demolitions, settlement encirclement by ring neighborhoods, and eviction proceedings in courts of a state whose sovereignty they did not consent to. Daily life — work, family, hospitals — ties them to the city; relocating means losing residency outright.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, east_jerusalem_palestinians, payer,
    powerless, generational, trapped, local).

% Runs the state's ministries and coalitions but governs alongside a bloc that treats the territories as non-negotiable. Proposing withdrawal triggers coalition collapse, rabbinic denunciation, and — at the lethal edge — violence, as the 1995 assassination of a sitting prime minister demonstrated. Many of the same leaders also draw electoral benefit from national-religious symbolism, so the actors who feel boxed in also compete to defend the box.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, secular_israeli_political_leadership, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__religious_restoration_reading, secular_israeli_political_leadership, beneficiary).

% Organizations and voters favoring territorial compromise and a negotiated two-state outcome. Formally inside the conversation — Knesset seats, media access — yet repeatedly outmaneuvered: coalition shifts after 1977, the collapse of trust in the Second Intifada, and legal harassment (anti-boycott statutes, NGO funding probes) narrowed their room. Some members emigrate or withdraw from politics; others persist against long odds.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, israeli_peace_camp, excluded,
    organized, biographical, constrained, national).

% Descendants of those displaced in 1948 and afterward, living in Lebanon, Jordan, Syria, the Gulf, Chile, and elsewhere under widely varying legal statuses. Holds the strongest historical grievance against the arrangement yet has no seat in any forum where the doctrine is debated; the right-of-return claim they advance is precisely what the settlement enterprise forecloses.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, palestinian_refugee_diaspora, excluded,
    powerless, generational, trapped, global).

% Teenagers and young adults who establish unauthorized outposts ahead of state approval, acting on a more radical reading of the same mandate. Conducts reprisal actions against Palestinian villages and occasionally against security forces. The state alternates between demolishing their outposts and retroactively legalizing them; they answer to rabbis and peers more than to ministries.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, hilltop_youth_vanguard, beneficiary,
    moderate, immediate, identity_locked, regional).

% Central Command administers the military government in the West Bank: permits, raids, coordination with the Palestinian Authority, protection of settlements. Officers implement policy whose contours are shaped by the covenantal bloc's political weight; serving officers privately describe the deployment as corrosive and publicly execute it. Individual rotation softens exposure; the institution persists.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, israeli_defense_establishment, agenda_setter,
    institutional, generational, constrained, national).

% UN organs, the International Court of Justice, and treaty monitors assess the settlement regime against the Geneva Conventions and Security Council resolutions, including the 2024 advisory opinion finding the settlement enterprise unlawful. Produces findings with limited enforcement purchase; the pronouncements feed the movement's persecution narrative as often as they constrain it.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, international_legal_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(zionist_legitimacy_basis__religious_restoration_reading, religious_zionist_settler_community).
narrative_ontology:fixing_cost_class(zionist_legitimacy_basis__religious_restoration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the national-religious community into the state project: resolves the centuries-old tension between diasporic quietism and sovereign power, channels religious youth into army service and civic participation, and provides a shared framework integrating Torah observance with national life. Stated without evaluation of how the framework treats outsiders.
% TRANSFER_FUNCTION: Moves land, jurisdiction, and political permanence toward Jewish sovereign control of the territories; moves state resources (housing subsidies, infrastructure, military protection) to the settlement enterprise; moves the costs of the arrangement — displacement, statelessness, restricted movement — onto Palestinians in the West Bank and East Jerusalem; and moves policy autonomy away from secular compromise politics.
% ABSENT_VOICES: West Bank and East Jerusalem Palestinians bear the heaviest costs of the arrangement yet sit outside the Israeli conversation in which the doctrine operates — physically present, politically voiceless in the forums that decide their lives. The refugee diaspora, holding the oldest claims, is absent from every negotiating frame the doctrine tolerates. Dissenting religious voices (anti-maximalist rabbis) are marginalized within the yeshiva hierarchy itself. They are located under the military administration the doctrine legitimates, or outside the frame entirely.
% DISAPPEARANCE_RATIONALE: If the divine-mandate framework vanished overnight, the settlement movement would lose its mobilizing core even where material interests persisted; territorial compromise would re-enter the feasible set of Israeli coalition politics; covenant-driven outpost expansion would halt; and the religious-nationalist bloc's veto over withdrawal would dissolve. The political geometry of the conflict — which options are thinkable, which coalitions hold — rearranges around the reopened space.
% FOUNDING_PROBLEM: The post-1967 crisis of meaning: what to do with unexpectedly conquered territories, answered by reframing them from a policy problem into a redemptive stage. The deeper founding problem is older — Rav Kook's project of reconciling traditional Judaism with modern sovereignty, which the victory of 1967 appeared to confirm and radicalize.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: Mizrachi's 1947 acceptance of partition attests that religious Zionism faced and answered the religion-and-state problem WITHOUT territorial maximalism, confirming the maximalist layer arrived later in response to a new situation; the 1967 Allon Plan debates among secular strategists attest the territorial question was live independently of the doctrine; and the ICJ's 2024 advisory opinion, Palestinian testimony, and Israeli constitutional scholarship all attest the arrangement the doctrine sustains remains contested and unresolved.
narrative_ontology:disappearance_verdict(zionist_legitimacy_basis__religious_restoration_reading, world_rearranges).
narrative_ontology:founding_problem_status(zionist_legitimacy_basis__religious_restoration_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__religious_restoration_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(zionist_legitimacy_basis__religious_restoration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zionist_legitimacy_basis__religious_restoration_reading, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zionist_legitimacy_basis__religious_restoration_reading_tests).
:- end_tests(zionist_legitimacy_basis__religious_restoration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.38 is reading-indexed over the fixed referent: the frame experiences the arrangement as commanded restoration, and the residual value tracks the costs the frame itself must acknowledge and manage rather than deny — the Palestinian presence treated as an obstacle to redemption, cumulative soldier casualties, diaspora strain, international condemnation. Suppression 0.70 is a RAW structural property, unscaled by power or scope: the machinery holding the arrangement against alternatives is heavy — military administration, legal suppression of dissent, rabbinic enforcement, and one lethal instance against an Israeli head of government. Theater 0.32: the functional core (settlement building, army service, coalition politics) dominates, but ceremonial messianism (jubilee cycles, march rituals, temple-mount activism) grows as a share of activity. Accessibility collapse 0.76: inside the frame, accepting the covenant premise collapses the alternative space nearly completely — compromise becomes not unwise but sacrilegious — while the frame's boundary keeps exits (secularization, dissent) technically open at severe identity cost, keeping the value below the mountain range. Resistance 0.58: sustained Palestinian resistance, an Israeli opposition current, and international legal pressure meet the arrangement continuously. Temporal grid: ONE shared grid, all three metrics authored at all eight points (1967, 1974, 1977, 1987, 1995, 2005, 2017, 2025). Suppression_requirement is tracked deliberately because enforcement capacity demonstrably hardened across the interval (Oslo-era through the war-era) — not a static enforcement picture. The 2005 dip in both epsilon and suppression traces the disengagement shock: the arrangement contracted, enforcement proved it could suppress settler resistance, and the visible cost of doing so registered inside the frame before entrenchment resumed.
 *
 * PERSPECTIVAL GAP:
 *   Per-seat classifications diverge sharply and the engine computes them from the structural data: from the settler seat the arrangement is covenantal duty plus state subsidy — coordination flavored; from the Palestinian seats the same structure is dispossession without rights — extraction flavored; from the rabbinate's seat it is sacred obligation; from the secular leadership's seat it is a foreclosed policy space it simultaneously profits from symbolically; from the observer seat it is unlawful. Five seats, one structure, five computations. The authored claimed_type does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (settler community, rabbinic establishment) derive low directionality — the arrangement subsidizes them with land, money, protection, and meaning; identity_locked exit stabilizes the settler seat deep on the beneficiary side. Victim declarations (West Bank and East Jerusalem Palestinians) with trapped exit derive high directionality — full-target treatment amplified by the difficulty of verification at regional scope. The secular leadership's payer role with partial symbolic benefit yields an elevated but moderated d. No directionality_overrides are authored: the derivation chain's role-plus-exit inputs carry the needed differentiation, and the override mechanism is keyed by power atom, which cannot distinguish the four institutional-power agents here (rabbinate, defense establishment, secular leadership, international bodies) whose directionalities differ — a power-atom override would misfire across all four simultaneously.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim guards against mislabeling in both directions. Calling this pure coordination (rope) would erase the victim structure — millions living under the arrangement without rights — that the same structure produces and requires. Calling it pure extraction (snare) would erase the demonstrated coordination function: Mizrachi's 1947 partition acceptance shows religious Zionism integrated religion and state, fielded soldiers, and sustained community WITHOUT territorial maximalism, so the maximalist layer is separable in principle (see omega coordination_extraction_separability). It is not a piton: the function is live, not vestigial, and the theater ratio reflects growth of ceremony around a working core, not replacement of function by performance. It is emphatically not a scaffold: there is no sunset clause and the mandate is explicitly eternal — this is the anti-scaffold, a frame that declares its own permanence. Founding problem status is live; mandatrophy is not resolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of kernel zionist_legitimacy_basis (reading: religious_restoration_reading). What structurally changes under the sibling readings, and where exactly is the disagreement located?',
    'Generate and compare national_liberation_reading and settler_colonial_reading over the same referent (the post-1967 territorial arrangement); diff epsilon, victim sets, and enforcement structure across the three files.',
    'Under the settler_colonial_reading, epsilon rises sharply and the victim set expands backward to the 1948-displaced populations; under the national_liberation_reading, epsilon falls toward justified-return levels and the victim set narrows to post-1967 occupation effects. The disagreement is located in the source of legitimate title: divine covenant (this reading), national self-determination (NL), or colonial usurpation (SC).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure routed here per Rule 2: this file instantiates one reading of a three-reading kernel; sibling readings are separate constraints, not parameters of this one.').

omega_variable(
    coordination_extraction_separability,
    'Is the community-coordination function (religion-state integration, meaning provision, channeling religious youth into army service and civic participation) separable from the territorial-maximalist function?',
    'Historical natural experiment: Mizrachi''s 1947 acceptance of partition and pre-1967 religious Zionism operated the coordination function without territorial maximalism; contemporary non-territorial religious-Zionist communities (diaspora branches, post-disengagement evacuees who retained faith and cohesion after relocating) test whether the community survives without the land mandate.',
    'If separable, the maximalist layer is removable without collapsing the community — the burden on Palestinians rides on captured coordination; if fused, part of the measured cost is the price of the coordination itself and removal would fracture the community.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, empirical, 'Whether the doctrine''s genuine coordination component can be pried apart from its maximalist component.').

omega_variable(
    divine_grant_adjudicability,
    'Can the divine land-grant premise be adjudicated at all, or does it sit outside the framework''s contestable surface as an uncontestable floor?',
    'Internal-theological only: whether rabbinic tradition itself contains conditional-or-revocable covenant strands that the current reading marginalizes; externally, revelation claims do not yield to empirical test.',
    'If conditional-grant strands are recovered from within the tradition, the maximalist mandate loses its absolutist form without external pressure; if not, the premise functions as a fixed floor beneath every other classification move in this story.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(divine_grant_adjudicability, conceptual, 'Whether the foundational premise admits internal revision or is structurally insulated from contest.').

omega_variable(
    settler_identity_lock_composition,
    'Is the settler community''s inability to exit internalized (identity fusion with the divine mission) or structural (economic dependence on state housing subsidies, school networks, municipal employment)?',
    'Post-prop-removal cohort tracking: behavior of communities that lost the structural supports (Gaza 2005 evacuees) — did identity and cohesion persist after subsidies and homes were gone?',
    'If internalized, the bind travels with the agent past any structural reform and the arrangement outlives its enforcement machinery; if structural, redirecting subsidies decays the arrangement without confrontation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settler_identity_lock_composition, empirical, 'Structural versus internalized composition of the identity-lock on the beneficiary seat.').

omega_variable(
    enforcement_ceiling_or_break,
    'Does the rising enforcement trajectory consolidate (annexation normalizes the covenantal mandate into statute) or break (internal schism, external sanctions, coalition realignment)?',
    'Track statutory annexation steps, court independence, settler-party coalition weight, and international sanction regimes over the next decade.',
    'Consolidation pushes the arrangement toward entrenched asymmetry with the doctrine as constitutional floor; break reopens the alternative space the doctrine currently forecloses and would date a type transition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_ceiling_or_break, empirical, 'Future trajectory of the enforcement machinery the doctrine depends on.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis__religious_restoration_reading, 1967, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zlb_religious_restoration_tr_t1967, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 1967, 0.12).
narrative_ontology:measurement(zlb_religious_restoration_tr_t1974, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 1974, 0.15).
narrative_ontology:measurement(zlb_religious_restoration_tr_t1977, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 1977, 0.18).
narrative_ontology:measurement(zlb_religious_restoration_tr_t1987, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 1987, 0.22).
narrative_ontology:measurement(zlb_religious_restoration_tr_t1995, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 1995, 0.25).
narrative_ontology:measurement(zlb_religious_restoration_tr_t2005, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 2005, 0.24).
narrative_ontology:measurement(zlb_religious_restoration_tr_t2017, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 2017, 0.28).
narrative_ontology:measurement(zlb_religious_restoration_tr_t2025, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 2025, 0.32).

% Extraction over time
narrative_ontology:measurement(zlb_religious_restoration_be_t1967, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 1967, 0.24).
narrative_ontology:measurement(zlb_religious_restoration_be_t1974, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 1974, 0.27).
narrative_ontology:measurement(zlb_religious_restoration_be_t1977, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 1977, 0.31).
narrative_ontology:measurement(zlb_religious_restoration_be_t1987, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 1987, 0.33).
narrative_ontology:measurement(zlb_religious_restoration_be_t1995, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 1995, 0.36).
narrative_ontology:measurement(zlb_religious_restoration_be_t2005, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 2005, 0.35).
narrative_ontology:measurement(zlb_religious_restoration_be_t2017, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 2017, 0.37).
narrative_ontology:measurement(zlb_religious_restoration_be_t2025, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 2025, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(zlb_religious_restoration_su_t1967, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 1967, 0.3).
narrative_ontology:measurement(zlb_religious_restoration_su_t1974, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 1974, 0.38).
narrative_ontology:measurement(zlb_religious_restoration_su_t1977, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 1977, 0.45).
narrative_ontology:measurement(zlb_religious_restoration_su_t1987, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 1987, 0.52).
narrative_ontology:measurement(zlb_religious_restoration_su_t1995, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 1995, 0.62).
narrative_ontology:measurement(zlb_religious_restoration_su_t2005, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 2005, 0.58).
narrative_ontology:measurement(zlb_religious_restoration_su_t2017, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 2017, 0.66).
narrative_ontology:measurement(zlb_religious_restoration_su_t2025, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 2025, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis__religious_restoration_reading, identity_coordination).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, national_liberation_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, settler_colonial_reading).

% DUAL FORMULATION NOTE:
% Kernel decomposition: 'Zionism's legitimacy basis' is one colloquial label covering three structurally distinct constraints over the same referent (the post-1967 territorial arrangement). Each reading carries its own epsilon, victim set, and enforcement structure; they are linked here rather than merged because averaging epsilon across readings would violate epsilon-invariance. Genealogy: national_liberation_reading is upstream (the pre-1967 consensus frame) and supplied the statehood legitimacy this reading sacralizes; settler_colonial_reading arose as external critique. This file links both siblings per the constraint-family rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
