% ============================================================================
% CONSTRAINT STORY: zionist_legitimacy_basis__religious_restoration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   human_readable: Religious-Restoration Reading: Settlement as Divine Command and Messianic Process (Post-1967)
 *   domain: political_history/nationalism/religion_and_territory
 *
 * SUMMARY:
 *   After 1967, the religious Zionist camp converted a military conquest into
 *   a covenantal fact: the recovered territories were read as the visible
 *   edge of a divine promise, settlement as the performance of a commandment,
 *   and any surrender of territory as sacrilege that no elected government
 *   may authorize. This story instantiates THAT READING ONLY — one of three
 *   sibling constraints decomposed from the kernel zionist_legitimacy_basis
 *   (what grounds the Zionist project's claim). The standing arrangement
 *   under contest is the same for all siblings: the post-1967
 *   occupation-and-settlement regime with its rabbinic warrant, state
 *   enforcement, and Palestinian cost structure. The readings differ in
 *   epsilon because they differ in what they count the arrangement to be.
 *   This reading authors epsilon at 0.72: the structural transfers (land,
 *   jurisdiction, movement freedom, legal equality) are massive and ongoing,
 *   though the reading's internal theology discounts their moral weight (see
 *   omega theological_extraction_discount). The national_liberation sibling
 *   authors a materially lower epsilon over the identical referent — it
 *   registers the same transfers as the tragic price of emancipation for a
 *   persecuted people — while the settler_colonial sibling authors a higher
 *   one, treating the transfer as illegitimate from inception. Same referent,
 *   three constraints, three epsilons; cross-sibling comparison is the
 *   intended use of the family, and the network links below exist so
 *   contamination and drift propagate across it.
 *
 * KEY AGENTS:
 *   - religious_zionist_rabbinic_authority: agenda-setting interpreter (institutional/identity_locked) — issues the rulings that define the mandate; its standing is inseparable from the enterprise continuing
 *   - ideological_settler_core: primary beneficiary (organized/identity_locked) — receives land, subsidy, and protection; mission-fused; exit means self-betrayal
 *   - quality_of_life_settlers: secondary beneficiary (moderate/constrained) — transactional participation; demonstrated exit precedent from 2005
 *   - west_bank_palestinian_residents: primary target (powerless/trapped) — bears land requisition, legal duality, and movement restriction under a government they cannot vote in or out
 *   - east_jerusalem_palestinian_residents: target (powerless/trapped) — revocable residency under annexation with demolition exposure
 *   - israeli_security_establishment: enforcer-administrator (institutional/constrained) — dual-positioned: runs the occupation, gains strategic assets, absorbs its political and legal costs
 *   - secular_israeli_public: secondary target (organized/constrained) — pays in taxes and conscription; its electoral preferences for compromise are structurally overridden
 *   - palestinian_authority_officialdom: excluded interlocutor (moderate/constrained) — negotiates with a counterpart whose legitimacy rests on refusing what it offers
 *   - haredi_nonzionist_leadership: excluded intra-traditional objector (organized/identity_locked) — denies the theological premise from inside Judaism itself
 *   - diaspora_liberal_jewish_institutions: excluded external critic (organized/mobile) — contests the enterprise while funding the state; can disengage institutionally
 *   - international_legal_institutions: analytical observer (institutional/analytical) — accumulates adverse findings without enforcement capacity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis__religious_restoration_reading, 0.72).
domain_priors:suppression_score(zionist_legitimacy_basis__religious_restoration_reading, 0.8).
domain_priors:theater_ratio(zionist_legitimacy_basis__religious_restoration_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__religious_restoration_reading, tangled_rope).
narrative_ontology:human_readable(zionist_legitimacy_basis__religious_restoration_reading, "Religious-Restoration Reading: Settlement as Divine Command and Messianic Process (Post-1967)").
narrative_ontology:topic_domain(zionist_legitimacy_basis__religious_restoration_reading, "political_history/nationalism/religion_and_territory").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__religious_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__religious_restoration_reading, 'd748c095-d26f-41f5-9410-00cd1d550249').
narrative_ontology:cs_kernel_codification('d748c095-d26f-41f5-9410-00cd1d550249', fixed_text).
narrative_ontology:cs_authority_grounding('d748c095-d26f-41f5-9410-00cd1d550249', lineage).
narrative_ontology:cs_interpretation_layer_present('d748c095-d26f-41f5-9410-00cd1d550249').
narrative_ontology:cs_reading_relation('d748c095-d26f-41f5-9410-00cd1d550249', zionist_legitimacy_basis__national_liberation_reading, coexists_with).
narrative_ontology:cs_reading_relation('d748c095-d26f-41f5-9410-00cd1d550249', zionist_legitimacy_basis__settler_colonial_reading, forecloses).
narrative_ontology:cs_axiom('d748c095-d26f-41f5-9410-00cd1d550249', foundational, eternal_divine_grant_of_land_of_israel).
narrative_ontology:cs_axiom_status(eternal_divine_grant_of_land_of_israel, holdable).
narrative_ontology:cs_axiom_grounding('d748c095-d26f-41f5-9410-00cd1d550249', eternal_divine_grant_of_land_of_israel, theological).
narrative_ontology:cs_axiom('d748c095-d26f-41f5-9410-00cd1d550249', foundational, settlement_commandment_binding_over_state_policy).
narrative_ontology:cs_axiom_status(settlement_commandment_binding_over_state_policy, holdable).
narrative_ontology:cs_axiom_grounding('d748c095-d26f-41f5-9410-00cd1d550249', settlement_commandment_binding_over_state_policy, theological).
narrative_ontology:cs_axiom('d748c095-d26f-41f5-9410-00cd1d550249', secondary, state_as_redemptive_instrument).
narrative_ontology:cs_axiom_status(state_as_redemptive_instrument, holdable).
narrative_ontology:cs_axiom_grounding('d748c095-d26f-41f5-9410-00cd1d550249', state_as_redemptive_instrument, theological).
narrative_ontology:cs_reference_frame('d748c095-d26f-41f5-9410-00cd1d550249', covenantal_full_restoration_frame).
narrative_ontology:cs_drift_state('d748c095-d26f-41f5-9410-00cd1d550249', post_oslo_post_disengagement, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d748c095-d26f-41f5-9410-00cd1d550249', '').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__religious_restoration_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__religious_restoration_reading, religious_zionist_rabbinic_authority).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__religious_restoration_reading, ideological_settler_core).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__religious_restoration_reading, quality_of_life_settlers).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, west_bank_palestinian_residents).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, east_jerusalem_palestinian_residents).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, secular_israeli_public).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__religious_restoration_reading, israeli_security_establishment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Yeshiva heads and settlement rabbis in the Mercaz HaRav lineage issue rulings that dwelling in the land is a positive commandment and that handing territory to non-Jewish sovereignty violates halakha. They certify which political positions are open to observant soldiers and officials, train much of the national-religious officer and teaching corps through their school networks, and publish fresh rulings around each negotiation round. Their schools, stipends, and standing flow through institutions built on the settlement project continuing.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, religious_zionist_rabbinic_authority, agenda_setter,
    institutional, civilizational, identity_locked, national).

% Families in the deeper hilltop communities and established West Bank settlements who moved primarily for reasons of faith. They received land allocated by state institutions, subsidized housing, and army protection, and their children serve in combat units at rates well above the national average. Their self-understanding is bound up with holding the land; moving back inside the old line would mean abandoning what they hold to be commanded, and their social world — schools, synagogues, workplaces, marriage networks — sits entirely inside the settlement web.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, ideological_settler_core, beneficiary,
    organized, generational, identity_locked, regional).

% The majority of settler households, drawn by cheaper and larger housing within commuting distance of Jerusalem and Tel Aviv rather than by theology. They accept the security burden and periodic violence as the price of affordability. If subsidies ended or commuting costs rose sharply, most could relocate back across the line, as Gaza evacuees did in 2005, though at real financial and schooling disruption.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, quality_of_life_settlers, beneficiary,
    moderate, immediate, constrained, regional).

% Around three million people living under Israeli military administration in Area C and under mixed arrangements elsewhere. Farmland and hilltops have been requisitioned or closed for settlement use and barrier routing; travel between towns runs through checkpoints and permit regimes; residents are tried under military law while nearby settlers answer to civilian courts. They vote in Palestinian elections that govern fragments of daily life but not in the state that controls their borders, water quotas, and planning approval. Permanent departure usually means forfeiting residency and property.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, west_bank_palestinian_residents, payer,
    powerless, generational, trapped, local).

% Several hundred thousand people in the annexed eastern part of the city holding revocable permanent-resident status rather than citizenship. Building permits are scarce, so families extend homes without them and carry demolition orders; a ring of government-backed settlements encircles and bisects their neighborhoods; residency lapses after extended absence or failed center-of-life tests. Citizenship is available in principle but few take it, because accepting it reads as consenting to annexation.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, east_jerusalem_palestinian_residents, payer,
    powerless, generational, trapped, local).

% Ministers, governors, and security commanders administering the pockets of self-rule created by the Oslo agreements. Their mandate depends on delivering statehood through negotiation, while the strongest Israeli coalition partners campaign on refusing exactly that outcome. Security coordination with Israel continues month to month; formal recognition of their claims has receded. They possess no channel into the religious rulings that define the other side's red lines.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, palestinian_authority_officialdom, excluded,
    moderate, biographical, constrained, regional).

% The IDF, internal security services, and Civil Administration that operate the occupied territories day to day: staffing checkpoints, guarding settlement roads, running military courts, coordinating with Palestinian forces. The arrangement yields strategic depth, early-warning terrain, and a held Jordan Valley; it also consumes manpower year after year, exposes officers to foreign-court scrutiny, and assigns them tasks that tear at domestic cohesion. Withdrawals happen only when a civilian cabinet forces them, as in Gaza in 2005, over internal warnings.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, israeli_security_establishment, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__religious_restoration_reading, israeli_security_establishment, beneficiary).

% The large share of Jewish Israelis outside the national-religious camp: taxpayers funding settlement subsidies and duplicated administrations, parents of conscripts doing guard duty on settlement perimeters, voters whose repeated majorities for territorial compromise convert into little, because coalition arithmetic hands small pro-settlement parties vetoes and rabbinic authorities brand withdrawal orders illegitimate for soldiers to carry out. Emigration exists but means leaving family networks, army friendships, and a security identity formed next to a war zone.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, secular_israeli_public, payer,
    organized, biographical, constrained, national).

% Ultra-Orthodox rabbinic courts and parties commanding large populations who never accepted that a human-run state could be an instrument of redemption. One strand condemns the state's very existence as presumptuous before the Messiah; another cooperates pragmatically while denying the theological premise. They hold that the ingathering must await divine initiative, and their objection arrives from inside the tradition the restoration reading claims to speak for.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, haredi_nonzionist_leadership, excluded,
    organized, generational, identity_locked, national).

% Federations, denominations, and advocacy organizations abroad that fund and defend Israel broadly while growing estranged from the settlement enterprise specifically. Identification among younger members has declined across successive surveys; some congregations have affiliated openly with anti-occupation groups. They can redirect donations and lobbying away from the settlement project at the cost of communal rupture, and they hold no seat in Israeli coalition mathematics.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, diaspora_liberal_jewish_institutions, excluded,
    organized, biographical, mobile, global).

% The International Court of Justice, UN treaty bodies, and Security Council majorities that have repeatedly found the settlements inconsistent with the Geneva Conventions and, most recently, the occupation itself unlawful. Opinions accumulate — the 2004 wall opinion, the 2024 advisory opinion — backed by no enforcement arm of their own, shaping diplomatic weather rather than ground facts.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, international_legal_institutions, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(zionist_legitimacy_basis__religious_restoration_reading, ideological_settler_core).
narrative_ontology:fixing_cost_class(zionist_legitimacy_basis__religious_restoration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the national-religious community's collective action across generations: channels youth through schools and youth movements into settlement careers, synchronizes rabbinic rulings with municipal planning and army service, pools funds and volunteer labor for building outposts and neighborhoods, and sustains a shared calendar of meaning around the land.
% TRANSFER_FUNCTION: Moves land and planning jurisdiction from Palestinian residents to Jewish settlement authorities under state guarantee; moves state money — housing subsidies, infrastructure, security deployments — toward the settlement enterprise; moves decision-authority over territory from electoral and diplomatic processes to halakhic rulings; imposes movement, water, and legal-status costs on Palestinians living under the same administration.
% ABSENT_VOICES: Palestinian residents of the West Bank and East Jerusalem would object first — their consent is never sought and their title has no standing inside the reading's premise of an unconditional grant. Also absent: ultra-Orthodox authorities who deny the theological identification of state and redemption from within Judaism, diaspora liberal institutions who contest the settlement project while funding the state, and the Palestinian negotiating counterpart whose existence the messianic frame renders provisional. They sit outside the room because the reading's founding premise assigns the land's disposition to divine grant rather than to negotiation among claimants.
% DISAPPEARANCE_RATIONALE: Overnight removal of the mandate-and-forbiddance structure would force several hundred thousand settlers to re-found their lives on non-theological ground, collapse the institutions (yeshivas, municipalities, youth movements) built around the mission, release the Israeli state to bargain over territory on purely strategic terms, reopen Palestinian claims that the frame currently refuses to hear, and unsettle allied governments whose support is calibrated to the settlement question. The regional diplomatic order would reorganize around whatever legitimacy basis replaced it.
% FOUNDING_PROBLEM: After the June 1967 war the religious Zionist camp faced an unanticipated problem: the historic centers of Jewish life in Judea and Samaria were suddenly under Israeli control, and the camp had to decide whether the conquest was a temporary strategic asset to be traded or a sacred trust to be settled and never surrendered. This reading was built to answer that question — to convert a military outcome into a covenantal fact and to make withdrawal unthinkable.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: contemporary accounts and later histories of the post-1967 period (including scholarship on Gush Emunim's formation) document that the settlement drive was a deliberate interpretive choice made against competing religious and secular positions at the time; religious dissenters inside Judaism (ultra-Orthodox authorities, religious peace circles) attested then and since that the covenantal reading was one option among several, not an inevitability; and state archives of successive governments show the mandate being asserted against cabinets that preferred compromise. No corroborating source outside the beneficiary parties attests that the problem is settled; the movement's own institutions alone maintain that it is.
narrative_ontology:disappearance_verdict(zionist_legitimacy_basis__religious_restoration_reading, world_rearranges).
narrative_ontology:founding_problem_status(zionist_legitimacy_basis__religious_restoration_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__religious_restoration_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(zionist_legitimacy_basis__religious_restoration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zionist_legitimacy_basis__religious_restoration_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

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
 *   Claimed type and metrics are authored independently. Structurally this is a tangled_rope: a genuine, functioning coordination machine (it mobilizes and maintains a multi-generational community, solves real collective-action problems in building and defending communities) fused, through the same institutions and the same enforcement, to severe asymmetric extraction from Palestinians and from secular-Israeli democratic agency; it cannot persist without active enforcement (army deployment, military courts, permit regimes, rabbinic discipline), hence requires_active_enforcement. Metrics are descriptive of actual operation. Extractiveness 0.72: the transfer of land, water, and jurisdiction is continuous and compounding. Suppression 0.80 is authored as the RAW STRUCTURAL property it is — unscaled by power or scope; the scaling happens in the engine's chi computation, not here. Theater ratio 0.30: most activity is functionally real (building, administering, fighting), but a persistent minority of the enterprise is performative — archaeological pageantry, pilgrimage politics, rally culture — peaking visibly at the 2005 disengagement confrontation (orange-ribbon mass mobilization) before subsiding. Accessibility_collapse 0.60: within the frame, territorial compromise collapses as an option almost entirely (theologically forbidden), yet exit from the frame itself remains possible at real cost — the post-2005 drift of parts of the national-religious public toward state-centered pragmatism demonstrates the boundary is permeable, unlike a natural law. Resistance 0.75: two intifadas, sustained diplomatic and legal campaigns, intra-Jewish dissent, and repeated mass Israeli protest movements. The measurement series run on ONE shared ten-point grid (1967-2025); every tracked metric is authored at every point. The trajectories are cyclical rather than monotonic: Oslo (1993) and disengagement (2005) produce visible troughs in suppression and extractiveness followed by re-hardening — tension, crisis, partial thaw, accumulation. The oscillation is not noise: each crisis round functions as intermittent reinforcement, validating the no-partner theology, expanding settlement in the aftermath, and raising the next round's baseline. Suppression_requirement is authored deliberately because the story traces enforcement-machinery maturation — the occupation apparatus built up across the interval from ad hoc military government to a hardened, routinized system; a static scalar would hide that ratchet. Endpoint values equal the base_properties scalars by construction (metrics measured at interval end).
 *
 * PERSPECTIVAL GAP:
 *   The seats compute radically different constraints from identical structure. From the rabbinic seat the arrangement presents as something closer to natural law — an eternal grant, experienced as no more revisable than the text itself; the messianic frame makes the constraint feel discovered, not built. From the ideological settler seat it presents as home plus duty: coordination they live inside willingly. From the quality-of-life seat it is a housing market with a security surcharge. From the Palestinian payer seats the same structure presents as pure extraction — a snare-shaped world of permits, walls, and military law with no exit and no vote. The secular Israeli public experiences democratic override: majorities that do not become policy. The engine derives these per-seat divergences from the structural data (roles, power, exits, traps); this story does not adjudicate them — it declares them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. The rabbinic authority and both settler seats sit at the beneficiary end (low d): they collect land, subsidy, protection, and standing. The ideological core's identity_locked exit keeps it pinned near the full-beneficiary pole even though it pays real security costs — fusion damps the perceived price. Quality-of-life settlers derive slightly above the core because their constrained (not locked) exit and transactional motivation register costs more readily. The two Palestinian seats derive nearest the full-target end (d approaching 1.0): trapped exit, powerless power, total scope of exposure — the trap amplifies effective extraction. The secular Israeli public derives moderately high: a declared victim with organized power and constrained (not trapped) exit, bearing diffuse fiscal, personal, and democratic costs. The security establishment is deliberately NOT placed in the beneficiary or victim arrays — it is dual-positioned, and its directionality should compute near symmetric with slight beneficiary tilt; forcing it into either array would falsify the structure. Excluded seats (PA officialdom, haredi leadership, diaspora institutions) contribute consensus-provenance signal — the unanimity of the frame arises partly because these objectors were never seated — not correction-grade input.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — what to do with 1967 — remains live: every negotiation round, every annexation debate, every soldier-refusal letter reopens it, so this constraint has not outlived its mandate and mandatrophy_resolved is not declared. The classification guards against two symmetrical mislabels. Calling it a snare would erase the genuine coordination the frame performs for hundreds of thousands of people whose communal life, education, and meaning are real goods it delivers; calling it a rope would erase the fact that the same structure strips land, law, and movement from millions who never consented and cannot leave. Tangled_rope holds both truths: coordination for insiders, extraction of outsiders, welded by enforcement. Note also what the frame is designed to prevent: mandatrophy. Because the mandate is defined as eternal — a covenant, not a program — the constraint carries no sunset clause and structurally repels obsolescence; the messianic horizon converts every setback into deferred redemption. A constraint built to be un-expirable is the opposite failure mode from a scaffold, and the absence of has_sunset_clause is itself diagnostic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_sibling_structural_delta,
    'How does this constraint''s structure change if instantiated under the sibling readings of kernel zionist_legitimacy_basis instead of this one?',
    'Compile all three sibling stories against the same standing arrangement and compare per-seat classifications: national_liberation_reading reframes the transfers as the price of emancipation (epsilon materially lower, victim set narrowed to wartime and displacement costs); settler_colonial_reading treats the transfer as illegitimate ab initio (epsilon materially higher, entire settler beneficiary set reframed as extraction apparatus).',
    'This file''s verdict covers only the restoration reading. The identical territory classifies differently under each sibling; cross-sibling divergence is the designed measurement, not inconsistency. Any synthesis that averages epsilons across readings destroys the indexical information the decomposition exists to preserve.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_sibling_structural_delta, conceptual, 'Reading-relative epsilon over a fixed referent (committer frame, Rule 1-2 routing).').

omega_variable(
    theological_extraction_discount,
    'Does the reading''s internal moral accounting register the costs borne by Palestinians as extraction at all, or are those costs absorbed as providentially ordered transition?',
    'Ethnographic and textual study of how adherents encode Palestinian harm in their moral ledgers: rabbinic statements after violent episodes, school curricula, funeral sermons, post-crisis pastoral literature. If harm is systematically recoded as tragedy-without-liability or as redemptive necessity, the discount is operating.',
    'A strong discount widens the computed seat divergence — experienced extraction inside beneficiary seats falls far below the structural measure, making the frame feel benign from inside and monstrous from outside. Payer-seat classifications are unaffected; the divergence is the finding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_extraction_discount, conceptual, 'Whether the frame''s theology suppresses registration of its own extraction costs.').

omega_variable(
    enforcement_attribution_problem,
    'How much of the enforcement burden (checkpoints, military courts, settlement protection deployments) is attributable to the theological mandate versus a security-first occupation Israel would run regardless?',
    'Counterfactual budget and deployment analysis separating settlement-protection allocations from a modeled security-minimal occupation; natural experiments where rabbinic rulings visibly forced state action (Amona eviction standoff, Evyatar, hilltop-youth confrontations where the state moved against its own citizens on rabbinic timing).',
    'Isolates this constraint''s marginal chi from baseline occupation. A small marginal share would demote the reading from driver to symbolic legitimation layer riding a security constraint; a large share confirms it as the operative agenda-setter.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_attribution_problem, empirical, 'Marginal enforcement footprint of the theological mandate versus security-only occupation.').

omega_variable(
    messianic_unfalsifiability,
    'Can any observed event disconfirm the messianic-process claim, given that delay is canonically interpreted as concealment rather than absence of redemption?',
    'Historical analysis of the movement''s response sequence to disconfirming shocks: the 1973 war, the Oslo accords, the 2005 disengagement — testing whether belief updated on evidence or absorbed each blow into the redemption timetable.',
    'If unfalsifiable by design, accessibility_collapse for insiders approaches totality irrespective of outcomes — the constraint behaves mountain-like from the inside while remaining wholly constructed externally. This asymmetry is exactly the false-summit signature the FSM machinery watches for on mountain-claimed stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(messianic_unfalsifiability, conceptual, 'Disconfirmability of the messianic-process claim and its effect on insider alternative-collapse.').

omega_variable(
    disengagement_precedent_scope,
    'Does the 2005 Gaza withdrawal demonstrate that the constraint is breakable by state power, or was Gaza structurally exceptional?',
    'Structured comparison against a hypothetical West Bank withdrawal: settler population (roughly eight thousand versus half a million), density of theologically central sites, annexation and residency histories, documented reserve-soldier refusal rates and rabbinic mobilization intensity in each case, coalition stability effects.',
    'Calibrates fixing_cost. If Gaza was the easy case and it still fractured the state''s politics, the authored ''prohibitive'' rating is conservative; if Gaza''s periphery made it uniquely tractable, the rating may understate the difficulty by an order of magnitude.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disengagement_precedent_scope, empirical, 'External validity of the single completed withdrawal as evidence about fixability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis__religious_restoration_reading, 1967, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zion_tr_t1967, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 1967, 0.14).
narrative_ontology:measurement(zion_tr_t1974, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 1974, 0.19).
narrative_ontology:measurement(zion_tr_t1980, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 1980, 0.23).
narrative_ontology:measurement(zion_tr_t1988, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 1988, 0.27).
narrative_ontology:measurement(zion_tr_t1993, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 1993, 0.25).
narrative_ontology:measurement(zion_tr_t2000, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 2000, 0.28).
narrative_ontology:measurement(zion_tr_t2005, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 2005, 0.38).
narrative_ontology:measurement(zion_tr_t2012, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 2012, 0.34).
narrative_ontology:measurement(zion_tr_t2019, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 2019, 0.32).
narrative_ontology:measurement(zion_tr_t2025, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 2025, 0.3).

% Extraction over time
narrative_ontology:measurement(zion_be_t1967, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 1967, 0.35).
narrative_ontology:measurement(zion_be_t1974, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 1974, 0.44).
narrative_ontology:measurement(zion_be_t1980, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 1980, 0.51).
narrative_ontology:measurement(zion_be_t1988, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 1988, 0.59).
narrative_ontology:measurement(zion_be_t1993, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 1993, 0.61).
narrative_ontology:measurement(zion_be_t2000, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 2000, 0.66).
narrative_ontology:measurement(zion_be_t2005, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 2005, 0.63).
narrative_ontology:measurement(zion_be_t2012, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 2012, 0.67).
narrative_ontology:measurement(zion_be_t2019, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 2019, 0.7).
narrative_ontology:measurement(zion_be_t2025, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 2025, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(zion_su_t1967, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 1967, 0.46).
narrative_ontology:measurement(zion_su_t1974, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 1974, 0.5).
narrative_ontology:measurement(zion_su_t1980, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 1980, 0.57).
narrative_ontology:measurement(zion_su_t1988, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 1988, 0.7).
narrative_ontology:measurement(zion_su_t1993, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 1993, 0.64).
narrative_ontology:measurement(zion_su_t2000, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 2000, 0.73).
narrative_ontology:measurement(zion_su_t2005, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 2005, 0.71).
narrative_ontology:measurement(zion_su_t2012, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 2012, 0.76).
narrative_ontology:measurement(zion_su_t2019, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 2019, 0.79).
narrative_ontology:measurement(zion_su_t2025, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 2025, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis__religious_restoration_reading, identity_coordination).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, zionist_legitimacy_basis__national_liberation_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, zionist_legitimacy_basis__settler_colonial_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'Zionism's legitimacy.' The label conflates three structurally distinct claims that share one referent (the post-1967 occupation-and-settlement arrangement) and diverge in epsilon: national_liberation_reading (upstream — historically prior, broadest coalition, lowest epsilon; its success supplied the political vehicle the restoration reading rode after 1967), religious_restoration_reading (this file — intermediate epsilon; converts the national achievement into covenant and thereby hardens the victim structure), settler_colonial_reading (downstream critique — highest epsilon; treats the transfer as illegitimate from inception and is itself energized by observing the restoration reading's outputs). Upstream feeds downstream: the liberation reading's achievements are cited as evidence BY the restoration reading, and its failures cited by the settler_colonial reading. Each file stands alone with its own stable epsilon per DP-001; the family edges exist for contamination-propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
