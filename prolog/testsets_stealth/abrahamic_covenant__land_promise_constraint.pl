% ============================================================================
% CONSTRAINT STORY: abrahamic_covenant__land_promise_constraint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_abrahamic_covenant__land_promise_constraint, []).

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
 *   constraint_id: abrahamic_covenant__land_promise_constraint
 *   human_readable: Abrahamic Covenant — Land-Promise Reading (Territorial Grant)
 *   domain: religious/political/territorial
 *
 * SUMMARY:
 *   This story instantiates one reading of the abrahamic_covenant kernel —
 *   the land_promise_constraint reading: the claim that the Genesis covenant
 *   includes a territorial grant of the Land of Canaan. The standing
 *   arrangement under contest is the modern territorial regime this reading
 *   legitimates: a state and settlement project whose title narrative runs
 *   through divine grant, maintained by military occupation and
 *   administrative machinery, with displaced and occupied Palestinian
 *   populations bearing the costs and no adjudicating seat for rival readings
 *   of the same text. Per the kernel-reading rules, the sibling readings
 *   (isaac_covenant_reading, ishmael_covenant_reading) are separate
 *   constraints — their victim and beneficiary structures are neither
 *   adjudicated nor hedged here; the contest they carry is routed to omega
 *   variables and the cs_structure reading relations. Epsilon's referent is
 *   the standing arrangement — the territorial regime the reading legitimates
 *   — assessed as it operates, never the arrangement the reading's holders
 *   imagine completion would bring. The claim and the metrics are independent
 *   authored facts: claimed_type is authored from the structural analysis
 *   (the reading's operative function in the standing arrangement is
 *   title-legitimation for dispossession; persistence depends on coercion;
 *   victims are identifiable; alternatives are suppressed), while the metrics
 *   describe observed operation. Where a beneficiary seat computes rope-like
 *   and a payer seat computes snare-like, that divergence is the measurement.
 *
 * KEY AGENTS:
 *   - israeli_state_apparatus: agenda-setter and primary beneficiary (institutional power, constrained exit) — administers the arrangement the reading legitimates and collects its sovereignty dividends
 *   - religious_zionist_settlement_movement: direct beneficiary (organized, identity_locked) — the reading's most committed constituency, whose way of life is constituted by the claim
 *   - diaspora_jewish_supporters: beneficiary (organized, mobile) — external support channel, internally divided on the reading
 *   - moderate_israeli_citizenry: dual-positioned beneficiary/payer (organized, constrained) — inside the narrative, against the maximalism, outvoted
 *   - palestinian_authority: payer (organized, trapped) — institution built for a sovereignty the title narrative forecloses
 *   - displaced_palestinian_refugees: primary target (powerless, trapped, generational horizon) — bears the arrangement's founding extraction and its transmission across generations
 *   - west_bank_palestinian_residents and gaza_palestinian_residents: primary targets (powerless, trapped) — bear the ongoing occupation's extraction directly
 *   - palestinian_citizens_of_israel: payer (moderate power, constrained) — equality claims subordinated to the title narrative
 *   - ishmael_line_covenant_communities: excluded (organized, trapped) — hold the rival reading of the same text with no standing in the framework
 *   - international_legal_institutions: analytical observer (institutional, no enforcement arm) — assesses the arrangement against law it cannot enforce
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(abrahamic_covenant__land_promise_constraint, 0.85).
domain_priors:suppression_score(abrahamic_covenant__land_promise_constraint, 0.9).
domain_priors:theater_ratio(abrahamic_covenant__land_promise_constraint, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, extractiveness, 0.85).
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(abrahamic_covenant__land_promise_constraint, snare).
narrative_ontology:human_readable(abrahamic_covenant__land_promise_constraint, "Abrahamic Covenant — Land-Promise Reading (Territorial Grant)").
narrative_ontology:topic_domain(abrahamic_covenant__land_promise_constraint, "religious/political/territorial").

domain_priors:requires_active_enforcement(abrahamic_covenant__land_promise_constraint).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(abrahamic_covenant__land_promise_constraint, '531bddd8-2581-452c-8be5-dacd39b083e3').
narrative_ontology:cs_kernel_codification('531bddd8-2581-452c-8be5-dacd39b083e3', fixed_text).
narrative_ontology:cs_authority_grounding('531bddd8-2581-452c-8be5-dacd39b083e3', lineage).
narrative_ontology:cs_interpretation_layer_present('531bddd8-2581-452c-8be5-dacd39b083e3').
narrative_ontology:cs_reading_relation('531bddd8-2581-452c-8be5-dacd39b083e3', abrahamic_covenant__isaac_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('531bddd8-2581-452c-8be5-dacd39b083e3', abrahamic_covenant__ishmael_covenant_reading, forecloses).
narrative_ontology:cs_axiom('531bddd8-2581-452c-8be5-dacd39b083e3', foundational, territorial_grant_is_covenant_substance).
narrative_ontology:cs_axiom_status(territorial_grant_is_covenant_substance, holdable).
narrative_ontology:cs_axiom_grounding('531bddd8-2581-452c-8be5-dacd39b083e3', territorial_grant_is_covenant_substance, theological).
narrative_ontology:cs_axiom('531bddd8-2581-452c-8be5-dacd39b083e3', secondary, covenant_title_persists_across_exile).
narrative_ontology:cs_axiom_status(covenant_title_persists_across_exile, holdable).
narrative_ontology:cs_axiom_grounding('531bddd8-2581-452c-8be5-dacd39b083e3', covenant_title_persists_across_exile, theological).
narrative_ontology:cs_reference_frame('531bddd8-2581-452c-8be5-dacd39b083e3', unconditional_eternal_land_grant).
narrative_ontology:cs_drift_state('531bddd8-2581-452c-8be5-dacd39b083e3', contemporary_post_1967_occupation_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('531bddd8-2581-452c-8be5-dacd39b083e3', '').
narrative_ontology:cs_kernel_id(abrahamic_covenant__land_promise_constraint, abrahamic_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(abrahamic_covenant__land_promise_constraint, israeli_state_apparatus).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__land_promise_constraint, religious_zionist_settlement_movement).
narrative_ontology:constraint_victim(abrahamic_covenant__land_promise_constraint, displaced_palestinian_refugees).
narrative_ontology:constraint_victim(abrahamic_covenant__land_promise_constraint, west_bank_palestinian_residents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__land_promise_constraint, diaspora_jewish_supporters).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__land_promise_constraint, moderate_israeli_citizenry).
narrative_ontology:constraint_victim(abrahamic_covenant__land_promise_constraint, moderate_israeli_citizenry).
narrative_ontology:constraint_victim(abrahamic_covenant__land_promise_constraint, palestinian_authority).
narrative_ontology:constraint_victim(abrahamic_covenant__land_promise_constraint, gaza_palestinian_residents).
narrative_ontology:constraint_victim(abrahamic_covenant__land_promise_constraint, palestinian_citizens_of_israel).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Runs the state whose founding and ongoing territorial claims draw on the covenant narrative: it administers the West Bank through a military government, registers land, approves settlements, funds their infrastructure, and teaches the narrative in its school curricula. It collects the sovereignty dividends of the arrangement — territory, strategic depth, a legitimacy story — while bearing its costs: military expenditure, international censure, and coalition dependence on parties who treat the narrative as non-negotiable. Reframing its claims on purely secular and legal grounds is available in principle and politically ruinous in practice.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, israeli_state_apparatus, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(abrahamic_covenant__land_promise_constraint, israeli_state_apparatus, beneficiary).

% Builds and lives in communities beyond the 1967 lines on the conviction that the land was promised to the people and that settling it advances redemption. Receives land, housing, roads, and legal defense through state channels. Members' religious practice, family decisions, and life projects are organized around the settlement enterprise; giving up the claim would not be a policy change for them but the collapse of a way of life.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, religious_zionist_settlement_movement, beneficiary,
    organized, generational, identity_locked, regional).

% Sustains the arrangement from abroad through philanthropy, political lobbying, and immigration. The promise narrative supplies part of the attachment story that organizes this support; the community is divided, with liberal denominations reading the promise as conditional or non-territorial. Individual supporters can reduce their involvement at will; the institutions they fund are more durably invested.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, diaspora_jewish_supporters, beneficiary,
    organized, generational, mobile, global).

% Lives inside the state the narrative underwrites and pays the conflict's bills: conscription, reserve duty, security spending, international isolation, and moral controversy. Survey majorities have repeatedly supported trading territory for a settlement; their preferences are overridden by coalition partners who hold the promise reading as a red line.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, moderate_israeli_citizenry, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(abrahamic_covenant__land_promise_constraint, moderate_israeli_citizenry, payer).

% Governs limited self-rule enclaves under the Oslo framework while the sovereignty it was built to achieve is the claim the title narrative denies. It coordinates security with the state it negotiates against, pays salaries in territory it does not control, depends on donor funding and withheld clearance revenues, and pursues statehood through recognition votes rather than territory.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, palestinian_authority, payer,
    organized, generational, trapped, regional).

% Lost homes, land, and livelihoods in the 1948 war and again in 1967; several million registered descendants live across the region with a recognized right of return that the regime's enforcement prevents them from exercising. Citizenship in host states ranges from full to none. Their title to the same land is the mirror of the covenant title, and the framework that administers the arrangement gives their claim no adjudicating seat.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, displaced_palestinian_refugees, payer,
    powerless, generational, trapped, regional).

% Live under military administration that controls land, water, movement, and building permits while settlements expand around and sometimes through their communities. Families have lost groves and hilltops to outposts; the law applied to them differs from the law applied to settlers a few hundred meters away. Leaving means emigration; staying means navigating the permit regime.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, west_bank_palestinian_residents, payer,
    powerless, biographical, trapped, local).

% Have lived under blockade since 2007 and through successive military campaigns; the 2023-2025 war displaced the large majority of the strip's population and destroyed most of its housing. Officials overseeing aid and reconstruction proposals have framed the territory in biblical terms. Exit is sealed by the blockade; return to destroyed neighborhoods is contingent on military discretion.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, gaza_palestinian_residents, payer,
    powerless, biographical, trapped, local).

% Hold citizenship and vote, inside a state whose self-definition — supplied by the title narrative — privileges one community's historical claim. Equal-citizenship arguments run against the grain of that narrative; planning regimes have historically restricted their towns' growth, and their loyalty is politically contested in ways the majority's is not.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, palestinian_citizens_of_israel, payer,
    moderate, biographical, constrained, national).

% Read the same Genesis text as continuing the covenant through Ishmael and understand the promise's beneficiaries to include the region's Arab population. Inside the framework the title narrative administers, this reading has no standing: it is not taught, not adjudicated, and its political expression is classified as hostility rather than interpretation. Their co-religionists are among the population paying the arrangement's costs.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, ishmael_line_covenant_communities, excluded,
    organized, generational, trapped, global).

% Assess the arrangement against treaty and humanitarian law; the International Court of Justice found the occupation unlawful and settlement policy in violation in its 2024 advisory opinion. They issue findings and resolutions but hold no enforcement arm; the state absorbs their pronouncements as costs and the narrative's coalition dismisses them as hostility.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, international_legal_institutions, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(abrahamic_covenant__land_promise_constraint, israeli_state_apparatus).
narrative_ontology:fixing_cost_class(abrahamic_covenant__land_promise_constraint, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The reading coordinates the beneficiary coalition's attachment to a contested territory across generations: it supplies a shared answer to why this land is theirs, motivates settlement, military service, and diaspora support, and sustains commitment through defeat and exile. It solves a real collective-action problem — maintaining a dispersed people's connection to a specific territory — for the population it addresses.
% TRANSFER_FUNCTION: Moves territorial control, land value, and legitimacy from the populations resident on the land to the covenant-leveraging state and its settlement constituency; moves the costs of dispossession — statelessness, displacement, restricted movement — onto the displaced and occupied populations; and moves identity goods (meaning, continuity, warrant) to the beneficiary coalition and its diaspora supporters.
% ABSENT_VOICES: The displaced were absent when the reading's modern territorial form was fixed: the Balfour Declaration, the Mandate, and the 1947 partition were decided without a Palestinian seat, and the reading's operative form was consolidated after 1967 in forums — government, rabbinate, settlement institutions — where no Palestinian counter-reading was ever adjudicated. Holders of the ishmael reading of the same text are structurally outside the framework; their interpretation is treated as rejectionism rather than exegesis. Palestinian Christian and Muslim attachments to the same land carry no standing in the title narrative.
% DISAPPEARANCE_RATIONALE: If the territorial-grant reading vanished overnight, the settlement movement's warrant would collapse — its communities are constituted by the claim — annexation and sovereignty politics would lose their theological floor, and the conflict would re-sort onto security, demographic, and international-law axes with a different distribution of veto players: compromise coalitions currently blocked by covenant red lines would become possible, and the diaspora support channel would restructure. The coercive machinery would not vanish with the reading, but its legitimacy architecture and veto distribution would rearrange.
% FOUNDING_PROBLEM: A defeated and exiled people needed a warrant that its connection to a specific territory outlived the loss of sovereignty: a promise that made continued attachment meaningful across two millennia of landlessness and marked the land as held-in-trust rather than merely remembered.
% FOUNDING_PROBLEM_CORROBORATION: Academic historiography of religious Zionism — produced outside the settlement coalition's institutional apparatus — documents the mutation of the reading's function from exile-warrant to activist mandate for possession; Palestinian and international legal scholarship attests that the reading's current operative function is title-legitimation rather than exile-hope. No source inside the beneficiary coalition attests the dead status — religious authorities hold redemption incomplete and the problem live — and that dispute is itself the contested terrain the promise_conditionality and fulfillment_status omegas track.
narrative_ontology:disappearance_verdict(abrahamic_covenant__land_promise_constraint, world_rearranges).
narrative_ontology:founding_problem_status(abrahamic_covenant__land_promise_constraint, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(abrahamic_covenant__land_promise_constraint, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(abrahamic_covenant__land_promise_constraint, 'none', 1).
narrative_ontology:epsilon_provenance(abrahamic_covenant__land_promise_constraint, 0.85, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(abrahamic_covenant__land_promise_constraint_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(abrahamic_covenant__land_promise_constraint, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(abrahamic_covenant__land_promise_constraint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.85 (interval end): the arrangement's founding act displaced roughly 750,000 people in 1948 and the 1967 occupation displaced hundreds of thousands more; extraction accumulated since through settlement expansion, land expropriation, and — in 2023-2025 — the displacement of the large majority of Gaza's population. Suppression is authored at 0.90 and is structural: military government, the permit regime, the blockade, denial of return by force, and legal architecture that applies different law by identity. The internalized component of suppression operates on the BENEFICIARY side, not the victim side: the reading fuses religious identity with the land claim so that compromise is experienced as betrayal rather than choice — this is the identity-lock mechanism, and if the frame broke (a major rabbinic authority reverting to the classical position that redemption is not human-achievable), beneficiary-side exit would open and the suppression requirement would spike and then decay. Theater rises monotonically (0.18 to 0.62): as the arrangement's operative work shifted to administrative and military machinery, a growing share of the reading's activity became legitimation performance — biblical nomenclature, heritage budgets, sovereignty ceremonies — maintaining the claim while coercion does the functional work; this is Goodhart drift in the legitimation function itself. The measurement series run on one shared time grid (all three metrics at all eight points); the Oslo dip is visible in all three series simultaneously — the one interval where extraction, suppression, and performative maintenance all eased together — followed by the post-2000 ratchet. Resistance is high (0.75) and sustained: two intifadas, international litigation culminating in the 2024 ICJ advisory opinion, UN action, boycott movements, and internal Israeli dissent. Accessibility_collapse is 0.6: alternatives (two-state frameworks, rights-based frames, conditional covenant readings) exist but are progressively foreclosed by facts on the ground and coalition hardening — far from mountain-like collapse, but tightening. Coalition potential among the powerless victims is real — shared narrative, solidarity infrastructure, the 2005 boycott call — but is fragmented by host-state divergence, intra-Palestinian governance rivalry, and a separation architecture that physically prevents coalition formation; the victims are numerous and structurally unable to act as one. Inter-institutionally, the arrangement runs through three interfaces: state-to-PA (the Oslo architecture as managed subordination), state-to-international-institutions (lawfare absorption), and movement-to-state (the settlement constituency as internal veto player).
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute sharply different types from the same verses. The payer seats — refugees, West Bank and Gaza residents — sit inside a structure that takes land, home, and legal personhood while returning nothing; from their position the arrangement has no coordination content whatsoever. The beneficiary seats experience genuine goods: the settlement movement receives meaning, community, and warrant; moderate Israelis receive a state narrative that answers why here; diaspora supporters receive an identity anchor — from those seats the same structure computes rope-like. The agenda-setter seat experiences self-legitimation: the state both administers the arrangement and is legitimated by it, so its self-assessment is not evidence about the structure. Same-level lateral divergence: the ishmael-line communities and the settlement movement are both organized, transgenerational constituencies reading the same text — what differentiates their positions is not power but which side of the title claim the reading places them on, and the framework gives one side an adjudicating apparatus and the other none.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: israeli_state_apparatus and religious_zionist_settlement_movement — these seats sit near the beneficiary end of directionality; the state's d is partially offset by the costs it bears (security expenditure, international isolation, coalition dependence), which is why it is authored as agenda_setter with secondary beneficiary rather than pure collector. Victims declared: displaced_palestinian_refugees and west_bank_palestinian_residents, joined on the stakeholder surface by gaza_palestinian_residents and palestinian_citizens_of_israel — trapped exit, powerless to moderate power, these seats sit near the full-target end; trapped and identity-locked positions amplify effective extraction, and the refugees' generational horizon means the extraction transmits rather than amortizes. The excluded seat (ishmael_line_covenant_communities) pays a discursive cost — its reading of the same text has no standing — which the derivation should read as target-side. The international legal institutions are analytical: they assess but collect and pay nothing. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, and the arrangement's large spatial scope (a claim administered across contested territory with global discursive reach) amplifies verification difficulty for the extraction it embeds.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem the territorial-grant reading was built to solve — giving a landless, exiled people a transgenerational warrant that possession could be restored — is dead: the sovereign exists, the exile-warrant function is obsolete, and the reading now performs a different function (legitimating control over another population). The R5 mismatch (status=dead with disappearance_verdict=world_rearranges) flags capture: the arrangement persists because a named seat captures its gains — the state apparatus — not because anyone still needs the original function. The classification prevents mislabeling in both directions: it is not a rope — the coordination goods are real but they are the cover that makes the extraction politically durable, not the operative function; and it is not a piton — the arrangement is not inertially maintained by nobody-in-charge, it is actively captured and enforced by a named agenda-setter, so the piton cost-asymmetry (the administrator could change it but the cost to fix exceeds what it bears) does not describe it: here the administrator bears the cost of fixing, which is why fixing_cost is prohibitive and why the arrangement persists. The mandate is resolved-as-captured, not resolved-as-atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    abrahamic_kernel_reading_position,
    'This story instantiates one reading of the abrahamic_covenant kernel — the land_promise_constraint reading (the territorial-grant claim). What would the sibling readings — isaac_covenant_reading (election/lineage boundary) and ishmael_covenant_reading (continuation through Ishmael) — change structurally if operative, and where exactly does the disagreement sit?',
    'Generate the sibling stories as separate constraints and compare their beneficiary/victim sets and epsilon values: the isaac reading without the territorial axis relocates the extraction to identity-boundary exclusion; the ishmael reading relocates the title claim to the Arab/Muslim population and inverts the victim set. The disagreement is located on two distinct axes — who carries the covenant (lineage) and what it grants (territory) — and this story fixes only the territorial axis.',
    'The snare structure computed here is a property of THIS reading''s operation in the standing arrangement, not of the kernel as such; a corpus that averaged across readings would misattribute the displacement arrangement''s costs to the covenant complex as a whole.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(abrahamic_kernel_reading_position, conceptual, 'Committer structure: one reading of the abrahamic_covenant kernel; siblings are separate constraints with their own epsilon and victim sets.').

omega_variable(
    promise_conditionality,
    'Is the territorial promise conditional on covenant fidelity or unconditional? Conditional readings make current title forfeitable by conduct; unconditional readings foreclose forfeiture arguments entirely.',
    'Track rabbinic and religious-authority rulings on territorial compromise over time, alongside textual analysis of the covenant''s condition clauses (Genesis 17 against the Deuteronomy 28-30 blessing-and-curse frame); observe how religious parties vote on land transfers.',
    'Conditional readings license religiously-grounded territorial compromise and make the title claim internally revisable; unconditional readings harden the arrangement, foreclose forfeiture arguments, and raise the suppression requirement — the classification would shift from a contested title claim toward an absolute one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(promise_conditionality, conceptual, 'Whether the grant is conditional on covenant fidelity — the internal contest this reading carries.').

omega_variable(
    fulfillment_status,
    'Is the promise read as fulfilled (1948/1967 completed it, so claims are bounded at current lines) or ongoing (fulfillment is incomplete, so expansion remains mandated)?',
    'Track doctrinal statements and settlement-movement literature: does possession complete the promise or does incompleteness mandate continued settling? Observable in movement texts, ministerial rhetoric, and the doctrinal justification offered for each new settlement wave.',
    'Determines whether the arrangement''s extraction is bounded or structurally unbounded — a fulfilled reading caps claims and opens a settlement path; an ongoing reading makes the extraction expandable without limit, which is the difference between a settling claim and an expanding one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fulfillment_status, empirical, 'Whether the promise is read as fulfilled or ongoing — the expansion engine of the arrangement.').

omega_variable(
    coordination_function_or_cover,
    'Is the reading''s coordination of the beneficiary coalition (identity, meaning, mobilization durability) a genuine independent function of the territorial-grant claim, or the cover story that makes title-legitimation for dispossession politically durable?',
    'Historical counterfactual comparison: secular territorial nationalism produced overlapping arrangements before the religious turn (Mandate-era Zionism pre-dated religious-nationalist settlement doctrine); test whether covenant-framed constituencies show mobilization durability or sacrifice tolerance exceeding security-and-nation-framed constituencies under identical conditions.',
    'If the in-group coordination is genuine and independent, the constraint computes as a tangled rope carrying catastrophic extraction; if the coordination is cover, the snare claim stands and the identity framing is doing legitimacy work for extraction — the exact pattern the identity_coordination gaming alert watches for.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_or_cover, conceptual, 'Whether the covenant reading''s in-group coordination is genuine function or cover for extraction.').

omega_variable(
    covenant_reading_causal_weight,
    'How load-bearing is the covenant reading in the arrangement''s persistence, relative to security and demographic drivers — would the arrangement persist on security logic alone if the reading lost its holders?',
    'Natural experiments at moments where covenant-reading actors vetoed security-establishment compromises (Oslo ratification, 2000 Camp David, 2008 Olmert-Abbas talks, annexation pauses): measure whether the arrangement''s trajectory diverges when the reading''s holders gain or lose veto power.',
    'If the reading is epiphenomenal, the arrangement would persist without it and its independent epsilon contribution is small; if it is load-bearing — blocking exits and compromises that security logic would permit — the reading is the binding element and the measured extraction attaches to it directly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(covenant_reading_causal_weight, empirical, 'How much of the arrangement''s persistence the covenant reading independently causes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(abrahamic_covenant__land_promise_constraint, 1948, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(abra_tr_t1948, abrahamic_covenant__land_promise_constraint, theater_ratio, 1948, 0.18).
narrative_ontology:measurement(abra_tr_t1967, abrahamic_covenant__land_promise_constraint, theater_ratio, 1967, 0.3).
narrative_ontology:measurement(abra_tr_t1977, abrahamic_covenant__land_promise_constraint, theater_ratio, 1977, 0.38).
narrative_ontology:measurement(abra_tr_t1987, abrahamic_covenant__land_promise_constraint, theater_ratio, 1987, 0.42).
narrative_ontology:measurement(abra_tr_t1993, abrahamic_covenant__land_promise_constraint, theater_ratio, 1993, 0.46).
narrative_ontology:measurement(abra_tr_t2000, abrahamic_covenant__land_promise_constraint, theater_ratio, 2000, 0.5).
narrative_ontology:measurement(abra_tr_t2010, abrahamic_covenant__land_promise_constraint, theater_ratio, 2010, 0.56).
narrative_ontology:measurement(abra_tr_t2025, abrahamic_covenant__land_promise_constraint, theater_ratio, 2025, 0.62).

% Extraction over time
narrative_ontology:measurement(abra_be_t1948, abrahamic_covenant__land_promise_constraint, base_extractiveness, 1948, 0.7).
narrative_ontology:measurement(abra_be_t1967, abrahamic_covenant__land_promise_constraint, base_extractiveness, 1967, 0.76).
narrative_ontology:measurement(abra_be_t1977, abrahamic_covenant__land_promise_constraint, base_extractiveness, 1977, 0.78).
narrative_ontology:measurement(abra_be_t1987, abrahamic_covenant__land_promise_constraint, base_extractiveness, 1987, 0.8).
narrative_ontology:measurement(abra_be_t1993, abrahamic_covenant__land_promise_constraint, base_extractiveness, 1993, 0.7).
narrative_ontology:measurement(abra_be_t2000, abrahamic_covenant__land_promise_constraint, base_extractiveness, 2000, 0.76).
narrative_ontology:measurement(abra_be_t2010, abrahamic_covenant__land_promise_constraint, base_extractiveness, 2010, 0.8).
narrative_ontology:measurement(abra_be_t2025, abrahamic_covenant__land_promise_constraint, base_extractiveness, 2025, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(abra_su_t1948, abrahamic_covenant__land_promise_constraint, suppression_requirement, 1948, 0.65).
narrative_ontology:measurement(abra_su_t1967, abrahamic_covenant__land_promise_constraint, suppression_requirement, 1967, 0.74).
narrative_ontology:measurement(abra_su_t1977, abrahamic_covenant__land_promise_constraint, suppression_requirement, 1977, 0.72).
narrative_ontology:measurement(abra_su_t1987, abrahamic_covenant__land_promise_constraint, suppression_requirement, 1987, 0.79).
narrative_ontology:measurement(abra_su_t1993, abrahamic_covenant__land_promise_constraint, suppression_requirement, 1993, 0.66).
narrative_ontology:measurement(abra_su_t2000, abrahamic_covenant__land_promise_constraint, suppression_requirement, 2000, 0.81).
narrative_ontology:measurement(abra_su_t2010, abrahamic_covenant__land_promise_constraint, suppression_requirement, 2010, 0.85).
narrative_ontology:measurement(abra_su_t2025, abrahamic_covenant__land_promise_constraint, suppression_requirement, 2025, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(abrahamic_covenant__land_promise_constraint, identity_coordination).
narrative_ontology:affects_constraint(abrahamic_covenant__land_promise_constraint, isaac_covenant_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__land_promise_constraint, ishmael_covenant_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Abrahamic covenant' conflates at least three structurally distinct claims: lineage-election (who carries the covenant — the isaac_covenant_reading), continuation/inclusion (whether it extends through Ishmael — the ishmael_covenant_reading), and territorial grant (what it promises — this story). Their epsilon values differ sharply: the lineage readings' extraction turns on identity-boundary exclusion; the territorial reading's epsilon is dominated by the modern displacement arrangement. This story instantiates only the territorial-grant reading. The lineage readings are upstream: their exclusivity premise is cited as warrant by the territorial reading's holders, which is why the edges run from this story to both siblings and the family decomposition must be read as a unit.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
