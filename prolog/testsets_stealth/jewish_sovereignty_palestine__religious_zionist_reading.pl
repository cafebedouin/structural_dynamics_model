% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__religious_zionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__religious_zionist_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: jewish_sovereignty_palestine__religious_zionist_reading
 *   human_readable: Divine-Promise Territorial Claim, Religious Zionist Reading (Statehood as Theological Fulfillment)
 *   domain: political/religious/nationalist
 *
 * SUMMARY:
 *   This story instantiates the religious_zionist_reading of the contested
 *   kernel jewish_sovereignty_palestine: the commitment that a divine promise
 *   of the land to the Jewish people grounds an inalienable territorial
 *   title, and that Jewish statehood is the theological fulfillment — the
 *   beginning of redemption — of that promise. As an operative political
 *   arrangement, the claim structures the settlement enterprise, forecloses
 *   partition as illegitimate (the land cannot be ceded because its title was
 *   never Israel's to cede), and assigns Palestinians no standing as rival
 *   claimants — their presence is at most a subordinate problem within the
 *   frame. The claim/metric gap is deliberate and is the diagnostic payload:
 *   the reading presents the title as theological fact, beyond human
 *   arrangement and enforcement-independent — a mountain-shaped presentation
 *   — while the authored metrics describe an arrangement whose operation is
 *   enforcement-dependent, massively costly to a named victim population, and
 *   actively resisted. The engine measures that divergence; the false-summit
 *   signature fires because the mountain claim carries declared
 *   beneficiaries. Interval anchoring: t=0 is 1967, when the occupation began
 *   and the territorial-messianic reading (Kookian lineage, later Gush
 *   Emunim) moved from marginal to operative; t=58 is 2025, when
 *   messianic-territorial factions sit inside the governing coalition and the
 *   reading's program is mainstreamed. One time-point unit is one year; all
 *   tracked metrics share this single grid. The four sibling readings of the
 *   same kernel are separate constraint files with their own extraction
 *   measures, beneficiaries, and victims; they are linked here, not folded
 *   in. KEY AGENTS (by structural relationship): -
 *   religious_zionist_settlement_movement: Agenda-setter and direct capturer
 *   (powerful/identity_locked) — administers the settlement enterprise,
 *   translates theology into territorial policy, receives land, budgets, and
 *   state power - jewish_covenant_community: Primary beneficiary
 *   (organized/identity_locked) — collects theological fulfillment, identity,
 *   and covenantal continuity from the arrangement's operation -
 *   israeli_state_apparatus: Enforcement agenda-setter (institutional/mobile)
 *   — administers land, military governance, and settlement approval;
 *   formally retains residual capacity to change the arrangement -
 *   palestinians_in_occupied_territories: Primary target (powerless/trapped)
 *   — bears land expropriation, movement restriction, and denial of
 *   self-determination; the frame grants their claims no standing -
 *   palestinian_citizens_of_israel: Secondary target (moderate/constrained) —
 *   formal citizens subordinated by the covenant framing of state membership
 *   - palestinian_refugee_diaspora: Excluded target (powerless/trapped) —
 *   return foreclosed absolutely; no seat in the conversation -
 *   israeli_territorial_compromise_advocates: Excluded dissenters
 *   (moderate/constrained) — partition arguments heard as infidelity within
 *   the frame - international_legal_institutions: Analytical observer
 *   (institutional/analytical) — attests illegality from outside the covenant
 *   framework
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__religious_zionist_reading, 0.88).
domain_priors:suppression_score(jewish_sovereignty_palestine__religious_zionist_reading, 0.85).
domain_priors:theater_ratio(jewish_sovereignty_palestine__religious_zionist_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__religious_zionist_reading, mountain).
narrative_ontology:human_readable(jewish_sovereignty_palestine__religious_zionist_reading, "Divine-Promise Territorial Claim, Religious Zionist Reading (Statehood as Theological Fulfillment)").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__religious_zionist_reading, "political/religious/nationalist").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__religious_zionist_reading).
domain_priors:emerges_naturally(jewish_sovereignty_palestine__religious_zionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__religious_zionist_reading, '65d6d405-f9de-44c5-a24e-a50b44b8ba91').
narrative_ontology:cs_kernel_codification('65d6d405-f9de-44c5-a24e-a50b44b8ba91', fixed_text).
narrative_ontology:cs_authority_grounding('65d6d405-f9de-44c5-a24e-a50b44b8ba91', lineage).
narrative_ontology:cs_interpretation_layer_present('65d6d405-f9de-44c5-a24e-a50b44b8ba91').
narrative_ontology:cs_reading_relation('65d6d405-f9de-44c5-a24e-a50b44b8ba91', jewish_sovereignty_palestine__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('65d6d405-f9de-44c5-a24e-a50b44b8ba91', jewish_sovereignty_palestine__settler_colonial_reading, forecloses).
narrative_ontology:cs_reading_relation('65d6d405-f9de-44c5-a24e-a50b44b8ba91', jewish_sovereignty_palestine__cultural_zionist_reading, forecloses).
narrative_ontology:cs_reading_relation('65d6d405-f9de-44c5-a24e-a50b44b8ba91', jewish_sovereignty_palestine__post_zionist_reading, coexists_with).
narrative_ontology:cs_axiom('65d6d405-f9de-44c5-a24e-a50b44b8ba91', foundational, divine_covenant_grants_inalienable_land_title).
narrative_ontology:cs_axiom_status(divine_covenant_grants_inalienable_land_title, holdable).
narrative_ontology:cs_axiom_grounding('65d6d405-f9de-44c5-a24e-a50b44b8ba91', divine_covenant_grants_inalienable_land_title, theological).
narrative_ontology:cs_axiom('65d6d405-f9de-44c5-a24e-a50b44b8ba91', foundational, political_sovereignty_is_redemptive_fulfillment).
narrative_ontology:cs_axiom_status(political_sovereignty_is_redemptive_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('65d6d405-f9de-44c5-a24e-a50b44b8ba91', political_sovereignty_is_redemptive_fulfillment, theological).
narrative_ontology:cs_axiom('65d6d405-f9de-44c5-a24e-a50b44b8ba91', secondary, territorial_partition_is_illegitimate).
narrative_ontology:cs_axiom_status(territorial_partition_is_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('65d6d405-f9de-44c5-a24e-a50b44b8ba91', territorial_partition_is_illegitimate, theological).
narrative_ontology:cs_reference_frame('65d6d405-f9de-44c5-a24e-a50b44b8ba91', covenantal_whole_land_title).
narrative_ontology:cs_drift_state('65d6d405-f9de-44c5-a24e-a50b44b8ba91', contemporary_annexationist_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('65d6d405-f9de-44c5-a24e-a50b44b8ba91', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__religious_zionist_reading, jewish_covenant_community).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__religious_zionist_reading, religious_zionist_settlement_movement).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__religious_zionist_reading, palestinians_in_occupied_territories).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_citizens_of_israel).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_refugee_diaspora).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__religious_zionist_reading, divine_promise_of_eretz_yisrael).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__religious_zionist_reading, inalienable_covenant_title).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__religious_zionist_reading, statehood_as_redemptive_fulfillment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the settlement enterprise through rabbinic authorities, yeshivot, land-trust bodies, and coalition factions, translating the covenant theology into territorial policy: settlement construction, outpost legalization, and annexation initiatives. Receives land, state budgets, military protection, and governing power directly. Its exit is fused with the frame itself: leaving the arrangement would mean abandoning the community's entire meaning structure, as the trauma of the 2005 disengagement demonstrated at small scale.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, religious_zionist_settlement_movement, agenda_setter,
    powerful, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__religious_zionist_reading, religious_zionist_settlement_movement, beneficiary).

% The collective beneficiary of the divine grant: the arrangement delivers theological fulfillment, covenantal continuity, national meaning, and the ingathering of exiles. Most members do not administer the arrangement; they receive its identity goods and are bound to it by a transgenerational covenantal horizon. Exit would mean leaving the covenant framework in which the community's identity, liturgy, and historical narrative are constituted — not a policy choice but a self-dissolution.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, jewish_covenant_community, beneficiary,
    organized, civilizational, identity_locked, global).

% Administers land registration, military governance of the occupied territories, settlement approval, and the legal regime that implements the claim. It alternates between containing and advancing the settlement movement's program, and absorbs the international costs of enforcement (isolation, legal exposure, sanctions risk). It formally retains the capacity to change the arrangement — partition, withdrawal, negotiated bounds — though each year of deepened settlement and coalition dependence erodes that residual mobility.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, israeli_state_apparatus, agenda_setter,
    institutional, generational, mobile, national).

% Bear the arrangement's direct costs: land expropriation for settlement, movement restrictions, home demolitions, statelessness, and denial of self-determination. Within the reading's frame their claims have no standing — the land's title is settled divinely, leaving nothing to negotiate. Exit would require leaving the very land whose title the arrangement denies them any voice over; there is no state to exit into and no equal citizenship to exit under.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, palestinians_in_occupied_territories, payer,
    powerless, generational, trapped, national).

% Hold formal citizenship in a state the reading frames as covenant fulfillment, which ranks membership theologically and structurally subordinates non-Jewish citizens. They bear status subordination, land and planning discrimination, and the delegitimation of their political expression, while retaining more mobility and legal recourse than the occupied population. Emigration is possible but costly, and exit does not escape the arrangement's definition of the state they would leave.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_citizens_of_israel, payer,
    moderate, generational, constrained, national).

% Displaced in 1948 and after, they hold the arrangement's deepest historical cost: the reading forecloses return absolutely, since their villages sit inside the inalienable grant. They have no seat in the conversation the frame permits — no standing as claimants, no negotiated remedy, no framework in which their objection registers. They bear the cost from outside every institutional surface the arrangement maintains.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_refugee_diaspora, excluded,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_refugee_diaspora, payer).

% Would argue for partition, land-for-peace, and bounded claims. Within the reading's frame their position is not merely mistaken but unfaithful — ceding land is ceding what was never Israel's to cede — so their arguments are heard as betrayal rather than policy disagreement. They persist in opposition politics, media, and academia, marginalized electorally and, at the extreme, targeted (the 1995 assassination of a prime minister by an adherent of the reading's frame).
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, israeli_territorial_compromise_advocates, excluded,
    moderate, biographical, constrained, national).

% Assess the occupation, settlement enterprise, and annexation moves against international law through advisory opinions, resolutions, and case work. They attest the arrangement's structure from outside the covenant framework entirely; the reading dismisses their standing on precisely that ground — outsiders cannot adjudicate a divine grant — which is why their analytical position never converts into enforcement.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, international_legal_institutions, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_sovereignty_palestine__religious_zionist_reading, religious_zionist_settlement_movement).
narrative_ontology:fixing_cost_class(jewish_sovereignty_palestine__religious_zionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the Jewish covenant community around a shared sacred geography: it solves the collective-action problems of sustaining a transgenerational national-religious project — maintaining diaspora-era identity, mobilizing migration and settlement, binding a religious tradition to a political program, and giving dispersed populations a single common purpose (ingathering, settlement, redemption).
% TRANSFER_FUNCTION: Moves land, sovereignty, and self-determination from Palestinians to the Jewish covenant community — via state land administration, settlement construction, and military governance — and moves identity goods (meaning, continuity, covenantal purpose) to the community, while moving the costs of dispossession, statelessness, and restricted movement onto Palestinians.
% ABSENT_VOICES: Palestinians — most absolutely the refugee diaspora, whose return the reading forecloses without a hearing — are absent from the reading's calculus by design: divine title leaves no seat for a rival claimant. Israeli territorial-compromise advocates are present but delegitimized (their position reads as infidelity, not policy). International legal institutions are dismissed as outsiders to the covenant. The unanimity of the frame is produced by these exclusions; the excluded object persistently, from outside every surface the arrangement maintains.
% DISAPPEARANCE_RATIONALE: If the divine-promise claim lost its operative force overnight, the settlement enterprise would lose its theological engine and mobilization capacity, partition and land-for-peace would return to the thinkable, coalition politics would reorganize around bounded claims, and Palestinian claims would regain standing in negotiation. The regional arrangement is built on the claim's operation and would rearrange around its absence — nothing about the arrangement is self-maintaining without it.
% FOUNDING_PROBLEM: The founding problem was twofold: Jewish landlessness and exile — a stateless people with no territorial refuge, rendered acute by the twentieth century — and, for the religious stream specifically, the theological problem of what a secular Jewish state MEANT. The Kookian answer was that statehood is the beginning of redemption, converting a refugee-and-sovereignty problem into a redemptive project whose completion requires the whole land.
% FOUNDING_PROBLEM_CORROBORATION: From outside the benefiting parties: international legal institutions and Palestinian parties attest the statelessness problem was resolved by 1948 statehood and that the arrangement now persists expansionarily rather than remedially; historians of the Kookian tradition writing outside the movement attest that the redemptive-whole-land reframing consolidated only after 1967, converting a solved problem into a maximalist mandate. The covenant community attests liveness (redemption incomplete) almost entirely from within — the 'live' side of the contest has no strong external corroboration, which is itself signal.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__religious_zionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__religious_zionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__religious_zionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__religious_zionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__religious_zionist_reading, 0.88, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__religious_zionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_sovereignty_palestine__religious_zionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, ExtMetricName, E),
    domain_priors:suppression_score(jewish_sovereignty_palestine__religious_zionist_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(jewish_sovereignty_palestine__religious_zionist_reading),
    narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(jewish_sovereignty_palestine__religious_zionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.88 because the arrangement's demands on Palestinians are near-maximal and unbounded inside the frame: land expropriation, denial of self-determination, statelessness for the occupied population, subordinated membership for citizens, and absolute foreclosure of refugee return — with no internal limit, because divine title admits no negotiated boundary. Suppression is 0.85, authored as a raw structural property (the engine scales only extractiveness, by directionality and scope; suppression passes through unscaled): military governance, movement permits, administrative detention, legal suppression of Palestinian political expression, and — inside the covenant community — the framing of compromise as infidelity. Theater is low (0.20): the commitment is operationally serious — settlements are built, budgets flow, laws are passed — with only a growing ceremonial and symbolic share as the movement institutionalized. Accessibility_collapse is 0.65: within the frame, alternatives (partition, withdrawal, shared sovereignty) collapse almost completely — they are not merely unwise but theologically impossible — yet the collapse is incomplete even internally (a minority religious-Zionist partition tradition persists) and alternatives remain fully live outside the frame, which is what distinguishes this from a genuine natural law. Resistance is 0.70: two intifadas, sustained legal and diplomatic resistance, internal Israeli dissent, and international legal rulings — high, though partially exhausted and suppressed. The three measurement series share one grid (t=0,10,20,30,40,50,58). Suppression_requirement is authored because this story specifically tracks enforcement-capacity maturation: from ad hoc post-1967 military governance to a hardened, institutionalized enforcement apparatus; its rising trajectory is that hardening, not extraction drift alone.
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence is extreme. From the covenant community's seat the arrangement computes as fulfillment: the community is identity-locked into the frame, collects meaning and continuity, and bears little of its cost — its computed type will sit at the coordination end. From the Palestinian seats the same structure computes as maximal enforced extraction: trapped populations bearing land loss, movement restriction, and denial of standing, with exit that would require abandoning the very land whose title the arrangement claims. The state apparatus seat is split: it administers the enforcement while absorbing international isolation and internal division, and its formal mobility — the residual capacity to fix the arrangement — is exactly what the reading's deepening capture erodes. Same-level dynamics: palestinian_citizens_of_israel and palestinians_in_occupied_territories hold the same nominal victim position but different exit options (formal citizenship with constrained mobility versus trapped statelessness), so their computed directionalities diverge despite equal standing outside the frame. Coalition potential for the powerless seats is real in principle but structurally blocked by the arrangement itself: the frame denies precisely the shared standing in which a cross-cut coalition of citizens, occupied residents, and refugees could form, and the citizen/occupied/diaspora split is partly a product of the arrangement's own history.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: jewish_covenant_community (beneficiary, identity_locked, civilizational horizon) sits deep at the beneficiary end — the arrangement subsidizes its identity and continuity at near-zero structural cost to it. religious_zionist_settlement_movement (agenda_setter with beneficiary secondary role, identity_locked) also sits low: it administers the arrangement AND captures its material gains. The victim declarations drive the target end: palestinians_in_occupied_territories (trapped) and palestinian_refugee_diaspora (trapped and excluded) sit near the full-target end — no arbitrage, no mobility, and exit would mean leaving the land itself; palestinian_citizens_of_israel (constrained) sits high but slightly below the trapped seats. israeli_state_apparatus is declared neither beneficiary nor victim: it is the enforcement seat, and its structural relationship is genuinely mixed — it channels gains it does not ultimately keep while bearing international costs, so its derived directionality sits mid-range with capture dynamics pulling it beneficiary-ward over time. International legal institutions are analytical observers. No directionality overrides were needed: the beneficiary/victim declarations plus exit options produce the correct structure for every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding mandate was the redemption of exile: converting Jewish landlessness into covenantal return, and — in the Kookian reframing — making statehood itself theologically meaningful. The mandate's status is genuinely contested: outside the beneficiary set, the statelessness problem is attested as solved in 1948 (international legal institutions, Palestinian parties, and historians of the tradition converge on the view that the arrangement now persists expansionarily rather than remedially); inside the tradition, the mandate is live by definition, since redemption is incomplete until the whole land is under covenant sovereignty. The classification prevents mislabeling in both directions. It prevents the reading's self-presentation — divine fact, beyond politics, a mountain claim — from being accepted at face value: the structural data show enforcement-dependence, named victims, and sustained resistance, and the false-summit machinery exists precisely to catch a mountain claim carrying beneficiaries. It equally prevents the opposite error of dismissing the arrangement as pure cover: the coordination the frame performs for the covenant community — transgenerational identity, mobilization, cohesion — is real and sincere, and it is what makes the extraction durable; the honest structure holds genuine coordination and asymmetric extraction in one enforced arrangement, not one masquerading as the other. The R5 mismatch check reads status=contested against verdict=world_rearranges: no settled zombie flag, but a live contest over the mandate — the arrangement persists on a mandate whose liveness is itself the dispute.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_grant_vs_constructed_claim,
    'Is the territorial claim a genuine theological mountain — a divine grant that would hold regardless of who enforces it and whether anyone defends it — or a constructed political claim whose persistence depends on enforcement and which benefits identifiable agents?',
    'Internal-tradition analysis combined with enforcement-dependence evidence: the three-oaths debate, dinah d''malkhuta, and pre-1967 religious Zionist positions that accepted partition or deferred sovereignty to messianic times, tested against the observation that the claim''s political force tracks enforcement capacity and movement organization rather than any enforcement-independent warrant.',
    'If constructed, the false-summit signature reclassifies the mountain claim to a coordination/extraction hybrid; if the divine grant is accepted as enforcement-independent theological fact, mountain certification stands and the extraction metrics must be re-read as the price of covenant fulfillment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_grant_vs_constructed_claim, conceptual, 'Whether the divine-title claim is natural-law-like or an enforced construct with identifiable beneficiaries.').

omega_variable(
    palestinian_standing_in_theology,
    'Is the subordination of Palestinian standing a content of the theology itself, or a political deployment of a theology whose internal resources (Kookian universalism, resident-alien legal categories, the tradition''s own pre-state partition debates) could ground recognition?',
    'Textual and historical analysis of the tradition''s treatment of non-Jewish inhabitants across periods, comparing the universalist strands of the founding texts against the territorialist deployment that consolidated after 1967.',
    'If deployment rather than doctrine, a reform reading within the tradition could bound the extraction without abandoning the covenant frame, and the victim structure is contingent; if doctrinal, the reading inherently forecloses Palestinian standing and the extraction is structural to the theology itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(palestinian_standing_in_theology, conceptual, 'Whether the victim structure is doctrine or deployment.').

omega_variable(
    enforcement_dependency_of_persistence,
    'Would the claim''s operative force persist absent active enforcement (settlement administration, military governance, legal suppression), or is its persistence entirely enforcement-dependent?',
    'Natural experiments: settlement-freeze periods during the Oslo era, the 2005 disengagement''s aftermath (the frame intensified when enforcement withdrew from Gaza rather than contracting), and counterfactual demographic-political modeling of the enterprise without state enforcement.',
    'Enforcement-dependent persistence supports reclassification away from the mountain claim toward a coordination/extraction structure; enforcement-independent persistence of the frame would strengthen the reading''s own mountain presentation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_dependency_of_persistence, empirical, 'Whether the arrangement''s persistence is enforcement-dependent.').

omega_variable(
    kernel_reading_contest_location,
    'This constraint instantiates the religious_zionist_reading of the jewish_sovereignty_palestine kernel — one of five live readings. What structurally changes under sibling readings, and where exactly is the disagreement located?',
    'Comparative classification across the sibling stories: the liberal_nationalist_reading grounds title in collective self-determination (partition legitimate, claims bounded); the settler_colonial_reading characterizes the arrangement as displacement regardless of title source; the cultural_zionist_reading severs the claim from sovereignty requirements; the post_zionist_reading reframes the founding narrative as a present-day obstruction to civic equality and regional integration. The disagreement is located in the source of title and the scope of the claim — the two variables that determine partition legitimacy and Palestinian standing.',
    'Under sibling readings the extraction measure falls substantially (bounded claims, recognized Palestinian standing, partition legitimacy); the maximal extraction of this reading is a property of the divine-title/whole-land structure, not of the kernel itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Committer structure: which kernel, which reading, where the sibling disagreement bites.').

omega_variable(
    suppression_mechanism_split,
    'Is the measured suppression structural (military-legal enforcement of the occupation and settlement regime) or internalized (theological framing that makes compromise unthinkable for adherents even where enforcement relaxes)?',
    'Post-enforcement trajectory analysis: the 2005 disengagement removed enforcement from Gaza yet the frame intensified afterward (post-disengagement radicalization); tracking whether frame-intensity follows enforcement presence or persists independently distinguishes the mechanisms.',
    'If internalized, suppression persists beyond enforcement capacity — the arrangement''s stability is higher than enforcement metrics alone suggest, and dismantling it requires theological reframing, not only policy change; the omega feeds the classification''s robustness assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_split, empirical, 'Structural versus internalized suppression mechanism in a constraint with both.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__religious_zionist_reading, 0, 58).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(jewi_tr_t10, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 10, 0.13).
narrative_ontology:measurement(jewi_tr_t20, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement(jewi_tr_t30, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement(jewi_tr_t40, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 40, 0.16).
narrative_ontology:measurement(jewi_tr_t50, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 50, 0.18).
narrative_ontology:measurement(jewi_tr_t58, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 58, 0.2).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(jewi_be_t10, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(jewi_be_t20, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(jewi_be_t30, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 30, 0.74).
narrative_ontology:measurement(jewi_be_t40, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 40, 0.79).
narrative_ontology:measurement(jewi_be_t50, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 50, 0.85).
narrative_ontology:measurement(jewi_be_t58, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 58, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(jewi_su_t10, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(jewi_su_t20, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 20, 0.66).
narrative_ontology:measurement(jewi_su_t30, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(jewi_su_t40, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 40, 0.77).
narrative_ontology:measurement(jewi_su_t50, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 50, 0.82).
narrative_ontology:measurement(jewi_su_t58, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 58, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__religious_zionist_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine__cultural_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine__post_zionist_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Jewish sovereignty over Palestine' decomposes into at least five structurally distinct readings of one kernel, per the epsilon-invariance principle. This story instantiates the religious_zionist_reading: title from divine covenant, scope the whole land, sovereignty as redemptive fulfillment. Its high extraction value is a property of THIS reading's structure (divine title forecloses partition and Palestinian standing), not of the kernel: the liberal_nationalist_reading over the shared referent authors bounded claims and partition legitimacy; the settler_colonial_reading authors the displacement structure itself as the constraint; the cultural_zionist_reading severs the claim from sovereignty; the post_zionist_reading authors the present narrative's obstructiveness. Each sibling carries its own extraction value, beneficiary set, and victims; the files form one constraint family linked through network edges, with this reading as the upstream source whose territorial maximalism changes the operating environment (facts on the ground, coalition configuration, delegitimation of compromise) for all four siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
