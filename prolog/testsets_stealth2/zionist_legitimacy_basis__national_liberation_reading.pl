% ============================================================================
% CONSTRAINT STORY: zionist_legitimacy_basis__national_liberation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zionist_legitimacy_basis__national_liberation_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: zionist_legitimacy_basis__national_liberation_reading
 *   human_readable: National-Liberation Legitimacy Basis of Zionism (Reading of Contested Kernel)
 *   domain: political_history/nationalism/settler_colonialism_studies
 *
 * SUMMARY:
 *   SUMMARY: This story instantiates ONE reading of the contested kernel
 *   zionist_legitimacy_basis — the national-liberation reading, under which
 *   persecution and historical connection justify the return and the
 *   displacements it entailed, and Arab opposition is recast as denial of
 *   Jewish rights rather than resistance to dispossession. The standing
 *   arrangement under contest (the ε referent, fixed across sibling files) is
 *   the existing structure: a sovereign Jewish state founded through the
 *   1947-49 war and displacement of roughly 700,000 Palestinians, the
 *   prevention of their return, the post-1967 occupation and settlement
 *   enterprise, the blockade of Gaza, and the perpetual enforcement and
 *   narrative apparatus sustaining all of it. Values are reading-indexed over
 *   that fixed referent: this file authors ε through the national-liberation
 *   reading's own lights, which register the imposed costs on Palestinians as
 *   real but attribute much of them to war and Arab rejection, and which hold
 *   the rescue-coordination core to be genuine. The claim/metric gap is
 *   deliberate: claimed_type is rope (the reading's self-understanding —
 *   national liberation as rescue coordination whose coercions are defensive
 *   or justified), while the authored metrics describe a heavily enforced
 *   arrangement with four declared victim seats — the engine measures the
 *   divergence; nothing here reconciles them. KEY AGENTS (by structural
 *   relationship): - israeli_state_establishment: agenda-setter
 *   (institutional/arbitrage) — runs enforcement, curates narrative, receives
 *   the sovereignty dividend - great_power_patrons: agenda-setter +
 *   beneficiary (institutional/arbitrage) — authored the diplomatic
 *   architecture, extend the shield - diaspora_jewry: beneficiary
 *   (organized/mobile) — refuge guarantee and identity anchor -
 *   mizrahi_jewish_immigrants: beneficiary (moderate/constrained) — parallel
 *   refugee stream reinforcing the return claim - israeli_jewish_public:
 *   beneficiary + payer (organized/constrained) — receives sovereignty, pays
 *   in conscription and moral burden - settler_movement_communities:
 *   beneficiary + agenda-setter (organized/identity_locked) — hardestens the
 *   arrangement, fused with it - palestinian_refugees_1948: payer
 *   (powerless/trapped) — displaced, denied return, citizenship limbo -
 *   west_bank_palestinian_residents: payer (powerless/trapped) — occupied,
 *   disenfranchised - gaza_strip_residents: payer (powerless/trapped) —
 *   blockaded, war-exposed - palestinian_arab_israeli_citizens: payer +
 *   beneficiary (moderate/constrained) — enfranchised yet subordinated -
 *   neighboring_arab_states: payer (institutional/arbitrage) — belligerents
 *   turned hedging normalizers - internal_postzionist_dissenters: excluded
 *   (moderate/constrained) — marginalized internal critics -
 *   international_legal_bodies: observer (institutional/analytical) —
 *   adjudicates, documents, enforces nothing directly
 *
 * KEY AGENTS:
 *   - - israeli_state_establishment: agenda-setter/administrator (institutional/arbitrage) — collects the sovereignty dividend and administers enforcement and narrative
 *   - - great_power_patrons: agenda-setter + beneficiary (institutional/arbitrage) — authored partition diplomacy, extend military and diplomatic shelter for strategic rents
 *   - - diaspora_jewry: primary beneficiary (organized/mobile) — standing refuge option and identity anchor, conditional-to-solidary support
 *   - - mizrahi_jewish_immigrants: beneficiary (moderate/constrained) — expelled-from-Arab-states absorbees who double the return claim
 *   - - israeli_jewish_public: beneficiary + payer (organized/constrained) — sovereignty and services received; conscription, taxes, and moral-political burden paid
 *   - - settler_movement_communities: beneficiary + agenda-setter (organized/identity_locked) — settlement enterprise that reshapes policy and cannot exit without betraying its own founding premise
 *   - - palestinian_refugees_1948: payer (powerless/trapped) — displaced 1947-49, barred from return, multigenerational statelessness
 *   - - west_bank_palestinian_residents: payer (powerless/trapped) — occupied since 1967, governed without a vote
 *   - - gaza_strip_residents: payer (powerless/trapped) — blockaded since 2007, subjected to recurring major wars
 *   - - palestinian_arab_israeli_citizens: payer + beneficiary (moderate/constrained) — enfranchised minority carrying land loss and constitutional subordination
 *   - - neighboring_arab_states: payer (institutional/arbitrage) — absorbed refugees and lost wars, now monetize normalization
 *   - - internal_postzionist_dissenters: excluded (moderate/constrained) — internal critics marginalized at each security peak
 *   - - international_legal_bodies: analytical observer (institutional/analytical) — registers the refugee count and applies humanitarian law without direct enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis__national_liberation_reading, 0.62).
domain_priors:suppression_score(zionist_legitimacy_basis__national_liberation_reading, 0.83).
domain_priors:theater_ratio(zionist_legitimacy_basis__national_liberation_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 0.83).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__national_liberation_reading, rope).
narrative_ontology:human_readable(zionist_legitimacy_basis__national_liberation_reading, "National-Liberation Legitimacy Basis of Zionism (Reading of Contested Kernel)").
narrative_ontology:topic_domain(zionist_legitimacy_basis__national_liberation_reading, "political_history/nationalism/settler_colonialism_studies").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__national_liberation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__national_liberation_reading, 'e9a3ec12-5e89-4912-b491-109282d608d3').
narrative_ontology:cs_kernel_codification('e9a3ec12-5e89-4912-b491-109282d608d3', distributed).
narrative_ontology:cs_authority_grounding('e9a3ec12-5e89-4912-b491-109282d608d3', lineage).
narrative_ontology:cs_interpretation_layer_present('e9a3ec12-5e89-4912-b491-109282d608d3').
narrative_ontology:cs_reading_relation('e9a3ec12-5e89-4912-b491-109282d608d3', zionist_legitimacy_basis__settler_colonial_reading, forecloses).
narrative_ontology:cs_reading_relation('e9a3ec12-5e89-4912-b491-109282d608d3', zionist_legitimacy_basis__religious_restoration_reading, influences).
narrative_ontology:cs_axiom('e9a3ec12-5e89-4912-b491-109282d608d3', foundational, ancestral_connection_grounds_return_right).
narrative_ontology:cs_axiom_status(ancestral_connection_grounds_return_right, holdable).
narrative_ontology:cs_axiom_grounding('e9a3ec12-5e89-4912-b491-109282d608d3', ancestral_connection_grounds_return_right, deontological).
narrative_ontology:cs_axiom('e9a3ec12-5e89-4912-b491-109282d608d3', foundational, existential_peril_justifies_displacement_costs).
narrative_ontology:cs_axiom_status(existential_peril_justifies_displacement_costs, holdable).
narrative_ontology:cs_axiom_grounding('e9a3ec12-5e89-4912-b491-109282d608d3', existential_peril_justifies_displacement_costs, empirically_contingent).
narrative_ontology:cs_reference_frame('e9a3ec12-5e89-4912-b491-109282d608d3', ancestral_return_self_determination).
narrative_ontology:cs_drift_state('e9a3ec12-5e89-4912-b491-109282d608d3', post_october_seventh_contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e9a3ec12-5e89-4912-b491-109282d608d3', '2026-08-03T12:00:00Z').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__national_liberation_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__national_liberation_reading, israeli_state_establishment).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__national_liberation_reading, great_power_patrons).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__national_liberation_reading, diaspora_jewry).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__national_liberation_reading, mizrahi_jewish_immigrants).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__national_liberation_reading, israeli_jewish_public).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__national_liberation_reading, settler_movement_communities).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__national_liberation_reading, palestinian_refugees_1948).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__national_liberation_reading, west_bank_palestinian_residents).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__national_liberation_reading, gaza_strip_residents).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__national_liberation_reading, palestinian_arab_israeli_citizens).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__national_liberation_reading, palestinian_arab_israeli_citizens).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__national_liberation_reading, israeli_jewish_public).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__national_liberation_reading, neighboring_arab_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Runs the state the national return built: commands the military, administers land including property vested from refugees barred from returning, controls borders and residency, and curates the founding narrative taught in schools and presented abroad. Receives the sovereignty dividend — diplomatic standing, alliance backing, domestic cohesion — and pays for it with permanent mobilization and exposure to delegitimation campaigns. The institution manages the arrangement rather than choosing among alternatives to it.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, israeli_state_establishment, agenda_setter,
    institutional, generational, arbitrage, global).

% Authored the diplomatic architecture that recognized the national home — the Balfour Declaration, the UNSCOP partition recommendation, early recognition — and continue to shield it diplomatically and militarily in exchange for strategic alignment, intelligence cooperation, and regional footholds. Patronage can be redirected and occasionally conditioned, but no patron's portfolio improves if the arrangement collapses.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, great_power_patrons, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__national_liberation_reading, great_power_patrons, beneficiary).

% Scattered communities across dozens of countries for whom the arrangement functions as a standing refuge guarantee and an identity anchor: wherever persecution recurs, a sovereign door stands open — proven from 1948 through Soviet-era departures, Ethiopian rescues, and Ukrainian arrivals. Most never move, but the option reprices their insecurity; many fund, lobby, and advocate. Drifting into indifference is possible and increasingly common in younger cohorts; drifting into open opposition carries communal cost.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, diaspora_jewry, beneficiary,
    organized, generational, mobile, global).

% Families expelled or driven out of Arab and Muslim countries after 1948, absorbed by the new state after losing property there comparable to what refugees lost in Palestine. Their arrival doubled the return claim — persecuted returnees on both sides of the ledger — while their early marginalization inside the absorbing society, in transit camps and under a cultural hierarchy, left grievances that now fuel right-leaning politics.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, mizrahi_jewish_immigrants, beneficiary,
    moderate, generational, constrained, regional).

% Citizens who receive sovereignty, security services, and a revived national language, and pay through universal conscription, combat losses, taxation, reserve-duty cycles, and the moral-political burden of administering disputed territories. Emigration exists but is socially costly and usually temporary; the population is bound to the arrangement's outcomes whether it endorses the narrative or not.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, israeli_jewish_public, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__national_liberation_reading, israeli_jewish_public, payer).

% Communities planted in the territories captured in 1967 under state sponsorship — subsidies, land allocation, army protection — who understand their presence as the continuation of the original return. They shape coalition politics well beyond their numbers and are the most identity-fused constituency: dismantling their communities would be experienced not as a policy adjustment but as betrayal of the founding promise. Individuals can leave; collective departure is not on their horizon.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, settler_movement_communities, beneficiary,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__national_liberation_reading, settler_movement_communities, agenda_setter).

% Several hundred thousand displaced in the 1948 war, now millions of descendants, holding deeds and keys to homes inside Israel, registered with UNRWA across Lebanon, Syria, Jordan, the West Bank, and Gaza. Barred from returning by successive governments, most lack full citizenship in host states — most severely in Lebanon. Nothing they control alters the arrangement; their leverage runs through host states, armed factions, and international bodies.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, palestinian_refugees_1948, payer,
    powerless, generational, trapped, regional).

% Residents of the territory occupied since 1967, governed by military administration, settlement encirclement, permit regimes, and periodic closures, with fragments of Palestinian Authority self-rule separated by checkpoints. They hold no vote in the state that governs most of their lives. Movement, residency, and family reunification are administered from outside; individual departure means exile.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, west_bank_palestinian_residents, payer,
    powerless, generational, trapped, regional).

% Over two million people in a coastal strip under blockade since 2007, governed until 2023 by Hamas, subjected to recurring large-scale military campaigns in 2008-09, 2012, 2014, 2021, and the devastating war that followed the October 7, 2023 attack, with reconstruction throttled by dual-use restrictions. Every border is sealed; displacement happens internally.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, gaza_strip_residents, payer,
    powerless, biographical, trapped, regional).

% About a fifth of Israel's citizens: enfranchised, represented in the Knesset, descended from the roughly 150,000 who remained through the 1948 war while most of their society was displaced. They lived under military administration until 1966, lost much of their land to state custodianship, and were formally subordinated to the Jewish-national definition by the 2018 Nation-State Law. Citizenship and public services flow to them; status, land, and trust flow away. Leaving would mean renouncing the only citizenship most have ever held.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, palestinian_arab_israeli_citizens, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__national_liberation_reading, palestinian_arab_israeli_citizens, beneficiary).

% Egypt, Jordan, Syria, and Lebanon — belligerents of 1948-73 that absorbed refugee populations, lost wars, and eventually signed peace or disengagement agreements trading recognition for territory, aid, and alignment. Gulf states have moved toward open normalization. They retain the refugee file as diplomatic leverage and domestic legitimacy currency, and can arbitrage between confrontation and accommodation as prices shift.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, neighboring_arab_states, payer,
    institutional, generational, arbitrage, regional).

% Historians, draft-refusers, joint-list politicians, and peace activists inside Israeli society who challenge the founding narrative — documenting expulsions, opposing occupation, advocating federation or shared statehood. In security crises they are pushed to the margins as traitors or naifs; their books face boycotts and their organizations face funding investigations. They remain inside the polity — voting, serving — but outside the rooms where the official story is curated.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, internal_postzionist_dissenters, excluded,
    moderate, biographical, constrained, national).

% The UN system, the International Court of Justice, and treaty bodies that register the refugee count and the return principle of Resolution 194, apply Geneva Convention law to the territories, issue advisory opinions and commissions of inquiry, and host the delegitimation and counter-delegitimation contest. They adjudicate and document but enforce nothing directly; their findings feed patron decisions and litigation strategies.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, international_legal_bodies, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(zionist_legitimacy_basis__national_liberation_reading, israeli_state_establishment).
narrative_ontology:fixing_cost_class(zionist_legitimacy_basis__national_liberation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the persecuted-minority collective-action problem: a scattered, door-closed population converged on one destination, one revived language, one set of institutions, and one mutual-defense commitment — rescue logistics, nation-building, and a standing refuge guarantee that reprices persecution everywhere.
% TRANSFER_FUNCTION: Moves land title, residency rights, and security control from Palestine's Arab inhabitants toward the Jewish national collective, via state custodianship of refugee property and post-1967 territorial administration; moves diaspora immigration, capital, and philanthropy into the state; moves military and diplomatic patronage from great powers to the state; and exports narrative legitimacy while routing criticism inward toward delegitimation.
% ABSENT_VOICES: Palestinian refugees were objects, not participants, from 1917 through 1993 — excluded from the rooms where final-status questions were deferred; internal post-Zionist dissenters are present in the polity but pushed out of the narrative-curating rooms (curricula, commemoration, public diplomacy) whenever security crises peak; Arab citizens carried no consent-weight in the Nation-State Law. They stand outside the arrangement's legitimacy-production process, which is where their objections would land.
% DISAPPEARANCE_RATIONALE: The state would not evaporate overnight, but its operative claim-structure would collapse: with the liberation basis gone, the religious-restoration and settler-colonial frames would contend to refill the vacuum; diaspora attachment and great-power shielding would reprice immediately; dormant legal exposure on return and restitution would activate. Demonstrably many arrangements depend on this basis continuing to hold — hence rearrangement, with contested succession among the surviving frames.
% FOUNDING_PROBLEM: Stateless minority, exterminatory persecution, closed doors: the Jewish question as Herzl framed it after the Dreyfus affair and the eastern pogroms — a people everywhere resident and nowhere safe, for whom the Evian Conference proved no exit existed. The arrangement was built to solve this by producing sovereign refuge and normalizing Jewish peoplehood among nations.
% FOUNDING_PROBLEM_CORROBORATION: The problem itself is corroborated from outside the beneficiary set: the Evian Conference record documents the closed-door consensus independently of Jewish advocacy; the UNSCOP majority report cites Jewish distress as a decision factor from a non-benefiting body; Holocaust historiography is externally established; the October 7 massacre renewed external acknowledgment that the danger is not historical. No external seat, however, attests that this particular arrangement remains the necessary answer — Arab seats and much of the Global South locate present insecurity in the arrangement itself rather than in the problem it answers. Problem-liveness is corroborated; arrangement-necessity is disputed, and recorded as such.
narrative_ontology:disappearance_verdict(zionist_legitimacy_basis__national_liberation_reading, world_rearranges).
narrative_ontology:founding_problem_status(zionist_legitimacy_basis__national_liberation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__national_liberation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(zionist_legitimacy_basis__national_liberation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zionist_legitimacy_basis__national_liberation_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zionist_legitimacy_basis__national_liberation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(zionist_legitimacy_basis__national_liberation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(zionist_legitimacy_basis__national_liberation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.62 through this reading's own lights — far below what the settler-colonial sibling will assign over the same referent, far above what a maximal adherent concedes. The reading's lights register the 1948 displacement and prevented return (visible spike at t=52), the land custodianship that transferred most Arab-owned land inside Israel, and the accumulating occupation burden; they attribute part of the total to war initiated against the arrangement and treat the remainder as the price of survival. Suppression is 0.83 and is a RAW STRUCTURAL VALUE — unscaled by power or scope; it measures the enforcement stack itself (standing mass army, occupation administration, permit regime, blockade, censorship layers, and the delegitimation-defense apparatus) that the arrangement requires to persist; the engine owns any scaling arithmetic. Theater_ratio 0.58 and rising: the rescue-and-sovereignty function remains real, but the narrative-maintenance share (commemoration, curriculum, hasbara, persecution-invocation deployed to shield current policy) has grown faster than the function since 1967 — the frame increasingly performs the founding justification over an occupational reality it did not predict. Accessibility_collapse 0.64: within the movement, alternatives (Brit Shalom's binationalism, diasporism, territorialism) collapsed almost completely by the 1930s-40s; outside it, alternatives remain live, which keeps the value below mountain-range. Resistance 0.78: a century of near-continuous armed revolt, interstate war, intifadas, boycott campaigns, and diplomatic assault. CYCLICAL PATTERN: the measurement record shows a war-ceasefire-accumulation ratchet rather than smooth drift — 1948-49, 1967, the post-1977 settlement surge, the intifada cycles, and post-2023 each converted acute crisis into permanent control at a higher extraction floor; the oscillation is not noise but the accumulation mechanism itself (crisis converts contingency into permanence). Base_properties scalars reflect the end-state (t=129, projected). All three tracked series run on ONE shared ten-point grid (1897 baseline; t=20 Balfour; t=50 partition; t=52 postwar displacement and absentee-property vesting; t=67-70 occupation onset at t=70; t=80 Begin-era entrenchment; t=90 First Intifada; t=103 Second Intifada and barrier; t=126 October 7 war; t=129 present), so no metric borrows another's endpoints.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently and the divergence is the datum. Trapped payer seats (refugees, West Bank, Gaza) sit at the full-target end and should classify the arrangement as enforced dispossession wearing a rescue story; the mobile diaspora seat experiences the same structure as priced insurance and should compute near-pure coordination; the identity-locked settler seat experiences it as sacred continuity in which exit is unthinkable. Among institutionally powered seats the split is sharpest: the establishment computes defensive necessity, neighboring Arab states compute adversarial containment, and the legal bodies compute adjudication — same power atom, three different directionalities. A same-level lateral contrast worth isolating: Palestinian citizens of Israel versus West Bank residents share ethnicity, historical grievance, and rough power class, yet differ structurally in exit (citizenship versus occupation) and therefore in derived directionality — proof that exit options, not identity categories, differentiate seats. Coalition check for the powerless seats: refugee, West Bank, and Gaza constituencies lack coalition infrastructure (host-state rivalry, divided governance, blockade-enforced isolation), which is itself a product of the arrangement's divide-and-administer geometry; the framework should register their theoretical coalition power as suppressed rather than absent.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries cluster at the beneficiary pole: the establishment and patrons collect the arrangement's dividends directly (low d despite the establishment's enforcement labor — enforcement cost is borne, but the sovereignty, land-administration, and narrative rents received exceed it); diaspora Jewry's mobile exit keeps it nearest d=0 (an option holder, not a captive supporter); settlers derive subsidy and protection (very low d) with identity lock preventing any exit discount. Declared victims carry high d amplified by trapped exit: multigenerational statelessness, occupation, and blockade place all three territorial payer seats at or near the full-target end. Palestinian citizens of Israel derive intermediate d from their dual declaration (services and franchise received; land, status, and trust extracted). Neighboring Arab states derive mid-to-high d tempered by arbitrage exit. NO DIRECTIONALITY OVERRIDES are authored: the override mechanism keys on the power atom, and this story contains four institutionally powered seats (establishment, patrons, neighbors, legal bodies) with radically different structural relationships — any per-power-atom override would stamp all four with one d and corrupt rather than correct the derivation. Roles plus exit options carry the differentiation the overrides cannot express.
 *
 * MANDATROPHY ANALYSIS:
 *   This is not a classic mandatrophy case: the founding problem (exterminatory persecution of a doorless minority) is live and externally corroborated, so the arrangement's core function has not atrophied into pure ceremony. The mandatrophy-relevant risk is DRIFT-shaped rather than death-shaped: the liberation narrative increasingly performs over an occupation practice it cannot absorb — the theater trajectory (0.05 to 0.58) is the leading indicator, and the persecution premise is progressively invoked ceremonially (each new campaign justified by the founding peril) rather than descriptive of a steady-state function. If the premise decays while enforcement grows, the frame slides toward inertial performance: ceremony substituting for the liberation it commemorates. The classification discipline cuts both ways here: the victim declarations and the enforcement flag prevent the genuine rescue-coordination core from laundering a century of displacement into a certified pure coordination story (no pure rope verdict should survive four trapped payer seats and a 0.83 suppression requirement); conversely, the live founding problem prevents caricaturing a real persecuted-population refuge structure as pure predation. The engine's per-seat computation is what holds both errors off simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story is the national-liberation reading of kernel zionist_legitimacy_basis over one fixed standing arrangement; the settler-colonial sibling authors materially higher epsilon and stronger victim salience over the same referent, and the religious-restoration sibling authors a divine-grant beneficiary topology. Which reading''s characterization tracks the arrangement''s operative structure, and how should cross-reading divergence be weighted?',
    'Comparative per-seat computation across all three sibling files plus historiographic convergence tests: the archival record on 1948, land-transfer and custodianship ledgers, and seat-level exit data.',
    'If the settler-colonial reading computes consistently across seats, this file''s rope claim marks a beneficiary-seat perception rather than operative structure; if seats diverge along the lines modeled here, the kernel contest is real and indexical, and cross-file comparison becomes the primary analysis object.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Indexicality of classification to kernel reading: same referent, three readings, three epsilons.').

omega_variable(
    nakba_attribution_dispute,
    'Were the 1948 displacements predominantly war-initiated flight (this reading''s account) or designed expulsion (the settler-colonial sibling''s account), and in what proportion?',
    'New Historians'' declassified archival work, village-by-village depopulation studies, and IDF archive causation analysis distinguishing flight-under-fire, panic, and explicit expulsion orders.',
    'Expulsion-dominant findings raise epsilon toward sibling levels and dissolve the rope claim even on charitable lights; flight-dominant findings preserve this reading''s framing with epsilon nearer 0.40-0.45. The mix determines whether t=52 is a war cost or a design cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nakba_attribution_dispute, empirical, 'Attribution of the 1948 displacement between flight and expulsion design.').

omega_variable(
    persecution_premise_durability,
    'Is the existential-persecution premise that grounds this reading''s justification still empirically live at current threat levels, or increasingly invoked ceremonially to shield policy from critique?',
    'Threat-assessment data versus rhetorical-frequency analysis across decades; the post-October 7 evidence establishes short-term liveness; cohort tracking tests decay of the premise''s descriptive accuracy over time.',
    'Premise decay drives theater_ratio upward and pushes the frame toward inertial performance; a durably live premise sustains the rescue-coordination claim and keeps the arrangement anchored to its founding function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persecution_premise_durability, empirical, 'Durability versus ceremonial invocation of the founding persecution premise.').

omega_variable(
    mizrahi_exchange_complication,
    'Does the parallel displacement of Jews from Arab and Muslim states after 1948 — with comparable property loss — function as a symmetric population exchange that rebuts the colonial-implantation charge, or as a retrospective balancing device that launders one displacement with another?',
    'Comparative restitution and historiography across both refugee bodies, property registry analysis, and which refugee file negotiators actually traded on in peace processes.',
    'A symmetric-exchange finding lowers this reading''s epsilon and strengthens its coordination core; an asymmetric finding leaves the Palestinian ledger uncompensated, keeps epsilon elevated, and exposes the exchange framing as narrative maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mizrahi_exchange_complication, empirical, 'Whether the Mizrahi refugee stream constitutes symmetry or laundering.').

omega_variable(
    diaspora_exit_heterogeneity,
    'How much of diaspora-jewry attachment is mobile (option-priced support) versus identity-locked (exit into criticism experienced as betrayal), given pronounced generational divergence?',
    'Cohort survey panels measuring attachment intensity, willingness to criticize, aliyah propensity, and defection rates into anti-Zionist identification.',
    'If identity-lock dominates, diaspora directionality rises toward trapped-support behavior — the arrangement extracting unconditional solidarity from a seat the derivation currently reads as mobile; if mobility dominates, support stays conditional and the seat remains near the beneficiary pole.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diaspora_exit_heterogeneity, empirical, 'Mobile versus identity-locked composition of the diaspora beneficiary seat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis__national_liberation_reading, 0, 129).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zion_tr_t0, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(zion_tr_t0, observed).
narrative_ontology:measurement(zion_tr_t20, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 20, 0.07).
narrative_ontology:measurement_basis(zion_tr_t20, observed).
narrative_ontology:measurement(zion_tr_t50, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 50, 0.14).
narrative_ontology:measurement_basis(zion_tr_t50, observed).
narrative_ontology:measurement(zion_tr_t52, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 52, 0.2).
narrative_ontology:measurement_basis(zion_tr_t52, observed).
narrative_ontology:measurement(zion_tr_t70, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 70, 0.26).
narrative_ontology:measurement_basis(zion_tr_t70, observed).
narrative_ontology:measurement(zion_tr_t80, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 80, 0.33).
narrative_ontology:measurement_basis(zion_tr_t80, observed).
narrative_ontology:measurement(zion_tr_t90, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 90, 0.38).
narrative_ontology:measurement_basis(zion_tr_t90, observed).
narrative_ontology:measurement(zion_tr_t103, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 103, 0.44).
narrative_ontology:measurement_basis(zion_tr_t103, observed).
narrative_ontology:measurement(zion_tr_t126, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 126, 0.54).
narrative_ontology:measurement_basis(zion_tr_t126, observed).
narrative_ontology:measurement(zion_tr_t129, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 129, 0.58).
narrative_ontology:measurement_basis(zion_tr_t129, projected).

% Extraction over time
narrative_ontology:measurement(zion_be_t0, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 0, 0.06).
narrative_ontology:measurement_basis(zion_be_t0, observed).
narrative_ontology:measurement(zion_be_t20, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 20, 0.1).
narrative_ontology:measurement_basis(zion_be_t20, observed).
narrative_ontology:measurement(zion_be_t50, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 50, 0.28).
narrative_ontology:measurement_basis(zion_be_t50, observed).
narrative_ontology:measurement(zion_be_t52, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 52, 0.48).
narrative_ontology:measurement_basis(zion_be_t52, observed).
narrative_ontology:measurement(zion_be_t70, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 70, 0.52).
narrative_ontology:measurement_basis(zion_be_t70, observed).
narrative_ontology:measurement(zion_be_t80, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 80, 0.54).
narrative_ontology:measurement_basis(zion_be_t80, observed).
narrative_ontology:measurement(zion_be_t90, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 90, 0.56).
narrative_ontology:measurement_basis(zion_be_t90, observed).
narrative_ontology:measurement(zion_be_t103, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 103, 0.58).
narrative_ontology:measurement_basis(zion_be_t103, observed).
narrative_ontology:measurement(zion_be_t126, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 126, 0.6).
narrative_ontology:measurement_basis(zion_be_t126, observed).
narrative_ontology:measurement(zion_be_t129, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 129, 0.62).
narrative_ontology:measurement_basis(zion_be_t129, projected).

% Suppression requirement over time
narrative_ontology:measurement(zion_su_t0, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 0, 0.04).
narrative_ontology:measurement_basis(zion_su_t0, observed).
narrative_ontology:measurement(zion_su_t20, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 20, 0.16).
narrative_ontology:measurement_basis(zion_su_t20, observed).
narrative_ontology:measurement(zion_su_t50, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 50, 0.4).
narrative_ontology:measurement_basis(zion_su_t50, observed).
narrative_ontology:measurement(zion_su_t52, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 52, 0.58).
narrative_ontology:measurement_basis(zion_su_t52, observed).
narrative_ontology:measurement(zion_su_t70, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 70, 0.62).
narrative_ontology:measurement_basis(zion_su_t70, observed).
narrative_ontology:measurement(zion_su_t80, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 80, 0.67).
narrative_ontology:measurement_basis(zion_su_t80, observed).
narrative_ontology:measurement(zion_su_t90, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 90, 0.71).
narrative_ontology:measurement_basis(zion_su_t90, observed).
narrative_ontology:measurement(zion_su_t103, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 103, 0.74).
narrative_ontology:measurement_basis(zion_su_t103, observed).
narrative_ontology:measurement(zion_su_t126, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 126, 0.81).
narrative_ontology:measurement_basis(zion_su_t126, observed).
narrative_ontology:measurement(zion_su_t129, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 129, 0.83).
narrative_ontology:measurement_basis(zion_su_t129, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis__national_liberation_reading, identity_coordination).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__national_liberation_reading, zionist_legitimacy_basis__settler_colonial_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__national_liberation_reading, zionist_legitimacy_basis__religious_restoration_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Zionism's legitimacy' decomposes (per epsilon-invariance) into three structurally distinct readings of one kernel, each a separate file with its own epsilon, beneficiary topology, and type. This file is the national-liberation reading (claimed rope, moderate epsilon, four victim seats); the settler_colonial_reading is the condemnation frame (highest expected epsilon, identical victim referent); the religious_restoration_reading is the divine-grant frame (different beneficiary structure — covenant rather than persecution — and its own enforcement history). Upstream/downstream: the national-liberation reading is the upstream civic frame whose diplomatic success supplied the legitimacy conditions the religious reading metabolized after 1967, and against which the settler-colonial reading defines its referential opposition. Every file in the family links the other two; orphan stories would break contamination-propagation analysis across the kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
