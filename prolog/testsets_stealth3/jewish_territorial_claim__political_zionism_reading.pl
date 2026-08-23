% ============================================================================
% CONSTRAINT STORY: jewish_territorial_claim__political_zionism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-13
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_territorial_claim__political_zionism_reading, []).

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
 *   constraint_id: jewish_territorial_claim__political_zionism_reading
 *   human_readable: Political Zionism Reading: Majoritarian Territorial Sovereignty as Solution to the Jewish Question
 *   domain: political_history/settler_colonialism/nationalism_studies
 *
 * SUMMARY:
 *   A persecuted, stateless diasporic minority adopts a program declaring
 *   that its insecurity can be ended only by territorial sovereignty
 *   maintained through a durable demographic majority in a specific homeland.
 *   The program coordinates immigration, land assembly, institution-building,
 *   and defense, and succeeds: a state exists. Achieving and keeping the
 *   majority, however, runs through the resident Arab population — the
 *   reading's own structural delta declares that population an obstacle to
 *   the majority condition and accepts population transfer as a necessary
 *   mechanism — so the same structure that rescues one people dispossesses
 *   another. KEY AGENTS (by structural relationship): -
 *   persecuted_diaspora_jewry: rescued constituency (moderate/trapped) —
 *   subsidized seat; - zionist_institutional_leadership: agenda setter
 *   (powerful/identity_locked) — runs the machinery; -
 *   jewish_settler_communities: primary beneficiary (organized/constrained) —
 *   receives land, membership, protection; - palestinian_arab_residents:
 *   primary target (organized/trapped) — bears displacement and occupation; -
 *   palestinian_refugee_diaspora: target with no seat (powerless/trapped) —
 *   bears permanent dispossession; - israeli_arab_citizens: dual-positioned
 *   residual target (moderate/constrained); - western_great_power_patrons:
 *   secondary beneficiary (institutional/arbitrage); -
 *   neighboring_arab_states: regional cost-bearer
 *   (institutional/constrained); - new_historians_archive: analytical
 *   observer. FAMILY NOTE (epsilon-invariance decomposition): the colloquial
 *   label 'the Jewish territorial claim' conflates four structurally distinct
 *   commitments — cultural, labor, political, and revisionist readings of the
 *   same kernel — each instantiated as a separate constraint story with its
 *   own epsilon, victim set, and enforcement profile. This file authors ONLY
 *   the political reading; the sibling files carry the rest, linked via
 *   network edges. The disagreement between readings is routed to omega
 *   variables and kernel_context, not averaged into this constraint's
 *   metrics.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__political_zionism_reading, 0.72).
domain_priors:suppression_score(jewish_territorial_claim__political_zionism_reading, 0.8).
domain_priors:theater_ratio(jewish_territorial_claim__political_zionism_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__political_zionism_reading, tangled_rope).
narrative_ontology:human_readable(jewish_territorial_claim__political_zionism_reading, "Political Zionism Reading: Majoritarian Territorial Sovereignty as Solution to the Jewish Question").
narrative_ontology:topic_domain(jewish_territorial_claim__political_zionism_reading, "political_history/settler_colonialism/nationalism_studies").

domain_priors:requires_active_enforcement(jewish_territorial_claim__political_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__political_zionism_reading, 'ff8ed1e5-923f-468f-af67-5b194391e9a9').
narrative_ontology:cs_kernel_codification('ff8ed1e5-923f-468f-af67-5b194391e9a9', formalized).
narrative_ontology:cs_authority_grounding('ff8ed1e5-923f-468f-af67-5b194391e9a9', lineage).
narrative_ontology:cs_interpretation_layer_present('ff8ed1e5-923f-468f-af67-5b194391e9a9').
narrative_ontology:cs_reading_relation('ff8ed1e5-923f-468f-af67-5b194391e9a9', jewish_territorial_claim__labor_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('ff8ed1e5-923f-468f-af67-5b194391e9a9', jewish_territorial_claim__cultural_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('ff8ed1e5-923f-468f-af67-5b194391e9a9', jewish_territorial_claim__revisionist_zionism_reading, influences).
narrative_ontology:cs_axiom('ff8ed1e5-923f-468f-af67-5b194391e9a9', foundational, statehood_solves_jewish_question).
narrative_ontology:cs_axiom_status(statehood_solves_jewish_question, holdable).
narrative_ontology:cs_axiom_grounding('ff8ed1e5-923f-468f-af67-5b194391e9a9', statehood_solves_jewish_question, empirically_contingent).
narrative_ontology:cs_axiom('ff8ed1e5-923f-468f-af67-5b194391e9a9', foundational, demographic_majority_precondition_of_sovereignty).
narrative_ontology:cs_axiom_status(demographic_majority_precondition_of_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('ff8ed1e5-923f-468f-af67-5b194391e9a9', demographic_majority_precondition_of_sovereignty, conventional).
narrative_ontology:cs_axiom('ff8ed1e5-923f-468f-af67-5b194391e9a9', secondary, population_transfer_legitimate_mechanism).
narrative_ontology:cs_axiom_status(population_transfer_legitimate_mechanism, overridden).
narrative_ontology:cs_axiom_grounding('ff8ed1e5-923f-468f-af67-5b194391e9a9', population_transfer_legitimate_mechanism, conventional).
narrative_ontology:cs_reference_frame('ff8ed1e5-923f-468f-af67-5b194391e9a9', majoritarian_sovereignty_program).
narrative_ontology:cs_drift_state('ff8ed1e5-923f-468f-af67-5b194391e9a9', contemporary_post_statehood_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ff8ed1e5-923f-468f-af67-5b194391e9a9', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__political_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, persecuted_diaspora_jewry).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, jewish_settler_communities).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, western_great_power_patrons).
narrative_ontology:constraint_victim(jewish_territorial_claim__political_zionism_reading, palestinian_arab_residents).
narrative_ontology:constraint_victim(jewish_territorial_claim__political_zionism_reading, palestinian_refugee_diaspora).
narrative_ontology:constraint_victim(jewish_territorial_claim__political_zionism_reading, israeli_arab_citizens).
narrative_ontology:constraint_victim(jewish_territorial_claim__political_zionism_reading, neighboring_arab_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, israeli_arab_citizens).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__political_zionism_reading, majoritarian_self_determination_doctrine).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__political_zionism_reading, diaspora_minority_insecurity_permanence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Lives as a minority under regimes that have periodically stripped rights, expelled, or murdered them. The program offers a destination that converts them from tolerated guests into citizens with sovereign protection and an army answerable to their state. Most cannot relocate elsewhere on comparable terms; for those fleeing immediate violence, the realistic choice set is this program or whatever host governments currently tolerate.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, persecuted_diaspora_jewry, beneficiary,
    moderate, biographical, trapped, global).

% Runs the World Zionist Organization, the Jewish Agency, and after 1948 the state's governing institutions. Sets immigration, land-acquisition, and settlement policy; negotiates treaties with empires and powers; directs the defense establishment. Careers, self-concept, and family legacy are fused with the national project; abandoning it would dissolve the meaning of a lifetime's institutional work.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, zionist_institutional_leadership, agenda_setter,
    powerful, generational, identity_locked, global).

% Immigrate, build farms, towns, and industries, staff the defense forces, and receive land, housing, credit, and citizenship through institutions the movement builds. Gains are anchored in the specific territory; relocating means forfeiting homes, livelihoods, and a society they built, and few destinations offer equivalent membership. They also bury their dead in the wars the arrangement requires.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, jewish_settler_communities, beneficiary,
    organized, generational, constrained, regional).

% Constitute the majority population of the territory when the program begins. They farm land that is purchased, requisitioned, or taken; see several hundred towns and villages depopulated around them in the 1948 war; and those who remain live under military administration and later under occupation. Rooted in villages, orchards, and family graves, they have nowhere to move that restores what they lose. Strikes, revolts, and uprisings are repeatedly defeated by superior organized force.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, palestinian_arab_residents, payer,
    organized, generational, trapped, regional).

% Fled or were driven out in 1948 and 1967 into camps in Lebanon, Syria, Jordan, Gaza, and the West Bank. Most hold no citizenship in host states; the state that replaced their towns denies their return by law. They are absent from every settlement framework that decides their status, and camp residency passes to children and grandchildren.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, palestinian_refugee_diaspora, payer,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__political_zionism_reading, palestinian_refugee_diaspora, excluded).

% Arabs who remained and took citizenship after 1948. They vote, sit in the parliament, and receive state services, but lived under military administration until 1966, lost large village lands to state custodianship, and are subject to loyalty suspicion in a state constitutionally defined as belonging to another nation. Emigration is physically possible but severs family, land, and community.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, israeli_arab_citizens, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__political_zionism_reading, israeli_arab_citizens, beneficiary).

% Britain first, then the United States: gain a loyal strategic anchor, intelligence cooperation, arms-market relationships, and a reliable partner in a volatile region. Support is conditional and redirectable; domestic constituencies reward maintaining it, and the patron can withdraw or rebalance without dissolving its own statehood.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, western_great_power_patrons, beneficiary,
    institutional, generational, arbitrage, global).

% Fight repeated wars against the state, absorb refugee populations whose presence strains their economies and politics, and sign peace treaties that freeze rather than resolve the underlying dispute. Their publics punish leaders seen as conceding the Palestinian question, so even treaty-based exits leave the core grievance intact.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, neighboring_arab_states, payer,
    institutional, generational, constrained, regional).

% Israeli and international scholars who opened the state and movement archives from the late 1980s onward and published the documentary record of 1948 expulsions, transfer deliberations, absentee-property mechanisms, and land transfers. Every other seat argues partly from the evidentiary baseline they produced.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, new_historians_archive, observer,
    moderate, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_territorial_claim__political_zionism_reading, jewish_settler_communities).
narrative_ontology:fixing_cost_class(jewish_territorial_claim__political_zionism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective problem of a dispersed, stateless, periodically massacred minority: pools resources, coordinates immigration, assembles land, builds defense capacity and state institutions, so that physical survival stops depending on the goodwill of whichever host government currently rules.
% TRANSFER_FUNCTION: Moves land, housing, property, and political control from the resident Arab population to the incoming and settled Jewish population; moves persecuted Jews from vulnerable minority positions into sovereign membership; moves strategic anchoring and regional leverage to great-power patrons; moves war costs and refugee burdens onto neighboring states.
% ABSENT_VOICES: Palestinian Arab representatives had no seat at Paris in 1919 where the claim was internationalized, despite the King-Crane Commission recording their overwhelming objection; the refugees of 1948 were absent from the Rhodes armistice architecture; camp descendants are absent from every subsequent settlement conference. They are stateless, in camps, under occupation, or outside the rooms where their status is decided.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, roughly seven million citizens would wake up stateless, the region's densest security architecture would dissolve, the refugee-return question would reopen violently rather than resolve, patrons would lose their anchor, and neighboring states would face immediate border chaos. Enormous numbers of lives, properties, laws, and armed formations are organized around this constraint's persistence.
% FOUNDING_PROBLEM: The Jewish Question: European antisemitism rendered Jewish life structurally insecure wherever Jews lived as a minority, culminating in pogroms and, eventually, the Holocaust. Herzl's diagnosis after the Dreyfus affair was that no degree of emancipation or assimilation removes the vulnerability; only sovereignty does.
% FOUNDING_PROBLEM_CORROBORATION: The problem's reality is corroborated extensively from outside the benefiting parties: the King-Crane Commission records, British government assessments, postwar Allied documentation of the Holocaust, UNSCOP testimony, and contemporary non-Jewish monitoring bodies all attest persistent, sometimes lethal, antisemitic persecution. What no outside party attests is that majoritarian territorial sovereignty is the uniquely necessary solution — that step rests on the reading's own premises and is disputed by the sibling readings and by binational and rights-based proposals.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__political_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__political_zionism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__political_zionism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_territorial_claim__political_zionism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_territorial_claim__political_zionism_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_territorial_claim__political_zionism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_territorial_claim__political_zionism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_territorial_claim__political_zionism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.72: the referent is the standing arrangement under contest — the pursuit and maintenance of majoritarian territorial sovereignty as this reading pursues it — scored by this reading's own lights. The reading does not hide the cost; its own delta names the Arab population as the demographic obstacle and accepts transfer as the necessary mechanism. Even granting the rescue framing in full, the arrangement converted a resident majority into a displaced minority, transferred their land and property, and maintains the result by law and force: the extraction is constitutive of the majoritarian premise, not an accident of execution. The series peaks at 1948 (mass displacement) and declines gradually as acute transformation settles into administered control. Suppression 0.80 is authored as a raw, unscaled structural property — enforcement against revolt and uprising, legal closure of refugee return, military rule, occupation administration — and is deliberately NOT tuned against the scaled extraction figure. Theater_ratio 0.25: core functions (immigration absorption, defense, state services) are real and load-bearing; a growing minority of activity is legitimation and public diplomacy defending the arrangement's reputation. Accessibility_collapse 0.55: within the majoritarian logic, once accepted, alternatives collapse sharply — but binational, one-state, and return frameworks persist as live external alternatives, so collapse is substantial yet incomplete. Resistance 0.75: revolts (1936-39), interstate wars, intifadas, boycott campaigns, and international litigation meet this constraint continuously. Measurement discipline: all three tracked metrics run on one shared ten-point grid (1897-2025) with every metric authored at every point; the trajectory is shock-driven (step changes at 1936, 1948, 1967, 1987, 2000) rather than cyclically oscillating, so no intermittent-reinforcement cycle is posited. Claim/metric independence: claimed_type tangled_rope is my structural judgment — a genuine, historically validated rescue-and-sovereignty coordination function bound to asymmetric extraction through the same land-and-membership machinery, actively enforced — asserted independently of the metric values above; the engine computes per-seat classifications from the structural data and any divergence from my claim is the datum the corpus exists to take.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the leadership seat the structure is a coordination machine it built and staffed: congresses, agencies, absorption ministries — a rescue operation with a price. From the resident and refugee seats the identical machinery operates as enforced dispossession: the land registry, the custodian of absentee property, the military governor, the denial-of-return law. The patron seat computes a cheap, redirectable subsidy; the neighbor seats pay recurring war costs with state-backed exits short of exit from the dispute. The engine derives these divergent classifications from power atoms, exit options, and directionalities — the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: persecuted_diaspora_jewry (d near the beneficiary end — the arrangement subsidizes them with refuge and membership; their trapped exit reflects the persecution that generates demand, not extraction borne); jewish_settler_communities (damped further — direct recipients of land, credit, and membership); western_great_power_patrons (near-zero effective cost, arbitrage-grade exit keeps them nearest the beneficiary pole). Targets: palestinian_arab_residents and palestinian_refugee_diaspora (d near the full-target end — trapped or stateless exit pushes them to the maximum-amplification side); israeli_arab_citizens (dual-positioned: formal membership dampens, land loss and second-class status raise — net target); neighboring_arab_states (partial target — war and refugee costs, but statehood preserves partial insulation). No directionality_overrides were needed: the beneficiary/victim declarations plus exit options reproduce these relationships without correction, and the override surface keys on power atoms too coarsely to improve on the structural derivation here.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: antisemitic persecution predates the program, was catastrophically validated by the Holocaust, and persists in documented episodes after statehood. Because founding_problem_status is 'live' and disappearance_verdict is 'world_rearranges', the mismatch consumer finds no dead-mandate/zombie signature, and mandatrophy is correctly NOT declared. The classification work this story performs cuts both ways: it prevents the pure-snare mislabel (which would erase the real, historically corroborated rescue function that saved and shelters a persecuted people) and prevents the pure-rope mislabel (which would erase the constitutive extraction the reading's own transfer mechanism visits on the resident population). The tangled-rope verdict holds both truths in one structure: the same land-and-membership machinery that coordinates rescue extracts territory and permanence from the people who were there first.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_decomposition,
    'This story instantiates the political_zionism_reading of the kernel jewish_territorial_claim: would the labor, cultural, or revisionist sibling readings, instantiated over the same referent, produce materially different victim sets, enforcement requirements, or classifications?',
    'Compile the three sibling stories and compare computed per-seat classifications and effective extraction over the fixed referent; differences localize which structural element (majoritarian precondition, settlement method, territorial extent) carries the extraction.',
    'If extraction tracks the majoritarian premise specifically (political and revisionist high, cultural low), the measured cost is attributed to this reading''s distinguishing axiom rather than to the kernel as such; if all siblings converge high, the kernel itself is the extractive element.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_decomposition, conceptual, 'Committer-frame decomposition uncertainty: which reading of the territorial-claim kernel carries the measured extraction.').

omega_variable(
    transfer_structural_necessity,
    'Was the demographic transformation this reading requires achievable without coerced transfer — could voluntary purchase and immigration alone have produced the majority — or does the majoritarian premise make displacement structurally necessary given the resident Arab majority?',
    'Counterfactual reconstruction against the historical ceiling: after fifty years of legal purchase and waves of immigration, Jews reached roughly one-third of mandatory Palestine''s population by 1947; combine purchase-record analysis, immigration-capacity studies, and leadership deliberation archives (including the Peel-era transfer discussions) to test whether any voluntary path to majority existed.',
    'If transfer is structurally necessary to the majoritarian premise, the extraction is constitutive of this reading and its classification hardens toward the extractive end of the hybrid band; if a plausible voluntary path existed and was forgone, extraction is partly a chosen instrument rather than a logical consequence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transfer_structural_necessity, conceptual, 'Whether displacement is a necessary consequence of the majority requirement or an optional instrument.').

omega_variable(
    coordination_extraction_separability,
    'Is the rescue function — sovereign refuge for a persecutable minority — separable from the majoritarian-demographic mechanism, i.e., could persecution-scale refuge have been delivered under binational or minority-rights arrangements?',
    'Assess the operational record of the binational alternatives (Brit Shalom, the 1947 federal and cantonal proposals): their absorption capacity, their survivability under regional rejection, and whether any delivered or could have delivered refuge at Holocaust-and-after scale.',
    'If inseparable, part of the measured extraction is the unavoidable price of the coordination itself and belongs below the Boltzmann floor as coordination cost; if separable, the excess is extractive overhead riding on the rescue function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the rescue coordination and the demographic extraction are structurally separable components.').

omega_variable(
    founding_problem_solution_efficacy,
    'Does sovereignty-with-majority actually reduce the insecurity it targets — does statehood lower antisemitic threat to Jewish life relative to diaspora baselines — or does it relocate and reproduce the insecurity as permanent regional conflict?',
    'Longitudinal comparison of casualty, displacement, and discrimination outcomes for sovereign versus diaspora Jewish populations since 1948, controlling for the Holocaust shock and regional war exposure; supplement with trend data on post-statehood antisemitic incidents globally.',
    'Demonstrated efficacy supports the coordination-first reading of the structure; demonstrated relocation-or-reproduction would deepen the axiom_overriding drift recorded in cs_structure.drift_state and push reassessment toward inertial or theatrical maintenance of the original premise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_solution_efficacy, empirical, 'Whether the arrangement delivers the security its founding problem demands.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__political_zionism_reading, 1897, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1897, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1897, 0.26).
narrative_ontology:measurement_basis(jewi_tr_t1897, observed).
narrative_ontology:measurement(jewi_tr_t1917, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1917, 0.22).
narrative_ontology:measurement_basis(jewi_tr_t1917, observed).
narrative_ontology:measurement(jewi_tr_t1929, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1929, 0.17).
narrative_ontology:measurement_basis(jewi_tr_t1929, observed).
narrative_ontology:measurement(jewi_tr_t1936, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1936, 0.21).
narrative_ontology:measurement_basis(jewi_tr_t1936, observed).
narrative_ontology:measurement(jewi_tr_t1948, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement_basis(jewi_tr_t1948, observed).
narrative_ontology:measurement(jewi_tr_t1950, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1950, 0.16).
narrative_ontology:measurement_basis(jewi_tr_t1950, observed).
narrative_ontology:measurement(jewi_tr_t1967, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1967, 0.19).
narrative_ontology:measurement_basis(jewi_tr_t1967, observed).
narrative_ontology:measurement(jewi_tr_t1987, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1987, 0.27).
narrative_ontology:measurement_basis(jewi_tr_t1987, observed).
narrative_ontology:measurement(jewi_tr_t2000, jewish_territorial_claim__political_zionism_reading, theater_ratio, 2000, 0.29).
narrative_ontology:measurement_basis(jewi_tr_t2000, observed).
narrative_ontology:measurement(jewi_tr_t2025, jewish_territorial_claim__political_zionism_reading, theater_ratio, 2025, 0.25).
narrative_ontology:measurement_basis(jewi_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1897, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1897, 0.38).
narrative_ontology:measurement_basis(jewi_be_t1897, observed).
narrative_ontology:measurement(jewi_be_t1917, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1917, 0.46).
narrative_ontology:measurement_basis(jewi_be_t1917, observed).
narrative_ontology:measurement(jewi_be_t1929, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1929, 0.54).
narrative_ontology:measurement_basis(jewi_be_t1929, observed).
narrative_ontology:measurement(jewi_be_t1936, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1936, 0.63).
narrative_ontology:measurement_basis(jewi_be_t1936, observed).
narrative_ontology:measurement(jewi_be_t1948, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1948, 0.88).
narrative_ontology:measurement_basis(jewi_be_t1948, observed).
narrative_ontology:measurement(jewi_be_t1950, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1950, 0.84).
narrative_ontology:measurement_basis(jewi_be_t1950, observed).
narrative_ontology:measurement(jewi_be_t1967, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1967, 0.81).
narrative_ontology:measurement_basis(jewi_be_t1967, observed).
narrative_ontology:measurement(jewi_be_t1987, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1987, 0.77).
narrative_ontology:measurement_basis(jewi_be_t1987, observed).
narrative_ontology:measurement(jewi_be_t2000, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 2000, 0.75).
narrative_ontology:measurement_basis(jewi_be_t2000, observed).
narrative_ontology:measurement(jewi_be_t2025, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 2025, 0.72).
narrative_ontology:measurement_basis(jewi_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1897, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1897, 0.15).
narrative_ontology:measurement_basis(jewi_su_t1897, observed).
narrative_ontology:measurement(jewi_su_t1917, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1917, 0.24).
narrative_ontology:measurement_basis(jewi_su_t1917, observed).
narrative_ontology:measurement(jewi_su_t1929, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1929, 0.34).
narrative_ontology:measurement_basis(jewi_su_t1929, observed).
narrative_ontology:measurement(jewi_su_t1936, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1936, 0.56).
narrative_ontology:measurement_basis(jewi_su_t1936, observed).
narrative_ontology:measurement(jewi_su_t1948, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1948, 0.92).
narrative_ontology:measurement_basis(jewi_su_t1948, observed).
narrative_ontology:measurement(jewi_su_t1950, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1950, 0.68).
narrative_ontology:measurement_basis(jewi_su_t1950, observed).
narrative_ontology:measurement(jewi_su_t1967, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1967, 0.74).
narrative_ontology:measurement_basis(jewi_su_t1967, observed).
narrative_ontology:measurement(jewi_su_t1987, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1987, 0.8).
narrative_ontology:measurement_basis(jewi_su_t1987, observed).
narrative_ontology:measurement(jewi_su_t2000, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 2000, 0.85).
narrative_ontology:measurement_basis(jewi_su_t2000, observed).
narrative_ontology:measurement(jewi_su_t2025, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 2025, 0.8).
narrative_ontology:measurement_basis(jewi_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__political_zionism_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, jewish_territorial_claim__labor_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, jewish_territorial_claim__cultural_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, jewish_territorial_claim__revisionist_zionism_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the kernel jewish_territorial_claim per the epsilon-invariance principle. The colloquial label 'Zionism'/'the Jewish territorial claim' covers four structurally distinct commitments with different epsilons, victim sets, and enforcement requirements: cultural (spiritual center; lowest extraction; no majority requirement), labor (settlement socialism; extraction via labor-market and land facts on the ground), political (THIS FILE — majoritarian sovereignty; constitutive extraction via the transfer mechanism), revisionist (maximalist borders; highest enforcement demand via the Iron Wall). Upstream/downstream: the cultural reading precedes and legitimizes the kernel; the political reading institutionalized it into statehood and is therefore the node the others cite; the revisionist reading arose as reaction to political gradualism and pressures it militarily. Every member links the family via network.affects_constraints; orphan stories would break contamination-propagation analysis across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
