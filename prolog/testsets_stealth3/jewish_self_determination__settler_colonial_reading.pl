% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__settler_colonial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__settler_colonial_reading, []).

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
 *   constraint_id: jewish_self_determination__settler_colonial_reading
 *   human_readable: Settler-Colonial Reading: The Zionist State-Territorial Arrangement
 *   domain: political philosophy/nationalism studies/postcolonial theory
 *
 * SUMMARY:
 *   Under the settler-colonial reading, the standing arrangement under
 *   contest is the state-territorial order built by political Zionism from
 *   1897 onward and consolidated in 1948 and 1967: a European-origin national
 *   movement established sovereignty in a populated country, displaced most
 *   of its Arab inhabitants in 1948, and has since maintained a
 *   differentiated legal regime over the remainder - military law for
 *   Palestinians, civilian law for settlers, immediate citizenship for Jews
 *   worldwide, and barred return for Palestinian refugees. This story
 *   instantiates ONE reading of the jewish_self_determination kernel (see
 *   commentary.kernel_context); the sibling readings are separate constraints
 *   with their own epsilon, beneficiary, and victim structures, linked
 *   through network.affects_constraints. The epsilon referent is the standing
 *   arrangement itself, assessed by this reading's own lights - never the
 *   arrangement this reading would put in its place, which is why epsilon
 *   stays high rather than collapsing toward zero.
 *
 * KEY AGENTS:
 *   - - israeli_state_apparatus: Agenda setter ([institutional]/[arbitrage]) - authors and administers the legal architecture, collects custody of transferred assets
 *   - - jewish_israeli_citizenry: Primary beneficiary ([organized]/[identity_locked]) - receives rights, land priority, and defense
 *   - - west_bank_settlement_enterprise: Concentrated beneficiary ([powerful]/[identity_locked]) - the expanding edge of the arrangement
 *   - - palestinian_citizens_of_israel: Target ([moderate]/[constrained]) - formal inclusion, structural subordination
 *   - - occupied_west_bank_palestinians: Primary target ([powerless]/[trapped]) - dual legal system, no vote in the governing state
 *   - - gaza_strip_palestinians: Primary target ([powerless]/[trapped]) - blockaded enclave, collapsed planning horizon
 *   - - palestinian_refugees_diaspora: Excluded target ([powerless]/[trapped]) - denied return by the same order that ingathers Jews worldwide
 *   - - western_patron_states: Enabling beneficiary ([institutional]/[mobile]) - funds and shields, revocably
 *   - - palestinian_authority_officials: Excluded negotiator ([moderate]/[constrained]) - talks without agenda power over the decisive structures
 *   - - international_legal_institutions: Analytical observer ([institutional]/[analytical]) - documents the structure, cannot enforce
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__settler_colonial_reading, 0.86).
domain_priors:suppression_score(jewish_self_determination__settler_colonial_reading, 0.88).
domain_priors:theater_ratio(jewish_self_determination__settler_colonial_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, extractiveness, 0.86).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__settler_colonial_reading, snare).
narrative_ontology:human_readable(jewish_self_determination__settler_colonial_reading, "Settler-Colonial Reading: The Zionist State-Territorial Arrangement").
narrative_ontology:topic_domain(jewish_self_determination__settler_colonial_reading, "political philosophy/nationalism studies/postcolonial theory").

domain_priors:requires_active_enforcement(jewish_self_determination__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__settler_colonial_reading, '5eb605f4-36fd-4412-9208-eb4e5aa38ba8').
narrative_ontology:cs_kernel_codification('5eb605f4-36fd-4412-9208-eb4e5aa38ba8', distributed).
narrative_ontology:cs_authority_grounding('5eb605f4-36fd-4412-9208-eb4e5aa38ba8', distributed).
narrative_ontology:cs_reading_relation('5eb605f4-36fd-4412-9208-eb4e5aa38ba8', jewish_self_determination__indigenous_return_reading, forecloses).
narrative_ontology:cs_reading_relation('5eb605f4-36fd-4412-9208-eb4e5aa38ba8', jewish_self_determination__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('5eb605f4-36fd-4412-9208-eb4e5aa38ba8', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('5eb605f4-36fd-4412-9208-eb4e5aa38ba8', jewish_self_determination__diasporist_reading, coexists_with).
narrative_ontology:cs_axiom('5eb605f4-36fd-4412-9208-eb4e5aa38ba8', foundational, zionism_constitutes_settler_colonization).
narrative_ontology:cs_axiom_status(zionism_constitutes_settler_colonization, holdable).
narrative_ontology:cs_axiom_grounding('5eb605f4-36fd-4412-9208-eb4e5aa38ba8', zionism_constitutes_settler_colonization, empirically_contingent).
narrative_ontology:cs_axiom('5eb605f4-36fd-4412-9208-eb4e5aa38ba8', secondary, dispossession_requires_decolonial_remedy).
narrative_ontology:cs_axiom_status(dispossession_requires_decolonial_remedy, holdable).
narrative_ontology:cs_axiom_grounding('5eb605f4-36fd-4412-9208-eb4e5aa38ba8', dispossession_requires_decolonial_remedy, instrumental).
narrative_ontology:cs_reference_frame('5eb605f4-36fd-4412-9208-eb4e5aa38ba8', european_settler_colonial_frame).
narrative_ontology:cs_drift_state('5eb605f4-36fd-4412-9208-eb4e5aa38ba8', contemporary_post_icj_advisory_opinions, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('5eb605f4-36fd-4412-9208-eb4e5aa38ba8', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__settler_colonial_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, israeli_state_apparatus).
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, jewish_israeli_citizenry).
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, west_bank_settlement_enterprise).
narrative_ontology:constraint_victim(jewish_self_determination__settler_colonial_reading, palestinian_citizens_of_israel).
narrative_ontology:constraint_victim(jewish_self_determination__settler_colonial_reading, occupied_west_bank_palestinians).
narrative_ontology:constraint_victim(jewish_self_determination__settler_colonial_reading, gaza_strip_palestinians).
narrative_ontology:constraint_victim(jewish_self_determination__settler_colonial_reading, palestinian_refugees_diaspora).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, western_patron_states).
narrative_ontology:constraint_vindicates(jewish_self_determination__settler_colonial_reading, comparative_settler_colonial_theory).
narrative_ontology:constraint_vindicates(jewish_self_determination__settler_colonial_reading, apartheid_framework_analysis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authors and administers the governing legal architecture: the Law of Return grants immediate citizenship to any Jew worldwide while the Absentees' Property Law and land-authority arrangements place refugee land under state custody; military orders, permit regimes, and planning bodies govern the West Bank. Allocates land, water, and budgets between the populations under its control, and can amend Basic Laws, extend jurisdiction, or annex by internal act. Collects the custody and revenue flows the arrangement generates.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, israeli_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__settler_colonial_reading, israeli_state_apparatus, beneficiary).

% Roughly seven million Jewish citizens hold full civil and political rights, preferential access to state and national-fund land, first-priority housing and infrastructure investment, and security services staffed in their defense. Most are native-born; national identity and the state's continuation are fused for the majority, so emigration is individually available but means leaving the only polity constituted around their collective membership.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, jewish_israeli_citizenry, beneficiary,
    organized, generational, identity_locked, regional).

% Around half a million residents beyond the Green Line plus East Jerusalem neighborhoods live under civilian Israeli law while neighboring Palestinians live under military orders. Receive subsidized housing, bypass-road infrastructure, and per-capita water allocations well above neighboring levels; construction continues outward each year. The ideological core treats any withdrawal as betrayal, and the built footprint is designed to be difficult to reverse.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, west_bank_settlement_enterprise, beneficiary,
    powerful, biographical, identity_locked, regional).

% About 1.7 million citizens, roughly a fifth of the population, descend mostly from Palestinians who remained through 1948, many of them internally displaced from destroyed villages. They vote and hold office but face unrecognized villages, planning refusals, historically confiscatory land regimes, and persistently lower budget allocations. Emigration is individually possible but dissolves the very community whose standing they would be defending.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, palestinian_citizens_of_israel, payer,
    moderate, generational, constrained, regional).

% About three million people governed by military administration: military orders apply to them while adjacent settlers are tried under civilian law. Homes are demolished for lacking permits the planning system rarely grants in Area C; movement runs through checkpoints and a permit regime; administrative detention is available without charge. They do not vote in the state that controls most of the territory they live on.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, occupied_west_bank_palestinians, payer,
    powerless, biographical, trapped, regional).

% About two million people in a blockaded strip: land, sea, and air access controlled from outside since 2007, fishing limited to a narrow zone, construction materials and dual-use goods restricted. Successive large-scale military campaigns have destroyed housing, water, and power infrastructure faster than it can be rebuilt. Borders are closed to ordinary exit; the operative planning horizon is daily subsistence.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, gaza_strip_palestinians, payer,
    powerless, immediate, trapped, regional).

% About 5.9 million UNRWA-registered refugees across Lebanon, Syria, Jordan, the West Bank, and Gaza, plus a wider diaspora. The same legal order that grants any Jew worldwide immediate citizenship bars their return to homes within living memory; property claims sit frozen under custodian statutes. Host-state policies in places like Lebanon withhold citizenship across generations, keeping the population stateless and immobile.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, palestinian_refugees_diaspora, payer,
    powerless, generational, trapped, global).

% Led by the United States with European states alongside: supply military assistance, intelligence cooperation, trade integration, and diplomatic shielding, including Security Council vetoes. Receive strategic anchoring in the eastern Mediterranean, arms-industry circulation, and domestic coalition benefits. Their support is discretionary and revocable, which is why sustaining it is a standing requirement the arrangement must continuously meet.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, western_patron_states, beneficiary,
    institutional, generational, mobile, global).

% Administer civil affairs in fragments of the West Bank under interim arrangements now three decades old. Negotiate on behalf of constituencies whose central grievances - Jerusalem, refugees, settlements - lie outside what they control; security coordination binds them against parts of their own constituency. Hold no agenda-setting power over the structures that decide the territory's disposition, and their consent is not required for decisions taken about them.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, palestinian_authority_officials, excluded,
    moderate, biographical, constrained, regional).

% The International Court of Justice, the International Criminal Court, and UN treaty bodies issue advisory opinions, open investigations, and findings on the arrangement's structure - the wall opinion, the 2024 occupation-legality opinion, apartheid inquiries. They document from outside and carry no independent enforcement power; their output shapes legitimacy costs rather than facts on the ground.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, international_legal_institutions, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__settler_colonial_reading, israeli_state_apparatus).
narrative_ontology:fixing_cost_class(jewish_self_determination__settler_colonial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the ingathering, settlement, and collective defense of a Jewish national population: immigration absorption, land allocation, national institutions, and unified security provision are solved once, centrally, for the insider population.
% TRANSFER_FUNCTION: Moves land, water, housing stock, movable property, labor market access, and ultimate political authority from Palestinian Arabs to the Jewish-Israeli collective and its state apparatus; simultaneously moves diaspora Jewish immigration inward under automatic citizenship while barring reverse movement for displaced Palestinians.
% ABSENT_VOICES: The displaced and killed of 1948 and their descendants - the refugees in camps and exile who were never consulted about the state built on their property and whose return the legal order forbids; Palestinians under occupation who vote in neither the state that governs most of their territory nor an effective sovereign of their own; the internally displaced second-class of 1948 inside Israel. They are outside the room in which the arrangement's terms are set, and their absence is maintained by the same legal architecture the arrangement runs on.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, the rearrangement would be total: millions of refugees would press immediate return claims to specific properties; the settlement enterprise, land custodianship, and the citizenship asymmetry would dissolve together; water, budgets, and jurisdiction would be renegotiated from zero; regional alliances, patron-state commitments, and diaspora politics would all reorganize around the new fact. Nothing about the current distribution survives the arrangement's removal.
% FOUNDING_PROBLEM: Securing a small, persecuted, stateless minority against European antisemitism that culminated in attempted annihilation - the search for a guaranteed collective refuge and self-governance for a people with nowhere that was safe.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem itself is corroborated from entirely outside the benefiting parties: the European documentary record of pogroms, discriminatory statutes, and the Holocaust; testimony of persecuted Middle Eastern and North African Jewish communities; and mainstream historiography none of which depends on the arrangement's defenders. What is contested is the status: the state and its patrons attest the problem remains live and that the arrangement is its necessary answer, while Palestinian historians, the sibling diasporist reading, and much of the international legal record attest that the remedy chosen manufactured a new founding problem - dispossession - without extinguishing the original insecurity. No neutral seat attests that the standing arrangement uniquely solves the founding problem.
narrative_ontology:disappearance_verdict(jewish_self_determination__settler_colonial_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__settler_colonial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__settler_colonial_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_self_determination__settler_colonial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__settler_colonial_reading, 0.86, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__settler_colonial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__settler_colonial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_self_determination__settler_colonial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.86) because the transfer is structural and continuing: land custodianship, settlement expansion, water allocation ratios, and the return asymmetry operate every year the arrangement persists, not as legacy cost alone. Suppression (0.88) is a raw structural property - unscaled by power or scope - reflecting the enforcement stack the arrangement requires: military administration, permit regimes, blockade, demolition, administrative detention. Theater ratio (0.40) is moderate: the arrangement performs negotiation-readiness and democratic self-description while built facts expand; the 1993 theater spike marks the Oslo decade, which this reading interprets as process substituting for sovereignty while settler numbers roughly doubled. Accessibility collapse (0.62): alternatives are heavily narrowed - return legally barred, armed channels criminalized, boycott activity legislated against inside patron states - yet not annihilated, since litigation, UN procedure, and civil-society mobilization remain open and costly. Resistance (0.78) is among the highest sustained in the modern record: intifadas, mass protest, legal challenge, and armed and unarmed repertoires alike. The three measurement series share one eight-point grid (1897-2025); the 1948 step-change and the 1993 Oslo dip are the two inflections. Coalition potential across the four Palestinian seats is deliberately fragmented by legal status - citizens, occupied, blockaded, and exiled each face different jurisdictions and different permissible repertoires - and that fragmentation is itself part of the suppression picture rather than an accident.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats the same structure computes as genuine coordination: ingathering, defense, membership, housing - real goods delivered reliably, which is why the arrangement commands mass attachment and cannot be dismissed as mere racket. From the payer seats the identical structure computes as enforced dispossession: the same Law of Return that ingathers one population bars another's return; the same planning system builds for one population and demolishes for the other. The engine computes per-seat classifications from the structural data; the divergence between those computations is the finding, not an inconsistency to be reconciled away.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations drive the three insider seats toward the low-d end: the state apparatus sits nearest zero (it writes the rules and collects the custody flows), with the citizenry and settlement enterprise close behind; their identity_locked exits pin them in place rather than raising d - lock-in stabilizes their position, it does not tax it. The four Palestinian seats derive high d from victim declaration compounded by trapped or constrained exits: trapped targets sit nearer the full-target end than mobile ones, and the refugee seat is the extreme case, with exit barred in both directions. Western patron states derive moderately low d from their beneficiary role but remain mobile, so their support is conditional by construction. International legal institutions occupy the analytical seat with no directional stake in the arrangement's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - securing a persecuted, stateless minority against European annihilation - was real, externally corroborated, and urgent; the arrangement built for it delivered genuine refuge, which is exactly what makes the structure stable and what a pure-extraction label would miss. But the mandate never carried a sunset: what began as rescue hardened into permanent territorial maintenance, and the mechanism of delivery became the mechanism of dispossession. Classifying this as a snare rather than a rope keeps the asymmetric transfer visible; recording the beneficiaries' real gains keeps the account honest about why millions defend it. The mandatrophy question - is the founding problem still served by the standing arrangement? - is precisely what the R5 mismatch consumer checks: the problem's status is contested, the arrangement is unconditional, and the gap between those two facts is where capture lives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the jewish_self_determination kernel - what would each sibling reading change structurally if adopted?',
    'Cross-reading comparison across the constraint family: instantiate each sibling as its own story and diff the beneficiary/victim sets, epsilon, and computed types against this file.',
    'The indigenous_return_reading inverts the valence entirely - same referent arrangement, opposite assignment of who is indigenous and who is the arriving population, flipping every seat''s directionality. The liberal_nationalist_reading lowers epsilon by legitimating the state form while conceding equal-rights defects. The diasporist_reading removes the territorial referent altogether. Classification of THIS file is valid only within this reading''s frame.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: which kernel, which reading, and what each sibling reading would structurally change.').

omega_variable(
    settler_frame_completeness,
    'Does the settler-colonial frame fully describe the arrangement, or does it partially misdescribe the origin populations - Holocaust survivors and Jews expelled from Middle Eastern and North African countries whose migration was flight rather than colonial venture?',
    'Wave-by-wave migration-motive analysis: demographic and archival study of each aliyah''s composition and drivers, distinguishing metropolitan-directed settlement from refugee flight.',
    'If substantial shares arrived as refugees rather than settlers, the ''European settler'' characterization narrows chronologically and demographically, weakening the frame''s intent attribution - without changing the structural outcome for Palestinians, since the destination population''s fate is unaffected by the migrants'' motives. The frame would shift from describing the actors to describing the structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settler_frame_completeness, conceptual, 'Whether the settler-colonial analytic is complete or partially misdescribes the migrating population''s composition.').

omega_variable(
    dispossession_intentionality,
    'Was the 1948 dispossession executed according to pre-war transfer planning (Transfer Committee records, Plan Dalet''s provisions) or did it emerge contingently from war conditions?',
    'Archival historiography: the New Historians'' document work against their critics, captured Arabic and Israeli archives, village-level reconstruction of expulsion versus flight sequences.',
    'A systematic-intent finding supports snare-by-design genealogy; a contingent finding supports a tangled-rope-gone-wrong genealogy with an identical present structure. The present-day classification barely moves either way - the enforcement requirements and victim sets are the same - but the persistence mechanism differs: design implies replacement of intent across leadership generations, contingency implies path dependence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dispossession_intentionality, empirical, 'Whether the founding dispossession was planned or emergent - the genealogical fork beneath the same present structure.').

omega_variable(
    remedy_path_contestation,
    'Is the arrangement correctable through partition-equalization (two sovereign states) or only through decolonial restructuring (equal rights on the whole territory, refugee return)?',
    'None empirical - this is resolved politically, by which remedy coalitions can assemble and sustain; track feasibility signals (annexation momentum, patron-state positioning, demographic trajectories) without pretending they settle the normative question.',
    'Determines whether the authored fixing_cost of ''prohibitive'' reflects genuine impossibility or accumulated unwillingness, and changes which seats count as agenda-setters for remedy: under partition, the two state-apparatuses; under decolonial restructuring, the whole enfranchised population. The classification of the standing arrangement is unchanged; the exit landscape of every seat is not.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(remedy_path_contestation, preference, 'Which remedy frame applies - a values-and-power question routed through the omega apparatus rather than settled by metrics.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__settler_colonial_reading, 1897, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1897, jewish_self_determination__settler_colonial_reading, theater_ratio, 1897, 0.1).
narrative_ontology:measurement_basis(jewi_tr_t1897, observed).
narrative_ontology:measurement(jewi_tr_t1922, jewish_self_determination__settler_colonial_reading, theater_ratio, 1922, 0.16).
narrative_ontology:measurement_basis(jewi_tr_t1922, observed).
narrative_ontology:measurement(jewi_tr_t1948, jewish_self_determination__settler_colonial_reading, theater_ratio, 1948, 0.12).
narrative_ontology:measurement_basis(jewi_tr_t1948, observed).
narrative_ontology:measurement(jewi_tr_t1967, jewish_self_determination__settler_colonial_reading, theater_ratio, 1967, 0.18).
narrative_ontology:measurement_basis(jewi_tr_t1967, observed).
narrative_ontology:measurement(jewi_tr_t1987, jewish_self_determination__settler_colonial_reading, theater_ratio, 1987, 0.22).
narrative_ontology:measurement_basis(jewi_tr_t1987, observed).
narrative_ontology:measurement(jewi_tr_t1993, jewish_self_determination__settler_colonial_reading, theater_ratio, 1993, 0.46).
narrative_ontology:measurement_basis(jewi_tr_t1993, observed).
narrative_ontology:measurement(jewi_tr_t2005, jewish_self_determination__settler_colonial_reading, theater_ratio, 2005, 0.34).
narrative_ontology:measurement_basis(jewi_tr_t2005, observed).
narrative_ontology:measurement(jewi_tr_t2025, jewish_self_determination__settler_colonial_reading, theater_ratio, 2025, 0.4).
narrative_ontology:measurement_basis(jewi_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1897, jewish_self_determination__settler_colonial_reading, base_extractiveness, 1897, 0.28).
narrative_ontology:measurement_basis(jewi_be_t1897, observed).
narrative_ontology:measurement(jewi_be_t1922, jewish_self_determination__settler_colonial_reading, base_extractiveness, 1922, 0.36).
narrative_ontology:measurement_basis(jewi_be_t1922, observed).
narrative_ontology:measurement(jewi_be_t1948, jewish_self_determination__settler_colonial_reading, base_extractiveness, 1948, 0.76).
narrative_ontology:measurement_basis(jewi_be_t1948, observed).
narrative_ontology:measurement(jewi_be_t1967, jewish_self_determination__settler_colonial_reading, base_extractiveness, 1967, 0.79).
narrative_ontology:measurement_basis(jewi_be_t1967, observed).
narrative_ontology:measurement(jewi_be_t1987, jewish_self_determination__settler_colonial_reading, base_extractiveness, 1987, 0.81).
narrative_ontology:measurement_basis(jewi_be_t1987, observed).
narrative_ontology:measurement(jewi_be_t1993, jewish_self_determination__settler_colonial_reading, base_extractiveness, 1993, 0.73).
narrative_ontology:measurement_basis(jewi_be_t1993, observed).
narrative_ontology:measurement(jewi_be_t2005, jewish_self_determination__settler_colonial_reading, base_extractiveness, 2005, 0.83).
narrative_ontology:measurement_basis(jewi_be_t2005, observed).
narrative_ontology:measurement(jewi_be_t2025, jewish_self_determination__settler_colonial_reading, base_extractiveness, 2025, 0.86).
narrative_ontology:measurement_basis(jewi_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1897, jewish_self_determination__settler_colonial_reading, suppression_requirement, 1897, 0.18).
narrative_ontology:measurement_basis(jewi_su_t1897, observed).
narrative_ontology:measurement(jewi_su_t1922, jewish_self_determination__settler_colonial_reading, suppression_requirement, 1922, 0.28).
narrative_ontology:measurement_basis(jewi_su_t1922, observed).
narrative_ontology:measurement(jewi_su_t1948, jewish_self_determination__settler_colonial_reading, suppression_requirement, 1948, 0.68).
narrative_ontology:measurement_basis(jewi_su_t1948, observed).
narrative_ontology:measurement(jewi_su_t1967, jewish_self_determination__settler_colonial_reading, suppression_requirement, 1967, 0.71).
narrative_ontology:measurement_basis(jewi_su_t1967, observed).
narrative_ontology:measurement(jewi_su_t1987, jewish_self_determination__settler_colonial_reading, suppression_requirement, 1987, 0.77).
narrative_ontology:measurement_basis(jewi_su_t1987, observed).
narrative_ontology:measurement(jewi_su_t1993, jewish_self_determination__settler_colonial_reading, suppression_requirement, 1993, 0.72).
narrative_ontology:measurement_basis(jewi_su_t1993, observed).
narrative_ontology:measurement(jewi_su_t2005, jewish_self_determination__settler_colonial_reading, suppression_requirement, 2005, 0.83).
narrative_ontology:measurement_basis(jewi_su_t2005, observed).
narrative_ontology:measurement(jewi_su_t2025, jewish_self_determination__settler_colonial_reading, suppression_requirement, 2025, 0.88).
narrative_ontology:measurement_basis(jewi_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__settler_colonial_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, jewish_self_determination__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, jewish_self_determination__indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, jewish_self_determination__religious_covenant_reading).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, jewish_self_determination__diasporist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Zionism' covers at least five structurally distinct claims about the same kernel, and this file decomposes exactly one member - the settler-colonial reading - with its own epsilon (high), its own beneficiary set (the three insider seats), and its own victim set (the four Palestinian seats). The indigenous_return_reading is the epsilon-inverted sibling: same referent arrangement, opposite valence on which population is indigenous and which is the arriving one. Per-seat classifications must never be averaged across family members; each reading is a separate constraint with a separate stable epsilon, linked here through network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
