% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__settler_colonial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
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
 *   constraint_id: jewish_self_determination__settler_colonial_reading
 *   human_readable: Settler-Colonial Reading of the Zionist Arrangement: Dispossession and Legal Exclusion Structure
 *   domain: political philosophy/nationalism studies/postcolonial theory
 *
 * SUMMARY:
 *   From the settler-colonial reading's seat, the standing arrangement under
 *   contest is a structure built by successive waves of mainly European
 *   Jewish immigration that acquired land, established state institutions,
 *   and in 1948 displaced the majority of the indigenous Arab population,
 *   then consolidated through military occupation after 1967, settlement
 *   expansion, and a legal architecture (Law of Return versus denied refugee
 *   return, differentiated jurisdiction in the West Bank,
 *   demographic-priority statutes) that allocates land, water, residency, and
 *   political authority by ethnic membership. The coordination story offered
 *   for the arrangement - national self-determination and refuge for a
 *   persecuted people - is real for the beneficiary population and is
 *   precisely what this reading identifies as the cover under which
 *   dispossession proceeds. Epsilon's referent is fixed throughout: the
 *   existing arrangement as this reading assesses it, never the
 *   equal-citizenship alternative this reading would endorse. This file is
 *   one member of a five-story constraint family decomposing the kernel
 *   'jewish_self_determination'; the sibling readings are separate
 *   constraints with their own epsilon over the same referent, linked via
 *   network.affects_constraints. Claim and metrics are authored
 *   independently: claimed_type snare is asserted from the structural
 *   analysis below, and the metrics are authored descriptively from the
 *   historical record.
 *
 * KEY AGENTS:
 *   - israeli_state_apparatus: agenda-setter (institutional/arbitrage) - writes and enforces the Law of Return, land regime, occupation orders, and permits system; can reconfigure parts of the structure while keeping the core
 *   - european_jewish_settlers_descendants: primary beneficiary (powerful/mobile) - holds the land titles, housing, water allocations, and citizenship privileges the structure distributes
 *   - diaspora_jewish_communities: secondary beneficiary (organized/mobile) - holds unused immigration entitlements and collects identity, refuge, and network benefits from abroad
 *   - palestinian_refugees: primary target (powerless/trapped) - multigenerational displaced population barred from return, property confiscated
 *   - occupied_west_bank_palestinians: primary target (powerless/trapped) - live under zoned military administration with settlement expansion taking land
 *   - gaza_strip_residents: primary target (powerless/trapped) - confined behind blockade; post-2023 mass displacement and destruction
 *   - palestinian_citizens_of_israel: partial target, partial beneficiary (moderate/constrained) - vote and draw services while bearing land confiscation, budget discrimination, and identity-subordinating statutes
 *   - palestinian_internally_displaced: excluded voice (powerless/trapped) - displaced inside the state in 1948, present in no negotiating framework
 *   - united_states_government: external patron (institutional/mobile) - funds and diplomatically shields the structure while collecting alliance value
 *   - international_legal_bodies: analytical observer (institutional/analytical) - ICJ, ICC, and treaty bodies see the whole structure and hold judgment without execution power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__settler_colonial_reading, 0.85).
domain_priors:suppression_score(jewish_self_determination__settler_colonial_reading, 0.88).
domain_priors:theater_ratio(jewish_self_determination__settler_colonial_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__settler_colonial_reading, snare).
narrative_ontology:human_readable(jewish_self_determination__settler_colonial_reading, "Settler-Colonial Reading of the Zionist Arrangement: Dispossession and Legal Exclusion Structure").
narrative_ontology:topic_domain(jewish_self_determination__settler_colonial_reading, "political philosophy/nationalism studies/postcolonial theory").

domain_priors:requires_active_enforcement(jewish_self_determination__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__settler_colonial_reading, '35eff836-b5fe-4f63-abf4-09c49149018a').
narrative_ontology:cs_kernel_codification('35eff836-b5fe-4f63-abf4-09c49149018a', distributed).
narrative_ontology:cs_authority_grounding('35eff836-b5fe-4f63-abf4-09c49149018a', extraction).
narrative_ontology:cs_interpretation_layer_present('35eff836-b5fe-4f63-abf4-09c49149018a').
narrative_ontology:cs_reading_relation('35eff836-b5fe-4f63-abf4-09c49149018a', jewish_self_determination__liberal_nationalist_reading, forecloses).
narrative_ontology:cs_reading_relation('35eff836-b5fe-4f63-abf4-09c49149018a', jewish_self_determination__indigenous_return_reading, forecloses).
narrative_ontology:cs_reading_relation('35eff836-b5fe-4f63-abf4-09c49149018a', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('35eff836-b5fe-4f63-abf4-09c49149018a', jewish_self_determination__diasporist_reading, influences).
narrative_ontology:cs_axiom('35eff836-b5fe-4f63-abf4-09c49149018a', foundational, zionism_structurally_settler_colonial).
narrative_ontology:cs_axiom_status(zionism_structurally_settler_colonial, holdable).
narrative_ontology:cs_axiom_grounding('35eff836-b5fe-4f63-abf4-09c49149018a', zionism_structurally_settler_colonial, empirically_contingent).
narrative_ontology:cs_axiom('35eff836-b5fe-4f63-abf4-09c49149018a', foundational, self_determination_cannot_rest_on_dispossession).
narrative_ontology:cs_axiom_status(self_determination_cannot_rest_on_dispossession, holdable).
narrative_ontology:cs_axiom_grounding('35eff836-b5fe-4f63-abf4-09c49149018a', self_determination_cannot_rest_on_dispossession, deontological).
narrative_ontology:cs_axiom('35eff836-b5fe-4f63-abf4-09c49149018a', secondary, law_of_return_asymmetry_constitutes_exclusion).
narrative_ontology:cs_axiom_status(law_of_return_asymmetry_constitutes_exclusion, holdable).
narrative_ontology:cs_axiom_grounding('35eff836-b5fe-4f63-abf4-09c49149018a', law_of_return_asymmetry_constitutes_exclusion, empirically_contingent).
narrative_ontology:cs_reference_frame('35eff836-b5fe-4f63-abf4-09c49149018a', settler_national_home_framework).
narrative_ontology:cs_drift_state('35eff836-b5fe-4f63-abf4-09c49149018a', contemporary_annexation_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('35eff836-b5fe-4f63-abf4-09c49149018a', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__settler_colonial_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, european_jewish_settlers_descendants).
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, israeli_state_apparatus).
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, diaspora_jewish_communities).
narrative_ontology:constraint_victim(jewish_self_determination__settler_colonial_reading, palestinian_refugees).
narrative_ontology:constraint_victim(jewish_self_determination__settler_colonial_reading, occupied_west_bank_palestinians).
narrative_ontology:constraint_victim(jewish_self_determination__settler_colonial_reading, palestinian_citizens_of_israel).
narrative_ontology:constraint_victim(jewish_self_determination__settler_colonial_reading, gaza_strip_residents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, palestinian_citizens_of_israel).
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, united_states_government).
narrative_ontology:constraint_vindicates(jewish_self_determination__settler_colonial_reading, jewish_demographic_majority_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the land titles, housing stock, water allocations, and citizenship privileges the arrangement distributes. Descends from waves of immigration that arrived under admission and land-acquisition rules open only to Jews. Most hold additional passports and could emigrate without losing accumulated assets; few do, because income, security, and communal life concentrate inside the structure.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, european_jewish_settlers_descendants, beneficiary,
    powerful, generational, mobile, regional).

% Writes and administers the Law of Return, the land-regime statutes, the military-government orders in the West Bank, and the permit system governing Palestinian movement, building, and water access. Sets settlement policy, collects taxes and customs duties on behalf of the Palestinian Authority, and commands the enforcement forces. It can reconfigure parts of the arrangement, as with the 2005 Gaza withdrawal, while keeping the core intact, and answers electorally to the beneficiary population.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, israeli_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, regional).

% Hold an unconditional immigration-and-citizenship entitlement they rarely exercise, plus a state that anchors identity, refuge guarantees, and philanthropic and political networks. They fund and lobby for the arrangement from abroad while bearing almost none of its daily costs; their exposure is reputational and, for a minority, targeted violence attributed to the conflict.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, diaspora_jewish_communities, beneficiary,
    organized, biographical, mobile, global).

% Descend from roughly 700,000 people displaced in 1948 and further displaced in 1967; registered with UNRWA across Lebanon, Jordan, Syria, and beyond, many in camps into a fourth generation. Barred from returning by the state that took their property, with host-state status ranging from full citizenship in Jordan to employment and property bans in Lebanon. Their exit is exile itself; the arrangement's continuation is what keeps return closed.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, palestinian_refugees, payer,
    powerless, generational, trapped, continental).

% Live under military administration split into zones: fuller local authority in Area A, joint control in B, and full Israeli control in Area C, where settlement growth, outposts, and training grounds take land. Movement runs through checkpoints and permit gates; building without permits brings demolition; water quotas favor settlements. Work inside Israel or in settlements pays more than the local economy offers, tying households to the system that restricts them.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, occupied_west_bank_palestinians, payer,
    powerless, biographical, trapped, regional).

% Confined to 365 square kilometers behind a land, air, and sea blockade maintained since 2007, most descended from 1948 refugees. Movement of people and goods runs through crossings the state controls; fishing limits, agricultural restrictions, and utility cutoffs are levers of the closure. After October 2023 the strip underwent mass displacement and destruction on a scale that dominates every other fact of life there.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, gaza_strip_residents, payer,
    powerless, biographical, trapped, local).

% About twenty percent of the state's citizens: they vote, sit in the Knesset, and draw state services, while carrying cumulative disadvantages - confiscated family land held by the state, admissions-committee housing exclusions, budget gaps in Arab municipalities, and statutes privileging Jewish identity in citizenship and national symbols. Emigration is possible but severs them from the only polity where they hold citizenship.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, palestinian_citizens_of_israel, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__settler_colonial_reading, palestinian_citizens_of_israel, beneficiary).

% Roughly a quarter-million Palestinian citizens whose families were displaced inside the state in 1948 and barred from returning to villages a few kilometers away, now often built over or designated as parks and military zones. They appear in no negotiating framework - not the refugee file, since they did not cross a border, and not the citizen-equality file - and their absent villages are the clearest unresolved ledger inside the Green Line.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, palestinian_internally_displaced, excluded,
    powerless, generational, trapped, national).

% Supplies roughly 3.8 billion dollars in annual military financing, diplomatic protection at the Security Council, and intelligence cooperation, receiving in exchange a stable allied foothold in the region, deep defense-technology ties, and domestic political alignment. It occasionally disciplines the arrangement through loan guarantees and settlement statements but has never made aid conditional enough to alter the core structure.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, united_states_government, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__settler_colonial_reading, united_states_government, agenda_setter).

% The ICJ issued a 2024 advisory opinion finding the occupation unlawful and calling for its cessation; the ICC maintains investigations into conduct in Palestine; UN treaty bodies and special rapporteurs publish findings the state rejects and rarely implements. They see the whole structure from outside and hold judgment without execution power.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, international_legal_bodies, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__settler_colonial_reading, european_jewish_settlers_descendants).
narrative_ontology:fixing_cost_class(jewish_self_determination__settler_colonial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates immigration, naturalization, land allocation, defense, and economic development for the Jewish national community through a single administrative, legal, and security apparatus spanning the territory under its control; solves, for that community, the collective-action problems of physical security, institution-building, and the ingathering of migrants.
% TRANSFER_FUNCTION: Moves land title, housing, water allocations, residency and citizenship rights, and ultimate political authority from Palestinian Arabs to the Jewish-Israeli population and its state institutions; moves diaspora capital and foreign military and economic aid into the state's budget; historically moved Palestinian labor into the settler economy on sub-equal terms.
% ABSENT_VOICES: Palestinian refugees - the majority of the displaced population - sit outside every negotiating framework that determines their return; internally displaced Palestinians inside the state appear in no framework at all; factions outside the Oslo-era PLO channel were excluded from the table; the populations of neighboring states absorb spillover effects without a seat anywhere in the process.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, everything regional would move: the status of millions of refugees reopens immediately, the state's legal and land architecture dissolves, settlement and blockade infrastructures lose their governing frame, host-state refugee policies come under reversal pressure, and alliance structures built around the state re-form. Nothing about the regional order stays in place.
% FOUNDING_PROBLEM: European antisemitism and Jewish statelessness: securing collective physical safety and national continuity for a persecuted minority after emancipation failed, culminating in the Holocaust.
% FOUNDING_PROBLEM_CORROBORATION: Holocaust historiography, European state archives, and contemporaneous diplomatic correspondence corroborate the founding problem's reality from outside the beneficiary set. That the problem has since transformed - the arrangement's operative drivers now being the dispossession itself rather than European persecution - is attested by Palestinian oral-history archives, UNRWA registration records, and critical scholarship; the state's own institutions dispute that transformation, which is why the status is contested rather than dead.
narrative_ontology:disappearance_verdict(jewish_self_determination__settler_colonial_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__settler_colonial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__settler_colonial_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_self_determination__settler_colonial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__settler_colonial_reading, 0.85, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.85 because the principal flows - land title, water, residency rights, political authority - move from the paying population to the beneficiary population with no reciprocal contribution, and the rate of transfer has been governed by the beneficiary side's capacity rather than by any agreed price. Suppression is higher still (0.88) because persistence depends on continuous active machinery: military government, checkpoints, permit denials, blockade, denial of return, and emergency law; remove the machinery and the structure does not reproduce itself. Theater ratio (0.55) rose steeply after 1993: the Oslo process performed resolution while settlement expansion continued, and the democratic self-description increasingly diverges from differentiated-jurisdiction practice; roughly half of visible activity now maintains appearances rather than functions. Accessibility collapse is 0.60 - alternatives are heavily narrowed (no state, no return, blockaded exits) but not eliminated: litigation, boycott campaigns, civic and armed resistance, and international fora remain partially available, which is characteristic of a construct that must be defended rather than a natural limit. Resistance is 0.75, reflecting a century of revolt, intifadas, legal challenge, and solidarity mobilization. All three temporal series are authored on one shared ten-point grid (1897-2025) so the engine samples every metric at every examined time point; the series show extraction accumulating with enforcement intensifying and theater rising, consistent with the T17 abductive hypothesis rather than any reclassification by this file.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences the structure as sovereign self-defense and the ingathering of a persecuted people; from that seat the same facts read as security necessity. The trapped payer seats experience the identical facts as dispossession: the checkpoint is safety from one seat and confinement from the other. The citizen seat straddles - it draws state services and votes while bearing confiscation and statutory subordination, so its computed classification should differ from both the settler seat and the refugee seat. The external patron seat sees alliance value and domestic politics, not daily cost. The analytical seat sees the whole flow structure. The engine computes these per-seat classifications from the structural data; this file's claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for the settler-descendant population, the diaspora communities, and the state apparatus (the state additionally holds arbitrage-grade ability to reconfigure the structure's form, placing it nearest the beneficiary end). Victim declarations drive high directionality for refugees, West Bank residents, and Gaza residents, amplified by trapped exit - none can leave the structure's reach, and the structure is what closes their exits. Palestinian citizens of Israel derive a mid-high directionality from their payer role, moderated by their secondary beneficiary position. Suppression enters the computation unscaled as a raw structural property; only extractiveness is scaled by directionality and scope, and the continental-to-global scopes of the refugee and patron seats amplify effective extraction accordingly.
 *
 * MANDATROPHY ANALYSIS:
 *   The snare classification performs two protective functions here. Against laundering: it refuses to let the genuine coordination achievement - security, institutions, and refuge delivered for one population - stand as the whole account, by requiring the victim set and the enforcement dependence to be named in the same breath. Against caricature: it preserves the real coordination content, so the structure is not modeled as pure elimination with no organizing function; the coordination is real, and it is the vehicle of the extraction. On the genealogy question, the founding mandate (refuge from European persecution) is contested rather than dead: antisemitism persists and spikes, giving the mandate residual life, while the arrangement now generates its own security cycle that no longer depends on the European driver. A dead-mandate verdict would push this toward piton territory; the live-contested verdict keeps it a functioning, enforced structure - which the enforcement-intensifying suppression series corroborates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates the settler_colonial_reading of the jewish_self_determination kernel; the same referent assessed under the indigenous_return_reading or liberal_nationalist_reading yields inverted beneficiary/victim structures and a different epsilon. Which reading governs a given evaluation?',
    'Cross-read the five sibling stories linked in network.affects_constraints; classification is per-reading and never averaged; disputes route to the kernel level rather than to metric adjustment inside this file.',
    'Under the indigenous_return_reading the beneficiary/victim sets invert and epsilon falls toward coordination-cost levels; under this reading epsilon stays high. Treating one reading''s numbers as kernel-level facts misclassifies every sibling.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed classification over a shared kernel; sibling readings are separate constraints, not measurement noise.').

omega_variable(
    epsilon_referent_audit,
    'Does the authored epsilon describe the standing arrangement (the existing Law of Return, settlement, occupation, and blockade architecture) as this reading assesses it, rather than the arrangement this reading endorses (dissolution into equal citizenship with full return)?',
    'Audit each metric against existing statutes, court rulings, and administrative practices; no metric may cite the counterfactual preferred arrangement.',
    'If any metric drifted to the endorsed alternative, epsilon would collapse toward zero and the story would stop measuring the contest; the remedy is re-referencing the metric, not rescaling it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epsilon_referent_audit, conceptual, 'Epsilon referent discipline: the standing arrangement under contest, never the reading''s preferred alternative.').

omega_variable(
    mizrahi_beneficiary_composition,
    'Does ''European Jewish settlers'' accurately name the beneficiary class, given that roughly half of Israel''s Jewish population descends from Mizrahi immigrants who came from Arab and Muslim states and were settled into the same land-and-housing allocation system?',
    'Comparative historical analysis of land, housing, and transit-camp allocation across Ashkenazi and Mizrahi immigrant cohorts in the 1950s-1960s.',
    'If Mizrahi Jews are co-beneficiaries of the allocation structure despite their own displacement histories, the beneficiary set widens and the ''European'' frame weakens; the extraction structure and the victim set are unchanged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mizrahi_beneficiary_composition, empirical, 'Composition of the beneficiary class under the settler-colonial frame.').

omega_variable(
    palestinian_coalition_capacity,
    'Can the divided Palestinian constituencies (citizens of Israel, West Bank, Gaza, diaspora refugees) convert latent numerical weight into effective coalition power against the arrangement?',
    'Historical and ongoing analysis of unified-action episodes (PNC reunification attempts, the 2021 Unity Intifada, general strikes) and their durability under repression.',
    'A sustained coalition would raise measured resistance above 0.75, raise enforcement costs sharply, and stress the arrangement''s stability; persistent fragmentation keeps each constituency individually tractable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(palestinian_coalition_capacity, empirical, 'Coalition potential among fragmented victim constituencies.').

omega_variable(
    post_2023_trajectory_break,
    'Is the post-October-2023 phase (mass displacement, infrastructure destruction, famine conditions, annexation legislation) a continuation of the measured extraction trend or a qualitative break requiring re-authoring?',
    'Track displacement counts, destruction assessments, ICJ and ICC proceedings, and annexation votes against the 2018-2025 series endpoints.',
    'Continuation extends the rising series; a qualitative break such as mass permanent transfer would push epsilon toward its ceiling and shift the operative dynamic from extraction-plus-suppression toward elimination, demanding a new story rather than a re-measurement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_2023_trajectory_break, empirical, 'Whether the current escalation is drift or discontinuity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__settler_colonial_reading, 1897, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1897, jewish_self_determination__settler_colonial_reading, theater_ratio, 1897, 0.12).
narrative_ontology:measurement_basis(jewi_tr_t1897, observed).
narrative_ontology:measurement(jewi_tr_t1922, jewish_self_determination__settler_colonial_reading, theater_ratio, 1922, 0.18).
narrative_ontology:measurement_basis(jewi_tr_t1922, observed).
narrative_ontology:measurement(jewi_tr_t1936, jewish_self_determination__settler_colonial_reading, theater_ratio, 1936, 0.21).
narrative_ontology:measurement_basis(jewi_tr_t1936, observed).
narrative_ontology:measurement(jewi_tr_t1948, jewish_self_determination__settler_colonial_reading, theater_ratio, 1948, 0.24).
narrative_ontology:measurement_basis(jewi_tr_t1948, observed).
narrative_ontology:measurement(jewi_tr_t1967, jewish_self_determination__settler_colonial_reading, theater_ratio, 1967, 0.27).
narrative_ontology:measurement_basis(jewi_tr_t1967, observed).
narrative_ontology:measurement(jewi_tr_t1987, jewish_self_determination__settler_colonial_reading, theater_ratio, 1987, 0.31).
narrative_ontology:measurement_basis(jewi_tr_t1987, observed).
narrative_ontology:measurement(jewi_tr_t1993, jewish_self_determination__settler_colonial_reading, theater_ratio, 1993, 0.46).
narrative_ontology:measurement_basis(jewi_tr_t1993, observed).
narrative_ontology:measurement(jewi_tr_t2005, jewish_self_determination__settler_colonial_reading, theater_ratio, 2005, 0.49).
narrative_ontology:measurement_basis(jewi_tr_t2005, observed).
narrative_ontology:measurement(jewi_tr_t2018, jewish_self_determination__settler_colonial_reading, theater_ratio, 2018, 0.52).
narrative_ontology:measurement_basis(jewi_tr_t2018, observed).
narrative_ontology:measurement(jewi_tr_t2025, jewish_self_determination__settler_colonial_reading, theater_ratio, 2025, 0.55).
narrative_ontology:measurement_basis(jewi_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1897, jewish_self_determination__settler_colonial_reading, base_extractiveness, 1897, 0.44).
narrative_ontology:measurement_basis(jewi_be_t1897, observed).
narrative_ontology:measurement(jewi_be_t1922, jewish_self_determination__settler_colonial_reading, base_extractiveness, 1922, 0.51).
narrative_ontology:measurement_basis(jewi_be_t1922, observed).
narrative_ontology:measurement(jewi_be_t1936, jewish_self_determination__settler_colonial_reading, base_extractiveness, 1936, 0.57).
narrative_ontology:measurement_basis(jewi_be_t1936, observed).
narrative_ontology:measurement(jewi_be_t1948, jewish_self_determination__settler_colonial_reading, base_extractiveness, 1948, 0.78).
narrative_ontology:measurement_basis(jewi_be_t1948, observed).
narrative_ontology:measurement(jewi_be_t1967, jewish_self_determination__settler_colonial_reading, base_extractiveness, 1967, 0.81).
narrative_ontology:measurement_basis(jewi_be_t1967, observed).
narrative_ontology:measurement(jewi_be_t1987, jewish_self_determination__settler_colonial_reading, base_extractiveness, 1987, 0.79).
narrative_ontology:measurement_basis(jewi_be_t1987, observed).
narrative_ontology:measurement(jewi_be_t1993, jewish_self_determination__settler_colonial_reading, base_extractiveness, 1993, 0.75).
narrative_ontology:measurement_basis(jewi_be_t1993, observed).
narrative_ontology:measurement(jewi_be_t2005, jewish_self_determination__settler_colonial_reading, base_extractiveness, 2005, 0.78).
narrative_ontology:measurement_basis(jewi_be_t2005, observed).
narrative_ontology:measurement(jewi_be_t2018, jewish_self_determination__settler_colonial_reading, base_extractiveness, 2018, 0.83).
narrative_ontology:measurement_basis(jewi_be_t2018, observed).
narrative_ontology:measurement(jewi_be_t2025, jewish_self_determination__settler_colonial_reading, base_extractiveness, 2025, 0.85).
narrative_ontology:measurement_basis(jewi_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1897, jewish_self_determination__settler_colonial_reading, suppression_requirement, 1897, 0.22).
narrative_ontology:measurement_basis(jewi_su_t1897, observed).
narrative_ontology:measurement(jewi_su_t1922, jewish_self_determination__settler_colonial_reading, suppression_requirement, 1922, 0.38).
narrative_ontology:measurement_basis(jewi_su_t1922, observed).
narrative_ontology:measurement(jewi_su_t1936, jewish_self_determination__settler_colonial_reading, suppression_requirement, 1936, 0.56).
narrative_ontology:measurement_basis(jewi_su_t1936, observed).
narrative_ontology:measurement(jewi_su_t1948, jewish_self_determination__settler_colonial_reading, suppression_requirement, 1948, 0.71).
narrative_ontology:measurement_basis(jewi_su_t1948, observed).
narrative_ontology:measurement(jewi_su_t1967, jewish_self_determination__settler_colonial_reading, suppression_requirement, 1967, 0.73).
narrative_ontology:measurement_basis(jewi_su_t1967, observed).
narrative_ontology:measurement(jewi_su_t1987, jewish_self_determination__settler_colonial_reading, suppression_requirement, 1987, 0.7).
narrative_ontology:measurement_basis(jewi_su_t1987, observed).
narrative_ontology:measurement(jewi_su_t1993, jewish_self_determination__settler_colonial_reading, suppression_requirement, 1993, 0.67).
narrative_ontology:measurement_basis(jewi_su_t1993, observed).
narrative_ontology:measurement(jewi_su_t2005, jewish_self_determination__settler_colonial_reading, suppression_requirement, 2005, 0.76).
narrative_ontology:measurement_basis(jewi_su_t2005, observed).
narrative_ontology:measurement(jewi_su_t2018, jewish_self_determination__settler_colonial_reading, suppression_requirement, 2018, 0.8).
narrative_ontology:measurement_basis(jewi_su_t2018, observed).
narrative_ontology:measurement(jewi_su_t2025, jewish_self_determination__settler_colonial_reading, suppression_requirement, 2025, 0.88).
narrative_ontology:measurement_basis(jewi_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__settler_colonial_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, religious_covenant_reading).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, diasporist_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'Zionism / Jewish self-determination' covers five structurally distinct claims, each with its own epsilon, beneficiary/victim structure, and classification. The liberal_nationalist_reading is the mainstream upstream claim routinely cited as legitimation for the standing arrangement; this settler_colonial_reading is its structural contestation; the indigenous_return_reading is the direct descriptive negation of this reading; the religious_covenant_reading competes on a different warrant register; and the diasporist_reading draws evidentiary support from this reading's findings while reaching a distinct prescriptive conclusion. Every member links to the others via affects_constraints; no member averages over the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
