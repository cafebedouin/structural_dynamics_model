% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy_dual__zionist_refuge_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy_dual__zionist_refuge_reading, []).

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
 *   constraint_id: territorial_legitimacy_dual__zionist_refuge_reading
 *   human_readable: Zionist Refuge Reading of Territorial Legitimacy (Persecution, Covenant, Partition)
 *   domain: political/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested
 *   territorial-legitimacy kernel: the zionist_refuge_reading, under which
 *   Israel's sovereignty is legitimate because it answers historical
 *   persecution, rests on covenantal promise, and was accepted through the UN
 *   partition process — making 1948 legitimacy uncontested within this frame,
 *   1967 boundaries negotiable but security-justified, and the Palestinian
 *   displacement a consequence of the Arab rejection of partition rather than
 *   of the arrangement itself. The standing arrangement under contest — the
 *   sovereignty and territorial-control regime this reading grounds —
 *   nonetheless imposes large, asymmetric, actively enforced costs on
 *   identifiable populations, and this story authors those costs honestly
 *   from the reading's own lights rather than averaging across readings. The
 *   claim/metric gap is deliberate: the reading CLAIMS tangled_rope (genuine
 *   refuge-coordination plus asymmetric enforced costs) while the metrics
 *   describe the arrangement's actual operation; the engine computes per-seat
 *   classifications from the structural data, and divergence between claim
 *   and computed type is the datum, not an error.
 *
 * KEY AGENTS:
 *   - - israeli_state_institutions: Agenda-setter (institutional/identity_locked) — administers sovereignty, transmits the founding narrative, controls the enforcement apparatus
 *   - - israeli_citizenry: Primary beneficiary (organized/constrained) — receives refuge and defense, pays conscription and war exposure
 *   - - world_jewry_diaspora: Beneficiary (organized/mobile) — holds the refuge guarantee, contributes resources, bears few direct costs
 *   - - west_bank_settlers: Concentrated beneficiary (organized/constrained) — receives subsidized expansion beyond the 1948 lines
 *   - - palestinian_displaced_descendants: Founding-cost bearer (powerless/trapped) — stateless, denied return, no seat in the arrangement
 *   - - west_bank_gaza_palestinians: Ongoing cost bearer (moderate/trapped) — live under the security administration they cannot vote out
 *   - - palestinian_authority_leadership: Dual-positioned (moderate/constrained) — bears the occupation's limits while collecting governing prerogatives from it
 *   - - us_strategic_patrons: External beneficiary (institutional/arbitrage) — collects strategic alignment, supplies the material floor under the arrangement
 *   - - united_nations_member_states: Analytical observer (institutional/analytical) — authored the partition reference and continues adjudication
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__zionist_refuge_reading, 0.68).
domain_priors:suppression_score(territorial_legitimacy_dual__zionist_refuge_reading, 0.75).
domain_priors:theater_ratio(territorial_legitimacy_dual__zionist_refuge_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__zionist_refuge_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy_dual__zionist_refuge_reading, "Zionist Refuge Reading of Territorial Legitimacy (Persecution, Covenant, Partition)").
narrative_ontology:topic_domain(territorial_legitimacy_dual__zionist_refuge_reading, "political/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__zionist_refuge_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__zionist_refuge_reading, '69634e52-b0af-4972-ac4e-2e6978145a73').
narrative_ontology:cs_kernel_codification('69634e52-b0af-4972-ac4e-2e6978145a73', fixed_text).
narrative_ontology:cs_authority_grounding('69634e52-b0af-4972-ac4e-2e6978145a73', lineage).
narrative_ontology:cs_interpretation_layer_present('69634e52-b0af-4972-ac4e-2e6978145a73').
narrative_ontology:cs_reading_relation('69634e52-b0af-4972-ac4e-2e6978145a73', territorial_legitimacy_dual__palestinian_autochthony_reading, coexists_with).
narrative_ontology:cs_reading_relation('69634e52-b0af-4972-ac4e-2e6978145a73', territorial_legitimacy_dual__two_state_coexistence_reading, influences).
narrative_ontology:cs_axiom('69634e52-b0af-4972-ac4e-2e6978145a73', foundational, partition_resolution_conferred_valid_title).
narrative_ontology:cs_axiom_status(partition_resolution_conferred_valid_title, holdable).
narrative_ontology:cs_axiom_grounding('69634e52-b0af-4972-ac4e-2e6978145a73', partition_resolution_conferred_valid_title, conventional).
narrative_ontology:cs_axiom('69634e52-b0af-4972-ac4e-2e6978145a73', foundational, persecution_history_creates_refuge_entitlement).
narrative_ontology:cs_axiom_status(persecution_history_creates_refuge_entitlement, holdable).
narrative_ontology:cs_axiom_grounding('69634e52-b0af-4972-ac4e-2e6978145a73', persecution_history_creates_refuge_entitlement, deontological).
narrative_ontology:cs_axiom('69634e52-b0af-4972-ac4e-2e6978145a73', secondary, security_necessity_extends_control_beyond_partition_lines).
narrative_ontology:cs_axiom_status(security_necessity_extends_control_beyond_partition_lines, holdable).
narrative_ontology:cs_axiom_grounding('69634e52-b0af-4972-ac4e-2e6978145a73', security_necessity_extends_control_beyond_partition_lines, instrumental).
narrative_ontology:cs_reference_frame('69634e52-b0af-4972-ac4e-2e6978145a73', partition_sanctioned_1948_sovereignty).
narrative_ontology:cs_drift_state('69634e52-b0af-4972-ac4e-2e6978145a73', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('69634e52-b0af-4972-ac4e-2e6978145a73', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__zionist_refuge_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, israeli_citizenry).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, world_jewry_diaspora).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, west_bank_settlers).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, us_strategic_patrons).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_displaced_descendants).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__zionist_refuge_reading, west_bank_gaza_palestinians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_authority_leadership).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_authority_leadership).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the sovereignty arrangement: controls land registration and planning in Area C, sets security doctrine, runs the education and memory institutions that transmit the founding narrative, and conducts the diplomacy that defends recognition. Abandoning the founding account of persecution, covenant, and partition acceptance would dissolve the state's own self-understanding, so exit from the narrative is not a live option for it.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, israeli_state_institutions, agenda_setter,
    institutional, generational, identity_locked, global).

% Receives guaranteed refuge, citizenship, collective defense, and a durable national home. Pays through universal conscription, recurring war exposure, and the fiscal burden of administering the territories. Emigration exists but severs citizenship, family, and social embeddedness, so leaving is costly rather than impossible.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, israeli_citizenry, beneficiary,
    organized, biographical, constrained, regional).

% Holds the refuge guarantee as insurance against persecution and draws identity anchoring from the state's existence. Contributes funding, advocacy, and political lobbying. Most bear none of the direct security costs; those who disagree with state policy can reduce engagement or emigrate elsewhere without statelessness.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, world_jewry_diaspora, beneficiary,
    organized, generational, mobile, global).

% Receive subsidized housing, infrastructure, and military protection on land beyond the 1948 lines, administered under a legal regime separate from that applied to neighboring Palestinian towns. Their position depends on the continuation of territorial control; withdrawal frameworks would require relocating communities and abandoning built assets and, for many, a religious-national vocation.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, west_bank_settlers, beneficiary,
    organized, generational, constrained, regional).

% Descendants of those displaced in 1948, many still stateless in host countries or camps with restricted legal status. The arrangement denies their return and transfers their former property and standing to the state's title system. They have no sovereign representation in any forum that maintains the arrangement and cannot exit their condition.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_displaced_descendants, payer,
    powerless, generational, trapped, global).

% Live under movement restrictions, checkpoint regimes, closure policies, and in Gaza under blockade; their land and water access are administered by authorities they did not elect and cannot remove. Citizenship or residence elsewhere is largely unavailable; the arrangement's security administration structures daily life without offering them a path out of it.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, west_bank_gaza_palestinians, payer,
    moderate, biographical, trapped, regional).

% Administers civil affairs in parts of the West Bank under negotiated limits, collects cleared tax revenues channeled through the arrangement, and receives international patronage contingent on security coordination. Bears the visible costs of the occupation's limits on its constituents while drawing governing prerogatives, salaries, and diplomatic standing from the same structure; dissolving the arrangement would also dissolve its own position.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_authority_leadership, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_authority_leadership, beneficiary).

% Collect intelligence cooperation, technology transfer, and a stable aligned presence in the region, sustained by the arrangement's persistence. Provide the military assistance and diplomatic cover that materially lower the cost of maintaining territorial control. Their support is a policy choice that can be redirected at lower cost than any other party's position, though domestic politics raises the price of doing so.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, us_strategic_patrons, beneficiary,
    institutional, generational, arbitrage, global).

% Authored the partition recommendation in 1947 and continue to adjudicate the arrangement's standing through General Assembly resolutions, Security Council action shaped by veto politics, and referral of legal questions to international courts. They take positions from outside the arrangement's direct benefit and cost flows.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, united_nations_member_states, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy_dual__zionist_refuge_reading, israeli_state_institutions).
narrative_ontology:fixing_cost_class(territorial_legitimacy_dual__zionist_refuge_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Consolidates a historically dispersed, repeatedly persecuted minority into a single defended polity with a guaranteed refuge, collective defense, and a durable membership boundary — solving, for that population, the statelessness-and-no-protector problem that recurrent pogroms and the Holocaust made acute.
% TRANSFER_FUNCTION: Moves territorial sovereignty and title from the Mandate territory's non-Jewish inhabitants to the Jewish national home; moves the daily costs of security administration — movement, land, water, jurisdiction — onto Palestinians under occupation; moves diaspora financial and political capital into the state.
% ABSENT_VOICES: The Palestinian Arab majority of 1947 held no sovereign seat at the partition decision; objections were entered by neighboring states, not by the population that would be displaced. Refugees denied return remain outside every forum that reaffirms the arrangement, and their objection reaches it only through proxies. Within the occupying administration, the governed population of the territories has no vote over the rules that govern them.
% DISAPPEARANCE_RATIONALE: If the legitimacy arrangement vanished overnight, the sovereignty claim it grounds would collapse into a contested vacuum: nine-plus million residents' citizenship status, the refuge guarantee for a global diaspora, a web of security treaties and patronage flows, and the legal title system over the territory would all require renegotiation under conditions likely to produce immediate regional war.
% FOUNDING_PROBLEM: Two millennia of expulsion and persecution left a worldwide Jewish population with no sovereign protector, culminating in the Holocaust's attempted extermination; the arrangement was built to solve that by establishing a sovereign homeland with guaranteed admission and collective defense.
% FOUNDING_PROBLEM_CORROBORATION: Independent Holocaust historiography, contemporaneous Red Cross and Allied records, and the UN's own 1947 deliberations corroborate the founding problem from entirely outside the beneficiary set. Arab-state archives independently corroborate the 1948 rejection of the partition plan that this reading cites for displacement causation, even as those same parties dispute the legitimacy conclusion drawn from it. No party to the dispute, including the reading's sharpest critics, asserts that antisemitic persecution has ended.
narrative_ontology:disappearance_verdict(territorial_legitimacy_dual__zionist_refuge_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy_dual__zionist_refuge_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__zionist_refuge_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(territorial_legitimacy_dual__zionist_refuge_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy_dual__zionist_refuge_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy_dual__zionist_refuge_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy_dual__zionist_refuge_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy_dual__zionist_refuge_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness ends at 0.68: even granting this reading's causal framing of 1948, the standing arrangement transfers land, movement freedom, water access, and jurisdiction from a population with no exit to a population and its patrons, and the transfer has widened with settlement growth since the Oslo-era dip. Suppression is 0.75 and is authored as a RAW structural property — the checkpoint, closure, and blockade machinery is unscaled by power or scope; only extractiveness gets scaled downstream by directionality and scope. Theater_ratio 0.48 reflects a legitimacy apparatus now split between real functions (defense, refuge administration, treaty maintenance) and a growing share of narrative-maintenance activity (memory politics, hasbara, recognition campaigns) that defends the account rather than operates the refuge. Accessibility_collapse 0.50: alternatives (two-state frameworks, rights-based constitutional proposals) remain partially live — the reading itself concedes 1967 negotiability — so alternatives are narrowed, not eliminated. Resistance 0.70: two intifadas, sustained UN contestation, boycott movements, and litigation meet the arrangement continuously. The measurement series run on ONE shared grid (t = 0, 19, 26, 34, 46, 55, 67, 77 years from 1948) with all three metrics authored at every point; the mid-series dips at t=46 reflect the Oslo interim-transfer of civil administration, an external diplomatic event, not oscillating internal reinforcement.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and the engine owns that computation. From the agenda-setter seat, the arrangement is a refuge it built and defends — coordination-dominant, with costs reframed as security necessity. From the trapped payer seats, the same structure is experienced as totalizing: no vote over the rules, no exit from the territory, no forum where their objection has standing. The diaspora and patron seats see a functioning insurance policy and alliance respectively, at near-zero personal cost. Same-power asymmetry appears between the two Palestinian seats and the settler seat: nominally comparable local standing, radically different legal regimes, which is why exit_options rather than power alone differentiates their computed positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive d toward 0 for israeli_citizenry, world_jewry_diaspora, west_bank_settlers, and us_strategic_patrons — with the diaspora and patrons nearest the subsidy end (benefit without direct cost, mobile or arbitrage exit) and settlers concentrated on the specific extraction surface (land beyond the 1948 lines). Victim declarations drive d toward 1 for palestinian_displaced_descendants and west_bank_gaza_palestinians, amplified by trapped exit — the descendants sit nearest full-target because their loss is the arrangement's founding act and their exclusion from remedy is total. The state sits low-mid: it collects the arrangement's gains but also bears genuine security costs, pulling it off the pure-beneficiary pole. The PA's dual position nets it mid-high d — it pays on behalf of its constituents while collecting administrative rents. No directionality overrides are declared: the beneficiary/victim plus exit data already differentiate every seat correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification discipline cuts both ways here. Labeling the arrangement a pure snare would erase the genuine coordination function — a persecuted, previously stateless population does receive a working refuge, and the founding problem is corroborated as live by sources outside the beneficiary set. Labeling it a rope would erase the identified victims and the enforcement dependence. Tangled_rope holds both halves: real coordination, asymmetric extraction, active enforcement. The rising theater_ratio series is the drift signal to watch: if narrative maintenance continues replacing operational function while the founding problem's solution narrows to performance, the structure trends toward piton — but the live founding problem and the heavy real enforcement machinery keep it short of that today. The R5 interview confirms no zombie condition: the founding problem is live, the world would rearrange on disappearance, and the mismatch consumer finds no dead-problem/world-rearranges flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of kernel territorial_legitimacy_dual (reading: zionist_refuge_reading). What would instantiating the palestinian_autochthony_reading change structurally?',
    'Author the sibling story separately: under the autochthony reading the same standing arrangement is assessed as founded dispossession rather than war-consequence displacement, raising epsilon sharply, expanding the victim set to include the founding act itself, and shifting the computed type toward snare at the payer seats.',
    'The disagreement is located in the displacement-causation premise and the weight given to habitation continuity versus partition consent. Classification of the identical territory flips between readings; no observable selection reconciles them because they are different constraints over the same referent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer-frame omega: sibling reading would reclassify the same arrangement with materially higher extraction.').

omega_variable(
    displacement_causation_ambiguity,
    'Were the 1948 displacements primarily wartime flight and expulsion triggered by the Arab rejection of partition, as this reading holds, or substantially the product of planned transfer preceding the war?',
    'Declassified IDF and political-archives analysis (Plan Dalet scholarship, village-destruction records, contemporaneous refugee-flow timing studies) weighed against Arab-archive records of the rejection decisions.',
    'If planned transfer predominates, the reading''s exculpatory framing fails, epsilon rises toward the autochthony reading''s assessment, and the coordination-function claim weakens; if wartime-triggered flight predominates, the reading''s framing holds and part of the measured extraction is war cost rather than design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displacement_causation_ambiguity, empirical, 'Whether the founding displacement was war consequence or design — the hinge of this reading''s causal framing.').

omega_variable(
    security_vs_annexation_boundary,
    'Is territorial control beyond the 1948 lines driven by genuine security necessity, as this reading asserts, or by settlement-expansion and annexation objectives riding on security language?',
    'Compare settlement placement against military-necessity assessments: locations defensible on terrain and threat models versus locations maximizing land contiguity and demographic leverage; track policy under governments of differing security assessments.',
    'If annexation-dominant, the post-1967 component stops being a defensible extension of the refuge arrangement and becomes extraction on its own account, pushing the computed classification toward snare-flavored at the payer seats and validating the sibling readings'' critique of exactly this delta.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_vs_annexation_boundary, empirical, 'Whether the security justification for post-1967 control tracks defense needs or expansion aims.').

omega_variable(
    divine_promise_epistemic_role,
    'Does the covenantal-promise premise function as a load-bearing normative ground of the legitimacy claim, or as identity-reinforcing narrative decoration over the persecution and partition grounds?',
    'Conceptual analysis of the claim''s practice: test whether the reading''s holders treat covenant premises as defeasible by secular counterargument or as prior to it; examine whether state legitimacy arguments addressed to third parties ever rest on the covenant limb alone.',
    'If load-bearing, the legitimacy claim is partly theological and unreachable by empirical resolution, hardening the coexistence structure among readings; if decorative, the claim reduces to conventional (partition) plus deontological (refuge) grounds, which are addressable in ordinary international argument and soften the kernel contest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_promise_epistemic_role, conceptual, 'Epistemic status of the divine-promise limb within this reading''s legitimacy stack.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__zionist_refuge_reading, 0, 77).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t0, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(terr_tr_t19, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 19, 0.2).
narrative_ontology:measurement(terr_tr_t26, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 26, 0.22).
narrative_ontology:measurement(terr_tr_t34, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 34, 0.27).
narrative_ontology:measurement(terr_tr_t46, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 46, 0.32).
narrative_ontology:measurement(terr_tr_t55, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 55, 0.38).
narrative_ontology:measurement(terr_tr_t67, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 67, 0.43).
narrative_ontology:measurement(terr_tr_t77, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 77, 0.48).

% Extraction over time
narrative_ontology:measurement(terr_be_t0, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(terr_be_t19, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 19, 0.56).
narrative_ontology:measurement(terr_be_t26, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 26, 0.58).
narrative_ontology:measurement(terr_be_t34, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 34, 0.6).
narrative_ontology:measurement(terr_be_t46, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 46, 0.5).
narrative_ontology:measurement(terr_be_t55, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 55, 0.6).
narrative_ontology:measurement(terr_be_t67, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 67, 0.64).
narrative_ontology:measurement(terr_be_t77, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 77, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t0, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(terr_su_t19, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 19, 0.55).
narrative_ontology:measurement(terr_su_t26, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 26, 0.58).
narrative_ontology:measurement(terr_su_t34, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 34, 0.62).
narrative_ontology:measurement(terr_su_t46, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 46, 0.48).
narrative_ontology:measurement(terr_su_t55, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 55, 0.66).
narrative_ontology:measurement(terr_su_t67, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 67, 0.71).
narrative_ontology:measurement(terr_su_t77, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 77, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy_dual__zionist_refuge_reading, identity_coordination).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_autochthony_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__zionist_refuge_reading, two_state_coexistence_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'Israel's legitimacy': the label conflates three structurally distinct claims with different epsilon values over the same territorial referent. This story (zionist_refuge_reading) authors the partition-consent and refuge-entitlement claim at moderate epsilon; palestinian_autochthony_reading authors the habitation-and-displacement-trauma claim, which assesses the same standing arrangement at high epsilon; two_state_coexistence_reading authors the mutual-recognition compromise claim, which treats both others as inputs. The refuge reading structurally influences the coexistence reading (its security doctrine and settlement facts define what boundaries the compromise framework can offer) and coexists with the autochthony reading (rival live positions held by different parties, neither logically eliminating the other across frameworks). Family members are linked via affects_constraints per the BGS pattern.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
