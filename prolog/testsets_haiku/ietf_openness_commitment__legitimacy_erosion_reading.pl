% ============================================================================
% CONSTRAINT STORY: ietf_openness_commitment__legitimacy_erosion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ietf_openness_commitment__legitimacy_erosion_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ietf_openness_commitment__legitimacy_erosion_reading
 *   human_readable: IETF Rough Consensus Legitimacy Erosion (Capture-Vulnerable Reading)
 *   domain: institutional_governance/internet_standards
 *
 * SUMMARY:
 *   This constraint instantiates the LEGITIMACY_EROSION reading of the
 *   contested kernel: IETF_OPENNESS_COMMITMENT. The reading frames the rough
 *   consensus mechanism not as a shared commons (stewardship reading) or as a
 *   coordination substrate (capture substrate reading), but as a
 *   legitimacy-extraction target—a procedural mechanism whose authority to
 *   certify decisions as 'merit-based' has been increasingly captured by
 *   well-resourced vendor coalitions. Under this reading, vendors do not
 *   merely extract rents from the standards themselves (higher extractiveness
 *   of compatible products, lock-in boundaries); they extract the legitimacy
 *   of the process itself, converting organized bloc voting and procedural
 *   narration into evidence of 'consensus,' which ratifies vendor preferences
 *   as technically inevitable. The victim is not directly the implementers
 *   (though they suffer downstream) but the consensus commons—the shared
 *   belief that rough consensus reflects distributed technical merit rather
 *   than organized power.
 *
 * KEY AGENTS:
 *   - well_resourced_technology_vendors: Institutional beneficiaries; author proposals aligned with product roadmaps; deploy organized voting blocs; capture procedural legitimacy
 *   - corporate_standards_coalitions: Institutional agenda-setters; amplify vendor influence via collective action; fund liaison teams; narrate bloc positions as evidence of consensus
 *   - resource_constrained_implementers: Moderate-power payers; absent due to cost of participation; implement what emerges; cannot steer; lose the shield of legitimate consensus
 *   - emerging_technology_communities: Powerless, identity-locked payers; depend on standards legitimacy for credibility; face institutional gatekeeping; cannot exit without undermining their own value proposition
 *   - IETF secretariat and working group chairs: Organized agenda-setters constrained by procedural authority; must interpret what rough consensus means when factions are organized; face pressure to accept bloc positions without appearing partisan
 *   - consensus_legitimacy_commons: The shared epistemic asset (non-agent entity); erodes when vendors extract procedural legitimacy; loss of this commons undermines the entire justification for open standards
 *   - academic_and_independent_researchers: Powerless, identity-locked payers; develop novel approaches; must navigate standards process controlled by vendors with competing interests; fused identity with open standards contribution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ietf_openness_commitment__legitimacy_erosion_reading, 0.68).
domain_priors:suppression_score(ietf_openness_commitment__legitimacy_erosion_reading, 0.71).
domain_priors:theater_ratio(ietf_openness_commitment__legitimacy_erosion_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ietf_openness_commitment__legitimacy_erosion_reading, tangled_rope).
narrative_ontology:human_readable(ietf_openness_commitment__legitimacy_erosion_reading, "IETF Rough Consensus Legitimacy Erosion (Capture-Vulnerable Reading)").
narrative_ontology:topic_domain(ietf_openness_commitment__legitimacy_erosion_reading, "institutional_governance/internet_standards").

domain_priors:requires_active_enforcement(ietf_openness_commitment__legitimacy_erosion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ietf_openness_commitment__legitimacy_erosion_reading, '5443f44d-7fb7-42d9-b2b3-6f78b1168e39').
narrative_ontology:cs_kernel_codification('5443f44d-7fb7-42d9-b2b3-6f78b1168e39', distributed).
narrative_ontology:cs_authority_grounding('5443f44d-7fb7-42d9-b2b3-6f78b1168e39', practice).
narrative_ontology:cs_interpretation_layer_present('5443f44d-7fb7-42d9-b2b3-6f78b1168e39').
narrative_ontology:cs_reading_relation('5443f44d-7fb7-42d9-b2b3-6f78b1168e39', ietf_openness_commitment__commons_stewardship_reading, coexists_with).
narrative_ontology:cs_reading_relation('5443f44d-7fb7-42d9-b2b3-6f78b1168e39', ietf_openness_commitment__capture_substrate_reading, influences).
narrative_ontology:cs_axiom('5443f44d-7fb7-42d9-b2b3-6f78b1168e39', foundational, rough_consensus_degrades_through_capture).
narrative_ontology:cs_axiom_status(rough_consensus_degrades_through_capture, holdable).
narrative_ontology:cs_axiom_grounding('5443f44d-7fb7-42d9-b2b3-6f78b1168e39', rough_consensus_degrades_through_capture, empirically_contingent).
narrative_ontology:cs_axiom('5443f44d-7fb7-42d9-b2b3-6f78b1168e39', foundational, procedural_legitimacy_is_extractable_asset).
narrative_ontology:cs_axiom_status(procedural_legitimacy_is_extractable_asset, holdable).
narrative_ontology:cs_axiom_grounding('5443f44d-7fb7-42d9-b2b3-6f78b1168e39', procedural_legitimacy_is_extractable_asset, deontological).
narrative_ontology:cs_reference_frame('5443f44d-7fb7-42d9-b2b3-6f78b1168e39', open_merit_based_standardization).
narrative_ontology:cs_drift_state('5443f44d-7fb7-42d9-b2b3-6f78b1168e39', contemporary_vendor_consolidation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5443f44d-7fb7-42d9-b2b3-6f78b1168e39', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(ietf_openness_commitment__legitimacy_erosion_reading, ietf_openness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__legitimacy_erosion_reading, well_resourced_technology_vendors).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__legitimacy_erosion_reading, corporate_standards_coalitions).
narrative_ontology:constraint_victim(ietf_openness_commitment__legitimacy_erosion_reading, consensus_legitimacy_commons).
narrative_ontology:constraint_victim(ietf_openness_commitment__legitimacy_erosion_reading, resource_constrained_implementers).
narrative_ontology:constraint_victim(ietf_openness_commitment__legitimacy_erosion_reading, emerging_technology_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ietf_openness_commitment__legitimacy_erosion_reading, academic_and_independent_researchers).
narrative_ontology:constraint_vindicates(ietf_openness_commitment__legitimacy_erosion_reading, procedural_legitimacy_as_extractive_asset).
narrative_ontology:constraint_vindicates(ietf_openness_commitment__legitimacy_erosion_reading, organizational_capture_of_consensus_machinery).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Large technology firms with dedicated standards teams, patent portfolios, and implementation capacity. They author proposals aligned with their product roadmaps, fund expensive liaison roles, and deploy organized voting blocs in working groups. They benefit from standards that encode their preferred interoperability boundaries and lock in compatibility constraints favoring their existing implementations. Can arbitrage across multiple standards bodies and geographies.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, well_resourced_technology_vendors, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ietf_openness_commitment__legitimacy_erosion_reading, well_resourced_technology_vendors, beneficiary).

% Formally organized groups of vendors coordinating their standards positions and voting strategies. They amplify their individual influence through collective action, negotiate bloc positions in working groups, and fund joint liaison teams. Their coordinated presence in deliberation changes the perception and actual weight of consensus.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, corporate_standards_coalitions, agenda_setter,
    institutional, generational, arbitrage, global).

% Smaller firms, nonprofit organizations, and individual engineers who implement standards but lack the capacity to participate in expensive multi-year standards negotiations. They implement what emerges but cannot steer it. Their absence from deliberation means the rough consensus that forms does not weigh their implementation costs or use-case constraints.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, resource_constrained_implementers, payer,
    moderate, biographical, constrained, regional).

% Communities around new protocols and technologies (decentralized systems, novel transport layers, alternative naming schemes) that depend on standardization for interoperability legitimacy but lack the institutional resources to shape the standards process. They are structurally identity-locked: their credibility depends on standards endorsement, but standards bodies are controlled by entrenched vendors who view the new work as threatening. Exit (proprietary standards, fragmented implementations) undermines their entire value proposition.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, emerging_technology_communities, payer,
    powerless, biographical, identity_locked, global).

% IETF staff and working group chairs tasked with administering the consensus process. They interpret what 'rough consensus' means when factions are organized, decide when consensus exists despite explicit dissent, and enforce participation norms. Their procedural authority is real but bounded — chairs cannot simply reject a vendor coalition's bloc position without appearing partisan, yet accepting it without scrutiny endorses capture.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, ietf_secretariat_and_chairs, agenda_setter,
    organized, biographical, constrained, global).

% The shared epistemic asset: the collective belief that the IETF process produces technical consensus reflecting the merit of ideas rather than the power of factions. When vendors extract procedural legitimacy (vote in bloc, narrate defeat as consensus, capture agenda-setting), the commons erodes. This is not an agent but a non-agent entity whose degradation is the constraint's primary extraction target.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, consensus_legitimacy_commons, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(ietf_openness_commitment__legitimacy_erosion_reading, consensus_legitimacy_commons).

% End users whose network experience depends on standards that may encode vendor lock-in or compatibility tax rather than genuine interoperability. They bear the cost of standards capture in the form of higher switching costs, vendor-specific feature lock-in, and reduced innovation pressure. They have no voice in the process and no seat at the table.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, global_internet_users_indirectly_affected, excluded,
    powerless, civilizational, trapped, global).

% Researchers in universities and independent labs who develop novel approaches but lack institutional affiliation with standards-steering firms. To get their work implemented at scale, they must navigate a standards process where rough consensus is increasingly shaped by vendors with competing product interests. Their identity as researchers is fused with contributions to open standards; exiting the process means losing professional legitimacy.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, academic_and_independent_researchers, payer,
    powerless, biographical, identity_locked, global).

% Analysts, governance scholars, and policy researchers who study how standards bodies function and what legitimacy conditions they require. They measure whether consensus is genuine or manufactured, document the resource disparities in participation, and produce evidence about who is excluded and why. They take no position but record the structural data.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, internet_standards_governance_community, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ietf_openness_commitment__legitimacy_erosion_reading, well_resourced_technology_vendors).
narrative_ontology:fixing_cost_class(ietf_openness_commitment__legitimacy_erosion_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The IETF rough consensus mechanism is designed to solve the coordination problem of decentralized internet protocol development: how to converge on shared technical choices (packet formats, behavior rules, compatibility boundaries) without a central authority, so that independently-built systems can interoperate. The mechanism channels disagreement into working group deliberation, weight-balances by technical argument rather than organizational hierarchy, and produces standards that implementers can trust reflect engineering merit rather than political fiat.
% TRANSFER_FUNCTION: Well-resourced vendors extract procedural legitimacy by deploying organized blocs, funding intensive liaison roles, and narrating organized voting outcomes as evidence of 'rough consensus.' This converts the legitimacy of the mechanism itself (the belief that consensus reflects merit) into a private asset that ratifies vendor-preferred outcomes. The transfer is: from the consensus commons (the credibility of the entire process) to well-resourced institutional actors (who gain the ability to stamp their self-interested proposals as technically inevitable). Resource-constrained implementers and emerging communities are the victims because they lose the shield that legitimate consensus once provided against being railroaded by vendor coalitions.
% ABSENT_VOICES: End users have no formal seat in the IETF and are entirely absent from deliberation; their interest in genuine interoperability (lower switching costs, more innovation pressure) is never voiced. Smaller implementers and nonprofit technology organizations are absent not by design but by cost: a working group role requires funding dedicated staff to attend multiple meetings per year, read hundreds of email threads, and maintain technical depth in contested topics — a cost structure that excludes anyone not backed by a large organization. Decentralized and alternative-technology communities are absent by institutional gatekeeping: chairs and established vendors discourage work that threatens incumbents as 'not ready for standards,' so emerging work remains unofficial and never accesses the legitimacy the formal process confers.
% DISAPPEARANCE_RATIONALE: If the rough consensus mechanism and its legitimacy claim vanished overnight, the internet standards landscape would splinter into multiple substrate layers: large vendors would encode proprietary compatibility requirements, technical minorities and emerging communities would establish parallel standards bodies with higher participation barriers for rivals, and implementers would navigate a fragmented standards ecosystem where technical choices became explicit corporate positions rather than cloaked in consensus language. The legitimacy shield that allowed resource-constrained actors to invoke 'IETF standard' as sufficient justification for implementation would be gone, shifting power entirely toward whoever controls deployment platforms.
% FOUNDING_PROBLEM: In the 1980s–1990s, internet standards development faced the risk that either a single government or corporation would control the process (like the ITU model, where national telecom monopolies set telephony standards) or standards would fragment across incompatible vendor silos with no coordination mechanism. The IETF was chartered to solve this by making standards development open, distributed, and based on technical merit: anyone could participate, rough consensus (not voting) would decide outcomes, and 'running code and rough consensus' (later formalized) would weigh implementation experience heavily against paper arguments.
% FOUNDING_PROBLEM_CORROBORATION: The IETF's founding documents and early histories (RFC 2026, Crocker's origin essays) confirm the problem was real in the era of closed, government-dominated telecom standards. Academic histories and internet governance scholars (Milton Mueller, Laura DeNardis) corroborate that decentralized, merit-based standards development was a genuine innovation. However, network economics research by Susan Crawford and others shows that the founding problem (centralized control) is SOLVED for large, profitable protocols but the mechanism now ENABLES a new problem: vendors with scale can make centralized choices (their proprietary implementations) appear decentralized (via organized consensus in working groups). So the founding problem is simultaneously dead (the IETF succeeded in preventing ITU-style monopoly) and live (the mechanism is now vulnerable to resource-concentrated capture). Dissenting voices from emerging-tech communities, smaller implementers, and governance scholars explicitly attest this shift.
narrative_ontology:disappearance_verdict(ietf_openness_commitment__legitimacy_erosion_reading, world_rearranges).
narrative_ontology:founding_problem_status(ietf_openness_commitment__legitimacy_erosion_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ietf_openness_commitment__legitimacy_erosion_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(ietf_openness_commitment__legitimacy_erosion_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ietf_openness_commitment__legitimacy_erosion_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ietf_openness_commitment__legitimacy_erosion_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ietf_openness_commitment__legitimacy_erosion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.68) because the constraint does coordinate real work (IETF standards do get written and widely implemented), but that coordination function is increasingly being used to ratify vendor-preferred outcomes rather than to surface genuine technical merit. Suppression is correspondingly high (0.71) because the mechanism's persistence depends on actively suppressing alternatives (keeping emerging communities out of formal standards channels, ignoring resource-constrained implementer objections, narrating organized voting as consensus despite dissent). Theater_ratio is moderately elevated (0.52) and rising steeply in the early interval (t=0-5), indicating that a growing share of procedural activity is performative: chairs and vendors narrate closure even when dissent is substantial, liaisons issue press releases declaring consensus that internal deliberations did not support, and the 'rough' part of 'rough consensus' is increasingly used to suppress formal dissent without engaging it. Accessibility_collapse (0.48) is relatively low compared to suppression, capturing the fact that alternatives remain (parallel standards bodies, proprietary protocols, fragmented implementations) but they are costly and carry reputational penalties—not impossible exit, but heavily discouraged. Resistance (0.62) is substantial because emerging communities, smaller implementers, and independent researchers actively object to the capture, publish critiques, and attempt to form alternative governance structures, but they lack the institutional power to shift the mechanism without outside intervention (regulatory or financial). The measurement series track extractiveness rising from 0.42 (early period, based on historical accounts of more diverse participation) to 0.68 (contemporary); theater rising from 0.28 to 0.52; and suppression rising from 0.54 to 0.71. The grid shows individual-level resistance and stakes rising (emerging communities feeling pressure), class-level stakes inflating (resource-constrained implementers increasingly excluded), organizational-level suppression holding steady (vendors maintain bloc discipline), and structural-level accessibility paradoxically declining slightly (broader alternatives become visible even as formal IETF participation barriers rise). This pattern indicates a system where coercion is rising at multiple levels but is being expressed through procedural exclusion and reputational cost rather than formal prohibition—a classic capture dynamic where the captured process itself becomes the suppression mechanism.
 *
 * PERSPECTIVAL GAP:
 *   The well-resourced vendor seats should compute as beneficiaries experiencing modest effective extraction (d near 0.2–0.3: they pay dues and meet costs, but they capture procedural legitimacy that justifies their product choices). Resource-constrained implementer and emerging-community seats should compute as high-d targets (d near 0.75–0.95) because they bear the suppression cost (exclusion, procedural barriers, identity lock) and receive no offsetting coordination benefit (the standards that emerge do not reflect their interests). The IETF secretariat seats should compute differently from both: they are trapped between their procedural authority (supposed to oversee legitimacy) and their structural dependence (cannot operate without vendor participation funding and large-firm volunteer staff). Their d is likely moderate-to-high (0.5–0.7) because they are being used as the mechanism of legitimacy extraction—their role itself becomes the extracted asset. The engine computes these per-seat divergences from the structural data (who benefits, who pays, exit options, identity locks); the authored claim does not reconcile these seated views but rather names the theoretical reading (legitimacy_erosion) that makes sense of the structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and payer declarations map to directionality as follows: (1) Well-resourced vendors are declared as beneficiaries because they gain procedural legitimacy that justifies their product choices and market positions—their d derives from organizational power (institutional) + arbitrage exit options, putting them at the beneficiary end (d ≈ 0.15–0.25). (2) Corporate coalitions are agenda-setters (administrative power) + beneficiaries of coordinated bloc advantage; their d derives from institutional power + arbitrage, slightly higher than individual vendors because they explicitly extract procedural advantage, (d ≈ 0.25–0.35). (3) Resource-constrained implementers are declared victims because they bear the cost of procedural exclusion without capturing coordin benefits; moderate power + constrained exit → d ≈ 0.65–0.75. (4) Emerging technology communities are declared victims and are identity_locked (cannot exit without destroying their own legitimacy claim); powerless + identity_locked → d ≈ 0.80–0.95, the highest extraction target. (5) The consensus_legitimacy_commons is declared as a victim (non-agent) because its degradation is the primary extraction target; it has no power and no exit (it is a collective epistemic state), so d ≈ 1.0 in the sense that it bears the full cost. (6) Academic and independent researchers are declared victims, identity_locked, and powerless; d ≈ 0.80–0.90. These directionality values are input to the engine's effective extraction (χ) computation, where χ scales base extractiveness (ε) by the target's directionality and scope. A resource-constrained implementer at constrained exit sees χ = 0.68 × d × scope_amplifier, where d is high; a vendor beneficiary sees χ negative or near-zero because their d places them as beneficiaries receiving the subsidy of procedural legitimacy.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (decentralized standards development without government or corporate monopoly) was genuinely live when the IETF was chartered and has been substantially solved for the first 30 years of the organization's existence. However, the solving of that problem has exposed a new problem: the mechanism that solved monopoly risk (open process, rough consensus, no formal voting) has become vulnerable to a different form of monopoly—resource-concentrated capture. The rough consensus mechanism requires that organized factions NOT exist (or be kept weak relative to individual technical merit) in order to function as advertised. The founding problem would be satisfied by current IETF operation if vendors were participating as individual implementers on equal terms; instead, they are participating as coordinated coalitions with dedicated liaison staff, patent strategy alignment, and bloc voting. This is not a violation of IETF rules (the rules permit organized participation), but it is a violation of the founding problem's tacit assumption (that no faction would be powerful enough to coordinate procedural outcomes). The constraint avoids the Snare classification and stays in Tangled_Rope territory because there IS real coordination happening (IETF standards do achieve interoperability that would not exist without the mechanism). However, the coordination has become entangled with extraction: vendors coordinate among themselves to produce standards that favor their products, then extract the legitimacy of the mechanism to justify those standards as technically inevitable. The extractiveness metric (0.68) reflects this entanglement. Mandatrophy is NOT resolved in the sense of 'the constraint has outlived its function and should be removed'—the coordinating function is still live. But mandatrophy IS present in the sense of 'the original problem the constraint solved is now solved, and new problems have emerged that the constraint now enables.' A reformed constraint might separate the coordination function (still needed) from the legitimacy extraction (the new harm). This is why the constraint is plausibly a Scaffold (temporary structure meant to be replaced when its founding problem is solved) rather than a stable Tangled Rope; the founding_problem_status of 'contested' captures exactly this uncertainty.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    procedural_capture_vs_genuine_consensus,
    'Is the measured loss of accessibility (consensus appearing despite substantive dissent) evidence of procedural capture by well-resourced vendors, or does rough consensus inherently favor organized coalitions regardless of organized malice?',
    'Historical analysis of rejected proposals and exit interviews with resource-constrained participants: if rejection patterns track to vendor coalition opposition rather than technical argument, capture is the mechanism; if rejection tracks to genuine technical grounds, the outcome may be correct even if procedurally lopsided.',
    'Structural capture (vendors organizing to win regardless of merit) justifies remedies like resource-equalization funding, quota participation rules, or split-vote counting. Procedural bias (rough consensus naturally favors organized coalitions even without malice) justifies softer remedies like transparency requirements and chair training.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedural_capture_vs_genuine_consensus, empirical, 'Whether consensus erosion is intentional capture or structural artifact of open process.').

omega_variable(
    identity_lock_mechanism_in_emerging_communities,
    'For emerging-technology communities coded as identity_locked, is the lock structural (they genuinely cannot exit without losing technical viability) or internalized (they have internalized the narrative that IETF endorsement is necessary for legitimacy when it is actually optional)?',
    'Counterfactual observation: if a community develops a competing standard body with vigorous participation, low cost-of-entry, and achieves adoption without IETF endorsement, the lock was partly internalized. If they remain trapped despite attempting parallel bodies, the lock is structural.',
    'If structural, the constraint is genuinely coercive (they are forced to navigate capture risk because there is no alternative path to scale). If internalized, the constraint''s suppression is partly psychological, and exit-path communication could reduce effective extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_in_emerging_communities, empirical, 'Degree to which emerging-community identity-lock is structural vs. internalized.').

omega_variable(
    commons_degradation_vs_minor_malfunction,
    'Is the rising theater_ratio (0.28 → 0.52) evidence that the IETF consensus mechanism is becoming primarily theatrical—legitimacy extraction with minimal real coordination function—or does it reflect normal procedural overhead increasing as scale grows?',
    'Compare participation diversity and idea diversity in winning standards across the interval: if diversity is collapsing (fewer voices, fewer novel proposals in final standards), theater_ratio rise is symptomatic of real commons degradation; if diversity persists, the overhead is procedural rather than extractive.',
    'True commons degradation (high theater, low diversity) suggests the mechanism is past recovery and needs structural replacement (new governance models, new standards bodies). Procedural overhead (high theater, maintained diversity) suggests governance repairs (chair training, transparency, timeline management) are sufficient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commons_degradation_vs_minor_malfunction, empirical, 'Whether the rising theater ratio is evidence of commons degradation or scalable-procedural overhead.').

omega_variable(
    legitimacy_reading_vs_stewardship_reading,
    'This reading frames the IETF mechanism as vulnerable to capture and increasingly extractive of legitimacy from the commons. The sibling stewardship_reading would frame the same mechanism as a shared commons that well-resourced actors are increasingly damaging through extractive behavior. Are these readings incommensurable (foreclosing each other) or do they coexist as different framings of the same underlying structure?',
    'Test whether a party can coherently hold both readings: can one argue that ''the commons is being extracted from by organized vendors'' AND ''the commons is being stewarded by well-intentioned but increasingly outnumbered gatekeepers''? If yes, coexistence; if the first reading''s core (mechanism is a capture substrate) directly contradicts the second''s (mechanism is a resilient commons), then foreclosure.',
    'Coexistence means reform efforts can address capture without replacing the mechanism entirely. Foreclosure means choosing one reading commits you to either replacement (legitimacy reading, if adopted by majorities, implies the mechanism is unfixable) or renewed stewardship investment (stewardship reading, if adopted, implies capture is fixable via recommitment).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_reading_vs_stewardship_reading, conceptual, 'Reading-to-reading structural relationship: whether the legitimacy_erosion and commons_stewardship readings of the kernel are logically compatible or mutually foreclosing.').

omega_variable(
    measurement_timing_and_artifact,
    'The measurement series shows extractiveness and theater rising sharply from t=0 to t=5, then leveling off. Is this trajectory real (capture happened, then stabilized at a new equilibrium) or an artifact of data availability (early data is unavailable, so t=0 is actually the post-capture state)?',
    'Historical reconstruction of IETF working group composition and decision patterns for t < -5 (early 2000s): if participation was more diverse and decisions less organized then, the rise is real; if early data was simply uncollected, the t=0 value was already post-capture.',
    'Real rise suggests the mechanism degraded within the observation window and may continue degrading toward theater_ratio=1. Artifact (already captured at t=0) suggests the constraint has been in a stable extractive state longer than the measurements show, and the interval captures maintenance, not change.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(measurement_timing_and_artifact, empirical, 'Whether measured metric rise is real temporal change or data-availability artifact.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ietf_openness_commitment__legitimacy_erosion_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ietf_tr_t0, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(ietf_tr_t0, projected).
narrative_ontology:measurement(ietf_tr_t2, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 2, 0.33).
narrative_ontology:measurement_basis(ietf_tr_t2, observed).
narrative_ontology:measurement(ietf_tr_t5, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 5, 0.38).
narrative_ontology:measurement_basis(ietf_tr_t5, observed).
narrative_ontology:measurement(ietf_tr_t10, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 10, 0.45).
narrative_ontology:measurement_basis(ietf_tr_t10, observed).
narrative_ontology:measurement(ietf_tr_t15, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 15, 0.5).
narrative_ontology:measurement_basis(ietf_tr_t15, observed).
narrative_ontology:measurement(ietf_tr_t20, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 20, 0.52).
narrative_ontology:measurement_basis(ietf_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(ietf_be_t0, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(ietf_be_t0, projected).
narrative_ontology:measurement(ietf_be_t2, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 2, 0.48).
narrative_ontology:measurement_basis(ietf_be_t2, observed).
narrative_ontology:measurement(ietf_be_t5, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement_basis(ietf_be_t5, observed).
narrative_ontology:measurement(ietf_be_t10, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement_basis(ietf_be_t10, observed).
narrative_ontology:measurement(ietf_be_t15, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement_basis(ietf_be_t15, observed).
narrative_ontology:measurement(ietf_be_t20, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement_basis(ietf_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(ietf_su_t0, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement_basis(ietf_su_t0, projected).
narrative_ontology:measurement(ietf_su_t2, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 2, 0.59).
narrative_ontology:measurement_basis(ietf_su_t2, observed).
narrative_ontology:measurement(ietf_su_t5, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 5, 0.64).
narrative_ontology:measurement_basis(ietf_su_t5, observed).
narrative_ontology:measurement(ietf_su_t10, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement_basis(ietf_su_t10, observed).
narrative_ontology:measurement(ietf_su_t15, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement_basis(ietf_su_t15, observed).
narrative_ontology:measurement(ietf_su_t20, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(ietf_su_t20, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=20
narrative_ontology:measurement(ietf_grid_01, ietf_openness_commitment__legitimacy_erosion_reading, accessibility_collapse(class), 0, 0.48).
narrative_ontology:measurement(ietf_grid_02, ietf_openness_commitment__legitimacy_erosion_reading, accessibility_collapse(class), 20, 0.52).
narrative_ontology:measurement(ietf_grid_03, ietf_openness_commitment__legitimacy_erosion_reading, accessibility_collapse(individual), 0, 0.35).
narrative_ontology:measurement(ietf_grid_04, ietf_openness_commitment__legitimacy_erosion_reading, accessibility_collapse(individual), 20, 0.48).
narrative_ontology:measurement(ietf_grid_05, ietf_openness_commitment__legitimacy_erosion_reading, accessibility_collapse(organizational), 0, 0.62).
narrative_ontology:measurement(ietf_grid_06, ietf_openness_commitment__legitimacy_erosion_reading, accessibility_collapse(organizational), 20, 0.58).
narrative_ontology:measurement(ietf_grid_07, ietf_openness_commitment__legitimacy_erosion_reading, accessibility_collapse(structural), 0, 0.55).
narrative_ontology:measurement(ietf_grid_08, ietf_openness_commitment__legitimacy_erosion_reading, accessibility_collapse(structural), 20, 0.43).
narrative_ontology:measurement(ietf_grid_09, ietf_openness_commitment__legitimacy_erosion_reading, resistance(class), 0, 0.68).
narrative_ontology:measurement(ietf_grid_10, ietf_openness_commitment__legitimacy_erosion_reading, resistance(class), 20, 0.64).
narrative_ontology:measurement(ietf_grid_11, ietf_openness_commitment__legitimacy_erosion_reading, resistance(individual), 0, 0.58).
narrative_ontology:measurement(ietf_grid_12, ietf_openness_commitment__legitimacy_erosion_reading, resistance(individual), 20, 0.48).
narrative_ontology:measurement(ietf_grid_13, ietf_openness_commitment__legitimacy_erosion_reading, resistance(organizational), 0, 0.44).
narrative_ontology:measurement(ietf_grid_14, ietf_openness_commitment__legitimacy_erosion_reading, resistance(organizational), 20, 0.52).
narrative_ontology:measurement(ietf_grid_15, ietf_openness_commitment__legitimacy_erosion_reading, resistance(structural), 0, 0.52).
narrative_ontology:measurement(ietf_grid_16, ietf_openness_commitment__legitimacy_erosion_reading, resistance(structural), 20, 0.48).
narrative_ontology:measurement(ietf_grid_17, ietf_openness_commitment__legitimacy_erosion_reading, stakes_inflation(class), 0, 0.61).
narrative_ontology:measurement(ietf_grid_18, ietf_openness_commitment__legitimacy_erosion_reading, stakes_inflation(class), 20, 0.72).
narrative_ontology:measurement(ietf_grid_19, ietf_openness_commitment__legitimacy_erosion_reading, stakes_inflation(individual), 0, 0.48).
narrative_ontology:measurement(ietf_grid_20, ietf_openness_commitment__legitimacy_erosion_reading, stakes_inflation(individual), 20, 0.65).
narrative_ontology:measurement(ietf_grid_21, ietf_openness_commitment__legitimacy_erosion_reading, stakes_inflation(organizational), 0, 0.52).
narrative_ontology:measurement(ietf_grid_22, ietf_openness_commitment__legitimacy_erosion_reading, stakes_inflation(organizational), 20, 0.48).
narrative_ontology:measurement(ietf_grid_23, ietf_openness_commitment__legitimacy_erosion_reading, stakes_inflation(structural), 0, 0.4).
narrative_ontology:measurement(ietf_grid_24, ietf_openness_commitment__legitimacy_erosion_reading, stakes_inflation(structural), 20, 0.38).
narrative_ontology:measurement(ietf_grid_25, ietf_openness_commitment__legitimacy_erosion_reading, suppression(class), 0, 0.51).
narrative_ontology:measurement(ietf_grid_26, ietf_openness_commitment__legitimacy_erosion_reading, suppression(class), 20, 0.68).
narrative_ontology:measurement(ietf_grid_27, ietf_openness_commitment__legitimacy_erosion_reading, suppression(individual), 0, 0.42).
narrative_ontology:measurement(ietf_grid_28, ietf_openness_commitment__legitimacy_erosion_reading, suppression(individual), 20, 0.58).
narrative_ontology:measurement(ietf_grid_29, ietf_openness_commitment__legitimacy_erosion_reading, suppression(organizational), 0, 0.64).
narrative_ontology:measurement(ietf_grid_30, ietf_openness_commitment__legitimacy_erosion_reading, suppression(organizational), 20, 0.62).
narrative_ontology:measurement(ietf_grid_31, ietf_openness_commitment__legitimacy_erosion_reading, suppression(structural), 0, 0.61).
narrative_ontology:measurement(ietf_grid_32, ietf_openness_commitment__legitimacy_erosion_reading, suppression(structural), 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ietf_openness_commitment__legitimacy_erosion_reading, information_standard).
narrative_ontology:boltzmann_floor_override(ietf_openness_commitment__legitimacy_erosion_reading, 0.12).
narrative_ontology:affects_constraint(ietf_openness_commitment__legitimacy_erosion_reading, ietf_openness_commitment__commons_stewardship_reading).
narrative_ontology:affects_constraint(ietf_openness_commitment__legitimacy_erosion_reading, ietf_openness_commitment__capture_substrate_reading).

% DUAL FORMULATION NOTE:
% This story is one reading of the contested IETF_OPENNESS_COMMITMENT kernel. The legitimacy_erosion reading emphasizes the mechanism's vulnerability to capture and its function as a legitimacy-extraction target. The commons_stewardship reading emphasizes the mechanism's resilience as a shared epistemic commons. The capture_substrate reading emphasizes that the mechanism was always a coordination substrate where power translates to gatekeeping. All three readings address the same institutional object (rough consensus in IETF working groups) but make different structural claims about its function and trajectory. The constraint families are linked because each reading's claim about the IETF mechanism depends on and influences the others: if legitimacy_erosion is correct (mechanism is being captured), it undermines commons_stewardship (which assumes repair is possible); if commons_stewardship is correct (mechanism is resilient), it forecloses legitimacy_erosion (which assumes the mechanism is past repair); if capture_substrate is correct (mechanism was always a substrate), both legitimacy_erosion and commons_stewardship are misidentifying a constitutional feature as a degradation or corruption.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ietf_openness_commitment__legitimacy_erosion_reading, organized, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
