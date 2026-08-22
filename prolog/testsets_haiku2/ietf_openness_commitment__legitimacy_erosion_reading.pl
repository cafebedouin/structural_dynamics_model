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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ietf_openness_commitment__legitimacy_erosion_reading
 *   human_readable: IETF Rough Consensus Legitimacy Erosion: Procedural Capture
 *   domain: institutional_governance/technology_standards
 *
 * SUMMARY:
 *   The IETF's rough consensus mechanism is presented as a procedurally
 *   neutral way to produce interoperable internet standards through open
 *   participation and merit-based deliberation. This constraint story
 *   instantiates the LEGITIMACY EROSION reading of the contested kernel
 *   'ietf_openness_commitment': it characterizes the rough consensus
 *   mechanism itself as a resource that well-resourced vendor coalitions have
 *   systematically captured to ratify self-serving standards outcomes. The
 *   reading does not deny that the mechanism coordinates technical work;
 *   rather, it argues that the mechanism's procedural legitimacy has been
 *   extracted as a commons and weaponized to legitimize outcomes that the
 *   procedure did not neutrally produce. The structured delta from this
 *   reading: high extractiveness targeting the legitimacy commons itself;
 *   beneficiaries are well-resourced vendors; victims include both the
 *   consensus mechanism's credibility and marginalized stakeholders shut out
 *   of procedural participation. This reading coexists with the
 *   capture_substrate_reading (which emphasizes resource advantage
 *   translating to gatekeeping) and the commons_stewardship_reading (which
 *   emphasizes interoperability as public infrastructure)—all three readings
 *   of the same kernel, each with its own ε, beneficiary structure, and
 *   strategic implications.
 *
 * KEY AGENTS:
 *   - Well-resourced vendor coalitions (institutional power, can sustain long-term WG presence, can implement proposals immediately, shape agendas through draft sponsorship and travel funding)
 *   - Marginalized stakeholders (powerless, identity-locked participation, cannot sustain time investment, excluded from agenda-setting despite caring about interoperability)
 *   - Working group leadership (institutional, incentive-misaligned through vendor employment and funding dependency)
 *   - Consensus mechanism itself (non-agent, legitimate authority whose credibility is extracted and exploited)
 *   - Interoperability commons (non-agent, the collective benefit of open standards, degraded when standards encode vendor preferences)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ietf_openness_commitment__legitimacy_erosion_reading, 0.68).
domain_priors:suppression_score(ietf_openness_commitment__legitimacy_erosion_reading, 0.71).
domain_priors:theater_ratio(ietf_openness_commitment__legitimacy_erosion_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ietf_openness_commitment__legitimacy_erosion_reading, tangled_rope).
narrative_ontology:human_readable(ietf_openness_commitment__legitimacy_erosion_reading, "IETF Rough Consensus Legitimacy Erosion: Procedural Capture").
narrative_ontology:topic_domain(ietf_openness_commitment__legitimacy_erosion_reading, "institutional_governance/technology_standards").

domain_priors:requires_active_enforcement(ietf_openness_commitment__legitimacy_erosion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ietf_openness_commitment__legitimacy_erosion_reading, '25f71e5b-6705-47bb-ac76-c954007ac2b8').
narrative_ontology:cs_kernel_codification('25f71e5b-6705-47bb-ac76-c954007ac2b8', formalized).
narrative_ontology:cs_authority_grounding('25f71e5b-6705-47bb-ac76-c954007ac2b8', lineage).
narrative_ontology:cs_interpretation_layer_present('25f71e5b-6705-47bb-ac76-c954007ac2b8').
narrative_ontology:cs_reading_relation('25f71e5b-6705-47bb-ac76-c954007ac2b8', ietf_openness_commitment__commons_stewardship_reading, influences).
narrative_ontology:cs_reading_relation('25f71e5b-6705-47bb-ac76-c954007ac2b8', ietf_openness_commitment__capture_substrate_reading, coexists_with).
narrative_ontology:cs_axiom('25f71e5b-6705-47bb-ac76-c954007ac2b8', foundational, procedural_legitimacy_extraction_occurs).
narrative_ontology:cs_axiom_status(procedural_legitimacy_extraction_occurs, holdable).
narrative_ontology:cs_axiom_grounding('25f71e5b-6705-47bb-ac76-c954007ac2b8', procedural_legitimacy_extraction_occurs, empirically_contingent).
narrative_ontology:cs_axiom('25f71e5b-6705-47bb-ac76-c954007ac2b8', foundational, consensus_neutral_facade_masks_vendor_dominance).
narrative_ontology:cs_axiom_status(consensus_neutral_facade_masks_vendor_dominance, holdable).
narrative_ontology:cs_axiom_grounding('25f71e5b-6705-47bb-ac76-c954007ac2b8', consensus_neutral_facade_masks_vendor_dominance, empirically_contingent).
narrative_ontology:cs_reference_frame('25f71e5b-6705-47bb-ac76-c954007ac2b8', neutral_open_consensus_mechanism).
narrative_ontology:cs_drift_state('25f71e5b-6705-47bb-ac76-c954007ac2b8', contemporary_vendor_concentration_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('25f71e5b-6705-47bb-ac76-c954007ac2b8', '').
narrative_ontology:cs_kernel_id(ietf_openness_commitment__legitimacy_erosion_reading, ietf_openness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__legitimacy_erosion_reading, well_resourced_vendor_coalitions).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__legitimacy_erosion_reading, incumbent_implementers).
narrative_ontology:constraint_victim(ietf_openness_commitment__legitimacy_erosion_reading, consensus_mechanism_itself).
narrative_ontology:constraint_victim(ietf_openness_commitment__legitimacy_erosion_reading, marginalized_stakeholders).
narrative_ontology:constraint_victim(ietf_openness_commitment__legitimacy_erosion_reading, interoperability_commons).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__legitimacy_erosion_reading, academic_and_nonprofit_researchers).
narrative_ontology:constraint_victim(ietf_openness_commitment__legitimacy_erosion_reading, academic_and_nonprofit_researchers).
narrative_ontology:constraint_victim(ietf_openness_commitment__legitimacy_erosion_reading, working_group_leadership).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Large technology vendors (equipment manufacturers, cloud providers, incumbent network operators) with dedicated standards staff, legal teams, and the ability to implement proposals immediately. They shape working group agendas by sponsoring drafts, funding travel, organizing side meetings, and committing implementation resources to preferred directions. Their dominance is structural: they can absorb the cost of engaging deeply while smaller parties cannot. They justify their leadership as necessary expertise and implementation commitment.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, well_resourced_vendor_coalitions, agenda_setter,
    institutional, biographical, arbitrage, global).

% Contribute foundational research, novel algorithm designs, and security analysis that grounds many standards. They benefit from the open standards that result and from the peer-review legitimacy rough consensus claims to grant. They pay indirectly by accepting outcomes that vendors shape but label as consensus; many researchers lack the resources to sustain long-term WG participation against vendor time investment.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, academic_and_nonprofit_researchers, beneficiary,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(ietf_openness_commitment__legitimacy_erosion_reading, academic_and_nonprofit_researchers, payer).

% Smaller implementers, developing-world operators, civil society organizations, and security researchers outside the vendor coalition. They care deeply about interoperability and open standards as a public good, but lack the bandwidth to maintain steady WG presence. They cannot credibly threaten to withdraw or propose binding alternatives. Their identity as stakeholders in internet infrastructure depends on participation; exit means losing standing in the conversation entirely.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, marginalized_stakeholders, payer,
    powerless, biographical, identity_locked, global).

% Chairs, document editors, and Area Directors who manage discussion, call consensus, and decide which proposals advance. They are under pressure to deliver published standards and to maintain relationships with well-resourced stakeholders who fund travel and provide implementation signals. They are often employed by vendors themselves or depend on vendor-funded conference attendance. Their structural position makes them subject to capture through incentive misalignment.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, working_group_leadership, agenda_setter,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ietf_openness_commitment__legitimacy_erosion_reading, working_group_leadership, payer).

% The Internet Engineering Steering Group and IETF administrative body that approve standards and set procedural rules. They observe working group consensus (rather than set it directly) but can, in principle, intervene to enforce openness. In practice, intervention is rare and faces resistance; the IESG often lacks the granular information to detect consensus capture at the working group level and views intervention as governance overreach.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, ietf_secretariat_and_iesg, observer,
    institutional, generational, constrained, global).

% Internet rights organizations, privacy advocates, and open-source communities who care about standards outcomes but are structurally underrepresented in IETF working groups. They lack standing to propose amendments, attend sporadically due to resource constraints, and find their concerns dismissed as outside the IETF's technical scope—even when standards choices have direct civil liberties consequences.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, civil_society_and_open_advocates, excluded,
    moderate, generational, constrained, global).

% The procedural legitimacy and epistemic credibility that the rough consensus label carries. Not an actor, but a commons-character entity: the mechanism's authority depends on being perceived as genuinely open and not gamed. When well-resourced parties systematically shape consensus through procedural dominance while the mechanism claims neutrality, the mechanism loses the integrity that justifies its legitimacy.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, consensus_mechanism_itself, payer,
    analytical, generational, analytical, universal).
narrative_ontology:stakeholder_non_agent(ietf_openness_commitment__legitimacy_erosion_reading, consensus_mechanism_itself).

% The collective benefit of open, non-proprietary standards that permit any implementer to build compatible systems. When standards encode vendor preferences rather than neutral interoperability requirements, the commons is degraded: implementers face proprietary lock-in, compatibility breaks, and switching costs increase.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, interoperability_commons, payer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(ietf_openness_commitment__legitimacy_erosion_reading, interoperability_commons).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ietf_openness_commitment__legitimacy_erosion_reading, well_resourced_vendor_coalitions).
narrative_ontology:fixing_cost_class(ietf_openness_commitment__legitimacy_erosion_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The rough consensus mechanism is meant to pool distributed technical expertise across competing implementers and constrain vendor capture through procedural neutrality. It coordinates by claiming that outcomes reflect technical merit and collective legitimacy rather than market power. This coordination function is real: complex internet protocols require input from security researchers, hardware manufacturers, software developers, and end-user operators; centralized or vendor-dominated standard-setting would miss critical technical constraints. The mechanism enables this pooling by providing a forum where any stakeholder can speak, and where decisions are made by consensus rather than weighted voting.
% TRANSFER_FUNCTION: Well-resourced vendor coalitions transfer costs to the interoperability commons and marginalized stakeholders. The transfer has two components: (1) encoded technical preferences (vendors gain standards that reduce their implementation burden, privilege their architectures, lock out competitors—these are economically valuable preferences); (2) extracted legitimacy (by invoking procedural neutrality to ratify vendor-shaped outcomes, vendors extract the mechanism's credibility as a resource, deploying it to make self-serving standards appear merit-based and universally beneficial). Marginalized stakeholders bear the cost of non-participation without compensation, the cost of incompatibility when their concerns are dismissed as out-of-scope, and the opportunity cost of a mechanism that no longer credibly represents their interests.
% ABSENT_VOICES: Smaller implementers and developing-world operators are absent because they cannot sustain the time investment to maintain steady working group presence against vendor staff. Civil society organizations and open-source communities are absent from most technical discussions, even though standards have security, privacy, and intellectual-property consequences that affect their constituencies. Their absence is partly structural (cost of attendance, unpaid participation) and partly by design (they are framed as non-technical and out-of-scope by working group chairs and the broader IETF culture). Their objections would center on: access to standards by smaller implementers (cost barriers); privacy and surveillance implications of certain proposals; intellectual-property consequences of technical choices; and the legitimacy cost to the mechanism itself of excluding those voices.
% DISAPPEARANCE_RATIONALE: If the rough consensus mechanism's legitimacy and enforcement vanished overnight (replaced by pure vendor voting, closed technical bodies, or fragmented fork-based standards), the internet's architecture would reorganize around vendor-specific ecosystems. The IETF's authority depends entirely on the perception that its outcomes represent distributed expertise and open process; without that legitimacy, the institution cannot bind implementers. Smaller implementers would abandon participation, open-source communities would fork, and interoperability would depend on bilateral negotiations between large vendors rather than universal standards. The legitimacy itself—not the coordination capacity—is what makes the mechanism binding.
% FOUNDING_PROBLEM: In the 1980s–1990s, internet fragmentation threatened by vendor lock-in required a neutral forum for standard-setting. The IETF's rough consensus mechanism was designed to let any implementer propose standards and have their concerns heard, preventing any single vendor from dictating the architecture.
% FOUNDING_PROBLEM_CORROBORATION: Large vendors and IETF leadership claim the founding problem remains live—they cite emerging threats (security, IPv6 adoption, 5G coordination), the complexity of modern standards work, and the need for expert-driven processes. Academic analyses of IETF participation data (cited in community reviews and RFC editorials) show the founding problem of individual vendor lock-in has been substantially solved; the current problem is coalition capture. Marginalized stakeholders and open advocates attest that the mechanism no longer serves its intended function of preventing capture; smaller implementers confirm they cannot sustain presence. The corroboration for the erosion reading comes from participation surveys, working group attendance data, and documented cases where vendor-aligned proposals advance despite scientific or interoperability concerns.
narrative_ontology:disappearance_verdict(ietf_openness_commitment__legitimacy_erosion_reading, world_rearranges).
narrative_ontology:founding_problem_status(ietf_openness_commitment__legitimacy_erosion_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ietf_openness_commitment__legitimacy_erosion_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ietf_openness_commitment__legitimacy_erosion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ietf_openness_commitment__legitimacy_erosion_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness rises from 0.42 to 0.68 over the interval, indicating progressive capture: as vendor coalitions develop more sophisticated coordination strategies (shared drafts, coordinated implementations, side-meeting scheduling), the mechanism encodes more self-serving preferences while claiming neutrality. Theater rises from 0.35 to 0.62: the ratio of procedural legitimacy-work (framing outcomes as consensus, citing openness, invoking merit-based justification) to actual functional coordination work (solving genuine technical problems) increases—a signal that procedural legitimacy itself has become the extracted good. Suppression rises from 0.48 to 0.71: the structural cost to marginalized stakeholders and smaller implementers of resisting vendor-shaped outcomes increases (they cannot afford to maintain presence, cannot credibly propose counter-coalitions, face dismissal as non-expert). The plateau after t=30 suggests capture has matured: the mechanism's legitimacy erosion has stabilized at a high level, with procedural resistance now institutionalized. Accessibility_collapse at 0.48 indicates alternatives exist but are costly: smaller implementers could fork, civil society could establish rival standards bodies, but the cost of coordination and the switching cost of internet fragmentation remain prohibitive. Resistance at 0.74 is high because marginalized stakeholders and open advocates actively push back (community calls for diversity, documented critiques of vendor dominance, governance reform proposals)—but this resistance is channeled back into the mechanism itself, where it can be absorbed and rhetorically neutralized.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (marginalized stakeholders, consensus mechanism, interoperability commons) and the beneficiary seats (well-resourced vendors) experience this constraint through radically different lenses. From the vendor seat, the IETF is an open forum they have strengthened through participation and implementation commitment. From the marginalized seat, the IETF is a venue where they lose voice precisely because they cannot afford constant presence. The gap is not resolvable by claiming the mechanism is neutral—neutrality is what is at stake. The engine's per-seat computation should show the beneficiary seat computing rope or shallow tangled rope, while the payer seats compute deep tangled rope or snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Well-resourced vendor coalitions are near the beneficiary end (d ≈ 0.1–0.2): the constraint amplifies their power, encodes their preferences, and invokes procedural legitimacy to make those outcomes appear neutral. Marginalized stakeholders are near the target end (d ≈ 0.85–0.95): they bear the cost of non-participation without choice (identity-locked exit means losing standing entirely), face suppression through structural exclusion, and have their concerns subordinated to vendor-defined technical scope. Academic researchers sit near symmetric (d ≈ 0.5): they genuinely benefit from open standards while accepting that vendor-shaped outcomes are invoked in their name. The consensus mechanism itself (d = analytical, not computed) and the interoperability commons (d = analytical) are victims, not seats with directionality. Working group leadership is the most ambiguous: they benefit from vendor relationships (high d toward vendors) but are also constrained by the need to claim neutrality (moderate d toward marginalized stakeholders they must at least appear to include)—no directionality override is needed because the structural data (institutional power, constrained exit, dual beneficiary/payer role) produces the right computation naturally.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint shows mandatrophy symptoms (founding problem status = contested) but is NOT in resolved mandatrophy. The founding problem was genuine: vendor fragmentation threatened interoperability in the 1980s. The mechanism was built to solve it through procedural neutrality. However, the founding problem has shifted: the current problem is not individual vendor lock-in (which the dominant vendors have solved for themselves through the coalition) but coalition capture of the mechanism itself. The constraint persists because well-resourced vendors benefit from invoking its legitimacy to ratify outcomes, while marginalized stakeholders and open advocates cannot coordinate a sufficient withdrawal. Mandatrophy would be fully resolved only if the founding problem (vendor fragmentation) returned and the mechanism could no longer address it—i.e., if the coalitions fragmented and rough consensus collapsed entirely. Instead, we see a hybrid: the mechanism still solves the original problem (for the dominant coalition) while creating a new extraction problem (for everyone else). This is a signature tangled_rope dynamic: genuine coordination function + asymmetric extraction of legitimacy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_extraction_vs_emergent_incompetence,
    'Is the consensus mechanism''s deviation from neutrality due to deliberate organized capture by vendor coalitions, or is it an emergent result of structural incentives (vendor funding of travel, employment of chairs) that no single party orchestrated?',
    'Documentary and interview evidence: leaked communications from vendor strategy meetings, testimony from WG chairs about pressure from employers, tracking of draft sponsorship and implementation commitment correlations. If coordinated strategy is documented, deliberate capture is established; if only incentive misalignment is found, it is systemic rather than organized.',
    'Deliberate capture implies the mechanism is a snare masquerading as rope; systemic incentive misalignment implies tangled_rope with less malign intent. The classification difference determines whether remedies should focus on transparency/accountability (snare) or structural incentive reform (tangled_rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_extraction_vs_emergent_incompetence, empirical, 'Whether consensus capture is orchestrated or emergent from structural incentives.').

omega_variable(
    vendor_coalition_coherence,
    'Do well-resourced vendors act as a coherent coalition with shared preferences, or do they independently pursue conflicting interests that happen to dominate because of their size?',
    'Analysis of working group voting patterns, draft sponsorship alignment, and implementation commitments. If vendors consistently support overlapping proposals despite nominal competition, coalition coherence is established; if their votes diverge randomly, dominance is independent rather than coordinated.',
    'If coherent, the constraint is a tangled_rope with organized beneficiaries (the coalition) deliberately shaping outcomes. If independent, it is a piton—vendor dominance persists through structural inertia, not because beneficiaries maintain it. The coherence question determines whether the constraint is sustainably enforceable or brittle to defection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vendor_coalition_coherence, empirical, 'Whether vendor dominance reflects organized coalition or independent size advantage.').

omega_variable(
    marginalized_voice_structural_vs_capability,
    'Are marginalized stakeholders absent from IETF working groups because procedural structures exclude them (structural suppression), or because they lack technical expertise and resource capacity (capability deficit)?',
    'Intervention study: lower the cost of participation (remote attendance, paid representation support) and measure whether marginalized stakeholder engagement increases. If participation increases substantially, the barrier is structural; if it does not, the barrier is capability-based.',
    'If structural, the suppression metric is correctly authored and the constraint is a true tangled_rope with enforceable exclusion. If capability-based, suppression is partly internalized (the excluded parties have internalized the belief they do not belong) and partly structural. Post-intervention measurement of suppression persistence would discriminate between mechanisms.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(marginalized_voice_structural_vs_capability, empirical, 'Whether marginalized-voice absence is caused by structural barriers or capability constraints.').

omega_variable(
    kernel_reading_foreclosure_possibility,
    'Can the commons_stewardship_reading and the legitimacy_erosion_reading coexist within a single IETF framework, or does accepting this reading''s claim (that the mechanism''s legitimacy has been extracted) logically foreclose the stewardship reading''s claim (that the mechanism preserves open interoperability)?',
    'Philosophical/structural analysis: if the mechanism genuinely preserves open interoperability for all implementers (stewardship claim) while simultaneously extracting legitimacy to ratify vendor preferences (this reading''s claim), both claims can be true—different aspects of the same mechanism. If the mechanism has lost its interoperability-preservation function (this reading''s diagnosis), then the stewardship reading''s core claim is falsified.',
    'If the readings are compatible, they coexist and the engine should show coexists_with relations. If incompatible, one reading forecloses the other, and the engine should show forecloses relations. The outcome determines whether the kernel admits multiple readings or whether this reading''s diagnosis invalidates the stewardship reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_possibility, conceptual, 'Whether the legitimacy erosion and commons stewardship readings are logically compatible or mutually foreclosing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ietf_openness_commitment__legitimacy_erosion_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ietf_tr_t0, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(ietf_tr_t0, observed).
narrative_ontology:measurement(ietf_tr_t5, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 5, 0.41).
narrative_ontology:measurement_basis(ietf_tr_t5, observed).
narrative_ontology:measurement(ietf_tr_t10, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 10, 0.47).
narrative_ontology:measurement_basis(ietf_tr_t10, observed).
narrative_ontology:measurement(ietf_tr_t15, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 15, 0.52).
narrative_ontology:measurement_basis(ietf_tr_t15, observed).
narrative_ontology:measurement(ietf_tr_t20, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 20, 0.56).
narrative_ontology:measurement_basis(ietf_tr_t20, observed).
narrative_ontology:measurement(ietf_tr_t25, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 25, 0.59).
narrative_ontology:measurement_basis(ietf_tr_t25, observed).
narrative_ontology:measurement(ietf_tr_t30, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 30, 0.61).
narrative_ontology:measurement_basis(ietf_tr_t30, observed).
narrative_ontology:measurement(ietf_tr_t40, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 40, 0.62).
narrative_ontology:measurement_basis(ietf_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(ietf_be_t0, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(ietf_be_t0, observed).
narrative_ontology:measurement(ietf_be_t5, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement_basis(ietf_be_t5, observed).
narrative_ontology:measurement(ietf_be_t10, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement_basis(ietf_be_t10, observed).
narrative_ontology:measurement(ietf_be_t15, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 15, 0.59).
narrative_ontology:measurement_basis(ietf_be_t15, observed).
narrative_ontology:measurement(ietf_be_t20, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement_basis(ietf_be_t20, observed).
narrative_ontology:measurement(ietf_be_t25, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement_basis(ietf_be_t25, observed).
narrative_ontology:measurement(ietf_be_t30, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(ietf_be_t30, observed).
narrative_ontology:measurement(ietf_be_t40, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(ietf_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(ietf_su_t0, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(ietf_su_t0, observed).
narrative_ontology:measurement(ietf_su_t5, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 5, 0.54).
narrative_ontology:measurement_basis(ietf_su_t5, observed).
narrative_ontology:measurement(ietf_su_t10, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 10, 0.59).
narrative_ontology:measurement_basis(ietf_su_t10, observed).
narrative_ontology:measurement(ietf_su_t15, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 15, 0.63).
narrative_ontology:measurement_basis(ietf_su_t15, observed).
narrative_ontology:measurement(ietf_su_t20, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 20, 0.66).
narrative_ontology:measurement_basis(ietf_su_t20, observed).
narrative_ontology:measurement(ietf_su_t25, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 25, 0.69).
narrative_ontology:measurement_basis(ietf_su_t25, observed).
narrative_ontology:measurement(ietf_su_t30, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(ietf_su_t30, observed).
narrative_ontology:measurement(ietf_su_t40, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(ietf_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ietf_openness_commitment__legitimacy_erosion_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ietf_openness_commitment__legitimacy_erosion_reading, 0.14).
narrative_ontology:affects_constraint(ietf_openness_commitment__legitimacy_erosion_reading, ietf_openness_commitment__commons_stewardship_reading).
narrative_ontology:affects_constraint(ietf_openness_commitment__legitimacy_erosion_reading, ietf_openness_commitment__capture_substrate_reading).
narrative_ontology:affects_constraint(ietf_openness_commitment__legitimacy_erosion_reading, internet_standards_legitimacy_commons).
narrative_ontology:affects_constraint(ietf_openness_commitment__legitimacy_erosion_reading, vendor_coalition_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel 'ietf_openness_commitment.' The commons_stewardship_reading instantiates the same kernel as coordination for interoperability benefit to all implementers (ε ≈ 0.15, rope). The capture_substrate_reading instantiates the same kernel as resource-advantage capture mechanism (ε ≈ 0.72, tangled_rope). All three readings share the same kernel text (IETF Bylaws) and authority structure but diagnose different structural and ethical consequences. Each has its own constraint_id, distinct ε, and unique stakeholder roles. They are linked by affects_constraints to enable contamination propagation analysis: if legitimacy erosion is established, the interoperability commons is degraded (stewardship reading's coordination function is undermined); if the capture substrate reading's resource-advantage mechanism is documented, legitimacy erosion becomes more plausible.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
