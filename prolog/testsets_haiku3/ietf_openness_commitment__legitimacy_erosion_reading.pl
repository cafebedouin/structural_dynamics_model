% ============================================================================
% CONSTRAINT STORY: ietf_openness_commitment__legitimacy_erosion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: IETF Openness Commitment—Legitimacy Erosion Reading
 *   domain: institutional/technological/governance
 *
 * SUMMARY:
 *   The IETF's rough-consensus mechanism is presented as a procedurally open,
 *   resource-agnostic standard-setting process. This reading instantiates the
 *   claim that the mechanism itself has become a site of extractive capture:
 *   well-resourced vendor coalitions translate corporate advantage into
 *   procedural legitimacy, ratifying self-serving specifications while
 *   extracting the IETF's credibility halo. The victim is not a tangible
 *   resource but the legitimacy commons—the credible claim that IETF
 *   represents distributed consensus. As outcomes increasingly reflect vendor
 *   power rather than diverse input, the mechanism's foundational
 *   justification (openness, interoperability for all) erodes. This reading
 *   differs from the 'commons stewardship' reading (which treats IETF as
 *   successfully preserving public goods despite vendor incentives) and the
 *   'capture substrate' reading (which treats standards as a coordination
 *   platform where vendor advantage is encoded). This reading focuses on the
 *   legitimacy extraction mechanism itself and its erosion over time.
 *
 * KEY AGENTS:
 *   - Well-resourced vendor coalitions: multinational tech firms with dedicated standards teams, market leverage, exit options via forking or informal standard-setting. Structurally extract legitimacy.
 *   - Under-resourced implementers: open-source projects, smaller vendors, academic implementers. Constrained by participation costs; bear hidden compatibility burdens. Victims.
 *   - IETF administrative leadership: preside over 'rough consensus' calls, set agendas, manage procedure. Constrained to enforce openness commitments while lacking visibility into coalition coordination. Agenda-setters; theatrical enforcement.
 *   - Consensus mechanism credibility: the non-agent entity whose erosion is the extraction. Measured by divergence between the legitimacy claim and actual outcomes.
 *   - Public-interest alignment: the structural goal of broad interoperability, eroded as outcomes encode vendor advantage.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ietf_openness_commitment__legitimacy_erosion_reading, 0.68).
domain_priors:suppression_score(ietf_openness_commitment__legitimacy_erosion_reading, 0.72).
domain_priors:theater_ratio(ietf_openness_commitment__legitimacy_erosion_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ietf_openness_commitment__legitimacy_erosion_reading, tangled_rope).
narrative_ontology:human_readable(ietf_openness_commitment__legitimacy_erosion_reading, "IETF Openness Commitment—Legitimacy Erosion Reading").
narrative_ontology:topic_domain(ietf_openness_commitment__legitimacy_erosion_reading, "institutional/technological/governance").

domain_priors:requires_active_enforcement(ietf_openness_commitment__legitimacy_erosion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ietf_openness_commitment__legitimacy_erosion_reading, '9dd851c1-0510-41f5-8e21-24cb4a09a93e').
narrative_ontology:cs_kernel_codification('9dd851c1-0510-41f5-8e21-24cb4a09a93e', fixed_text).
narrative_ontology:cs_authority_grounding('9dd851c1-0510-41f5-8e21-24cb4a09a93e', extraction).
narrative_ontology:cs_interpretation_layer_present('9dd851c1-0510-41f5-8e21-24cb4a09a93e').
narrative_ontology:cs_reading_relation('9dd851c1-0510-41f5-8e21-24cb4a09a93e', ietf_openness_commitment__commons_stewardship_reading, coexists_with).
narrative_ontology:cs_reading_relation('9dd851c1-0510-41f5-8e21-24cb4a09a93e', ietf_openness_commitment__capture_substrate_reading, influences).
narrative_ontology:cs_axiom('9dd851c1-0510-41f5-8e21-24cb4a09a93e', foundational, rough_consensus_legitimacy_is_extractable_commodity).
narrative_ontology:cs_axiom_status(rough_consensus_legitimacy_is_extractable_commodity, holdable).
narrative_ontology:cs_axiom_grounding('9dd851c1-0510-41f5-8e21-24cb4a09a93e', rough_consensus_legitimacy_is_extractable_commodity, empirically_contingent).
narrative_ontology:cs_axiom('9dd851c1-0510-41f5-8e21-24cb4a09a93e', foundational, resource_asymmetry_determines_consensus_outcomes).
narrative_ontology:cs_axiom_status(resource_asymmetry_determines_consensus_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('9dd851c1-0510-41f5-8e21-24cb4a09a93e', resource_asymmetry_determines_consensus_outcomes, empirically_contingent).
narrative_ontology:cs_reference_frame('9dd851c1-0510-41f5-8e21-24cb4a09a93e', genuine_distributed_consensus).
narrative_ontology:cs_drift_state('9dd851c1-0510-41f5-8e21-24cb4a09a93e', contemporary_vendor_resource_dominance, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9dd851c1-0510-41f5-8e21-24cb4a09a93e', '').
narrative_ontology:cs_kernel_id(ietf_openness_commitment__legitimacy_erosion_reading, ietf_openness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__legitimacy_erosion_reading, well_resourced_vendor_coalitions).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__legitimacy_erosion_reading, incumbent_standards_gatekeepers).
narrative_ontology:constraint_victim(ietf_openness_commitment__legitimacy_erosion_reading, consensus_mechanism_credibility).
narrative_ontology:constraint_victim(ietf_openness_commitment__legitimacy_erosion_reading, under_resourced_implementers).
narrative_ontology:constraint_victim(ietf_openness_commitment__legitimacy_erosion_reading, public_interest_alignment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Large multinational technology vendors with dedicated standards teams, legal resources, and market leverage. They participate intensively in IETF working groups, sponsor hosting, author competing proposals, and coordinate bloc positions. They extract legitimacy by translating resource advantage into procedural outcomes: their drafted text becomes the 'rough consensus,' their objections carry disproportionate weight in last-call review, their implementation timeline drives feature prioritization. Exit: they can fork standards informally or withdraw from IETF while maintaining market dominance.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, well_resourced_vendor_coalitions, agenda_setter,
    institutional, generational, arbitrage, global).

% The non-agent entity whose credibility is extracted: the idea that 'rough consensus' meaningfully reflects diverse implementer opinion, that the process is open to all equally, that outcomes are decoupled from vendor power. As this credibility erodes—as outcomes increasingly appear pre-determined by vendor coalitions—the mechanism's legitimacy base decays. The extraction is targeting the legitimacy commons itself, not a tangible resource.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, consensus_mechanism_credibility, payer,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(ietf_openness_commitment__legitimacy_erosion_reading, consensus_mechanism_credibility).

% Smaller vendors, open-source projects, academic implementers, and single-engineer shops that cannot sustain full-time IETF participation. They attend meetings sporadically, cannot maintain detailed technical commentary on competing proposals, lack legal review capacity, and cannot coordinate bloc positions. They face a choice: accept outcomes shaped without their input, or expend scarce resources to participate in a process increasingly dominated by coalitions they cannot match. Their implementation constraints are invisible in the 'rough consensus' framing.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, under_resourced_implementers, payer,
    moderate, biographical, constrained, regional).

% Organizations that benefit from ratifying lock-in, compatibility moats, or patent-friendly language. They benefit from the IETF's legitimacy halo: a standard bearing the IETF mark is trusted by governments, procurement authorities, and users to be interoperable and unpatented. They capture the legitimacy to encode advantage (patent licensing hooks, proprietary extensions, specification vagueness that favors their implementation). Exit: they have sufficient market power to absorb standards changes or influence future IETF directions.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, incumbent_standards_gatekeepers, beneficiary,
    institutional, generational, mobile, global).

% The structural goal that standards should serve broad interoperability, not vendor lock-in. As the mechanism becomes extractive, this alignment erodes: standards increasingly encode vendor advantage, feature specifications are shaped by who shows up to meetings, and the 'rough consensus' label obscures whose interests it actually represents. The extraction is indirect—targeting the perceived legitimacy of outcomes—but the victim is the public-interest framing that justified IETF as an alternative to proprietary standards bodies.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, public_interest_alignment, payer,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(ietf_openness_commitment__legitimacy_erosion_reading, public_interest_alignment).

% Chairs, directors, and process-administration actors who formally define and enforce working-group procedures. They preside over 'rough consensus' calls, set agendas, schedule meetings (affecting who can attend), and interpret ambiguous procedure language. They nominally enforce openness commitments but lack visibility into (or leverage over) vendor coalition coordination. Their enforcement is theatrical: announcing commitment to openness while outcomes reflect resource asymmetry. Their constrained exit: reforming the mechanism risks fragmenting the standards body entirely.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, ietf_administrative_leadership, agenda_setter,
    institutional, generational, constrained, global).

% Individual and organizational participants who take the 'rough consensus' claim at face value, attend meetings expecting democratic process, and accept outcomes because they believe the mechanism is fair. As they observe outcomes diverging from the process's legitimacy claim, they either recognize the mechanism's extractive character or maintain faith that procedural safeguards will reassert themselves. Their analytical position reveals the mechanism's erosion to those who look.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, ietf_formal_membership, observer,
    organized, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ietf_openness_commitment__legitimacy_erosion_reading, well_resourced_vendor_coalitions).
narrative_ontology:fixing_cost_class(ietf_openness_commitment__legitimacy_erosion_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Distributed technical consensus-building for interoperable internet protocols: agents with diverse implementation interests, geographic distribution, and resource levels convene asynchronously and synchronously to draft standards that coordinate behavior across vendors without proprietarization, ensuring network effects benefit the entire ecosystem.
% TRANSFER_FUNCTION: Transfers procedural legitimacy from the consensus mechanism to vendor-favorable outcomes: well-resourced coalitions extract the legitimacy halo of 'rough consensus' to ratify specifications that encode their competitive advantage, lock-in, or patent licensing hooks. The cost is borne by the mechanism's credibility itself and by under-resourced implementers who bear compatibility burdens from inadequate input.
% ABSENT_VOICES: Open-source projects without corporate backing, developing-nation implementers, academic researchers, and small-business vendors are structurally excluded from intensive participation by resource asymmetry (not formal rules—the exclusion is procedural, not written). They would object to outcomes reflecting vendor coalition priorities if present and organized, but their absence is constitutive of how 'rough consensus' is constructed.
% DISAPPEARANCE_RATIONALE: If the IETF rough-consensus mechanism and its legitimacy shield vanished, standards development would fragment: vendor coalitions would publish de-facto standards directly (as happens in unstandardized domains), implementers would lose the credibility halo of 'open' ratification (governments and users trust IETF more than ISO or consortia), and the gap between the theory of distributed consensus and the practice of resource-driven outcomes would become explicit—forcing either radical procedural reform or migration to alternative governance models.
% FOUNDING_PROBLEM: Early internet standardization was proprietary and fragmented: different vendors encoded incompatible protocol choices, network interoperability was contingent on bilateral agreements, and centralized standards bodies (ITU, ISO) operated at state-negotiation speeds and served incumbent telecom interests, not software implementers.
% FOUNDING_PROBLEM_CORROBORATION: IETF founding documents and institutional history from the 1980s attest the problem as live. Contemporary advocates (ISOC, many open-source projects) attest the founding problem persists where vendor standards fork. However, large technology vendors and incumbent gatekeepers attest the problem is substantially solved—IETF now produces the dominant internet protocols—and characterize current concerns as redistributive complaints by under-resourced parties. Academic studies of IETF participation patterns (Eggert et al., RFC 7282 commentary literature) corroborate the resource-asymmetry picture from outside the benefiting parties.
narrative_ontology:disappearance_verdict(ietf_openness_commitment__legitimacy_erosion_reading, world_rearranges).
narrative_ontology:founding_problem_status(ietf_openness_commitment__legitimacy_erosion_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ietf_openness_commitment__legitimacy_erosion_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored as 0.68 at interval end, rising from 0.48 at t0. The extraction targets not tangible resources but the legitimacy commons: vendor coalitions ratify specifications bearing the IETF mark of openness while encoding lock-in or patent-friendly language. The mechanism works because IETF legitimacy is valuable—governments, procurement authorities, users trust IETF more than proprietary consortia. Suppression is higher (0.72) because the extraction depends on sustaining the 'rough consensus' fiction despite resource asymmetry: under-resourced implementers are suppressed not by formal rules but by participation costs and invisible coalition coordination. Theater is high (0.58) because IETF administrative procedures announce openness commitments (RFC 7282 on rough consensus, open-membership policies) while outcomes reflect vendor power. The coercion grid shows asymmetric effect: well-resourced vendors face low accessibility_collapse (they can arbitrage into informal standards if IETF fails) and low stakes_inflation (their market position is stable); under-resourced implementers face high accessibility_collapse (IETF is their only credible standards venue) and high stakes_inflation (non-participation risks compatibility irrelevance). Suppression is concentrated at organizational and class levels: vendor coalitions suppress organized attempts at broader governance, and the class of under-resourced actors as a whole bears suppression through process design (meeting timing, documentation depth, voting conventions). Resistance is distributed across all levels but highest at organizational (working-group chairs, open-source projects pushing back on outcomes) and structured around the mechanism's erosion narrative (people recognize the divergence between promise and performance).
 *
 * PERSPECTIVAL GAP:
 *   From the vendor-coalition perspective: 'We participate intensively, author high-quality specifications, and deserve influence proportional to our expertise and implementation investment. The mechanism is working; outcomes reflect substantive technical merit.' From the under-resourced-implementer perspective: 'We cannot match the resource commitment vendors dedicate to standards work. Our concerns are not heard. The mechanism appears fair but systematically excludes our input.' From the IETF administrative perspective: 'We follow open procedures and enforce fairness as best we can, but we lack visibility into vendor coordination and cannot mandate equitable participation.' The engine computes these as different directionalities: vendors near 0.1 (beneficiary), under-resourced implementers near 0.85 (target), administrators near 0.4 (asymmetrically placed between enforcement and limitation). The legitimacy commons is a victim (role=payer, non-agent) because its erosion is the extraction itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Well-resourced vendor coalitions: Power=institutional, time_horizon=generational (standards they encode last decades), exit_options=arbitrage (they can fork or set standards informally). These attributes push d toward 0.0 (beneficiary end). They extract legitimacy—the IETF mark of openness adds credibility to their specifications—without bearing the cost of erosion. Directionality override not needed; the derivation from power + exit already captures this. Under-resourced implementers: Power=moderate, time_horizon=biographical (their products must interoperate on IETF-shaped standards), exit_options=constrained (they lack resources to set alternative standards). These push d toward 0.85 (target end). They bear the cost of non-participation (compatibility risk, specification tailoring) and the implicit taxation of participation (time spent in meetings rather than building products). Consensus mechanism credibility: non-agent, but structurally the victim. Its erosion is measured by the rising theater_ratio and extractiveness_accumulation. Public-interest alignment: non-agent victim. As vendor power concentrates outcomes, the public-good framing of 'open standards for all' decays. IETF administrative leadership: Sits near 0.45 (moderately constrained). They benefit from procedural legitimacy (their role is meaningful because the mechanism is trusted), but they are constrained by the limits of what they can enforce and the power asymmetries they cannot redress. No override needed.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading frames the IETF rough-consensus mechanism as having evolved from genuine coordination (solving the fragmentation problem of proprietary standards, enabling distributed input) toward coordinated extraction (the mechanism's legitimacy is now the commodity being extracted by vendor coalitions). The mandatrophy claim is: the founding problem (fragmented, proprietary standards) is substantially solved—IETF standards dominate internet infrastructure—but the arrangement persists as rent-collection on legitimacy. The founding_problem_status is 'contested' because incumbent gatekeepers attest the problem is solved and IETF is the solution, while critics attest the problem has transmuted (from 'no standards' to 'captured standards'). The disappearance_verdict is 'world_rearranges' because if IETF lost credibility, standards development would fragment again (vendors would fork, proprietary standards would re-emerge), showing that arrangements do depend on the mechanism. However, the dependence is now mediated by legitimacy, not by genuine coordination necessity. The tangled_rope type reflects this: there is a real coordination function (distributed protocol development), but it is now dominated by asymmetric extraction (vendor coalitions capturing legitimacy). The mechanism persists because vendors benefit from the IETF mark, under-resourced implementers cannot coordinate alternatives, and IETF administrative leadership is constrained to enforce openness without visibility into coalition structures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_asymmetry_necessity,
    'Is resource asymmetry in IETF participation a structural feature of internet-scale standardization, or is it a policy choice that could be redistributed?',
    'Empirical comparison: study alternative standards bodies (ITU, ISO, W3C, 3GPP) for resource-distribution patterns. If asymmetry is universal, it is structural; if some bodies achieve more balanced participation, policy levers exist.',
    'If structural, the extractive character is an unavoidable feature of any standards process; if policy-contingent, remedies (subsidized participation, asynchronous decision-making, weighted voting) could reduce extraction. The claim shifts from ''capture of mechanism'' to ''design flaw enabling capture.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_asymmetry_necessity, empirical, 'Whether participation asymmetry reflects structural necessity or design choices').

omega_variable(
    legitimacy_commons_resilience,
    'At what point does divergence between the legitimacy claim (rough consensus reflects diverse input) and actual outcomes (outcomes reflect vendor power) cause the IETF mark to lose credibility value?',
    'Monitor vendor and implementer behavior: do vendors continue to seek IETF ratification as their standards are questioned, or do they migrate to consortia? Do governments and procurement authorities continue to trust IETF or seek alternative assurance? The threshold is where the legitimacy extraction becomes self-undermining.',
    'If credibility erodes below some threshold, vendors lose the incentive to capture the mechanism (legitimacy no longer valuable); the mechanism reverts to genuine coordination or dies. If credibility proves resilient (trusted despite visible capture), extraction can continue indefinitely. This determines whether the mechanism is self-correcting or requires external intervention.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_commons_resilience, empirical, 'Whether IETF''s legitimacy halo can be extracted indefinitely or has a finite resilience').

omega_variable(
    commons_stewardship_alternative,
    'Could the IETF mechanism be read as successfully stewardship of a public goods coordination function, with vendor advantages constituting acceptable redistribution payments?',
    'The commons_stewardship_reading of this kernel claims exactly this: public goods (interoperability, open standards) are preserved; vendor advantage is a legitimate cost of participation. This omega records the conceptual alternative: does legitimacy_erosion reading foreclose the stewardship reading, or do they coexist as different framings of the same facts?',
    'If they coexist (different framings, both internally coherent), then the classification divergence is observational—the engine computes extraction and the reading selects what it means. If legitimacy_erosion forecloses stewardship (the erosion claim is incompatible with successful stewardship), the readings are logically exclusive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commons_stewardship_alternative, conceptual, 'Whether legitimacy erosion and commons stewardship are coexistent or foreclosed readings').

omega_variable(
    procedural_safeguard_adequacy,
    'Can the IETF''s announced procedural safeguards (RFC 7282 on rough consensus, open-membership policy, transparency about decision-making) prevent or detect legitimacy extraction if administrators apply them vigilantly?',
    'Historical case studies: instances where administrators invoked safeguards to overturn outcomes that appeared to reflect vendor power (or instances where they conspicuously did not). The test is whether the safeguards are capable tools or theater.',
    'If safeguards are adequate, the extraction mechanism requires administrative collusion or inattention (making it a governance failure, not a structural flaw). If safeguards are inherently insufficient (theater), the mechanism is structurally extractive. This determines whether reform is procedural (improve enforcement) or structural (redesign the mechanism).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedural_safeguard_adequacy, empirical, 'Whether announced procedural safeguards can prevent legitimacy extraction or are theater').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ietf_openness_commitment__legitimacy_erosion_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ietf_tr_t0, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(ietf_tr_t5, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 5, 0.39).
narrative_ontology:measurement(ietf_tr_t10, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 10, 0.46).
narrative_ontology:measurement(ietf_tr_t15, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 15, 0.52).
narrative_ontology:measurement(ietf_tr_t20, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 20, 0.55).
narrative_ontology:measurement(ietf_tr_t25, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 25, 0.58).

% Extraction over time
narrative_ontology:measurement(ietf_be_t0, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(ietf_be_t5, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(ietf_be_t10, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(ietf_be_t15, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(ietf_be_t20, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(ietf_be_t25, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ietf_su_t0, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(ietf_su_t5, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement(ietf_su_t10, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(ietf_su_t15, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(ietf_su_t20, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(ietf_su_t25, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 25, 0.72).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=25
narrative_ontology:measurement(ietf_grid_01, ietf_openness_commitment__legitimacy_erosion_reading, accessibility_collapse(class), 0, 0.72).
narrative_ontology:measurement(ietf_grid_02, ietf_openness_commitment__legitimacy_erosion_reading, accessibility_collapse(class), 25, 0.81).
narrative_ontology:measurement(ietf_grid_03, ietf_openness_commitment__legitimacy_erosion_reading, accessibility_collapse(individual), 0, 0.35).
narrative_ontology:measurement(ietf_grid_04, ietf_openness_commitment__legitimacy_erosion_reading, accessibility_collapse(individual), 25, 0.48).
narrative_ontology:measurement(ietf_grid_05, ietf_openness_commitment__legitimacy_erosion_reading, accessibility_collapse(organizational), 0, 0.58).
narrative_ontology:measurement(ietf_grid_06, ietf_openness_commitment__legitimacy_erosion_reading, accessibility_collapse(organizational), 25, 0.72).
narrative_ontology:measurement(ietf_grid_07, ietf_openness_commitment__legitimacy_erosion_reading, accessibility_collapse(structural), 0, 0.68).
narrative_ontology:measurement(ietf_grid_08, ietf_openness_commitment__legitimacy_erosion_reading, accessibility_collapse(structural), 25, 0.75).
narrative_ontology:measurement(ietf_grid_09, ietf_openness_commitment__legitimacy_erosion_reading, resistance(class), 0, 0.68).
narrative_ontology:measurement(ietf_grid_10, ietf_openness_commitment__legitimacy_erosion_reading, resistance(class), 25, 0.74).
narrative_ontology:measurement(ietf_grid_11, ietf_openness_commitment__legitimacy_erosion_reading, resistance(individual), 0, 0.55).
narrative_ontology:measurement(ietf_grid_12, ietf_openness_commitment__legitimacy_erosion_reading, resistance(individual), 25, 0.62).
narrative_ontology:measurement(ietf_grid_13, ietf_openness_commitment__legitimacy_erosion_reading, resistance(organizational), 0, 0.71).
narrative_ontology:measurement(ietf_grid_14, ietf_openness_commitment__legitimacy_erosion_reading, resistance(organizational), 25, 0.78).
narrative_ontology:measurement(ietf_grid_15, ietf_openness_commitment__legitimacy_erosion_reading, resistance(structural), 0, 0.62).
narrative_ontology:measurement(ietf_grid_16, ietf_openness_commitment__legitimacy_erosion_reading, resistance(structural), 25, 0.68).
narrative_ontology:measurement(ietf_grid_17, ietf_openness_commitment__legitimacy_erosion_reading, stakes_inflation(class), 0, 0.61).
narrative_ontology:measurement(ietf_grid_18, ietf_openness_commitment__legitimacy_erosion_reading, stakes_inflation(class), 25, 0.68).
narrative_ontology:measurement(ietf_grid_19, ietf_openness_commitment__legitimacy_erosion_reading, stakes_inflation(individual), 0, 0.42).
narrative_ontology:measurement(ietf_grid_20, ietf_openness_commitment__legitimacy_erosion_reading, stakes_inflation(individual), 25, 0.54).
narrative_ontology:measurement(ietf_grid_21, ietf_openness_commitment__legitimacy_erosion_reading, stakes_inflation(organizational), 0, 0.68).
narrative_ontology:measurement(ietf_grid_22, ietf_openness_commitment__legitimacy_erosion_reading, stakes_inflation(organizational), 25, 0.76).
narrative_ontology:measurement(ietf_grid_23, ietf_openness_commitment__legitimacy_erosion_reading, stakes_inflation(structural), 0, 0.55).
narrative_ontology:measurement(ietf_grid_24, ietf_openness_commitment__legitimacy_erosion_reading, stakes_inflation(structural), 25, 0.62).
narrative_ontology:measurement(ietf_grid_25, ietf_openness_commitment__legitimacy_erosion_reading, suppression(class), 0, 0.68).
narrative_ontology:measurement(ietf_grid_26, ietf_openness_commitment__legitimacy_erosion_reading, suppression(class), 25, 0.76).
narrative_ontology:measurement(ietf_grid_27, ietf_openness_commitment__legitimacy_erosion_reading, suppression(individual), 0, 0.48).
narrative_ontology:measurement(ietf_grid_28, ietf_openness_commitment__legitimacy_erosion_reading, suppression(individual), 25, 0.59).
narrative_ontology:measurement(ietf_grid_29, ietf_openness_commitment__legitimacy_erosion_reading, suppression(organizational), 0, 0.72).
narrative_ontology:measurement(ietf_grid_30, ietf_openness_commitment__legitimacy_erosion_reading, suppression(organizational), 25, 0.81).
narrative_ontology:measurement(ietf_grid_31, ietf_openness_commitment__legitimacy_erosion_reading, suppression(structural), 0, 0.52).
narrative_ontology:measurement(ietf_grid_32, ietf_openness_commitment__legitimacy_erosion_reading, suppression(structural), 25, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ietf_openness_commitment__legitimacy_erosion_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ietf_openness_commitment__legitimacy_erosion_reading, 0.15).
narrative_ontology:affects_constraint(ietf_openness_commitment__legitimacy_erosion_reading, ietf_openness_commitment__commons_stewardship_reading).
narrative_ontology:affects_constraint(ietf_openness_commitment__legitimacy_erosion_reading, ietf_openness_commitment__capture_substrate_reading).

% DUAL FORMULATION NOTE:
% The 'ietf_openness_commitment' kernel decomposes into three structurally distinct readings per OQ-26 (ε-invariance): legitimacy_erosion_reading (this file, ε=0.68, tangled_rope, mechanism corrupted by resource asymmetry); commons_stewardship_reading (ε≈0.25, rope, mechanism successfully preserves public goods despite vendor incentives); capture_substrate_reading (ε≈0.75, snare, mechanism was always a vendor-coordination platform, never truly open). The readings diverge on the empirical claim (is openness present or absent?) and the diachronic claim (did the mechanism decay or remain structurally constant?). Each reading treats the same kernel (rough consensus as open standard-setting) but assesses it through different interpretive frameworks: procedural integrity, public-goods preservation, and power-dynamics. Linking via network.affects_constraints establishes family coherence; each reading's omegas address the interpretive contest. The engine does not compute across readings; each reading is independently classified. A corpus consumer examining all three readings observes the divergence itself as diagnostic—evidence that the legitimacy claim is genuinely contested.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
