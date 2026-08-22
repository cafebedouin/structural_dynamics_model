% ============================================================================
% CONSTRAINT STORY: catastrophe_avoidance_retention__hybrid_near_miss_learning
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_avoidance_retention__hybrid_near_miss_learning, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: catastrophe_avoidance_retention__hybrid_near_miss_learning
 *   human_readable: Competence Maintenance via Hybrid Incident-Learning Networks
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   High-reliability industries face a competence paradox: catastrophes are
 *   so rare that organizations and practitioners lose the sharp selection
 *   pressure needed to maintain critical skills and vigilance. This reading
 *   asserts that competence cannot be sustained by simulation alone (it lacks
 *   the full affective and organizational consequences of real failure) nor
 *   by waiting for rare catastrophes (the interval is too long; people
 *   forget, organizations atrophy). Instead, a hybrid system of distributed
 *   incident-sharing networks, mandatory near-miss reporting,
 *   foreign-incident integration, and high-realism drills creates a synthetic
 *   learning signal that approximates the selection pressure of frequent
 *   catastrophes without requiring actual harm. This reading contrasts with
 *   two sibling readings: one argues only catastrophes provide authentic
 *   selection pressure (the 'catastrophe_as_necessary_selector' reading),
 *   another argues simulation sufficiency (the
 *   'simulation_as_proxy_catastrophe' reading). This story instantiates the
 *   hybrid reading as a distinct constraint with its own extractive
 *   structure, beneficiary set, and institutional dynamics.
 *
 * KEY AGENTS:
 *   - Incident-sharing networks (aviation ASRS, weaker medical M&M systems): agenda-setters, institutional power, arbitrage exit — control what counts as reportable, who sees the data, enforcement of mandatory participation.
 *   - Regulatory authorities (FAA, medical boards): agenda-setters, institutional power, analytical exit — mandate network participation and drill cycles, claim success via the counterfactual (accidents prevented, not seen).
 *   - Frontline practitioners (pilots, surgeons, nurses): powerless payers trapped in mandatory participation; also genuine beneficiaries of lower catastrophe risk and cognitively manageable competence maintenance.
 *   - Organizations isolated from networks (smaller hospitals, regional operators): moderate-power payers facing higher catastrophe risk and learning-from-catastrophe-only burden.
 *   - Practitioners arguing catastrophe necessity: moderate-power observers outside the constraint, arguing the system trades authentic learning signal for palatability.
 *   - Catastrophe survivors and communities: excluded, powerless, carrying uncompensated risk — their voices are kept out by confidentiality law and operational security doctrine.
 *   - Simulation infrastructure providers: organized beneficiaries whose market depends on the mandatory-training requirement.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.58).
domain_priors:suppression_score(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.62).
domain_priors:theater_ratio(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, extractiveness, 0.58).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_avoidance_retention__hybrid_near_miss_learning, tangled_rope).
narrative_ontology:human_readable(catastrophe_avoidance_retention__hybrid_near_miss_learning, "Competence Maintenance via Hybrid Incident-Learning Networks").
narrative_ontology:topic_domain(catastrophe_avoidance_retention__hybrid_near_miss_learning, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(catastrophe_avoidance_retention__hybrid_near_miss_learning).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_avoidance_retention__hybrid_near_miss_learning, '0a2f9451-9ddd-45b9-99fb-bf88ece8c943').
narrative_ontology:cs_kernel_codification('0a2f9451-9ddd-45b9-99fb-bf88ece8c943', distributed).
narrative_ontology:cs_authority_grounding('0a2f9451-9ddd-45b9-99fb-bf88ece8c943', practice).
narrative_ontology:cs_interpretation_layer_present('0a2f9451-9ddd-45b9-99fb-bf88ece8c943').
narrative_ontology:cs_reading_relation('0a2f9451-9ddd-45b9-99fb-bf88ece8c943', catastrophe_avoidance_retention__catastrophe_as_necessary_selector, coexists_with).
narrative_ontology:cs_reading_relation('0a2f9451-9ddd-45b9-99fb-bf88ece8c943', catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, influences).
narrative_ontology:cs_axiom('0a2f9451-9ddd-45b9-99fb-bf88ece8c943', foundational, competence_maintenance_requires_distributed_signal).
narrative_ontology:cs_axiom_status(competence_maintenance_requires_distributed_signal, holdable).
narrative_ontology:cs_axiom_grounding('0a2f9451-9ddd-45b9-99fb-bf88ece8c943', competence_maintenance_requires_distributed_signal, empirically_contingent).
narrative_ontology:cs_axiom('0a2f9451-9ddd-45b9-99fb-bf88ece8c943', foundational, incident_networks_substitute_for_catastrophe_selection).
narrative_ontology:cs_axiom_status(incident_networks_substitute_for_catastrophe_selection, holdable).
narrative_ontology:cs_axiom_grounding('0a2f9451-9ddd-45b9-99fb-bf88ece8c943', incident_networks_substitute_for_catastrophe_selection, empirically_contingent).
narrative_ontology:cs_reference_frame('0a2f9451-9ddd-45b9-99fb-bf88ece8c943', competence_paradox_solved_by_networks).
narrative_ontology:cs_drift_state('0a2f9451-9ddd-45b9-99fb-bf88ece8c943', contemporary_regulatory_expansion, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('0a2f9451-9ddd-45b9-99fb-bf88ece8c943', '').
narrative_ontology:cs_kernel_id(catastrophe_avoidance_retention__hybrid_near_miss_learning, catastrophe_avoidance_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, incident_sharing_networks).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, cross_organizational_learning_infrastructure).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__hybrid_near_miss_learning, frontline_practitioners).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__hybrid_near_miss_learning, organizations_isolated_from_networks).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, frontline_practitioners).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, high_realism_drill_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the distributed infrastructure (ASRS in aviation, less formalized networks in medicine and other safety-critical domains). Sets reporting standards, data retention and access policies, de-identification rules, mandatory participation thresholds. Controls what counts as a reportable incident, who can see the analysis, and how organizations must demonstrate learning from the feed. Derives institutional authority from the claim that their curation prevents catastrophes. Could change the rules unilaterally (has arbitrage-level exit); the constraint persists because the current form benefits them.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, incident_sharing_networks, agenda_setter,
    institutional, generational, arbitrage, global).

% Mandate participation in the hybrid system (near-miss reporting, drill participation, adoption of foreign-incident learnings) as a condition of licensure, certification, or operational authorization. Claim the mandate is proportionate because it is less costly than actual catastrophes. Enforce continuously because the system's maintenance requires compliance even when organizations face pressures to economize on training. Collect legitimacy from the counterfactual: 'We require this; the accidents you do not see are our measure of success.' Their authority depends on the system's continuation, so they defend the mandate against deregulation pressure.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, regulatory_and_safety_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Must participate in mandatory incident reporting, drill cycles, and foreign-incident integration (reading case studies, redesigning local protocols based on others' failures). Bear the time cost (hours per quarter in drills, incident-review meetings), emotional labor (recounting failures, reviewing others' mistakes, confronting mortality salience), and career consequence risk (raising a problem incident publicly, being named in a foreign-incident case study that circulates). Also genuinely benefit: learning from near-misses lowers their own catastrophe risk, makes their work more cognitively manageable by distributing the learning load across the network, and builds professional identity around a learning culture. Exit via leaving the profession or the organization, both high-identity-cost moves because many practitioners define themselves through this work. Identity-locked: professional identity is fused with participation in the learning system.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, frontline_practitioners, payer,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__hybrid_near_miss_learning, frontline_practitioners, beneficiary).

% Lack access to or formal participation in cross-organizational incident-sharing networks (smaller hospitals, regional airlines, developing-world operators, isolated rural clinics). Bear the learning burden alone: learning from their own catastrophes only, with limited access to foreign-incident databases or international best practices. Their practitioners carry higher individual disaster risk because the learning signal is sparse. Some organizations choose isolation for operational autonomy; most are excluded by geography, resources, regulatory tiering, or language barriers. Remain vulnerable to the failure modes the networks have already caught and documented. Subject to regulatory pressure to participate but unable to meet the requirements for access (cost, technical infrastructure, organizational scale). This is the constraint's asymmetry: the benefited organizations (those with network access and participation infrastructure) are protected; the organizations that cannot afford participation carry the uncompensated risk.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, organizations_isolated_from_networks, payer,
    moderate, biographical, constrained, local).

% Run the high-fidelity simulations, scenario training, tabletop exercises, and immersive drills that constitute the 'near-miss-like' portion of the hybrid learning system. Provide the infrastructure (flight simulators, operating-room labs, control-room mockups), trained scenario facilitators, scenario libraries (often built by synthesizing real incidents and foreign-incident learnings). Depend on the institutional mandate for demand: drills would be optional if not mandated; the mandate ensures their market. Benefit from the requirement and from the continuous stream of real incidents (and near-misses from the incident-sharing networks) that refresh and calibrate their scenario libraries. Could exit by shifting to different markets (corporate team-building simulations, military training, entertainment) but have sunk costs in safety-critical scenario libraries and regulatory relationships.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, high_realism_drill_operators, beneficiary,
    organized, biographical, mobile, regional).

% Are not present in the incident-sharing infrastructure, learning governance, or policy design, even though the constraint's stated purpose is to prevent catastrophes that would harm them. Carry the uncompensated risk if the hybrid system fails: if a near-miss is missed, a drill scenario is incomplete, or a foreign-incident learning is not adopted, the consequences (death, injury, property loss, trauma) fall on them. Might argue for direct seats in incident analysis (victim representation), mandatory public disclosure of incidents and near-misses, compensation funds for catastrophe survivors, or mandatory engagement with bereaved families in learning cycles. Are kept out by medical privacy law (HIPAA, etc.), commercial confidentiality (proprietary incident databases are legally protected), operational security doctrine ('publishing incident data helps adversaries'), and institutional inertia (the networks predate survivor advocacy). This is the exclusion face of the constraint: those whose harm it purports to prevent are the ones most structured out of its governance.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, catastrophe_survivors_and_affected_communities, excluded,
    powerless, immediate, trapped, local).

% Occupy a minority but persistent position in professional and academic discourse, arguing that the hybrid system (drills, near-miss reports, foreign-incident learning) is insufficient — only actual catastrophes provide the full affective, organizational, and existential selection pressure necessary to reshape behavior and maintain true competence. Cite evidence from organizational change literature (only crisis creates the will to change), from practitioners who survived real incidents (the learning signal is incomparably sharp), and from some isolated-organization comparisons (practitioners in catastrophe-exposed organizations sometimes show different vigilance profiles than those in protected networks). Observe the hybrid constraint from outside its authority structure, arguing that accepting it as sufficient may create false confidence and competence decay at the population level. This reading coexists with the hybrid reading in professional discourse but remains marginalized in policy and regulation.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, practitioners_catastrophe_necessity_tradition, observer,
    moderate, biographical, constrained, global).

% Argue from the alternative reading (simulation_as_proxy_catastrophe) that high-fidelity simulation is functionally equivalent to real catastrophic events for competence maintenance and risk recognition. Advocate for investment in simulation sufficiency such that reliance on actual incidents or cross-organizational near-miss networks would become unnecessary. See the hybrid system as a transitional or incomplete state: if simulation technology and pedagogical methods advance sufficiently, the need for incident-sharing networks would diminish (lower transaction costs, easier control of scenario realism, less emotional labor on practitioners from reviewing others' real failures). They observe the hybrid constraint from a position of technological optimism, arguing that the constraint's extractive costs could be eliminated by substitution with better simulation infrastructure.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, simulation_fidelity_advocates, observer,
    organized, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_avoidance_retention__hybrid_near_miss_learning, incident_sharing_networks).
narrative_ontology:fixing_cost_class(catastrophe_avoidance_retention__hybrid_near_miss_learning, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains distributed incident-learning infrastructure (aviation ASRS model: near-miss and foreign-incident aggregation, analysis, de-identified feedback, mandatory drill integration) such that competence does not erode when catastrophes are rare. Solves the collective-action problem: any single organization learning only from its own catastrophes is under-informed and at high risk; the network pools the learning load and raises the base floor of competence across participants.
% TRANSFER_FUNCTION: Moves institutional authority from local practitioners and organizations to the incident-sharing network and regulatory body; moves time and emotional labor from frontline practitioners into participation in drills, incident analysis, and foreign-incident integration; moves operational autonomy constraints onto organizations in exchange for access to the learning feed.
% ABSENT_VOICES: Catastrophe survivors and affected communities are kept out of incident analysis and learning design by confidentiality law and operational security doctrine. Practitioners and researchers who argue competence requires actual catastrophic selection pressure (not just near-misses and drills) are pushed to the margin of professional discourse as advocating for harm. Isolated organizations and practitioners outside the network infrastructure have no formal seat in learning-system design.
% DISAPPEARANCE_RATIONALE: If the hybrid incident-sharing and mandatory drill infrastructure vanished overnight, organizations would revert to learning from their own catastrophes only; competence would erode between rare events; practitioners would lose access to foreign incident data and mandatory training cycles. Industries with strong networks (aviation) would face sudden fragmentation and elevated accident risk; industries relying on weaker networks would see competence cascades. The constraint's removal would not just weaken coordination — it would structurally collapse the information and enforcement apparatus that keeps catastrophe frequency low enough for competence to persist without constant selection pressure.
% FOUNDING_PROBLEM: Rare, high-consequence events (aviation crashes, hospital disasters) were treated as isolated organizational failures. Each organization learned only from its own catastrophes or near-misses; learning was slow, fragmented, and incomplete. Competence eroded between rare events because the signal-to-noise ratio in any single organization's data was too low. The constraint was built to create a distributed learning architecture that substitutes for the selection pressure of frequent catastrophes.
% FOUNDING_PROBLEM_CORROBORATION: Aviation authorities and safety-culture researchers attest the founding problem is substantially solved by networks like ASRS: accident rates have fallen, competence retention is demonstrable, and foreign-incident learning prevents local rediscoveries of fatal failure modes. Medical professionals outside of strong morbidity/mortality conference networks (and those who have experienced isolated-organization catastrophes) argue the founding problem persists: without mandatory networks, competence still erodes, and the constraint's success is unevenly distributed. The empirical divergence between aviation (high network participation, low accident rates) and medicine (weaker network infrastructure, repeated near-identical institutional disasters) is cited as evidence that the constraint solves the founding problem only when the network is sufficiently dense and enforced.
narrative_ontology:disappearance_verdict(catastrophe_avoidance_retention__hybrid_near_miss_learning, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_avoidance_retention__hybrid_near_miss_learning, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_avoidance_retention__hybrid_near_miss_learning, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_avoidance_retention__hybrid_near_miss_learning, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_avoidance_retention__hybrid_near_miss_learning_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_avoidance_retention__hybrid_near_miss_learning, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_avoidance_retention__hybrid_near_miss_learning_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures how much institutional authority and operational autonomy is moved from practitioners and local organizations to the network and regulatory apparatus. The metric runs 0.42→0.58, showing accumulation: as the constraint matures, the reporting burden intensifies, drill requirements expand, and foreign-incident integration becomes more prescriptive. Theater rises from 0.25→0.41, indicating that an increasing share of the reported near-misses and drills are performative compliance rather than genuine learning — organizations file reports to satisfy auditors, run drills to certify readiness, rather than because the specific incident would shape competence. Suppression runs 0.48→0.62, reflecting the active enforcement needed to keep organizations and practitioners participating (time costs, career consequences for resisting, confidentiality pressures). The slight dip at t=40 (extractiveness to 0.58, theater to 0.41, suppression to 0.62) reflects a stability plateau: the constraint reaches a steady-state enforcement level after initial expansion; growth rates slow but do not reverse. All measurements are on a single shared time grid (0, 8, 16, 24, 32, 40) to avoid misalignment-induced type shifts.
 *
 * PERSPECTIVAL GAP:
 *   From the network and regulatory seat, the constraint coordinates a genuine safety function: it maintains competence and prevents catastrophes at scale. The measurement of success is the counterfactual (accidents that do not happen). From the frontline practitioner seat, the same structure operates as mandatory participation in a learning apparatus whose benefits are diffuse and whose costs are immediate (time, emotional labor, exposure to others' failures). From the isolated-organization seat, the constraint is pure extraction: they cannot access the network, bear the full learning burden alone, and are subject to regulatory punishment for not participating. The engine computes these divergences from the structural data: practitioners have trapped exit (high d toward target); networks have arbitrage exit and agenda-setting power (low d toward beneficiary); isolated organizations sit between (constrained exit, moderate power, but excluded from benefits). These computed directionalities should produce per-seat classification divergence: from the network seat, rope or coordination; from the practitioner and isolated-organization seats, tangled_rope or snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Incident-sharing networks and regulatory authorities are structural beneficiaries: they derive institutional authority, operational mandates, and legitimacy from the system's existence and enforcement. They set what counts as learning, who participates, what the standards are. Their exit options are arbitrage — they could change the rules, shift the incentives, loosen enforcement; the constraint persists because the current form benefits them. Frontline practitioners and isolated organizations bear the direct costs (time, emotional labor, learning from catastrophes only) and cannot simply leave without leaving the profession or organization. They are trapped or identity-locked. Yet practitioners also genuinely benefit from lower catastrophe risk — they sit somewhere between pure target and symmetric, depending on how much the network's learning actually reduces their disaster exposure (an empirical uncertainty, hence an omega). Catastrophe survivors are excluded and carry the risk if the system fails — they are the implicit targets whose harm the constraint is supposed to prevent, but they are kept out of the learning and governance apparatus. This asymmetry — people benefiting from the network's authority at the expense of trapped practitioners and excluded communities — is the extractive structure that tangled_rope identifies. Without it, the constraint might be pure rope (genuine coordination with minimal overhead). With it, it is tangled: coordination function + asymmetric extraction + active enforcement to hold the asymmetry in place.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem ('competence erodes when catastrophes are rare') is contested. Aviation practitioners and safety researchers attest the problem is solved: ASRS-style networks demonstrably reduce accident rates and maintain competence. Medical professionals outside of strong networks (and isolated-organization operators) attest the problem persists: without dense networks and mandatory participation, competence still erodes, and the constraint's success is unevenly distributed. The measurement of success is entirely counterfactual — accidents prevented, not visible — which creates an ideal cover story. The constraint could claim credit for all low-frequency events (whether due to the network or to independent improvements in technology, equipment reliability, or practice) and blame isolated organizations or practitioners for any accidents (they did not participate fully enough). The mandate to participate prevents easy exit, so organizations cannot experimentally opt out to test the counterfactual. This is the mandatrophy structure: a constraint whose founding function is partially attested but whose persistence is also sustained by its power structure, making it difficult to know whether the constraint is still necessary or has become partly extractive. The tangled_rope classification holds the ambiguity: the constraint DOES coordinate learning (genuine function) AND does extract institutional authority and operational autonomy from practitioners (asymmetric structure). Both are true; the engine computes the tension between them.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hybrid_system_sufficiency,
    'Is the hybrid system (near-miss reporting + foreign incidents + drills) genuinely sufficient to maintain competence between rare catastrophes, or does it substitute palatability for authenticity?',
    'Natural experiment: compare competence trajectories in organizations with equivalent network participation but different catastrophe frequencies (e.g., aviation operators with zero accidents over 20 years vs. those with one incident). Also compare industries with strong networks (aviation) and weak networks (medicine) directly on competence metrics and disaster outcomes.',
    'If the hybrid system maintains competence equivalently across low and moderate catastrophe frequencies, the system solves the founding problem. If competence erosion is detectable even within networks, or if network participation correlates with lower competence than catastrophe-exposed organizations, the hybrid reading is weakened and the catastrophe_as_necessary_selector reading gains credence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_system_sufficiency, empirical, 'Whether distributed incident-learning networks provide authentic competence maintenance or substitute palatability for real selection pressure.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is participation in mandatory drills and incident-reporting networks maintained by structural coercion (regulations, licensure conditions) or by internalized belief that the networks are necessary?',
    'Post-deregulation observation: if regulations on mandatory participation were removed, would organizations and practitioners continue participating at similar rates? Also post-catastrophe observation: do practitioners in organizations that experience a real catastrophe increase network participation beyond regulatory minimums, or revert to isolated learning?',
    'If suppression is primarily structural, removing the mandate would risk network collapse and competence erosion. If suppression is partly internalized, practitioners carry the constraint with them (identity-locked exit) even if deregulated. This affects classification stability: a constraint with high internalized suppression persists even when formal enforcement loosens, which makes it more extractive (the target cannot easily exit). A constraint relying on structural suppression is more fragile but also more clearly coercive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Whether mandatory participation is maintained by regulation or by internalized professional identity.').

omega_variable(
    network_density_threshold,
    'Is there a critical density threshold for incident-sharing networks below which the hybrid system fails to maintain competence?',
    'Comparative analysis of aviation (very high network participation, near-universal incident reporting) vs. medicine (lower participation, selective incident reporting). Also track organizations entering and exiting networks to see whether competence maintenance depends on network size or density.',
    'If there is a threshold, the constraint''s success is conditional on maintaining network density — isolated organizations below the threshold remain at high risk even if nominally participating. This would refine the extractive analysis: the constraint benefits from the fact that some organizations cannot reach critical mass and thus remain learning-from-catastrophe-only, creating pressure to participate in the network for safety reasons.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_density_threshold, empirical, 'Whether competence maintenance depends on a critical incident-network density threshold.').

omega_variable(
    committer_catastrophe_selector_vs_hybrid,
    'Are the core premises of the catastrophe_as_necessary_selector reading and this hybrid reading logically incompatible, or do they coexist in different organizational contexts?',
    'Qualitative: organizations and practitioners who hold the catastrophe-necessary view report their reasoning (e.g., ''drills miss the organizational trauma that reshapes culture'' vs. ''networks provide equivalent learning without requiring deaths''). Quantitative: track whether practitioners trained in catastrophe-heavy vs. network-heavy contexts show different competence trajectories and incident-response behaviors.',
    'If logically incompatible (one reading forecloses the other), the readings cannot coexist in a single organization''s framework — the constraint is built to enforce this reading''s victory over the alternative. If coexistent, both readings are live and the constraint is contested at every implementation point. The relation type determines how the engine routes the sibling reading''s influence on this constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_catastrophe_selector_vs_hybrid, conceptual, 'Whether hybrid learning and catastrophe-necessary selection are logically foreclose each other or coexist as live positions.').

omega_variable(
    extractiveness_accumulation_driver,
    'What drives the measured extraction accumulation from 0.42→0.58 over the interval? Is it legitimate expansion of a necessary function (more organizations and practitioners requiring training), or rent-seeking expansion of bureaucratic overhead?',
    'Track the ratio of (incident reports processed + organizations trained) to (extractive time burden on practitioners + regulatory compliance overhead). Also track the distribution of network benefits: do organizations with highest participation show lowest incident rates (coordination function), or do they show compliance performance independent of safety outcomes (theater)?',
    'If extraction growth tracks genuine safety improvements and benefit distribution, the expansion is functional (tangled_rope is legitimate, albeit asymmetric). If extraction growth outpaces safety gains, the constraint is drifting toward snare (pure extraction with a coordination cover story). The measurement series showing rising theater_ratio (0.25→0.41) suggests the latter, but theater itself is an ambiguous metric: it could reflect genuine normalization and professionalization (less crisis reactivity) or mere certification theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extractiveness_accumulation_driver, empirical, 'Whether rising extractiveness corresponds to legitimate safety function expansion or to rent-seeking bureaucratic growth.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cata_tr_t8, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 8, 0.31).
narrative_ontology:measurement(cata_tr_t16, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 16, 0.37).
narrative_ontology:measurement(cata_tr_t24, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 24, 0.42).
narrative_ontology:measurement(cata_tr_t32, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 32, 0.44).
narrative_ontology:measurement(cata_tr_t40, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(cata_be_t8, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(cata_be_t16, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 16, 0.54).
narrative_ontology:measurement(cata_be_t24, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 24, 0.58).
narrative_ontology:measurement(cata_be_t32, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 32, 0.61).
narrative_ontology:measurement(cata_be_t40, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(cata_su_t8, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 8, 0.54).
narrative_ontology:measurement(cata_su_t16, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 16, 0.58).
narrative_ontology:measurement(cata_su_t24, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 24, 0.62).
narrative_ontology:measurement(cata_su_t32, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 32, 0.64).
narrative_ontology:measurement(cata_su_t40, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_avoidance_retention__hybrid_near_miss_learning, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.12).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__hybrid_near_miss_learning, catastrophe_as_necessary_selector).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__hybrid_near_miss_learning, simulation_as_proxy_catastrophe).

% DUAL FORMULATION NOTE:
% This constraint (hybrid_near_miss_learning) instantiates one reading of the contested kernel 'catastrophe_avoidance_retention'. The sibling readings are authored as separate constraint stories in the same family: catastrophe_as_necessary_selector (argues only real catastrophes maintain competence); simulation_as_proxy_catastrophe (argues simulation sufficiency). The three readings compete in professional discourse and policy development; they are linked here as a constraint family via affects_constraints. Each reading has a distinct epsilon (this one measures the extractive structure of the hybrid network; the others measure the extractive/coordinative structures of their respective approaches), distinct beneficiary/victim sets, and distinct institutional dynamics. The ε-invariance principle requires decomposition: trying to fold all three readings into one constraint story would create an under-determined epsilon that varies with the measurement basis. Instead, each reading is a clean constraint, linked via network relations to show the family structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_avoidance_retention__hybrid_near_miss_learning, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
