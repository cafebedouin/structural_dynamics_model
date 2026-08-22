% ============================================================================
% CONSTRAINT STORY: ai_risk_governance_priority__bridge_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_governance_priority__bridge_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: ai_risk_governance_priority__bridge_reading
 *   human_readable: Bridge Reading: Unified Near-Term/Existential AI Risk Governance Framework
 *   domain: AI Governance / Technology Ethics / Risk Assessment
 *
 * SUMMARY:
 *   This story instantiates the bridge reading of the contested
 *   ai_risk_governance_priority kernel: the claim that present harms and
 *   existential risks are non-mutually-exclusive, structurally entangled
 *   concerns requiring unified governance frameworks. Unlike the sibling
 *   readings (existential_risk_reading, which prioritizes
 *   superintelligence-scenario prevention, and near_term_harms_reading, which
 *   prioritizes documented present harms to marginalized populations), the
 *   bridge reading asserts a coordination claim: that governing either risk
 *   category in isolation produces contradictory or fragmented policy. That
 *   coordination claim is real and produces a genuine function, but the
 *   reading also creates a specific extraction structure — a small set of
 *   broker institutions capture disproportionate funding and legitimacy from
 *   occupying the connective position, and both victim populations (present
 *   marginalized groups and future humanity) bear costs from resources being
 *   redirected toward maintaining the bridging apparatus rather than toward
 *   either camp's direct remediation work.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__bridge_reading, 0.48).
domain_priors:suppression_score(ai_risk_governance_priority__bridge_reading, 0.42).
domain_priors:theater_ratio(ai_risk_governance_priority__bridge_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__bridge_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_governance_priority__bridge_reading, "Bridge Reading: Unified Near-Term/Existential AI Risk Governance Framework").
narrative_ontology:topic_domain(ai_risk_governance_priority__bridge_reading, "AI Governance / Technology Ethics / Risk Assessment").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__bridge_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__bridge_reading, '6d108f4f-d057-4f6a-9236-aefc6be88562').
narrative_ontology:cs_kernel_codification('6d108f4f-d057-4f6a-9236-aefc6be88562', distributed).
narrative_ontology:cs_authority_grounding('6d108f4f-d057-4f6a-9236-aefc6be88562', distributed).
narrative_ontology:cs_reading_relation('6d108f4f-d057-4f6a-9236-aefc6be88562', ai_risk_governance_priority__existential_risk_reading, influences).
narrative_ontology:cs_reading_relation('6d108f4f-d057-4f6a-9236-aefc6be88562', ai_risk_governance_priority__near_term_harms_reading, influences).
narrative_ontology:cs_axiom('6d108f4f-d057-4f6a-9236-aefc6be88562', foundational, present_and_existential_risk_structurally_entangled).
narrative_ontology:cs_axiom_status(present_and_existential_risk_structurally_entangled, holdable).
narrative_ontology:cs_axiom_grounding('6d108f4f-d057-4f6a-9236-aefc6be88562', present_and_existential_risk_structurally_entangled, empirically_contingent).
narrative_ontology:cs_axiom('6d108f4f-d057-4f6a-9236-aefc6be88562', secondary, unified_frameworks_required_over_specialized_prioritization).
narrative_ontology:cs_axiom_status(unified_frameworks_required_over_specialized_prioritization, holdable).
narrative_ontology:cs_axiom_grounding('6d108f4f-d057-4f6a-9236-aefc6be88562', unified_frameworks_required_over_specialized_prioritization, instrumental).
narrative_ontology:cs_reference_frame('6d108f4f-d057-4f6a-9236-aefc6be88562', siloed_governance_pre_bridge_era).
narrative_ontology:cs_drift_state('6d108f4f-d057-4f6a-9236-aefc6be88562', post_2024_bridge_institutionalization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6d108f4f-d057-4f6a-9236-aefc6be88562', '').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__bridge_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, cross_field_broker_institutions).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, integrated_safety_ethics_research_centers).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, present_marginalized_populations_under_ai_systems).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, future_humanity_under_uncontrolled_ai_trajectories).
narrative_ontology:constraint_vindicates(ai_risk_governance_priority__bridge_reading, structural_entanglement_of_present_and_existential_ai_risk).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A small set of labs, journals, and funding bodies that publish and fund work explicitly bridging near-term harm research and existential-safety research. They set the terms of what counts as 'integrated' scholarship, control the conferences and grant panels where bridging work is legitimated, and capture citation, funding, and convening prestige from occupying the connective position. Their institutional survival depends on the bridge narrative remaining plausible and funded.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, cross_field_broker_institutions, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__bridge_reading, cross_field_broker_institutions, beneficiary).

% People subject to biased hiring algorithms, predictive policing, surveillance, and labor displacement today. Under the bridge framework, resources and attention that could go directly to remediating their documented harms are partially redirected toward long-horizon existential scenarios and toward maintaining the connective research infrastructure itself. They have no seat in the governance panels that decide the resource split and cannot exit the AI systems governing them.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, present_marginalized_populations_under_ai_systems, payer,
    powerless, immediate, trapped, global).

% Represented only by proxy advocates; bears the cost if bridging institutions dilute existential-safety technical work by insisting on integration with present-harms framing that some safety researchers argue slows deployment of hard technical alignment work. Cannot exit or object; entirely dependent on present institutional choices made in its name.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, future_humanity_under_uncontrolled_ai_trajectories, payer,
    powerless, civilizational, trapped, universal).

% University centers and think tanks funded specifically for producing work that spans both risk categories. They receive dedicated grants, media attention, and policy access precisely because they occupy the bridging niche; their funding models depend on the unified-framework premise continuing to be treated as the correct governance stance rather than as one contested position among several.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, integrated_safety_ethics_research_centers, beneficiary,
    organized, generational, mobile, global).

% Researchers who believe the bridge framing dilutes urgent technical alignment work by mandating engagement with present-harms framing they see as a distraction from the highest-stakes failure mode. Their objections are represented in the sibling existential_risk_reading constraint, not resolved here; within this reading's institutions they are treated as one contributing voice among many, not the decisive one.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, existential_risk_specialists, excluded,
    moderate, civilizational, constrained, global).

% Researchers and advocates focused on documented present harms who argue the bridge framing is used to launder existential-risk funding priorities as urgent present-tense work, diverting attention and money from remediable current harms. Their objections are represented in the sibling near_term_harms_reading constraint, not resolved here.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, near_term_harms_advocates, excluded,
    moderate, immediate, constrained, global).

% Regulatory and standard-setting bodies that must decide how to allocate governance attention and funding mandates across the contested framings; they consult broker institutions disproportionately because brokers produce the citable synthesis literature, which itself reinforces broker centrality.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, ai_governance_policy_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_governance_priority__bridge_reading, cross_field_broker_institutions).
narrative_ontology:fixing_cost_class(ai_risk_governance_priority__bridge_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Genuinely solves a real coordination problem: without some bridging function, present-harms and existential-risk research communities talk past each other, duplicate governance infrastructure, and produce policy recommendations that contradict one another at the point of implementation (e.g., transparency mandates that help present-harm audits but that some existential-safety researchers argue could accelerate capability diffusion). The bridge function translates between vocabularies and prevents purely siloed governance capture by either camp alone.
% TRANSFER_FUNCTION: Moves citation capital, grant funding, and policy-advisory access toward the small set of broker institutions and centers that can credibly claim to synthesize both framings, and moves attention/resources away from directly remediating present documented harms (which could be funded without any bridging apparatus) and away from unencumbered highest-priority technical alignment work (which some argue is diluted by mandatory integration).
% ABSENT_VOICES: Directly affected present populations (workers displaced by automation, communities subject to biased algorithmic decisions) are almost never in the room where the bridge framework's funding allocations are set; future humanity has no voice at all and is represented only by self-appointed proxies who are themselves often broker-institution affiliated, creating a structural conflict between the proxy's advocacy role and its funding interest in the bridge status quo persisting.
% DISAPPEARANCE_RATIONALE: Broker institutions and the funders who back them would say the world rearranges catastrophically without bridging work — governance fragments, present-harm and existential-safety camps produce contradictory regulation, and useful synthesis knowledge disappears. Both excluded camps would say the world barely changes for their core work — near-term harm remediation and technical alignment research would proceed on their own tracks, arguably faster, without the overhead of mandatory integration and the diversion of resources to broker-institution convening activity.
% FOUNDING_PROBLEM: Early-2020s AI governance discourse split into two camps that developed separate vocabularies, separate conferences, and separate policy asks (algorithmic fairness/accountability regulation vs. frontier-model safety regulation), producing policy proposals that sometimes worked at cross-purposes and left governments unable to synthesize a coherent regulatory posture.
% FOUNDING_PROBLEM_CORROBORATION: Independent policy analysts and legislative staff who are not affiliated with either camp or with the bridging institutions attest that the coordination gap was real and that early governance proposals were genuinely contradictory; however, several economists and science-policy researchers studying grant allocation (outside the bridging institutions themselves) have found that the resulting bridge funding stream is now substantially larger than the coordination problem it addresses, and that the same handful of broker institutions receive a disproportionate share of both present-harm and existential-risk grant lines by virtue of occupying the connective position, suggesting partial goal displacement from coordination toward rent capture.
narrative_ontology:disappearance_verdict(ai_risk_governance_priority__bridge_reading, contested).
narrative_ontology:founding_problem_status(ai_risk_governance_priority__bridge_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__bridge_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_risk_governance_priority__bridge_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_governance_priority__bridge_reading, 0.48, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_governance_priority__bridge_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_governance_priority__bridge_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_risk_governance_priority__bridge_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48) and suppression (0.42) are both moderate, matching the expected structural delta: this reading is neither a clean coordination mechanism nor a pure extraction vehicle. Theater ratio rises over the interval (0.20 to 0.44) reflecting a genuine Goodhart-style drift — as bridging work becomes a funded category with its own conferences, journals, and grant lines, an increasing share of 'integrated' scholarship functions as credentialing and convening activity for broker institutions rather than as work that actually resolves policy contradictions. Accessibility collapse is moderate (0.40): the two unbridged framings remain fully articulable and are actively defended by excluded stakeholders, so alternatives have not disappeared even as the bridge framing gains institutional dominance in funding panels.
 *
 * PERSPECTIVAL GAP:
 *   From the broker-institution seat, this reading is experienced as necessary infrastructure preventing governance chaos — a rope. From the seat of either excluded specialist camp, the same structure looks like a tax on their more urgent work, extracted to fund a synthesis apparatus whose main output is its own continued relevance. From the two victim populations, who are not even represented in either camp's advocacy, it reads as a structure that defers concrete remediation (for the present-harm side) or concrete technical safety work (for the existential side) in favor of an institutionally convenient middle position.
 *
 * DIRECTIONALITY LOGIC:
 *   Broker institutions and integrated research centers are structural beneficiaries: they collect funding, citation capital, and policy access specifically because they occupy the connective niche, and their exit options are mobile-to-arbitrage because they can pivot framing as funding incentives shift. Both victim populations sit near the full-target end for different reasons: present marginalized populations are trapped in the AI systems governing them today and have zero say in the resource split; future humanity is trapped by definition (cannot act now) and represented only by proxies whose institutional interests are entangled with the bridge framework's continuation. This is a structurally unusual directionality pattern — the victims do not share a time horizon or a scope, but they share exclusion from the panels that allocate bridging resources.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (contradictory governance proposals from siloed camps) was real and, per outside policy-staff corroboration, has been substantially addressed at the level of discourse coordination. But funding-allocation researchers outside the beneficiary set find the resulting bridge funding stream now exceeds what the coordination problem requires, and disproportionately flows to the same handful of broker institutions — a mismatch between founding_problem_status (contested, trending toward 'addressed at the coordination layer') and a persisting, growing resource claim. This is precisely the founding-problem/disappearance-verdict mismatch the framework is built to surface: it flags a zombie-coordination risk without asserting it as settled, since both excluded camps and the broker institutions dispute whether the coordination function is still load-bearing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_function_still_load_bearing,
    'Does the bridge framework still solve a live coordination problem (contradictory governance proposals from siloed camps), or has that problem already been substantially resolved at the discourse level, leaving the bridging apparatus as inertial funding infrastructure?',
    'Track whether policy proposals from unbridged specialist camps (post-2024) continue to produce mutually contradictory regulatory recommendations, versus whether camps have organically converged on compatible proposals independent of broker-institution mediation.',
    'If the coordination problem is resolved, this reading''s beneficiary structure (broker institutions) is closer to a piton — extraction without a live coordination function; if unresolved, the tangled_rope classification with genuine coordination stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_still_load_bearing, empirical, 'Whether the bridge framework''s founding coordination problem is live or has become inertial.').

omega_variable(
    committer_structure_kernel_disagreement_location,
    'This constraint is one of three readings of the ai_risk_governance_priority kernel (bridge_reading here; sibling readings existential_risk_reading and near_term_harms_reading are separate constraints). Where exactly is the disagreement located: is it a factual dispute about whether present and existential risks share causal mechanisms, or a values dispute about which population''s claims take precedence when resources are scarce?',
    'Distinguish empirical claims (do present-harm mitigation techniques and existential-safety techniques share technical substrate, e.g., interpretability research serving both) from values claims (whose harm counts more given scarce governance attention) within each reading''s advocacy literature.',
    'If the disagreement is primarily empirical and resolvable (shared technical substrate is demonstrated), the bridge_reading''s coordination claim strengthens and its extraction character weakens over time. If primarily a values dispute over scarce-resource priority, the three readings will remain in permanent coexistence rather than converging, and the bridge_reading''s beneficiary capture is more likely to persist indefinitely as a structural feature rather than a transitional one.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_structure_kernel_disagreement_location, conceptual, 'Whether the kernel disagreement is empirical (resolvable) or a values dispute (persistent).').

omega_variable(
    broker_institution_capture_reversibility,
    'Is the concentration of bridging funding and legitimacy in a handful of broker institutions (5% of papers, 85% of cross-field links, per the structural delta) a natural network effect of any bridging function, or a reversible artifact of specific funder decisions that could be redistributed toward broader collaboration?',
    'Compare against other interdisciplinary bridging fields (e.g., bioethics/biosecurity) to see whether similarly extreme concentration is typical of bridging functions generally or specific to AI risk governance''s funding structure.',
    'If concentration is a generic feature of bridging functions, the extraction is closer to an inherent coordination cost (raise the effective Boltzmann floor); if specific to funder choices in this domain, it represents excess extraction that could be corrected by funding-structure reform without abandoning the bridging function itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(broker_institution_capture_reversibility, empirical, 'Whether broker concentration is structurally inherent to bridging or a correctable funding artifact.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__bridge_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_governance_priority__bridge_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ai_r_tr_t4, ai_risk_governance_priority__bridge_reading, theater_ratio, 4, 0.26).
narrative_ontology:measurement(ai_r_tr_t8, ai_risk_governance_priority__bridge_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(ai_r_tr_t12, ai_risk_governance_priority__bridge_reading, theater_ratio, 12, 0.34).
narrative_ontology:measurement(ai_r_tr_t16, ai_risk_governance_priority__bridge_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement(ai_r_tr_t20, ai_risk_governance_priority__bridge_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(ai_r_tr_t24, ai_risk_governance_priority__bridge_reading, theater_ratio, 24, 0.44).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_governance_priority__bridge_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(ai_r_be_t4, ai_risk_governance_priority__bridge_reading, base_extractiveness, 4, 0.33).
narrative_ontology:measurement(ai_r_be_t8, ai_risk_governance_priority__bridge_reading, base_extractiveness, 8, 0.37).
narrative_ontology:measurement(ai_r_be_t12, ai_risk_governance_priority__bridge_reading, base_extractiveness, 12, 0.41).
narrative_ontology:measurement(ai_r_be_t16, ai_risk_governance_priority__bridge_reading, base_extractiveness, 16, 0.44).
narrative_ontology:measurement(ai_r_be_t20, ai_risk_governance_priority__bridge_reading, base_extractiveness, 20, 0.46).
narrative_ontology:measurement(ai_r_be_t24, ai_risk_governance_priority__bridge_reading, base_extractiveness, 24, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_governance_priority__bridge_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ai_r_su_t4, ai_risk_governance_priority__bridge_reading, suppression_requirement, 4, 0.32).
narrative_ontology:measurement(ai_r_su_t8, ai_risk_governance_priority__bridge_reading, suppression_requirement, 8, 0.34).
narrative_ontology:measurement(ai_r_su_t12, ai_risk_governance_priority__bridge_reading, suppression_requirement, 12, 0.36).
narrative_ontology:measurement(ai_r_su_t16, ai_risk_governance_priority__bridge_reading, suppression_requirement, 16, 0.38).
narrative_ontology:measurement(ai_r_su_t20, ai_risk_governance_priority__bridge_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(ai_r_su_t24, ai_risk_governance_priority__bridge_reading, suppression_requirement, 24, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__bridge_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(ai_risk_governance_priority__bridge_reading, 0.12).
narrative_ontology:affects_constraint(ai_risk_governance_priority__bridge_reading, existential_risk_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__bridge_reading, near_term_harms_reading).

% DUAL FORMULATION NOTE:
% This constraint is the bridge_reading member of a three-story kernel family under ai_risk_governance_priority. existential_risk_reading and near_term_harms_reading are separate constraint stories with their own ε, their own beneficiary/victim sets, and their own classifications; this story does not average or hedge across them. All three should carry reciprocal network links documenting the shared kernel and the structural pressure each reading places on funding and legitimacy conditions for the others (this bridge reading, if dominant, structurally starves both specialist camps of undivided resources; each specialist reading, if dominant, would eliminate the bridge reading's funding rationale entirely).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
