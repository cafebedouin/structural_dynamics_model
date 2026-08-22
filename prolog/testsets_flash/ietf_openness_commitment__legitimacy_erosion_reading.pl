% ============================================================================
% CONSTRAINT STORY: ietf_openness_commitment__legitimacy_erosion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: IETF Rough Consensus: Legitimacy Erosion Reading
 *   domain: technology_governance/internet_standards/institutional_economics
 *
 * SUMMARY:
 *   The IETF's 'rough consensus' mechanism, intended to ensure broad
 *   agreement without formal voting, is here read as a contested constraint
 *   vulnerable to organized capture. Despite procedural safeguards,
 *   well-resourced factions can leverage their influence (e.g., by funding
 *   working group chairs, dominating mailing lists, or submitting numerous
 *   RFCs) to steer 'rough consensus' towards outcomes that benefit their
 *   commercial interests, eroding the mechanism's perceived legitimacy and
 *   the open commons it is meant to steward. This reading focuses on the
 *   extraction of procedural legitimacy itself.
 *
 * KEY AGENTS:
 *   - well_resourced_factions: Primary beneficiary (institutional/arbitrage) — leverage resources to shape consensus.
 *   - incumbent_vendors: Secondary beneficiary (institutional/constrained) — benefit from standards that favor their existing products.
 *   - consensus_mechanism_credibility: Primary victim (non-agent) — the abstract quality of the process itself is degraded.
 *   - independent_developers: Primary payer (moderate/constrained) — bear the cost of standards that are less open or harder to implement.
 *   - future_internet_users: Secondary payer (powerless/generational) — bear the long-term cost of a less open, more fragmented internet.
 *   - ietf_leadership: Agenda setter (institutional/analytical) — administers the process, but faces pressure from powerful factions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ietf_openness_commitment__legitimacy_erosion_reading, 0.78).
domain_priors:suppression_score(ietf_openness_commitment__legitimacy_erosion_reading, 0.65).
domain_priors:theater_ratio(ietf_openness_commitment__legitimacy_erosion_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ietf_openness_commitment__legitimacy_erosion_reading, tangled_rope).
narrative_ontology:human_readable(ietf_openness_commitment__legitimacy_erosion_reading, "IETF Rough Consensus: Legitimacy Erosion Reading").
narrative_ontology:topic_domain(ietf_openness_commitment__legitimacy_erosion_reading, "technology_governance/internet_standards/institutional_economics").

domain_priors:requires_active_enforcement(ietf_openness_commitment__legitimacy_erosion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ietf_openness_commitment__legitimacy_erosion_reading, '3e7ca4c6-b6c1-4f8e-ba0d-62aaeaf98fba').
narrative_ontology:cs_kernel_codification('3e7ca4c6-b6c1-4f8e-ba0d-62aaeaf98fba', formalized).
narrative_ontology:cs_authority_grounding('3e7ca4c6-b6c1-4f8e-ba0d-62aaeaf98fba', practice).
narrative_ontology:cs_interpretation_layer_present('3e7ca4c6-b6c1-4f8e-ba0d-62aaeaf98fba').
narrative_ontology:cs_reading_relation('3e7ca4c6-b6c1-4f8e-ba0d-62aaeaf98fba', ietf_openness_commitment__commons_stewardship_reading, coexists_with).
narrative_ontology:cs_reading_relation('3e7ca4c6-b6c1-4f8e-ba0d-62aaeaf98fba', ietf_openness_commitment__capture_substrate_reading, coexists_with).
narrative_ontology:cs_axiom('3e7ca4c6-b6c1-4f8e-ba0d-62aaeaf98fba', foundational, procedural_legitimacy_is_extractable).
narrative_ontology:cs_axiom_status(procedural_legitimacy_is_extractable, holdable).
narrative_ontology:cs_axiom_grounding('3e7ca4c6-b6c1-4f8e-ba0d-62aaeaf98fba', procedural_legitimacy_is_extractable, empirically_contingent).
narrative_ontology:cs_axiom('3e7ca4c6-b6c1-4f8e-ba0d-62aaeaf98fba', foundational, resource_disparity_corrupts_consensus).
narrative_ontology:cs_axiom_status(resource_disparity_corrupts_consensus, holdable).
narrative_ontology:cs_axiom_grounding('3e7ca4c6-b6c1-4f8e-ba0d-62aaeaf98fba', resource_disparity_corrupts_consensus, empirically_contingent).
narrative_ontology:cs_reference_frame('3e7ca4c6-b6c1-4f8e-ba0d-62aaeaf98fba', ideal_rough_consensus_as_fair_process).
narrative_ontology:cs_drift_state('3e7ca4c6-b6c1-4f8e-ba0d-62aaeaf98fba', contemporary_corporate_influence_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3e7ca4c6-b6c1-4f8e-ba0d-62aaeaf98fba', '').
narrative_ontology:cs_kernel_id(ietf_openness_commitment__legitimacy_erosion_reading, ietf_openness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__legitimacy_erosion_reading, well_resourced_factions).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__legitimacy_erosion_reading, incumbent_vendors).
narrative_ontology:constraint_victim(ietf_openness_commitment__legitimacy_erosion_reading, consensus_mechanism_credibility).
narrative_ontology:constraint_victim(ietf_openness_commitment__legitimacy_erosion_reading, independent_developers).
narrative_ontology:constraint_victim(ietf_openness_commitment__legitimacy_erosion_reading, future_internet_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the IETF process, including the 'rough consensus' mechanism. They are committed to the ideals of openness but face constant pressure from powerful commercial interests. Their role is to balance competing demands while maintaining the integrity of the standards body.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, ietf_leadership, agenda_setter,
    institutional, generational, constrained, global).

% These are large corporations or industry consortia with significant resources (staff, funding) that allow them to heavily influence working groups, dominate mailing list discussions, and submit numerous proposals. They benefit by shaping standards to align with their commercial strategies, effectively extracting procedural legitimacy.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, well_resourced_factions, beneficiary,
    organized, biographical, arbitrage, global).

% Established technology companies that benefit from standards that reinforce their existing market positions or product ecosystems. They may not actively 'capture' the process but benefit from the outcomes shaped by well-resourced factions, often aligning with them.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, incumbent_vendors, beneficiary,
    powerful, biographical, constrained, global).

% Individual developers or small teams who rely on open standards for interoperability but lack the resources to participate effectively in the IETF process. They bear the cost of standards that are less open, more complex, or biased towards incumbent technologies, making their products harder to implement or less competitive.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, independent_developers, payer,
    moderate, biographical, constrained, global).

% The global population that will use the internet in the future. They are the ultimate payers of a compromised standards process, as it can lead to a less open, less innovative, or more fragmented internet, with reduced choice and increased vendor lock-in. Their costs are diffuse and long-term.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, future_internet_users, payer,
    powerless, generational, trapped, universal).

% The abstract quality of the IETF's 'rough consensus' mechanism itself. Its value is eroded when the process is perceived as captured or unfair, leading to a loss of trust in the legitimacy of the resulting standards. This is a non-agent entity that bears the cost of the erosion.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, consensus_mechanism_credibility, payer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(ietf_openness_commitment__legitimacy_erosion_reading, consensus_mechanism_credibility).

% Study the IETF process, its governance, and the impact of its standards. They provide critical analysis of the 'rough consensus' mechanism's effectiveness and vulnerabilities, often highlighting instances of capture or legitimacy erosion.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, academic_researchers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To produce globally interoperable technical standards for the internet through a process of 'rough consensus' among participants, ensuring broad agreement without formal voting.
% TRANSFER_FUNCTION: Transfers procedural legitimacy and influence over internet standards from the collective, open participation to well-resourced factions, in exchange for their continued (and often dominant) participation in the standards-setting process.
% ABSENT_VOICES: Smaller, less-resourced organizations, individual innovators, and civil society groups who cannot afford the time or travel to participate effectively. They would advocate for stronger safeguards against capture and more equitable participation mechanisms.
% DISAPPEARANCE_RATIONALE: If the 'rough consensus' mechanism and its associated commitment to openness vanished, the IETF's legitimacy would collapse. Standards would likely fragment, leading to a less interoperable internet dominated by proprietary solutions from the largest vendors, or a shift to other, potentially less open, standards bodies.
% FOUNDING_PROBLEM: To create a decentralized, open, and collaborative process for developing technical standards for the internet, avoiding the pitfalls of formal, slow, and often commercially biased national or international standards organizations.
% FOUNDING_PROBLEM_CORROBORATION: IETF leadership and some participants argue the problem is still live, citing the ongoing need for open, rapidly evolving standards. Academic researchers and independent developers, however, attest that while the problem of standards development remains, the 'rough consensus' mechanism has become a substrate for capture, shifting the nature of the problem from 'how to make standards' to 'how to make standards fairly and openly'.
narrative_ontology:disappearance_verdict(ietf_openness_commitment__legitimacy_erosion_reading, world_rearranges).
narrative_ontology:founding_problem_status(ietf_openness_commitment__legitimacy_erosion_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ietf_openness_commitment__legitimacy_erosion_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ietf_openness_commitment__legitimacy_erosion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ietf_openness_commitment__legitimacy_erosion_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.78) because the mechanism is actively used to secure outcomes that disproportionately benefit powerful actors, effectively extracting 'legitimacy' from the process to ratify self-serving standards. Suppression (0.65) reflects the difficulty for less-resourced participants to counter the organized influence of well-funded groups. Theater ratio (0.45) indicates that while the 'openness' procedures are maintained, a significant portion of the activity is performative, masking the underlying capture. The claimed type is Tangled Rope because there is a genuine coordination function (producing interoperable standards), but it is coupled with asymmetric extraction of legitimacy and influence.
 *
 * PERSPECTIVAL GAP:
 *   Well-resourced factions perceive the mechanism as a legitimate, if competitive, means to achieve consensus. Independent developers and the analytical observer perceive it as a system where procedural fairness is eroded by resource disparities, leading to outcomes that are not truly 'rough consensus' but rather 'managed consensus'. The IETF leadership may genuinely believe they are upholding the process, even as its legitimacy is eroded.
 *
 * DIRECTIONALITY LOGIC:
 *   Well-resourced factions and incumbent vendors are beneficiaries, as the mechanism allows them to shape standards to their advantage. Independent developers and future internet users are payers, bearing the costs of less open or more complex standards. The 'consensus_mechanism_credibility' is a non-agent victim, as its value is diminished by the perceived capture. IETF leadership is an agenda-setter, responsible for maintaining the process, but also subject to the pressures that drive the erosion.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the IETF's 'rough consensus' as a pure Rope (genuine coordination) by highlighting the active extraction of legitimacy and the suppression of genuine dissent. It also avoids mislabeling it as a pure Snare by acknowledging the underlying, albeit compromised, coordination function of producing standards. The Mandatrophy is not fully resolved, as the founding problem of open standards is still live, but the mechanism for achieving it is eroding.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine commitment to open standards, or a mechanism for organized capture?',
    'Longitudinal analysis of IETF RFCs for evidence of disproportionate influence from well-resourced factions, and impact on interoperability for smaller players.',
    'If capture is confirmed, the constraint reclassifies from Tangled Rope to Snare, and the ''legitimacy erosion'' reading is validated as the dominant structural reality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is the ''legitimacy_erosion_reading'' of the ''ietf_openness_commitment'' kernel. Sibling readings include ''commons_stewardship_reading'' and ''capture_substrate_reading''. The disagreement is located in the true function of the ''rough consensus'' mechanism.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (e.g., resource disparity) or internalized (e.g., belief in the fairness of the process)?',
    'Post-exit suppression trajectory: if independent developers continue to self-censor or avoid participation even after resource barriers are lowered, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as participants carry the suppression with them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in IETF participation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ietf_openness_commitment__legitimacy_erosion_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ietf_tr_t0, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ietf_tr_t5, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(ietf_tr_t10, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 10, 0.4).
narrative_ontology:measurement(ietf_tr_t15, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 15, 0.45).

% Extraction over time
narrative_ontology:measurement(ietf_be_t0, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(ietf_be_t5, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(ietf_be_t10, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 10, 0.7).
narrative_ontology:measurement(ietf_be_t15, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 15, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(ietf_su_t0, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(ietf_su_t5, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 5, 0.5).
narrative_ontology:measurement(ietf_su_t10, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(ietf_su_t15, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 15, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ietf_openness_commitment__legitimacy_erosion_reading, information_standard).
narrative_ontology:affects_constraint(ietf_openness_commitment__legitimacy_erosion_reading, ietf_openness_commitment__commons_stewardship_reading).
narrative_ontology:affects_constraint(ietf_openness_commitment__legitimacy_erosion_reading, ietf_openness_commitment__capture_substrate_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'ietf_openness_commitment' kernel. This 'legitimacy_erosion_reading' focuses on the degradation of the consensus mechanism itself due to organized capture, leading to high extractiveness of procedural legitimacy. The 'commons_stewardship_reading' (a Rope) emphasizes the positive coordination function of open standards, while the 'capture_substrate_reading' (a Snare) focuses on the direct encoding of commercial advantage into standards.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
