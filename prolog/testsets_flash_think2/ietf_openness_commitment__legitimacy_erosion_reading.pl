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
 *   human_readable: IETF Rough Consensus Legitimacy Erosion
 *   domain: technology_governance/internet_standards/institutional_economics
 *
 * SUMMARY:
 *   This constraint is the 'legitimacy_erosion_reading' of the
 *   'ietf_openness_commitment' kernel. It describes how the IETF's rough
 *   consensus mechanism, intended for open and technically driven standards
 *   development, is perceived as being increasingly vulnerable to organized
 *   capture by well-resourced factions. This capture leads to an erosion of
 *   the mechanism's credibility and the perception of fairness, as outcomes
 *   are seen to serve specific interests rather than broad technical merit.
 *   Sibling readings include 'commons_stewardship_reading' and
 *   'capture_substrate_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ietf_openness_commitment__legitimacy_erosion_reading, 0.85).
domain_priors:suppression_score(ietf_openness_commitment__legitimacy_erosion_reading, 0.75).
domain_priors:theater_ratio(ietf_openness_commitment__legitimacy_erosion_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ietf_openness_commitment__legitimacy_erosion_reading, snare).
narrative_ontology:human_readable(ietf_openness_commitment__legitimacy_erosion_reading, "IETF Rough Consensus Legitimacy Erosion").
narrative_ontology:topic_domain(ietf_openness_commitment__legitimacy_erosion_reading, "technology_governance/internet_standards/institutional_economics").

domain_priors:requires_active_enforcement(ietf_openness_commitment__legitimacy_erosion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ietf_openness_commitment__legitimacy_erosion_reading, '5096738e-d796-441a-b7b5-c1d8108d5347').
narrative_ontology:cs_kernel_codification('5096738e-d796-441a-b7b5-c1d8108d5347', formalized).
narrative_ontology:cs_authority_grounding('5096738e-d796-441a-b7b5-c1d8108d5347', practice).
narrative_ontology:cs_interpretation_layer_present('5096738e-d796-441a-b7b5-c1d8108d5347').
narrative_ontology:cs_reading_relation('5096738e-d796-441a-b7b5-c1d8108d5347', ietf_openness_commitment__commons_stewardship_reading, coexists_with).
narrative_ontology:cs_reading_relation('5096738e-d796-441a-b7b5-c1d8108d5347', ietf_openness_commitment__capture_substrate_reading, coexists_with).
narrative_ontology:cs_axiom('5096738e-d796-441a-b7b5-c1d8108d5347', foundational, legitimacy_derived_from_unbiased_process).
narrative_ontology:cs_axiom_status(legitimacy_derived_from_unbiased_process, holdable).
narrative_ontology:cs_axiom_grounding('5096738e-d796-441a-b7b5-c1d8108d5347', legitimacy_derived_from_unbiased_process, deontological).
narrative_ontology:cs_axiom('5096738e-d796-441a-b7b5-c1d8108d5347', secondary, technical_merit_is_paramount).
narrative_ontology:cs_axiom_status(technical_merit_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('5096738e-d796-441a-b7b5-c1d8108d5347', technical_merit_is_paramount, conventional).
narrative_ontology:cs_reference_frame('5096738e-d796-441a-b7b5-c1d8108d5347', unbiased_technical_meritocracy).
narrative_ontology:cs_drift_state('5096738e-d796-441a-b7b5-c1d8108d5347', contemporary_internet_governance, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5096738e-d796-441a-b7b5-c1d8108d5347', '').
narrative_ontology:cs_kernel_id(ietf_openness_commitment__legitimacy_erosion_reading, ietf_openness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__legitimacy_erosion_reading, well_resourced_factions).
narrative_ontology:constraint_victim(ietf_openness_commitment__legitimacy_erosion_reading, ietf_legitimacy_commons).
narrative_ontology:constraint_victim(ietf_openness_commitment__legitimacy_erosion_reading, independent_participants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These are organized groups (e.g., large corporations, state-sponsored actors) that strategically engage with the IETF process. They leverage their resources to influence discussions, shape proposals, and ultimately ratify standards that serve their specific interests, thereby extracting procedural legitimacy from the rough consensus mechanism.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, well_resourced_factions, agenda_setter,
    institutional, generational, arbitrage, global).

% Represents the collective credibility, perceived fairness, and public trust in the IETF's rough consensus process. This 'commons' is the primary target of extraction, as its integrity is eroded when well-resourced factions capture the process to ratify self-serving outcomes.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, ietf_legitimacy_commons, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(ietf_openness_commitment__legitimacy_erosion_reading, ietf_legitimacy_commons).

% Individual engineers, researchers, and smaller organizations who genuinely seek to contribute to open internet standards based on technical merit. They bear the cost of a degraded process through increased effort, frustration, and the marginalization of their input when confronted by organized capture.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, independent_participants, payer,
    moderate, biographical, constrained, global).

% The Internet Engineering Steering Group (IESG) and other IETF administrative bodies. They are tasked with upholding the IETF's principles of rough consensus and technical merit. In this reading, they may be overwhelmed by the subtle nature of capture, or their efforts to maintain fairness are insufficient against well-resourced manipulation, leading to an unwitting complicity in the erosion of legitimacy.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, ietf_leadership, agenda_setter,
    institutional, biographical, constrained, global).

% The global community that relies on open internet standards for interoperability, security, and innovation. They are indirectly affected by the erosion of the IETF's legitimacy, as captured standards may lead to less optimal, less open, or biased internet infrastructure, but they have no direct voice in the IETF process.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, internet_users, observer,
    powerless, generational, trapped, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The IETF's rough consensus mechanism aims to coordinate diverse technical expertise and perspectives to produce interoperable, technically sound internet standards through open discussion and broad agreement, avoiding formal voting.
% TRANSFER_FUNCTION: Transfers procedural legitimacy and the perception of open, fair decision-making from the collective IETF process to specific, often self-serving, technical outcomes favored by well-resourced factions. It also transfers the burden of countering organized influence onto independent participants.
% ABSENT_VOICES: Participants lacking significant organizational backing, funding, or dedicated staff to sustain long-term engagement are effectively marginalized. Their technical input, even if meritorious, can be drowned out or outmaneuvered by well-resourced factions, making them functionally absent from critical phases of consensus building.
% DISAPPEARANCE_RATIONALE: If the rough consensus mechanism's legitimacy completely eroded, the IETF's ability to produce widely adopted, trusted standards would collapse. This would lead to fragmentation of the internet's technical foundations, a proliferation of proprietary standards, and a less interoperable, less secure global network, fundamentally reorganizing how internet technology evolves.
% FOUNDING_PROBLEM: To create a decentralized, open, and technically sound process for developing internet standards that avoids capture by commercial or political interests, ensuring the internet's continued openness and interoperability.
% FOUNDING_PROBLEM_CORROBORATION: The IETF leadership and many long-time participants attest that the problem of maintaining open, uncaptured standards is still live and an ongoing challenge. However, critics, independent researchers, and some academic observers attest that the mechanism has been substantially captured by well-resourced factions, and the original problem is now exacerbated by the very process meant to solve it, supporting the shifted-function reading.
narrative_ontology:disappearance_verdict(ietf_openness_commitment__legitimacy_erosion_reading, world_rearranges).
narrative_ontology:founding_problem_status(ietf_openness_commitment__legitimacy_erosion_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ietf_openness_commitment__legitimacy_erosion_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(ietf_openness_commitment__legitimacy_erosion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ietf_openness_commitment__legitimacy_erosion_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.85) reflects the significant cost to the IETF's legitimacy and the independent participants' efforts. Suppression (0.75) is high because organized factions actively marginalize dissenting voices and manipulate procedural safeguards, making genuine consensus difficult to achieve against their interests. The theater ratio (0.60) indicates that while the outward forms of open discussion and rough consensus are maintained, a substantial portion of the activity serves to legitimize pre-determined outcomes rather than genuinely seek broad agreement. The increasing trend in all metrics over the interval reflects the growing perception of capture and erosion.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of well-resourced factions, the process is merely a strategic arena where influence is exerted to achieve legitimate outcomes. From the perspective of independent participants and the 'legitimacy commons,' the same process is a snare, extracting the very credibility it purports to uphold. The IETF leadership may perceive it as a challenging but ultimately functional coordination mechanism, struggling to maintain its ideals.
 *
 * DIRECTIONALITY LOGIC:
 *   Well-resourced factions are the primary beneficiaries (d near 0.0) as they successfully ratify self-serving standards. The 'ietf_legitimacy_commons' and independent participants are the primary targets (d near 1.0), bearing the costs of eroded trust and marginalized input. IETF leadership, while administering the process, may find themselves constrained, with their efforts to maintain fairness often subverted, placing them closer to a symmetric or slightly targeted position depending on their effectiveness.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading suggests a form of mandatrophy where the original mandate of open, uncaptured standards development has been subverted. The mechanism persists, but its function has drifted from genuine coordination to a legitimizing cover for extraction. The classification as a Snare highlights that the coordination story (rough consensus) is largely a cover for the extraction of legitimacy, preventing mislabeling it as a functional Rope or Tangled Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capture_detection_threshold,
    'At what point does ''rough consensus'' become ''organized capture''?',
    'Empirical studies analyzing IETF working group dynamics, resource disparities among participants, and correlation between factional influence and standard outcomes, combined with a clear definition of ''capture'' in this context.',
    'A clearer threshold would allow for more precise classification of specific IETF standards as products of capture, rather than genuine consensus, potentially reclassifying some as Snares or Tangled Ropes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capture_detection_threshold, empirical, 'Ambiguity in distinguishing genuine rough consensus from organized capture.').

omega_variable(
    legitimacy_measurement_validity,
    'How can the ''erosion of legitimacy'' be objectively measured and corroborated beyond anecdotal evidence or participant perception?',
    'Development of quantitative metrics for legitimacy, such as participant diversity trends, rates of appeals against IESG decisions, external stakeholder trust surveys, or analysis of standard adoption patterns by non-aligned implementers.',
    'Robust measurement would strengthen the empirical basis for the ''legitimacy erosion'' claim, providing stronger evidence for the high extractiveness and supporting the Snare classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_measurement_validity, empirical, 'Challenges in objectively measuring the erosion of an abstract ''legitimacy commons''.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint truly a distinct ''legitimacy_erosion_reading'' of the ''ietf_openness_commitment'' kernel, or is it better understood as a specific instance of the ''capture_substrate_reading''?',
    'Conceptual analysis distinguishing between the mechanism *being* a substrate for capture (capture_substrate_reading) versus the *consequence* of capture being the erosion of the mechanism''s perceived legitimacy (this reading). If the primary focus is the *effect* on trust and credibility, this reading is distinct.',
    'If the distinction is weak, this reading might be merged or re-framed as a specific aspect of the ''capture_substrate_reading'', potentially altering its classification or network relationships.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Distinction between the mechanism as a substrate for capture versus the erosion of its legitimacy as a consequence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ietf_openness_commitment__legitimacy_erosion_reading, 2005, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ietf_tr_t2005, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 2005, 0.3).
narrative_ontology:measurement(ietf_tr_t2009, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 2009, 0.38).
narrative_ontology:measurement(ietf_tr_t2013, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 2013, 0.45).
narrative_ontology:measurement(ietf_tr_t2017, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 2017, 0.52).
narrative_ontology:measurement(ietf_tr_t2021, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 2021, 0.57).
narrative_ontology:measurement(ietf_tr_t2025, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 2025, 0.6).

% Extraction over time
narrative_ontology:measurement(ietf_be_t2005, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 2005, 0.6).
narrative_ontology:measurement(ietf_be_t2009, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 2009, 0.68).
narrative_ontology:measurement(ietf_be_t2013, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 2013, 0.75).
narrative_ontology:measurement(ietf_be_t2017, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 2017, 0.8).
narrative_ontology:measurement(ietf_be_t2021, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 2021, 0.83).
narrative_ontology:measurement(ietf_be_t2025, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 2025, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(ietf_su_t2005, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 2005, 0.55).
narrative_ontology:measurement(ietf_su_t2009, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 2009, 0.62).
narrative_ontology:measurement(ietf_su_t2013, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 2013, 0.68).
narrative_ontology:measurement(ietf_su_t2017, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 2017, 0.72).
narrative_ontology:measurement(ietf_su_t2021, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 2021, 0.74).
narrative_ontology:measurement(ietf_su_t2025, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 2025, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ietf_openness_commitment__legitimacy_erosion_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ietf_openness_commitment__legitimacy_erosion_reading, ietf_standard_x_adoption).
narrative_ontology:affects_constraint(ietf_openness_commitment__legitimacy_erosion_reading, internet_protocol_evolution).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'ietf_openness_commitment' kernel. It focuses on the erosion of legitimacy due to capture, distinct from the 'commons_stewardship_reading' (which views it as public infrastructure) and the 'capture_substrate_reading' (which views the process itself as a substrate for capture).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
