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
 *   human_readable: IETF Rough Consensus: Legitimacy Erosion Reading
 *   domain: technology_governance/internet_standards/institutional_economics
 *
 * SUMMARY:
 *   This constraint describes the IETF's 'rough consensus' mechanism from the
 *   perspective of its legitimacy eroding due to organized capture. While the
 *   IETF aims for open, technically driven standards, this reading argues
 *   that well-resourced factions exploit procedural openness to ratify
 *   self-serving outcomes, effectively extracting the mechanism's
 *   credibility. The constraint is claimed as a 'snare' because the
 *   coordination story (open standards development) serves as a cover for
 *   asymmetric extraction of legitimacy and market advantage, with
 *   identifiable victims in the form of independent implementers and the
 *   IETF's overall credibility.
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
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ietf_openness_commitment__legitimacy_erosion_reading, snare).
narrative_ontology:human_readable(ietf_openness_commitment__legitimacy_erosion_reading, "IETF Rough Consensus: Legitimacy Erosion Reading").
narrative_ontology:topic_domain(ietf_openness_commitment__legitimacy_erosion_reading, "technology_governance/internet_standards/institutional_economics").

domain_priors:requires_active_enforcement(ietf_openness_commitment__legitimacy_erosion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ietf_openness_commitment__legitimacy_erosion_reading, 'c55f3297-d747-43b4-8a55-1a63b4d51e1a').
narrative_ontology:cs_kernel_codification('c55f3297-d747-43b4-8a55-1a63b4d51e1a', formalized).
narrative_ontology:cs_authority_grounding('c55f3297-d747-43b4-8a55-1a63b4d51e1a', practice).
narrative_ontology:cs_interpretation_layer_present('c55f3297-d747-43b4-8a55-1a63b4d51e1a').
narrative_ontology:cs_reading_relation('c55f3297-d747-43b4-8a55-1a63b4d51e1a', ietf_openness_commitment__commons_stewardship_reading, coexists_with).
narrative_ontology:cs_reading_relation('c55f3297-d747-43b4-8a55-1a63b4d51e1a', ietf_openness_commitment__capture_substrate_reading, coexists_with).
narrative_ontology:cs_axiom('c55f3297-d747-43b4-8a55-1a63b4d51e1a', foundational, procedural_openness_is_insufficient_for_fairness).
narrative_ontology:cs_axiom_status(procedural_openness_is_insufficient_for_fairness, holdable).
narrative_ontology:cs_axiom_grounding('c55f3297-d747-43b4-8a55-1a63b4d51e1a', procedural_openness_is_insufficient_for_fairness, empirically_contingent).
narrative_ontology:cs_axiom('c55f3297-d747-43b4-8a55-1a63b4d51e1a', foundational, resource_disparity_corrupts_consensus).
narrative_ontology:cs_axiom_status(resource_disparity_corrupts_consensus, holdable).
narrative_ontology:cs_axiom_grounding('c55f3297-d747-43b4-8a55-1a63b4d51e1a', resource_disparity_corrupts_consensus, empirically_contingent).
narrative_ontology:cs_reference_frame('c55f3297-d747-43b4-8a55-1a63b4d51e1a', ideal_rough_consensus_fairness).
narrative_ontology:cs_drift_state('c55f3297-d747-43b4-8a55-1a63b4d51e1a', contemporary_corporate_influence_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('c55f3297-d747-43b4-8a55-1a63b4d51e1a', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(ietf_openness_commitment__legitimacy_erosion_reading, ietf_openness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__legitimacy_erosion_reading, well_resourced_factions).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__legitimacy_erosion_reading, incumbent_vendors).
narrative_ontology:constraint_victim(ietf_openness_commitment__legitimacy_erosion_reading, ietf_legitimacy_commons).
narrative_ontology:constraint_victim(ietf_openness_commitment__legitimacy_erosion_reading, independent_implementers).
narrative_ontology:constraint_victim(ietf_openness_commitment__legitimacy_erosion_reading, future_innovators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary forum for technical discussion and decision-making. While procedurally open, the sheer volume of work and technical depth can be overwhelming, making it susceptible to influence by well-resourced participants who can dedicate full-time staff to the process.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, ietf_working_groups, agenda_setter,
    organized, biographical, constrained, global).

% Large corporations or consortia that can deploy significant resources (staff, legal, lobbying) to shape standards in their favor. They benefit by having their preferred technical solutions ratified as 'rough consensus,' gaining market advantage and locking in competitors. They extract procedural legitimacy to ratify self-serving outcomes.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, well_resourced_factions, beneficiary,
    institutional, generational, arbitrage, global).

% Companies with existing market dominance that can leverage their installed base and technical expertise to influence standards, ensuring new specifications are compatible with their products or disadvantage competitors. They benefit from the stability and predictability of standards that align with their interests.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, incumbent_vendors, beneficiary,
    powerful, biographical, mobile, global).

% Smaller companies, startups, or individual developers who rely on open standards for interoperability but lack the resources to actively participate in the IETF process. They bear the cost of implementing standards that may be biased towards incumbent interests, or face market exclusion if they cannot keep up.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, independent_implementers, payer,
    moderate, immediate, constrained, global).

% The collective belief in the fairness, openness, and technical merit of the IETF's rough consensus process. This 'commons' is eroded when the process is perceived as captured, leading to a loss of trust and participation from the broader community. Its credibility is the primary victim.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, ietf_legitimacy_commons, payer,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(ietf_openness_commitment__legitimacy_erosion_reading, ietf_legitimacy_commons).

% Those who will build on internet standards in the future. They are victims of a captured process that stifles genuine innovation by entrenching existing technologies and limiting the design space for new protocols, leading to a less open and dynamic internet.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, future_innovators, payer,
    powerless, generational, identity_locked, global).

% Provides architectural oversight and guidance to the IETF. While not directly involved in day-to-day standards development, it observes the health of the process and can raise concerns about systemic issues, including potential capture or erosion of consensus principles.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, internet_architecture_board, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ietf_openness_commitment__legitimacy_erosion_reading, well_resourced_factions).
narrative_ontology:fixing_cost_class(ietf_openness_commitment__legitimacy_erosion_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Facilitates the development of interoperable internet standards by bringing together diverse technical experts to achieve 'rough consensus' on protocol specifications, ensuring global connectivity and functionality.
% TRANSFER_FUNCTION: Transfers procedural legitimacy from the IETF's 'rough consensus' mechanism to specific technical proposals, effectively ratifying them as open standards. This legitimacy is then leveraged by well-resourced factions to gain market advantage, while the cost is borne by the overall credibility and fairness of the process.
% ABSENT_VOICES: Smaller, less-resourced implementers and academic researchers who cannot afford the time or travel to participate consistently. They would advocate for simpler, more accessible standards and stronger safeguards against corporate influence, but their absence allows well-resourced factions to dominate the discussion.
% DISAPPEARANCE_RATIONALE: If the IETF's rough consensus mechanism vanished overnight, the internet's ability to evolve through open, interoperable standards would collapse. Major vendors would likely fork into proprietary ecosystems, leading to fragmentation and a significant increase in transaction costs for anyone building on the internet. The global commons of interoperability would be severely damaged.
% FOUNDING_PROBLEM: The need for a decentralized, open, and technically driven process to develop and maintain the core protocols of the internet, ensuring global interoperability and preventing single-vendor lock-in.
% FOUNDING_PROBLEM_CORROBORATION: The IETF leadership and many long-time participants attest that the founding problem of open, interoperable standards development is still live and the rough consensus mechanism is the best way to address it. However, independent researchers, civil society groups, and some smaller implementers (outside the direct beneficiaries of specific standards) argue that while the problem is live, the mechanism's effectiveness has been compromised by capture, leading to a 'dead' status for its original intent of truly open and fair consensus.
narrative_ontology:disappearance_verdict(ietf_openness_commitment__legitimacy_erosion_reading, world_rearranges).
narrative_ontology:founding_problem_status(ietf_openness_commitment__legitimacy_erosion_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ietf_openness_commitment__legitimacy_erosion_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   Extractiveness is high (0.85) because the process is leveraged to secure market advantage for specific actors, often at the expense of broader interoperability or innovation. Suppression (0.75) is achieved through the sheer resource disparity and the complexity of participation, effectively marginalizing less-resourced voices. The theater ratio (0.6) reflects that while the outward appearance of open, consensus-driven debate is maintained, a significant portion of the activity is performative, masking the underlying capture dynamics. Resistance (0.7) is present from independent implementers and some academic circles, but it struggles against the organized power of the beneficiaries. Accessibility collapse (0.4) is moderate, as the process is technically open, but the effective ability to influence outcomes is severely limited for many.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of well-resourced factions, the mechanism is a 'rope' or 'tangled rope' – a necessary coordination tool that allows them to shape the internet's future, with their contributions seen as legitimate participation. From the perspective of independent implementers and those concerned with the IETF's long-term health, it operates as a 'snare,' extracting legitimacy and imposing costs. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Well-resourced factions and incumbent vendors are clear beneficiaries (d near 0.0) as they directly shape and benefit from the ratified standards. Independent implementers and future innovators are targets (d near 1.0) as they bear the costs of biased standards and reduced innovation. The 'IETF legitimacy commons' is a direct victim, as its credibility is eroded by the perceived capture. The IETF working groups, while nominally agenda-setters, are themselves constrained by the dynamics of participation and resource imbalance, making them both beneficiaries (of a functioning process) and targets (of capture).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading suggests a form of mandatrophy where the original mandate of open, fair standards development has been subverted. The classification as a 'snare' prevents mislabeling this as genuine coordination, highlighting the active extraction of legitimacy. The high theater ratio indicates that the performative aspects of 'rough consensus' are maintained even as its core function is compromised. Resolving this mandatrophy would require structural changes to rebalance participation and reduce the influence of well-resourced factions, rather than merely adjusting procedural rules.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capture_extent_empirical,
    'To what extent is the IETF''s rough consensus mechanism actually captured by well-resourced factions, versus genuinely reflecting broad technical agreement?',
    'Empirical analysis of standards outcomes: correlation between resource investment by specific actors and the adoption of their preferred technical solutions; analysis of dissenting voices'' suppression or marginalization; comparison of IETF standards with alternative, less-resourced open source projects.',
    'Higher empirical evidence of capture would strengthen the ''snare'' classification and justify interventions to rebalance participation. Lower evidence would support a ''tangled rope'' or even ''rope'' classification, suggesting a more balanced coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capture_extent_empirical, empirical, 'Quantifying the degree of influence by well-resourced factions on IETF standards outcomes.').

omega_variable(
    legitimacy_perception_vs_reality,
    'Is the perceived erosion of legitimacy a subjective interpretation, or does it reflect an objective decline in the fairness and openness of the process?',
    'Longitudinal studies of participant satisfaction, diversity of participation, and independent expert assessments of standard neutrality. Analysis of public discourse and media coverage regarding IETF processes.',
    'If primarily subjective, interventions might focus on communication and transparency. If objective, it necessitates structural reforms to the consensus mechanism itself. This would shift the classification from a ''snare'' (active extraction) to potentially a ''piton'' (if the erosion is due to atrophy rather than active capture) or a ''tangled rope'' (if a genuine coordination function remains but is heavily biased).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_perception_vs_reality, conceptual, 'Distinguishing between perceived and actual erosion of IETF''s legitimacy.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (resource disparity, complexity) or internalized (belief in the ''rough consensus'' ideal despite evidence of capture)?',
    'Post-exit suppression trajectory: if independent implementers who leave the IETF process continue to self-censor or avoid challenging established norms in other forums, it suggests internalized suppression. If they thrive and innovate freely, it points to structural barriers within the IETF.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — targets carry the suppression with them after exit. This would amplify the ''snare'' classification''s severity. If purely structural, interventions can focus on reducing barriers to participation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in IETF participation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ietf_openness_commitment__legitimacy_erosion_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ietf_tr_t1995, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 1995, 0.1).
narrative_ontology:measurement(ietf_tr_t2000, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(ietf_tr_t2005, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 2005, 0.35).
narrative_ontology:measurement(ietf_tr_t2010, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 2010, 0.45).
narrative_ontology:measurement(ietf_tr_t2015, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 2015, 0.52).
narrative_ontology:measurement(ietf_tr_t2020, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 2020, 0.57).
narrative_ontology:measurement(ietf_tr_t2025, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 2025, 0.6).

% Extraction over time
narrative_ontology:measurement(ietf_be_t1995, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 1995, 0.3).
narrative_ontology:measurement(ietf_be_t2000, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 2000, 0.45).
narrative_ontology:measurement(ietf_be_t2005, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 2005, 0.6).
narrative_ontology:measurement(ietf_be_t2010, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 2010, 0.7).
narrative_ontology:measurement(ietf_be_t2015, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 2015, 0.78).
narrative_ontology:measurement(ietf_be_t2020, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 2020, 0.82).
narrative_ontology:measurement(ietf_be_t2025, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 2025, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(ietf_su_t1995, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 1995, 0.2).
narrative_ontology:measurement(ietf_su_t2000, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 2000, 0.35).
narrative_ontology:measurement(ietf_su_t2005, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 2005, 0.5).
narrative_ontology:measurement(ietf_su_t2010, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 2010, 0.6).
narrative_ontology:measurement(ietf_su_t2015, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 2015, 0.68).
narrative_ontology:measurement(ietf_su_t2020, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 2020, 0.72).
narrative_ontology:measurement(ietf_su_t2025, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 2025, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ietf_openness_commitment__legitimacy_erosion_reading, information_standard).
narrative_ontology:affects_constraint(ietf_openness_commitment__legitimacy_erosion_reading, ietf_openness_commitment__commons_stewardship_reading).
narrative_ontology:affects_constraint(ietf_openness_commitment__legitimacy_erosion_reading, ietf_openness_commitment__capture_substrate_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'IETF openness commitment' kernel. This 'legitimacy erosion' reading focuses on the subversion of the consensus mechanism, while the 'commons stewardship' reading emphasizes its positive coordination function, and the 'capture substrate' reading views it as a platform for resource-advantaged gatekeeping.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
