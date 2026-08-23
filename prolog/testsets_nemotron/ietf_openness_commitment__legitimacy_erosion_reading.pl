% ============================================================================
% CONSTRAINT STORY: ietf_openness_commitment__legitimacy_erosion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-28
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   The IETF's rough consensus mechanism — 'we reject kings, presidents, and
 *   voting; we believe in rough consensus and running code' — was designed as
 *   a lightweight coordination mechanism for protocol standardization. Over
 *   three decades, the mechanism itself has become a contested terrain:
 *   well-resourced actors (large tech consortia, incumbent editors, aligned
 *   open-source projects) have learned to harvest the procedural legitimacy
 *   of 'IETF consensus' to ratify outcomes that serve their architectural and
 *   commercial interests. The constraint is not the IETF as an institution,
 *   but the rough consensus mechanism as a legitimacy-generating device that
 *   can be gamed. This reading instantiates the legitimacy_erosion_reading of
 *   the ietf_openness_commitment kernel: the mechanism's credibility is the
 *   resource being extracted, and its erosion is the primary victim.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ietf_openness_commitment__legitimacy_erosion_reading, 0.68).
domain_priors:suppression_score(ietf_openness_commitment__legitimacy_erosion_reading, 0.55).
domain_priors:theater_ratio(ietf_openness_commitment__legitimacy_erosion_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ietf_openness_commitment__legitimacy_erosion_reading, tangled_rope).
narrative_ontology:human_readable(ietf_openness_commitment__legitimacy_erosion_reading, "IETF Rough Consensus Legitimacy Erosion").
narrative_ontology:topic_domain(ietf_openness_commitment__legitimacy_erosion_reading, "technology_governance/internet_standards/institutional_economics").

domain_priors:requires_active_enforcement(ietf_openness_commitment__legitimacy_erosion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ietf_openness_commitment__legitimacy_erosion_reading, 'f2c0d1e3-ddea-4214-8c41-d2786c8ec574').
narrative_ontology:cs_kernel_codification('f2c0d1e3-ddea-4214-8c41-d2786c8ec574', implicit).
narrative_ontology:cs_authority_grounding('f2c0d1e3-ddea-4214-8c41-d2786c8ec574', practice).
narrative_ontology:cs_interpretation_layer_present('f2c0d1e3-ddea-4214-8c41-d2786c8ec574').
narrative_ontology:cs_reading_relation('f2c0d1e3-ddea-4214-8c41-d2786c8ec574', ietf_openness_commitment__commons_stewardship_reading, coexists_with).
narrative_ontology:cs_reading_relation('f2c0d1e3-ddea-4214-8c41-d2786c8ec574', ietf_openness_commitment__capture_substrate_reading, influences).
narrative_ontology:cs_axiom('f2c0d1e3-ddea-4214-8c41-d2786c8ec574', foundational, legitimacy_is_extractable_resource).
narrative_ontology:cs_axiom_status(legitimacy_is_extractable_resource, holdable).
narrative_ontology:cs_axiom_grounding('f2c0d1e3-ddea-4214-8c41-d2786c8ec574', legitimacy_is_extractable_resource, empirically_contingent).
narrative_ontology:cs_axiom('f2c0d1e3-ddea-4214-8c41-d2786c8ec574', foundational, rough_consensus_mechanism_is_capture_vulnerable).
narrative_ontology:cs_axiom_status(rough_consensus_mechanism_is_capture_vulnerable, holdable).
narrative_ontology:cs_axiom_grounding('f2c0d1e3-ddea-4214-8c41-d2786c8ec574', rough_consensus_mechanism_is_capture_vulnerable, empirically_contingent).
narrative_ontology:cs_reference_frame('f2c0d1e3-ddea-4214-8c41-d2786c8ec574', rough_consensus_as_functional_coordination).
narrative_ontology:cs_drift_state('f2c0d1e3-ddea-4214-8c41-d2786c8ec574', post_consolidation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f2c0d1e3-ddea-4214-8c41-d2786c8ec574', '').
narrative_ontology:cs_kernel_id(ietf_openness_commitment__legitimacy_erosion_reading, ietf_openness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__legitimacy_erosion_reading, incumbent_standards_editors).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__legitimacy_erosion_reading, large_tech_consortia).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__legitimacy_erosion_reading, protocol_oss_maintainers_aligned_with_incumbents).
narrative_ontology:constraint_victim(ietf_openness_commitment__legitimacy_erosion_reading, independent_implementers).
narrative_ontology:constraint_victim(ietf_openness_commitment__legitimacy_erosion_reading, emerging_standards_proponents).
narrative_ontology:constraint_victim(ietf_openness_commitment__legitimacy_erosion_reading, consensus_credibility_commons).
narrative_ontology:constraint_vindicates(ietf_openness_commitment__legitimacy_erosion_reading, procedural_legitimacy_can_be_harvested_as_an_asset).
narrative_ontology:constraint_vindicates(ietf_openness_commitment__legitimacy_erosion_reading, rough_consensus_is_insufficient_against_organized_capture).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold editorial control over working group charters, document progression, and consensus determination. They determine what counts as 'rough consensus' in practice. Their position depends on the legitimacy of the process they administer, yet they benefit from procedural flexibility that lets aligned proposals advance while blocking challengers. Exit means leaving the standards body entirely — they have high mobility but lose the authority that makes them valuable.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, incumbent_standards_editors, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ietf_openness_commitment__legitimacy_erosion_reading, incumbent_standards_editors, beneficiary).

% Deploy dedicated standards teams that attend every meeting, write reference implementations, and shape the 'running code' that rough consensus privileges. They extract value by encoding their proprietary architectures into standards, then claiming interoperability mandates as validation. They can exit to proprietary APIs or competing standards bodies, but the IETF's legitimacy commons makes their captured standards more valuable than alternatives.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, large_tech_consortia, beneficiary,
    powerful, biographical, mobile, global).

% Maintain the reference implementations that become de facto compliance tests. Their projects receive corporate funding and ecosystem adoption because they align with the consortia's architecture. They benefit from the legitimacy halo of 'IETF standard' while their exit is constrained by ecosystem lock-in — forking means losing the network effects the standard confers.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, protocol_oss_maintainers_aligned_with_incumbents, beneficiary,
    organized, biographical, constrained, global).

% Must implement standards shaped by incumbent priorities to achieve interoperability. They bear the cost of conforming to architectures they had no meaningful voice in shaping — complex, over-specified, or encumbered by patent pools. Their exit options are constrained: non-compliance means market exclusion; clean-room alternatives require resources they lack. They pay in engineering effort and strategic dependency.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, independent_implementers, payer,
    moderate, biographical, constrained, global).

% Propose alternatives that challenge incumbent architectures. They face procedural hurdles — charter scope objections, 'running code' requirements that favor incumbents with dedicated teams, consensus calls where volume of participation is mistaken for breadth. They are trapped: the only venue with global legitimacy is the one where the game is rigged. Their extraction is the opportunity cost of proposals that never reach RFC.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, emerging_standards_proponents, payer,
    powerless, biographical, trapped, global).

% The shared epistemic asset that makes 'IETF standard' a meaningful quality signal. It is depleted each time rough consensus ratifies an outcome that serves a narrow coalition rather than the implementer commons. It cannot exit — it is the substrate being mined. Its degradation is the constraint's primary victim: when credibility collapses, the coordination function fails for everyone.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, consensus_credibility_commons, payer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(ietf_openness_commitment__legitimacy_erosion_reading, consensus_credibility_commons).

% Study standards governance, measure capture indicators, document legitimacy erosion. They have no formal role in the process but their analyses inform the external legitimacy that the IETF trades on. Their exit is analytical — they can shift attention to other venues, but their work constitutes the historical record of whether the mechanism works.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, academic_internet_researchers, observer,
    analytical, generational, analytical, global).

% Monitor whether standards bodies function as open coordination mechanisms or as cartel vehicles. They can impose remedies (mandating FRAND terms, blocking standard-essential patent abuse) that alter the constraint's enforcement. Their exit is analytical — they engage when the legitimacy signal degrades enough to trigger investigation.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, regulatory_competition_authorities, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Rough consensus enables distributed technical coordination without formal voting: implementers signal support through participation and running code, allowing standards to emerge from operational experience rather than institutional fiat. It solves the problem of agreeing on protocols across organizational boundaries where no party has authority to compel.
% TRANSFER_FUNCTION: Moves procedural legitimacy — the credible signal that a specification reflects broad implementer agreement — from the consensus commons to specific proposals backed by well-resourced factions. The legitimacy is extracted by incumbent-aligned coalitions to ratify architectures that entrench their market position, while the cost (eroded credibility, excluded alternatives) is borne by the commons and independent implementers.
% ABSENT_VOICES: Small implementers and users in the Global South who lack resources to sustain multi-year standards participation. Civil society groups advocating for privacy, accessibility, or human-rights-by-design in protocols — they are excluded by the resource intensity of effective participation, not by formal rules. Their absence is structural: the mechanism selects for actors who can afford sustained presence.
% DISAPPEARANCE_RATIONALE: If rough consensus vanished overnight, the IETF would either adopt formal voting (shifting power to national bodies and corporate members), fragment into competing venues (W3C, IEEE, industry consortia), or collapse into de facto proprietary standardization. The global interoperability commons would reorganize around whatever coordination mechanism replaces it — likely less open, more fragmented, and more captured.
% FOUNDING_PROBLEM: Early Internet standardization needed a decision procedure that worked without centralized authority, accommodated diverse stakeholders (vendors, researchers, operators), and produced specifications that implementers would actually deploy. Formal voting was seen as vulnerable to block-voting by large organizations; rough consensus aimed to weight operational commitment over institutional headcount.
% FOUNDING_PROBLEM_CORROBORATION: The IETF's own historical record (RFC 7282, RFC 5218) attests the founding problem as live — operational consensus remains the espoused ideal. Independent scholars (e.g., Russell 2006 'Open Standards and the Digital Age', DeNardis 2014 'The Global War for Internet Governance') document that the mechanism has always favored resource-rich participants; the 'running code' criterion was co-opted by those who could fund development teams. No corroborating source outside the beneficiary set affirms the mechanism works as originally envisioned.
narrative_ontology:disappearance_verdict(ietf_openness_commitment__legitimacy_erosion_reading, world_rearranges).
narrative_ontology:founding_problem_status(ietf_openness_commitment__legitimacy_erosion_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ietf_openness_commitment__legitimacy_erosion_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(ietf_openness_commitment__legitimacy_erosion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ietf_openness_commitment__legitimacy_erosion_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.68) is high because the legitimacy commons — the credible signal that a standard reflects broad implementer agreement — is being mined by coalitions that can simulate consensus through sustained participation, reference implementations, and editorial influence. Suppression (0.55) is moderate: the mechanism does not formally block alternatives, but the resource requirements for effective participation (travel, dedicated engineers, multi-year commitment) functionally suppress challengers. Theater ratio (0.42) is significant: the ritual of 'humming' and mailing list debate persists, but a growing share of the process is performative — the outcome is often determined before the hum by who wrote the running code and who chairs the working group. Accessibility collapse (0.38) is partial: alternatives exist (other SDOs, proprietary APIs) but the IETF's legitimacy halo makes them inferior substitutes for global deployment. Resistance (0.52) is real: independent implementers and emerging proponents do challenge capture, but their resistance is asymmetric — they must win every time; incumbents need only win once to encode their architecture.
 *
 * PERSPECTIVAL GAP:
 *   From the incumbent editor's seat, rough consensus is a functional coordination mechanism that produces deployable standards — they see the successes (HTTP, TLS, DNS) and attribute them to the process. From the independent implementer's seat, the same process is a barrier that privileges architectures they had no say in — they see the failed proposals, the charter scope games, the 'running code' requirements that favor funded teams. The engine computes this divergence from the structural data; the authored claim (tangled_rope) reflects that both coordination and extraction are real and inseparable in the current mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent editors and large consortia sit at the beneficiary end (d ~0.15–0.25): they control the agenda, define what counts as consensus, and capture the legitimacy surplus. Independent implementers and emerging proponents sit at the target end (d ~0.75–0.85): they pay the cost of conformance without meaningful voice, and their exit is constrained by the network effects of the standard. The consensus_credibility_commons is a non-agent victim — it bears the extraction but has no voice. The derivation chain assigns low d to agenda_setters with arbitrage exit, high d to powerless/constrained payers, analytical d to observers. The structural asymmetry is genuine: the same procedural mechanism produces radically different lived experiences depending on resource position.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (coordination without central authority) is contested as live — the mechanism's defenders argue it still works, critics argue it has been captured. The mandate has not atrophied; rather, the mechanism has been repurposed. The coordination function persists (standards still get written and deployed) but the extraction function has grown to dominate. This is not mandatrophy (a former function persisting inertially) but functional capture: the coordination machinery is actively used to produce extractive outcomes. The theater ratio rise tracks this — more process energy goes into defending the legitimacy signal than into genuine coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consensus_legitimacy_separability,
    'Can the legitimacy signal of ''rough consensus'' be separated from the coordination function it was designed to serve, or are they structurally inseparable such that degrading legitimacy inevitably degrades coordination?',
    'Natural experiment: observe coordination outcomes in venues that have adopted formal voting (e.g., W3C, IEEE) or in IETF working groups that have lost external credibility. If coordination persists without legitimacy, the signal is extractable; if coordination collapses with legitimacy, they are coupled.',
    'If separable, the mechanism is a snare — legitimacy is pure extraction riding on a coordination function that could survive without it. If inseparable, it is a tangled rope — the extraction damages the very coordination it parasitizes, creating a self-limiting dynamic.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consensus_legitimacy_separability, conceptual, 'Whether legitimacy and coordination are separable in the rough consensus mechanism.').

omega_variable(
    capture_threshold_identification,
    'At what level of resource asymmetry does rough consensus shift from functional coordination to capture substrate? Is there a measurable threshold (participation concentration, implementation funding share, editorial tenure) that predicts capture?',
    'Longitudinal analysis of working group participation metrics, funding disclosures, and consensus outcomes correlated with post-standardization market concentration. Compare working groups with high vs. low resource concentration.',
    'A measurable threshold would enable early-warning metrics for capture and structural reforms (quorum rules, funding transparency, editorial term limits). Absent a threshold, capture is a gradual degradation with no clear intervention point.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capture_threshold_identification, empirical, 'Whether capture has a detectable structural threshold or is a continuous degradation.').

omega_variable(
    reading_relation_to_commons_stewardship,
    'Does the legitimacy_erosion_reading foreclose the commons_stewardship_reading, or do they coexist as competing framings of the same kernel held by different factions?',
    'Analyze whether any single institutional framework could simultaneously hold both: that the mechanism preserves the commons AND that its legitimacy is being extracted. If the IETF''s own reform efforts (e.g., RFC 7282 updates, diversity initiatives) assume the commons is preservable while acknowledging erosion, they coexist. If reform is structurally impossible under the erosion reading, it forecloses.',
    'If forecloses, the kernel has a logical fracture — one reading must be abandoned for the other to be maintained. If coexists_with, the dispute is political/institutional, not logical, and the engine should model them as simultaneous live positions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_relation_to_commons_stewardship, conceptual, 'Structural relationship between legitimacy_erosion_reading and commons_stewardship_reading.').

omega_variable(
    reading_relation_to_capture_substrate,
    'Does the legitimacy_erosion_reading influence the capture_substrate_reading (creating downstream pressure) or coexist with it as a parallel framing?',
    'Trace whether legitimacy erosion (this reading''s focus) changes the conditions for resource-advantage gatekeeping (capture_substrate_reading''s focus). If depleted legitimacy makes the process more vulnerable to gatekeeping (e.g., fewer independent participants, lower scrutiny), this reading influences the sibling.',
    'If influences, the readings form a causal chain: legitimacy erosion enables deeper capture. If coexists_with, they are independent lenses on the same phenomenon.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_relation_to_capture_substrate, conceptual, 'Structural relationship between legitimacy_erosion_reading and capture_substrate_reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ietf_openness_commitment__legitimacy_erosion_reading, 1992, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ietf_legitimacy_erosion_tr_t1992, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 1992, 0.05).
narrative_ontology:measurement(ietf_legitimacy_erosion_tr_t1998, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 1998, 0.12).
narrative_ontology:measurement(ietf_legitimacy_erosion_tr_t2004, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 2004, 0.22).
narrative_ontology:measurement(ietf_legitimacy_erosion_tr_t2010, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 2010, 0.3).
narrative_ontology:measurement(ietf_legitimacy_erosion_tr_t2016, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 2016, 0.37).
narrative_ontology:measurement(ietf_legitimacy_erosion_tr_t2020, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 2020, 0.4).
narrative_ontology:measurement(ietf_legitimacy_erosion_tr_t2025, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(ietf_legitimacy_erosion_be_t1992, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 1992, 0.15).
narrative_ontology:measurement(ietf_legitimacy_erosion_be_t1998, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 1998, 0.22).
narrative_ontology:measurement(ietf_legitimacy_erosion_be_t2004, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 2004, 0.35).
narrative_ontology:measurement(ietf_legitimacy_erosion_be_t2010, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 2010, 0.45).
narrative_ontology:measurement(ietf_legitimacy_erosion_be_t2016, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 2016, 0.55).
narrative_ontology:measurement(ietf_legitimacy_erosion_be_t2020, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 2020, 0.62).
narrative_ontology:measurement(ietf_legitimacy_erosion_be_t2025, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ietf_legitimacy_erosion_su_t1992, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 1992, 0.1).
narrative_ontology:measurement(ietf_legitimacy_erosion_su_t1998, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 1998, 0.18).
narrative_ontology:measurement(ietf_legitimacy_erosion_su_t2004, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 2004, 0.3).
narrative_ontology:measurement(ietf_legitimacy_erosion_su_t2010, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 2010, 0.4).
narrative_ontology:measurement(ietf_legitimacy_erosion_su_t2016, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 2016, 0.48).
narrative_ontology:measurement(ietf_legitimacy_erosion_su_t2020, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 2020, 0.52).
narrative_ontology:measurement(ietf_legitimacy_erosion_su_t2025, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ietf_openness_commitment__legitimacy_erosion_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ietf_openness_commitment__legitimacy_erosion_reading, 0.12).
narrative_ontology:affects_constraint(ietf_openness_commitment__legitimacy_erosion_reading, ietf_openness_commitment__commons_stewardship_reading).
narrative_ontology:affects_constraint(ietf_openness_commitment__legitimacy_erosion_reading, ietf_openness_commitment__capture_substrate_reading).

% DUAL FORMULATION NOTE:
% Part of the ietf_openness_commitment constraint family. This reading (legitimacy_erosion_reading) focuses on the rough consensus mechanism's credibility as the extraction target. The commons_stewardship_reading frames the kernel as a protective constraint preserving interoperability. The capture_substrate_reading frames it as an enabling substrate where resource advantage becomes encoded gatekeeping. The three readings share the kernel but instantiate different constraints with different ε values and victim structures. Legitimacy erosion (this reading) is upstream of capture substrate — depleted legitimacy lowers the cost of gatekeeping. Commons stewardship is the normative ideal both other readings reference as the degraded or captured state.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ietf_openness_commitment__legitimacy_erosion_reading, institutional, 0.18).
constraint_indexing:directionality_override(ietf_openness_commitment__legitimacy_erosion_reading, powerful, 0.22).
constraint_indexing:directionality_override(ietf_openness_commitment__legitimacy_erosion_reading, organized, 0.3).
constraint_indexing:directionality_override(ietf_openness_commitment__legitimacy_erosion_reading, moderate, 0.78).
constraint_indexing:directionality_override(ietf_openness_commitment__legitimacy_erosion_reading, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
