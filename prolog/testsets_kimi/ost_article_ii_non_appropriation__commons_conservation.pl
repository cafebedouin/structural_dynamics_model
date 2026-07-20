% ============================================================================
% CONSTRAINT STORY: ost_article_ii_non_appropriation__commons_conservation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ost_article_ii_non_appropriation__commons_conservation, []).

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
 *   constraint_id: ost_article_ii_non_appropriation__commons_conservation
 *   human_readable: Article II Commons Conservation Reading: Extraction-as-Appropriation Prohibition
 *   domain: international_space_law/treaty_interpretation/commons_governance
 *
 * SUMMARY:
 *   This constraint instantiates the commons_conservation reading of the
 *   Article II non-appropriation kernel. The reading holds that Article II's
 *   prohibition on 'national appropriation by claim of sovereignty, by means
 *   of use or occupation, or by any other means' prohibits de facto
 *   appropriation through resource extraction, binding both states and
 *   private actors. It functions as a wall constraint: extraction is
 *   prohibited absent multilateral authorization, first-mover mining
 *   investments are stranded, non-spacefaring states preserve a veto over
 *   enclosure, and benefits are distributed by negotiation rather than
 *   capability. The constraint is actively contested by spacefaring nations
 *   and commercial operators who advance the extraction_permissive reading.
 *
 * KEY AGENTS:
 *   - non_spacefaring_nations (beneficiary/organized) â preserve veto and multilateral bargaining position
 *   - commons_preservation_movement (beneficiary/moderate) â advances anti-enclosure framing
 *   - first_mover_investors (payer/powerful) â bear stranding risk and trapped capital
 *   - spacefaring_nations (payer/institutional) â bear opportunity cost of constrained unilateral extraction
 *   - multilateral_space_governance_bodies (agenda_setter/institutional) â administer interpretive boundary
 *   - unilateral_extraction_advocates (excluded/powerful) â marginalized in treaty fora
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ost_article_ii_non_appropriation__commons_conservation, 0.7).
domain_priors:suppression_score(ost_article_ii_non_appropriation__commons_conservation, 0.72).
domain_priors:theater_ratio(ost_article_ii_non_appropriation__commons_conservation, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, extractiveness, 0.7).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__commons_conservation, tangled_rope).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__commons_conservation, "Article II Commons Conservation Reading: Extraction-as-Appropriation Prohibition").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__commons_conservation, "international_space_law/treaty_interpretation/commons_governance").

domain_priors:requires_active_enforcement(ost_article_ii_non_appropriation__commons_conservation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__commons_conservation, '58040696-d3e9-4d39-9851-d64a4edccb93').
narrative_ontology:cs_kernel_codification('58040696-d3e9-4d39-9851-d64a4edccb93', formalized).
narrative_ontology:cs_authority_grounding('58040696-d3e9-4d39-9851-d64a4edccb93', lineage).
narrative_ontology:cs_interpretation_layer_present('58040696-d3e9-4d39-9851-d64a4edccb93').
narrative_ontology:cs_reading_relation('58040696-d3e9-4d39-9851-d64a4edccb93', ost_article_ii_non_appropriation__extraction_permissive, forecloses).
narrative_ontology:cs_reading_relation('58040696-d3e9-4d39-9851-d64a4edccb93', ost_article_ii_non_appropriation__international_regime, coexists_with).
narrative_ontology:cs_axiom('58040696-d3e9-4d39-9851-d64a4edccb93', foundational, extraction_as_de_facto_appropriation).
narrative_ontology:cs_axiom_status(extraction_as_de_facto_appropriation, holdable).
narrative_ontology:cs_axiom_grounding('58040696-d3e9-4d39-9851-d64a4edccb93', extraction_as_de_facto_appropriation, conventional).
narrative_ontology:cs_axiom('58040696-d3e9-4d39-9851-d64a4edccb93', foundational, article_ii_binds_private_actors).
narrative_ontology:cs_axiom_status(article_ii_binds_private_actors, holdable).
narrative_ontology:cs_axiom_grounding('58040696-d3e9-4d39-9851-d64a4edccb93', article_ii_binds_private_actors, conventional).
narrative_ontology:cs_reference_frame('58040696-d3e9-4d39-9851-d64a4edccb93', common_heritage_preservation_framework).
narrative_ontology:cs_drift_state('58040696-d3e9-4d39-9851-d64a4edccb93', artemis_accords_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('58040696-d3e9-4d39-9851-d64a4edccb93', '').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__commons_conservation, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__commons_conservation, non_spacefaring_nations).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__commons_conservation, commons_preservation_movement).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__commons_conservation, first_mover_investors).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__commons_conservation, spacefaring_nations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Lack independent space resource extraction capability; benefit from treaty interpretation that prevents de facto enclosure by spacefaring states and preserves a multilateral veto over resource access, ensuring distribution by negotiation rather than first-mover advantage.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, non_spacefaring_nations, beneficiary,
    organized, generational, constrained, global).

% Advocacy networks advancing common heritage framing of outer space; benefit from legal interpretation that blocks unilateral extraction and preserves the anti-enclosure principle.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, commons_preservation_movement, beneficiary,
    moderate, civilizational, mobile, global).

% Commercial entities with capital committed to space mining R&D and infrastructure; face asset stranding if extraction is classified as prohibited appropriation, with capital locked in long-cycle projects lacking legal certainty of return.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, first_mover_investors, payer,
    powerful, biographical, trapped, global).

% States with technological capability to extract space resources; bear opportunity cost of constrained unilateral activity and diplomatic burden of complying with multilateral authorization requirements.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, spacefaring_nations, payer,
    institutional, generational, constrained, global).

% UN COPUOS and related treaty bodies that maintain interpretive boundaries between permissible use and prohibited appropriation; administer compliance discourse and produce legal opinions that reinforce the commons conservation frame.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, multilateral_space_governance_bodies, agenda_setter,
    institutional, generational, analytical, global).

% Commercial lobbies and spacefaring state agencies arguing extraction constitutes use not appropriation; structurally marginalized in treaty fora that have consolidated the conservation reading.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, unilateral_extraction_advocates, excluded,
    powerful, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents de facto territorial and resource enclosure in outer space by interpreting Article II to prohibit resource extraction that constitutes appropriation, preserving the commons for all states regardless of capability.
% TRANSFER_FUNCTION: Transfers the authority to legitimize space resource extraction from unilateral capable actors to multilateral negotiation processes; transfers expected returns from first-mover investors to non-spacefaring states through veto and bargaining rights.
% ABSENT_VOICES: Commercial space mining operators and Artemis-aligned space agencies advocating unilateral extraction rights are largely excluded from authoritative interpretive fora; their legal briefs and national legislation are treated as minority or non-compliant positions in treaty body debates.
% DISAPPEARANCE_RATIONALE: If the prohibition on extraction-as-appropriation vanished, spacefaring nations and commercial operators would proceed with unilateral resource extraction, first-mover investments would be legally validated rather than stranded, and the open commons would begin enclosure through de facto occupation.
% FOUNDING_PROBLEM: Prevention of colonial-style territorial seizure in outer space; ensuring that early spacefaring nations cannot monopolize celestial bodies and exclude late-entering or non-spacefaring states from space resource benefits.
% FOUNDING_PROBLEM_CORROBORATION: Non-spacefaring states and the UN Committee on the Peaceful Uses of Outer Space attest the problem remains live. Spacefaring nations and commercial extraction advocates attest that technological maturity has shifted the problem and unilateral extraction is necessary for practical development; independent legal scholarship is divided, with no outside consensus.
narrative_ontology:disappearance_verdict(ost_article_ii_non_appropriation__commons_conservation, world_rearranges).
narrative_ontology:founding_problem_status(ost_article_ii_non_appropriation__commons_conservation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ost_article_ii_non_appropriation__commons_conservation, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ost_article_ii_non_appropriation__commons_conservation, 'none', 1).
narrative_ontology:epsilon_provenance(ost_article_ii_non_appropriation__commons_conservation, 0.7, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ost_article_ii_non_appropriation__commons_conservation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ost_article_ii_non_appropriation__commons_conservation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ost_article_ii_non_appropriation__commons_conservation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.70) is high because the constraint extracts unilateral extraction rights from capable actors and transfers negotiating leverage to non-capable states. Suppression (0.72) reflects the active diplomatic and legal pressure required to maintain the prohibition against growing technological capability and commercial interest. Theater_ratio (0.45) is moderate-to-high: while the anti-enclosure principle is genuine, an increasing share of interpretive activity performs compliance without corresponding enforcement capacity as the Artemis era advances. Accessibility_collapse (0.58) is moderate â unilateral extraction remains physically possible but legally foreclosed. Resistance (0.68) is substantial, driven by spacefaring nations and commercial lobbies advancing rival readings.
 *
 * PERSPECTIVAL GAP:
 *   The non_spacefaring_nations seat experiences the constraint as protective coordination preserving commons access; the first_mover_investors and spacefaring_nations seats experience the same structure as extraction of their capability advantage and investment returns. The multilateral governance bodies experience it as legitimate treaty administration. The engine computes this divergence from the structural data rather than the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Non-spacefaring nations and the commons movement are structural beneficiaries (d near beneficiary end) because the constraint subsidizes their bargaining position and preserves veto rights. First-mover investors and spacefaring nations are structural targets (d near target end) because the constraint extracts from their capability and capital. The excluded unilateral advocates sit at high directionality as excluded targets.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled rope prevents mislabeling the constraint as pure coordination (rope) or pure extraction (snare). The constraint carries a genuine coordination function â preventing colonial-style enclosure and preserving space for late entrants â but it simultaneously extracts capability-based advantage and strands first-mover capital. Both functions are structurally necessary to the constraint's operation; removing either dissolves the arrangement into a different type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_appropriation_boundary,
    'Can a stable legal boundary be maintained between permissible ''use'' of space resources and prohibited ''appropriation'' via extraction under Article II?',
    'Comparative analysis of state practice, national space legislation, and an ICJ advisory opinion on whether extraction constitutes appropriation.',
    'If no coherent boundary exists, the constraint collapses into either a pure extraction-permissive frame or a total prohibition, eliminating the tangled rope structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_appropriation_boundary, conceptual, 'Legal boundary between use and appropriation via extraction').

omega_variable(
    enforcement_effectiveness_ambiguity,
    'Does the constraint''s persistence depend on active legal and diplomatic enforcement, or primarily on the historical absence of viable extraction technology?',
    'Natural experiment as extraction technology matures: if extraction proceeds despite the legal constraint, enforcement was illusory and the constraint was a latent piton; if legal pressure actively blocks projects, enforcement is real.',
    'If the constraint only persisted due to technological impossibility, its theater_ratio was historically masked and the constraint may reclassify as piton or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_effectiveness_ambiguity, empirical, 'Whether enforcement or technology absence sustains the constraint').

omega_variable(
    private_actor_binding_status,
    'Does Article II bind private actors directly, or only states, with private activity mediated through national authorization?',
    'Analysis of national space legislation implementing Article II and litigation involving commercial space entities.',
    'If Article II binds only states, the constraint on private extraction is mediated and weaker; unilateral extraction could proceed under permissive national licensing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(private_actor_binding_status, conceptual, 'Direct versus mediated binding of private actors').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__commons_conservation, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ost__tr_t0, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ost__tr_t3, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 3, 0.23).
narrative_ontology:measurement(ost__tr_t6, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 6, 0.28).
narrative_ontology:measurement(ost__tr_t9, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 9, 0.35).
narrative_ontology:measurement(ost__tr_t12, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 12, 0.41).
narrative_ontology:measurement(ost__tr_t15, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 15, 0.45).

% Extraction over time
narrative_ontology:measurement(ost__be_t0, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(ost__be_t3, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(ost__be_t6, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(ost__be_t9, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 9, 0.62).
narrative_ontology:measurement(ost__be_t12, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 12, 0.68).
narrative_ontology:measurement(ost__be_t15, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 15, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(ost__su_t0, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(ost__su_t3, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 3, 0.48).
narrative_ontology:measurement(ost__su_t6, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 6, 0.55).
narrative_ontology:measurement(ost__su_t9, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 9, 0.63).
narrative_ontology:measurement(ost__su_t12, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 12, 0.68).
narrative_ontology:measurement(ost__su_t15, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 15, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ost_article_ii_non_appropriation__commons_conservation, resource_allocation).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__commons_conservation, ost_article_ii_non_appropriation__extraction_permissive).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__commons_conservation, ost_article_ii_non_appropriation__international_regime).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested Article II non-appropriation kernel. The extraction_permissive reading has a substantially lower epsilon and different beneficiary/victim structure (commercial miners as beneficiaries). The international_regime reading defers epsilon determination to a future framework. All three are structurally distinct constraints sharing a regulatory domain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
