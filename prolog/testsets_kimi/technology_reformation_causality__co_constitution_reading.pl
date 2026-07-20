% ============================================================================
% CONSTRAINT STORY: technology_reformation_causality__co_constitution_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_reformation_causality__co_constitution_reading, []).

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
 *   constraint_id: technology_reformation_causality__co_constitution_reading
 *   human_readable: Co-Constitution Reading of Reformation Causality
 *   domain: historiographic/epistemic/religious_history
 *
 * SUMMARY:
 *   The co-constitution reading of Reformation causality treats the printing
 *   press and reformist movements as mutually constitutive: the press
 *   provided the rope of coordination (standardized vernacular textual
 *   circulation), while reformer agency shaped which texts were printed and
 *   how presses were deployed. As an institutionalized historiographic
 *   constraint, this reading occupies the epistemic middle ground, capturing
 *   disciplinary resources by claiming the interaction term. It genuinely
 *   coordinates better explanation than mono-causal predecessors, but
 *   asymmetrically extracts by marginalizing technological determinist and
 *   theological-agency scholars through peer-review orthodoxy, funding
 *   priorities, and hiring gatekeeping. The source material identifies
 *   technology as rope and reformers as piton within this reading; the
 *   constraint itself is the institutionalized framework that binds these
 *   elements together.
 *
 * KEY AGENTS:
 *   - sts_history_programs: agenda_setter (institutional/arbitrage/global) â administers the paradigm and captures resources
 *   - interactionist_historians: beneficiary (organized/mobile/global) â collect prestige and citations
 *   - public_history_institutions: beneficiary (organized/mobile/national) â disseminate the narrative
 *   - technodeterminist_historians: payer (moderate/constrained/national) â marginalized mono-causal scholars
 *   - theological_agency_historians: payer (moderate/constrained/national) â marginalized agency-centered scholars
 *   - junior_reformation_scholars: payer (powerless/identity_locked/national) â trapped early-career researchers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_reformation_causality__co_constitution_reading, 0.55).
domain_priors:suppression_score(technology_reformation_causality__co_constitution_reading, 0.48).
domain_priors:theater_ratio(technology_reformation_causality__co_constitution_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_reformation_causality__co_constitution_reading, tangled_rope).
narrative_ontology:human_readable(technology_reformation_causality__co_constitution_reading, "Co-Constitution Reading of Reformation Causality").
narrative_ontology:topic_domain(technology_reformation_causality__co_constitution_reading, "historiographic/epistemic/religious_history").

domain_priors:requires_active_enforcement(technology_reformation_causality__co_constitution_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_reformation_causality__co_constitution_reading, '11a744ac-b919-43f0-9fdd-4c1a24828a3a').
narrative_ontology:cs_kernel_codification('11a744ac-b919-43f0-9fdd-4c1a24828a3a', distributed).
narrative_ontology:cs_authority_grounding('11a744ac-b919-43f0-9fdd-4c1a24828a3a', expertise).
narrative_ontology:cs_interpretation_layer_present('11a744ac-b919-43f0-9fdd-4c1a24828a3a').
narrative_ontology:cs_reading_relation('11a744ac-b919-43f0-9fdd-4c1a24828a3a', technology_reformation_causality__technological_determinism_reading, coexists_with).
narrative_ontology:cs_reading_relation('11a744ac-b919-43f0-9fdd-4c1a24828a3a', technology_reformation_causality__beneficiary_agency_reading, influences).
narrative_ontology:cs_axiom('11a744ac-b919-43f0-9fdd-4c1a24828a3a', foundational, bidirectional_causality_required).
narrative_ontology:cs_axiom_status(bidirectional_causality_required, holdable).
narrative_ontology:cs_axiom_grounding('11a744ac-b919-43f0-9fdd-4c1a24828a3a', bidirectional_causality_required, instrumental).
narrative_ontology:cs_axiom('11a744ac-b919-43f0-9fdd-4c1a24828a3a', secondary, technological_affordances_constitute_agency).
narrative_ontology:cs_axiom_status(technological_affordances_constitute_agency, holdable).
narrative_ontology:cs_axiom_grounding('11a744ac-b919-43f0-9fdd-4c1a24828a3a', technological_affordances_constitute_agency, empirically_contingent).
narrative_ontology:cs_reference_frame('11a744ac-b919-43f0-9fdd-4c1a24828a3a', interactionist_historiography).
narrative_ontology:cs_drift_state('11a744ac-b919-43f0-9fdd-4c1a24828a3a', mature_sts_orthodoxy, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('11a744ac-b919-43f0-9fdd-4c1a24828a3a', '').
narrative_ontology:cs_kernel_id(technology_reformation_causality__co_constitution_reading, technology_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_reformation_causality__co_constitution_reading, sts_history_programs).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__co_constitution_reading, interactionist_historians).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__co_constitution_reading, public_history_institutions).
narrative_ontology:constraint_victim(technology_reformation_causality__co_constitution_reading, technodeterminist_historians).
narrative_ontology:constraint_victim(technology_reformation_causality__co_constitution_reading, theological_agency_historians).
narrative_ontology:constraint_victim(technology_reformation_causality__co_constitution_reading, junior_reformation_scholars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the co-constitution paradigm through dedicated journals, conference tracks, and interdisciplinary hiring. They capture institutional resources and tuition flows by claiming the interaction between technology and society as their core explanatory territory, enforcing bidirectional causality as the normative standard for funded research in Reformation historiography.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, sts_history_programs, agenda_setter,
    institutional, generational, arbitrage, global).

% Produce research demonstrating mutual shaping of printing technology and reformist movements. They collect citations, tenure, and prestige within the dominant framework. Their work is preferentially funded and published because it adopts the co-constitution idiom, giving them disciplinary mobility and security.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, interactionist_historians, beneficiary,
    organized, biographical, mobile, global).

% Museums, archives, and educational media that narrate the Reformation through co-evolution frameworks receive grants and public engagement metrics. They coordinate public understanding toward the interactionist story, benefiting from the paradigm's status as sophisticated orthodoxy.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, public_history_institutions, beneficiary,
    organized, biographical, mobile, national).

% Argue that printing press technology was the primary or sufficient driver of the Reformation. Their work is routinely rejected from top history of technology journals as reductionist, and their grant applications are disadvantaged when review panels prioritize bidirectional framing. They bear the epistemic cost of marginalization.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, technodeterminist_historians, payer,
    moderate, biographical, constrained, national).

% Emphasize the internal theological logic of reformers and ecclesiastical politics, treating technology as incidental background. Viewed by the co-constitution framework as neglecting material factors, they are pushed to the margins of interdisciplinary funding streams and flagship publications.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, theological_agency_historians, payer,
    moderate, biographical, constrained, national).

% Graduate students and early-career researchers who must adopt co-constitution framing to secure advisor support, fellowships, and tenure-track jobs. Their professional identity is fused with the paradigm; dissenting from bidirectional causality is treated as methodological naivety, closing off alternative career paths.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, junior_reformation_scholars, payer,
    powerless, biographical, identity_locked, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(technology_reformation_causality__co_constitution_reading, sts_history_programs).
narrative_ontology:fixing_cost_class(technology_reformation_causality__co_constitution_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Integrates technological and social explanations of the Reformation into a single interdisciplinary framework that neither reduces technology to passive tool-use nor reduces human actors to technological effects, enabling shared historiographical standards across media studies and religious history.
% TRANSFER_FUNCTION: Moves epistemic authority, publication prestige, and institutional funding from mono-causal historiographical traditions to interdisciplinary STS and media-history programs that claim the interaction term as their exclusive disciplinary competence.
% ABSENT_VOICES: Technological determinist historians and theological agency scholars are formally present in the discipline but epistemically excluded from the dominant conversation; their objections are pre-emptively framed as theoretically unsophisticated, and they rarely sit on the editorial boards or review panels that enforce the paradigm.
% DISAPPEARANCE_RATIONALE: If the co-constitution reading vanished overnight, history curricula would shift toward mono-causal technological or theological narratives, interdisciplinary STS funding streams would contract, journal editorial boards would rebalance, and the academic job market would reorganize around separated technology and religious history departments.
% FOUNDING_PROBLEM: Mid-twentieth-century Reformation historiography was split between crude technological determinism that treated the press as an independent cause and idealist theological history that treated reformers as disembodied agents; neither framework could adequately account for the evident mutual shaping of material technology and social movements.
% FOUNDING_PROBLEM_CORROBORATION: Traditional political historians and church historians outside the STS beneficiary set acknowledge that crude mono-causal accounts were insufficient, corroborating the existence of the founding problem while disputing whether the co-constitution framework is the only or best resolution.
narrative_ontology:disappearance_verdict(technology_reformation_causality__co_constitution_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_reformation_causality__co_constitution_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_reformation_causality__co_constitution_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(technology_reformation_causality__co_constitution_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_reformation_causality__co_constitution_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_reformation_causality__co_constitution_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(technology_reformation_causality__co_constitution_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(technology_reformation_causality__co_constitution_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.55) because the framework captures real resources and epistemic authority while also producing genuine explanatory value that outperforms determinist or idealist predecessors. Suppression is moderate (0.48) because enforcement is epistemic and structural rather than physicalâfunding gatekeeping, tenure decisions, and peer review rejections. Theater ratio 0.40 reflects the increasing ritualization of 'co-evolution' and 'mutual shaping' citations that sometimes substitute for original argument as the paradigm ages. Accessibility collapse 0.60 indicates that mono-causal alternatives are widely treated as intellectually naive once the framework is understood. Resistance 0.35 is limited because marginalized scholars are fragmented across unrelated departments and lack collective power. The temporal series show institutionalization: extraction, theater, and enforcement all rise together as the co-constitution framework matures from a novel intervention (T=0) to orthodoxy (T=50).
 *
 * PERSPECTIVAL GAP:
 *   STS programs and interactionist historians experience the constraint as necessary interdisciplinary coordination that repaired a broken field; technodeterminist and theological-agency historians experience it as an enforced epistemic snare that withholds legitimacy and resources; junior scholars experience it as identity-locked because their professional survival depends on internalizing the bidirectional idiom. The engine computes this divergence from the structural dataâthe authored claim of tangled_rope does not adjudicate between these seats.
 *
 * DIRECTIONALITY LOGIC:
 *   STS history programs are the structural beneficiary and agenda setter (d near 0.0) because they collect the extraction and set the rules. Interactionist historians and public history institutions are beneficiaries (d near 0.15â0.25) because the framework subsidizes their work. Technodeterminist and theological-agency historians are targets (d near 0.85) because the constraint extracts epistemic standing and funding access from them. Junior reformation scholars are trapped near full target (d near 0.95) because their identity-locked exit options amplify the effective extraction of their labor and intellectual compliance.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint was built to solve a real coordination failureâthe mid-century split between determinism and idealismâand the founding_problem_status is contested, indicating the problem is still live. This prevents mislabeling the framework as a pure snare. However, the rising theater_ratio and base_extractiveness over the interval show that extraction has accumulated onto the coordination function, and the suppression_requirement trajectory shows enforcement hardening as the paradigm ages. It is therefore a tangled_rope rather than a scaffold (no sunset) or a piton (the coordination function has not fully atrophied, though reformer agency within the reading is treated as inertial).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    explanatory_value_vs_extraction,
    'Does the co-constitution framework''s dominance reflect superior explanatory power over mono-causal alternatives, or has it captured institutional resources through hiring and funding gatekeeping regardless of marginal explanatory returns?',
    'Comparative historiometric analysis: measure the predictive or integrative yield of co-constitution scholarship against high-quality mono-causal work on the same archives, assessed by independent historians outside both camps.',
    'If explanatory value is high and independent of institutional position, the extraction metric should be revised downward toward rope; if institutional capture drives dominance independently of yield, the classification leans toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(explanatory_value_vs_extraction, empirical, 'Whether dominance tracks explanatory power or institutional capture.').

omega_variable(
    reformer_piton_ambiguity,
    'Within the co-constitution reading, does the ''reformer as piton'' claim refer to the historical Protestant churches becoming inertial institutions, or to the atrophy of agency-centered historiography under co-constitution orthodoxy?',
    'Disambiguate by mapping which stakeholder seat bears the inertial cost: if Protestant institutions, look at organizational behavior post-Reformation; if agency historiography, look at citation rates and hiring data for theological-agency scholars.',
    'If the piton is historiographic, the constraint''s theater ratio is partly driven by scholarly inertia; if the piton is historical, the reading imports an empirical claim about the Reformation that may not be warranted by the historiographic constraint itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reformer_piton_ambiguity, conceptual, 'Ambiguity about whether reformer piton status is historical or historiographic.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative causal readings structural (funding scarcity, editorial rejection, hiring exclusion) or internalized (graduate students believe mono-causal accounts are inherently unsophisticated before encountering evidence)?',
    'Post-exit trajectory study: track scholars who leave the co-constitution framework for determinist or agency positions; if suppression persists in their new contexts, internalization is substantial.',
    'If internalized, the constraint''s effective suppression exceeds the structural measureâtargets carry the constraint with them after exit, raising extraction for identity-locked agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression of alternative readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_reformation_causality__co_constitution_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t0, technology_reformation_causality__co_constitution_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(tech_tr_t10, technology_reformation_causality__co_constitution_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(tech_tr_t20, technology_reformation_causality__co_constitution_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(tech_tr_t30, technology_reformation_causality__co_constitution_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement(tech_tr_t40, technology_reformation_causality__co_constitution_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement(tech_tr_t50, technology_reformation_causality__co_constitution_reading, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(tech_be_t0, technology_reformation_causality__co_constitution_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(tech_be_t10, technology_reformation_causality__co_constitution_reading, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(tech_be_t20, technology_reformation_causality__co_constitution_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(tech_be_t30, technology_reformation_causality__co_constitution_reading, base_extractiveness, 30, 0.48).
narrative_ontology:measurement(tech_be_t40, technology_reformation_causality__co_constitution_reading, base_extractiveness, 40, 0.52).
narrative_ontology:measurement(tech_be_t50, technology_reformation_causality__co_constitution_reading, base_extractiveness, 50, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t0, technology_reformation_causality__co_constitution_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(tech_su_t10, technology_reformation_causality__co_constitution_reading, suppression_requirement, 10, 0.28).
narrative_ontology:measurement(tech_su_t20, technology_reformation_causality__co_constitution_reading, suppression_requirement, 20, 0.35).
narrative_ontology:measurement(tech_su_t30, technology_reformation_causality__co_constitution_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement(tech_su_t40, technology_reformation_causality__co_constitution_reading, suppression_requirement, 40, 0.45).
narrative_ontology:measurement(tech_su_t50, technology_reformation_causality__co_constitution_reading, suppression_requirement, 50, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_reformation_causality__co_constitution_reading, identity_coordination).
narrative_ontology:affects_constraint(technology_reformation_causality__co_constitution_reading, technology_reformation_causality__technological_determinism_reading).
narrative_ontology:affects_constraint(technology_reformation_causality__co_constitution_reading, technology_reformation_causality__beneficiary_agency_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the technology_reformation_causality kernel family, decomposed per the Îµ-invariance principle because the sibling readings instantiate structurally distinct claims with different Îµ profiles. The co-constitution reading extracts from the interaction term; determinism and beneficiary-agency readings extract from unilateral causality claims. Family members are linked to enable contamination propagation analysis across the kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
