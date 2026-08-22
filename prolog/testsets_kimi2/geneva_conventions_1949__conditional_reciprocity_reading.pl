% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__conditional_reciprocity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_1949__conditional_reciprocity_reading, []).

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
 *   constraint_id: geneva_conventions_1949__conditional_reciprocity_reading
 *   human_readable: Geneva Conventions 1949 â Conditional Reciprocity Reading
 *   domain: legal/international_relations
 *
 * SUMMARY:
 *   The Geneva Conventions of 1949 constitute a contested kernel in
 *   international humanitarian law. This constraint story instantiates the
 *   conditional reciprocity reading: the conventions operate as reciprocal
 *   restraints that apply fully only when adversaries comply with criteria
 *   such as Article 4's organized command, distinctive insignia, and open
 *   carriage of arms. Under this reading, states receive the coordination
 *   benefit of reciprocal protections for their regular armed forces, while
 *   irregular combatants who fail the criteria are classified as unlawful
 *   combatants without full POW protections, and civilian immunity is
 *   preserved but narrowed by proportionality calculations. The reading
 *   competes with the humanitarian ceiling reading (absolute minimums
 *   regardless of compliance) and the security maximization reading
 *   (conventions yield to operational necessity).
 *
 * KEY AGENTS:
 *   - state_parties: Agenda-setters who draft, ratify, and enforce the conventions; classify detainees under Article 4; control reciprocity determinations
 *   - regular_combatants: Beneficiaries who receive full POW protections when captured, contingent on state compliance
 *   - irregular_combatants: Payers who fail Article 4 criteria and bear degraded legal protections, detention without POW status, and prosecution for mere participation
 *   - civilians_in_conflict_zones: Payers whose immunity from attack is narrowed by proportionality calculations in counterinsurgency operations
 *   - international_judicial_bodies: Analytical observers who interpret convention obligations in advisory opinions and war crimes tribunals
 *   - humanitarian_organizations: Excluded voices advocating unconditional protections, structurally absent from military legal classification decisions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__conditional_reciprocity_reading, 0.65).
domain_priors:suppression_score(geneva_conventions_1949__conditional_reciprocity_reading, 0.6).
domain_priors:theater_ratio(geneva_conventions_1949__conditional_reciprocity_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__conditional_reciprocity_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_1949__conditional_reciprocity_reading, "Geneva Conventions 1949 â Conditional Reciprocity Reading").
narrative_ontology:topic_domain(geneva_conventions_1949__conditional_reciprocity_reading, "legal/international_relations").

domain_priors:requires_active_enforcement(geneva_conventions_1949__conditional_reciprocity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__conditional_reciprocity_reading, 'f5dc9d00-7e2e-4609-ab94-4a0d5985225f').
narrative_ontology:cs_kernel_codification('f5dc9d00-7e2e-4609-ab94-4a0d5985225f', formalized).
narrative_ontology:cs_authority_grounding('f5dc9d00-7e2e-4609-ab94-4a0d5985225f', lineage).
narrative_ontology:cs_interpretation_layer_present('f5dc9d00-7e2e-4609-ab94-4a0d5985225f').
narrative_ontology:cs_reading_relation('f5dc9d00-7e2e-4609-ab94-4a0d5985225f', geneva_conventions_1949__humanitarian_ceiling_reading, coexists_with).
narrative_ontology:cs_reading_relation('f5dc9d00-7e2e-4609-ab94-4a0d5985225f', geneva_conventions_1949__security_maximization_reading, coexists_with).
narrative_ontology:cs_axiom('f5dc9d00-7e2e-4609-ab94-4a0d5985225f', foundational, reciprocal_compliance_gateway).
narrative_ontology:cs_axiom_status(reciprocal_compliance_gateway, holdable).
narrative_ontology:cs_axiom_grounding('f5dc9d00-7e2e-4609-ab94-4a0d5985225f', reciprocal_compliance_gateway, conventional).
narrative_ontology:cs_axiom('f5dc9d00-7e2e-4609-ab94-4a0d5985225f', foundational, article_four_status_determinative).
narrative_ontology:cs_axiom_status(article_four_status_determinative, holdable).
narrative_ontology:cs_axiom_grounding('f5dc9d00-7e2e-4609-ab94-4a0d5985225f', article_four_status_determinative, conventional).
narrative_ontology:cs_reference_frame('f5dc9d00-7e2e-4609-ab94-4a0d5985225f', interstate_reciprocal_restraint_framework).
narrative_ontology:cs_drift_state('f5dc9d00-7e2e-4609-ab94-4a0d5985225f', asymmetric_conflict_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f5dc9d00-7e2e-4609-ab94-4a0d5985225f', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__conditional_reciprocity_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__conditional_reciprocity_reading, state_parties).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__conditional_reciprocity_reading, regular_combatants).
narrative_ontology:constraint_victim(geneva_conventions_1949__conditional_reciprocity_reading, irregular_combatants).
narrative_ontology:constraint_victim(geneva_conventions_1949__conditional_reciprocity_reading, civilians_in_conflict_zones).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft, ratify, and enforce the Geneva Conventions; classify detainees under Article 4 criteria; determine when reciprocity conditions are met; benefit from operational latitude in asymmetric conflicts and reciprocal protections for their regular forces.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, state_parties, agenda_setter,
    institutional, generational, constrained, global).

% State soldiers who meet Article 4 criteria. They receive full POW protections upon capture, including humane treatment, due process, and release upon cessation of hostilities. Their protection depends on their state's continued compliance with convention obligations.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, regular_combatants, beneficiary,
    moderate, biographical, trapped, global).

% Non-state fighters who lack organized command, distinctive insignia, or open carriage of arms. They are classified as unlawful combatants under this reading, denied full POW protections, and may be prosecuted for mere participation in hostilities. Their legal status degrades proportionally to their non-compliance with Article 4.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, irregular_combatants, payer,
    powerless, immediate, trapped, global).

% Civilians residing in areas of asymmetric conflict whose immunity from direct attack is preserved in principle but narrowed by proportionality calculations when states target irregular forces among them. Collateral damage frameworks permit substantial incidental harm if the anticipated military advantage is deemed sufficient.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, civilians_in_conflict_zones, payer,
    powerless, immediate, trapped, regional).

% Courts and tribunals that interpret and apply the conventions in advisory opinions and war crimes prosecutions. They operate within the treaty framework and sometimes push expansive interpretations, but lack enforcement capacity against non-compliant states.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, international_judicial_bodies, observer,
    institutional, generational, analytical, global).

% Advocate for unconditional humanitarian protections and broader interpretations of convention obligations. They are structurally excluded from military legal classification decisions and detention review processes, and their objections are treated as external advocacy rather than binding interpretation.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, humanitarian_organizations, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_1949__conditional_reciprocity_reading, state_parties).
narrative_ontology:fixing_cost_class(geneva_conventions_1949__conditional_reciprocity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: States reciprocally restrain violence against each other's regular armed forces in exchange for mutual protections; establishes predictable rules for interstate warfare that reduce uncertainty about treatment of captured soldiers and create incentives for compliance.
% TRANSFER_FUNCTION: Moves full legal protections and procedural guarantees away from irregular combatants and proximate civilians toward state parties and their regular armed forces, contingent on Article 4 compliance and proportionality calculations.
% ABSENT_VOICES: Irregular armed groups are excluded from treaty drafting and interpretation; humanitarian organizations advocating unconditional protections are structurally absent from military classification decisions and detention review processes.
% DISAPPEARANCE_RATIONALE: If the conditional reciprocity framework vanished, states would lose the Article 4 classification mechanism and the operational latitude it provides in asymmetric conflicts; irregular detainees would gain equal POW protections; proportionality calculations would lose their current legal anchor; the architecture of international armed conflict law would shift toward unconditional or security-maximization poles.
% FOUNDING_PROBLEM: Interstate warfare in the early twentieth century produced unchecked brutality against captured regular soldiers and no legal mechanism to incentivize reciprocal restraint between sovereign states.
% FOUNDING_PROBLEM_CORROBORATION: State delegations to the 1949 Diplomatic Conference attest to the interstate reciprocity logic. ICRC, human rights treaty bodies, and legal scholars contest that this logic should govern contemporary asymmetric and non-international conflicts, arguing the framework has been misapplied beyond its founding purpose; these sources sit outside the state-party beneficiary set.
narrative_ontology:disappearance_verdict(geneva_conventions_1949__conditional_reciprocity_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_1949__conditional_reciprocity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__conditional_reciprocity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(geneva_conventions_1949__conditional_reciprocity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_1949__conditional_reciprocity_reading, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_1949__conditional_reciprocity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_1949__conditional_reciprocity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_1949__conditional_reciprocity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is substantial because the framework systematically withholds full legal protections from irregular combatants and narrows civilian immunity through proportionality, transferring operational latitude to states. Suppression (0.60) reflects the active exclusion of alternative legal classifications (such as equal POW status for all combatants) through the treaty Article 4 gateway and state classification practice. Theater ratio (0.38) is moderate: the reciprocal restraint function is genuine for interstate warfare, but an increasing share of legal activity in the asymmetric era performs the classification of irregulars rather than genuine reciprocal coordination. Accessibility collapse (0.60) is moderate-high because once the treaty framework is accepted, alternative protective frameworks collapse within the international legal order, though humanitarian counter-readings persist. Resistance (0.50) captures sustained opposition from humanitarian organizations and some judicial bodies to the conditional application of protections.
 *
 * PERSPECTIVAL GAP:
 *   The state-party seat experiences the constraint as a coordination mechanism that secures protections for its soldiers through mutual restraint. The irregular-combatant seat experiences the identical legal framework as an extraction mechanism that legitimizes their exclusion from protections based on criteria they cannot meet without abandoning asymmetric tactics. The civilian seat experiences the narrowing of immunity as a diffuse cost of the state's operational latitude. These divergences are structural: they follow from the same legal text but from different positions relative to the Article 4 classification gateway.
 *
 * DIRECTIONALITY LOGIC:
 *   State parties and regular combatants are structural beneficiaries: the former gain operational flexibility and reciprocal assurance, the latter gain POW protections. Their directionality sits near the beneficiary end. Irregular combatants and conflict-zone civilians are structural targets: they bear the costs of degraded protections and narrowed immunity. Their directionality sits near the target end, amplified by trapped exit options (irregulars cannot simply become regular forces without abandoning their organizational form; civilians cannot exit conflict zones). Humanitarian organizations are excluded from the classification decisions that operationalize the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled rope prevents the error of treating conditional reciprocity as pure coordination (rope) â the asymmetry toward irregulars is too severe for that â while also preventing the error of treating it as pure extraction (snare) â the reciprocal restraint function for regular state forces is genuine and historically motivated. The temporal measurements show extraction accumulating as the framework is applied to asymmetric conflicts for which it was not originally designed, indicating drift without mandatrophy resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_dominance,
    'Is the conditional reciprocity reading the dominant interpretation in contemporary state practice, or has customary international law shifted toward the humanitarian ceiling reading?',
    'Comparative analysis of state military manuals, detention policies, and international judicial opinions to determine which reading commands greater compliance pull.',
    'If humanitarian ceiling has become custom, the effective extraction of this reading is lower than the treaty text suggests; if conditional reciprocity dominates state practice, the authored Îµ is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_dominance, empirical, 'Whether conditional reciprocity or unconditional minimums dominate state practice').

omega_variable(
    asymmetric_warfare_fit,
    'Does the conditional reciprocity framework structurally fit asymmetric non-international armed conflict, or is its application to such conflicts a category error that produces extraction without coordination?',
    'Empirical assessment of whether Article 4 criteria track any meaningful behavioral distinction in asymmetric conflict, or whether they simply exclude all non-state actors by design.',
    'If a category error, the coordination story collapses for the dominant conflict type and the constraint approaches snare; if structurally fitting, the tangled rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asymmetric_warfare_fit, conceptual, 'Framework fit for asymmetric versus interstate warfare').

omega_variable(
    reciprocity_behavioral_effect,
    'Does the conditional withdrawal of protections actually incentivize irregular forces to comply with Article 4 criteria, or does it merely legitimate reduced protections without altering non-state actor behavior?',
    'Conflict-data analysis comparing irregular-group compliance rates across conflicts where states apply conditional versus unconditional protective frameworks.',
    'If no behavioral effect, the coordination justification for conditional reciprocity fails and the extraction component dominates; if compliance increases, the coordination function is vindicated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reciprocity_behavioral_effect, empirical, 'Whether reciprocity conditions produce compliance incentives or merely legitimize degradation').

omega_variable(
    authority_distributed_vs_lineage,
    'Does the authority of this reading rest on the 1949 treaty text and diplomatic lineage, or on subsequent state practice that has substantially modified the original interstate scope?',
    'Treaty-intent historiography paired with state-practice mapping to identify whether contemporary application to asymmetric conflicts is an interpretation or an amendment.',
    'If authority is lineage-based, the drift to asymmetric application is unauthorized expansion; if practice-based, the drift is legitimate evolution. This changes whether the constraint is a commitment system or a distributed coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_distributed_vs_lineage, conceptual, 'Whether authority derives from founding text or evolved practice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__conditional_reciprocity_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gccr_tr_t0, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gccr_tr_t15, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 15, 0.12).
narrative_ontology:measurement(gccr_tr_t30, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(gccr_tr_t45, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 45, 0.25).
narrative_ontology:measurement(gccr_tr_t60, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 60, 0.32).
narrative_ontology:measurement(gccr_tr_t75, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 75, 0.38).

% Extraction over time
narrative_ontology:measurement(gccr_be_t0, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(gccr_be_t15, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 15, 0.25).
narrative_ontology:measurement(gccr_be_t30, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 30, 0.35).
narrative_ontology:measurement(gccr_be_t45, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 45, 0.48).
narrative_ontology:measurement(gccr_be_t60, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 60, 0.58).
narrative_ontology:measurement(gccr_be_t75, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 75, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(gccr_su_t0, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(gccr_su_t15, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 15, 0.42).
narrative_ontology:measurement(gccr_su_t30, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 30, 0.48).
narrative_ontology:measurement(gccr_su_t45, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 45, 0.58).
narrative_ontology:measurement(gccr_su_t60, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 60, 0.68).
narrative_ontology:measurement(gccr_su_t75, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 75, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__conditional_reciprocity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_1949__conditional_reciprocity_reading, humanitarian_ceiling_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__conditional_reciprocity_reading, security_maximization_reading).

% DUAL FORMULATION NOTE:
% The Geneva Conventions 1949 kernel decomposes into three structurally distinct readings: conditional_reciprocity_reading (this file), humanitarian_ceiling_reading, and security_maximization_reading. Each reading has a different Îµ, beneficiary/victim structure, and classification. They compete for authority within the same treaty text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
