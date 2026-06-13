% ============================================================================
% CONSTRAINT STORY: magna_carta_constraint_authority__living_constitutionalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_constraint_authority__living_constitutionalism_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: magna_carta_constraint_authority__living_constitutionalism_reading
 *   human_readable: Magna Carta: Inherited Due Process (Living Constitutionalism Reading)
 *   domain: constitutional_history/legal_philosophy/political_theory
 *
 * SUMMARY:
 *   This constraint represents the 'living constitutionalism' reading of
 *   Magna Carta, where its principles of due process and lawful restraint are
 *   understood to bind all subsequent rulers through juridical precedent and
 *   evolutionary interpretation. It is not a static document but a
 *   foundational text whose meaning adapts to contemporary challenges,
 *   limiting executive power and protecting citizens. This reading emphasizes
 *   the charter's enduring relevance beyond its original feudal context.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__living_constitutionalism_reading, 0.25).
domain_priors:suppression_score(magna_carta_constraint_authority__living_constitutionalism_reading, 0.15).
domain_priors:theater_ratio(magna_carta_constraint_authority__living_constitutionalism_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__living_constitutionalism_reading, rope).
narrative_ontology:human_readable(magna_carta_constraint_authority__living_constitutionalism_reading, "Magna Carta: Inherited Due Process (Living Constitutionalism Reading)").
narrative_ontology:topic_domain(magna_carta_constraint_authority__living_constitutionalism_reading, "constitutional_history/legal_philosophy/political_theory").

domain_priors:requires_active_enforcement(magna_carta_constraint_authority__living_constitutionalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__living_constitutionalism_reading, 'f0cd9933-fc96-4ffe-ac98-68e788930b49').
narrative_ontology:cs_kernel_codification('f0cd9933-fc96-4ffe-ac98-68e788930b49', fixed_text).
narrative_ontology:cs_authority_grounding('f0cd9933-fc96-4ffe-ac98-68e788930b49', lineage).
narrative_ontology:cs_interpretation_layer_present('f0cd9933-fc96-4ffe-ac98-68e788930b49').
narrative_ontology:cs_reading_relation('f0cd9933-fc96-4ffe-ac98-68e788930b49', magna_carta_constraint_authority__feudal_obsolescence_reading, forecloses).
narrative_ontology:cs_reading_relation('f0cd9933-fc96-4ffe-ac98-68e788930b49', magna_carta_constraint_authority__parliamentary_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('f0cd9933-fc96-4ffe-ac98-68e788930b49', foundational, inherited_constitutional_restraint).
narrative_ontology:cs_axiom_status(inherited_constitutional_restraint, holdable).
narrative_ontology:cs_axiom_grounding('f0cd9933-fc96-4ffe-ac98-68e788930b49', inherited_constitutional_restraint, deontological).
narrative_ontology:cs_axiom('f0cd9933-fc96-4ffe-ac98-68e788930b49', foundational, evolutionary_interpretation_legitimacy).
narrative_ontology:cs_axiom_status(evolutionary_interpretation_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('f0cd9933-fc96-4ffe-ac98-68e788930b49', evolutionary_interpretation_legitimacy, conventional).
narrative_ontology:cs_reference_frame('f0cd9933-fc96-4ffe-ac98-68e788930b49', foundational_charter_binding_all_rulers).
narrative_ontology:cs_drift_state('f0cd9933-fc96-4ffe-ac98-68e788930b49', contemporary_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f0cd9933-fc96-4ffe-ac98-68e788930b49', '').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__living_constitutionalism_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__living_constitutionalism_reading, citizens).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__living_constitutionalism_reading, judiciary).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__living_constitutionalism_reading, royal_prerogative).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__living_constitutionalism_reading, executive_discretion).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__living_constitutionalism_reading, monarch_or_executive).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the protection of due process and lawful judgment, which limits arbitrary state power. Their ability to exit the system is limited, but the constraint provides a shield against overreach.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, citizens, beneficiary,
    organized, generational, constrained, national).

% Interprets and applies Magna Carta's principles through juridical precedent, ensuring its continued relevance. Their institutional identity is bound to upholding constitutional principles, making exit from this role unthinkable.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, judiciary, agenda_setter,
    institutional, civilizational, identity_locked, national).

% Bears the cost of limited arbitrary power and executive discretion. Must operate within the bounds of law and precedent, which constrains their actions. Exit options are limited to constitutional crises or legislative reform.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, monarch_or_executive, payer,
    institutional, biographical, constrained, national).

% Legislates within the framework established by Magna Carta, often codifying or expanding its principles. While theoretically sovereign, it generally respects the foundational principles, but can also amend or reinterpret them through statute.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, parliament, agenda_setter,
    institutional, generational, mobile, national).

% Analyze the historical evolution and contemporary application of Magna Carta, contributing to its 'living' interpretation. They provide critical commentary and theoretical frameworks for understanding its binding authority.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the exercise of state power by establishing a shared understanding of fundamental rights and limitations, ensuring stability and predictability in governance across generations of rulers.
% TRANSFER_FUNCTION: Transfers the right to arbitrary action from the executive to the collective body of law and precedent, granting due process protections to subjects and establishing a framework for lawful governance.
% ABSENT_VOICES: Those who advocate for absolute monarchical or executive power, or for a purely positivist view of law where only explicit statute binds, are structurally marginalized by this reading. They would argue against the idea of inherited, evolving constitutional restraint.
% DISAPPEARANCE_RATIONALE: If Magna Carta's authority as a living constitutional document vanished, the foundational principles of due process and lawful restraint would erode, leading to a significant rearrangement of state-citizen relations, potentially increasing arbitrary power and decreasing legal predictability.
% FOUNDING_PROBLEM: The problem of arbitrary royal power and the need to establish a legal framework that binds the monarch and protects subjects from unlawful judgment and seizure.
% FOUNDING_PROBLEM_CORROBORATION: Historians, legal scholars, and international human rights organizations corroborate that the problem of arbitrary state power and the need for constitutional restraint remains live, even if its specific manifestations have evolved from the 13th century. The judiciary's ongoing role in interpreting and applying these principles further attests to its contemporary relevance.
narrative_ontology:disappearance_verdict(magna_carta_constraint_authority__living_constitutionalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_constraint_authority__living_constitutionalism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__living_constitutionalism_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(magna_carta_constraint_authority__living_constitutionalism_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_constraint_authority__living_constitutionalism_reading_tests).
:- end_tests(magna_carta_constraint_authority__living_constitutionalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Rope because it primarily functions as a coordination mechanism for lawful governance, benefiting citizens and the judiciary by providing a stable framework for rights. Extractiveness is low-to-moderate (0.25) as it primarily limits arbitrary power rather than extracting resources. Suppression is low (0.15) because its authority is largely accepted through tradition and judicial practice, requiring minimal active coercion. Theater ratio is low (0.1) as its principles are genuinely applied and debated, not merely performed. The temporal measurements reflect a gradual increase in extractiveness as the scope of 'lawful restraint' expanded, and a decrease in suppression as its authority became more institutionalized.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of citizens and the judiciary, this constraint is a vital safeguard and a source of legitimacy. From the perspective of the executive, it is a necessary but sometimes inconvenient limitation on their power. The 'living' aspect means its interpretation is always subject to debate, but the core function of restraint remains.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary and citizens are primary beneficiaries, gaining a framework for justice and protection from arbitrary power. The monarch/executive is the primary payer, as their prerogative is constrained. Parliament acts as an agenda-setter, shaping the interpretation through legislation. Legal scholars observe and influence the interpretive process. The 'victims' are abstract concepts like 'royal_prerogative' and 'executive_discretion', which are curtailed by the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading actively resists mandatrophy by emphasizing evolutionary interpretation, ensuring the constraint remains 'live' and relevant to contemporary problems, rather than becoming an obsolete feudal document. The ongoing judicial and scholarly engagement prevents its function from atrophying, re-legitimizing its mandate over time.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_evolutionary_interpretation,
    'What are the legitimate bounds of ''evolutionary interpretation'' before it becomes judicial activism or legislative overreach?',
    'Ongoing legal and political debate, judicial review of interpretive methods, and public acceptance of new precedents. A clear consensus on interpretive methodology would resolve this.',
    'If interpretation is deemed too expansive, the constraint''s legitimacy could erode, increasing resistance and potentially shifting its classification towards a Snare (if seen as judicial extraction) or Piton (if its authority becomes purely theatrical). If too narrow, it risks becoming obsolete, shifting towards the ''feudal_obsolescence_reading''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_evolutionary_interpretation, conceptual, 'Ambiguity in the scope of ''living'' interpretation.').

omega_variable(
    parliamentary_supremacy_tension,
    'To what extent does this ''living constitutionalism'' reading genuinely bind Parliament, given the doctrine of parliamentary sovereignty?',
    'A definitive constitutional ruling or a codified constitution that explicitly defines the relationship between foundational charters and parliamentary statute. Ongoing legal challenges and political practice will continue to test this boundary.',
    'If parliamentary sovereignty is absolute, this reading''s binding force on Parliament is weak, making it more of a ''Rope'' for the executive but less so for the legislature. If it genuinely limits Parliament, it strengthens the constraint''s overall authority and shifts the ''parliamentary_sovereignty_reading'' towards ''foreclosed'' within this framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(parliamentary_supremacy_tension, conceptual, 'Tension between living constitutionalism and parliamentary sovereignty.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__living_constitutionalism_reading, 1215, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1215, 0.05).
narrative_ontology:measurement(magn_tr_t1688, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1688, 0.08).
narrative_ontology:measurement(magn_tr_t1945, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(magn_tr_t2024, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1215, 0.1).
narrative_ontology:measurement(magn_be_t1688, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1688, 0.2).
narrative_ontology:measurement(magn_be_t1945, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1945, 0.25).
narrative_ontology:measurement(magn_be_t2024, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1215, 0.3).
narrative_ontology:measurement(magn_su_t1688, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1688, 0.2).
narrative_ontology:measurement(magn_su_t1945, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1945, 0.15).
narrative_ontology:measurement(magn_su_t2024, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 2024, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_constraint_authority__living_constitutionalism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__living_constitutionalism_reading, rule_of_law_principle).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__living_constitutionalism_reading, habeas_corpus_right).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'magna_carta_constraint_authority' kernel. This 'living_constitutionalism_reading' emphasizes its evolving, binding nature, distinct from the 'feudal_obsolescence_reading' and the 'parliamentary_sovereignty_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
