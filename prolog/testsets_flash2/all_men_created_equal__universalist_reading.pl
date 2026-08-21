% ============================================================================
% CONSTRAINT STORY: all_men_created_equal__universalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_all_men_created_equal__universalist_reading, []).

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
 *   constraint_id: all_men_created_equal__universalist_reading
 *   human_readable: Universalist Reading of 'All Men Are Created Equal'
 *   domain: constitutional_law/political_philosophy/american_studies
 *
 * SUMMARY:
 *   This constraint represents the 'universalist reading' of the phrase 'All
 *   Men Are Created Equal' from the US Declaration of Independence. This
 *   reading interprets the principle as a dynamic, expansive ideal that
 *   demands iterative application to all human beings, regardless of the
 *   original intent or historical context of its authors. It serves as a
 *   foundational justification for civil rights movements and ongoing efforts
 *   to achieve social justice, constantly pushing against existing boundaries
 *   of inclusion. The constraint's extractiveness reflects the coordination
 *   costs of this expansion, while its beneficiaries are marginalized groups
 *   and civil rights advocates.
 *
 * KEY AGENTS:
 *   - marginalized_groups_claiming_inclusion: Primary beneficiary (organized/constrained) — benefits from expanded rights
 *   - civil_rights_advocates: Agenda setter (institutional/mobile) — actively champions expansion
 *   - institutions_resisting_expansion: Primary payer (institutional/constrained) — bears costs of adaptation
 *   - groups_losing_exclusive_privilege: Payer (powerful/constrained) — bears social/political costs
 *   - originalist_scholars: Excluded (analytical/analytical) — their interpretation is actively challenged
 *   - supreme_court: Agenda setter (institutional/constrained) — arbitrates legal expansion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(all_men_created_equal__universalist_reading, 0.45).
domain_priors:suppression_score(all_men_created_equal__universalist_reading, 0.3).
domain_priors:theater_ratio(all_men_created_equal__universalist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(all_men_created_equal__universalist_reading, rope).
narrative_ontology:human_readable(all_men_created_equal__universalist_reading, "Universalist Reading of 'All Men Are Created Equal'").
narrative_ontology:topic_domain(all_men_created_equal__universalist_reading, "constitutional_law/political_philosophy/american_studies").

domain_priors:requires_active_enforcement(all_men_created_equal__universalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(all_men_created_equal__universalist_reading, '2b9a78e1-a370-45e9-9e96-bc3e64a7b402').
narrative_ontology:cs_kernel_codification('2b9a78e1-a370-45e9-9e96-bc3e64a7b402', fixed_text).
narrative_ontology:cs_authority_grounding('2b9a78e1-a370-45e9-9e96-bc3e64a7b402', lineage).
narrative_ontology:cs_interpretation_layer_present('2b9a78e1-a370-45e9-9e96-bc3e64a7b402').
narrative_ontology:cs_reading_relation('2b9a78e1-a370-45e9-9e96-bc3e64a7b402', all_men_created_equal__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('2b9a78e1-a370-45e9-9e96-bc3e64a7b402', all_men_created_equal__textualist_paradox_reading, influences).
narrative_ontology:cs_axiom('2b9a78e1-a370-45e9-9e96-bc3e64a7b402', foundational, equality_as_dynamic_ideal).
narrative_ontology:cs_axiom_status(equality_as_dynamic_ideal, holdable).
narrative_ontology:cs_axiom_grounding('2b9a78e1-a370-45e9-9e96-bc3e64a7b402', equality_as_dynamic_ideal, deontological).
narrative_ontology:cs_axiom('2b9a78e1-a370-45e9-9e96-bc3e64a7b402', secondary, founder_intent_subordinate_to_principle).
narrative_ontology:cs_axiom_status(founder_intent_subordinate_to_principle, holdable).
narrative_ontology:cs_axiom_grounding('2b9a78e1-a370-45e9-9e96-bc3e64a7b402', founder_intent_subordinate_to_principle, conventional).
narrative_ontology:cs_reference_frame('2b9a78e1-a370-45e9-9e96-bc3e64a7b402', iterative_moral_expansion).
narrative_ontology:cs_drift_state('2b9a78e1-a370-45e9-9e96-bc3e64a7b402', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2b9a78e1-a370-45e9-9e96-bc3e64a7b402', '').
narrative_ontology:cs_kernel_id(all_men_created_equal__universalist_reading, all_men_created_equal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(all_men_created_equal__universalist_reading, marginalized_groups_claiming_inclusion).
narrative_ontology:constraint_beneficiary(all_men_created_equal__universalist_reading, civil_rights_advocates).
narrative_ontology:constraint_victim(all_men_created_equal__universalist_reading, institutions_resisting_expansion).
narrative_ontology:constraint_victim(all_men_created_equal__universalist_reading, groups_losing_exclusive_privilege).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These groups (e.g., women, racial minorities, LGBTQ+ individuals) invoke the principle to demand equal rights and protections, benefiting from its expansive interpretation. Their exit options are constrained by the ongoing struggle for recognition within the existing legal framework.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, marginalized_groups_claiming_inclusion, beneficiary,
    organized, generational, constrained, national).

% Legal scholars, activists, and organizations who actively champion the universalist interpretation, pushing for its application to new contexts and groups. They shape legal discourse and public opinion, benefiting from the principle's legitimizing power.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, civil_rights_advocates, agenda_setter,
    institutional, generational, mobile, national).

% Government bodies, legal systems, or social structures that historically upheld discriminatory practices and now face legal and social pressure to conform to an expanding definition of equality. They bear the costs of legal challenges, policy changes, and social upheaval.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, institutions_resisting_expansion, payer,
    institutional, biographical, constrained, national).

% Groups that historically benefited from a narrower definition of equality (e.g., white male landowners) and now perceive a loss of status or advantage as the principle expands. They bear the social and political costs of adapting to a more inclusive society.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, groups_losing_exclusive_privilege, payer,
    powerful, biographical, constrained, national).

% Academics and legal practitioners who adhere to an originalist interpretation, arguing that the principle's meaning is fixed by the founders' intent. While their arguments are part of the broader debate, this reading actively seeks to move beyond their interpretive bounds.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, originalist_scholars, excluded,
    analytical, generational, analytical, national).

% The ultimate arbiter of constitutional meaning, whose rulings iteratively expand or contract the application of the equality principle. Its decisions are a primary mechanism through which this reading gains or loses force.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, supreme_court, agenda_setter,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a foundational moral and legal principle around which diverse groups can coordinate their demands for justice and inclusion, offering a common language for rights claims and a framework for legal and social reform.
% TRANSFER_FUNCTION: Transfers moral and legal legitimacy, as well as material resources and opportunities, from historically privileged groups and institutions to historically marginalized groups, by expanding the scope of who is considered 'equal' under the law.
% ABSENT_VOICES: Future generations, whose understanding of equality will continue to evolve, are not directly represented but are implicitly invoked by the iterative nature of this reading. Those who believe in inherent, immutable hierarchies would be fundamentally opposed but are largely outside mainstream legal discourse.
% DISAPPEARANCE_RATIONALE: If the universalist reading of 'All Men Are Created Equal' vanished, the legal and moral basis for civil rights, anti-discrimination laws, and ongoing struggles for social justice would be severely undermined. The entire framework for progressive social change in the US would collapse, leading to a profound rearrangement of legal and political structures.
% FOUNDING_PROBLEM: The problem of establishing a legitimate basis for self-governance and individual rights, while simultaneously grappling with the inherent contradiction of slavery and other forms of inequality at the nation's founding.
% FOUNDING_PROBLEM_CORROBORATION: Historians and political theorists widely corroborate the founding problem, noting the tension between the Declaration's ideals and the realities of the time. Civil rights organizations and legal scholars continue to attest that the problem of fully realizing equality remains live, citing ongoing systemic inequalities and discrimination.
narrative_ontology:disappearance_verdict(all_men_created_equal__universalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(all_men_created_equal__universalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(all_men_created_equal__universalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(all_men_created_equal__universalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(all_men_created_equal__universalist_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(all_men_created_equal__universalist_reading_tests).
:- end_tests(all_men_created_equal__universalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate, reflecting the ongoing societal and institutional costs of expanding equality, which includes legal battles, policy changes, and shifts in social norms. Suppression (0.30) is relatively low, as this reading thrives on open debate and advocacy, though it faces resistance from entrenched interests. Theater ratio (0.10) is low, as the principle is actively invoked for genuine social change, not merely performative maintenance. Accessibility collapse (0.60) is moderate, as alternative frameworks for social organization exist but are increasingly challenged by the dominance of this principle. Resistance (0.40) is moderate, reflecting the continuous pushback from those who benefit from a narrower interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of marginalized groups and civil rights advocates, this reading is a powerful Rope, enabling coordination towards a more just society. For institutions and groups resisting expansion, it functions as a Snare, forcing them to relinquish historical privileges and adapt to new norms. The engine's per-seat classification will capture this divergence based on the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Marginalized groups and civil rights advocates are beneficiaries (low d) as the constraint expands their rights and influence. Institutions and groups losing privilege are payers (high d) as they bear the costs of this expansion. The Supreme Court, as an agenda setter, has a more complex directionality, balancing its role in upholding the constitution with the pressures for expansion.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is inherently dynamic, constantly re-evaluating its mandate. Mandatrophy is resolved by its very nature: the 'mandate' is to continually expand, so it cannot outlive its function as long as inequalities persist. The classification as a Rope (claimed) reflects its ongoing coordination function, even as its metrics show moderate extractiveness due to the friction of social change. It avoids mislabeling as pure extraction because the benefits of inclusion are real and widely distributed among its beneficiaries, despite the costs borne by others.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_universalism,
    'What are the ultimate, irreducible boundaries of ''universal'' in this principle, if any? Does it extend beyond human beings, or to all sentient life?',
    'Ongoing philosophical debate and future legal challenges regarding animal rights, AI personhood, or other non-human entities.',
    'If the scope expands beyond human beings, the constraint''s extractiveness and suppression would increase dramatically as new ''victims'' (e.g., animals in industrial agriculture) are identified, and new ''beneficiaries'' (e.g., animal rights advocates) emerge. This would fundamentally alter the constraint''s classification and stakeholder dynamics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scope_of_universalism, conceptual, 'The conceptual limits of the universalist principle''s application.').

omega_variable(
    pace_of_expansion_vs_social_cohesion,
    'At what point does the pace of iterative expansion of equality begin to erode social cohesion or create unsustainable resistance from those whose privileges are challenged?',
    'Empirical sociological studies on social fragmentation, political polarization, and the efficacy of legal reforms in achieving genuine integration versus generating backlash.',
    'If expansion outpaces social capacity for integration, the constraint could shift towards a Tangled Rope or even Snare, as enforcement becomes more coercive and the coordination function is overshadowed by conflict and extraction from resistant groups. This would increase suppression and resistance metrics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pace_of_expansion_vs_social_cohesion, empirical, 'The trade-off between rapid expansion of equality and social stability.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine universalist reading, or is it a strategic re-framing of the originalist kernel to achieve specific political outcomes?',
    'Analysis of the internal consistency of arguments made by proponents of this reading, and their willingness to apply the principle even when it conflicts with their immediate political interests. Comparison with the ''textualist_paradox_reading'' to see if the universalist reading fully resolves the paradox or merely sidesteps it.',
    'If it''s a strategic re-framing, its ''claimed_type'' as a Rope would be a cover story, and its true classification would be closer to a Snare, with higher theater_ratio and extractiveness, as the coordination function would be secondary to political gain. This would also affect the ''founding_problem_corroboration'' by revealing a self-serving narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the universalist reading is an authentic interpretation or a political tool.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(all_men_created_equal__universalist_reading, 1776, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(all__tr_t1776, all_men_created_equal__universalist_reading, theater_ratio, 1776, 0.05).
narrative_ontology:measurement(all__tr_t1865, all_men_created_equal__universalist_reading, theater_ratio, 1865, 0.08).
narrative_ontology:measurement(all__tr_t1920, all_men_created_equal__universalist_reading, theater_ratio, 1920, 0.09).
narrative_ontology:measurement(all__tr_t1965, all_men_created_equal__universalist_reading, theater_ratio, 1965, 0.1).
narrative_ontology:measurement(all__tr_t2000, all_men_created_equal__universalist_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(all__tr_t2024, all_men_created_equal__universalist_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(all__be_t1776, all_men_created_equal__universalist_reading, base_extractiveness, 1776, 0.1).
narrative_ontology:measurement(all__be_t1865, all_men_created_equal__universalist_reading, base_extractiveness, 1865, 0.25).
narrative_ontology:measurement(all__be_t1920, all_men_created_equal__universalist_reading, base_extractiveness, 1920, 0.3).
narrative_ontology:measurement(all__be_t1965, all_men_created_equal__universalist_reading, base_extractiveness, 1965, 0.4).
narrative_ontology:measurement(all__be_t2000, all_men_created_equal__universalist_reading, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement(all__be_t2024, all_men_created_equal__universalist_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(all__su_t1776, all_men_created_equal__universalist_reading, suppression_requirement, 1776, 0.1).
narrative_ontology:measurement(all__su_t1865, all_men_created_equal__universalist_reading, suppression_requirement, 1865, 0.2).
narrative_ontology:measurement(all__su_t1920, all_men_created_equal__universalist_reading, suppression_requirement, 1920, 0.25).
narrative_ontology:measurement(all__su_t1965, all_men_created_equal__universalist_reading, suppression_requirement, 1965, 0.3).
narrative_ontology:measurement(all__su_t2000, all_men_created_equal__universalist_reading, suppression_requirement, 2000, 0.28).
narrative_ontology:measurement(all__su_t2024, all_men_created_equal__universalist_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(all_men_created_equal__universalist_reading, identity_coordination).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, civil_rights_legislation).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, anti_discrimination_laws).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, affirmative_action_policies).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
