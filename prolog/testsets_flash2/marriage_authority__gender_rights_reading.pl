% ============================================================================
% CONSTRAINT STORY: marriage_authority__gender_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__gender_rights_reading, []).

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
 *   constraint_id: marriage_authority__gender_rights_reading
 *   human_readable: Marriage Authority: Gender Rights Reading
 *   domain: legal_pluralism/constitutional_law/comparative_family_law
 *
 * SUMMARY:
 *   This constraint represents the 'gender rights' reading of marriage
 *   authority, where constitutional courts actively intervene to ensure
 *   gender equality within diverse personal law systems. It is a snare
 *   because it targets specific patriarchal practices (e.g., triple talaq,
 *   unequal maintenance) that extract from women, and its persistence relies
 *   on active judicial enforcement against traditional religious authorities.
 *   The reading aims to dismantle existing extractive structures within
 *   personal law, but in doing so, it creates a new, albeit less extractive,
 *   constraint by imposing a constitutional floor. The high extractiveness
 *   reflects the severity of the patriarchal practices being challenged,
 *   while suppression reflects the resistance from traditional authorities
 *   and the need for judicial coercion.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__gender_rights_reading, 0.85).
domain_priors:suppression_score(marriage_authority__gender_rights_reading, 0.78).
domain_priors:theater_ratio(marriage_authority__gender_rights_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__gender_rights_reading, snare).
narrative_ontology:human_readable(marriage_authority__gender_rights_reading, "Marriage Authority: Gender Rights Reading").
narrative_ontology:topic_domain(marriage_authority__gender_rights_reading, "legal_pluralism/constitutional_law/comparative_family_law").

domain_priors:requires_active_enforcement(marriage_authority__gender_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__gender_rights_reading, 'b7fd5d6c-a336-49a0-89bb-25f7aab823e1').
narrative_ontology:cs_kernel_codification('b7fd5d6c-a336-49a0-89bb-25f7aab823e1', formalized).
narrative_ontology:cs_authority_grounding('b7fd5d6c-a336-49a0-89bb-25f7aab823e1', lineage).
narrative_ontology:cs_interpretation_layer_present('b7fd5d6c-a336-49a0-89bb-25f7aab823e1').
narrative_ontology:cs_reading_relation('b7fd5d6c-a336-49a0-89bb-25f7aab823e1', marriage_authority__communal_autonomy_reading, influences).
narrative_ontology:cs_reading_relation('b7fd5d6c-a336-49a0-89bb-25f7aab823e1', marriage_authority__secularist_reading, coexists_with).
narrative_ontology:cs_reading_relation('b7fd5d6c-a336-49a0-89bb-25f7aab823e1', marriage_authority__federalist_millet_reading, influences).
narrative_ontology:cs_reading_relation('b7fd5d6c-a336-49a0-89bb-25f7aab823e1', marriage_authority__judicial_harmonization_reading, coexists_with).
narrative_ontology:cs_axiom('b7fd5d6c-a336-49a0-89bb-25f7aab823e1', foundational, constitutional_equality_supremacy).
narrative_ontology:cs_axiom_status(constitutional_equality_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('b7fd5d6c-a336-49a0-89bb-25f7aab823e1', constitutional_equality_supremacy, deontological).
narrative_ontology:cs_axiom('b7fd5d6c-a336-49a0-89bb-25f7aab823e1', secondary, judicial_review_of_personal_law).
narrative_ontology:cs_axiom_status(judicial_review_of_personal_law, holdable).
narrative_ontology:cs_axiom_grounding('b7fd5d6c-a336-49a0-89bb-25f7aab823e1', judicial_review_of_personal_law, conventional).
narrative_ontology:cs_reference_frame('b7fd5d6c-a336-49a0-89bb-25f7aab823e1', constitutional_equality_framework).
narrative_ontology:cs_drift_state('b7fd5d6c-a336-49a0-89bb-25f7aab823e1', contemporary, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('b7fd5d6c-a336-49a0-89bb-25f7aab823e1', '').
narrative_ontology:cs_kernel_id(marriage_authority__gender_rights_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__gender_rights_reading, women_rights_advocates).
narrative_ontology:constraint_beneficiary(marriage_authority__gender_rights_reading, constitutional_courts).
narrative_ontology:constraint_victim(marriage_authority__gender_rights_reading, women_within_patriarchal_personal_law).
narrative_ontology:constraint_victim(marriage_authority__gender_rights_reading, traditional_religious_authorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively litigate for judicial intervention to expand constitutional equality guarantees into personal law, seeing judicial rulings as a primary mechanism for reform. They benefit from favorable rulings that advance gender equality within marriage.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, women_rights_advocates, beneficiary,
    organized, generational, constrained, national).

% Interpret constitutional equality provisions to review and, at times, strike down discriminatory practices within personal laws (e.g., triple talaq, unequal maintenance). They are the primary enforcers of this reading, expanding their own authority in the process.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, constitutional_courts, agenda_setter,
    institutional, civilizational, analytical, national).

% Are the direct targets of discriminatory personal law practices, bearing the costs of unequal divorce, maintenance, and property rights. While some benefit from judicial reforms, many remain trapped by social norms and lack of awareness or access to legal remedies.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, women_within_patriarchal_personal_law, payer,
    powerless, biographical, identity_locked, local).

% Administer personal laws based on religious texts and traditions. They resist judicial intervention, viewing it as an infringement on communal autonomy and religious freedom, and bear the cost of losing interpretive authority and control over their communities' family matters.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, traditional_religious_authorities, payer,
    organized, generational, constrained, national).

% Advocate for the preservation of community-specific personal laws and resist state or judicial interference, viewing it as an attack on their cultural and religious identity. They are excluded from the judicial process of interpreting constitutional equality, which they see as an external imposition.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, communal_leaders, excluded,
    powerful, generational, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Seeks to coordinate diverse personal laws under a common constitutional framework, ensuring that all citizens, regardless of religious affiliation, are afforded equal rights within marriage and family life.
% TRANSFER_FUNCTION: Transfers interpretive authority over personal law from traditional religious bodies to constitutional courts, and transfers rights and protections to women previously disadvantaged by patriarchal interpretations of personal law.
% ABSENT_VOICES: Many women within patriarchal personal law systems, particularly those in rural or marginalized communities, lack the agency or access to legal systems to voice their demands for equality, remaining subject to traditional norms. Communal leaders who prioritize religious autonomy over individual rights are also excluded from the judicial framing of the debate.
% DISAPPEARANCE_RATIONALE: If this reading (judicial expansion of constitutional equality) vanished, the legal landscape would revert to a more fragmented state, with personal laws operating with less constitutional oversight. Women's rights gains would be rolled back, and the power of traditional religious authorities would be reasserted, leading to significant societal rearrangement.
% FOUNDING_PROBLEM: The problem of gender inequality and discrimination within various personal law systems, where religious or customary laws often superseded constitutional guarantees of equality, leading to unequal treatment of women in marriage, divorce, and inheritance.
% FOUNDING_PROBLEM_CORROBORATION: Women's rights organizations, international human rights bodies, and independent legal scholars consistently corroborate that gender inequality within personal laws remains a live and pressing problem, despite judicial interventions. Traditional religious authorities, however, contest this, arguing that their laws are divinely ordained and not subject to secular equality norms.
narrative_ontology:disappearance_verdict(marriage_authority__gender_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__gender_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__gender_rights_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(marriage_authority__gender_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__gender_rights_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__gender_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority__gender_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority__gender_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high (0.85) because the traditional personal laws, which this reading seeks to reform, impose severe costs on women. Suppression (0.78) is also high, reflecting the significant resistance from traditional religious authorities and the need for robust judicial enforcement to overcome it. The theater ratio is low (0.15) because the judicial interventions are genuinely aimed at reform, not merely performative. Accessibility collapse is moderate (0.45) as legal avenues for reform exist but are often difficult for individual women to access, and resistance is high (0.7) due to strong opposition from traditional groups.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of women's rights advocates and constitutional courts, this constraint is a necessary intervention to correct historical injustices and ensure fundamental rights. From the perspective of traditional religious authorities and communal leaders, it is an external imposition that undermines religious freedom and communal autonomy. The engine's classification will reflect the high extractiveness from the perspective of those subjected to patriarchal personal laws, even as it acknowledges the coordination function of the courts in establishing a constitutional floor.
 *
 * DIRECTIONALITY LOGIC:
 *   Constitutional courts and women's rights advocates are beneficiaries, as they gain authority and achieve their goals of gender equality. Women within patriarchal personal law are victims, as they bear the costs of the discriminatory practices this reading challenges. Traditional religious authorities are also victims, as their authority and control over personal law are curtailed by judicial interventions. The directionality for women within patriarchal personal law is particularly high due to their identity-locked exit options and the direct costs they bear.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as it is an active, evolving intervention against an existing problem. The 'mandate' is to ensure constitutional equality, which remains a live issue. The classification as a snare highlights that while it aims to dismantle extraction, it does so through an actively enforced, and thus itself constraining, mechanism. It prevents mislabeling the judicial intervention as pure coordination, acknowledging the coercive aspect of imposing constitutional norms on resistant traditional systems.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_overreach_vs_constitutional_duty,
    'Is judicial intervention into personal law an appropriate exercise of constitutional duty to ensure equality, or an overreach into communal autonomy and religious freedom?',
    'Long-term societal impact assessment: if judicial interventions lead to sustained, broad-based improvements in women''s status without eroding social cohesion or religious freedom, it supports the constitutional duty framing. If it leads to backlash, social fragmentation, or perceived tyranny, it supports the overreach framing.',
    'If deemed overreach, the legitimacy of the judicial authority in this domain would be undermined, potentially leading to increased resistance and a reclassification towards a more coercive snare. If affirmed as duty, it strengthens the constraint''s legitimacy and its classification as a necessary, albeit extractive, reform mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_overreach_vs_constitutional_duty, conceptual, 'Ambiguity regarding the legitimate scope of judicial power in personal law reform.').

omega_variable(
    effectiveness_of_judicial_reform,
    'How effectively do judicial rulings translate into actual changes in women''s lives and practices on the ground, beyond formal legal pronouncements?',
    'Empirical studies tracking implementation rates, changes in social norms, and women''s access to justice post-ruling. This would involve surveying affected communities and legal aid organizations.',
    'If implementation is low and social norms remain unchanged, the constraint''s effective extractiveness (from women) remains high despite formal legal victories, and the ''beneficiary'' status of women''s rights advocates might be re-evaluated as more performative. If effective, it strengthens the claim of genuine reform.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(effectiveness_of_judicial_reform, empirical, 'Gap between de jure legal reform and de facto social change.').

omega_variable(
    identity_locked_exit_ambiguity,
    'To what extent is the ''identity_locked'' exit option for women within patriarchal personal law a result of internalized norms versus structural barriers?',
    'Post-exit support programs and awareness campaigns: if women continue to adhere to traditional norms even after structural barriers are removed and alternatives are presented, it suggests a higher degree of internalized identity lock. If they readily exit, it points to structural barriers as primary.',
    'If internalized identity lock is dominant, the effective suppression is higher than structural measures suggest, as the constraint persists even after external enforcement weakens. This would make the constraint more resilient and harder to dismantle through purely legal means.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_exit_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for women''s exit options.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__gender_rights_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1950, marriage_authority__gender_rights_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(marr_tr_t1970, marriage_authority__gender_rights_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(marr_tr_t1990, marriage_authority__gender_rights_reading, theater_ratio, 1990, 0.14).
narrative_ontology:measurement(marr_tr_t2010, marriage_authority__gender_rights_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(marr_tr_t2024, marriage_authority__gender_rights_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(marr_be_t1950, marriage_authority__gender_rights_reading, base_extractiveness, 1950, 0.7).
narrative_ontology:measurement(marr_be_t1970, marriage_authority__gender_rights_reading, base_extractiveness, 1970, 0.75).
narrative_ontology:measurement(marr_be_t1990, marriage_authority__gender_rights_reading, base_extractiveness, 1990, 0.8).
narrative_ontology:measurement(marr_be_t2010, marriage_authority__gender_rights_reading, base_extractiveness, 2010, 0.83).
narrative_ontology:measurement(marr_be_t2024, marriage_authority__gender_rights_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1950, marriage_authority__gender_rights_reading, suppression_requirement, 1950, 0.6).
narrative_ontology:measurement(marr_su_t1970, marriage_authority__gender_rights_reading, suppression_requirement, 1970, 0.65).
narrative_ontology:measurement(marr_su_t1990, marriage_authority__gender_rights_reading, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(marr_su_t2010, marriage_authority__gender_rights_reading, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(marr_su_t2024, marriage_authority__gender_rights_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__gender_rights_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, secularist_reading).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, federalist_millet_reading).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'marriage_authority' kernel, focusing on gender equality through judicial intervention. It is part of a family of constraints that represent different interpretations of marriage authority in a plural legal system.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
