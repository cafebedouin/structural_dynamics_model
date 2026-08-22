% ============================================================================
% CONSTRAINT STORY: marriage_authority__judicial_harmonization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__judicial_harmonization_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: marriage_authority__judicial_harmonization_reading
 *   human_readable: Judicial Harmonization of Marriage Authority via Constitutional Floor
 *   domain: legal/constitutional/family_law
 *
 * SUMMARY:
 *   This constraint story instantiates the judicial_harmonization_reading of
 *   the marriage_authority kernel: the Supreme Court imposes constitutional
 *   floors on personal law codes through case-by-case fundamental rights
 *   review, bypassing the legislature and creating a
 *   convergence-without-legislation pathway. The judiciary acts as both
 *   agenda-setter and beneficiary, expanding its jurisdiction over family law
 *   while presenting the mechanism as transitional pending formal UCC
 *   legislation.
 *
 * KEY AGENTS:
 *   - judiciary: Agenda-setter and primary beneficiary (institutional/analytical) â expands authority via constitutional interpretation over personal law
 *   - personal_law_communities: Primary target (organized/constrained) â bear the loss of communal autonomy as personal law codes are overridden
 *   - union_legislature: Excluded actor (institutional/constrained) â law-making function bypassed by case-by-case judicial review
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__judicial_harmonization_reading, 0.45).
domain_priors:suppression_score(marriage_authority__judicial_harmonization_reading, 0.5).
domain_priors:theater_ratio(marriage_authority__judicial_harmonization_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__judicial_harmonization_reading, scaffold).
narrative_ontology:human_readable(marriage_authority__judicial_harmonization_reading, "Judicial Harmonization of Marriage Authority via Constitutional Floor").
narrative_ontology:topic_domain(marriage_authority__judicial_harmonization_reading, "legal/constitutional/family_law").

domain_priors:requires_active_enforcement(marriage_authority__judicial_harmonization_reading).
narrative_ontology:has_sunset_clause(marriage_authority__judicial_harmonization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__judicial_harmonization_reading, 'e3f9d4a5-5cd8-4654-a974-f1635151617d').
narrative_ontology:cs_kernel_codification('e3f9d4a5-5cd8-4654-a974-f1635151617d', formalized).
narrative_ontology:cs_authority_grounding('e3f9d4a5-5cd8-4654-a974-f1635151617d', lineage).
narrative_ontology:cs_interpretation_layer_present('e3f9d4a5-5cd8-4654-a974-f1635151617d').
narrative_ontology:cs_reading_relation('e3f9d4a5-5cd8-4654-a974-f1635151617d', marriage_authority__communal_autonomy_reading, influences).
narrative_ontology:cs_reading_relation('e3f9d4a5-5cd8-4654-a974-f1635151617d', marriage_authority__secularist_reading, influences).
narrative_ontology:cs_reading_relation('e3f9d4a5-5cd8-4654-a974-f1635151617d', marriage_authority__gender_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('e3f9d4a5-5cd8-4654-a974-f1635151617d', marriage_authority__federalist_millet_reading, influences).
narrative_ontology:cs_axiom('e3f9d4a5-5cd8-4654-a974-f1635151617d', foundational, incremental_judicial_harmonization_as_ucc_substitute).
narrative_ontology:cs_axiom_status(incremental_judicial_harmonization_as_ucc_substitute, holdable).
narrative_ontology:cs_axiom_grounding('e3f9d4a5-5cd8-4654-a974-f1635151617d', incremental_judicial_harmonization_as_ucc_substitute, instrumental).
narrative_ontology:cs_axiom('e3f9d4a5-5cd8-4654-a974-f1635151617d', foundational, constitutional_floor_as_judicially_discoverable).
narrative_ontology:cs_axiom_status(constitutional_floor_as_judicially_discoverable, holdable).
narrative_ontology:cs_axiom_grounding('e3f9d4a5-5cd8-4654-a974-f1635151617d', constitutional_floor_as_judicially_discoverable, conventional).
narrative_ontology:cs_reference_frame('e3f9d4a5-5cd8-4654-a974-f1635151617d', constitutional_fundamental_rights_framework).
narrative_ontology:cs_drift_state('e3f9d4a5-5cd8-4654-a974-f1635151617d', contemporary_judicial_review_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e3f9d4a5-5cd8-4654-a974-f1635151617d', '').
narrative_ontology:cs_kernel_id(marriage_authority__judicial_harmonization_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__judicial_harmonization_reading, judiciary).
narrative_ontology:constraint_victim(marriage_authority__judicial_harmonization_reading, personal_law_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Supreme Court expands its constitutional interpretation authority over personal law codes through case-by-case fundamental rights review, imposing uniform floors without awaiting legislative action. Presents the mechanism as transitional pending formal UCC legislation while consolidating institutional control over family law norm-development.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, judiciary, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__judicial_harmonization_reading, judiciary, beneficiary).

% Religious and cultural communities whose marriage and family norms are governed by distinct personal law codes. Their communal autonomy is progressively overridden by judicial constitutional interpretation; resistance requires constitutional amendment or legislative counter-action, both politically blocked by the convergence narrative.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, personal_law_communities, payer,
    organized, generational, constrained, national).

% Parliament holds constitutional authority to enact a Uniform Civil Code under Article 44 but has deferred action due to political complexity. The judiciary's case-by-case harmonization bypasses the legislative process, reducing political urgency for comprehensive codification while encroaching on the legislature's domain without formal debate.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, union_legislature, excluded,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority__judicial_harmonization_reading, judiciary).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a transitional mechanism to harmonize disparate personal law codes under a minimal constitutional floor when legislative action is politically blocked; enables incremental protection of fundamental rights across communities without requiring comprehensive statutory reform.
% TRANSFER_FUNCTION: Transfers marriage-law authority from communal personal law traditions and the legislative domain to the Supreme Court's case-by-case constitutional interpretation; transfers autonomy from personal law communities to judicial oversight.
% ABSENT_VOICES: Conservative religious authorities within personal law communities are structurally marginalized in constitutional review; the legislature is bypassed; proponents of comprehensive legislative UCC are sidelined by the convergence-without-legislation narrative.
% DISAPPEARANCE_RATIONALE: If case-by-case judicial imposition of constitutional floors vanished, personal law codes would revert to community-administered norms without uniform rights overrides, the legislature would face renewed pressure to codify or tolerate pluralism, and the judiciary's expanded family law jurisdiction would contract.
% FOUNDING_PROBLEM: Post-colonial personal law pluralism generated conflicting rights outcomes and gender-unequal protections across communities; political deadlock prevented Uniform Civil Code legislation; constitutional fundamental rights required a minimal floor in family law.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars and women's rights organizations outside the judiciary attest to the persistence of rights violations under unreformed personal law. Personal law community leaders and federalism scholars dispute that judicial harmonization was the proper response, arguing the problem should be solved by political negotiation, not constitutional override.
narrative_ontology:disappearance_verdict(marriage_authority__judicial_harmonization_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__judicial_harmonization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__judicial_harmonization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority__judicial_harmonization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__judicial_harmonization_reading, 0.45, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__judicial_harmonization_reading_tests).
:- end_tests(marriage_authority__judicial_harmonization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.45 reflects moderate extraction: communities lose autonomy and the legislature is bypassed, but a genuine coordination function exists in the form of rights protection and cross-community uniformity. Suppression at 0.50 reflects that the constitutional floor suppresses alternative personal law norms through judicial authority rather than raw coercion. Theater ratio at 0.40 captures the Court's performative claim that it merely interprets existing law while actively harmonizing personal law codes. Accessibility collapse at 0.60 because once a constitutional floor is established in a domain, alternative communal norms collapse for that population. Resistance at 0.55 reflects moderate political and legal resistance from personal law communities and legislative passivity.
 *
 * PERSPECTIVAL GAP:
 *   The judiciary experiences this as legitimate constitutional interpretation and transitional coordination (low effective extraction, high coordination value). Personal law communities experience it as external imposition eroding religious and cultural autonomy (high extraction). The legislature experiences authority erosion. The engine computes per-seat divergence from these structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary sits at low directionality (near-beneficiary) because the constraint expands its institutional authority and jurisdiction; it has analytical exit (can modulate its own doctrine). Personal law communities sit at high directionality (near-target) because the constraint overrides their norms without their consent and their exit is constrained by constitutional supremacy. The legislature, though excluded, would sit at elevated directionality as its domain is encroached upon.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold classification prevents mislabeling by requiring transitional intent (convergence-without-legislation) and a coordination function (constitutional rights protection). Without the sunset/transitional framing, the same metrics might compute as tangled_rope. The judiciary's dual role as agenda-setter and beneficiary is the structural signal that distinguishes transitional judicial expansion from permanent capture; if the transitional justification is abandoned, the constraint drifts toward snare or tangled_rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_harmonization_transience,
    'Is this constraint a transitional scaffold toward eventual legislative harmonization, or has it become a permanent institutional settlement concentrating authority in the judiciary?',
    'Track Supreme Court rhetoric and doctrine across two decades: if decisions increasingly invite legislative codification and defer to parliamentary domains, transience is confirmed; if doctrine entrenches judicial supremacy as the normal mode of family law development, permanence is established.',
    'Confirmed transience sustains scaffold classification; entrenched permanence reclassifies as tangled_rope or piton depending on theater_ratio and beneficiary concentration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_harmonization_transience, conceptual, 'Whether judicial harmonization is genuinely transitional or permanent capture').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of communal autonomy structural (coercive judicial power) or internalized (communities accepting constitutional legitimacy)?',
    'Measure post-decision compliance trajectories and political resistance: sustained protest indicates structural suppression; quiet compliance indicates internalized legitimacy.',
    'Internalized suppression lowers effective resistance and raises accessibility_collapse; structural suppression raises resistance and indicates coercive maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression of communal autonomy').

omega_variable(
    cs_framing_under_determination,
    'Does the commitment system authority derive from the constitutional text as fixed lineage, or from the Court''s interpretive practice as evolving practice?',
    'Examine doctrinal foundations: if the Court consistently anchors decisions in specific constitutional articles and original intent, lineage dominates; if precedent and prudential reasoning dominate, practice dominates.',
    'Lineage framing strengthens interpretation_layer_present as buffer; practice framing shifts authority_grounding and exposes the kernel to practice_drift classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cs_framing_under_determination, conceptual, 'Alternative CS framing under-determination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__judicial_harmonization_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marriage_judicial_harm_tr_t0, marriage_authority__judicial_harmonization_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(marriage_judicial_harm_tr_t6, marriage_authority__judicial_harmonization_reading, theater_ratio, 6, 0.2).
narrative_ontology:measurement(marriage_judicial_harm_tr_t12, marriage_authority__judicial_harmonization_reading, theater_ratio, 12, 0.26).
narrative_ontology:measurement(marriage_judicial_harm_tr_t18, marriage_authority__judicial_harmonization_reading, theater_ratio, 18, 0.32).
narrative_ontology:measurement(marriage_judicial_harm_tr_t24, marriage_authority__judicial_harmonization_reading, theater_ratio, 24, 0.37).
narrative_ontology:measurement(marriage_judicial_harm_tr_t30, marriage_authority__judicial_harmonization_reading, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(marriage_judicial_harm_be_t0, marriage_authority__judicial_harmonization_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(marriage_judicial_harm_be_t6, marriage_authority__judicial_harmonization_reading, base_extractiveness, 6, 0.25).
narrative_ontology:measurement(marriage_judicial_harm_be_t12, marriage_authority__judicial_harmonization_reading, base_extractiveness, 12, 0.3).
narrative_ontology:measurement(marriage_judicial_harm_be_t18, marriage_authority__judicial_harmonization_reading, base_extractiveness, 18, 0.36).
narrative_ontology:measurement(marriage_judicial_harm_be_t24, marriage_authority__judicial_harmonization_reading, base_extractiveness, 24, 0.41).
narrative_ontology:measurement(marriage_judicial_harm_be_t30, marriage_authority__judicial_harmonization_reading, base_extractiveness, 30, 0.45).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(marriage_authority__judicial_harmonization_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__judicial_harmonization_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_authority__judicial_harmonization_reading, communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__judicial_harmonization_reading, secularist_reading).
narrative_ontology:affects_constraint(marriage_authority__judicial_harmonization_reading, gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__judicial_harmonization_reading, federalist_millet_reading).

% DUAL FORMULATION NOTE:
% The marriage_authority kernel decomposes into multiple structurally distinct readings. This reading isolates the judicial case-by-case harmonization mechanism as a constraint distinct from normative claims about communal autonomy, secular legislation, gender equality, or federal pluralism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
