% ============================================================================
% CONSTRAINT STORY: marriage_authority__federalist_millet_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__federalist_millet_reading, []).

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
 *   constraint_id: marriage_authority__federalist_millet_reading
 *   human_readable: Federalist-Millet Reading of Fragmented Marriage Authority
 *   domain: legal/political/constitutional
 *
 * SUMMARY:
 *   This constraint describes the 'federalist-millet' reading of marriage
 *   authority, where legal pluralism in family law is deliberately maintained
 *   as a consociational mechanism to prevent majoritarian domination and
 *   ensure political stability in deeply divided societies. It views the
 *   fragmentation not as a historical anomaly, but as a functional feature of
 *   the constitutional design, sustained by an elite bargain. This reading
 *   emphasizes the coordination function of preventing tyranny and
 *   maintaining peace, with minority communities as primary beneficiaries.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__federalist_millet_reading, 0.15).
domain_priors:suppression_score(marriage_authority__federalist_millet_reading, 0.2).
domain_priors:theater_ratio(marriage_authority__federalist_millet_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__federalist_millet_reading, rope).
narrative_ontology:human_readable(marriage_authority__federalist_millet_reading, "Federalist-Millet Reading of Fragmented Marriage Authority").
narrative_ontology:topic_domain(marriage_authority__federalist_millet_reading, "legal/political/constitutional").

domain_priors:requires_active_enforcement(marriage_authority__federalist_millet_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__federalist_millet_reading, '42710a30-b1ec-4ec5-a27b-7b37c2fb6d9c').
narrative_ontology:cs_kernel_codification('42710a30-b1ec-4ec5-a27b-7b37c2fb6d9c', formalized).
narrative_ontology:cs_authority_grounding('42710a30-b1ec-4ec5-a27b-7b37c2fb6d9c', lineage).
narrative_ontology:cs_interpretation_layer_present('42710a30-b1ec-4ec5-a27b-7b37c2fb6d9c').
narrative_ontology:cs_reading_relation('42710a30-b1ec-4ec5-a27b-7b37c2fb6d9c', marriage_authority__communal_autonomy_reading, coexists_with).
narrative_ontology:cs_reading_relation('42710a30-b1ec-4ec5-a27b-7b37c2fb6d9c', marriage_authority__secularist_reading, coexists_with).
narrative_ontology:cs_reading_relation('42710a30-b1ec-4ec5-a27b-7b37c2fb6d9c', marriage_authority__gender_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('42710a30-b1ec-4ec5-a27b-7b37c2fb6d9c', marriage_authority__judicial_harmonization_reading, coexists_with).
narrative_ontology:cs_axiom('42710a30-b1ec-4ec5-a27b-7b37c2fb6d9c', foundational, fragmentation_as_anti_tyranny_mechanism).
narrative_ontology:cs_axiom_status(fragmentation_as_anti_tyranny_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('42710a30-b1ec-4ec5-a27b-7b37c2fb6d9c', fragmentation_as_anti_tyranny_mechanism, instrumental).
narrative_ontology:cs_axiom('42710a30-b1ec-4ec5-a27b-7b37c2fb6d9c', secondary, elite_bargain_as_stability_driver).
narrative_ontology:cs_axiom_status(elite_bargain_as_stability_driver, holdable).
narrative_ontology:cs_axiom_grounding('42710a30-b1ec-4ec5-a27b-7b37c2fb6d9c', elite_bargain_as_stability_driver, empirically_contingent).
narrative_ontology:cs_reference_frame('42710a30-b1ec-4ec5-a27b-7b37c2fb6d9c', consociational_constitutionalism).
narrative_ontology:cs_drift_state('42710a30-b1ec-4ec5-a27b-7b37c2fb6d9c', contemporary_globalization_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('42710a30-b1ec-4ec5-a27b-7b37c2fb6d9c', '').
narrative_ontology:cs_kernel_id(marriage_authority__federalist_millet_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__federalist_millet_reading, minority_religious_communities).
narrative_ontology:constraint_beneficiary(marriage_authority__federalist_millet_reading, political_elites).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_authority__federalist_millet_reading, national_legislature).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the ability to govern their internal family matters according to their own traditions, protected from majoritarian legislative interference. Their exit options are constrained by their identity and geographic rootedness.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, minority_religious_communities, beneficiary,
    organized, generational, constrained, national).

% Maintain stability by accommodating diverse religious and ethnic groups through legal pluralism, avoiding potentially destabilizing conflicts over a uniform civil code. They manage the consociational bargain.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, political_elites, agenda_setter,
    institutional, biographical, mobile, national).

% Bears the cost of legislative paralysis on family law, as attempts to unify civil code are blocked by the consociational bargain. Its power is constrained by the need to maintain elite consensus.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, national_legislature, payer,
    institutional, biographical, constrained, national).

% Advocate for a uniform civil code based on secular principles, viewing legal pluralism as discriminatory and an impediment to national unity. They are excluded from the elite bargain that maintains the fragmented authority.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, secularist_advocates, excluded,
    moderate, generational, constrained, national).

% Challenge the internal norms of personal laws that they argue discriminate against women, seeking judicial intervention to enforce constitutional equality guarantees. Their concerns are often sidelined by the consociational framework.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, gender_equality_advocates, excluded,
    organized, generational, constrained, national).

% Interprets the constitutional framework that permits legal pluralism, balancing community rights with individual equality. Its role is to adjudicate disputes within the fragmented system, not to unify it.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, constitutional_court, observer,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the peaceful coexistence of diverse religious and ethnic communities within a single state by granting them autonomy over personal law, thereby preventing majoritarian imposition of a single family law system.
% TRANSFER_FUNCTION: Transfers the authority to define and enforce marriage norms from a centralized national legislature to various recognized religious and ethnic communities, in exchange for political stability and elite consensus.
% ABSENT_VOICES: Secularist and gender equality advocates are largely absent from the elite bargain that sustains this fragmentation; they would argue for a uniform civil code or judicial reform to ensure individual rights, but their voices are structurally marginalized by the consociational arrangement.
% DISAPPEARANCE_RATIONALE: If this fragmented authority vanished overnight, it would likely lead to immediate legislative attempts to impose a uniform civil code, triggering significant resistance and potential social unrest from minority communities who would perceive it as an attack on their identity and autonomy. The political landscape would be fundamentally reshaped.
% FOUNDING_PROBLEM: The problem of governing a deeply pluralistic society with diverse religious and ethnic groups, each with distinct personal laws, without resorting to majoritarian tyranny or civil conflict.
% FOUNDING_PROBLEM_CORROBORATION: Political scientists and constitutional scholars from outside the immediate benefiting parties corroborate that the problem of managing deep societal cleavages through consociational arrangements remains live in many pluralistic states, and that this fragmentation serves as a mechanism for stability, even if imperfectly.
narrative_ontology:disappearance_verdict(marriage_authority__federalist_millet_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__federalist_millet_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__federalist_millet_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(marriage_authority__federalist_millet_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__federalist_millet_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__federalist_millet_reading_tests).
:- end_tests(marriage_authority__federalist_millet_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the primary function is coordination (preventing majoritarian overreach) rather than rent extraction. Suppression is also low (0.2) as the system relies on elite consensus and community buy-in, not overt coercion, to maintain the fragmentation. Theater ratio is negligible (0.05) as the system genuinely performs its function of managing diversity. The stability of these metrics over time reflects the enduring nature of the consociational bargain.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of minority communities and political elites, this is a functional rope that ensures stability and autonomy. From the perspective of secularist and gender equality advocates, it is a tangled rope or even a snare, as it entrenches discriminatory practices and prevents national integration. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Minority religious communities are clear beneficiaries (low d) as they retain autonomy over personal law. Political elites are agenda-setters and beneficiaries (low d) as they maintain stability and their own power through this arrangement. The national legislature, while powerful, is a 'payer' in terms of its inability to enact a uniform civil code, bearing the cost of legislative paralysis (moderate d). Secularist and gender equality advocates are excluded (high d) as their preferred outcomes are suppressed by the existing framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consociational_efficacy_vs_tyranny,
    'Is the consociational mechanism of fragmented marriage authority genuinely preventing majoritarian tyranny, or is it merely entrenching elite power and avoiding difficult reforms?',
    'Comparative analysis of states with similar deep cleavages but different approaches to family law, assessing long-term stability, minority rights protection, and democratic participation outcomes.',
    'If it''s primarily entrenching elite power, the extractiveness for political elites would be higher, and the claimed ''rope'' classification would shift towards ''tangled_rope'' or ''snare'' for the excluded seats. If it genuinely prevents tyranny, the low extractiveness and ''rope'' classification are robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consociational_efficacy_vs_tyranny, empirical, 'Assessing the true function of legal pluralism: anti-tyranny or elite entrenchment.').

omega_variable(
    internal_community_extraction,
    'Does the autonomy granted to minority religious communities (beneficiaries of this reading) lead to internal extraction or suppression of individual rights within those communities, particularly for women or dissenting members?',
    'Detailed sociological and legal studies of personal law application within specific communities, focusing on individual agency, exit options, and access to justice for vulnerable members.',
    'If significant internal extraction is found, the ''minority_religious_communities'' seat, while a beneficiary of the federalist-millet reading, would itself be an agenda-setter of an internal ''snare'' or ''tangled_rope'' constraint, complicating the overall classification and highlighting nested constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(internal_community_extraction, empirical, 'Investigating potential internal extraction within beneficiary communities.').

omega_variable(
    framing_of_legislative_paralysis,
    'Is the ''legislative paralysis'' on family law, seen as a feature by this reading, a legitimate cost of stability or a defect that prevents necessary social evolution?',
    'Analysis of public opinion, social indicators, and expert consensus on the need for family law reform, and the impact of its absence on societal well-being and individual rights.',
    'If widely seen as a defect, the ''national_legislature''s'' role as ''payer'' would be amplified, and the overall ''rope'' classification would be challenged by the growing costs of inaction, potentially shifting towards a ''piton'' if the original problem is dead but the structure persists inertially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_of_legislative_paralysis, preference, 'Conceptual framing of legislative inaction: feature or defect.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__federalist_millet_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority__federalist_millet_reading, theater_ratio, 0, 0.03).
narrative_ontology:measurement(marr_tr_t10, marriage_authority__federalist_millet_reading, theater_ratio, 10, 0.04).
narrative_ontology:measurement(marr_tr_t20, marriage_authority__federalist_millet_reading, theater_ratio, 20, 0.04).
narrative_ontology:measurement(marr_tr_t30, marriage_authority__federalist_millet_reading, theater_ratio, 30, 0.05).
narrative_ontology:measurement(marr_tr_t40, marriage_authority__federalist_millet_reading, theater_ratio, 40, 0.05).
narrative_ontology:measurement(marr_tr_t50, marriage_authority__federalist_millet_reading, theater_ratio, 50, 0.05).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority__federalist_millet_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(marr_be_t10, marriage_authority__federalist_millet_reading, base_extractiveness, 10, 0.12).
narrative_ontology:measurement(marr_be_t20, marriage_authority__federalist_millet_reading, base_extractiveness, 20, 0.13).
narrative_ontology:measurement(marr_be_t30, marriage_authority__federalist_millet_reading, base_extractiveness, 30, 0.14).
narrative_ontology:measurement(marr_be_t40, marriage_authority__federalist_millet_reading, base_extractiveness, 40, 0.15).
narrative_ontology:measurement(marr_be_t50, marriage_authority__federalist_millet_reading, base_extractiveness, 50, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority__federalist_millet_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(marr_su_t10, marriage_authority__federalist_millet_reading, suppression_requirement, 10, 0.17).
narrative_ontology:measurement(marr_su_t20, marriage_authority__federalist_millet_reading, suppression_requirement, 20, 0.18).
narrative_ontology:measurement(marr_su_t30, marriage_authority__federalist_millet_reading, suppression_requirement, 30, 0.19).
narrative_ontology:measurement(marr_su_t40, marriage_authority__federalist_millet_reading, suppression_requirement, 40, 0.2).
narrative_ontology:measurement(marr_su_t50, marriage_authority__federalist_millet_reading, suppression_requirement, 50, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__federalist_millet_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__secularist_reading).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'marriage_authority' kernel, focusing on legal pluralism as a consociational anti-tyranny mechanism. It is linked to other readings that emphasize community autonomy, secular unification, gender equality, and judicial harmonization, each representing a distinct structural claim about marriage authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
