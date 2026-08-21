% ============================================================================
% CONSTRAINT STORY: us_constitution_text__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_text__originalist_reading, []).

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
 *   constraint_id: us_constitution_text__originalist_reading
 *   human_readable: US Constitution: Originalist Interpretation
 *   domain: constitutional_law/legal_philosophy
 *
 * SUMMARY:
 *   This constraint represents the originalist reading of the US
 *   Constitution, asserting that its meaning was fixed at the time of
 *   ratification and judicial interpretation must recover that original
 *   public understanding. This reading acts as a rigid constraint, binding
 *   judges to historical evidence and largely dismissing post-ratification
 *   practice unless it illuminates original meaning. It leads to high
 *   suppression of adaptive interpretation and benefits the conservative
 *   legal movement by providing a framework for desired judicial outcomes,
 *   while victimizing rights claims not explicitly grounded in 18th/19th
 *   century practice. This is one reading of the 'us_constitution_text'
 *   kernel.
 *
 * KEY AGENTS:
 *   - conservative_legal_movement: Primary beneficiary (institutional/arbitrage)
 *   - judicial_conservatives: Agenda setter (institutional/constrained)
 *   - rights_advocates_not_historically_grounded: Primary victim (powerless/trapped)
 *   - progressive_social_movements: Victim (organized/constrained)
 *   - legal_academics_originalist: Secondary beneficiary (powerful/mobile)
 *   - legal_academics_non_originalist: Excluded (powerful/mobile)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_text__originalist_reading, 0.68).
domain_priors:suppression_score(us_constitution_text__originalist_reading, 0.75).
domain_priors:theater_ratio(us_constitution_text__originalist_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_text__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_text__originalist_reading, "US Constitution: Originalist Interpretation").
narrative_ontology:topic_domain(us_constitution_text__originalist_reading, "constitutional_law/legal_philosophy").

domain_priors:requires_active_enforcement(us_constitution_text__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_text__originalist_reading, 'b4d6f815-eb6b-4c32-8594-e3c1fb61c6bb').
narrative_ontology:cs_kernel_codification('b4d6f815-eb6b-4c32-8594-e3c1fb61c6bb', fixed_text).
narrative_ontology:cs_authority_grounding('b4d6f815-eb6b-4c32-8594-e3c1fb61c6bb', lineage).
narrative_ontology:cs_interpretation_layer_present('b4d6f815-eb6b-4c32-8594-e3c1fb61c6bb').
narrative_ontology:cs_reading_relation('b4d6f815-eb6b-4c32-8594-e3c1fb61c6bb', us_constitution_text__living_constitutionalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('b4d6f815-eb6b-4c32-8594-e3c1fb61c6bb', us_constitution_text__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('b4d6f815-eb6b-4c32-8594-e3c1fb61c6bb', foundational, original_public_meaning_supremacy).
narrative_ontology:cs_axiom_status(original_public_meaning_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('b4d6f815-eb6b-4c32-8594-e3c1fb61c6bb', original_public_meaning_supremacy, deontological).
narrative_ontology:cs_axiom('b4d6f815-eb6b-4c32-8594-e3c1fb61c6bb', secondary, judicial_restraint_from_originalism).
narrative_ontology:cs_axiom_status(judicial_restraint_from_originalism, holdable).
narrative_ontology:cs_axiom_grounding('b4d6f815-eb6b-4c32-8594-e3c1fb61c6bb', judicial_restraint_from_originalism, instrumental).
narrative_ontology:cs_reference_frame('b4d6f815-eb6b-4c32-8594-e3c1fb61c6bb', ratification_era_public_understanding).
narrative_ontology:cs_drift_state('b4d6f815-eb6b-4c32-8594-e3c1fb61c6bb', contemporary_judicial_practice, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('b4d6f815-eb6b-4c32-8594-e3c1fb61c6bb', '').
narrative_ontology:cs_kernel_id(us_constitution_text__originalist_reading, us_constitution_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_text__originalist_reading, conservative_legal_movement).
narrative_ontology:constraint_beneficiary(us_constitution_text__originalist_reading, judicial_conservatives).
narrative_ontology:constraint_victim(us_constitution_text__originalist_reading, rights_advocates_not_historically_grounded).
narrative_ontology:constraint_victim(us_constitution_text__originalist_reading, progressive_social_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(us_constitution_text__originalist_reading, legal_academics_originalist).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the originalist reading by providing a stable, ideologically aligned framework for judicial appointments and legal outcomes, reinforcing its institutional dominance and policy preferences.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, conservative_legal_movement, beneficiary,
    institutional, generational, arbitrage, national).

% Actively interpret and enforce the Constitution according to originalist principles, shaping jurisprudence and legal precedent. Their careers and legitimacy are often tied to adherence to this interpretive method.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, judicial_conservatives, agenda_setter,
    institutional, biographical, constrained, national).

% Bear the costs of the originalist reading when their claims for new or evolving rights (e.g., privacy, LGBTQ+ rights) are rejected because they lack explicit grounding in the original public understanding of the Constitution. Their legal avenues are severely constrained.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, rights_advocates_not_historically_grounded, payer,
    powerless, immediate, trapped, national).

% Face significant obstacles in achieving their policy goals through judicial interpretation, as the originalist framework often limits the scope for adapting constitutional principles to contemporary social issues. They must resort to legislative or amendment processes, which are often more difficult.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, progressive_social_movements, payer,
    organized, generational, constrained, national).

% Benefit from the intellectual and professional prominence of originalism, contributing to its theoretical development and training future generations of lawyers and judges. Their work is central to maintaining the intellectual infrastructure of the reading.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, legal_academics_originalist, beneficiary,
    powerful, generational, mobile, national).

% Are often marginalized in judicial appointments and public discourse when their interpretive theories (e.g., living constitutionalism) are deemed incompatible with the dominant originalist framework. While not directly paying, their influence and career paths are constrained.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, legal_academics_non_originalist, excluded,
    powerful, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, predictable framework for constitutional interpretation, aiming to limit judicial discretion and ensure fidelity to the framers' intent, thereby coordinating legal outcomes across time.
% TRANSFER_FUNCTION: Transfers interpretive authority from contemporary societal values and evolving norms to historical evidence and the perceived original public meaning of the text, from rights advocates to those who benefit from a more constrained interpretation.
% ABSENT_VOICES: Rights advocates whose claims are not historically grounded, and legal scholars advocating for adaptive or evolving constitutional meaning, are often excluded from the interpretive conversation within the originalist framework. They would argue for a more flexible and responsive Constitution.
% DISAPPEARANCE_RATIONALE: If the originalist reading vanished overnight, the landscape of constitutional law would fundamentally shift. Judicial decisions would likely become more responsive to contemporary social values, new rights claims would gain traction, and the balance of power between judicial and legislative branches might alter, leading to a significant reorganization of legal and political structures.
% FOUNDING_PROBLEM: The problem of judicial activism and the perceived lack of objective constraints on judicial power, leading to concerns about unelected judges imposing their policy preferences.
% FOUNDING_PROBLEM_CORROBORATION: Conservative legal scholars and political figures consistently attest that judicial activism remains a live problem, necessitating originalism as a check. Critics, including many legal academics and progressive groups, argue that while judicial discretion is a concern, originalism itself has become a tool for ideological outcomes, making the 'founding problem' contested rather than universally accepted as live in its original framing.
narrative_ontology:disappearance_verdict(us_constitution_text__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_text__originalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_text__originalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(us_constitution_text__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_text__originalist_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_text__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_text__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_text__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.68) is high because the originalist framework systematically denies certain rights claims and policy outcomes that would otherwise be judicially accessible, channeling social change through more difficult legislative or amendment processes. Suppression (0.75) is also high, as the interpretive method actively delegitimizes and excludes alternative modes of constitutional reasoning, requiring active enforcement through judicial appointments and legal education. The theater ratio (0.20) is relatively low, as the commitment to historical inquiry is generally genuine, though critics argue it can sometimes be performative cover for ideological outcomes. The increasing extractiveness and suppression over time reflect the growing dominance and more rigid application of originalism in the judiciary since the late 20th century.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of conservative legal movements and originalist judges, this constraint is a legitimate and necessary 'rope' for judicial restraint and constitutional fidelity. From the perspective of rights advocates and progressive movements, it operates as a 'snare' or 'tangled rope,' extracting from them by denying evolving rights and suppressing adaptive interpretations, while benefiting a specific ideological agenda.
 *
 * DIRECTIONALITY LOGIC:
 *   The conservative legal movement and originalist judges are clear beneficiaries, as the constraint aligns with their ideological goals and enhances their institutional power. Rights advocates and progressive movements are victims, as their claims are systematically disadvantaged by the constraint's rigid interpretive rules. Non-originalist academics are excluded, as their interpretive frameworks are marginalized. The directionality for beneficiaries is low (subsidized), and for victims is high (extracted from), reflecting these structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling this as a pure 'rope' (as its proponents claim) by highlighting the asymmetric extraction and active suppression required to maintain its dominance. It also avoids classifying it as a pure 'snare' by acknowledging its genuine coordination function in providing a stable interpretive framework, even if that stability comes at a high cost to certain parties. The rising extractiveness and suppression over time suggest a drift towards greater extraction, even as the coordination function persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_meaning_determinacy,
    'How determinate is ''original public understanding'' in practice, and to what extent does its application involve subjective judicial choices rather than objective historical recovery?',
    'Empirical studies of judicial decision-making under originalist frameworks, comparing outcomes across cases with similar historical evidence but different policy implications. Analysis of dissenting opinions that challenge the determinacy of historical sources.',
    'If original meaning is highly indeterminate, the constraint''s ''suppression'' of judicial discretion is partly theatrical, and its ''extractiveness'' is more directly attributable to ideological preferences rather than historical fidelity, potentially reclassifying it closer to a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_meaning_determinacy, empirical, 'Ambiguity in recovering original public understanding and its impact on judicial objectivity.').

omega_variable(
    originalism_as_ideological_tool,
    'Is the originalist reading primarily a method of constitutional interpretation, or has it become an ideological tool to achieve specific policy outcomes?',
    'Longitudinal analysis of originalist judicial decisions, correlating outcomes with the political affiliations and stated policy preferences of the judges. Comparison of originalist arguments in cases with outcomes favored by conservative movements versus those with outcomes disfavored.',
    'If primarily an ideological tool, the ''theater_ratio'' would be higher, and the ''extractiveness'' would be more directly linked to the beneficiaries'' policy agenda, strengthening the ''snare'' component of the Tangled Rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(originalism_as_ideological_tool, conceptual, 'The extent to which originalism serves as an ideological instrument.').

omega_variable(
    kernel_reading_identity,
    'This constraint is the ''originalist_reading'' of the ''us_constitution_text'' kernel. How would the classification change if a ''living_constitutionalist_reading'' were adopted as the dominant interpretive framework?',
    'Hypothetical re-evaluation of metrics and stakeholder positions under a living constitutionalist framework, focusing on shifts in beneficiary/victim sets and the nature of ''suppression'' and ''extractiveness.''',
    'A living constitutionalist reading would likely shift beneficiaries and victims, potentially reducing ''suppression'' of adaptive interpretation and altering the distribution of ''extractiveness,'' possibly leading to a different constraint type (e.g., a Rope or a different form of Tangled Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Impact of adopting a different kernel reading on constraint classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_text__originalist_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1970, us_constitution_text__originalist_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(us_c_tr_t1985, us_constitution_text__originalist_reading, theater_ratio, 1985, 0.12).
narrative_ontology:measurement(us_c_tr_t2000, us_constitution_text__originalist_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(us_c_tr_t2010, us_constitution_text__originalist_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(us_c_tr_t2024, us_constitution_text__originalist_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1970, us_constitution_text__originalist_reading, base_extractiveness, 1970, 0.3).
narrative_ontology:measurement(us_c_be_t1985, us_constitution_text__originalist_reading, base_extractiveness, 1985, 0.45).
narrative_ontology:measurement(us_c_be_t2000, us_constitution_text__originalist_reading, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(us_c_be_t2010, us_constitution_text__originalist_reading, base_extractiveness, 2010, 0.63).
narrative_ontology:measurement(us_c_be_t2024, us_constitution_text__originalist_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1970, us_constitution_text__originalist_reading, suppression_requirement, 1970, 0.4).
narrative_ontology:measurement(us_c_su_t1985, us_constitution_text__originalist_reading, suppression_requirement, 1985, 0.55).
narrative_ontology:measurement(us_c_su_t2000, us_constitution_text__originalist_reading, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement(us_c_su_t2010, us_constitution_text__originalist_reading, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(us_c_su_t2024, us_constitution_text__originalist_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_text__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_text__originalist_reading, us_constitution_text__living_constitutionalist_reading).
narrative_ontology:affects_constraint(us_constitution_text__originalist_reading, us_constitution_text__positivist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'us_constitution_text' kernel. Each reading represents a structurally different constraint with its own ε, stakeholders, and classification. They are linked here to reflect their shared origin in the same foundational text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
