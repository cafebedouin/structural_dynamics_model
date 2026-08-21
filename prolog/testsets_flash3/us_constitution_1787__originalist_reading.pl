% ============================================================================
% CONSTRAINT STORY: us_constitution_1787__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_1787__originalist_reading, []).

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
 *   constraint_id: us_constitution_1787__originalist_reading
 *   human_readable: US Constitution (Originalist Reading): Meaning Fixed at Ratification
 *   domain: constitutional_law/legal_theory/political_philosophy
 *
 * SUMMARY:
 *   This constraint represents the 'originalist' reading of the US
 *   Constitution, asserting that its meaning was fixed at the time of
 *   ratification (1787) and is binding based on the framers' intent. This
 *   reading leads to a narrow interpretation of constitutional rights and
 *   powers, often legitimizing pre-1787 practices and placing modern social
 *   rights claims outside the constitutional boundary. It places high
 *   epistemic demands on historical evidence to discern original intent. The
 *   constraint is classified as a Tangled Rope because it provides a
 *   coordination function (stable interpretation) but also involves
 *   asymmetric extraction (limiting rights for some groups while empowering
 *   others through historical interpretation) and requires active enforcement
 *   by the judiciary.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_1787__originalist_reading, 0.45).
domain_priors:suppression_score(us_constitution_1787__originalist_reading, 0.6).
domain_priors:theater_ratio(us_constitution_1787__originalist_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_1787__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_1787__originalist_reading, "US Constitution (Originalist Reading): Meaning Fixed at Ratification").
narrative_ontology:topic_domain(us_constitution_1787__originalist_reading, "constitutional_law/legal_theory/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_1787__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_1787__originalist_reading, '2684d20a-8429-42f1-81cd-f446cd805622').
narrative_ontology:cs_kernel_codification('2684d20a-8429-42f1-81cd-f446cd805622', fixed_text).
narrative_ontology:cs_authority_grounding('2684d20a-8429-42f1-81cd-f446cd805622', lineage).
narrative_ontology:cs_interpretation_layer_present('2684d20a-8429-42f1-81cd-f446cd805622').
narrative_ontology:cs_reading_relation('2684d20a-8429-42f1-81cd-f446cd805622', us_constitution_1787__living_reading, forecloses).
narrative_ontology:cs_reading_relation('2684d20a-8429-42f1-81cd-f446cd805622', us_constitution_1787__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('2684d20a-8429-42f1-81cd-f446cd805622', foundational, constitutional_meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(constitutional_meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('2684d20a-8429-42f1-81cd-f446cd805622', constitutional_meaning_fixed_at_ratification, conventional).
narrative_ontology:cs_axiom('2684d20a-8429-42f1-81cd-f446cd805622', foundational, framers_intent_is_binding).
narrative_ontology:cs_axiom_status(framers_intent_is_binding, holdable).
narrative_ontology:cs_axiom_grounding('2684d20a-8429-42f1-81cd-f446cd805622', framers_intent_is_binding, conventional).
narrative_ontology:cs_reference_frame('2684d20a-8429-42f1-81cd-f446cd805622', original_public_meaning_1787).
narrative_ontology:cs_drift_state('2684d20a-8429-42f1-81cd-f446cd805622', contemporary_legal_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('2684d20a-8429-42f1-81cd-f446cd805622', '').
narrative_ontology:cs_kernel_id(us_constitution_1787__originalist_reading, us_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, originalist_scholars).
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, conservative_judiciary).
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, states_rights_advocates).
narrative_ontology:constraint_victim(us_constitution_1787__originalist_reading, social_rights_advocates).
narrative_ontology:constraint_victim(us_constitution_1787__originalist_reading, legislative_innovators).
narrative_ontology:constraint_victim(us_constitution_1787__originalist_reading, marginalized_groups).
narrative_ontology:constraint_vindicates(us_constitution_1787__originalist_reading, judicial_restraint_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_1787__originalist_reading, separation_of_powers_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and propagate the interpretive methodology, influencing judicial appointments and legal education. Their careers and intellectual identity are deeply tied to the originalist framework.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, originalist_scholars, agenda_setter,
    institutional, generational, identity_locked, national).

% Applies the originalist methodology in judicial rulings, shaping constitutional law. They benefit from the perceived stability and objectivity of the approach, which limits judicial discretion and external challenges to their authority.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, conservative_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from interpretations that limit federal power and expand state autonomy, aligning with the original understanding of federalism. They use originalist arguments to advance their policy goals.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, states_rights_advocates, beneficiary,
    organized, generational, mobile, national).

% Bear the costs of interpretations that deny or limit modern social rights (e.g., environmental protection, healthcare access, LGBTQ+ rights) not explicitly enumerated or contemplated by the framers. Their path to constitutional recognition is often foreclosed or made extremely difficult.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, social_rights_advocates, payer,
    organized, generational, constrained, national).

% Face judicial invalidation of laws that address contemporary social and economic problems through means not envisioned in 1787. They are forced to either abandon policy goals or seek constitutional amendments, a high-friction process.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, legislative_innovators, payer,
    powerful, biographical, constrained, national).

% Are often denied constitutional protections or remedies for systemic injustices because their claims do not fit within a narrow, historical understanding of rights. Their ability to achieve legal equality is severely constrained by this interpretive framework.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, marginalized_groups, payer,
    powerless, generational, trapped, national).

% Advocate for an evolving constitutional meaning but are often marginalized in judicial appointments and public discourse when originalism dominates. Their interpretive framework is actively resisted by the beneficiaries of originalism.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, living_constitutionalists, excluded,
    institutional, generational, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, predictable framework for constitutional interpretation, limiting judicial discretion and ensuring fidelity to the founding document, thereby coordinating legal and political actors around a fixed meaning.
% TRANSFER_FUNCTION: Transfers interpretive authority from contemporary society and evolving norms to historical figures and their presumed intentions, effectively transferring power to those who claim to discern that original intent.
% ABSENT_VOICES: Future generations and those whose rights were not recognized at the time of ratification are structurally absent from the 'framers' intent' conversation. Their perspectives would challenge the fixed nature of constitutional meaning and the legitimacy of applying 18th-century norms to 21st-century problems.
% DISAPPEARANCE_RATIONALE: If originalism as a binding interpretive method vanished overnight, the US legal and political landscape would undergo a profound rearrangement. Judicial decisions would immediately shift, legislative action would be less constrained by historical precedent, and the balance of power between branches and levels of government would be re-negotiated. The very nature of constitutional rights would be re-evaluated.
% FOUNDING_PROBLEM: To prevent arbitrary judicial rulings and ensure the Constitution's meaning remained consistent with the intentions of its creators, thereby preserving the democratic legitimacy of the founding document.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars and conservative legal groups attest the problem is live, citing concerns about judicial activism. Living constitutionalists and social rights advocates argue the problem is largely solved, and the constraint now serves to block necessary social evolution, with corroboration from historical analyses of evolving legal norms and public opinion polls on constitutional issues.
narrative_ontology:disappearance_verdict(us_constitution_1787__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_1787__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_1787__originalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(us_constitution_1787__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_1787__originalist_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_1787__originalist_reading_tests).
:- end_tests(us_constitution_1787__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate because while it provides a framework, it imposes significant costs on those whose interests or rights are not aligned with 18th-century understandings. Suppression (0.6) is substantial due to the active judicial enforcement required to maintain this interpretive framework against evolving societal norms and legislative efforts. Theater ratio (0.2) is low, as the interpretive work is genuinely complex, though some performativity exists in selectively applying historical evidence. Accessibility collapse (0.7) is high because this reading significantly narrows the pathways for new rights or powers to be recognized constitutionally. Resistance (0.5) is moderate, reflecting ongoing legal and political challenges from those advocating for a more dynamic interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of originalist scholars and the conservative judiciary, this constraint is a legitimate and necessary mechanism for preserving constitutional fidelity and stability. From the perspective of social rights advocates and marginalized groups, it is an extractive mechanism that perpetuates historical inequalities and blocks progress. The engine's classification will reflect this divergence based on the declared structural relationships and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Originalist scholars and the conservative judiciary are primary beneficiaries and agenda-setters, as their authority and interpretive framework are validated by this reading. States' rights advocates also benefit from interpretations that limit federal power. Social rights advocates, legislative innovators, and marginalized groups are victims, as their claims are often curtailed or denied by this fixed interpretation. Living constitutionalists are excluded, as their interpretive approach is actively suppressed within the dominant originalist discourse.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preventing arbitrary judicial rulings) is contested. While originalists argue it remains live, critics contend that the problem of arbitrary rulings has been replaced by the problem of anachronistic rulings, where the constraint's persistence serves to block necessary social evolution rather than genuinely coordinate. The classification as a Tangled Rope, rather than a pure Rope, acknowledges this dual function of coordination and asymmetric extraction, preventing mislabeling it as purely beneficial coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_intent_epistemic_certainty,
    'To what extent can the ''original intent'' of the framers be reliably and exhaustively determined, given historical distance and diverse individual motivations?',
    'Ongoing historical and textual scholarship, but with an acknowledgment of inherent limits. Consensus among a broad, ideologically diverse body of historians would increase confidence.',
    'If original intent is largely indeterminate, the constraint''s claim to objective, fixed meaning weakens, potentially reclassifying it closer to a Snare (pure extraction based on an unprovable claim) or a Piton (theatrical maintenance of an unknowable standard). If highly determinate, it reinforces the Mountain-like aspects of its claimed stability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(historical_intent_epistemic_certainty, empirical, 'The epistemic challenge of discerning original intent.').

omega_variable(
    originalism_vs_living_constitutionalism_foreclosure,
    'Does the originalist reading logically foreclose the ''living constitutionalist'' reading, or do they merely represent competing, coexisting interpretive frameworks?',
    'Analysis of the core premises: if one premise (e.g., ''meaning is fixed'') directly contradicts the other (''meaning evolves''), then foreclosure is structural. If they are merely different approaches to a shared object, they coexist.',
    'If originalism forecloses living constitutionalism, it implies a more rigid, Mountain-like structure for the originalist reading, as it actively negates alternatives. If they coexist, it highlights the political and social contestation, reinforcing the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalism_vs_living_constitutionalism_foreclosure, conceptual, 'The logical relationship between originalist and living constitutionalist interpretive frameworks.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (e.g., judicial precedent, appointment processes) or internalized (e.g., self-censorship by legal scholars, perceived futility of challenging originalism)?',
    'Post-ruling trajectory of legal scholarship and advocacy: if challenges persist and new interpretive methods emerge despite adverse rulings, suppression is more structural. If alternative interpretations diminish, it suggests internalized suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as targets carry the suppression with them. This would push the classification closer to a Snare, as exit from the interpretive framework becomes psychologically harder.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in legal interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_1787__originalist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_1787__originalist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(us_c_tr_t10, us_constitution_1787__originalist_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(us_c_tr_t20, us_constitution_1787__originalist_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(us_c_tr_t30, us_constitution_1787__originalist_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(us_c_tr_t40, us_constitution_1787__originalist_reading, theater_ratio, 40, 0.2).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_1787__originalist_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(us_c_be_t10, us_constitution_1787__originalist_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(us_c_be_t20, us_constitution_1787__originalist_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(us_c_be_t30, us_constitution_1787__originalist_reading, base_extractiveness, 30, 0.43).
narrative_ontology:measurement(us_c_be_t40, us_constitution_1787__originalist_reading, base_extractiveness, 40, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_1787__originalist_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(us_c_su_t10, us_constitution_1787__originalist_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(us_c_su_t20, us_constitution_1787__originalist_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(us_c_su_t30, us_constitution_1787__originalist_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(us_c_su_t40, us_constitution_1787__originalist_reading, suppression_requirement, 40, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_1787__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_1787__originalist_reading, us_constitution_1787__living_reading).
narrative_ontology:affects_constraint(us_constitution_1787__originalist_reading, us_constitution_1787__positivist_reading).
narrative_ontology:affects_constraint(us_constitution_1787__originalist_reading, judicial_review_doctrine).
narrative_ontology:affects_constraint(us_constitution_1787__originalist_reading, federalism_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'us_constitution_1787' kernel. Its structural properties and classification differ significantly from the 'living_reading' and 'positivist_reading' siblings, which are modeled as separate constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
