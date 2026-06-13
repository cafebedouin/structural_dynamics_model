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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: us_constitution_1787__originalist_reading
 *   human_readable: US Constitution: Originalist Reading
 *   domain: constitutional_law/legal_theory/political_philosophy
 *
 * SUMMARY:
 *   This constraint represents the 'originalist' reading of the US
 *   Constitution, which posits that the meaning of the Constitution is fixed
 *   at the time of its ratification (1787) and should be interpreted
 *   according to the original intent of the framers or the original public
 *   meaning of the text. This reading aims to constrain judicial
 *   interpretation and legislative action to a narrow, historically defined
 *   set of legitimate practices and rights, often excluding modern social
 *   rights claims. It is a contested interpretive framework within
 *   constitutional law.
 *
 * KEY AGENTS:
 *   - originalist_legal_scholars: Agenda setter (institutional/analytical) — define and propagate the interpretive methodology.
 *   - conservative_judicial_activists: Beneficiary (institutional/powerful) — use this reading to justify specific judicial outcomes.
 *   - social_rights_advocates: Payer (organized/powerless) — bear the costs of a constrained interpretation that limits the recognition of evolving rights.
 *   - legislative_bodies_seeking_flexibility: Payer (institutional/powerful) — find their policy options limited by a fixed constitutional meaning.
 *   - living_constitutionalists: Excluded (institutional/analytical) — advocate for an evolving constitutional meaning, but are structurally excluded from the originalist interpretive framework.
 *   - positivist_legal_theorists: Observer (analytical) — analyze the originalist framework from a textualist perspective, often critiquing its reliance on intent.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_1787__originalist_reading, 0.6).
domain_priors:suppression_score(us_constitution_1787__originalist_reading, 0.7).
domain_priors:theater_ratio(us_constitution_1787__originalist_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_1787__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_1787__originalist_reading, "US Constitution: Originalist Reading").
narrative_ontology:topic_domain(us_constitution_1787__originalist_reading, "constitutional_law/legal_theory/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_1787__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_1787__originalist_reading, 'bfb27098-6e8e-4959-a00f-e6349d0f5d2e').
narrative_ontology:cs_kernel_codification('bfb27098-6e8e-4959-a00f-e6349d0f5d2e', fixed_text).
narrative_ontology:cs_authority_grounding('bfb27098-6e8e-4959-a00f-e6349d0f5d2e', lineage).
narrative_ontology:cs_interpretation_layer_present('bfb27098-6e8e-4959-a00f-e6349d0f5d2e').
narrative_ontology:cs_reading_relation('bfb27098-6e8e-4959-a00f-e6349d0f5d2e', us_constitution_1787__living_reading, forecloses).
narrative_ontology:cs_reading_relation('bfb27098-6e8e-4959-a00f-e6349d0f5d2e', us_constitution_1787__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('bfb27098-6e8e-4959-a00f-e6349d0f5d2e', foundational, constitutional_meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(constitutional_meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('bfb27098-6e8e-4959-a00f-e6349d0f5d2e', constitutional_meaning_fixed_at_ratification, deontological).
narrative_ontology:cs_axiom('bfb27098-6e8e-4959-a00f-e6349d0f5d2e', foundational, framers_intent_is_binding).
narrative_ontology:cs_axiom_status(framers_intent_is_binding, holdable).
narrative_ontology:cs_axiom_grounding('bfb27098-6e8e-4959-a00f-e6349d0f5d2e', framers_intent_is_binding, conventional).
narrative_ontology:cs_reference_frame('bfb27098-6e8e-4959-a00f-e6349d0f5d2e', original_public_meaning_1787).
narrative_ontology:cs_drift_state('bfb27098-6e8e-4959-a00f-e6349d0f5d2e', contemporary_judicial_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bfb27098-6e8e-4959-a00f-e6349d0f5d2e', '').
narrative_ontology:cs_kernel_id(us_constitution_1787__originalist_reading, us_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, originalist_legal_scholars).
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, conservative_judicial_activists).
narrative_ontology:constraint_victim(us_constitution_1787__originalist_reading, social_rights_advocates).
narrative_ontology:constraint_victim(us_constitution_1787__originalist_reading, legislative_bodies_seeking_flexibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop, articulate, and propagate the originalist interpretive methodology. Their careers and intellectual authority are often tied to the acceptance and application of this reading. They actively shape legal discourse and judicial appointments.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, originalist_legal_scholars, agenda_setter,
    institutional, generational, constrained, national).

% Apply the originalist reading in judicial decisions, often leading to outcomes that align with conservative political goals. Their power and legitimacy within the legal system are enhanced by the perceived objectivity and historical fidelity of originalism.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, conservative_judicial_activists, beneficiary,
    powerful, biographical, constrained, national).

% Seek to expand constitutional protections for evolving social rights (e.g., LGBTQ+ rights, environmental rights). They find their claims often rejected or limited by the originalist framework, which restricts the scope of constitutional meaning to historical understandings.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, social_rights_advocates, payer,
    organized, generational, constrained, national).

% Attempt to enact legislation addressing contemporary social and economic issues, but face challenges from originalist interpretations that deem such laws unconstitutional if they deviate from historical norms or framers' intent. Their policy options are constrained.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, legislative_bodies_seeking_flexibility, payer,
    institutional, immediate, constrained, national).

% Advocate for a 'living Constitution' whose meaning evolves with societal values and needs. Their interpretive framework is often dismissed or actively opposed by originalists, limiting their influence in judicial appointments and legal education, making their 'exit' from the debate difficult due to professional identity.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, living_constitutionalists, excluded,
    institutional, generational, identity_locked, national).

% Analyze constitutional interpretation from a perspective focused on the text and formal legal processes, often critiquing both originalism's reliance on intent and living constitutionalism's flexibility. They are not directly subject to the constraint's enforcement but provide critical commentary.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, positivist_legal_theorists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_1787__originalist_reading, originalist_legal_scholars).
narrative_ontology:fixing_cost_class(us_constitution_1787__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, historically grounded framework for constitutional interpretation, aiming to limit judicial discretion and ensure fidelity to the original document, thereby coordinating legal expectations across different branches of government and over time.
% TRANSFER_FUNCTION: Transfers interpretive authority from contemporary judges and legislators to historical figures (the framers), and from evolving societal norms to fixed historical understandings. This limits the scope of rights and legislative power, benefiting those who prefer a static constitutional order.
% ABSENT_VOICES: Living constitutionalists and proponents of evolving social rights are often marginalized in originalist discourse, their arguments dismissed as lacking historical grounding. They would argue for a more adaptable Constitution responsive to modern challenges.
% DISAPPEARANCE_RATIONALE: If the originalist reading vanished, constitutional interpretation would immediately shift towards more flexible or purely textualist approaches. Judicial decisions would likely expand the scope of rights and legislative power, leading to a significant rearrangement of legal and political landscapes.
% FOUNDING_PROBLEM: The originalist reading was developed to address concerns about judicial activism and the perceived erosion of democratic self-governance through unelected judges imposing their own policy preferences under the guise of constitutional interpretation.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars and conservative politicians attest that judicial activism remains a live problem, necessitating a fixed interpretive method. Critics (including many legal historians and other constitutional scholars) argue that the problem of judicial overreach is often overstated or selectively applied, and that originalism itself can be a form of judicial activism, making the status of the founding problem highly contested by sources outside the benefiting parties.
narrative_ontology:disappearance_verdict(us_constitution_1787__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_1787__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_1787__originalist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(us_constitution_1787__originalist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_1787__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_1787__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_1787__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it offers a coordination function (providing a stable, predictable framework for legal interpretation) but also involves significant asymmetric extraction. The extractiveness (0.6) stems from the limitation of rights and legislative flexibility, benefiting those who prefer a static interpretation. Suppression (0.7) is high due to the active enforcement of this interpretive methodology in courts and legal discourse, often marginalizing alternative readings. The theater ratio (0.2) is relatively low, as the interpretive work is genuinely aimed at historical reconstruction, though some performativity exists in selectively emphasizing certain historical evidence.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of originalist legal scholars and conservative judicial activists, this framework provides a necessary, objective constraint on judicial power, ensuring fidelity to the founding document. From the perspective of social rights advocates and legislative bodies, it is an extractive mechanism that prevents adaptation to modern societal needs and entrenches historical power imbalances. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Originalist legal scholars and conservative judicial activists are beneficiaries (d near 0.0) as they gain intellectual authority and policy outcomes from this reading. Social rights advocates and legislative bodies are victims (d near 1.0) as their goals are constrained. Living constitutionalists are excluded, their interpretive framework actively suppressed by the originalist one. Positivist legal theorists are analytical observers, not directly impacted by the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The originalist reading prevents mislabeling a politically motivated interpretive choice as a 'natural law' (Mountain) of constitutional meaning. By classifying it as a Tangled Rope, the framework highlights that its persistence depends on active enforcement and the suppression of alternatives, rather than inherent, unchangeable truth. The 'founding problem' of judicial overreach is still 'contested,' indicating that the constraint's mandate is not fully resolved and its function is debated.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    framers_intent_epistemic_access,
    'Is it epistemically possible to reliably ascertain the ''original intent'' of the framers, given historical distance and diverse individual motivations?',
    'Consensus among historians and legal methodologists on the reliability and completeness of historical sources for inferring collective intent.',
    'If unreliable, the constraint''s claimed ''mountain'' status collapses, revealing it as a constructed interpretive choice; if reliable, it strengthens the claim of fixed meaning.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(framers_intent_epistemic_access, empirical, 'The epistemic challenge of determining original intent.').

omega_variable(
    originalism_vs_living_constitution,
    'Is the originalist reading of the US Constitution a genuine interpretation of a fixed text, or a political strategy to limit judicial discretion and legislative power?',
    'Analysis of judicial outcomes: if originalist rulings consistently align with pre-1787 practices even when socially detrimental, it supports genuine interpretation; if they align with contemporary conservative policy goals, it suggests political strategy.',
    'If a political strategy, the constraint''s extractiveness and suppression are higher, as its ''fixed meaning'' claim serves to legitimize specific power distributions; if genuine, it is a more benign (though still potentially extractive) interpretive framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalism_vs_living_constitution, conceptual, 'Ambiguity between genuine interpretation and political strategy.').

omega_variable(
    kernel_reading_identification,
    'This constraint is the ''originalist_reading'' of the ''us_constitution_1787'' kernel. What structural elements would change if a ''living_reading'' or ''positivist_reading'' were adopted?',
    'Comparative legal analysis of hypothetical judicial outcomes under alternative interpretive regimes.',
    'A ''living_reading'' would expand the set of legitimate social rights claims and increase legislative flexibility, reducing extraction from social_rights_advocates. A ''positivist_reading'' would constrain judicial interpretation more strictly to the text, potentially reducing judicial activism but not necessarily altering the scope of rights in the same way.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Identification of this constraint as one reading of the US Constitution kernel, and the structural implications of sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_1787__originalist_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_1787__originalist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(us_c_tr_t10, us_constitution_1787__originalist_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(us_c_tr_t20, us_constitution_1787__originalist_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_1787__originalist_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(us_c_be_t10, us_constitution_1787__originalist_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(us_c_be_t20, us_constitution_1787__originalist_reading, base_extractiveness, 20, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_1787__originalist_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(us_c_su_t10, us_constitution_1787__originalist_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(us_c_su_t20, us_constitution_1787__originalist_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_1787__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_1787__originalist_reading, us_constitution_1787__living_reading).
narrative_ontology:affects_constraint(us_constitution_1787__originalist_reading, us_constitution_1787__positivist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the US Constitution (us_constitution_1787) kernel. Each reading is modeled as a separate constraint due to differing epsilon values and structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
