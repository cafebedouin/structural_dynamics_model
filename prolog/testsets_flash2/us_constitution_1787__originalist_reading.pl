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
 *   human_readable: US Constitution (Originalist Reading): Framers' Intent as Binding Law
 *   domain: constitutional_law/legal_theory/political_philosophy
 *
 * SUMMARY:
 *   This constraint represents the 'originalist' reading of the US
 *   Constitution, asserting that its meaning is fixed at the time of
 *   ratification and binding by the framers' intent. This reading leads to a
 *   narrow constraint set, legitimizes pre-1787 practices, and places modern
 *   social rights claims outside the constitutional boundary, demanding high
 *   epistemic rigor for historical evidence. It is one reading of the
 *   'us_constitution_1787' kernel, distinct from 'living_reading' and
 *   'positivist_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_1787__originalist_reading, 0.65).
domain_priors:suppression_score(us_constitution_1787__originalist_reading, 0.7).
domain_priors:theater_ratio(us_constitution_1787__originalist_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_1787__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_1787__originalist_reading, "US Constitution (Originalist Reading): Framers' Intent as Binding Law").
narrative_ontology:topic_domain(us_constitution_1787__originalist_reading, "constitutional_law/legal_theory/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_1787__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_1787__originalist_reading, '20839534-955d-456b-a47c-c178913c1622').
narrative_ontology:cs_kernel_codification('20839534-955d-456b-a47c-c178913c1622', fixed_text).
narrative_ontology:cs_authority_grounding('20839534-955d-456b-a47c-c178913c1622', lineage).
narrative_ontology:cs_interpretation_layer_present('20839534-955d-456b-a47c-c178913c1622').
narrative_ontology:cs_reading_relation('20839534-955d-456b-a47c-c178913c1622', us_constitution_1787__living_reading, coexists_with).
narrative_ontology:cs_reading_relation('20839534-955d-456b-a47c-c178913c1622', us_constitution_1787__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('20839534-955d-456b-a47c-c178913c1622', foundational, original_public_meaning_binding).
narrative_ontology:cs_axiom_status(original_public_meaning_binding, holdable).
narrative_ontology:cs_axiom_grounding('20839534-955d-456b-a47c-c178913c1622', original_public_meaning_binding, conventional).
narrative_ontology:cs_axiom('20839534-955d-456b-a47c-c178913c1622', foundational, framers_intent_determines_meaning).
narrative_ontology:cs_axiom_status(framers_intent_determines_meaning, holdable).
narrative_ontology:cs_axiom_grounding('20839534-955d-456b-a47c-c178913c1622', framers_intent_determines_meaning, conventional).
narrative_ontology:cs_reference_frame('20839534-955d-456b-a47c-c178913c1622', constitutional_text_fixed_meaning).
narrative_ontology:cs_drift_state('20839534-955d-456b-a47c-c178913c1622', contemporary_legal_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('20839534-955d-456b-a47c-c178913c1622', '').
narrative_ontology:cs_kernel_id(us_constitution_1787__originalist_reading, us_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, originalist_legal_scholars).
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, conservative_political_movements).
narrative_ontology:constraint_victim(us_constitution_1787__originalist_reading, social_rights_advocates).
narrative_ontology:constraint_victim(us_constitution_1787__originalist_reading, legislative_bodies_seeking_modern_interpretations).
narrative_ontology:constraint_vindicates(us_constitution_1787__originalist_reading, constitutional_originalism_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_1787__originalist_reading, judicial_restraint_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret the Constitution based on the original public meaning or intent of the framers at the time of ratification. Their careers and influence are tied to the persistence and application of this interpretive method. They actively publish, teach, and litigate to advance this reading.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, originalist_legal_scholars, agenda_setter,
    institutional, generational, identity_locked, national).

% Benefit from this reading as it often aligns with their policy goals, limiting government action and preserving traditional social structures. They fund legal challenges and support judicial appointments that adhere to originalist principles.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, conservative_political_movements, beneficiary,
    organized, biographical, constrained, national).

% Bear the costs of this reading as it often restricts the recognition of new social rights (e.g., environmental rights, LGBTQ+ rights) not explicitly envisioned by the framers. They must seek legislative remedies or constitutional amendments, which are difficult.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, social_rights_advocates, payer,
    moderate, generational, constrained, national).

% Find their legislative power constrained by originalist judicial review, which can strike down laws based on interpretations of 18th-century intent. They must draft legislation within narrow historical bounds or face judicial invalidation.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, legislative_bodies_seeking_modern_interpretations, payer,
    institutional, biographical, constrained, national).

% Advocate for an evolving constitutional meaning that adapts to contemporary society. While they participate in legal discourse, their interpretive method is often dismissed or actively opposed by originalist courts and scholars, effectively excluding their framework from dominant judicial application.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, living_constitutionalists, excluded,
    institutional, generational, identity_locked, national).

% Are the primary enforcers of constitutional meaning. When dominated by originalist judges, they actively apply this reading, shaping legal precedent and policy. Their institutional legitimacy is tied to maintaining a consistent interpretive methodology.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, judicial_branches, agenda_setter,
    institutional, generational, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, predictable framework for legal interpretation by anchoring constitutional meaning to a fixed historical point, reducing judicial discretion and ensuring fidelity to the founding document.
% TRANSFER_FUNCTION: Transfers interpretive authority from contemporary society and evolving norms to historical figures and their documented (or inferred) intentions, limiting the scope of modern rights and legislative power.
% ABSENT_VOICES: Advocates for a 'living constitution' are present in academic and political discourse but are often structurally excluded from the judicial decision-making process when originalist judges dominate. Their arguments for evolving meaning are systematically rejected in favor of historical fidelity.
% DISAPPEARANCE_RATIONALE: If the originalist reading vanished overnight, judicial review would immediately shift, allowing for broader interpretations of rights and governmental powers. Legislative bodies would operate with fewer historical constraints, and the political landscape would reorient around a more flexible constitutional framework.
% FOUNDING_PROBLEM: To prevent judicial activism and ensure that the Constitution's meaning remains consistent over time, reflecting the original compact rather than the changing whims of judges or society.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars and conservative political movements attest that the problem of judicial overreach and interpretive instability remains live. Critics (living constitutionalists, some legal historians) argue that while the problem of judicial activism is real, the originalist solution creates its own form of judicial overreach by imposing anachronistic views, and that the 'founding problem' is often a rhetorical cover for policy preferences.
narrative_ontology:disappearance_verdict(us_constitution_1787__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_1787__originalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_1787__originalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(us_constitution_1787__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_1787__originalist_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.65) stems from the systematic denial of modern rights claims and the imposition of historical interpretations that may not align with contemporary societal values. Suppression (0.70) is high due to the active judicial enforcement of this interpretive method, which effectively closes off alternative legal avenues for change. Theater ratio (0.20) is moderate; while there is genuine scholarly effort in historical research, some of the 'historical fidelity' serves to legitimize policy outcomes favored by beneficiaries. The increasing extractiveness and suppression over time reflect the growing dominance and enforcement of this reading in judicial and political spheres.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of originalist scholars, this is a 'rope' that provides stability and fidelity to the founding document. From the perspective of social rights advocates, it operates as a 'snare' that traps them in an outdated legal framework. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Originalist legal scholars and conservative political movements are primary beneficiaries, as this reading provides a powerful framework for their agendas. Social rights advocates and legislative bodies seeking modern interpretations are victims, as their efforts are often curtailed. The judicial branch acts as an agenda-setter, actively enforcing this reading. Living constitutionalists are excluded, as their interpretive framework is systematically marginalized.
 *
 * MANDATROPHY ANALYSIS:
 *   The originalist reading claims to prevent mandatrophy by ensuring the Constitution's meaning doesn't drift from its original purpose. However, critics argue that by rigidly adhering to historical intent, it creates a different form of mandatrophy where the 'founding problem' of judicial overreach is solved by creating an equally problematic 'founding problem' of anachronistic governance, effectively making the constraint's original mandate (to govern a dynamic society) atrophy in favor of a static historical interpretation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_intent_epistemic_certainty,
    'To what extent can the ''original intent'' or ''original public meaning'' of the framers truly be known and applied consistently across diverse constitutional provisions?',
    'Consensus among historical and linguistic scholars on the interpretability of 18th-century texts and debates, or empirical studies on the consistency of originalist judicial outcomes.',
    'If original intent is largely unknowable or inconsistently applied, the constraint''s claim to provide stable, objective meaning is undermined, reclassifying it closer to a Snare or Tangled Rope due to arbitrary enforcement. If it is highly knowable, it strengthens the Mountain-like aspects of the claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(original_intent_epistemic_certainty, empirical, 'The epistemic challenge of reliably discerning and applying historical intent.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (of modern rights claims) structural (due to legal precedent) or internalized (due to a belief in the inherent correctness of originalism)?',
    'Post-exit suppression trajectory: if originalist legal scholars continue to reject modern rights claims even after a shift in judicial composition, reclassify as partially internalized. If the shift in judicial composition immediately opens new avenues for rights claims, it is primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the interpretive framework persists even if external barriers are removed. If purely structural, a change in judicial composition could rapidly alter the constraint''s impact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for modern rights claims.').

omega_variable(
    framing_under_determination_originalism_vs_living,
    'Is the originalist reading a distinct constraint, or merely a specific interpretation of the ''living constitution'' kernel that emphasizes historical continuity?',
    'Conceptual analysis of the logical coherence of holding both originalist and living constitutionalist principles simultaneously within a single interpretive framework. If they are logically incompatible, they are distinct constraints. If one can be seen as a subset or specific emphasis of the other, the distinction is weaker.',
    'If they are not distinct constraints, the ''originalist_reading'' might be reclassified as a specific ''stance'' within the broader ''living_reading'' constraint, altering its perceived extractiveness and coordination function. If distinct, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_under_determination_originalism_vs_living, conceptual, 'Whether originalism and living constitutionalism are truly distinct constraints or different framings of the same underlying kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_1787__originalist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_1787__originalist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(us_c_tr_t10, us_constitution_1787__originalist_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(us_c_tr_t20, us_constitution_1787__originalist_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(us_c_tr_t30, us_constitution_1787__originalist_reading, theater_ratio, 30, 0.19).
narrative_ontology:measurement(us_c_tr_t40, us_constitution_1787__originalist_reading, theater_ratio, 40, 0.2).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_1787__originalist_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(us_c_be_t10, us_constitution_1787__originalist_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(us_c_be_t20, us_constitution_1787__originalist_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(us_c_be_t30, us_constitution_1787__originalist_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(us_c_be_t40, us_constitution_1787__originalist_reading, base_extractiveness, 40, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_1787__originalist_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(us_c_su_t10, us_constitution_1787__originalist_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(us_c_su_t20, us_constitution_1787__originalist_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(us_c_su_t30, us_constitution_1787__originalist_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(us_c_su_t40, us_constitution_1787__originalist_reading, suppression_requirement, 40, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_1787__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_1787__originalist_reading, us_constitution_1787__living_reading).
narrative_ontology:affects_constraint(us_constitution_1787__originalist_reading, us_constitution_1787__positivist_reading).
narrative_ontology:affects_constraint(us_constitution_1787__originalist_reading, us_supreme_court_precedent).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the US Constitution (us_constitution_1787), each with its own structural properties and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
