% ============================================================================
% CONSTRAINT STORY: us_constitution_interpretive__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_interpretive__originalist_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: us_constitution_interpretive__originalist_reading
 *   human_readable: US Constitutional Originalist Interpretation
 *   domain: constitutional_law/legal_interpretation/political_theory
 *
 * SUMMARY:
 *   This constraint describes the 'originalist reading' of US Constitutional
 *   interpretation, which asserts that the Constitution's meaning was fixed
 *   at the time of its ratification and that interpretive authority derives
 *   from fidelity to the framers' intent or original public meaning. It is
 *   one reading of the broader 'us_constitution_interpretive' kernel, which
 *   also includes 'living_constitution_reading' and
 *   'popular_constitutionalism_reading'. This reading aims to constrain
 *   judicial power and limit federal expansion, often benefiting federalism
 *   advocates and certain rights claimants while imposing costs on those
 *   seeking broader, evolving rights or federal regulatory action.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_interpretive__originalist_reading, 0.7).
domain_priors:suppression_score(us_constitution_interpretive__originalist_reading, 0.8).
domain_priors:theater_ratio(us_constitution_interpretive__originalist_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_interpretive__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_interpretive__originalist_reading, "US Constitutional Originalist Interpretation").
narrative_ontology:topic_domain(us_constitution_interpretive__originalist_reading, "constitutional_law/legal_interpretation/political_theory").

domain_priors:requires_active_enforcement(us_constitution_interpretive__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_interpretive__originalist_reading, '1504037b-308f-4b1b-b6cf-e7eaf1e42296').
narrative_ontology:cs_kernel_codification('1504037b-308f-4b1b-b6cf-e7eaf1e42296', fixed_text).
narrative_ontology:cs_authority_grounding('1504037b-308f-4b1b-b6cf-e7eaf1e42296', lineage).
narrative_ontology:cs_interpretation_layer_present('1504037b-308f-4b1b-b6cf-e7eaf1e42296').
narrative_ontology:cs_reading_relation('1504037b-308f-4b1b-b6cf-e7eaf1e42296', us_constitution_interpretive__living_constitution_reading, forecloses).
narrative_ontology:cs_reading_relation('1504037b-308f-4b1b-b6cf-e7eaf1e42296', us_constitution_interpretive__popular_constitutionalism_reading, coexists_with).
narrative_ontology:cs_axiom('1504037b-308f-4b1b-b6cf-e7eaf1e42296', foundational, constitutional_meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(constitutional_meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('1504037b-308f-4b1b-b6cf-e7eaf1e42296', constitutional_meaning_fixed_at_ratification, conventional).
narrative_ontology:cs_axiom('1504037b-308f-4b1b-b6cf-e7eaf1e42296', foundational, judicial_role_fidelity_to_original_meaning).
narrative_ontology:cs_axiom_status(judicial_role_fidelity_to_original_meaning, holdable).
narrative_ontology:cs_axiom_grounding('1504037b-308f-4b1b-b6cf-e7eaf1e42296', judicial_role_fidelity_to_original_meaning, deontological).
narrative_ontology:cs_reference_frame('1504037b-308f-4b1b-b6cf-e7eaf1e42296', constitutional_text_as_supreme_law).
narrative_ontology:cs_drift_state('1504037b-308f-4b1b-b6cf-e7eaf1e42296', contemporary_legal_discourse, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('1504037b-308f-4b1b-b6cf-e7eaf1e42296', '').
narrative_ontology:cs_kernel_id(us_constitution_interpretive__originalist_reading, us_constitution_interpretive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, federalism_advocates).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, religious_liberty_claimants_original_understanding).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, property_rights_defenders).
narrative_ontology:constraint_victim(us_constitution_interpretive__originalist_reading, unenumerated_rights_claimants).
narrative_ontology:constraint_victim(us_constitution_interpretive__originalist_reading, federal_regulatory_expansion_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, general_public).
narrative_ontology:constraint_victim(us_constitution_interpretive__originalist_reading, general_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret the US Constitution according to its original public meaning or the framers' intent, shaping legal precedent and policy. They gain legitimacy and power by presenting their interpretations as objective fidelity to the founding document.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, supreme_court_justices_originalist, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from originalist interpretations that tend to limit federal power and preserve state autonomy, aligning with their political and legal philosophy.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, federalism_advocates, beneficiary,
    organized, generational, mobile, national).

% Seek to protect religious practices based on a historical understanding of First Amendment rights, often finding favorable outcomes under originalist jurisprudence.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, religious_liberty_claimants_original_understanding, beneficiary,
    moderate, biographical, constrained, local).

% Benefit from originalist interpretations that strictly protect property rights as understood at the time of the founding, often limiting government regulation.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, property_rights_defenders, beneficiary,
    powerful, biographical, mobile, national).

% Seek recognition of rights not explicitly listed in the Constitution (e.g., privacy, reproductive rights), which are often denied or severely limited by originalist interpretations, imposing significant personal costs.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, unenumerated_rights_claimants, payer,
    powerless, immediate, trapped, local).

% Seek to expand the federal government's power to address modern social and economic problems, often finding their efforts constrained by originalist limits on federal authority and the Commerce Clause.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, federal_regulatory_expansion_advocates, payer,
    organized, biographical, constrained, national).

% Advocate for an evolving constitutional meaning that adapts to contemporary societal values. Their interpretive method is often dismissed or marginalized by originalist courts, limiting their influence on legal outcomes.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, living_constitutionalist_scholars, excluded,
    analytical, biographical, analytical, national).

% Live under the legal framework shaped by originalist interpretations, experiencing both the benefits of perceived stability and limited government, and the costs of denied claims for new rights or federal action on modern issues.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, general_public, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_interpretive__originalist_reading, general_public, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_interpretive__originalist_reading, supreme_court_justices_originalist).
narrative_ontology:fixing_cost_class(us_constitution_interpretive__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, ostensibly objective framework for constitutional interpretation, aiming to limit judicial discretion and ensure fidelity to the founding document, thereby coordinating legal expectations across jurisdictions and over time.
% TRANSFER_FUNCTION: Transfers interpretive authority from evolving societal norms or judicial discretion to historical texts and intentions, effectively transferring power and legal outcomes to those whose interests align with that historical understanding, and away from those seeking contemporary adaptation.
% ABSENT_VOICES: Living constitutionalist scholars and popular constitutionalism advocates are often marginalized in the formal interpretive process, arguing for a more dynamic or democratically responsive Constitution. Unenumerated rights claimants, whose interests are often directly harmed, also lack a direct voice in shaping the interpretive methodology.
% DISAPPEARANCE_RATIONALE: If originalism as a dominant interpretive method vanished overnight, constitutional interpretation would immediately shift, likely towards more flexible or democratically responsive methods. This would lead to significant changes in judicial outcomes, the balance of federal-state power, and the recognition of rights, fundamentally reorganizing the legal and political landscape.
% FOUNDING_PROBLEM: To prevent arbitrary judicial rule, ensure fidelity to the written Constitution, and maintain the separation of powers by limiting judges to applying, not making, law, thereby preserving the democratic legitimacy of the legal system.
% FOUNDING_PROBLEM_CORROBORATION: Originalist proponents (judges, scholars) attest the problem of judicial overreach is still live, citing instances of 'judicial activism.' Critics (legal scholars, civil rights advocates) argue that while judicial overreach is a concern, originalism itself creates new forms of judicial activism by imposing outdated views, and that the problem of adapting an old document to new realities is the more pressing issue. Legislative hearing testimony and academic critiques from outside the benefiting parties support the shifted-function reading.
narrative_ontology:disappearance_verdict(us_constitution_interpretive__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_interpretive__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_interpretive__originalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(us_constitution_interpretive__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_interpretive__originalist_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_interpretive__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_interpretive__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_interpretive__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.7) because originalism, by fixing meaning to a historical moment, can deny claims based on evolving societal norms, imposing significant costs on those seeking broader rights or federal action to address modern problems. Suppression is also high (0.8) due to the hierarchical nature of the judiciary, where originalist interpretations from higher courts bind lower courts, effectively suppressing alternative interpretive methods within the legal system. The theater ratio is moderate-low (0.25); while there is genuine intellectual debate and scholarly rigor in originalist methodology, some applications may be perceived as performative to achieve specific policy outcomes. Accessibility collapse is high (0.75) for those seeking unenumerated rights or federal expansion, as originalism significantly narrows the legal avenues available to them. Resistance is high (0.7) as originalism faces strong academic, political, and social opposition from proponents of other interpretive methods.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of originalist justices and their beneficiaries, this interpretive method provides essential stability, fidelity to the text, and limits on judicial activism, functioning as a legitimate framework. However, from the perspective of victims (e.g., unenumerated rights claimants) and excluded voices (e.g., living constitutionalist scholars), the same structure operates as an extractive and suppressive force, imposing outdated views and denying legitimate claims for adaptation to modern society. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Originalist Supreme Court justices act as agenda-setters, wielding institutional power to enforce this interpretive method. Beneficiaries include federalism advocates, religious liberty claimants, and property rights defenders, whose interests often align with originalist outcomes. Victims are primarily unenumerated rights claimants and federal regulatory expansion advocates, who bear the costs of constrained legal avenues. The general public experiences both diffuse benefits (e.g., perceived stability) and costs (e.g., denied rights). Living constitutionalist scholars are structurally excluded from the dominant interpretive discourse.
 *
 * MANDATROPHY ANALYSIS:
 *   Originalism's mandate is to prevent judicial overreach and ensure fidelity to the Constitution. While this problem remains 'contested' (as per the six questions), the high extractiveness and suppression suggest that the method, in practice, may be generating significant costs for certain groups beyond what is necessary for its stated coordination function. The persistence of strong resistance and the 'contested' status of the founding problem indicate that the constraint's function is not universally accepted as benign coordination, preventing its mislabeling as a simple 'rope' or 'mountain.'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint truly a distinct ''originalist_reading'' or merely a variant of another interpretive method?',
    'Analysis of core tenets and judicial application: if its foundational axioms and interpretive outcomes consistently diverge from other methods, it is distinct.',
    'If not distinct, its classification would merge with the dominant interpretive method it most closely resembles, potentially altering its extractiveness and suppression metrics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Confirms the distinct identity of the originalist reading within the interpretive kernel.').

omega_variable(
    original_meaning_empirical_basis,
    'To what extent is ''original public meaning'' an empirically discoverable fact versus a constructed interpretive choice?',
    'Historical linguistic analysis and textual scholarship: if a single, clear original public meaning is consistently discoverable across diverse texts and contexts, it''s more empirical. If it requires significant interpretive choices and assumptions, it''s more constructed.',
    'If more constructed, the ''naturalness'' claim of originalism weakens, potentially increasing its perceived extractiveness and theater ratio as a chosen method rather than a discovered truth.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_meaning_empirical_basis, empirical, 'Ambiguity of ''original public meaning'' as an empirical referent.').

omega_variable(
    judicial_discretion_under_originalism,
    'Does originalism genuinely limit judicial discretion, or does it merely shift the locus of discretion to historical interpretation?',
    'Comparative case study of judicial decisions under originalist vs. non-originalist methods: if originalist decisions show less variance or more predictable outcomes, it limits discretion. If it introduces new forms of historical judgment, discretion is merely relocated.',
    'If discretion is merely relocated, the claim of originalism as a ''restraint'' on judges weakens, potentially increasing its perceived theater ratio and extractiveness as a tool for policy outcomes rather than pure fidelity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_discretion_under_originalism, conceptual, 'Whether originalism truly limits judicial discretion or merely re-channels it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_interpretive__originalist_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1970, us_constitution_interpretive__originalist_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(us_c_tr_t1985, us_constitution_interpretive__originalist_reading, theater_ratio, 1985, 0.15).
narrative_ontology:measurement(us_c_tr_t2000, us_constitution_interpretive__originalist_reading, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(us_c_tr_t2010, us_constitution_interpretive__originalist_reading, theater_ratio, 2010, 0.23).
narrative_ontology:measurement(us_c_tr_t2025, us_constitution_interpretive__originalist_reading, theater_ratio, 2025, 0.25).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1970, us_constitution_interpretive__originalist_reading, base_extractiveness, 1970, 0.4).
narrative_ontology:measurement(us_c_be_t1985, us_constitution_interpretive__originalist_reading, base_extractiveness, 1985, 0.55).
narrative_ontology:measurement(us_c_be_t2000, us_constitution_interpretive__originalist_reading, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(us_c_be_t2010, us_constitution_interpretive__originalist_reading, base_extractiveness, 2010, 0.68).
narrative_ontology:measurement(us_c_be_t2025, us_constitution_interpretive__originalist_reading, base_extractiveness, 2025, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1970, us_constitution_interpretive__originalist_reading, suppression_requirement, 1970, 0.5).
narrative_ontology:measurement(us_c_su_t1985, us_constitution_interpretive__originalist_reading, suppression_requirement, 1985, 0.65).
narrative_ontology:measurement(us_c_su_t2000, us_constitution_interpretive__originalist_reading, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement(us_c_su_t2010, us_constitution_interpretive__originalist_reading, suppression_requirement, 2010, 0.78).
narrative_ontology:measurement(us_c_su_t2025, us_constitution_interpretive__originalist_reading, suppression_requirement, 2025, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_interpretive__originalist_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
