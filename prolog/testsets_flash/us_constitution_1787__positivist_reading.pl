% ============================================================================
% CONSTRAINT STORY: us_constitution_1787__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_1787__positivist_reading, []).

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
 *   constraint_id: us_constitution_1787__positivist_reading
 *   human_readable: US Constitution (Positivist Reading)
 *   domain: constitutional_law/legal_theory/political_philosophy
 *
 * SUMMARY:
 *   This constraint represents a positivist reading of the US Constitution,
 *   where its meaning is derived strictly from the text itself and formal
 *   amendments, with judicial interpretation constrained to these explicit
 *   sources. This reading emphasizes democratic control over constitutional
 *   change through the amendment process and limits judicial activism. It is
 *   one of three competing readings of the 'us_constitution_1787' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_1787__positivist_reading, 0.35).
domain_priors:suppression_score(us_constitution_1787__positivist_reading, 0.45).
domain_priors:theater_ratio(us_constitution_1787__positivist_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_1787__positivist_reading, rope).
narrative_ontology:human_readable(us_constitution_1787__positivist_reading, "US Constitution (Positivist Reading)").
narrative_ontology:topic_domain(us_constitution_1787__positivist_reading, "constitutional_law/legal_theory/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_1787__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_1787__positivist_reading, '0c66d611-2019-4e71-b4c4-1bc5fe8453d2').
narrative_ontology:cs_kernel_codification('0c66d611-2019-4e71-b4c4-1bc5fe8453d2', fixed_text).
narrative_ontology:cs_authority_grounding('0c66d611-2019-4e71-b4c4-1bc5fe8453d2', lineage).
narrative_ontology:cs_interpretation_layer_present('0c66d611-2019-4e71-b4c4-1bc5fe8453d2').
narrative_ontology:cs_reading_relation('0c66d611-2019-4e71-b4c4-1bc5fe8453d2', us_constitution_1787__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('0c66d611-2019-4e71-b4c4-1bc5fe8453d2', us_constitution_1787__living_reading, forecloses).
narrative_ontology:cs_axiom('0c66d611-2019-4e71-b4c4-1bc5fe8453d2', foundational, constitutional_text_is_supreme_and_sufficient).
narrative_ontology:cs_axiom_status(constitutional_text_is_supreme_and_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('0c66d611-2019-4e71-b4c4-1bc5fe8453d2', constitutional_text_is_supreme_and_sufficient, conventional).
narrative_ontology:cs_axiom('0c66d611-2019-4e71-b4c4-1bc5fe8453d2', foundational, amendment_is_primary_mode_of_change).
narrative_ontology:cs_axiom_status(amendment_is_primary_mode_of_change, holdable).
narrative_ontology:cs_axiom_grounding('0c66d611-2019-4e71-b4c4-1bc5fe8453d2', amendment_is_primary_mode_of_change, conventional).
narrative_ontology:cs_reference_frame('0c66d611-2019-4e71-b4c4-1bc5fe8453d2', textual_supremacy_framework).
narrative_ontology:cs_drift_state('0c66d611-2019-4e71-b4c4-1bc5fe8453d2', contemporary, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('0c66d611-2019-4e71-b4c4-1bc5fe8453d2', '').
narrative_ontology:cs_kernel_id(us_constitution_1787__positivist_reading, us_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, legislative_branch).
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, electorate).
narrative_ontology:constraint_victim(us_constitution_1787__positivist_reading, judicial_activists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from a clear mandate to shape law through democratic processes and constitutional amendment, without undue judicial interference. Its power is enhanced by the constraint on judicial interpretation.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, legislative_branch, beneficiary,
    institutional, generational, constrained, national).

% Benefits from the clarity that constitutional change primarily occurs through the amendment process, reflecting popular will, rather than evolving judicial interpretations. This reinforces democratic accountability.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, electorate, beneficiary,
    organized, generational, mobile, national).

% Administers the constraint by interpreting the Constitution, but is itself constrained to the text and formal amendments. Its legitimacy is tied to this textual fidelity, limiting its ability to 'make' law.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, judicial_branch, agenda_setter,
    institutional, civilizational, identity_locked, national).

% Bear the cost of this constraint as their preferred mode of constitutional interpretation (finding evolving meaning beyond the text) is explicitly limited. Their professional identity is challenged by strict textualism.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, judicial_activists, payer,
    powerful, biographical, identity_locked, national).

% Would argue that the positivist reading does not go far enough in constraining judicial power, as it allows for interpretation of the text without strict adherence to original intent. They are excluded from the full scope of their preferred interpretive method.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, originalists, excluded,
    powerful, generational, constrained, national).

% Would argue that the positivist reading is too rigid and fails to allow the Constitution to adapt to modern societal needs. Their interpretive approach is directly suppressed by this constraint.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, living_constitutionalists, excluded,
    powerful, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the process of constitutional change and interpretation, ensuring that the fundamental law remains stable yet amendable through a clear, democratic process, preventing arbitrary judicial revision.
% TRANSFER_FUNCTION: Transfers interpretive authority from unelected judges (beyond the text) to the democratically elected legislative branch and the amendment process, thereby transferring power to the electorate.
% ABSENT_VOICES: Both originalists (who would demand stricter historical adherence) and living constitutionalists (who would demand more interpretive flexibility) are structurally excluded from the full realization of their preferred interpretive methods within this framework. Their arguments are heard in public discourse but not fully integrated into the constraint's operation.
% DISAPPEARANCE_RATIONALE: If this positivist reading vanished, judicial interpretation would likely become unmoored from the text, leading to a more fluid and potentially unpredictable constitutional landscape. The legislative branch's role in constitutional change would diminish, and the electorate's direct influence through amendments would be diluted, fundamentally altering the balance of power.
% FOUNDING_PROBLEM: To establish a stable, supreme law that could adapt to future needs while preventing arbitrary rule by any single branch, particularly an unelected judiciary.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, political scientists, and public opinion polls consistently show ongoing concern about judicial overreach and the need for clear mechanisms of constitutional change. This corroboration comes from outside the immediate judicial beneficiaries, reflecting a broad societal interest in the balance of power.
narrative_ontology:disappearance_verdict(us_constitution_1787__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_1787__positivist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_1787__positivist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(us_constitution_1787__positivist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_1787__positivist_reading_tests).
:- end_tests(us_constitution_1787__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.35) is moderate, as it primarily extracts from judicial actors who prefer broader interpretive authority, rather than from the general populace. Suppression (0.45) is also moderate, reflecting the ongoing debate and occasional resistance from those advocating for more expansive judicial roles. Theater ratio (0.20) is low, as the commitment to textualism is generally genuine, though some performative adherence may exist in practice. The metrics reflect a constraint that is generally accepted but faces persistent, though not overwhelming, challenge.
 *
 * PERSPECTIVAL GAP:
 *   The legislative branch and the electorate experience this as a beneficial 'rope' that secures democratic control over fundamental law. The judicial branch, while administering the constraint, experiences it as a 'tangled rope' due to the internal tension between its interpretive role and the textual limitations. Judicial activists, in particular, experience it as a 'snare' due to the direct suppression of their preferred interpretive methods.
 *
 * DIRECTIONALITY LOGIC:
 *   The legislative branch and the electorate are clear beneficiaries (d near 0.0) as their power to shape law is enhanced. The judicial branch, as the primary interpreter, has a more symmetric relationship (d near 0.5), balancing its authority with its textual constraints. Judicial activists are targets (d near 1.0) as their interpretive freedom is directly curtailed. Originalists and living constitutionalists are excluded, meaning their preferred directionalities are not fully realized within this framework.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mandatrophy by ensuring the Constitution's meaning remains tied to explicit, democratically sanctioned text, rather than allowing it to drift into an unmoored 'living' document or an anachronistic 'originalist' relic. The constraint's function (democratic control over fundamental law) remains live, and its persistence is tied to ongoing political and legal debates, not mere inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_ambiguity_resolution,
    'How are ambiguities or silences in the constitutional text resolved under a strictly positivist reading without recourse to extra-textual sources (e.g., original intent or evolving societal norms)?',
    'Analysis of judicial decisions in hard cases: do judges implicitly or explicitly draw on non-textual sources, or do they defer to legislative action in such instances?',
    'If non-textual sources are consistently used, the ''positivist_reading'' is less strictly textual than claimed, potentially shifting its classification towards a ''tangled_rope'' for the judiciary. If deference is consistent, the constraint on the judiciary is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_ambiguity_resolution, empirical, 'The practical limits of textualism in resolving constitutional ambiguities.').

omega_variable(
    democratic_legitimacy_vs_minority_rights,
    'Does a strict positivist reading, by prioritizing democratic amendment over judicial interpretation, adequately protect minority rights from majoritarian oppression, or does it create a ''snare'' for vulnerable groups?',
    'Comparative analysis of rights protection outcomes under different interpretive regimes, particularly in cases where majoritarian sentiment conflicts with minority interests.',
    'If minority rights are consistently undermined, the ''positivist_reading'' could be reclassified as a ''snare'' for minority groups, despite its ''rope'' classification for the general electorate. If protections remain robust, the ''rope'' classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_legitimacy_vs_minority_rights, preference, 'The balance between democratic legitimacy and minority rights protection under textualism.').

omega_variable(
    positivist_vs_originalist_distinction,
    'Is the ''positivist_reading'' truly distinct from the ''originalist_reading'', or does a strict textual interpretation inevitably lead to an originalist outcome in practice?',
    'Detailed comparison of judicial outcomes and reasoning in cases where textual meaning is ambiguous but original intent is clear. Do positivist judges explicitly reject original intent when it diverges from plain text?',
    'If the two readings consistently converge in practice, the ''positivist_reading'' may be a ''tangled_rope'' that implicitly leverages originalist authority while claiming textual neutrality. If they diverge, the distinction is robust.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(positivist_vs_originalist_distinction, conceptual, 'Conceptual overlap between textualism and originalism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_1787__positivist_reading, 1787, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1787, us_constitution_1787__positivist_reading, theater_ratio, 1787, 0.1).
narrative_ontology:measurement(us_c_tr_t1850, us_constitution_1787__positivist_reading, theater_ratio, 1850, 0.12).
narrative_ontology:measurement(us_c_tr_t1900, us_constitution_1787__positivist_reading, theater_ratio, 1900, 0.15).
narrative_ontology:measurement(us_c_tr_t1950, us_constitution_1787__positivist_reading, theater_ratio, 1950, 0.18).
narrative_ontology:measurement(us_c_tr_t2000, us_constitution_1787__positivist_reading, theater_ratio, 2000, 0.19).
narrative_ontology:measurement(us_c_tr_t2024, us_constitution_1787__positivist_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1787, us_constitution_1787__positivist_reading, base_extractiveness, 1787, 0.2).
narrative_ontology:measurement(us_c_be_t1850, us_constitution_1787__positivist_reading, base_extractiveness, 1850, 0.25).
narrative_ontology:measurement(us_c_be_t1900, us_constitution_1787__positivist_reading, base_extractiveness, 1900, 0.3).
narrative_ontology:measurement(us_c_be_t1950, us_constitution_1787__positivist_reading, base_extractiveness, 1950, 0.32).
narrative_ontology:measurement(us_c_be_t2000, us_constitution_1787__positivist_reading, base_extractiveness, 2000, 0.34).
narrative_ontology:measurement(us_c_be_t2024, us_constitution_1787__positivist_reading, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1787, us_constitution_1787__positivist_reading, suppression_requirement, 1787, 0.3).
narrative_ontology:measurement(us_c_su_t1850, us_constitution_1787__positivist_reading, suppression_requirement, 1850, 0.35).
narrative_ontology:measurement(us_c_su_t1900, us_constitution_1787__positivist_reading, suppression_requirement, 1900, 0.4).
narrative_ontology:measurement(us_c_su_t1950, us_constitution_1787__positivist_reading, suppression_requirement, 1950, 0.42).
narrative_ontology:measurement(us_c_su_t2000, us_constitution_1787__positivist_reading, suppression_requirement, 2000, 0.44).
narrative_ontology:measurement(us_c_su_t2024, us_constitution_1787__positivist_reading, suppression_requirement, 2024, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_1787__positivist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_1787__positivist_reading, us_constitution_1787__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_1787__positivist_reading, us_constitution_1787__living_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the US Constitution (1787) kernel. Each reading has a different structural relationship to the text, different beneficiaries/victims, and thus a different effective extraction and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
