% ============================================================================
% CONSTRAINT STORY: constitutional_interpretive_authority__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_interpretive_authority__judicial_supremacy_reading, []).

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
 *   constraint_id: constitutional_interpretive_authority__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy in Constitutional Interpretation
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   This constraint instantiates the judicial_supremacy_reading of the
 *   constitutional_interpretive_authority kernel. Under this reading, apex
 *   courts possess final and binding interpretive authority over
 *   constitutional text, grounded in a lineage of precedent stretching to
 *   Marbury v. Madison. Legislative acts found incompatible with judicially
 *   interpreted constitutional meaning are nullified. The judiciary is the
 *   structural beneficiary of this authority accumulation, while legislative
 *   bodies bear the cost of subordination. The constraint coordinates
 *   constitutional coherence and minority rights protection but extracts
 *   democratic authority from the legislative branch, legitimating coercion
 *   through rights-compliance rather than majoritarian will.
 *
 * KEY AGENTS:
 *   - apex_judiciary (agenda_setter/beneficiary, institutional/identity_locked) â accumulates interpretive authority and institutional purpose
 *   - legislative_bodies (payer, institutional/constrained) â enacts law subject to nullification
 *   - rights_litigants (beneficiary, moderate/mobile) â access rights-adjudication forum
 *   - electorate (excluded, organized/constrained) â majoritarian preferences overridden
 *   - constitutional_scholars (observer, analytical) â analyze structural tension
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_interpretive_authority__judicial_supremacy_reading, 0.62).
domain_priors:suppression_score(constitutional_interpretive_authority__judicial_supremacy_reading, 0.58).
domain_priors:theater_ratio(constitutional_interpretive_authority__judicial_supremacy_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_interpretive_authority__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_interpretive_authority__judicial_supremacy_reading, "Judicial Supremacy in Constitutional Interpretation").
narrative_ontology:topic_domain(constitutional_interpretive_authority__judicial_supremacy_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(constitutional_interpretive_authority__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_interpretive_authority__judicial_supremacy_reading, '55529625-a65f-4336-80c5-1d44582dbe0e').
narrative_ontology:cs_kernel_codification('55529625-a65f-4336-80c5-1d44582dbe0e', fixed_text).
narrative_ontology:cs_authority_grounding('55529625-a65f-4336-80c5-1d44582dbe0e', lineage).
narrative_ontology:cs_interpretation_layer_present('55529625-a65f-4336-80c5-1d44582dbe0e').
narrative_ontology:cs_reading_relation('55529625-a65f-4336-80c5-1d44582dbe0e', constitutional_interpretive_authority__parliamentary_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('55529625-a65f-4336-80c5-1d44582dbe0e', constitutional_interpretive_authority__coordinate_construction_reading, forecloses).
narrative_ontology:cs_axiom('55529625-a65f-4336-80c5-1d44582dbe0e', foundational, judicial_final_interpretive_authority).
narrative_ontology:cs_axiom_status(judicial_final_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('55529625-a65f-4336-80c5-1d44582dbe0e', judicial_final_interpretive_authority, conventional).
narrative_ontology:cs_axiom('55529625-a65f-4336-80c5-1d44582dbe0e', foundational, rights_compliance_supersedes_majoritarian_will).
narrative_ontology:cs_axiom_status(rights_compliance_supersedes_majoritarian_will, holdable).
narrative_ontology:cs_axiom_grounding('55529625-a65f-4336-80c5-1d44582dbe0e', rights_compliance_supersedes_majoritarian_will, deontological).
narrative_ontology:cs_reference_frame('55529625-a65f-4336-80c5-1d44582dbe0e', judicial_finality_framework).
narrative_ontology:cs_drift_state('55529625-a65f-4336-80c5-1d44582dbe0e', contemporary_polarization_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('55529625-a65f-4336-80c5-1d44582dbe0e', '').
narrative_ontology:cs_kernel_id(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__judicial_supremacy_reading, apex_judiciary).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__judicial_supremacy_reading, rights_litigants).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__judicial_supremacy_reading, legislative_bodies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercises final interpretive authority over constitutional meaning, reviews and nullifies legislative acts, and derives institutional prestige, agenda-setting power, and role-defining purpose from this authority. The judiciary cannot exit the role of constitutional guardian without existential damage to its institutional identity.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, apex_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__judicial_supremacy_reading, apex_judiciary, beneficiary).

% Enacts legislation subject to judicial nullification. Bears the cost of having democratic enactments overturned and policy preferences subordinated to judicial interpretation. Exit via jurisdiction stripping or constitutional amendment is theoretically available but politically prohibitive and institutionally constrained.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, legislative_bodies, payer,
    institutional, biographical, constrained, national).

% Invoke constitutional rights claims before apex courts. Benefit from a privileged forum where judicial authority can nullify legislation they oppose. Their exit is mobileâthey can choose whether to litigateâbut their success depends on the judiciary's retained authority.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, rights_litigants, beneficiary,
    moderate, biographical, mobile, national).

% Majoritarian policy preferences are enacted through legislative bodies but can be overridden by judicial interpretation. They lack direct voice in constitutional interpretation and are structurally excluded from the interpretive process despite being the source of democratic mandate.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, electorate, excluded,
    organized, biographical, constrained, national).

% Analyze and critique the allocation of interpretive authority. They observe the structural tension between democratic self-governance and judicial guardianship without occupying a seat that directly pays or collects from the constraint.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_interpretive_authority__judicial_supremacy_reading, apex_judiciary).
narrative_ontology:fixing_cost_class(constitutional_interpretive_authority__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a final, uniform arbiter for constitutional meaning to prevent inter-branch conflict and ensure coherent rights protection across jurisdictions and over time.
% TRANSFER_FUNCTION: Moves interpretive authority and policy veto power from legislative assemblies and democratic majorities to apex courts; moves constitutional legitimacy from popular enactment to judicial articulation.
% ABSENT_VOICES: Legislative supremacists and popular constitutionalists who would locate final interpretive authority in elected assemblies or the people directly; coordinate construction advocates who reject finality in any single branch.
% DISAPPEARANCE_RATIONALE: If judicial supremacy disappeared, legislatures would regain final constitutional interpretive authority, the architecture of constitutional litigation would collapse, rights claimants would lose their privileged nullification forum, and constitutional meaning would revert to political contestation rather than adjudication.
% FOUNDING_PROBLEM: How to maintain constitutional coherence and protect fundamental rights against transient majoritarian pressure and legislative overreach without collapsing into tyranny.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians and framers attest the original concern with majoritarian excess; comparative constitutionalists note similar designs elsewhere. Legislative historians outside the judiciary corroborate the democratic legitimacy problem, while critical legal scholars argue the judiciary has substituted its own power accumulation for the original rights-guardianship function.
narrative_ontology:disappearance_verdict(constitutional_interpretive_authority__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_interpretive_authority__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_interpretive_authority__judicial_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_interpretive_authority__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_interpretive_authority__judicial_supremacy_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_interpretive_authority__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_interpretive_authority__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_interpretive_authority__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the substantial authority transfer from democratic branches to the judiciary; the legislature's policy autonomy is extracted through nullification. Suppression (0.58) captures the active suppression of legislative self-interpretation and popular constitutionalism. Theater_ratio (0.45) registers the growing performative dimension of constitutional interpretationâelaborate doctrinal tests, originalist methodologies, and public ceremonies that maintain authority independently of outcomes. Accessibility_collapse (0.75) is high because once judicial supremacy is accepted as the constitutional background, alternatives (legislative override, departmentalism) become nearly unthinkable within the legal order. Resistance (0.45) reflects persistent political threats to jurisdiction, court packing, and populist backlash. The measurement series show extraction and theater rising from the founding through the twentieth century, plateauing as the constraint matured.
 *
 * PERSPECTIVAL GAP:
 *   From the apex_judiciary seat, the constraint appears as necessary guardianship against majoritarian excess; the computed classification will emphasize coordination. From the legislative_bodies seat, it reads as democratic subordination; the computed classification will emphasize extraction. From the rights_litigants seat, it is protective infrastructure with manageable exit costs. The engine derives this divergence from the same structural data rather than from competing narratives.
 *
 * DIRECTIONALITY LOGIC:
 *   The apex_judiciary is declared as agenda_setter with secondary_role beneficiary and identity_locked exit: structurally it subsidizes this agent (low d). Legislative_bodies is declared payer with constrained exit: structurally targeted (high d). Rights_litigants are beneficiaries with mobile exit: also subsidized (low d). The electorate is excluded and constrained: their preferences are overridden but they are not formal parties to the constraint, registering as high d if seated. Constitutional_scholars are analytical: neutral d.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents mislabeling: the constraint is not a pure rope because the judiciary captures asymmetric authority (legislature is victimized), and not a pure snare because there is a genuine coordination function in resolving constitutional conflict and articulating rights. If the rights-guardianship function were shown to be entirely performative, the constraint would degrade toward piton or snare; if the authority transfer were shown to be fully consensual, it would approach rope. Neither extreme is supported by the current structural data.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_supremacy_necessity,
    'Is final judicial authority structurally necessary for constitutional coherence and minority rights protection, or could coordinate inter-branch dialogue achieve equivalent stability?',
    'Comparative constitutional analysis of regimes with weak-form review or legislative override mechanisms; measurement of rights outcomes and constitutional stability in those regimes relative to judicial-supremacy systems.',
    'If coordinate construction proves functionally equivalent, the extraction of authority to the judiciary is unnecessary overhead and the constraint leans toward snare; if final judicial authority is uniquely stabilizing, the coordination function is genuine and the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_supremacy_necessity, empirical, 'Whether judicial finality is necessary for constitutional coordination').

omega_variable(
    kernel_reading_position,
    'How would the structural classification change if the same kernel were read through parliamentary_supremacy_reading or coordinate_construction_reading?',
    'Cross-reading comparison: the parliamentary reading would invert beneficiary and victim sets (legislature beneficiary, judiciary payer), while the coordinate reading would diffuse directionality across branches.',
    'This story''s epsilon and classification are invariant for this reading only; sibling readings instantiate different constraints with different structural data, validating the epsilon-invariance principle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Position of this reading within the constitutional_interpretive_authority kernel').

omega_variable(
    rights_guardianship_authenticity,
    'Does the judiciary''s rights-guardianship function genuinely protect disempowered minorities, or does it primarily serve as legitimation for judicial institutional power accumulation?',
    'Empirical tracking of judicial dockets: proportion of rights decisions favoring disadvantaged groups versus institutional or elite interests; comparison with legislative track record on same rights domains.',
    'If guardianship is authentic, the coordination function is robust; if power accumulation dominates, the theater_ratio rises and the constraint drifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rights_guardianship_authenticity, empirical, 'Authenticity of the rights-guardianship justification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_interpretive_authority__judicial_supremacy_reading, 0, 220).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ciau_jsr_tr_t0, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ciau_jsr_tr_t36, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 36, 0.28).
narrative_ontology:measurement(ciau_jsr_tr_t72, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 72, 0.35).
narrative_ontology:measurement(ciau_jsr_tr_t108, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 108, 0.4).
narrative_ontology:measurement(ciau_jsr_tr_t144, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 144, 0.43).
narrative_ontology:measurement(ciau_jsr_tr_t180, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 180, 0.45).
narrative_ontology:measurement(ciau_jsr_tr_t220, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 220, 0.45).

% Extraction over time
narrative_ontology:measurement(ciau_jsr_be_t0, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(ciau_jsr_be_t36, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 36, 0.42).
narrative_ontology:measurement(ciau_jsr_be_t72, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 72, 0.52).
narrative_ontology:measurement(ciau_jsr_be_t108, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 108, 0.58).
narrative_ontology:measurement(ciau_jsr_be_t144, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 144, 0.62).
narrative_ontology:measurement(ciau_jsr_be_t180, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 180, 0.64).
narrative_ontology:measurement(ciau_jsr_be_t220, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 220, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(ciau_jsr_su_t0, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(ciau_jsr_su_t36, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 36, 0.38).
narrative_ontology:measurement(ciau_jsr_su_t72, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 72, 0.48).
narrative_ontology:measurement(ciau_jsr_su_t108, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 108, 0.54).
narrative_ontology:measurement(ciau_jsr_su_t144, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 144, 0.57).
narrative_ontology:measurement(ciau_jsr_su_t180, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 180, 0.59).
narrative_ontology:measurement(ciau_jsr_su_t220, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 220, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_interpretive_authority__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__judicial_supremacy_reading, parliamentary_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__judicial_supremacy_reading, coordinate_construction_reading).

% DUAL FORMULATION NOTE:
% This constraint is the judicial_supremacy_reading of the constitutional_interpretive_authority kernel. It is structurally paired with parliamentary_supremacy_reading and coordinate_construction_reading as sibling instantiations of the same contested kernel. Each reading emits a distinct constraint with its own epsilon, beneficiary/victim structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
