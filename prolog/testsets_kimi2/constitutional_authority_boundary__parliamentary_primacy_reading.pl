% ============================================================================
% CONSTRAINT STORY: constitutional_authority_boundary__parliamentary_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_authority_boundary__parliamentary_primacy_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: constitutional_authority_boundary__parliamentary_primacy_reading
 *   human_readable: Parliamentary Sovereignty over Constitutional Text (Parliamentary Primacy Reading)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the parliamentary_primacy_reading of the
 *   constitutional_authority_boundary kernel. It holds that the
 *   constitutional text, where it exists, is subordinate to parliamentary
 *   sovereignty, and the elected legislature retains final authority to
 *   define constitutional meaning through ordinary or entrenched legislation.
 *   This reading is one of three contested readings of the same kernel,
 *   alongside judicial_supremacy_reading and coordinate_construction_reading.
 *   The constraint transfers interpretive finality from courts and entrenched
 *   texts to the legislative chamber, solving the coordination problem of
 *   constitutional deadlock at some cost to judicial independence and
 *   minority protections. The authored metrics (low extractiveness, moderate
 *   suppression) are independent of the claimed type (tangled_rope).
 *
 * KEY AGENTS:
 *   - elected_legislature: Agenda-setter (institutional/arbitrage) â sets and administers the rule of parliamentary sovereignty
 *   - parliamentary_majorities: Primary beneficiary (powerful/mobile) â collects unconstrained legislative authority
 *   - judiciary: Primary payer (institutional/constrained) â bears subordination of constitutional review power
 *   - constitutional_minorities: Secondary payer (powerless/trapped) â bears exposure to majority legislation without judicial recourse
 *   - democratic_electorate: Secondary beneficiary (organized/constrained) â benefits from direct electoral accountability
 *   - public_law_scholars: Analytical observer (moderate/analytical) â frames debate without institutional power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_authority_boundary__parliamentary_primacy_reading, 0.22).
domain_priors:suppression_score(constitutional_authority_boundary__parliamentary_primacy_reading, 0.45).
domain_priors:theater_ratio(constitutional_authority_boundary__parliamentary_primacy_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_authority_boundary__parliamentary_primacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_authority_boundary__parliamentary_primacy_reading, "Parliamentary Sovereignty over Constitutional Text (Parliamentary Primacy Reading)").
narrative_ontology:topic_domain(constitutional_authority_boundary__parliamentary_primacy_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(constitutional_authority_boundary__parliamentary_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_authority_boundary__parliamentary_primacy_reading, '86b1e6c0-7750-4c2e-adda-5dc71fb0ac02').
narrative_ontology:cs_kernel_codification('86b1e6c0-7750-4c2e-adda-5dc71fb0ac02', formalized).
narrative_ontology:cs_authority_grounding('86b1e6c0-7750-4c2e-adda-5dc71fb0ac02', lineage).
narrative_ontology:cs_interpretation_layer_present('86b1e6c0-7750-4c2e-adda-5dc71fb0ac02').
narrative_ontology:cs_reading_relation('86b1e6c0-7750-4c2e-adda-5dc71fb0ac02', constitutional_authority_boundary__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('86b1e6c0-7750-4c2e-adda-5dc71fb0ac02', constitutional_authority_boundary__coordinate_construction_reading, forecloses).
narrative_ontology:cs_axiom('86b1e6c0-7750-4c2e-adda-5dc71fb0ac02', foundational, democratic_sovereignty_of_elected_body).
narrative_ontology:cs_axiom_status(democratic_sovereignty_of_elected_body, holdable).
narrative_ontology:cs_axiom_grounding('86b1e6c0-7750-4c2e-adda-5dc71fb0ac02', democratic_sovereignty_of_elected_body, deontological).
narrative_ontology:cs_axiom('86b1e6c0-7750-4c2e-adda-5dc71fb0ac02', foundational, constitutional_amendment_by_ordinary_act).
narrative_ontology:cs_axiom_status(constitutional_amendment_by_ordinary_act, holdable).
narrative_ontology:cs_axiom_grounding('86b1e6c0-7750-4c2e-adda-5dc71fb0ac02', constitutional_amendment_by_ordinary_act, conventional).
narrative_ontology:cs_reference_frame('86b1e6c0-7750-4c2e-adda-5dc71fb0ac02', westminster_parliamentary_tradition).
narrative_ontology:cs_drift_state('86b1e6c0-7750-4c2e-adda-5dc71fb0ac02', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('86b1e6c0-7750-4c2e-adda-5dc71fb0ac02', '').
narrative_ontology:cs_kernel_id(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_authority_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__parliamentary_primacy_reading, parliamentary_majorities).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__parliamentary_primacy_reading, democratic_electorate).
narrative_ontology:constraint_victim(constitutional_authority_boundary__parliamentary_primacy_reading, judiciary).
narrative_ontology:constraint_victim(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_minorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possesses formal authority to define constitutional meaning through ordinary or entrenched legislation. Can override or modify any constitutional text by legislative act. Cannot be bound by past Parliaments. Exiting the constraint would require self-denial of sovereignty through constitutional entrenchment, which is structurally possible but politically rare.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, elected_legislature, agenda_setter,
    institutional, generational, arbitrage, national).

% Control legislative output and benefit from unconstrained ability to implement policy agendas without judicial veto. Collect the power to define constitutional meaning for the duration of their majority. Exit means losing an election, after which they may favor constraints on the new majority.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, parliamentary_majorities, beneficiary,
    powerful, biographical, mobile, national).

% Hears constitutional questions but lacks authority to invalidate primary legislation. Constrained to interpret statutes compatibly with constitutional texts or to issue non-binding declarations. Career and institutional identity bound to the legal tradition, making exit to another legal order difficult.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, judiciary, payer,
    institutional, generational, constrained, national).

% Groups whose rights or interests are vulnerable to legislative majorities. Lack judicial forum for striking down rights-restricting laws. Depend on legislative grace or political mobilization for protection. Geographic or identity-bound, making exit from the national jurisdiction costly.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_minorities, payer,
    powerless, generational, trapped, national).

% Benefits from the accountability mechanism that ties constitutional change directly to electoral outcomes. However, individual voters have limited leverage over specific constitutional interpretations and cannot easily exit the majoritarian system.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, democratic_electorate, beneficiary,
    organized, biographical, constrained, national).

% Analyze and debate the scope of parliamentary sovereignty without institutional power to alter it. Provide the conceptual framework through which the constraint is justified and criticized.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, public_law_scholars, observer,
    moderate, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_authority_boundary__parliamentary_primacy_reading, parliamentary_majorities).
narrative_ontology:fixing_cost_class(constitutional_authority_boundary__parliamentary_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Determines final constitutional authority in a system with multiple institutional actors, preventing deadlock between branches and ensuring democratic accountability by lodging ultimate interpretive power in the elected legislature.
% TRANSFER_FUNCTION: Moves final constitutional interpretive authority from courts and entrenched constitutional texts to the elected legislature; moves risk of minority rights erosion from legislative majorities to judicial minorities and constitutionally-protected groups.
% ABSENT_VOICES: Constitutional minorities lacking legislative leverage, future generations bound by present majorities, and jurists committed to strong-form judicial review are structurally sidelined; their positions are heard in academic discourse but not in legislative chambers where constitutional meaning is finally fixed.
% DISAPPEARANCE_RATIONALE: If parliamentary sovereignty vanished overnight, courts would gain or assert strike-down authority, constitutional texts would acquire entrenched status, legislative majorities would face new procedural constraints, and the locus of constitutional finality would shift â the institutional landscape would reorganize around judicial or coordinate models.
% FOUNDING_PROBLEM: The problem of constitutional finality in a democracy: who has the ultimate authority to resolve constitutional disputes without collapsing into either judicial oligarchy or anarchic inter-branch conflict?
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians and democratic theorists outside the legislative majority (e.g., Diceyan scholars in the UK tradition, comparative constitutionalists) attest to the founding problem of inter-branch deadlock. However, civil liberties organizations and judicial independence advocates attest that the problem has shifted: the current threat is legislative overreach, not judicial tyranny, and the arrangement persists as majority empowerment rather than deadlock prevention.
narrative_ontology:disappearance_verdict(constitutional_authority_boundary__parliamentary_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_authority_boundary__parliamentary_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_authority_boundary__parliamentary_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_authority_boundary__parliamentary_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_authority_boundary__parliamentary_primacy_reading, 0.22, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_authority_boundary__parliamentary_primacy_reading_tests).
:- end_tests(constitutional_authority_boundary__parliamentary_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.22) because the constraint transfers authority rather than material resources; its primary operation is not rent extraction. Suppression is moderate (0.45) because the constraint must actively suppress the live alternative of strong-form judicial review, which remains institutionally attractive to courts and rights advocates. Theater ratio is low (0.20) because parliamentary sovereignty is largely functional, though ritualized assertions of supremacy increasingly exceed operational reality as judicial power grows. Accessibility collapse is moderate-high (0.60) because entrenched constitutionalism is conceptually available but institutionally blocked. Resistance is moderate (0.55) because judicial actors and rights advocates actively contest legislative supremacy.
 *
 * PERSPECTIVAL GAP:
 *   The parliamentary majority and democratic electorate seats experience the constraint as genuine democratic coordination â a solution to the problem of who decides in a democracy. The judiciary and constitutional minority seats experience the same structure as subordination and exposure. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Parliamentary majorities and the democratic electorate are structural beneficiaries (low d, low effective extraction) because the constraint subsidizes their authority and accountability. The judiciary and constitutional minorities are structural targets (high d, high effective extraction) because the constraint extracts interpretive power from courts and protective capacity from minorities. The elected legislature as agenda-setter sits near the beneficiary end with arbitrage-grade exit, reflecting its ability to dissolve the constraint by enacting a new constitutional settlement.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mandatrophy mislabeling by acknowledging its dual character: it was built to solve constitutional deadlock (founding problem: contested) and continues to serve that coordination function, but it also asymmetrically empowers legislative majorities over minorities and courts. If the founding problem were dead and the constraint persisted purely to empower majorities, it would drift toward snare. The temporal measurements show stable rather than sharply rising extraction, arguing against advanced mandatrophy, while the contested founding status flags the ambiguity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'Is parliamentary primacy a necessary feature of democratic constitutionalism or one contingent reading among others?',
    'Comparative institutional analysis showing democracies function with judicial supremacy (US, Germany) or coordinate construction (South Africa, Canada post-override) without collapse.',
    'If primacy is contingent, the constraint is a constructed coordination choice with identifiable beneficiaries and victims rather than a natural feature of democratic legitimacy; classification remains tangled_rope but foreclosure claims weaken.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Whether parliamentary primacy is contingent or necessary').

omega_variable(
    majority_tyranny_vs_democratic_accountability,
    'Does the constraint''s low extractiveness reflect genuine democratic coordination, or does it mask extraction from minorities that is structurally invisible in majoritarian institutions?',
    'Longitudinal analysis of rights-restricting legislation passage rates and judicial override availability in parliamentary supremacy systems versus systems with strong-form review.',
    'If minorities systematically lose protections, the effective extraction is higher than the base metric suggests; if majorities rarely abuse the power, the coordination function dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majority_tyranny_vs_democratic_accountability, empirical, 'Hidden extraction from minorities in majoritarian systems').

omega_variable(
    foreclosure_of_judicial_review,
    'Does parliamentary primacy logically foreclose strong-form judicial review, or can both coexist in a single framework through reserve powers or transitional arrangements?',
    'Jurisprudential analysis of whether a court''s assertion of strike-down power is conceptually compatible with parliamentary sovereignty as a background norm.',
    'If foreclosure is absolute, the relation to judicial_supremacy_reading is forecloses; if partial coexistence is possible, the relation downgrades to influences or coexists_with.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreclosure_of_judicial_review, conceptual, 'Logical compatibility of parliamentary primacy with judicial review').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_authority_boundary__parliamentary_primacy_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cons_tr_t10, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(cons_tr_t20, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement(cons_tr_t30, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 30, 0.16).
narrative_ontology:measurement(cons_tr_t40, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement(cons_tr_t50, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(cons_be_t10, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 10, 0.19).
narrative_ontology:measurement(cons_be_t20, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 20, 0.2).
narrative_ontology:measurement(cons_be_t30, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 30, 0.21).
narrative_ontology:measurement(cons_be_t40, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 40, 0.22).
narrative_ontology:measurement(cons_be_t50, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 50, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(cons_su_t10, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(cons_su_t20, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(cons_su_t30, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 30, 0.45).
narrative_ontology:measurement(cons_su_t40, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 40, 0.48).
narrative_ontology:measurement(cons_su_t50, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 50, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_authority_boundary__parliamentary_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_authority_boundary__parliamentary_primacy_reading, judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_authority_boundary__parliamentary_primacy_reading, coordinate_construction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the constitutional_authority_boundary kernel. The kernel decomposes into three structurally distinct constraints (parliamentary_primacy_reading, judicial_supremacy_reading, coordinate_construction_reading) because the natural-language concept of constitutional authority boundary conflates incompatible claims about interpretive finality. Each reading has distinct beneficiaries, victims, and epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
