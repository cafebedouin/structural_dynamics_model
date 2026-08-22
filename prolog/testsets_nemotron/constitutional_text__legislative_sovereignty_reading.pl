% ============================================================================
% CONSTRAINT STORY: constitutional_text__legislative_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text__legislative_sovereignty_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: constitutional_text__legislative_sovereignty_reading
 *   human_readable: Legislative Sovereignty Reading of Constitutional Text
 *   domain: constitutional_theory/comparative_law
 *
 * SUMMARY:
 *   This constraint story captures the legislative sovereignty reading of
 *   constitutional text — the view that parliament is supreme and courts play
 *   an advisory role, with the legislature retaining final say on
 *   constitutional meaning through mechanisms like notwithstanding clauses
 *   (Canada Section 33) or simple legislative override (UK Human Rights Act
 *   model, Israeli Basic Laws). The reading resolves the counter-majoritarian
 *   difficulty by vesting interpretive authority in elected representatives.
 *   However, it creates a structural asymmetry: legislative majorities gain
 *   the power to define the scope of rights protections, while minority
 *   rights claimants lose counter-majoritarian enforcement. The constraint
 *   operates as a tangled rope: it solves a genuine coordination problem
 *   (democratic legitimacy of constitutional interpretation) while extracting
 *   from minority rights holders through the same structure. Active
 *   enforcement is required — the legislative override power must be
 *   exercised or credibly threatened to maintain the arrangement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__legislative_sovereignty_reading, 0.38).
domain_priors:suppression_score(constitutional_text__legislative_sovereignty_reading, 0.32).
domain_priors:theater_ratio(constitutional_text__legislative_sovereignty_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__legislative_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text__legislative_sovereignty_reading, "Legislative Sovereignty Reading of Constitutional Text").
narrative_ontology:topic_domain(constitutional_text__legislative_sovereignty_reading, "constitutional_theory/comparative_law").

domain_priors:requires_active_enforcement(constitutional_text__legislative_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__legislative_sovereignty_reading, '389a1432-ec02-4781-9bc7-4da084ce94ef').
narrative_ontology:cs_kernel_codification('389a1432-ec02-4781-9bc7-4da084ce94ef', formalized).
narrative_ontology:cs_authority_grounding('389a1432-ec02-4781-9bc7-4da084ce94ef', lineage).
narrative_ontology:cs_interpretation_layer_present('389a1432-ec02-4781-9bc7-4da084ce94ef').
narrative_ontology:cs_reading_relation('389a1432-ec02-4781-9bc7-4da084ce94ef', constitutional_text__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('389a1432-ec02-4781-9bc7-4da084ce94ef', constitutional_text__popular_sovereignty_reading, influences).
narrative_ontology:cs_axiom('389a1432-ec02-4781-9bc7-4da084ce94ef', foundational, legislature_final_constitutional_authority).
narrative_ontology:cs_axiom_status(legislature_final_constitutional_authority, holdable).
narrative_ontology:cs_axiom_grounding('389a1432-ec02-4781-9bc7-4da084ce94ef', legislature_final_constitutional_authority, conventional).
narrative_ontology:cs_axiom('389a1432-ec02-4781-9bc7-4da084ce94ef', foundational, democratic_legitimacy_requires_elected_interpretive_finality).
narrative_ontology:cs_axiom_status(democratic_legitimacy_requires_elected_interpretive_finality, holdable).
narrative_ontology:cs_axiom_grounding('389a1432-ec02-4781-9bc7-4da084ce94ef', democratic_legitimacy_requires_elected_interpretive_finality, deontological).
narrative_ontology:cs_reference_frame('389a1432-ec02-4781-9bc7-4da084ce94ef', parliamentary_supremacy_framework).
narrative_ontology:cs_drift_state('389a1432-ec02-4781-9bc7-4da084ce94ef', contemporary_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('389a1432-ec02-4781-9bc7-4da084ce94ef', '2026-08-04T14:32:17Z').
narrative_ontology:cs_kernel_id(constitutional_text__legislative_sovereignty_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__legislative_sovereignty_reading, majoritarian_legislative_majorities).
narrative_ontology:constraint_beneficiary(constitutional_text__legislative_sovereignty_reading, elected_representatives).
narrative_ontology:constraint_victim(constitutional_text__legislative_sovereignty_reading, minority_rights_claimants).
narrative_ontology:constraint_victim(constitutional_text__legislative_sovereignty_reading, constitutional_rights_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_text__legislative_sovereignty_reading, legislative_majority).
narrative_ontology:constraint_beneficiary(constitutional_text__legislative_sovereignty_reading, constitutional_courts).
narrative_ontology:constraint_victim(constitutional_text__legislative_sovereignty_reading, constitutional_courts).
narrative_ontology:constraint_vindicates(constitutional_text__legislative_sovereignty_reading, parliamentary_supremacy_doctrine).
narrative_ontology:constraint_vindicates(constitutional_text__legislative_sovereignty_reading, democratic_legitimacy_through_elected_representatives).
narrative_ontology:constraint_vindicates(constitutional_text__legislative_sovereignty_reading, constitutional_interpretation_as_political_question).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds final authority on constitutional meaning through notwithstanding clauses or simple legislative override. Can enact policy preferences even when courts advise against them. Benefits from the arrangement by retaining democratic control over constitutional interpretation, but also bears the political cost of rights-infringing legislation. Exit would require constitutional amendment or electoral defeat.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, legislative_majority, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text__legislative_sovereignty_reading, legislative_majority, beneficiary).

% Rely on judicial enforcement of constitutional rights against legislative majorities. When legislature overrides judicial advice, their rights protections collapse to political majoritarianism. No effective exit from the jurisdiction; constitutional amendment is controlled by the same majority. Bear concentrated costs when legislative override targets their rights.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, minority_rights_claimants, payer,
    powerless, biographical, trapped, national).

% Issue advisory opinions on constitutional questions but lack final authority. Retain institutional legitimacy and intellectual authority; their reasoning shapes public discourse and legislative deliberation. However, their decisions can be disregarded, creating institutional humiliation and undermining rule-of-law credibility. Exit would mean resigning or refusing to hear cases — structurally constrained by judicial oath and institutional role.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, constitutional_courts, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text__legislative_sovereignty_reading, constitutional_courts, payer).

% General population whose constitutional rights are subject to legislative override. Experience diffuse uncertainty about rights stability. Some capacity for political mobilization and electoral accountability, but rights protection depends on majority forbearance. Exit is emigration — costly and incomplete.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, constitutional_rights_holders, payer,
    moderate, biographical, constrained, national).

% Analyze the arrangement from comparative and theoretical perspectives. Track how legislative sovereignty interacts with rights protection across jurisdictions. No material stake in the constraint's operation; exit is trivial (intellectual disengagement).
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, legal_scholars_and_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the counter-majoritarian difficulty by giving elected representatives final say on constitutional meaning, ensuring democratic legitimacy of constitutional interpretation. Provides a clear, determinate rule for resolving interpretive disputes: the legislature decides.
% TRANSFER_FUNCTION: Transfers final interpretive authority from courts to legislature. Moves the power to determine constitutional meaning — and thus the scope of rights protections — from an unelected judiciary to elected representatives. The cost is borne by those whose rights depend on counter-majoritarian enforcement; the benefit accrues to majoritarian legislative agendas.
% ABSENT_VOICES: Future generations whose rights landscape is shaped by current legislative overrides; non-citizens subject to the jurisdiction's laws without electoral voice; the constitutional text itself as an authorial presence that may constrain legislative will even under this reading — these voices are structurally excluded from the interpretive moment.
% DISAPPEARANCE_RATIONALE: If legislative final authority vanished overnight, courts would become the final arbiters of constitutional meaning (judicial supremacy reading would instantiate). Rights protections would strengthen for minorities but democratic legitimacy of constitutional interpretation would be contested. The political-legal equilibrium would fundamentally restructure.
% FOUNDING_PROBLEM: The counter-majoritarian difficulty: unelected judges invalidating legislation enacted by elected representatives lacks democratic legitimacy. Legislative sovereignty reading was built to solve this by making constitutional interpretation a political question answerable to the electorate.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (e.g., Canadian parliamentary tradition, UK model, scholarly defenders like Richard Bellamy, Jeremy Waldron) attest the counter-majoritarian difficulty remains live and legislative sovereignty solves it. Critics (rights theorists, judicial supremacy advocates, minority rights organizations) attest the founding problem is overstated — courts protect minorities, not thwart democracy — and the arrangement persists as majoritarian power protection. Corroboration from outside beneficiaries: comparative constitutional scholars documenting rights erosion under legislative override regimes (e.g., Canadian Charter Section 33 uses, UK Human Rights Act debates, Israeli Basic Law overrides).
narrative_ontology:disappearance_verdict(constitutional_text__legislative_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text__legislative_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__legislative_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(constitutional_text__legislative_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text__legislative_sovereignty_reading, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text__legislative_sovereignty_reading_tests).
:- end_tests(constitutional_text__legislative_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38) is moderate: the arrangement transfers interpretive authority from courts to legislature, which is a real transfer of power but not pure rent extraction — the coordination function (democratic legitimacy) is genuine. Suppression (0.32) is moderate: minority rights claimants face structural barriers to rights enforcement, but alternatives exist (political mobilization, electoral accountability, international human rights mechanisms). Theater ratio (0.22) is low-moderate: legislative override is used sparingly (Canada's Section 33 invoked rarely), but the threat of override shapes judicial behavior (dialogue model). Accessibility collapse (0.45) is moderate: judicial review remains available as advisory, and political mobilization provides partial alternative paths. Resistance (0.58) is moderate-high: rights advocates, courts, and civil society actively contest legislative overrides, creating ongoing friction.
 *
 * PERSPECTIVAL GAP:
 *   The legislative majority experiences this as a rope (genuine coordination solving democratic legitimacy). Minority rights claimants experience it as a snare (extraction of rights protections). Courts experience it as a degraded rope (advisory role with real but non-final authority). The engine will compute these seat divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Legislative majority is the primary beneficiary (d ~0.15): collects interpretive authority, sets the agenda, has arbitrage-grade exit (electoral mandate, constitutional amendment power). Minority rights claimants are primary victims (d ~0.85): bear concentrated costs when rights are overridden, trapped exit (no effective alternative forum, amendment controlled by majority). Courts sit near symmetric (d ~0.5): retain intellectual authority and legitimacy but lose final say; constrained exit (institutional role binds them). General rights holders are payers with constrained exit (d ~0.65): diffuse costs, political mobilization as partial exit. Observers are analytical (d=0.5 by definition).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (counter-majoritarian difficulty) remains contested — not dead. The arrangement is not pure mandatrophy because the coordination function (democratic legitimacy) is still claimed and partially operative. However, extraction has accumulated over time as legislative override powers have been used or threatened in rights-sensitive domains. The classification as tangled rope captures this dual character: genuine coordination persisting alongside asymmetric extraction. The mandatrophy question is whether the coordination function could be preserved with less extraction (e.g., stronger dialogue models, time-limited overrides, supermajority requirements) — the constraint persists in its current form partly due to institutional inertia and majoritarian self-interest.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    countermajoritarian_difficulty_persistence,
    'Does the counter-majoritarian difficulty remain a live structural problem requiring legislative final authority, or has it been substantially resolved by institutional developments (dialogue models, proportionality review, judicial restraint)?',
    'Comparative analysis of rights protection outcomes under legislative sovereignty vs. judicial supremacy regimes; longitudinal study of legislative override usage and its rights impact.',
    'If the founding problem is dead, the arrangement''s coordination function is attenuated and its classification shifts toward snare/piton. If live, tangled rope classification is sustained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(countermajoritarian_difficulty_persistence, conceptual, 'Whether the founding problem that justified legislative sovereignty persists.').

omega_variable(
    minority_rights_extraction_measurement,
    'How much of the measured extractiveness represents necessary coordination cost (democratic legitimacy) vs. pure majoritarian rent extraction?',
    'Decompose override instances: those responding to genuine democratic legitimacy disputes vs. those targeting discrete minority rights. Compare rights outcomes under legislative override vs. judicial enforcement across comparable jurisdictions.',
    'If extraction is predominantly coordination cost, the constraint is closer to rope. If predominantly rent extraction, it is closer to snare. Current tangled rope classification assumes both are present.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(minority_rights_extraction_measurement, empirical, 'The coordination-extraction boundary within legislative override power.').

omega_variable(
    kernel_reading_foreclosure_structure,
    'Does the legislative sovereignty reading logically foreclose the judicial supremacy reading within a single constitutional framework, or do they coexist as competing interpretations?',
    'Analyze whether any constitutional system has stably institutionalized both readings simultaneously (e.g., different issue domains, different time periods) without structural contradiction.',
    'If forecloses, the kernel has a forced-choice structure. If coexists_with, the kernel supports pluralistic institutionalization. Affects cs_structure.reading_relations assignment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_structure, conceptual, 'Structural relationship between legislative sovereignty and judicial supremacy readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__legislative_sovereignty_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(const_text_leg_sov_tr_t0, constitutional_text__legislative_sovereignty_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(const_text_leg_sov_tr_t8, constitutional_text__legislative_sovereignty_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement(const_text_leg_sov_tr_t16, constitutional_text__legislative_sovereignty_reading, theater_ratio, 16, 0.18).
narrative_ontology:measurement(const_text_leg_sov_tr_t24, constitutional_text__legislative_sovereignty_reading, theater_ratio, 24, 0.2).
narrative_ontology:measurement(const_text_leg_sov_tr_t32, constitutional_text__legislative_sovereignty_reading, theater_ratio, 32, 0.21).
narrative_ontology:measurement(const_text_leg_sov_tr_t40, constitutional_text__legislative_sovereignty_reading, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(const_text_leg_sov_be_t0, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(const_text_leg_sov_be_t8, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 8, 0.28).
narrative_ontology:measurement(const_text_leg_sov_be_t16, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 16, 0.32).
narrative_ontology:measurement(const_text_leg_sov_be_t24, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 24, 0.35).
narrative_ontology:measurement(const_text_leg_sov_be_t32, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 32, 0.37).
narrative_ontology:measurement(const_text_leg_sov_be_t40, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 40, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(const_text_leg_sov_su_t0, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(const_text_leg_sov_su_t8, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 8, 0.25).
narrative_ontology:measurement(const_text_leg_sov_su_t16, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 16, 0.28).
narrative_ontology:measurement(const_text_leg_sov_su_t24, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 24, 0.3).
narrative_ontology:measurement(const_text_leg_sov_su_t32, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 32, 0.31).
narrative_ontology:measurement(const_text_leg_sov_su_t40, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 40, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text__legislative_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_text__legislative_sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_text__legislative_sovereignty_reading, constitutional_text__judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_text__legislative_sovereignty_reading, constitutional_text__popular_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This reading and judicial_supremacy_reading form a constraint family around the constitutional_text kernel. They share the same referent (the constitutional text) but instantiate different constraints with different ε values (legislative sovereignty: moderate extractiveness, genuine coordination; judicial supremacy: lower extractiveness for minorities, higher for majoritarian will). The popular_sovereignty_reading is upstream — both legislative and judicial readings claim democratic legitimacy but derive it differently (elected representatives vs. courts as guardians of popular constitutionalism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_text__legislative_sovereignty_reading, institutional, 0.15).
constraint_indexing:directionality_override(constitutional_text__legislative_sovereignty_reading, powerless, 0.85).
constraint_indexing:directionality_override(constitutional_text__legislative_sovereignty_reading, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
