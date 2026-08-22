% ============================================================================
% CONSTRAINT STORY: constitutional_interpretive_authority__parliamentary_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_interpretive_authority__parliamentary_supremacy_reading, []).

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
 *   constraint_id: constitutional_interpretive_authority__parliamentary_supremacy_reading
 *   human_readable: Parliamentary Supremacy: Elected Legislature as Final Constitutional Interpreter
 *   domain: constitutional_law/political_theory/jurisprudence
 *
 * SUMMARY:
 *   This constraint story captures the parliamentary supremacy reading of
 *   constitutional interpretive authority — the position that the elected
 *   legislature possesses final authority to determine constitutional
 *   meaning, with no judicial power to void parliamentary acts. This is one
 *   of three contested readings of the kernel
 *   'constitutional_interpretive_authority.' The constraint is structurally a
 *   tangled rope: it solves a genuine coordination problem (final democratic
 *   authority over constitutional meaning) while extracting asymmetrically
 *   from constitutional minorities and rights claimants who lack judicial
 *   recourse. The extraction is legitimated through electoral mandate rather
 *   than rights-grounding.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.35).
domain_priors:suppression_score(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.55).
domain_priors:theater_ratio(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_interpretive_authority__parliamentary_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_interpretive_authority__parliamentary_supremacy_reading, "Parliamentary Supremacy: Elected Legislature as Final Constitutional Interpreter").
narrative_ontology:topic_domain(constitutional_interpretive_authority__parliamentary_supremacy_reading, "constitutional_law/political_theory/jurisprudence").

domain_priors:requires_active_enforcement(constitutional_interpretive_authority__parliamentary_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_interpretive_authority__parliamentary_supremacy_reading, '0d10fc18-f7fb-4ed3-bd63-4e45ee7dc0f6').
narrative_ontology:cs_kernel_codification('0d10fc18-f7fb-4ed3-bd63-4e45ee7dc0f6', formalized).
narrative_ontology:cs_authority_grounding('0d10fc18-f7fb-4ed3-bd63-4e45ee7dc0f6', lineage).
narrative_ontology:cs_interpretation_layer_present('0d10fc18-f7fb-4ed3-bd63-4e45ee7dc0f6').
narrative_ontology:cs_reading_relation('0d10fc18-f7fb-4ed3-bd63-4e45ee7dc0f6', constitutional_interpretive_authority__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('0d10fc18-f7fb-4ed3-bd63-4e45ee7dc0f6', constitutional_interpretive_authority__coordinate_construction_reading, coexists_with).
narrative_ontology:cs_axiom('0d10fc18-f7fb-4ed3-bd63-4e45ee7dc0f6', foundational, legislative_supremacy_as_democratic_necessity).
narrative_ontology:cs_axiom_status(legislative_supremacy_as_democratic_necessity, holdable).
narrative_ontology:cs_axiom_grounding('0d10fc18-f7fb-4ed3-bd63-4e45ee7dc0f6', legislative_supremacy_as_democratic_necessity, deontological).
narrative_ontology:cs_axiom('0d10fc18-f7fb-4ed3-bd63-4e45ee7dc0f6', secondary, judicial_review_as_counter_majoritarian_usurpation).
narrative_ontology:cs_axiom_status(judicial_review_as_counter_majoritarian_usurpation, holdable).
narrative_ontology:cs_axiom_grounding('0d10fc18-f7fb-4ed3-bd63-4e45ee7dc0f6', judicial_review_as_counter_majoritarian_usurpation, deontological).
narrative_ontology:cs_reference_frame('0d10fc18-f7fb-4ed3-bd63-4e45ee7dc0f6', parliamentary_sovereignty_orthodoxy).
narrative_ontology:cs_drift_state('0d10fc18-f7fb-4ed3-bd63-4e45ee7dc0f6', contemporary_rights_based_constitutionalism, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0d10fc18-f7fb-4ed3-bd63-4e45ee7dc0f6', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__parliamentary_supremacy_reading, parliamentary_majority).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__parliamentary_supremacy_reading, executive_cabinet).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__parliamentary_supremacy_reading, electoral_constituencies).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_minorities).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__parliamentary_supremacy_reading, judicial_branch).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__parliamentary_supremacy_reading, rights_claimants).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__parliamentary_supremacy_reading, democratic_legitimacy_of_legislative_supremacy).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__parliamentary_supremacy_reading, electoral_mandate_as_constitutional_grounding).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls legislative agenda and exercises final interpretive authority over constitutional meaning. Benefits from the ability to define the scope of its own powers without judicial override. Electoral mandate legitimates this authority. Exit is arbitrage-grade: can reform or entrench the arrangement through ordinary legislative process.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, parliamentary_majority, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__parliamentary_supremacy_reading, parliamentary_majority, beneficiary).

% Dominates the parliamentary majority in most Westminster systems. Gains unified policy implementation capacity when legislative and executive interpretive authority align. Exit is mobile: cabinet ministers can move to opposition or private sector, but the institutional position benefits from the constraint.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, executive_cabinet, beneficiary,
    institutional, biographical, mobile, national).

% Majoritarian electoral coalitions see their policy preferences enacted without judicial filtration. Benefit from direct democratic responsiveness. Exit is constrained: voters can change the majority at elections, but cannot exit the constitutional structure itself.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, electoral_constituencies, beneficiary,
    organized, biographical, constrained, national).

% Groups whose fundamental rights or interests depend on counter-majoritarian protection. Bear the cost of unchecked legislative power with no judicial backstop. Exit is trapped: emigration is the only structural exit, carrying prohibitive personal and communal cost.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_minorities, payer,
    moderate, generational, trapped, national).

% Formally retains adjudicative function but is denied ultimate interpretive authority. Cannot void parliamentary acts. Professional identity and institutional legitimacy are fused to the judicial role, making exit identity-locked. The constraint defines the boundary of their institutional existence.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, judicial_branch, excluded,
    institutional, generational, identity_locked, national).

% Individuals or groups asserting rights violations by parliamentary acts. Face immediate harm with no structural remedy. Exit is trapped: the constraint denies the very forum where their claims could be heard.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, rights_claimants, payer,
    powerless, immediate, trapped, national).

% Analyze the constraint from outside the power structure. Produce the doctrinal frameworks that legitimate or contest parliamentary supremacy. Neither collect nor pay extraction; their exit is analytical by definition.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the coordination problem of final constitutional authority in a democratic system by vesting it in the elected legislature, avoiding inter-branch deadlock and ensuring democratic accountability for constitutional meaning.
% TRANSFER_FUNCTION: Transfers ultimate constitutional interpretive authority from the judiciary to the parliamentary majority, moving the power to define rights and institutional limits from a counter-majoritarian body to the majoritarian legislature.
% ABSENT_VOICES: Constitutional minorities and rights claimants are structurally excluded from the interpretive authority; they would object to the absence of a judicial backstop but their exclusion is constitutive of the parliamentary supremacy model. Future generations (unborn) are also absent — they inherit the constitutional structure without consent.
% DISAPPEARANCE_RATIONALE: If parliamentary supremacy vanished overnight, constitutional interpretation would shift to judicial review (as in most comparable democracies), rights protections would become justiciable against legislation, and the legislative agenda would be constrained by counter-majoritarian courts — the entire constitutional order would reorganize.
% FOUNDING_PROBLEM: The problem of democratic legitimacy in constitutional interpretation: how to ensure that the ultimate meaning of the constitution reflects the will of the people rather than unelected judges. The arrangement was built to solve the counter-majoritarian difficulty by making the elected legislature the final authority.
% FOUNDING_PROBLEM_CORROBORATION: Parliamentary sovereignty theorists (Dicey, Goldsworthy, Bellamy) attest the founding problem remains live — the counter-majoritarian difficulty is perennial. Judicial supremacy advocates (Dworkin, Waldron's critics, international human rights bodies) attest the problem is mischaracterized: rights protection requires institutional insulation from majoritarian politics. The corroboration is split along the kernel's reading lines.
narrative_ontology:disappearance_verdict(constitutional_interpretive_authority__parliamentary_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_interpretive_authority__parliamentary_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_interpretive_authority__parliamentary_supremacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(constitutional_interpretive_authority__parliamentary_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_interpretive_authority__parliamentary_supremacy_reading_tests).
:- end_tests(constitutional_interpretive_authority__parliamentary_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.35) — the constraint transfers interpretive authority but the legislature bears electoral accountability costs. Suppression is higher (0.55) — the constraint persists by actively excluding judicial review and minority veto points. Theater ratio is low-moderate (0.25) — the democratic legitimation is genuine, not merely performative, though ritualized parliamentary sovereignty discourse exists. Accessibility collapse is moderate (0.45) — alternative constitutional models (judicial review, coordinate construction) are conceptually available but structurally excluded in this system. Resistance is significant (0.6) — rights claimants, international bodies, and judicial actors continuously contest the arrangement.
 *
 * PERSPECTIVAL GAP:
 *   The parliamentary majority experiences this as democratic self-governance (coordination function dominant, extraction experienced as accountability). Constitutional minorities experience it as unchecked majoritarian power (extraction dominant, coordination function invisible). The judiciary experiences it as institutional truncation — their professional identity is constituted by a role the constraint denies them ultimate authority in. The engine computes these seat divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Parliamentary majority and executive cabinet are structural beneficiaries (d near 0.0): they control the interpretive agenda and collect the authority rents. Electoral constituencies are moderate beneficiaries (d ~ 0.3): they gain democratic responsiveness but bear diffuse costs of potential rights violations. Constitutional minorities and rights claimants are full targets (d near 1.0): they bear extraction with trapped exit. The judicial branch is excluded from beneficiary status but identity-locked (d ~ 0.8): they bear institutional truncation without exit. The derivation chain from beneficiary/victim + exit options produces these directionalities.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (counter-majoritarian difficulty) remains contested, not dead. The constraint has not atrophied into a piton — active democratic legitimation work maintains it. But the rising extraction trajectory (0.15→0.35) and suppression trajectory (0.3→0.55) over the interval suggest the coordination function is being stretched to cover expanding legislative authority beyond the original democratic legitimation. This is mandatrophy in motion: the mandate (democratic final authority) persists but the extraction profile has grown.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    democratic_legitimacy_vs_rights_protection,
    'Does the democratic legitimation of parliamentary supremacy structurally require the exclusion of judicial review, or can democratic final authority coexist with justiciable rights protections?',
    'Comparative analysis of systems with ''dialogue models'' (e.g., Canadian Charter s.33, UK Human Rights Act s.4) where legislative supremacy is formally preserved but courts can declare incompatibility — do these preserve the coordination function while reducing extraction?',
    'If coexistence is structurally stable, the measured extraction is partly a design choice, not a structural necessity of parliamentary supremacy. The constraint would be reclassifiable toward rope with lower extraction. If exclusion is necessary, the tangled_rope classification is structurally grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_legitimacy_vs_rights_protection, conceptual, 'Whether the extraction from minorities is inherent to parliamentary supremacy or a contingent institutional choice.').

omega_variable(
    electoral_accountability_as_extraction_damper,
    'Does the electoral accountability of the parliamentary majority functionally dampen extraction on minorities, or does majoritarian electoral logic amplify it?',
    'Longitudinal study of rights-violating legislation in parliamentary supremacy systems vs. judicial review systems: frequency, severity, and durability of minority-harming acts.',
    'If electoral accountability reliably constrains extraction, the effective extraction for minorities is lower than the base metric suggests. If majoritarian logic amplifies extraction (median voter theorem + minority exclusion), effective extraction is higher.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(electoral_accountability_as_extraction_damper, empirical, 'Whether the beneficiary''s accountability mechanism actually protects the victims.').

omega_variable(
    reading_foreclosure_structure,
    'Does the parliamentary supremacy reading logically foreclose the judicial supremacy reading within a single constitutional framework, or do they coexist as competing but simultaneously holdable positions?',
    'Analyze whether any constitutional system has stably institutionalized both readings simultaneously (e.g., departmentalism, coordinate construction as a third synthesis).',
    'If they foreclose, the kernel is a binary fork — adopting one reading structurally eliminates the other. If they coexist, the kernel supports stable pluralism and the constraint family is a persistent contest, not a resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_structure, conceptual, 'Structural relationship between this reading and the judicial_supremacy_reading sibling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_interpretive_authority__parliamentary_supremacy_reading, 1800, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(parl_supremacy_tr_t1800, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 1800, 0.1).
narrative_ontology:measurement(parl_supremacy_tr_t1850, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 1850, 0.12).
narrative_ontology:measurement(parl_supremacy_tr_t1900, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 1900, 0.15).
narrative_ontology:measurement(parl_supremacy_tr_t1950, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 1950, 0.2).
narrative_ontology:measurement(parl_supremacy_tr_t2000, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 2000, 0.22).
narrative_ontology:measurement(parl_supremacy_tr_t2025, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 2025, 0.25).

% Extraction over time
narrative_ontology:measurement(parl_supremacy_be_t1800, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 1800, 0.15).
narrative_ontology:measurement(parl_supremacy_be_t1850, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 1850, 0.2).
narrative_ontology:measurement(parl_supremacy_be_t1900, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 1900, 0.25).
narrative_ontology:measurement(parl_supremacy_be_t1950, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 1950, 0.3).
narrative_ontology:measurement(parl_supremacy_be_t2000, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 2000, 0.33).
narrative_ontology:measurement(parl_supremacy_be_t2025, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 2025, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(parl_supremacy_su_t1800, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 1800, 0.3).
narrative_ontology:measurement(parl_supremacy_su_t1850, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 1850, 0.35).
narrative_ontology:measurement(parl_supremacy_su_t1900, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 1900, 0.4).
narrative_ontology:measurement(parl_supremacy_su_t1950, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 1950, 0.48).
narrative_ontology:measurement(parl_supremacy_su_t2000, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 2000, 0.52).
narrative_ontology:measurement(parl_supremacy_su_t2025, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_interpretive_authority__parliamentary_supremacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.1).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_interpretive_authority__judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_interpretive_authority__coordinate_construction_reading).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__parliamentary_supremacy_reading, legislative_agenda_control).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__parliamentary_supremacy_reading, electoral_system_design).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__parliamentary_supremacy_reading, judicial_appointment_process).

% DUAL FORMULATION NOTE:
% This is the parliamentary_supremacy_reading of the constitutional_interpretive_authority kernel. The three readings form a constraint family linked by network.affects_constraints. This reading's epsilon (0.35) reflects moderate extraction from minorities; the judicial_supremacy_reading would author lower extraction from minorities but higher extraction from legislative majorities (judicial supremacy as counter-majoritarian constraint). The coordinate_construction_reading would author the lowest extraction but highest coordination complexity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_interpretive_authority__parliamentary_supremacy_reading, institutional, 0.15).
constraint_indexing:directionality_override(constitutional_interpretive_authority__parliamentary_supremacy_reading, moderate, 0.85).
constraint_indexing:directionality_override(constitutional_interpretive_authority__parliamentary_supremacy_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
