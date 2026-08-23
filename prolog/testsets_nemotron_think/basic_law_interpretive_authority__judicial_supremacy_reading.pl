% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_authority__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_authority__judicial_supremacy_reading, []).

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
 *   constraint_id: basic_law_interpretive_authority__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy in Constitutional Interpretation
 *   domain: constitutional_law/political_theory/institutional_design
 *
 * SUMMARY:
 *   This constraint story instantiates the judicial_supremacy_reading of the
 *   basic_law_interpretive_authority kernel. The reading claims courts hold
 *   final interpretive authority over constitutional meaning through
 *   specialized legal expertise and independence from political pressure.
 *   From this reading's perspective, the constraint provides essential
 *   coordination (final constitutional settlement) but extracts asymmetric
 *   authority from democratic branches. The judiciary and legal profession
 *   benefit from concentrated interpretive power; legislatures and electoral
 *   majorities bear costs when their enactments are voided. The constraint
 *   requires active enforcement through judicial review and the institutional
 *   compliance of other branches.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_authority__judicial_supremacy_reading, 0.65).
domain_priors:suppression_score(basic_law_interpretive_authority__judicial_supremacy_reading, 0.58).
domain_priors:theater_ratio(basic_law_interpretive_authority__judicial_supremacy_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_authority__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_authority__judicial_supremacy_reading, "Judicial Supremacy in Constitutional Interpretation").
narrative_ontology:topic_domain(basic_law_interpretive_authority__judicial_supremacy_reading, "constitutional_law/political_theory/institutional_design").

domain_priors:requires_active_enforcement(basic_law_interpretive_authority__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_authority__judicial_supremacy_reading, '1469aa1d-4278-4ff0-b1b7-14921964f31d').
narrative_ontology:cs_kernel_codification('1469aa1d-4278-4ff0-b1b7-14921964f31d', formalized).
narrative_ontology:cs_authority_grounding('1469aa1d-4278-4ff0-b1b7-14921964f31d', expertise).
narrative_ontology:cs_interpretation_layer_present('1469aa1d-4278-4ff0-b1b7-14921964f31d').
narrative_ontology:cs_reading_relation('1469aa1d-4278-4ff0-b1b7-14921964f31d', basic_law_interpretive_authority__parliamentary_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('1469aa1d-4278-4ff0-b1b7-14921964f31d', basic_law_interpretive_authority__popular_constitutionalism_reading, coexists_with).
narrative_ontology:cs_axiom('1469aa1d-4278-4ff0-b1b7-14921964f31d', foundational, judicial_interpretive_monopoly).
narrative_ontology:cs_axiom_status(judicial_interpretive_monopoly, holdable).
narrative_ontology:cs_axiom_grounding('1469aa1d-4278-4ff0-b1b7-14921964f31d', judicial_interpretive_monopoly, deontological).
narrative_ontology:cs_axiom('1469aa1d-4278-4ff0-b1b7-14921964f31d', foundational, counter_majoritarian_legitimacy).
narrative_ontology:cs_axiom_status(counter_majoritarian_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('1469aa1d-4278-4ff0-b1b7-14921964f31d', counter_majoritarian_legitimacy, deontological).
narrative_ontology:cs_reference_frame('1469aa1d-4278-4ff0-b1b7-14921964f31d', marbury_v_madison_framework).
narrative_ontology:cs_drift_state('1469aa1d-4278-4ff0-b1b7-14921964f31d', contemporary_rights_revolution_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1469aa1d-4278-4ff0-b1b7-14921964f31d', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_authority__judicial_supremacy_reading, basic_law_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__judicial_supremacy_reading, judiciary).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__judicial_supremacy_reading, constitutional_court).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__judicial_supremacy_reading, legal_profession).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__judicial_supremacy_reading, legislature).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__judicial_supremacy_reading, electoral_majorities).
narrative_ontology:constraint_vindicates(basic_law_interpretive_authority__judicial_supremacy_reading, judicial_supremacy_doctrine).
narrative_ontology:constraint_vindicates(basic_law_interpretive_authority__judicial_supremacy_reading, constitutional_interpretive_monopoly).
narrative_ontology:constraint_vindicates(basic_law_interpretive_authority__judicial_supremacy_reading, counter_majoritarian_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds final authority to interpret constitutional meaning through judicial review; sets binding precedent that all other branches must follow; controls the development of constitutional doctrine through case selection and opinion writing; collects institutional authority, prestige, and policy influence as the ultimate constitutional arbiter.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, judiciary, agenda_setter,
    institutional, generational, arbitrage, national).

% Specialized apex court that exercises concentrated constitutional review power; its decisions are final and binding on all other courts and political branches; gains institutional centrality, resource allocation priority, and status as guardian of the constitution.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, constitutional_court, beneficiary,
    institutional, generational, arbitrage, national).

% Monopolizes constitutional argument and interpretation through specialized training and bar admission; gains professional prestige, career opportunities, and gatekeeping authority over constitutional discourse; benefits from the demand for expert constitutional litigation.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, legal_profession, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__judicial_supremacy_reading, legal_profession, observer).

% Enacts legislation that can be invalidated by judicial review; loses final say on constitutional meaning of its own enactments; bears gridlock costs when courts strike down laws; must anticipate judicial reaction in legislative drafting; cannot easily exit the constraint without constitutional amendment.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, legislature, payer,
    institutional, biographical, constrained, national).

% Sees their democratically enacted preferences voided by unelected judges; bears the cost of policies they support being struck down; has no direct exit from judicial decisions short of constitutional amendment or court-packing; experiences the constraint as counter-majoritarian extraction.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, electoral_majorities, payer,
    organized, biographical, constrained, national).

% Analyze and critique judicial decisions from academic positions; produce the doctrinal frameworks courts draw on; do not directly collect rents or bear costs from the constraint but shape its intellectual legitimacy.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, constitutional_scholars, observer,
    analytical, generational, analytical, universal).

% Constitutional rights depend on judicial interpretation but have no formal voice in appointing or constraining judges; historically excluded from the legal profession and bench; bear disproportionate costs when judicial review fails to protect their rights; would object to both judicial supremacy and its alternatives if present.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, marginalized_communities, excluded,
    powerless, biographical, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides final, authoritative settlement of constitutional meaning, avoiding perpetual interpretive contestation and ensuring legal stability across political cycles; solves the problem of who ultimately decides what the constitution means when branches disagree.
% TRANSFER_FUNCTION: Moves final interpretive authority over constitutional meaning from the legislative branch and electoral majorities to the judiciary, concentrating constitutional decision-making in courts; transfers the power to invalidate democratically enacted laws from the political branches to unelected judges.
% ABSENT_VOICES: Citizens and communities whose constitutional rights are adjudicated without their direct participation; future generations bound by precedent they did not choose; legislative majorities whose enactments are voided without electoral recourse; marginalized communities historically excluded from the judiciary and legal profession.
% DISAPPEARANCE_RATIONALE: If judicial final say vanished overnight, constitutional meaning would become subject to legislative majorities (parliamentary sovereignty) or ongoing popular contestation (popular constitutionalism), fundamentally altering rights protection, federalism balances, and the separation of powers; the entire architecture of constitutional enforcement would reorganize.
% FOUNDING_PROBLEM: The need for an authoritative, non-partisan interpreter of constitutional text to prevent legislative tyranny and ensure stable constitutional meaning across political cycles, articulated in Federalist 78 as the judiciary's role as the 'least dangerous branch' exercising 'neither force nor will but merely judgment.'
% FOUNDING_PROBLEM_CORROBORATION: Federalist Papers (particularly Federalist 78) attest the founding problem from the framers' perspective; contemporary political scientists (e.g., Dahl, Rosenberg) and legal historians outside the judicial beneficiary set document the shift from counter-majoritarian check to policy-making body; the 'counter-majoritarian difficulty' literature (Bickel, Ely, Kramer) corroborates from outside the beneficiary set that the founding problem's status is contested.
narrative_ontology:disappearance_verdict(basic_law_interpretive_authority__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_authority__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_authority__judicial_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(basic_law_interpretive_authority__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_authority__judicial_supremacy_reading, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_authority__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_authority__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_authority__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) reflects substantial authority capture by the judiciary — the power to finally decide constitutional meaning is a significant resource transfer from political branches. Suppression (0.58) is moderate: judicial review actively blocks legislative alternatives, but compliance is largely voluntary/institutionalized rather than coercive. Theater ratio (0.38) captures the performative dimension of 'legal expertise' and 'independence' claims that partially mask policy-making. Accessibility collapse (0.62) reflects the difficulty of challenging judicial supremacy once entrenched (constitutional amendment is arduous). Resistance (0.52) shows persistent political pushback (court-curbing, jurisdiction stripping, popular constitutionalism). The metrics describe the constraint's actual operation; the claimed_type (tangled_rope) reflects my structural assessment that genuine coordination coexists with asymmetric extraction.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute divergent per-seat classifications: from the judiciary's seat (agenda_setter, institutional, arbitrage exit) the constraint appears as coordination (rope-like); from the legislature's seat (payer, institutional, constrained exit) it appears as extraction (snare-like); from electoral majorities (payer, organized, constrained exit) it appears as counter-majoritarian extraction; from marginalized communities (excluded, powerless, trapped) it appears as a Snare they cannot influence. This divergence is the measurement, not an error.
 *
 * DIRECTIONALITY LOGIC:
 *   Judiciary and constitutional court are structural beneficiaries (d near 0.0): they collect institutional authority, policy control, and professional monopoly rents. Legal profession is a secondary beneficiary (d ~0.2): gains professional gatekeeping but does not directly wield review power. Legislature and electoral majorities are targets (d near 0.8-0.9): bear costs of voided legislation and lost interpretive authority, with constrained exit (amendment is theoretically possible but practically prohibitive). Marginalized communities are identity-locked targets (d ~0.95): their rights depend on the constraint but they have no voice in its operation. Constitutional scholars are analytical (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing legislative tyranny through non-partisan interpretation) remains contested: rights-protective jurisprudence suggests it is live; the politicization of appointments and doctrinal expansion into policy-making suggests it is dead. The constraint persists partly through mandate drift — the original counter-majoritarian check has become a policy veto point. The classification as tangled_rope (not snare) turns on the genuine coordination function: some final interpreter is structurally necessary for a written constitution; the extraction is the concentration of that function in an unelected, unaccountable branch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'This constraint is one reading (judicial_supremacy_reading) of the contested kernel basic_law_interpretive_authority. What would the ε and beneficiary/victim structure be for the sibling readings?',
    'Author separate constraint stories for parliamentary_sovereignty_reading and popular_constitutionalism_reading with their own metrics and stakeholder structures; link via network.affects_constraints.',
    'Confirms ε-invariance: each reading instantiates a different constraint with different extraction profile. Prevents conflating the kernel label with a single constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment that this is one reading of a kernel, not the kernel itself.').

omega_variable(
    judicial_expertise_claim,
    'Is the claimed ''specialized legal expertise and independence from political pressure'' empirically substantiated, or is it a legitimating narrative that masks policy preferences?',
    'Empirical studies of judicial decision-making (attitudinal model, strategic model, historical analysis of appointment politics); comparative analysis of judicial independence across regimes.',
    'If expertise/independence is largely narrative, the coordination justification weakens and the constraint shifts toward snare; if substantiated, the coordination function is genuine and tangled_rope holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_expertise_claim, empirical, 'Whether the coordination justification (expertise/independence) is real or cover.').

omega_variable(
    counter_majoritarian_difficulty,
    'Does the constraint''s extraction from electoral majorities represent a necessary protection of minority rights (coordination) or an illegitimate power grab by elites (extraction)?',
    'Case-by-case analysis of judicial review outcomes: proportion protecting discrete and insular minorities vs. imposing majority-preference policies; longitudinal study of rights expansion vs. contraction.',
    'If predominantly rights-protective, the extraction is the price of coordination (tangled_rope); if predominantly policy-imposing, the coordination story is cover (snare).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counter_majoritarian_difficulty, preference, 'The normative evaluation of counter-majoritarian outcomes — irreducible to data.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_authority__judicial_supremacy_reading, 1803, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t1803, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1803, 0.1).
narrative_ontology:measurement(basi_tr_t1857, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1857, 0.2).
narrative_ontology:measurement(basi_tr_t1905, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1905, 0.35).
narrative_ontology:measurement(basi_tr_t1937, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1937, 0.25).
narrative_ontology:measurement(basi_tr_t1954, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1954, 0.3).
narrative_ontology:measurement(basi_tr_t1973, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1973, 0.4).
narrative_ontology:measurement(basi_tr_t2000, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 2000, 0.42).
narrative_ontology:measurement(basi_tr_t2024, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(basi_be_t1803, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1803, 0.15).
narrative_ontology:measurement(basi_be_t1857, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1857, 0.25).
narrative_ontology:measurement(basi_be_t1905, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1905, 0.45).
narrative_ontology:measurement(basi_be_t1937, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1937, 0.35).
narrative_ontology:measurement(basi_be_t1954, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1954, 0.55).
narrative_ontology:measurement(basi_be_t1973, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1973, 0.65).
narrative_ontology:measurement(basi_be_t2000, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement(basi_be_t2024, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t1803, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1803, 0.2).
narrative_ontology:measurement(basi_su_t1857, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1857, 0.4).
narrative_ontology:measurement(basi_su_t1905, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1905, 0.55).
narrative_ontology:measurement(basi_su_t1937, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1937, 0.45).
narrative_ontology:measurement(basi_su_t1954, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1954, 0.6).
narrative_ontology:measurement(basi_su_t1973, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1973, 0.65).
narrative_ontology:measurement(basi_su_t2000, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(basi_su_t2024, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 2024, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_authority__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(basic_law_interpretive_authority__judicial_supremacy_reading, 0.12).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__judicial_supremacy_reading, basic_law_interpretive_authority__parliamentary_sovereignty_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__judicial_supremacy_reading, basic_law_interpretive_authority__popular_constitutionalism_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the 'basic law interpretive authority' kernel into three readings with distinct ε values and beneficiary/victim structures. Judicial supremacy (this story) claims expertise-based authority with judiciary as beneficiary; parliamentary sovereignty claims democratic mandate with legislature as beneficiary; popular constitutionalism claims democratic contestation with the people as beneficiary. The ε values differ substantially: this reading has ε=0.65 (substantial extraction); parliamentary sovereignty would have lower ε (legislature already holds legislative power); popular constitutionalism would have lowest ε (diffuse authority).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(basic_law_interpretive_authority__judicial_supremacy_reading, institutional, 0.15).
constraint_indexing:directionality_override(basic_law_interpretive_authority__judicial_supremacy_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
