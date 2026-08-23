% ============================================================================
% CONSTRAINT STORY: copyright_constitutional_mandate__judicial_ambiguity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_copyright_constitutional_mandate__judicial_ambiguity_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: copyright_constitutional_mandate__judicial_ambiguity_reading
 *   human_readable: Judicial Ambiguity Reading of Copyright Term Constitutional Mandate
 *   domain: intellectual_property_law/constitutional_law/political_economy
 *
 * SUMMARY:
 *   This constraint instantiates the judicial_ambiguity_reading of the
 *   contested kernel copyright_constitutional_mandate. The kernel is Article
 *   I, Section 8, Clause 8 of the U.S. Constitution, which empowers Congress
 *   to secure exclusive rights for 'limited times.' Three readings contest
 *   this kernel: the public_scaffold_reading treats copyright as a temporary
 *   scaffold for public enrichment; the corporate_enclosure_reading treats it
 *   as maximal property protection; and this reading, the
 *   judicial_ambiguity_reading, treats the meaning of 'limited times' as a
 *   zone of legislative discretion subject only to rational basis review. The
 *   judicial ambiguity reading structurally enables the corporate enclosure
 *   reading by removing judicial invalidation as a check on congressional
 *   extension, while simultaneously undermining the public scaffold reading
 *   by disabling the judicial enforcement mechanism that would guarantee the
 *   temporary nature of the monopoly. The constraint's beneficiary is
 *   congressional authority (broad discretion); its victim is constitutional
 *   fixity as a constraint on legislative drift. Epsilon is low-to-moderate
 *   because the doctrine itself does not directly extract but rather disables
 *   a protective constitutional mechanism.
 *
 * KEY AGENTS:
 *   - congress: Primary beneficiary (institutional/constrained) â gains discretion to set and extend copyright terms without judicial ceiling.
 *   - supreme_court_majority: Agenda-setter (institutional/analytical) â administers rational basis review doctrine and controls constitutional interpretation.
 *   - incumbent_copyright_holders: Secondary beneficiary (powerful/mobile) â capture monopoly rents from term extensions enabled by judicial deference.
 *   - subsequent_creators: Primary target (moderate/constrained) â bear increased licensing costs and legal uncertainty.
 *   - general_public: Secondary target (powerless/constrained) â loses public domain access and pays higher cultural costs.
 *   - public_domain_advocates: Excluded voice (moderate/constrained) â argue for enforceable limits but are sidelined by rational basis framework.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.4).
domain_priors:suppression_score(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.65).
domain_priors:theater_ratio(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__judicial_ambiguity_reading, tangled_rope).
narrative_ontology:human_readable(copyright_constitutional_mandate__judicial_ambiguity_reading, "Judicial Ambiguity Reading of Copyright Term Constitutional Mandate").
narrative_ontology:topic_domain(copyright_constitutional_mandate__judicial_ambiguity_reading, "intellectual_property_law/constitutional_law/political_economy").

domain_priors:requires_active_enforcement(copyright_constitutional_mandate__judicial_ambiguity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__judicial_ambiguity_reading, '979e169b-3d7e-40cc-aa41-2888af788b5e').
narrative_ontology:cs_kernel_codification('979e169b-3d7e-40cc-aa41-2888af788b5e', fixed_text).
narrative_ontology:cs_authority_grounding('979e169b-3d7e-40cc-aa41-2888af788b5e', lineage).
narrative_ontology:cs_interpretation_layer_present('979e169b-3d7e-40cc-aa41-2888af788b5e').
narrative_ontology:cs_reading_relation('979e169b-3d7e-40cc-aa41-2888af788b5e', copyright_constitutional_mandate__public_scaffold_reading, influences).
narrative_ontology:cs_reading_relation('979e169b-3d7e-40cc-aa41-2888af788b5e', copyright_constitutional_mandate__corporate_enclosure_reading, influences).
narrative_ontology:cs_axiom('979e169b-3d7e-40cc-aa41-2888af788b5e', foundational, rational_basis_review_for_limited_times).
narrative_ontology:cs_axiom_status(rational_basis_review_for_limited_times, holdable).
narrative_ontology:cs_axiom_grounding('979e169b-3d7e-40cc-aa41-2888af788b5e', rational_basis_review_for_limited_times, conventional).
narrative_ontology:cs_axiom('979e169b-3d7e-40cc-aa41-2888af788b5e', foundational, judicial_abstention_in_economic_policy).
narrative_ontology:cs_axiom_status(judicial_abstention_in_economic_policy, holdable).
narrative_ontology:cs_axiom_grounding('979e169b-3d7e-40cc-aa41-2888af788b5e', judicial_abstention_in_economic_policy, instrumental).
narrative_ontology:cs_reference_frame('979e169b-3d7e-40cc-aa41-2888af788b5e', legislative_discretion_framework).
narrative_ontology:cs_drift_state('979e169b-3d7e-40cc-aa41-2888af788b5e', post_ctea_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('979e169b-3d7e-40cc-aa41-2888af788b5e', '').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__judicial_ambiguity_reading, congress).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__judicial_ambiguity_reading, incumbent_copyright_holders).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__judicial_ambiguity_reading, subsequent_creators).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__judicial_ambiguity_reading, general_public).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__judicial_ambiguity_reading, rational_basis_doctrine).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__judicial_ambiguity_reading, judicial_restraint_in_economic_policy).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__judicial_ambiguity_reading, plenary_congressional_power_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Congress sets copyright term lengths and retroactive extensions knowing that courts will defer to its policy judgments under rational basis review. This grants it broad discretion unconstrained by judicial enforcement of the 'limited times' textual ceiling.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, congress, beneficiary,
    institutional, generational, constrained, national).

% The Supreme Court maintains the doctrine that copyright term extensions are subject only to rational basis review. It sets the interpretive framework, chooses not to enforce a strict textual limit on 'limited times,' and structures the legal environment within which Congress operates.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, supreme_court_majority, agenda_setter,
    institutional, generational, analytical, national).

% Large media corporations and estate holders benefit from repeated term extensions and retroactive protections. They capture monopoly rents on back-catalogs that would otherwise enter the public domain, secure in the knowledge that judicial deference will prevent constitutional invalidation of extensions.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, incumbent_copyright_holders, beneficiary,
    powerful, biographical, mobile, global).

% Artists, writers, and developers who build on prior culture face higher licensing costs and legal uncertainty. Works that should have entered the public domain remain under lock and key, raising the cost of follow-on creation and limiting expressive freedom.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, subsequent_creators, payer,
    moderate, biographical, constrained, national).

% The general public loses access to works that would have entered the public domain under shorter terms. They pay higher prices for cultural goods and lose the ability to freely use, remix, or archive works from the extended copyright period.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, general_public, payer,
    powerless, biographical, constrained, national).

% Legal scholars, librarians, and digital archivists who argue for a judicially enforceable ceiling on copyright terms. They file amicus briefs and litigate challenges, but their arguments are systematically sidelined by the rational basis framework that treats term length as a legislative prerogative.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, public_domain_advocates, excluded,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(copyright_constitutional_mandate__judicial_ambiguity_reading, incumbent_copyright_holders).
narrative_ontology:fixing_cost_class(copyright_constitutional_mandate__judicial_ambiguity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates separation of powers by insulating legislative economic policy from judicial second-guessing; provides a stable doctrinal framework for copyright legislation by removing the threat of constitutional invalidation based on term length alone.
% TRANSFER_FUNCTION: Transfers interpretive authority over the meaning of 'limited times' from the judiciary to Congress; transfers public domain access from subsequent creators and the general public to incumbent copyright holders by disabling the constitutional mechanism that would otherwise limit legislative extension.
% ABSENT_VOICES: Public domain advocates and constitutional originalists who would argue for a judicially enforceable ceiling on copyright terms are present in litigation but structurally excluded from winning; their arguments are recognized but deemed non-justiciable under the rational basis framework.
% DISAPPEARANCE_RATIONALE: If courts began strictly enforcing 'limited times' as a judicially manageable constraint, Congress would face a constitutional ceiling on term extensions, works would enter the public domain on schedule, and the political economy of copyright lobbying would shift from seeking infinite extensions to operating within a fixed boundary.
% FOUNDING_PROBLEM: The problem of judicial competence in complex empirical policy-making: courts lack the economic and technological expertise to determine the optimal copyright term that maximizes creative production, and repeated judicial intervention would destabilize legislative policy.
% FOUNDING_PROBLEM_CORROBORATION: Separation-of-powers scholars outside the copyright industry attest that judicial restraint in economic policy is structurally warranted. Public domain advocates and constitutional textualists contest that this rationale justifies complete abdication of the judicial duty to enforce the 'limited times' textual limit; they note that the original founding problem has been solved by the creation of specialized policy institutions and that the doctrine now functions as a shield for legislative capture.
narrative_ontology:disappearance_verdict(copyright_constitutional_mandate__judicial_ambiguity_reading, world_rearranges).
narrative_ontology:founding_problem_status(copyright_constitutional_mandate__judicial_ambiguity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(copyright_constitutional_mandate__judicial_ambiguity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(copyright_constitutional_mandate__judicial_ambiguity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.4, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(copyright_constitutional_mandate__judicial_ambiguity_reading_tests).
:- end_tests(copyright_constitutional_mandate__judicial_ambiguity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.40 (low-to-moderate) because the judicial deference doctrine does not directly transfer wealth but instead disables a constitutional check, enabling downstream extraction by Congress and copyright industries. Suppression is 0.65 because the doctrine actively suppresses the alternative of heightened scrutiny or originalist textual enforcement. Accessibility collapse is high (0.75): once rational basis review is established as the framework, legal alternatives collapseâchallenges are virtually guaranteed to fail. Resistance is 0.60 because public domain advocates and scholars continue to mount doctrinal and political challenges. Theater ratio is 0.52 and rising: the rhetoric of judicial restraint and separation of powers performs an increasing amount of work relative to the actual coordination function, as the gap between the doctrine's justification (judicial incompetence in economic policy) and its effect (protection of legislative capture) widens.
 *
 * PERSPECTIVAL GAP:
 *   From the congressional and judicial seats, the arrangement appears as necessary separation-of-powers coordination: courts should not micromanage Congress's empirical judgments about optimal incentive structures. From the public and creator seats, the same arrangement appears as an extraction mechanism that removes the constitutional floor beneath the public domain. The engine computes this divergence from the structural dataâdeclared beneficiaries (Congress, copyright holders) with constrained or mobile exit versus declared payers (public, subsequent creators) with constrained exit.
 *
 * DIRECTIONALITY LOGIC:
 *   Congress sits near the beneficiary end: it gains discretion and faces no judicial penalty for extensions. The Supreme Court majority sits near symmetric but leaning beneficiary: it maintains institutional authority by avoiding contested political territory. Incumbent copyright holders sit at the beneficiary end: they collect the downstream monopoly rents. Subsequent creators and the general public sit near the full-target end: they bear the costs of foregone public domain access and have no meaningful exit from the copyright system. Public domain advocates are excluded rather than coordinated: their presence in litigation is recognized but their arguments are structurally prevented from succeeding.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the doctrine as a pure coordination rope (which would ignore the asymmetric extraction from the public domain) or as a pure snare (which would ignore the genuine separation-of-powers coordination function). The doctrine solves a real coordination problemâjudicial competence in economic policyâwhile simultaneously extracting by disabling a constitutional constraint on legislative drift. The temporal measurements show slow accumulation: base extractiveness rises from 0.25 to 0.40 over the interval as the consequences of deference compound, and theater ratio rises from 0.30 to 0.52 as the gap between justification and effect widens. This pattern is characteristic of a tangled rope whose coordination function has been progressively colonized by extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_ambiguity_kernel_position,
    'This constraint is one reading (judicial_ambiguity_reading) of the copyright_constitutional_mandate kernel, alongside public_scaffold_reading and corporate_enclosure_reading. Does the judicial deference doctrine structurally foreclose either sibling reading, or does it merely influence their operating environment?',
    'Engine computation of reading_relations foreclosure chains; doctrinal analysis of whether rational basis review is logically compatible with a judicially enforceable public-domain ceiling.',
    'If foreclosing, the engine should flag this reading as dominating the kernel space; if influencing, all three readings remain live positions in a contested interpretive field.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_ambiguity_kernel_position, conceptual, 'Kernel reading structural position').

omega_variable(
    rational_basis_extraction_nature,
    'Does rational basis review of copyright term length independently extract from the public domain by disabling a constitutional check, or is it a neutral coordination mechanism (separation of powers) that merely permits downstream legislative choices?',
    'Counterfactual analysis: would Congress extend terms as aggressively if courts applied heightened scrutiny or originalist textual limits?',
    'If independent extraction, epsilon should reflect the full extraction enabled by the doctrine; if neutral coordination, epsilon should be lower and the extraction assigned to the legislative constraints downstream.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rational_basis_extraction_nature, empirical, 'Whether judicial deference is an independent extraction mechanism').

omega_variable(
    separation_of_powers_vs_abdication,
    'Is the judicial deference doctrine a necessary separation-of-powers coordination device, or has it become a performative cover for abdication of judicial duty?',
    'Analysis of the Court''s capacity to articulate and enforce a manageable ''limited times'' standard compared to its willingness to do so.',
    'If abdication, theater_ratio and extractiveness should rise; if genuine coordination, the coordination function remains structurally primary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(separation_of_powers_vs_abdication, conceptual, 'Coordination function versus performative abdication').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__judicial_ambiguity_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ccm_jar_tr_t0, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(ccm_jar_tr_t5, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement(ccm_jar_tr_t10, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 10, 0.4).
narrative_ontology:measurement(ccm_jar_tr_t15, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 15, 0.45).
narrative_ontology:measurement(ccm_jar_tr_t20, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement(ccm_jar_tr_t25, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 25, 0.5).
narrative_ontology:measurement(ccm_jar_tr_t30, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 30, 0.52).

% Extraction over time
narrative_ontology:measurement(ccm_jar_be_t0, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(ccm_jar_be_t5, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(ccm_jar_be_t10, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(ccm_jar_be_t15, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 15, 0.35).
narrative_ontology:measurement(ccm_jar_be_t20, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 20, 0.37).
narrative_ontology:measurement(ccm_jar_be_t25, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 25, 0.39).
narrative_ontology:measurement(ccm_jar_be_t30, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 30, 0.4).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(copyright_constitutional_mandate__judicial_ambiguity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_constitutional_mandate__judicial_ambiguity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__judicial_ambiguity_reading, public_scaffold_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__judicial_ambiguity_reading, corporate_enclosure_reading).

% DUAL FORMULATION NOTE:
% This story is one member of the copyright_constitutional_mandate constraint family. The judicial_ambiguity_reading is linked to its siblings because the doctrine of rational basis review structurally influences the feasibility of both the public scaffold and corporate enclosure readings. Decomposition was necessary because the three readings have different epsilon values, different beneficiary/victim structures, and different failure modes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
