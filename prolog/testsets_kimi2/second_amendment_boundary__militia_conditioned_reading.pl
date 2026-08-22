% ============================================================================
% CONSTRAINT STORY: second_amendment_boundary__militia_conditioned_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_boundary__militia_conditioned_reading, []).

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
 *   constraint_id: second_amendment_boundary__militia_conditioned_reading
 *   human_readable: Second Amendment Militia-Conditioned Regulatory Framework
 *   domain: constitutional/law/political
 *
 * SUMMARY:
 *   This constraint instantiates the militia-conditioned reading of the
 *   Second Amendment kernel: the claim that the prefatory clause 'A well
 *   regulated Militia, being necessary to the security of a free State'
 *   defines the scope of the operative clause, bounding the right to 'keep
 *   and bear Arms' to collective defense contexts and thereby permitting
 *   comprehensive democratic regulation of private firearms possession. Under
 *   this reading, state regulatory authority is constitutionally presumed
 *   legitimate, private possession is subject to means-end scrutiny against
 *   the militia purpose, and the firearms market is exposed to democratic
 *   restriction. The constraint extracts heavily from gun
 *   ownersâparticularly collectors and self-defense claimants in
 *   high-regulation jurisdictionsâwhile coordinating collective security
 *   through a unified regulatory framework. It is actively enforced through
 *   judicial review, legislative prohibition, and criminal policing. As a
 *   kernel reading, it is mutually exclusive with the individual-right
 *   reading (which rejects the prefatory clause as scope-defining) and the
 *   insurrectionist reading (which locates the right in anti-tyranny
 *   resistance rather than state-regulated collective defense).
 *
 * KEY AGENTS:
 *   - federal_judiciary (institutional/analytical): agenda-setter that adjudicates the constitutional boundary
 *   - state_legislators (institutional/constrained): set regulatory agendas and benefit from expanded constitutional authority
 *   - law_enforcement (organized/constrained): enforce restrictions and gain operational advantages
 *   - public_safety_advocates (organized/mobile): benefit from regulatory capacity without bearing costs
 *   - restricted_gun_owners (moderate/constrained): bear regulatory burden and criminal penalties
 *   - firearms_collectors (moderate/constrained): specialized payers facing categorical bans
 *   - self_defense_claimants (powerless/constrained): individual payers in high-regulation jurisdictions
 *   - firearms_industry (powerful/constrained): faces market restrictions and liability exposure
 *   - constitutional_scholars (analytical/analytical): observer seat providing interpretive framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_boundary__militia_conditioned_reading, 0.62).
domain_priors:suppression_score(second_amendment_boundary__militia_conditioned_reading, 0.78).
domain_priors:theater_ratio(second_amendment_boundary__militia_conditioned_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__militia_conditioned_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_boundary__militia_conditioned_reading, "Second Amendment Militia-Conditioned Regulatory Framework").
narrative_ontology:topic_domain(second_amendment_boundary__militia_conditioned_reading, "constitutional/law/political").

domain_priors:requires_active_enforcement(second_amendment_boundary__militia_conditioned_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__militia_conditioned_reading, '43b408ea-4199-4088-aa7f-1e8ee03acc44').
narrative_ontology:cs_kernel_codification('43b408ea-4199-4088-aa7f-1e8ee03acc44', fixed_text).
narrative_ontology:cs_authority_grounding('43b408ea-4199-4088-aa7f-1e8ee03acc44', lineage).
narrative_ontology:cs_interpretation_layer_present('43b408ea-4199-4088-aa7f-1e8ee03acc44').
narrative_ontology:cs_reading_relation('43b408ea-4199-4088-aa7f-1e8ee03acc44', second_amendment_boundary__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('43b408ea-4199-4088-aa7f-1e8ee03acc44', second_amendment_boundary__insurrectionist_reading, forecloses).
narrative_ontology:cs_axiom('43b408ea-4199-4088-aa7f-1e8ee03acc44', foundational, prefatory_clause_defines_scope).
narrative_ontology:cs_axiom_status(prefatory_clause_defines_scope, holdable).
narrative_ontology:cs_axiom_grounding('43b408ea-4199-4088-aa7f-1e8ee03acc44', prefatory_clause_defines_scope, conventional).
narrative_ontology:cs_axiom('43b408ea-4199-4088-aa7f-1e8ee03acc44', foundational, comprehensive_regulation_permitted).
narrative_ontology:cs_axiom_status(comprehensive_regulation_permitted, holdable).
narrative_ontology:cs_axiom_grounding('43b408ea-4199-4088-aa7f-1e8ee03acc44', comprehensive_regulation_permitted, conventional).
narrative_ontology:cs_reference_frame('43b408ea-4199-4088-aa7f-1e8ee03acc44', collective_defense_constitutional_order).
narrative_ontology:cs_drift_state('43b408ea-4199-4088-aa7f-1e8ee03acc44', post_heller_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('43b408ea-4199-4088-aa7f-1e8ee03acc44', '').
narrative_ontology:cs_kernel_id(second_amendment_boundary__militia_conditioned_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, state_legislators).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, law_enforcement).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, public_safety_advocates).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, restricted_gun_owners).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, firearms_collectors).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, self_defense_claimants).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, firearms_industry).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicates Second Amendment challenges and determines whether firearms regulations fall within the militia-conditioned scope. Their interpretive choices directly enable or disable state regulatory authority, though individual judges operate within doctrinal precedent and political-appointment constraints.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Exercise expanded constitutional authority to enact comprehensive firearms regulation, background checks, and possession restrictions justified under the militia-conditioned framework. Their political survival depends on constituent support and electoral cycles, not on the constraint's persistence per se.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, state_legislators, agenda_setter,
    institutional, generational, constrained, national).

% Enforce firearms restrictions and benefit operationally from a legal framework that criminalizes unregulated possession. Their safety and tactical posture improve with reduced civilian armament, but they do not control the constitutional interpretation itself.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, law_enforcement, agenda_setter,
    organized, biographical, constrained, national).

% Advance policy goals of reducing gun violence through regulatory measures. They benefit from a constitutional interpretation that removes barriers to legislation they favor, without bearing the direct costs of restriction or enforcement.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, public_safety_advocates, beneficiary,
    organized, generational, mobile, national).

% Ordinary firearm owners subject to licensing, registration, assault weapons bans, and carry restrictions justified under the militia framework. Their ability to possess, transport, and use firearms is curtailed by regulatory requirements and criminal penalties for noncompliance.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, restricted_gun_owners, payer,
    moderate, biographical, constrained, national).

% Collect historical or technical firearms that fall under ban categories or regulatory schemes. Their collecting activity is restricted by classification of certain weapons as non-militia-relevant or by blanket prohibitions that do not distinguish collector purpose.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, firearms_collectors, payer,
    moderate, biographical, constrained, national).

% Individuals in high-regulation jurisdictions who seek firearms for personal protection but face restrictive permitting regimes justified by the collective-defense framing. Their individual need is treated as subordinate to regulatory goals and militia-relevance tests.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, self_defense_claimants, payer,
    powerless, immediate, constrained, local).

% Manufacturers and retailers facing restricted markets, product bans, and liability exposure under a regulatory regime that treats commercial firearms distribution as subject to comprehensive democratic control rather than individual right.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, firearms_industry, payer,
    powerful, biographical, constrained, national).

% Analyze and debate the original meaning, textual structure, and doctrinal implications of the Second Amendment. They occupy an interpretive seat outside the immediate policy stakes, though their scholarship influences judicial and legislative discourse.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_boundary__militia_conditioned_reading, diffuse).
narrative_ontology:fixing_cost_class(second_amendment_boundary__militia_conditioned_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective security by permitting democratically enacted firearms restrictions justified under a constitutional collective-defense framework, establishing a unified legal standard for lethal weapon possession across jurisdictions and reducing free-rider problems in public safety provision.
% TRANSFER_FUNCTION: Transfers regulatory authority and physical disarmament from individual gun owners, collectors, and the firearms industry to state legislatures and law enforcement, justified by the collective militia purpose and subject to democratic oversight.
% ABSENT_VOICES: Individual gun owners in high-regulation jurisdictions who lack organized representation; rural communities for whom militia service and individual possession are culturally continuous; abolitionist voices arguing for complete civilian disarmament who find the militia framework too permissive; and historians who emphasize the amendment's slave-patrol origins but are excluded from mainstream constitutional framing.
% DISAPPEARANCE_RATIONALE: If the militia-conditioned reading vanished overnight, existing assault weapons bans, may-issue permitting regimes, and comprehensive background check systems would lose their constitutional justification. State legislatures would face renewed constitutional challenges, previously prohibited categories of possession would be re-litigated, and the firearms market would reorganize around an expanded individual right or unregulated insurrectionist framework.
% FOUNDING_PROBLEM: The founding generation needed to balance state power to organize armed defense against the risk of federal standing army tyranny, while ensuring frontier security and, in some jurisdictions, slave patrol functions.
% FOUNDING_PROBLEM_CORROBORATION: Professional historians corroborate the militia-centric original understanding and the slave-patrol dimension. Constitutional scholars outside the gun-control advocacy community note the dual founding purposes. Gun rights scholars dispute that the problem justifies modern comprehensive regulation, arguing the original militia was universally understood to encompass individual possession. No neutral party attests unanimity of purpose.
narrative_ontology:disappearance_verdict(second_amendment_boundary__militia_conditioned_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_boundary__militia_conditioned_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_boundary__militia_conditioned_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_boundary__militia_conditioned_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_boundary__militia_conditioned_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_boundary__militia_conditioned_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_boundary__militia_conditioned_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_boundary__militia_conditioned_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the substantial restriction on acquisition, possession, and carriage imposed by comprehensive regulatory regimes justified under this reading. Suppression (0.78) is high because the constraint depends on criminal enforcement, licensing denials, and judicial suppression of individual-right claims. Theater ratio (0.25) is relatively low because much enforcement is substantive (background checks, prohibited-person categories), though some performative compliance exists. Accessibility collapse (0.82) is high: once the militia-conditioned framework is accepted as constitutional doctrine, legal alternatives to regulation collapse (unrestricted individual possession is ruled out as a constitutional alternative). Resistance (0.80) is high due to sustained opposition from gun-rights organizations, judicial challenges, and political mobilization. The temporal measurements trace a trajectory of intensifying extraction through the late twentieth century, followed by modest decline post-Heller (2008) as the reading lost doctrinal dominance, with corresponding theater-ratio increase as the reading shifted from controlling doctrine to dissenting and academic performance.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (judiciary, legislators, law enforcement) experience this constraint as a legitimate coordination framework for public safety and democratic order. The payer seats (gun owners, collectors, industry) experience it as structural extraction of their property and liberty interests. The engine will compute divergent per-seat classifications: from the state_legislators seat the constraint may appear as rope or scaffold, while from the self_defense_claimants seat it computes as snare or tangled_rope. This divergence is the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   State legislators and law enforcement are structural beneficiaries (low d): the constraint expands their authority and operational safety. Public safety advocates are diffuse beneficiaries (low d, mobile exit). Restricted gun owners, collectors, and self-defense claimants are structural targets (high d): they bear the compliance costs, licensing burdens, and criminal penalties. The firearms industry is a target despite powerful global standing because its exit is constrained by product bans and liability rules specific to this regulatory domain. The federal judiciary sits near the beneficiary end as administrator, though its analytical exit preserves neutrality.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resists mandatrophy mislabeling because it carries a live coordination function (collective security, democratic regulation of lethal weapons) alongside identifiable asymmetric extraction (gun owners bear costs, state gains authority). It is not a pure snare because the public safety justification is structurally separable from the extraction; it is not a pure rope because the victim set is non-empty and the enforcement is coercive. The R5 genealogy interview records a contested founding problem (collective defense versus slave patrol), preventing the coordination story from being accepted as an unexamined origin myth.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_foreclosure,
    'Does the militia_conditioned_reading logically foreclose the individual_right_reading and insurrectionist_reading, or do all three coexist as live positions in constitutional discourse?',
    'Jurisprudential analysis of whether a single judicial framework can simultaneously hold that the prefatory clause defines scope and that the operative clause establishes an unconditioned individual right.',
    'If foreclosed, the kernel is a zero-sum interpretive contest where adoption of one reading structurally eliminates the others. If coexisting, the kernel permits plural valid readings held by different institutional actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Structural relationship between competing second amendment readings').

omega_variable(
    collective_defense_efficacy,
    'Does a militia-conditioned regulatory framework produce measurable public safety benefits that justify the extraction from gun owners, or does it primarily displace possession without reducing harm?',
    'Comparative criminological study of jurisdictions with militia-conditioned regulatory regimes versus individual-right regimes, controlling for socioeconomic variables.',
    'If efficacy is low, the coordination story is cover for extraction; if high, the tangled rope classification is strengthened by validated coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_defense_efficacy, empirical, 'Empirical test of the coordination claim underlying militia-conditioned regulation').

omega_variable(
    prefatory_clause_semantic_status,
    'Is the prefatory clause grammatically and historically a conditional scope-delimiter on the operative clause, or merely a non-binding purposive statement?',
    'Linguistic analysis of eighteenth-century English usage of absolute constructions combined with historical corpus analysis of ratification-era legal drafting.',
    'If the prefatory clause is merely purposive, the militia-conditioned reading collapses into the individual-right reading; if it is scope-defining, the individual-right reading is structurally blocked.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prefatory_clause_semantic_status, empirical, 'Grammatical and historical ambiguity of the second amendment prefatory clause').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__militia_conditioned_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(seco_tr_t10, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(seco_tr_t20, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(seco_tr_t30, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 30, 0.25).
narrative_ontology:measurement(seco_tr_t40, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement(seco_tr_t50, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement(seco_tr_t60, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 60, 0.48).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(seco_be_t10, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(seco_be_t20, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(seco_be_t30, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 30, 0.66).
narrative_ontology:measurement(seco_be_t40, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 40, 0.6).
narrative_ontology:measurement(seco_be_t50, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 50, 0.58).
narrative_ontology:measurement(seco_be_t60, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 60, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(seco_su_t10, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(seco_su_t20, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(seco_su_t30, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 30, 0.78).
narrative_ontology:measurement(seco_su_t40, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 40, 0.82).
narrative_ontology:measurement(seco_su_t50, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 50, 0.75).
narrative_ontology:measurement(seco_su_t60, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 60, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_boundary__militia_conditioned_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_boundary__militia_conditioned_reading, second_amendment_boundary__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__militia_conditioned_reading, second_amendment_boundary__insurrectionist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three structurally distinct readings of the second_amendment_boundary kernel. The individual_right_reading and insurrectionist_reading share the same referent (the Second Amendment text) but instantiate different constraints with different epsilon values, beneficiary structures, and victim sets. Decomposition follows the epsilon-invariance principle: these are not the same constraint viewed from different angles but distinct structural claims linked by their common textual source.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
