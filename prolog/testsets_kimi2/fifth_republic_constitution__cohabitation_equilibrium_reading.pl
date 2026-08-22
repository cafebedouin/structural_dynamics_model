% ============================================================================
% CONSTRAINT STORY: fifth_republic_constitution__cohabitation_equilibrium_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fifth_republic_constitution__cohabitation_equilibrium_reading, []).

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
 *   constraint_id: fifth_republic_constitution__cohabitation_equilibrium_reading
 *   human_readable: Fifth Republic Dual Executive Cohabitation Equilibrium
 *   domain: constitutional_law/political_systems
 *
 * SUMMARY:
 *   This constraint story captures the cohabitation equilibrium reading of
 *   the French Fifth Republic constitution: when the president and the
 *   parliamentary majority belong to opposing camps, executive authority
 *   splits along a foreign-domestic axis, forcing negotiated cohabitation.
 *   The reading treats this arrangement as a structural constraint on both
 *   executives rather than as presidential supremacy or parliamentary
 *   subordination. It is one reading of a contested constitutional kernel;
 *   sibling readings include hyper-presidentialism (minimal legislative
 *   constraint on the president) and parliamentary constraint (president as
 *   an executive requiring legislative authorization).
 *
 * KEY AGENTS:
 *   - President: Structural beneficiary of foreign policy domain control (institutional/constrained)
 *   - Prime Minister: Structural beneficiary of domestic policy domain control (institutional/constrained)
 *   - Parliamentary Majority: Structural beneficiary that secures the premiership and domestic agenda (organized/constrained)
 *   - French Citizens: Structural target bearing policy incoherence and accountability blur (moderate/constrained)
 *   - Administrative State: Structural target navigating dual chains of command (moderate/constrained)
 *   - Constitutional Council: Agenda setter interpreting and enforcing domain boundaries (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.55).
domain_priors:suppression_score(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.52).
domain_priors:theater_ratio(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__cohabitation_equilibrium_reading, tangled_rope).
narrative_ontology:human_readable(fifth_republic_constitution__cohabitation_equilibrium_reading, "Fifth Republic Dual Executive Cohabitation Equilibrium").
narrative_ontology:topic_domain(fifth_republic_constitution__cohabitation_equilibrium_reading, "constitutional_law/political_systems").

domain_priors:requires_active_enforcement(fifth_republic_constitution__cohabitation_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__cohabitation_equilibrium_reading, 'f6025c78-7fee-4da8-960a-7a1b8d60bee6').
narrative_ontology:cs_kernel_codification('f6025c78-7fee-4da8-960a-7a1b8d60bee6', fixed_text).
narrative_ontology:cs_authority_grounding('f6025c78-7fee-4da8-960a-7a1b8d60bee6', lineage).
narrative_ontology:cs_interpretation_layer_present('f6025c78-7fee-4da8-960a-7a1b8d60bee6').
narrative_ontology:cs_reading_relation('f6025c78-7fee-4da8-960a-7a1b8d60bee6', fifth_republic_constitution__hyper_presidential_reading, forecloses).
narrative_ontology:cs_reading_relation('f6025c78-7fee-4da8-960a-7a1b8d60bee6', fifth_republic_constitution__parliamentary_constraint_reading, coexists_with).
narrative_ontology:cs_axiom('f6025c78-7fee-4da8-960a-7a1b8d60bee6', foundational, dual_executive_legitimacy).
narrative_ontology:cs_axiom_status(dual_executive_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('f6025c78-7fee-4da8-960a-7a1b8d60bee6', dual_executive_legitimacy, conventional).
narrative_ontology:cs_axiom('f6025c78-7fee-4da8-960a-7a1b8d60bee6', foundational, domain_separation_as_constitutional_convention).
narrative_ontology:cs_axiom_status(domain_separation_as_constitutional_convention, holdable).
narrative_ontology:cs_axiom_grounding('f6025c78-7fee-4da8-960a-7a1b8d60bee6', domain_separation_as_constitutional_convention, conventional).
narrative_ontology:cs_reference_frame('f6025c78-7fee-4da8-960a-7a1b8d60bee6', dual_executive_domain_equilibrium).
narrative_ontology:cs_drift_state('f6025c78-7fee-4da8-960a-7a1b8d60bee6', post_quinquennat_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f6025c78-7fee-4da8-960a-7a1b8d60bee6', '').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__cohabitation_equilibrium_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, president).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, prime_minister).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, parliamentary_majority).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, french_citizens).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, administrative_state).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retains constitutional control over foreign policy, defense, and EU strategy during periods of cohabitation, avoiding total loss of executive authority despite an opposing parliamentary majority; must accept loss of domestic policy dominance and share executive power with a prime minister from the hostile majority.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, president, beneficiary,
    institutional, biographical, constrained, national).

% Directs domestic policy and government administration during cohabitation, backed by a National Assembly majority; gains executive autonomy in domestic domains but must respect presidential domains rÃ©servÃ©s and negotiate on overlapping competences.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, prime_minister, beneficiary,
    institutional, biographical, constrained, national).

% Their legislative election victory creates the cohabitation condition and secures the prime minister's appointment; they obtain control over the domestic legislative agenda and government composition despite presidential opposition.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, parliamentary_majority, beneficiary,
    organized, biographical, constrained, national).

% Elect the president and National Assembly separately; when these elections produce opposing majorities, they experience divided executive leadership, inconsistent policy direction, and blurred accountability between the president and prime minister.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, french_citizens, payer,
    moderate, biographical, constrained, national).

% The civil service and administrative apparatus receive competing directives from the president on foreign and EU affairs and from the prime minister on domestic governance; must navigate dual chains of command and inconsistent policy signals during cohabitation.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, administrative_state, payer,
    moderate, biographical, constrained, national).

% Interprets and enforces the constitutional allocation of legislative and regulatory authority between the president and parliament or government; adjudicates disputes over domain boundaries but generally avoids intervening in the political management of cohabitation.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, constitutional_council, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fifth_republic_constitution__cohabitation_equilibrium_reading, diffuse).
narrative_ontology:fixing_cost_class(fifth_republic_constitution__cohabitation_equilibrium_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents total governmental paralysis when the president and the parliamentary majority belong to opposing political camps by allocating distinct policy domainsâforeign affairs, defense, and EU strategy to the president, domestic policy to the prime ministerâand requiring negotiation over shared competences.
% TRANSFER_FUNCTION: Moves policy control and executive initiative between the president and the prime minister depending on the policy domain, while transferring the costs of policy incoherence and divided accountability to the electorate and the administrative state.
% ABSENT_VOICES: Advocates for a fully parliamentary or fully presidential regime are marginalized in mainstream constitutional discourse; the electorate is not consulted on the constitutional division of authority during cohabitation and must accept the divided executive as a byproduct of separate electoral choices.
% DISAPPEARANCE_RATIONALE: Without the cohabitation equilibrium, either the president would dominate all policy domains and trigger a constitutional crisis, or the prime minister would claim full executive authority, collapsing the Fifth Republic's hybrid architecture into a different regime type.
% FOUNDING_PROBLEM: The Fourth Republic's parliamentary instability and the need for strong executive leadership without permitting presidential dictatorship; the 1958 constitution sought to combine authority with democratic legitimacy by creating a dual executive adaptable to divided electoral outcomes.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians and comparative political scientists outside the benefiting political coalitions attest that the problem of governability under divided government persists, though the cohabitation mechanism is only one solution among many parliamentary and presidential alternatives; the Constitutional Council does not corroborate the founding problem status independently of the constitutional text.
narrative_ontology:disappearance_verdict(fifth_republic_constitution__cohabitation_equilibrium_reading, world_rearranges).
narrative_ontology:founding_problem_status(fifth_republic_constitution__cohabitation_equilibrium_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__cohabitation_equilibrium_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fifth_republic_constitution__cohabitation_equilibrium_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fifth_republic_constitution__cohabitation_equilibrium_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fifth_republic_constitution__cohabitation_equilibrium_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fifth_republic_constitution__cohabitation_equilibrium_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.55) because the domain split genuinely prevents deadlock but imposes persistent policy incoherence costs on the public and administration. Suppression is moderate (0.52) because the arrangement depends on constitutional jurisprudence and elite political convention to suppress alternatives such as presidential dissolution of the assembly or prime ministerial claims to full authority. Theater ratio is moderate-low (0.25): the domain allocation is functionally real during cohabitation, though both executives perform symbolic assertions of supremacy. Accessibility collapse is high (0.70) because exiting the Fifth Republic framework requires constitutional replacement, which is structurally blocked. Resistance is moderate (0.45) because each cohabitation period generates boundary disputes, yet the framework has held across multiple episodes.
 *
 * PERSPECTIVAL GAP:
 *   The president and prime minister experience the constraint as a functional division that preserves their respective authority domains while limiting their reach; the engine should compute low directionality for these seats. French citizens and the administrative state experience it as an accountability blur and operational incoherence; the engine should compute high directionality for these payer seats. The Constitutional Council occupies an analytical seat with no directional stake in the extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   President, prime minister, and parliamentary majority are declared structural beneficiaries because each gains a defined domain of control under cohabitation rather than facing total victory or defeat. French citizens and the administrative state are declared victims because they bear the costs of policy incoherence and dual command without receiving offsetting domain benefits. Exit is constrained for all political actors because the constitutional framework is rigid; citizens cannot opt out of the Fifth Republic's electoral calendar.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâFourth Republic instabilityâwas addressed by the broader Fifth Republic architecture, not exclusively by cohabitation. The cohabitation equilibrium is a secondary adaptation that activates under divided government. Because it still functions when triggered (1986â1988, 1993â1995, 1997â2002), it has not atrophied into a piton, though the 2000 quinquennat reform reduced its activation frequency. The genuine coordination (preventing executive deadlock) remains live, preventing pure snare classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_validity,
    'Is the cohabitation equilibrium a constitutionally mandated structure or merely a political convention layered onto ambiguous constitutional text?',
    'Systematic review of Conseil Constitutionnel jurisprudence on domain allocation and comparative constitutional analysis tracking the written text against political practice.',
    'If convention-only, the constraint''s extraction derives from political practice rather than legal enforcement, potentially lowering suppression and shifting classification toward rope. If text-mandated, the higher accessibility collapse and enforcement profile support tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_validity, conceptual, 'Whether cohabitation is textual or conventional').

omega_variable(
    policy_incoherence_welfare_cost,
    'What is the measurable governance cost of the dual executive''s policy incoherence during cohabitation compared to unified government?',
    'Policy output studies, legislative productivity metrics, and expert governance ratings comparing cohabitation and unified government periods.',
    'High measurable coherence costs would validate the current extractiveness score; low costs would suggest the coordination benefit dominates and extraction is overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_incoherence_welfare_cost, empirical, 'Quantifiable cost of policy incoherence').

omega_variable(
    term_alignment_extinction,
    'Has the 2000 reduction of the presidential term to five years functionally extinguished cohabitation, rendering this constraint obsolete?',
    'Electoral data and coalition formation analysis comparing pre- and post-2000 divided government frequency, plus constitutional amendment trajectory.',
    'If cohabitation is now vanishingly rare, the constraint may have drifted toward piton statusâa latent structure maintained by inertia without active function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(term_alignment_extinction, empirical, 'Whether quinquennat eliminated cohabitation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__cohabitation_equilibrium_reading, 0, 66).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fift_tr_t0, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(fift_tr_t20, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(fift_tr_t28, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 28, 0.2).
narrative_ontology:measurement(fift_tr_t35, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 35, 0.22).
narrative_ontology:measurement(fift_tr_t39, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 39, 0.25).
narrative_ontology:measurement(fift_tr_t44, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 44, 0.35).
narrative_ontology:measurement(fift_tr_t54, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 54, 0.3).
narrative_ontology:measurement(fift_tr_t66, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 66, 0.25).

% Extraction over time
narrative_ontology:measurement(fift_be_t0, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(fift_be_t20, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(fift_be_t28, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 28, 0.55).
narrative_ontology:measurement(fift_be_t35, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 35, 0.58).
narrative_ontology:measurement(fift_be_t39, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 39, 0.62).
narrative_ontology:measurement(fift_be_t44, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 44, 0.5).
narrative_ontology:measurement(fift_be_t54, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 54, 0.48).
narrative_ontology:measurement(fift_be_t66, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 66, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(fift_su_t0, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(fift_su_t20, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(fift_su_t28, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 28, 0.65).
narrative_ontology:measurement(fift_su_t35, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 35, 0.62).
narrative_ontology:measurement(fift_su_t39, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 39, 0.68).
narrative_ontology:measurement(fift_su_t44, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 44, 0.45).
narrative_ontology:measurement(fift_su_t54, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 54, 0.42).
narrative_ontology:measurement(fift_su_t66, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 66, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
