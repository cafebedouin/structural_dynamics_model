% ============================================================================
% CONSTRAINT STORY: fifth_republic_constitution__parliamentary_constraint_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fifth_republic_parliamentary, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: fifth_republic_constitution__parliamentary_constraint_reading
 *   human_readable: Fifth Republic Constitutional Requirement: Presidential Executive Authority Contingent on Legislative Confidence
 *   domain: constitutional_law/political_systems
 *
 * SUMMARY:
 *   The Fifth Republic Constitution (1958) establishes a semi-presidential
 *   system in which the President holds executive authority but that
 *   authority is contingent on retaining the confidence of the legislative
 *   Assembly. This constraint embodies one reading of the Constitution's core
 *   kernel: the President as a coordinated executive whose policy
 *   implementation requires legislative authorization and whose tenure
 *   depends on Assembly confidence. This reading emphasizes the parliamentary
 *   accountability mechanisms — the no-confidence vote (Article 49.2), the
 *   legislative override of executive vetoes, and the requirement that the
 *   government present itself to the Assembly. Under this reading, when the
 *   Assembly withdraws confidence or blocks legislation, the President enters
 *   the victim set: formal authority collides with constitutional constraint.
 *   The sibling readings — hyper-presidential (President as direct sovereign
 *   minimally constrained by legislature) and cohabitation_equilibrium (dual
 *   executive with negotiated authority allocation) — represent alternative
 *   interpretations of the same constitutional kernel, each with different
 *   beneficiary structures and extraction profiles. This story generates ONLY
 *   the parliamentary_constraint_reading; the siblings are separate
 *   constraint stories.
 *
 * KEY AGENTS:
 *   - President: Formal head of executive branch; derives authority from popular election but is constitutionally required to govern with legislative confidence; when the Assembly withdraws support or blocks legislation, the President cannot unilaterally override the constraint without violating the constitutional framework (victim set under this reading)
 *   - Legislative Assembly: Primary repository of sovereign legislative authority; exercises no-confidence votes, approves budgets, passes legislation that the President cannot veto unilaterally; the beneficiary of the constraint insofar as it preserves legislative primacy over policy implementation
 *   - Prime Minister: Head of government; formally appointed by the President but must retain Assembly confidence to govern; under this reading, the PM is the President's subordinate who manages the legislative relationship on behalf of the executive; sits between the two executive heads
 *   - Legislative majority: The organized bloc of deputies whose confidence sustains the executive; benefits from the constraint by maintaining legislative control over government formation and policy direction
 *   - Constitutional Court: Interprets the constitutional boundaries of executive authority; adjudicates disputes between the President and Assembly over the scope of each institution's powers; the court's rulings define the enforceable limits of this constraint
 *   - Electorate: Provides popular legitimacy to the President and, separately, to the Assembly; under this reading, two sources of democratic legitimacy must coordinate for policy implementation — neither can act unilaterally
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__parliamentary_constraint_reading, 0.18).
domain_priors:suppression_score(fifth_republic_constitution__parliamentary_constraint_reading, 0.12).
domain_priors:theater_ratio(fifth_republic_constitution__parliamentary_constraint_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__parliamentary_constraint_reading, rope).
narrative_ontology:human_readable(fifth_republic_constitution__parliamentary_constraint_reading, "Fifth Republic Constitutional Requirement: Presidential Executive Authority Contingent on Legislative Confidence").
narrative_ontology:topic_domain(fifth_republic_constitution__parliamentary_constraint_reading, "constitutional_law/political_systems").

domain_priors:requires_active_enforcement(fifth_republic_constitution__parliamentary_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__parliamentary_constraint_reading, '42a07609-f239-439b-ad28-b768aa5e2202').
narrative_ontology:cs_kernel_codification('42a07609-f239-439b-ad28-b768aa5e2202', formalized).
narrative_ontology:cs_authority_grounding('42a07609-f239-439b-ad28-b768aa5e2202', lineage).
narrative_ontology:cs_interpretation_layer_present('42a07609-f239-439b-ad28-b768aa5e2202').
narrative_ontology:cs_reading_relation('42a07609-f239-439b-ad28-b768aa5e2202', fifth_republic_constitution__hyper_presidential_reading, forecloses).
narrative_ontology:cs_reading_relation('42a07609-f239-439b-ad28-b768aa5e2202', fifth_republic_constitution__cohabitation_equilibrium_reading, coexists_with).
narrative_ontology:cs_axiom('42a07609-f239-439b-ad28-b768aa5e2202', foundational, presidential_authority_contingent_on_assembly_confidence).
narrative_ontology:cs_axiom_status(presidential_authority_contingent_on_assembly_confidence, holdable).
narrative_ontology:cs_axiom_grounding('42a07609-f239-439b-ad28-b768aa5e2202', presidential_authority_contingent_on_assembly_confidence, conventional).
narrative_ontology:cs_axiom('42a07609-f239-439b-ad28-b768aa5e2202', foundational, dual_democratic_legitimacy_requires_coordination).
narrative_ontology:cs_axiom_status(dual_democratic_legitimacy_requires_coordination, holdable).
narrative_ontology:cs_axiom_grounding('42a07609-f239-439b-ad28-b768aa5e2202', dual_democratic_legitimacy_requires_coordination, deontological).
narrative_ontology:cs_reference_frame('42a07609-f239-439b-ad28-b768aa5e2202', constitutional_parliamentary_accountability).
narrative_ontology:cs_drift_state('42a07609-f239-439b-ad28-b768aa5e2202', contemporary_unified_government_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('42a07609-f239-439b-ad28-b768aa5e2202', '').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__parliamentary_constraint_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__parliamentary_constraint_reading, legislative_majority).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__parliamentary_constraint_reading, constitutional_democracy).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__parliamentary_constraint_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(fifth_republic_constitution__parliamentary_constraint_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fifth_republic_constitution__parliamentary_constraint_reading_tests).
:- end_tests(fifth_republic_constitution__parliamentary_constraint_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the constraint does not systematically transfer resources or authority from one party to another in the way a snare or even a tangled rope would. Instead, it coordinates two sources of democratic legitimacy (presidential popular election and legislative representation) by requiring them to align on policy. The President cannot extract rents by executing policy against legislative will; the Assembly cannot extract by blocking all executive initiative because the President retains the power to dissolve the Assembly and force new elections. Suppression is minimal (0.12) because the constraint operates through constitutional procedure and normative commitment rather than coercive enforcement. There is no police force making Presidents obey the Assembly; instead, Presidents internalize the constitutional norm that they govern through legislative confidence. Theater rises over the measurement interval (0.18 → 0.24 → 0.22) because the constraint's performative aspect grows: parliamentary procedures (question time, no-confidence debates, legislative scrutiny) become more elaborate and more visible as a way of demonstrating constitutional adherence, while the actual blocking of executive policy may decline under unified government. The measurement series share one time grid (every metric authored at t=0,5,10,15,20,25) so the engine can sample them jointly. Accessibility of alternatives is high (0.78) for the Assembly but lower for the President: if the President is blocked by the Assembly, the alternatives are limited (new legislative elections, constitutional amendment, or constraint violation); the Assembly's alternative to the President is simply voting no-confidence. Resistance is moderate (0.35) because the constraint is mostly accepted as legitimate but the President routinely pushes its boundaries through executive decree, emergency powers, and the appointment of loyal judges.
 *
 * PERSPECTIVAL GAP:
 *   The computational divergence between seats should be substantial. From the legislative majority's seat, the constraint is a rope — genuine coordination that solves the problem of reconciling popular presidential authority with parliamentary representation. From the President's seat, the constraint appears more extractive (d higher, effective extraction higher) because it prevents unilateral action on policy that the President believes the electorate authorized them to pursue. From the Prime Minister's seat, the constraint is more extractive still (d highest) because the PM is caught between two principals with conflicting interests. The engine computes per-seat classification from these directionality values; where the computed types diverge sharply, that divergence reflects the genuine perspectival asymmetry of the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Under this reading, directionality differs sharply across seats. The legislative majority (beneficiary) has d near 0.0 (full beneficiary): the constraint preserves their power over government formation and policy. The President has d near 0.6 (symmetric-to-slightly-victimized): the President retains significant powers (decree, appointment, foreign policy initiative) but cannot implement domestic policy against legislative will. The Prime Minister has d near 0.75 (closer to victim): formally subordinate to both the President and the Assembly, the PM must satisfy both and is vulnerable to dismissal by either. The Constitutional Court has d near 0.5 (analytical): the court interprets the constraint but does not benefit or suffer from it directly. The electorate has d near 0.5: they provide legitimacy to both branches but do not directly experience extraction or benefit from the constraint itself. The engine derives these directionality values from the beneficiary/victim declarations and exit options; this commentary explains the structural logic behind the differentiation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — coordination between two sources of democratic legitimacy (presidential election and legislative representation) — remains live under this reading. The constraint persists because both the President and Assembly continue to draw legitimacy from the electorate and both institutions benefit from the arrangement's existence. Mandatrophy would arise if one branch unilaterally dissolved or subordinated the other, but the constitutional text and practiced norms both preserve the dual legitimacy structure. However, the theater_ratio rising over time (0.18 → 0.24) suggests that the procedural performance of parliamentary accountability may grow more elaborate as the substantive blocking of executive policy declines under unified government. If theater continues to rise while extractiveness remains low, the constraint risks becoming a ritual display of parliamentary power without material effect — a transition toward piton status. This measurement series will flag whether that drift occurs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_vs_hyper_presidential_core_premise,
    'Is the President''s authority grounded in direct democratic legitimacy independent of legislative approval (hyper-presidential reading), or is it constitutionally contingent on legislative confidence even when the President carries genuine popular mandate?',
    'Jurisprudential examination of Constitutional Court rulings during cohabitation crises and votes of no-confidence; comparison of executive directive power deployed under unified vs. divided government; analysis of whether Presidents have claimed unilateral authority claims in their constitutional speeches.',
    'If this reading is correct, a President with majority popular support but Assembly opposition remains constitutionally constrained and enters the victim set when blocked; the hyper-presidential reading would place the President in the beneficiary set as an unchecked sovereign. The two readings foreclose each other within a single constitutional framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_hyper_presidential_core_premise, conceptual, 'Whether presidential authority derives from direct legitimacy or constitutional contingency.').

omega_variable(
    cohabitation_vs_parliamentary_structural_symmetry,
    'During cohabitation, is the constraint symmetrically applied (President and Prime Minister both subordinate to Assembly authority on policy implementation — parliamentary reading), or is executive authority allocated through negotiated power-sharing between co-equals (cohabitation_equilibrium_reading)?',
    'Historical case analysis of cohabitation periods (1986–1988, 1993–1995, 1997–2002): Who blocked whom? Did the Assembly exercise no-confidence against the executive, or did the President negotiate cabinet composition? Did the President deploy constitutional emergency powers (Article 16), and did the Assembly or courts restrain them?',
    'If negotiated allocation is the norm during cohabitation, the constraint operates as influence (the Assembly''s constitutional authority creates negotiation pressure) rather than direct accountability. If Assembly confidence remains the deciding gate, the parliamentary reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cohabitation_vs_parliamentary_structural_symmetry, empirical, 'Whether cohabitation represents constraint negotiation or constraint subordination.').

omega_variable(
    constitutional_authorization_vs_de_facto_autonomy,
    'Does the distinction between formal constitutional authorization and de facto executive autonomy in implementing policy matter for this reading''s classification? Can a President who lacks formal legislative authorization but exercises policy autonomy anyway be said to operate under a parliamentary constraint?',
    'Measurement of Presidential legislative success rates, executive decree usage, and legislative override rates across the Fifth Republic''s history; comparison of formal authority (written in the Constitution) vs. practice (actual enforcement of the constraint).',
    'If practice diverges systematically from formal authority, the constraint''s extractiveness may rise and theater may increase; the reading''s empirical status would shift from ''holdable'' to ''overridden'' if the President routinely operates outside legislative authorization.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_authorization_vs_de_facto_autonomy, empirical, 'Whether formal constitutional constraint aligns with practiced executive autonomy.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the constraint''s low measured suppression (0.12) a result of structural enforcement (legislative procedures that reliably block unauthorized executive action), or has the constraint become internalized — Presidents self-censor because they have adopted the constitutional framers'' normative commitment to parliamentary accountability, even when they possess de facto autonomy to act otherwise?',
    'Post-constraint analysis: if a President were to systematically defy legislative authorization requirements and face institutional resistance (Assembly votes of no-confidence, Constitutional Court rulings, civil service compliance breakdowns), the suppression is structural. If Presidents continue to self-censor even after one breaks the norm without immediate penalty, suppression is partially internalized.',
    'If suppression is internalized, the constraint''s effective strength is higher than the scalar measure suggests — it persists through shared constitutional commitment rather than enforcement machinery. If structural, the constraint depends on legislative majority vigilance; a change in Assembly composition could weaken it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Suppression mechanism: structural enforcement vs. internalized constitutional commitment.').

omega_variable(
    kernel_reading_identity_vs_historical_moment,
    'This reading identifies a specific interpretation of the Fifth Republic''s constitutional kernel — the executive is authorized and constrained by legislative confidence. But the kernel''s meaning has shifted across three distinct historical periods: de Gaulle''s presidency (assertion of unchecked executive authority), the cohabitation crises (negotiated dual authority), and contemporary unified government (strong executive under majority). Is this ONE reading, or are the shifts evidence of a contested kernel where different Presidents read the Constitution differently?',
    'Constitutional jurisprudence analysis: do Fifth Republic Constitutional Court rulings converge on a single reading of the President''s authority structure, or do they explicitly endorse multiple readings as contextually valid? Examine public statements by Constitutional scholars, judicial opinions, and Presidential speeches.',
    'If the kernel admits multiple legitimate readings within the same constitutional framework (as the cohabitation literature suggests), then the reading classification shifts from ''one constraint'' to ''one seat''s interpretation of a contested kernel''. The constraint''s extractiveness and beneficiary structure would be perspectival — different seats read the same Constitution differently, and their divergent readings instantiate different constraints.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_identity_vs_historical_moment, conceptual, 'Whether this is a unified reading or a perspective-dependent interpretation of a multivalent kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__parliamentary_constraint_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fr_parl_tr_t0, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(fr_parl_tr_t5, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 5, 0.19).
narrative_ontology:measurement(fr_parl_tr_t10, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 10, 0.21).
narrative_ontology:measurement(fr_parl_tr_t15, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 15, 0.23).
narrative_ontology:measurement(fr_parl_tr_t20, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement(fr_parl_tr_t25, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 25, 0.22).

% Extraction over time
narrative_ontology:measurement(fr_parl_be_t0, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 0, 0.16).
narrative_ontology:measurement(fr_parl_be_t5, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 5, 0.17).
narrative_ontology:measurement(fr_parl_be_t10, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 10, 0.18).
narrative_ontology:measurement(fr_parl_be_t15, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 15, 0.19).
narrative_ontology:measurement(fr_parl_be_t20, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 20, 0.19).
narrative_ontology:measurement(fr_parl_be_t25, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 25, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(fr_parl_su_t0, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(fr_parl_su_t5, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 5, 0.11).
narrative_ontology:measurement(fr_parl_su_t10, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 10, 0.12).
narrative_ontology:measurement(fr_parl_su_t15, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 15, 0.13).
narrative_ontology:measurement(fr_parl_su_t20, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 20, 0.13).
narrative_ontology:measurement(fr_parl_su_t25, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 25, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_republic_constitution__parliamentary_constraint_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fifth_republic_constitution__parliamentary_constraint_reading, 0.12).
narrative_ontology:affects_constraint(fifth_republic_constitution__parliamentary_constraint_reading, fifth_republic_constitution__hyper_presidential_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__parliamentary_constraint_reading, fifth_republic_constitution__cohabitation_equilibrium_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Fifth Republic's constitutional kernel. The kernel is the constitutional text and the authority structures that interpret it — it remains fixed across readings. Each reading instantiates a different constraint because the readings assign fundamentally different structural relationships: who benefits, who is constrained, what exit options exist. The three readings are distinct constraint stories linked by network.affects_constraints edges. The ε-invariance principle requires that each reading have its own ε value because changing which reading is operative changes the structural facts about beneficiaries, victims, and directionality. This story generates ONLY the parliamentary_constraint_reading; the siblings are authored separately and linked here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
