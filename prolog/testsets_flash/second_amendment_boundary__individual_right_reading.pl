% ============================================================================
% CONSTRAINT STORY: second_amendment_boundary__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_boundary__individual_right_reading, []).

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
 *   constraint_id: second_amendment_boundary__individual_right_reading
 *   human_readable: Second Amendment: Individual Right Reading
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This constraint represents the 'individual right' reading of the Second
 *   Amendment, where the right to keep and bear arms is understood as a
 *   fundamental individual right, largely unconditioned by the prefatory
 *   militia clause. This interpretation has gained significant legal
 *   traction, particularly since the late 20th century, leading to a
 *   constitutional shielding of private firearm possession and a high bar for
 *   state-level regulation. It is one reading of the
 *   'second_amendment_boundary' kernel, distinct from the
 *   'militia_conditioned_reading' and 'insurrectionist_reading'.
 *
 * KEY AGENTS:
 *   - firearms_owners: Primary beneficiary (organized/identity_locked) — benefits from constitutional protection, resists regulation.
 *   - firearms_manufacturers_and_retailers: Primary beneficiary (powerful/arbitrage) — benefits from shielded market, funds advocacy.
 *   - victims_of_gun_violence: Primary payer (powerless/trapped) — bears direct costs, advocates for stricter control.
 *   - public_safety_advocates: Payer (organized/constrained) — bears costs of legislative gridlock, seeks policy change.
 *   - state_and_local_governments: Payer (institutional/constrained) — responsible for public safety, faces legal challenges to regulation.
 *   - supreme_court: Agenda setter (institutional/analytical) — defines the scope of the right through rulings.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_boundary__individual_right_reading, 0.65).
domain_priors:suppression_score(second_amendment_boundary__individual_right_reading, 0.75).
domain_priors:theater_ratio(second_amendment_boundary__individual_right_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_boundary__individual_right_reading, "Second Amendment: Individual Right Reading").
narrative_ontology:topic_domain(second_amendment_boundary__individual_right_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_boundary__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__individual_right_reading, '368e3d1b-17ed-4522-9080-264e5532a182').
narrative_ontology:cs_kernel_codification('368e3d1b-17ed-4522-9080-264e5532a182', fixed_text).
narrative_ontology:cs_authority_grounding('368e3d1b-17ed-4522-9080-264e5532a182', lineage).
narrative_ontology:cs_interpretation_layer_present('368e3d1b-17ed-4522-9080-264e5532a182').
narrative_ontology:cs_reading_relation('368e3d1b-17ed-4522-9080-264e5532a182', second_amendment_boundary__militia_conditioned_reading, forecloses).
narrative_ontology:cs_reading_relation('368e3d1b-17ed-4522-9080-264e5532a182', second_amendment_boundary__insurrectionist_reading, coexists_with).
narrative_ontology:cs_axiom('368e3d1b-17ed-4522-9080-264e5532a182', foundational, individual_right_unconditioned_by_militia).
narrative_ontology:cs_axiom_status(individual_right_unconditioned_by_militia, holdable).
narrative_ontology:cs_axiom_grounding('368e3d1b-17ed-4522-9080-264e5532a182', individual_right_unconditioned_by_militia, deontological).
narrative_ontology:cs_axiom('368e3d1b-17ed-4522-9080-264e5532a182', secondary, firearms_as_ordinary_instruments).
narrative_ontology:cs_axiom_status(firearms_as_ordinary_instruments, holdable).
narrative_ontology:cs_axiom_grounding('368e3d1b-17ed-4522-9080-264e5532a182', firearms_as_ordinary_instruments, conventional).
narrative_ontology:cs_reference_frame('368e3d1b-17ed-4522-9080-264e5532a182', post_heller_jurisprudence).
narrative_ontology:cs_drift_state('368e3d1b-17ed-4522-9080-264e5532a182', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('368e3d1b-17ed-4522-9080-264e5532a182', '').
narrative_ontology:cs_kernel_id(second_amendment_boundary__individual_right_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__individual_right_reading, firearms_owners).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__individual_right_reading, firearms_manufacturers_and_retailers).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, victims_of_gun_violence).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, public_safety_advocates).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, state_and_local_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the constitutional protection of private firearm ownership, viewing it as a fundamental liberty. They actively resist regulations that infringe upon this right, often through political organization and litigation. For many, firearm ownership is deeply tied to personal and cultural identity.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, firearms_owners, beneficiary,
    organized, biographical, identity_locked, national).

% Benefit from a constitutionally shielded market for firearms, which limits regulatory burdens and expands sales opportunities. They fund advocacy groups and lobbying efforts to maintain and expand this interpretation of the Second Amendment.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, firearms_manufacturers_and_retailers, beneficiary,
    powerful, generational, arbitrage, national).

% Bear the direct costs of gun violence, including injury, death, and trauma. They advocate for stricter gun control measures, but their efforts are often outmatched by the political power of gun rights advocates and the constitutional protections afforded to firearms.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, victims_of_gun_violence, payer,
    powerless, immediate, trapped, local).

% Work to reduce gun violence through legislative and policy changes. They view the individual right reading as an impediment to effective public safety measures and bear the costs of legislative gridlock and the societal impact of gun violence.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, public_safety_advocates, payer,
    organized, generational, constrained, national).

% Are responsible for public safety and often seek to enact gun control measures tailored to local needs. They face legal challenges and political pressure when attempting to regulate firearms, bearing the costs of litigation and the inability to fully address gun violence within their jurisdictions.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, state_and_local_governments, payer,
    institutional, generational, constrained, national).

% The ultimate arbiter of constitutional meaning, whose rulings define the scope of the Second Amendment. Its interpretations shape the legal landscape for firearms policy, effectively setting the agenda for what regulations are permissible.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, supreme_court, agenda_setter,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for individual self-defense and a check on potential government overreach, coordinating the rights of citizens to possess firearms for various lawful purposes.
% TRANSFER_FUNCTION: Transfers the burden of gun violence and the costs of limited regulation from firearms owners and the industry to victims, public safety advocates, and state/local governments. It also transfers political power to organized gun rights groups.
% ABSENT_VOICES: Future generations who will inherit the consequences of current firearms policy, and those who are silenced by gun violence itself, are absent from the direct constitutional interpretation process. Their interests are represented by advocates, but they lack direct agency.
% DISAPPEARANCE_RATIONALE: If this individual right reading vanished, the legal landscape for firearms would fundamentally shift. State and local governments would gain significantly more power to regulate firearms, likely leading to a patchwork of stricter laws. The firearms industry would face increased regulatory burdens, and public safety advocates would see a major barrier removed. The entire political and legal discourse around gun control would reorganize.
% FOUNDING_PROBLEM: The Second Amendment was adopted to ensure the security of a free state, with the right of the people to keep and bear arms being seen as essential to that security, particularly in the context of a citizen militia.
% FOUNDING_PROBLEM_CORROBORATION: Historians and legal scholars outside the immediate beneficiary groups attest that the founding problem involved both individual and collective defense, but the precise balance and scope of the right have been contested since its inception. The individual right reading gained prominence through modern Supreme Court jurisprudence, not solely from original intent.
narrative_ontology:disappearance_verdict(second_amendment_boundary__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_boundary__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_boundary__individual_right_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(second_amendment_boundary__individual_right_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_boundary__individual_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_boundary__individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_boundary__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high because the individual right reading imposes significant costs on society in terms of gun violence and limits on public safety measures, while benefiting specific groups. Suppression (0.75) is also high, as this reading actively suppresses alternative regulatory approaches and the political will to enact them, primarily through judicial enforcement and organized lobbying. The theater ratio (0.20) is relatively low, indicating that while there's some performative aspect to 'defense of liberty' rhetoric, the constraint's core function (shielding gun ownership) is genuinely active and enforced. The rising extractiveness and suppression over time reflect the increasing judicial and political entrenchment of this reading.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of firearms owners and the industry, this is a legitimate constitutional protection (closer to a Rope or even Mountain). From the perspective of victims and public safety advocates, it operates as a Snare or Tangled Rope, extracting a high price in lives and safety while suppressing effective countermeasures. The Supreme Court, as the agenda-setter, experiences it as a complex legal challenge with profound societal implications.
 *
 * DIRECTIONALITY LOGIC:
 *   Firearms owners and the industry are clear beneficiaries (low d) as the constraint directly protects and enables their activities. Victims of gun violence, public safety advocates, and state/local governments are targets (high d) as they bear the costs and face limitations imposed by this reading. The Supreme Court, while an agenda-setter, has an 'analytical' exit and a more balanced directionality, as its role is to interpret, not directly benefit or suffer from, the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling by highlighting the active enforcement and identifiable beneficiaries and victims. It's not a Piton because it's actively defended and provides clear benefits to specific groups, nor a Mountain because its 'naturalness' is a contested legal interpretation, not an inherent physical law. The 'contested' status of the founding problem further underscores that its persistence is not due to an unmet original need, but rather ongoing political and legal contestation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    individual_vs_collective_right,
    'Is the Second Amendment primarily an individual right, or is it fundamentally conditioned by the prefatory militia clause, implying a collective or militia-related right?',
    'Further Supreme Court jurisprudence clarifying the relationship between the operative and prefatory clauses, or a constitutional amendment.',
    'If resolved as primarily collective, the constraint''s extractiveness and suppression would decrease significantly for public safety advocates, as state regulation would be more permissible. If resolved as an even stronger individual right, extractiveness and suppression would increase further.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(individual_vs_collective_right, conceptual, 'Ambiguity regarding the scope and nature of the Second Amendment right.').

omega_variable(
    societal_cost_of_unrestricted_access,
    'What is the quantifiable societal cost (lives, healthcare, economic disruption) directly attributable to the legal protections afforded by this individual right reading, compared to alternative regulatory regimes?',
    'Comprehensive, longitudinal epidemiological and economic studies comparing outcomes across jurisdictions with varying firearms regulations, controlling for confounding factors.',
    'Robust evidence of high, avoidable societal costs would strengthen arguments for reinterpreting the right or enacting new legislation, potentially shifting the constraint''s perceived extractiveness and legitimacy. Lack of clear evidence would reinforce the status quo.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(societal_cost_of_unrestricted_access, empirical, 'Empirical uncertainty regarding the full societal impact of the individual right reading.').

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''individual_right_reading'' of the ''second_amendment_boundary'' kernel, or does it conflate elements of other readings?',
    'Expert review by constitutional scholars specializing in Second Amendment jurisprudence, comparing the detailed structural claims against the established contours of each reading.',
    'Misidentification would lead to inaccurate classification and an inability to properly model the interactions between sibling readings. Correct identification ensures the integrity of the kernel analysis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Ensuring precise identification of this specific reading within the Second Amendment kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__individual_right_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1970, second_amendment_boundary__individual_right_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(seco_tr_t1980, second_amendment_boundary__individual_right_reading, theater_ratio, 1980, 0.12).
narrative_ontology:measurement(seco_tr_t1990, second_amendment_boundary__individual_right_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(seco_tr_t2000, second_amendment_boundary__individual_right_reading, theater_ratio, 2000, 0.17).
narrative_ontology:measurement(seco_tr_t2010, second_amendment_boundary__individual_right_reading, theater_ratio, 2010, 0.19).
narrative_ontology:measurement(seco_tr_t2024, second_amendment_boundary__individual_right_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(seco_be_t1970, second_amendment_boundary__individual_right_reading, base_extractiveness, 1970, 0.3).
narrative_ontology:measurement(seco_be_t1980, second_amendment_boundary__individual_right_reading, base_extractiveness, 1980, 0.4).
narrative_ontology:measurement(seco_be_t1990, second_amendment_boundary__individual_right_reading, base_extractiveness, 1990, 0.5).
narrative_ontology:measurement(seco_be_t2000, second_amendment_boundary__individual_right_reading, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(seco_be_t2010, second_amendment_boundary__individual_right_reading, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement(seco_be_t2024, second_amendment_boundary__individual_right_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1970, second_amendment_boundary__individual_right_reading, suppression_requirement, 1970, 0.4).
narrative_ontology:measurement(seco_su_t1980, second_amendment_boundary__individual_right_reading, suppression_requirement, 1980, 0.5).
narrative_ontology:measurement(seco_su_t1990, second_amendment_boundary__individual_right_reading, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(seco_su_t2000, second_amendment_boundary__individual_right_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(seco_su_t2010, second_amendment_boundary__individual_right_reading, suppression_requirement, 2010, 0.72).
narrative_ontology:measurement(seco_su_t2024, second_amendment_boundary__individual_right_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_boundary__individual_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, firearms_market_regulation).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, public_safety_legislation).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, self_defense_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'second_amendment_boundary' kernel. Each reading has a different structural impact and classification. This file models the 'individual_right_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
