% ============================================================================
% CONSTRAINT STORY: fifth_republic_constitution__cohabitation_equilibrium_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Fifth Republic Cohabitation Equilibrium (Dual Executive Reading)
 *   domain: constitutional law / political systems / comparative government
 *
 * SUMMARY:
 *   This constraint instantiates the cohabitation equilibrium reading of the
 *   French Fifth Republic constitution: during periods of divided government,
 *   authority is split between a president and a prime minister from opposing
 *   majorities, producing a dual executive that negotiates rather than
 *   monopolizes power. The kernel is contested â the same constitutional
 *   text is read by hyper-presidentialists as concentrating sovereignty in
 *   the president, and by parliamentary constraint theorists as subordinating
 *   the president to legislative authorization. This reading treats the text
 *   as establishing a stable (if unstable) negotiated allocation. The
 *   claim/metric gap is deliberate: the constraint is claimed as a tangled
 *   rope because it coordinates against deadlock while extracting policy
 *   coherence from the electorate.
 *
 * KEY AGENTS:
 *   - President: Primary agenda-setter for foreign/defense policy (institutional/constrained) â benefits in foreign domain, pays in domestic deadlock.
 *   - Prime Minister: Primary agenda-setter for domestic policy (institutional/constrained) â benefits in domestic domain, pays in presidential encroachment.
 *   - Legislative Majority: Beneficiary (organized/constrained) â gains governing power it would lack under unified presidentialism.
 *   - Electorate: Payer (powerless/constrained) â bears policy incoherence from dual executive conflict.
 *   - Constitutional Council: Observer (institutional/analytical) â enforces the boundary through jurisprudence.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.55).
domain_priors:suppression_score(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.56).
domain_priors:theater_ratio(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 0.56).
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__cohabitation_equilibrium_reading, tangled_rope).
narrative_ontology:human_readable(fifth_republic_constitution__cohabitation_equilibrium_reading, "Fifth Republic Cohabitation Equilibrium (Dual Executive Reading)").
narrative_ontology:topic_domain(fifth_republic_constitution__cohabitation_equilibrium_reading, "constitutional law / political systems / comparative government").

domain_priors:requires_active_enforcement(fifth_republic_constitution__cohabitation_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__cohabitation_equilibrium_reading, 'dc9a37cb-9efd-4d24-8230-1b4d7fec0265').
narrative_ontology:cs_kernel_codification('dc9a37cb-9efd-4d24-8230-1b4d7fec0265', formalized).
narrative_ontology:cs_authority_grounding('dc9a37cb-9efd-4d24-8230-1b4d7fec0265', lineage).
narrative_ontology:cs_interpretation_layer_present('dc9a37cb-9efd-4d24-8230-1b4d7fec0265').
narrative_ontology:cs_reading_relation('dc9a37cb-9efd-4d24-8230-1b4d7fec0265', fifth_republic_constitution__hyper_presidential_reading, coexists_with).
narrative_ontology:cs_reading_relation('dc9a37cb-9efd-4d24-8230-1b4d7fec0265', fifth_republic_constitution__parliamentary_constraint_reading, coexists_with).
narrative_ontology:cs_axiom('dc9a37cb-9efd-4d24-8230-1b4d7fec0265', foundational, executive_authority_is_indivisibly_shared).
narrative_ontology:cs_axiom_status(executive_authority_is_indivisibly_shared, holdable).
narrative_ontology:cs_axiom_grounding('dc9a37cb-9efd-4d24-8230-1b4d7fec0265', executive_authority_is_indivisibly_shared, conventional).
narrative_ontology:cs_axiom('dc9a37cb-9efd-4d24-8230-1b4d7fec0265', foundational, cohabitation_is_constitutional_norm).
narrative_ontology:cs_axiom_status(cohabitation_is_constitutional_norm, holdable).
narrative_ontology:cs_axiom_grounding('dc9a37cb-9efd-4d24-8230-1b4d7fec0265', cohabitation_is_constitutional_norm, conventional).
narrative_ontology:cs_reference_frame('dc9a37cb-9efd-4d24-8230-1b4d7fec0265', dual_executive_constitutional_order).
narrative_ontology:cs_drift_state('dc9a37cb-9efd-4d24-8230-1b4d7fec0265', contemporary_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('dc9a37cb-9efd-4d24-8230-1b4d7fec0265', '').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__cohabitation_equilibrium_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, president).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, prime_minister).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, legislative_majority).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, electorate).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, policy_coherence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds constitutional authority over foreign policy, defense, and EU affairs. During cohabitation, must negotiate with a prime minister backed by an opposing legislative majority who controls domestic policy. Cannot unilaterally remove the PM or dissolve the Assembly without severe political cost. Benefits from foreign policy domain control but pays through domestic deadlock and encroachment on presidential legislative initiative.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, president, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__cohabitation_equilibrium_reading, president, beneficiary).

% Directs domestic policy and the government, supported by a legislative majority. During cohabitation, must share authority with a president from an opposing camp who retains foreign policy, defense, and EU prerogatives. Benefits from domestic governance autonomy but pays through presidential vetoes, foreign policy interference, and the need to negotiate on appointments and European legislation.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, prime_minister, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__cohabitation_equilibrium_reading, prime_minister, beneficiary).

% The parliamentary majority that installs and sustains the prime minister. Benefits from cohabitation because it gains effective governing power over domestic policy that would otherwise be monopolized by the president under unified government. Pays through the need to manage dual-executive conflict and negotiate with the president on shared competences.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, legislative_majority, beneficiary,
    organized, biographical, constrained, national).

% Votes separately for president and legislature, sometimes producing divided government. Bears the cost of policy incoherence when foreign and domestic policy are pulled in opposing directions by competing executives with distinct electoral mandates. Exit is limited to the next electoral cycle; no direct mechanism exists to force alignment between the two executives.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, electorate, payer,
    powerless, biographical, constrained, national).

% The abstract public good of consistent, integrated policy across foreign and domestic domains. Harmed by the structural split that assigns these domains to competing political actors with divergent agendas, producing incoherent signaling, mixed diplomatic and legislative strategies, and disjointed governance.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, policy_coherence, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(fifth_republic_constitution__cohabitation_equilibrium_reading, policy_coherence).

% Interprets the constitutional boundary between presidential and prime ministerial domains during cohabitation. Its rulings actively enforce the negotiated authority allocation by validating or invalidating government and presidential actions, thereby maintaining the dual-executive equilibrium through jurisprudential oversight.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, constitutional_council, observer,
    institutional, generational, analytical, national).

narrative_ontology:fixing_cost_class(fifth_republic_constitution__cohabitation_equilibrium_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents total governmental deadlock when the president and legislative majority belong to opposing political camps by functionally dividing authority into a presidential foreign-policy domain and a prime-ministerial domestic-policy domain.
% TRANSFER_FUNCTION: Moves effective governing authority over domestic policy from the presidency to the prime minister and assembly majority during periods of political divergence, while preserving the president's foreign policy and defense role; moves the cost of policy incoherence to the electorate.
% ABSENT_VOICES: Advocates of pure presidentialism and pure parliamentarism are structurally marginalized in the operative constitutional debate; the constitutional text itself underdetermines the precise split, and no single-executive framework is represented in the institutional design.
% DISAPPEARANCE_RATIONALE: If the cohabitation equilibrium vanished, the Fifth Republic would collapse into either hyper-presidential domination or parliamentary supremacy, triggering a constitutional crisis; foreign and domestic policy would lose their stable arbitration mechanism during divided government.
% FOUNDING_PROBLEM: The Fourth Republic's parliamentary instability and the need for strong executive leadership without succumbing to either assembly paralysis or personal dictatorship.
% FOUNDING_PROBLEM_CORROBORATION: Comparative constitutional historians and political scientists outside the benefiting parties attest that the 1958 constitution was designed for unified presidential majorities; cohabitation emerged as an unintended equilibrium rather than an intended solution. The original architects did not foresee or design the dual-executive split, corroborating that the current arrangement is a drift from the founding problem's intended resolution.
narrative_ontology:disappearance_verdict(fifth_republic_constitution__cohabitation_equilibrium_reading, world_rearranges).
narrative_ontology:founding_problem_status(fifth_republic_constitution__cohabitation_equilibrium_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__cohabitation_equilibrium_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate (0.55) because the split genuinely prevents total deadlock but produces continuous inter-executive conflict over shared competences. Suppression is moderate-high (0.56) because the equilibrium requires active constitutional interpretation, jurisprudential enforcement, and political convention to hold against either executive's natural incentive to monopolize power. Theater ratio is moderate (0.30): public assertions of presidential dignity and prime ministerial authority exceed their functional power, but real negotiation occurs behind the scenes. Accessibility collapse is moderate (0.45) â pure presidential or parliamentary alternatives exist in comparative constitutional law, but within the Fifth Republic the constraint is institutionally sticky. Resistance is moderate (0.50) because each executive constantly resists the other's encroachments. The oscillating measurement series reflects the unstable, cyclical nature of cohabitation episodes.
 *
 * PERSPECTIVAL GAP:
 *   The president's seat experiences the constraint as a necessary check that preserves a foreign-policy role against an opposing majority; the prime minister's seat experiences it as an enabling condition for domestic governance that would otherwise be impossible. The electorate's seat experiences the same structure as a source of paralysis and contradictory policy signals. The engine computes this divergence from the structural data: shared agenda-setter status with opposed domain control, plus concentrated beneficiary status for the legislative majority and payer status for the public.
 *
 * DIRECTIONALITY LOGIC:
 *   President and prime minister are hybrid seats: each is a beneficiary in their reserved domain (foreign vs. domestic) and a payer in the domain where the other encroaches. The legislative majority is a net beneficiary because cohabitation empowers it relative to unified presidentialism. The electorate and policy coherence are net targets: they bear the diffuse costs of incoherent governance without institutional recourse between elections. Directionality is therefore asymmetric across seats, with the human electorate and the abstract public good sitting nearest the full-target end.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled rope prevents mislabeling: the constraint is not a snare because it genuinely coordinates â without the dual-executive split, divided government would produce either constitutional rupture or presidential usurpation. It is not a rope because the coordination is inseparable from asymmetric extraction: policy coherence is systematically sacrificed, and the arrangement requires active enforcement (constitutional jurisprudence, mutual vetoes) to prevent either actor from collapsing the equilibrium. If the founding problem (Fourth Republic instability) is treated as still live, the equilibrium could be misread as a rope; the contested status of the founding problem, combined with measurable extraction and victimhood, anchors the tangled rope classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cohabitation_intentionality,
    'Was the cohabitation equilibrium an intended feature of the 1958 constitution, or an unintended emergent property of the text?',
    'Historical archival research on constitutional debates and drafting committee records; comparative analysis of original intent versus subsequent practice.',
    'If unintended, the constraint is a tangled rope emergent from textual ambiguity; if intended, the classification would shift toward a scaffold or rope by design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cohabitation_intentionality, empirical, 'Whether cohabitation was designed or emerged').

omega_variable(
    domain_split_exhaustiveness,
    'Does the foreign/domestic policy split exhaustively cover all governmental authority, or does a ''reserved domain'' create an unregulated zone of executive conflict?',
    'Systematic mapping of Constitutional Council jurisprudence across policy domains to identify contested or unregulated competences.',
    'If the split is incomplete, extraction is higher due to contested zones; if complete, the coordination function is more structurally genuine.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(domain_split_exhaustiveness, conceptual, 'Completeness of the authority domain split').

omega_variable(
    reading_context_stability,
    'Does the cohabitation equilibrium reading collapse into the hyper-presidential reading when the president''s party holds the legislative majority?',
    'Observe institutional behavior during unified versus divided government; measure whether the dual-executive norms persist or evaporate.',
    'If the reading only holds during divided government, it is context-dependent and less stable as an independent constraint, suggesting higher theater or piton dynamics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_context_stability, conceptual, 'Context dependency of the cohabitation reading').

omega_variable(
    abstract_victim_ontology,
    'Can an abstract good such as policy coherence be a primary victim, or must victimhood be remapped to human agents such as the electorate?',
    'Ontological framework ruling on whether non-agent entities feed directionality and classification computations.',
    'If remapped to human agents, the effective victim set expands and directionality may shift; if retained, the framework accepts abstract victims as structural placeholders.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(abstract_victim_ontology, conceptual, 'Ontological status of abstract victims').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__cohabitation_equilibrium_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fift_tr_t0, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(fift_tr_t8, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(fift_tr_t16, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 16, 0.22).
narrative_ontology:measurement(fift_tr_t24, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(fift_tr_t32, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 32, 0.3).
narrative_ontology:measurement(fift_tr_t40, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(fift_be_t0, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(fift_be_t8, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 8, 0.51).
narrative_ontology:measurement(fift_be_t16, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 16, 0.47).
narrative_ontology:measurement(fift_be_t24, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 24, 0.58).
narrative_ontology:measurement(fift_be_t32, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 32, 0.53).
narrative_ontology:measurement(fift_be_t40, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 40, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(fift_su_t0, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(fift_su_t8, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(fift_su_t16, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 16, 0.52).
narrative_ontology:measurement(fift_su_t24, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 24, 0.6).
narrative_ontology:measurement(fift_su_t32, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 32, 0.58).
narrative_ontology:measurement(fift_su_t40, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 40, 0.56).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_republic_constitution__cohabitation_equilibrium_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.1).
narrative_ontology:affects_constraint(fifth_republic_constitution__cohabitation_equilibrium_reading, hyper_presidential_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__cohabitation_equilibrium_reading, parliamentary_constraint_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the fifth_republic_constitution kernel, decomposed from the colloquial label 'Fifth Republic constitution' into three structurally distinct claims. Each reading has a different epsilon, beneficiary/victim structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
