% ============================================================================
% CONSTRAINT STORY: fifth_republic_constitution__hyper_presidential_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fifth_republic_constitution__hyper_presidential_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: fifth_republic_constitution__hyper_presidential_reading
 *   human_readable: Fifth Republic Constitution — Hyper-Presidential Reading
 *   domain: constitutional_law/political_systems/comparative_government
 *
 * SUMMARY:
 *   The 1958 French Constitution established a semi-presidential system
 *   designed to cure Fourth Republic instability. The hyper-presidential
 *   reading — dominant during periods of presidential majority (1958-1986,
 *   1988-1993, 1995-1997, 2002-2012, 2017-present) — treats the president as
 *   the direct embodiment of national will, with Parliament reduced to
 *   ratification chamber. Article 49.3 (government engages responsibility to
 *   pass bill without vote) and Article 16 (emergency powers) are the
 *   enforcement mechanisms that convert constitutional text into operational
 *   extraction from the legislature. The constraint is a tangled rope: it
 *   genuinely coordinates stable executive governance (coordination function)
 *   while asymmetrically extracting legislative autonomy from Parliament
 *   (extraction function), requiring active enforcement via constitutional
 *   provisions. The 2000 quinquennat (five-year presidential term aligned
 *   with legislative term) reduced cohabitation frequency but intensified
 *   presidential dominance, raising extractiveness and theater ratio over the
 *   interval.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__hyper_presidential_reading, 0.78).
domain_priors:suppression_score(fifth_republic_constitution__hyper_presidential_reading, 0.72).
domain_priors:theater_ratio(fifth_republic_constitution__hyper_presidential_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__hyper_presidential_reading, tangled_rope).
narrative_ontology:human_readable(fifth_republic_constitution__hyper_presidential_reading, "Fifth Republic Constitution — Hyper-Presidential Reading").
narrative_ontology:topic_domain(fifth_republic_constitution__hyper_presidential_reading, "constitutional_law/political_systems/comparative_government").

domain_priors:requires_active_enforcement(fifth_republic_constitution__hyper_presidential_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__hyper_presidential_reading, '18893698-fe5a-4e85-bd89-9624b532f924').
narrative_ontology:cs_kernel_codification('18893698-fe5a-4e85-bd89-9624b532f924', fixed_text).
narrative_ontology:cs_authority_grounding('18893698-fe5a-4e85-bd89-9624b532f924', lineage).
narrative_ontology:cs_interpretation_layer_present('18893698-fe5a-4e85-bd89-9624b532f924').
narrative_ontology:cs_reading_relation('18893698-fe5a-4e85-bd89-9624b532f924', fifth_republic_constitution__parliamentary_constraint_reading, coexists_with).
narrative_ontology:cs_reading_relation('18893698-fe5a-4e85-bd89-9624b532f924', fifth_republic_constitution__cohabitation_equilibrium_reading, coexists_with).
narrative_ontology:cs_axiom('18893698-fe5a-4e85-bd89-9624b532f924', foundational, president_embodies_national_will_directly).
narrative_ontology:cs_axiom_status(president_embodies_national_will_directly, holdable).
narrative_ontology:cs_axiom_grounding('18893698-fe5a-4e85-bd89-9624b532f924', president_embodies_national_will_directly, conventional).
narrative_ontology:cs_axiom('18893698-fe5a-4e85-bd89-9624b532f924', secondary, parliamentary_legitimacy_is_derivative).
narrative_ontology:cs_axiom_status(parliamentary_legitimacy_is_derivative, holdable).
narrative_ontology:cs_axiom_grounding('18893698-fe5a-4e85-bd89-9624b532f924', parliamentary_legitimacy_is_derivative, conventional).
narrative_ontology:cs_reference_frame('18893698-fe5a-4e85-bd89-9624b532f924', gaullist_founding_moment_1958).
narrative_ontology:cs_drift_state('18893698-fe5a-4e85-bd89-9624b532f924', contemporary_fifth_republic, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('18893698-fe5a-4e85-bd89-9624b532f924', '').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__hyper_presidential_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__hyper_presidential_reading, presidency_as_institution).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__hyper_presidential_reading, incumbent_president).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, national_assembly).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, senate).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__hyper_presidential_reading, french_electorate).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, prime_minister).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, french_electorate).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, opposition_parties).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__hyper_presidential_reading, gaullist_constitutional_design).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__hyper_presidential_reading, direct_election_legitimacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Directly elected for five-year term; controls executive agenda, appoints prime minister, can dissolve National Assembly, invokes Article 49.3 to pass legislation without vote and Article 16 for emergency powers. Collects institutional authority and policy control; exit is term limit or electoral defeat.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, incumbent_president, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__hyper_presidential_reading, incumbent_president, beneficiary).

% The office itself accumulates precedent, institutional memory, and control over state apparatus across incumbents. Benefits from the constitutional architecture that concentrates executive initiative and emergency authority in the presidency. No exit — the institution persists across officeholders.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, presidency_as_institution, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(fifth_republic_constitution__hyper_presidential_reading, presidency_as_institution).

% Lower house of Parliament; nominally sovereign in legislation but subjected to Article 49.3 (government can force bill passage without vote unless motion of censure passes) and presidential dissolution power. When president's party holds majority, Assembly functions as rubber stamp; during cohabitation, regains initiative but remains constrained by presidential agenda-setting. Exit is electoral cycle but structural position is fixed by constitution.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, national_assembly, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__hyper_presidential_reading, national_assembly, excluded).

% Upper house with limited legislative veto (National Assembly has final word); indirectly elected, providing conservative bias. Cannot be dissolved. Bears extraction when presidential agenda overrides legislative process; its role reduced to delaying chamber. Exit is institutional — cannot leave the constitutional structure.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, senate, payer,
    organized, generational, constrained, national).

% Appointed by president, accountable to National Assembly. When president's party controls Assembly, PM is subordinate executor of presidential will; during cohabitation, PM becomes genuine head of government. Bears political cost of implementing presidential agenda; exit is resignation or dismissal by president.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, prime_minister, agenda_setter,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__hyper_presidential_reading, prime_minister, payer).

% Constitutional review body; members appointed by president, Assembly president, Senate president. Reviews constitutionality of laws and Article 49.3/16 invocations. Has struck down some executive overreach but generally validates presidential practice. Interprets the kernel; its jurisprudence shapes the constraint's operational boundaries.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, constitutional_council, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__hyper_presidential_reading, constitutional_council, agenda_setter).

% Elects president directly (five-year term since 2002) and National Assembly. Gains stable executive and clear accountability line; bears cost when presidential dominance weakens parliamentary representation and minority voices. Exit is emigration or abstention — neither alters structural position.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, french_electorate, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__hyper_presidential_reading, french_electorate, payer).

% Parties not holding presidency or Assembly majority. Subjected to Article 49.3 without recourse; censure motions rarely succeed due to party discipline. Their legislative amendments routinely rejected; speaking time limited. No structural exit — must wait for electoral realignment or cohabitation.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, opposition_parties, payer,
    moderate, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__hyper_presidential_reading, opposition_parties, excluded).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides stable, decisive executive authority capable of governing without legislative paralysis — solves the Fourth Republic's chronic government instability by concentrating initiative in a directly elected president.
% TRANSFER_FUNCTION: Moves legislative initiative and policy control from Parliament to the presidency; transfers the cost of executive dominance to legislative bodies (via Article 49.3 forced passage, dissolution threat) and to opposition forces (via agenda exclusion).
% ABSENT_VOICES: Territorial collectivities (regions, departments) whose autonomy is constrained by centralized presidential administration; overseas territories with distinct constitutional status but no veto; citizens in presidential election runoff who voted against the winner but are governed as if the president embodies unified national will.
% DISAPPEARANCE_RATIONALE: If the hyper-presidential reading vanished overnight, the Fifth Republic would revert to parliamentary or semi-presidential logic: prime minister would become genuine head of government, Article 49.3 would become exceptional rather than routine, cohabitation would become the normal equilibrium, and legislative-executive relations would require continuous negotiation rather than presidential decree.
% FOUNDING_PROBLEM: The Fourth Republic's parliamentary instability — 24 governments in 12 years, inability to manage decolonization crises (Algeria), legislative fragmentation preventing coherent executive action.
% FOUNDING_PROBLEM_CORROBORATION: Gaullist tradition and constitutional drafters (Debré, Michel) attest the founding problem was executive instability and the solution was strong presidency. Political scientists (Duverger, Huber, Elgie) and opposition constitutionalists attest the founding problem was substantially solved by 1962 direct election reform, and the current hyper-presidential operation exceeds the 1958 design — the 2000 quinquennat reform (aligning presidential and legislative terms) reduced cohabitation but intensified presidential dominance beyond the founding settlement.
narrative_ontology:disappearance_verdict(fifth_republic_constitution__hyper_presidential_reading, world_rearranges).
narrative_ontology:founding_problem_status(fifth_republic_constitution__hyper_presidential_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__hyper_presidential_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fifth_republic_constitution__hyper_presidential_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fifth_republic_constitution__hyper_presidential_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fifth_republic_constitution__hyper_presidential_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fifth_republic_constitution__hyper_presidential_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fifth_republic_constitution__hyper_presidential_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because presidential control of agenda, appointments, and legislative fast-track (49.3) transfers policy authority from elected legislature to single officeholder. Suppression (0.72) is high because the constraint's persistence depends on constitutional provisions that structurally disadvantage Parliament — dissolution threat, 49.3, presidential appointment of PM — not on voluntary compliance. Theater ratio (0.42) is moderate: parliamentary debate is real but increasingly performative when presidential majority exists; the 49.3 procedure stages a 'debate-then-override' ritual. Accessibility collapse (0.68) reflects that alternatives (parliamentary government, cohabitation equilibrium) exist but are structurally suppressed by term alignment and party discipline. Resistance (0.55) is moderate: Parliament resists via amendments, committee work, and rare censure motions, but structural position limits effectiveness.
 *
 * PERSPECTIVAL GAP:
 *   From the president's seat (agenda_setter/beneficiary), the constraint is genuine coordination — stable governance, clear accountability, decisive action. From the National Assembly's seat (payer/excluded), the same structure is extraction — legislative sovereignty hollowed out, reduced to registration chamber. From the Prime Minister's seat (agenda_setter/payer), the experience oscillates: subordinate executor during presidential majority, genuine head of government during cohabitation. The engine computes this divergence from the structural power/exit/scope data authored above.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent president and presidency as institution are structural beneficiaries (d near 0.0) — they collect authority, control agenda, face minimal constraint. National Assembly and Senate are structural targets (d near 1.0) — they bear the transfer of legislative initiative, their constitutional powers are overridden by 49.3/16, exit is constrained by fixed terms and indirect election. Prime Minister is dual: d shifts from ~0.3 (beneficiary of executive power) during presidential majority to ~0.7 (target of presidential dominance) during cohabitation. Opposition parties are trapped targets (d ~0.9). Electorate sits near symmetric (d ~0.5) — gains stability, loses parliamentary representation. Constitutional Council is analytical observer (d ~0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Fourth Republic instability) was live in 1958 and substantially solved by the 1962 direct election reform. The hyper-presidential reading persists by treating the emergency solution as permanent architecture — the mandate has atrophied but the constraint extracts via institutional inertia and the 2000 quinquennat reform that reduced the very cohabitation mechanism that provided democratic correction. This is not a scaffold (no sunset clause) nor a piton (presidency actively benefits and maintains the arrangement). It is a tangled rope: coordination function (stable executive) remains real but extraction (legislative subordination) has grown beyond the founding settlement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_framing,
    'Does the hyper-presidential reading represent the 1958 Constitution''s genuine design, or a post-hoc construction by presidential majorities to legitimize accumulated power?',
    'Comparative analysis of 1958 constitutional debates (Debré, Michel, Gaullist records) vs. subsequent institutional practice; jurisprudence of Constitutional Council on Article 49.3 and 16 scope.',
    'If the reading is post-hoc construction, the constraint is a false summit (claimed coordination masking extraction); if genuine design, the extraction is the price of the coordination function the kernel was built to deliver.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Whether the hyper-presidential reading is faithful to the kernel''s founding design or an expansive interpretation serving incumbent interests.').

omega_variable(
    article_49_3_coordination_extraction_boundary,
    'Is Article 49.3 a genuine coordination mechanism (preventing legislative paralysis) or an extraction tool (forcing executive bills without negotiation)?',
    'Empirical analysis of 49.3 usage: proportion used on controversial vs. consensus bills; success rate of censure motions; policy outcomes with vs. without 49.3.',
    'If primarily coordination, the constraint leans rope; if primarily extraction, it leans snare. The tangled_rope classification depends on this boundary being genuinely mixed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(article_49_3_coordination_extraction_boundary, empirical, 'Whether the enforcement mechanism at the heart of this constraint serves coordination or extraction as its primary function.').

omega_variable(
    cohabitation_as_correction_or_anomaly,
    'Are cohabitation periods (1986-88, 1993-95, 1997-2002) the constitution''s normal equilibrium revealing its semi-presidential design, or anomalies corrected by the 2000 quinquennat?',
    'Constitutional history: 1958 text does not mention cohabitation; Gaullist intent was presidential dominance. But 1986-2002 practice established cohabitation as functional equilibrium. The 2000 reform (quinquennat) explicitly aimed to reduce cohabitation.',
    'If cohabitation is the true equilibrium, the hyper-presidential reading is a deviation (extraction); if cohabitation is anomaly, the hyper-presidential reading is the kernel''s intended operation (coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cohabitation_as_correction_or_anomaly, conceptual, 'Whether the constitutional design''s true coordination function is presidential dominance or negotiated dual executive.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the legislature''s subordination structural (constitutional text, party discipline, electoral calendar) or internalized (deputies self-censor, accept presidential primacy as legitimate)?',
    'Post-cohabitation behavior analysis: when Assembly majority opposes president, does it fully exercise its powers or remain constrained by internalized presidential supremacy? Survey data on deputy self-conception.',
    'If internalized, effective suppression exceeds structural measure — the constraint persists even when structural pressure relaxes. This would increase the constraint''s extraction durability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the legislative target.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__hyper_presidential_reading, 0, 65).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fift_tr_t0, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(fift_tr_t15, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 15, 0.3).
narrative_ontology:measurement(fift_tr_t30, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(fift_tr_t45, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 45, 0.38).
narrative_ontology:measurement(fift_tr_t60, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 60, 0.41).
narrative_ontology:measurement(fift_tr_t65, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 65, 0.42).

% Extraction over time
narrative_ontology:measurement(fift_be_t0, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(fift_be_t15, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(fift_be_t30, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(fift_be_t45, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 45, 0.73).
narrative_ontology:measurement(fift_be_t60, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 60, 0.76).
narrative_ontology:measurement(fift_be_t65, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 65, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(fift_su_t0, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(fift_su_t15, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 15, 0.55).
narrative_ontology:measurement(fift_su_t30, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(fift_su_t45, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 45, 0.7).
narrative_ontology:measurement(fift_su_t60, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 60, 0.71).
narrative_ontology:measurement(fift_su_t65, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 65, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_republic_constitution__hyper_presidential_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fifth_republic_constitution__hyper_presidential_reading, 0.12).
narrative_ontology:affects_constraint(fifth_republic_constitution__hyper_presidential_reading, fifth_republic_constitution__parliamentary_constraint_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__hyper_presidential_reading, fifth_republic_constitution__cohabitation_equilibrium_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the Fifth Republic Constitution kernel into three structurally distinct readings. The hyper-presidential reading (this story) has high extractiveness (0.78) and active enforcement (49.3, Art. 16). The parliamentary_constraint_reading has lower extractiveness and treats 49.3 as exceptional. The cohabitation_equilibrium_reading models the dual-executive periods as the constraint's true coordination function. They share the same kernel (1958 Constitution) but instantiate different constraints with different ε, beneficiaries, and victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fifth_republic_constitution__hyper_presidential_reading, institutional, 0.15).
constraint_indexing:directionality_override(fifth_republic_constitution__hyper_presidential_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
