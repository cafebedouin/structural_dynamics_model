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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Fifth Republic Dual Executive — Cohabitation Equilibrium Reading
 *   domain: constitutional_law/political_systems/comparative_government
 *
 * SUMMARY:
 *   The Fifth Republic's Constitution (1958) creates a dual executive — a
 *   directly elected president and a prime minister answerable to the
 *   National Assembly — without a single clean textual allocation of
 *   authority between them. In ordinary times (unified majority), the
 *   president effectively directs both foreign and domestic policy through a
 *   compliant prime minister. During cohabitation (1986-88, 1993-95,
 *   1997-2002), the offices are held by opposing political camps and
 *   authority must be renegotiated in practice: the president retains the
 *   'domaine réservé' of foreign affairs and defense by convention, while the
 *   prime minister governs domestically. This story models the
 *   cohabitation-equilibrium reading specifically: a structurally unstable,
 *   actor-negotiated balance where both offices genuinely constrain each
 *   other and where policy coherence itself is the casualty. This is
 *   deliberately narrower than, and structurally distinct from, the
 *   hyper-presidential reading (in which the president is treated as an
 *   unconstrained sovereign executive regardless of Assembly composition) and
 *   the parliamentary-constraint reading (in which the president is treated
 *   as requiring legislative authorization even outside cohabitation). Each
 *   reading has a different ε because each describes a different operative
 *   constraint on a different set of facts; they are linked here as sibling
 *   constraints in the fifth_republic_constitution kernel family, not as
 *   alternative measurements of one constraint.
 *
 * KEY AGENTS:
 *   - president: institutional/constrained — holds foreign-policy domain by convention, loses domestic control during cohabitation
 *   - prime_minister: institutional/constrained — holds domestic policy domain, depends on Assembly majority survival
 *   - national_assembly_majority: organized/mobile — the electoral swing that determines which cohabitation configuration exists
 *   - policy_coherence_dependent_citizens: powerless/trapped — bear the diffuse cost of split accountability
 *   - constitutional_council: institutional/analytical — arbitrates escalated boundary disputes, gains interpretive relevance from the ambiguity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.48).
domain_priors:suppression_score(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.42).
domain_priors:theater_ratio(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__cohabitation_equilibrium_reading, tangled_rope).
narrative_ontology:human_readable(fifth_republic_constitution__cohabitation_equilibrium_reading, "Fifth Republic Dual Executive — Cohabitation Equilibrium Reading").
narrative_ontology:topic_domain(fifth_republic_constitution__cohabitation_equilibrium_reading, "constitutional_law/political_systems/comparative_government").

domain_priors:requires_active_enforcement(fifth_republic_constitution__cohabitation_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__cohabitation_equilibrium_reading, 'd325c648-c266-403d-ad1d-e6c8ebee891a').
narrative_ontology:cs_kernel_codification('d325c648-c266-403d-ad1d-e6c8ebee891a', formalized).
narrative_ontology:cs_authority_grounding('d325c648-c266-403d-ad1d-e6c8ebee891a', practice).
narrative_ontology:cs_interpretation_layer_present('d325c648-c266-403d-ad1d-e6c8ebee891a').
narrative_ontology:cs_reading_relation('d325c648-c266-403d-ad1d-e6c8ebee891a', fifth_republic_constitution__hyper_presidential_reading, coexists_with).
narrative_ontology:cs_reading_relation('d325c648-c266-403d-ad1d-e6c8ebee891a', fifth_republic_constitution__parliamentary_constraint_reading, coexists_with).
narrative_ontology:cs_axiom('d325c648-c266-403d-ad1d-e6c8ebee891a', foundational, authority_allocation_is_negotiated_not_fixed).
narrative_ontology:cs_axiom_status(authority_allocation_is_negotiated_not_fixed, holdable).
narrative_ontology:cs_axiom_grounding('d325c648-c266-403d-ad1d-e6c8ebee891a', authority_allocation_is_negotiated_not_fixed, conventional).
narrative_ontology:cs_axiom('d325c648-c266-403d-ad1d-e6c8ebee891a', secondary, electoral_misalignment_triggers_genuine_power_shift).
narrative_ontology:cs_axiom_status(electoral_misalignment_triggers_genuine_power_shift, holdable).
narrative_ontology:cs_axiom_grounding('d325c648-c266-403d-ad1d-e6c8ebee891a', electoral_misalignment_triggers_genuine_power_shift, empirically_contingent).
narrative_ontology:cs_reference_frame('d325c648-c266-403d-ad1d-e6c8ebee891a', negotiated_dual_mandate_balance).
narrative_ontology:cs_drift_state('d325c648-c266-403d-ad1d-e6c8ebee891a', post_quinquennat_2000_reform, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d325c648-c266-403d-ad1d-e6c8ebee891a', '').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__cohabitation_equilibrium_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, domain_controlling_executive_actor).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, constitutional_council).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, policy_coherence_dependent_citizens).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, opposition_coalition_voters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, president).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, prime_minister).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, national_assembly_majority).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, national_assembly_majority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds direct popular mandate and controls foreign policy, defense, and the 'domaine réservé' by long-standing constitutional practice rather than explicit textual grant. During cohabitation with an opposing Assembly majority, must negotiate domestic authority with the prime minister; retains dissolution power and treaty/military prerogatives but cannot govern domestically without the government's cooperation. Cannot simply exit the arrangement short of resignation or dissolution, both costly.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, president, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__cohabitation_equilibrium_reading, president, beneficiary).

% Derives authority from Assembly confidence and, during cohabitation, from a majority opposed to the president. Controls domestic policy, the budget, and day-to-day administration through Article 20 and 21 powers. Depends on the Assembly majority remaining intact; can be forced to resign by a censure motion. Negotiates the boundary of foreign-policy involvement with the president informally, since the text does not clearly allocate it.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, prime_minister, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__cohabitation_equilibrium_reading, prime_minister, beneficiary).

% Controls the prime minister's survival through confidence votes and legislative approval. Benefits when its majority controls the government and can implement its domestic program; bears the cost of policy paralysis and blurred accountability when it is out of step with the presidency. Can in principle replace the government but cannot remove the president.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, national_assembly_majority, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__cohabitation_equilibrium_reading, national_assembly_majority, payer).

% An abstraction naming whichever of the president or prime minister currently controls a given policy domain (foreign affairs or domestic affairs) at a given moment — the actor who captures policy credit and continuity in that domain benefits from the ambiguous allocation, since the constitution does not force a clean division and each cohabitation renegotiates the line anew.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, domain_controlling_executive_actor, beneficiary,
    institutional, biographical, constrained, national).

% Adjudicates disputes over the boundary of executive authority when they escalate to justiciable questions, but most allocation disputes are resolved by political practice and precedent rather than litigation, which expands the Council's discretionary interpretive role and its institutional relevance each time ambiguity resurfaces.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, constitutional_council, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__cohabitation_equilibrium_reading, constitutional_council, beneficiary).

% Experience the practical effects of divided executive authority: policy delay, contradictory signaling on issues that cross the foreign/domestic line (trade, immigration, EU affairs), and diffuse accountability when something goes wrong, since responsibility is split between two offices with no single answerable authority. Cannot exit the constitutional order; can only vote in the next election cycle.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, policy_coherence_dependent_citizens, payer,
    powerless, biographical, trapped, national).

% Voters who elected the Assembly majority opposed to the sitting president bear the cost of policy compromise and slower implementation of their preferred domestic agenda, since the president retains formal prerogatives (dissolution, appointment ceremony, foreign policy leverage) that can obstruct or dilute their government's program even when they hold a legislative majority.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, opposition_coalition_voters, payer,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fifth_republic_constitution__cohabitation_equilibrium_reading, domain_controlling_executive_actor).
narrative_ontology:fixing_cost_class(fifth_republic_constitution__cohabitation_equilibrium_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents unchecked concentration of executive power in a single office by requiring the popularly elected president and the Assembly-confirmed prime minister to share and continuously renegotiate authority, particularly in the recurring scenario where their political affiliations diverge (cohabitation).
% TRANSFER_FUNCTION: Moves practical policymaking authority back and forth between the presidency and the premiership depending on which office currently holds an aligned majority; moves the cost of the resulting ambiguity and delay onto citizens and onto whichever political coalition currently lacks control of both offices.
% ABSENT_VOICES: Citizens experiencing the practical effects of the split (delayed reform, contradictory signaling in trade and EU negotiations) have no direct forum to contest the allocation itself — they can only vote for one office at a time and cannot vote on the boundary rule between the offices, which is set by convention and Constitutional Council precedent rather than by referendum.
% DISAPPEARANCE_RATIONALE: If the negotiated dual-executive equilibrium disappeared — replaced by, say, a single unified executive or a strict textual allocation — French governance would reorganize substantially: cohabitation crises would vanish, foreign/domestic policy responsibility would consolidate into one office, and the Constitutional Council's interpretive arbitration role over executive boundary disputes would shrink sharply.
% FOUNDING_PROBLEM: The 1958 Constitution was built to escape the perceived instability of the Fourth Republic's pure parliamentary system, which produced weak, short-lived governments, while avoiding a return to unchecked monarchical-style executive power — the dual executive was a deliberate hedge between the two failure modes.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars outside both the presidency and the premiership (comparative-government academics analyzing the 1958-1962-1986-1997-2002 cohabitation cycles) attest that the instability problem the framers targeted has been substantially solved by the five-year term alignment (quinquennat, 2000) that synchronized presidential and legislative elections and sharply reduced cohabitation's likelihood going forward — meaning the ambiguous allocation mechanism persists as a live constitutional feature addressing a problem whose acute form has receded, while incumbents in both offices continue to defend the ambiguity as necessary flexibility.
narrative_ontology:disappearance_verdict(fifth_republic_constitution__cohabitation_equilibrium_reading, world_rearranges).
narrative_ontology:founding_problem_status(fifth_republic_constitution__cohabitation_equilibrium_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__cohabitation_equilibrium_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fifth_republic_constitution__cohabitation_equilibrium_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.48, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is moderate (0.48) and unstable across the measured interval because the constraint's cost to citizens is not a steady rent but a periodic spike tied to cohabitation episodes — the temporal series shows extraction and theater both rising sharply during the 1986-97 cohabitation cluster and receding after the 2000 quinquennat reform synchronized presidential and legislative terms, reducing cohabitation's structural likelihood. Suppression is moderate (0.42): citizens cannot exit the constitutional order, but the mechanism suppressing alternatives is convention and institutional practice rather than coercive enforcement — the ambiguity persists because no actor with the power to resolve it definitively (constitutional amendment requires a difficult supermajority or referendum process) has sufficient incentive to do so while it might someday benefit them. Accessibility collapse is moderate-low (0.35) because genuine alternative allocations (explicit textual division, unified executive) remain conceptually and legally available and have been debated in constitutional reform commissions; resistance is moderate-high (0.55), reflecting recurring political and scholarly contestation over where the domaine réservé boundary actually lies.
 *
 * PERSPECTIVAL GAP:
 *   From the presidency's seat, the arrangement looks like genuine constitutional coordination — a hedge against both instability and overreach that the office defends as functioning exactly as designed. From the vantage of citizens experiencing a cohabitation episode, the same structure looks like enforced ambiguity that produces delay and diffuses accountability precisely when clarity matters most (foreign crises, EU negotiations, budget fights). The engine's per-seat computation should reflect this: the president and prime minister seats, as agenda_setters with institutional power and negotiated but real authority, likely compute closer to tangled_rope or even rope in stable periods, while the policy_coherence_dependent_citizens seat, powerless and trapped, likely computes with substantially higher effective extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Whichever of the president or prime minister controls a given policy domain at a given moment is the structural beneficiary of that domain's ambiguity — the abstraction 'domain_controlling_executive_actor' captures this rotating capture. The Constitutional Council also benefits, in the sense that its interpretive authority over unresolved boundary disputes expands each time the political actors fail to settle the allocation themselves. Citizens and the temporarily out-of-domain political coalition are the victims: they bear delay, contradictory signaling, and diffuse non-accountability, and they have no direct lever to resolve the ambiguity — only sequential elections that may or may not produce alignment.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Fourth Republic instability vs. monarchical overreach) is genuinely contested as 'live' rather than flatly 'dead': the quinquennat reform substantially reduced cohabitation's likelihood, addressing the acute instability risk, yet the underlying dual-mandate structure remains fully in force and could reactivate its cohabitation dynamics under future electoral splits (e.g., snap dissolution producing a hostile Assembly, as occurred in 2024). This is not a pure zombie mandate — the mechanism is dormant-but-live, not dead-but-persisting — which is why founding_problem_status is authored as contested rather than dead, and why the classification should not be read as full-blown extraction theater despite the moderate theater_ratio spikes during past cohabitation episodes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domaine_reserve_textual_status,
    'Is the presidential ''domaine réservé'' over foreign affairs and defense a genuine constitutional allocation, or purely a political convention with no binding textual basis?',
    'Close textual analysis of Articles 5, 15, 20, and 21 combined with a survey of Constitutional Council jurisprudence on executive boundary disputes; comparison of cohabitation-era practice across all three cohabitation episodes (1986-88, 1993-95, 1997-2002) for consistency versus ad hoc negotiation.',
    'If the domaine réservé is genuinely constitutionally grounded, the cohabitation-equilibrium reading is closer to a rope (real coordination with a stable rule); if it is pure convention re-negotiated each time, the reading is closer to a snare or unstable tangled_rope where the ''rule'' is whatever the currently stronger actor can enforce.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domaine_reserve_textual_status, conceptual, 'Whether the foreign/domestic policy split has real textual grounding or is pure negotiated convention.').

omega_variable(
    kernel_reading_divergence_which_configuration_is_normal,
    'Is the cohabitation-equilibrium reading capturing the Fifth Republic''s TRUE operative logic, or is it capturing an atypical historical episode (three cohabitations in 66 years, none since 2002) that the hyper-presidential reading more accurately generalizes from the more common unified-majority condition?',
    'Comparative time-weighted analysis: what fraction of the Fifth Republic''s history has been unified-majority versus cohabitation, and whether post-quinquennat institutional design (2000 reform) has structurally foreclosed future cohabitation or merely reduced its probability (the 2024 snap-election near-miss suggests it remains live).',
    'If cohabitation is a structurally foreclosed historical artifact, this reading describes a retired constraint and the hyper-presidential reading should be treated as the dominant operative reading going forward; if cohabitation remains a live structural possibility (as 2024 suggests), this reading captures a dormant-but-real equilibrium that could reactivate at any electoral misalignment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_divergence_which_configuration_is_normal, empirical, 'Whether cohabitation is a foreclosed historical artifact or a persistently live structural possibility within the kernel.').

omega_variable(
    policy_coherence_victim_measurability,
    'Can ''policy coherence'' as a victim category be measured independently of partisan preference — i.e., is delayed or contradictory policy genuinely a cost to citizens as such, or is it experienced as a cost only by whichever coalition currently lacks power?',
    'Survey and outcome-based analysis distinguishing generalized public dissatisfaction with governmental gridlock during cohabitation periods from partisan dissatisfaction specific to the out-of-power coalition; comparison with citizen satisfaction in unified-majority periods.',
    'If the coherence cost is largely partisan rather than universal, ''policy_coherence_dependent_citizens'' as a distinct victim class is weaker than authored and the extraction is better modeled as purely inter-coalition transfer rather than a genuine diffuse public cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(policy_coherence_victim_measurability, empirical, 'Whether the diffuse citizen victim class is a genuine universal cost or primarily a partisan framing of ordinary political competition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__cohabitation_equilibrium_reading, 1958, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fift_tr_t1958, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 1958, 0.2).
narrative_ontology:measurement(fift_tr_t1974, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 1974, 0.25).
narrative_ontology:measurement(fift_tr_t1986, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 1986, 0.42).
narrative_ontology:measurement(fift_tr_t1997, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 1997, 0.45).
narrative_ontology:measurement(fift_tr_t2002, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 2002, 0.3).
narrative_ontology:measurement(fift_tr_t2024, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(fift_be_t1958, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 1958, 0.28).
narrative_ontology:measurement(fift_be_t1974, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 1974, 0.32).
narrative_ontology:measurement(fift_be_t1986, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 1986, 0.51).
narrative_ontology:measurement(fift_be_t1997, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 1997, 0.55).
narrative_ontology:measurement(fift_be_t2002, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 2002, 0.4).
narrative_ontology:measurement(fift_be_t2024, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 2024, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(fift_su_t1958, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 1958, 0.25).
narrative_ontology:measurement(fift_su_t1974, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 1974, 0.28).
narrative_ontology:measurement(fift_su_t1986, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 1986, 0.48).
narrative_ontology:measurement(fift_su_t1997, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 1997, 0.5).
narrative_ontology:measurement(fift_su_t2002, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 2002, 0.35).
narrative_ontology:measurement(fift_su_t2024, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 2024, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_republic_constitution__cohabitation_equilibrium_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.12).
narrative_ontology:affects_constraint(fifth_republic_constitution__cohabitation_equilibrium_reading, hyper_presidential_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__cohabitation_equilibrium_reading, parliamentary_constraint_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings decomposing the colloquial 'Fifth Republic dual executive' concept, per the ε-invariance principle. Each reading has a distinct ε, distinct beneficiary/victim structure, and distinct operative historical grounding: cohabitation_equilibrium_reading (this story, ε=0.48, moderate/unstable, grounded in the three historical cohabitation episodes), hyper_presidential_reading (grounded in unified-majority De Gaulle-era practice, expected lower ε from the presidential seat and higher suppression of Assembly autonomy), and parliamentary_constraint_reading (grounded in a restrictive textualist account of Articles 20-21, expected higher ε from the presidential seat as the president is treated as requiring legislative authorization). All three are linked bidirectionally as members of the fifth_republic_constitution kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fifth_republic_constitution__cohabitation_equilibrium_reading, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
