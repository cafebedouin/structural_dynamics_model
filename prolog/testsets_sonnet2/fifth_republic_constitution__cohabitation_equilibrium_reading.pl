% ============================================================================
% CONSTRAINT STORY: fifth_republic_constitution__cohabitation_equilibrium_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   The Fifth Republic's constitution establishes a dual executive: a
 *   directly elected president with reserved authority over foreign policy
 *   and defense, and a prime minister who must command an Assembly majority
 *   to govern domestically. Under this cohabitation-equilibrium reading, the
 *   two seats are best understood as mutually constraining one another
 *   through negotiated, largely informal domain allocation rather than
 *   through either seat dominating the other. This story authors only that
 *   reading. The hyper-presidential reading (the president as near-sovereign,
 *   minimally checked) and the parliamentary-constraint reading (the
 *   president as coordinated executive requiring legislative authorization)
 *   are structurally distinct constraints with their own ε and stakeholder
 *   sets — they are not blended into this one. Extractiveness here is
 *   authored as moderate and unstable because the reading's own premise is
 *   that power shifts unpredictably between the two seats depending on
 *   electoral alignment, producing periods of relatively low extraction
 *   (unified government, clear accountability) and periods of higher
 *   extraction (cohabitation, diffused accountability, domain disputes).
 *
 * KEY AGENTS:
 *   - president: institutional/constrained — reserved domain authority, cannot be removed by ordinary vote, cannot dictate domestic policy during cohabitation
 *   - prime_minister: institutional/constrained — domestic and budgetary authority contingent on Assembly majority, shares executive stage with an unremovable president
 *   - national_assembly_majority: organized/constrained — installs and sustains the PM, gains leverage specifically during cohabitation
 *   - civil_service_and_diplomatic_corps: moderate/trapped — absorbs the operational cost of dual and sometimes contradictory direction
 *   - electorate: powerless/trapped — bears diffuse accountability-diffusion cost, cannot cleanly assign blame across two executives
 *   - constitutional_council: institutional/analytical — residual arbiter, rarely invoked because most boundary disputes are settled by informal negotiation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.52).
domain_priors:suppression_score(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.4).
domain_priors:theater_ratio(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 0.4).
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
narrative_ontology:cs_story_uid(fifth_republic_constitution__cohabitation_equilibrium_reading, '8f4040cc-4bae-4c4d-9209-09a77d7ae0f4').
narrative_ontology:cs_kernel_codification('8f4040cc-4bae-4c4d-9209-09a77d7ae0f4', formalized).
narrative_ontology:cs_authority_grounding('8f4040cc-4bae-4c4d-9209-09a77d7ae0f4', lineage).
narrative_ontology:cs_interpretation_layer_present('8f4040cc-4bae-4c4d-9209-09a77d7ae0f4').
narrative_ontology:cs_reading_relation('8f4040cc-4bae-4c4d-9209-09a77d7ae0f4', fifth_republic_constitution__hyper_presidential_reading, coexists_with).
narrative_ontology:cs_reading_relation('8f4040cc-4bae-4c4d-9209-09a77d7ae0f4', fifth_republic_constitution__parliamentary_constraint_reading, coexists_with).
narrative_ontology:cs_axiom('8f4040cc-4bae-4c4d-9209-09a77d7ae0f4', foundational, domain_partition_is_the_operative_constitutional_norm).
narrative_ontology:cs_axiom_status(domain_partition_is_the_operative_constitutional_norm, holdable).
narrative_ontology:cs_axiom_grounding('8f4040cc-4bae-4c4d-9209-09a77d7ae0f4', domain_partition_is_the_operative_constitutional_norm, conventional).
narrative_ontology:cs_axiom('8f4040cc-4bae-4c4d-9209-09a77d7ae0f4', foundational, neither_executive_mandate_is_categorically_subordinate).
narrative_ontology:cs_axiom_status(neither_executive_mandate_is_categorically_subordinate, holdable).
narrative_ontology:cs_axiom_grounding('8f4040cc-4bae-4c4d-9209-09a77d7ae0f4', neither_executive_mandate_is_categorically_subordinate, conventional).
narrative_ontology:cs_reference_frame('8f4040cc-4bae-4c4d-9209-09a77d7ae0f4', de_gaulle_founding_equilibrium).
narrative_ontology:cs_drift_state('8f4040cc-4bae-4c4d-9209-09a77d7ae0f4', post_2000_quinquennat_and_2022_fragmentation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8f4040cc-4bae-4c4d-9209-09a77d7ae0f4', '').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__cohabitation_equilibrium_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, domain_controlling_executive_actor).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, presidency_in_foreign_and_defense_domain).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, prime_minister_in_domestic_domain).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, policy_coherence).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, electorate_seeking_accountable_governance).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, administrative_apparatus).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, president).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, prime_minister).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, national_assembly_majority).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, civil_service_and_diplomatic_corps).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, electorate).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__cohabitation_equilibrium_reading, separation_of_domains_doctrine).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__cohabitation_equilibrium_reading, reserved_domain_convention).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds direct popular mandate and constitutionally reserved authority over foreign policy, defense, and treaty negotiation. During cohabitation, cannot dictate domestic legislative agenda but retains chairmanship of the Council of Ministers and command of the armed forces. Negotiates the boundary of the 'reserved domain' informally with the prime minister when they come from opposing parliamentary majorities; cannot dismiss a prime minister who commands an Assembly majority.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, president, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__cohabitation_equilibrium_reading, president, beneficiary).

% Derives authority from commanding an Assembly majority and directs day-to-day domestic policy, the budget, and the civil service. Must share the executive stage with a president who cannot be removed by ordinary legislative vote and who controls diplomatic representation abroad. Negotiates continuously over ambiguous jurisdiction (e.g., European Council attendance, military deployments with domestic budgetary implications).
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, prime_minister, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__cohabitation_equilibrium_reading, prime_minister, beneficiary).

% The parliamentary bloc that installs and sustains the prime minister. Gains real domestic policymaking leverage during cohabitation that it lacks under unified government, since the president cannot govern domestically without the Assembly's cooperation. Its exit option is limited to the next election cycle or a successful censure motion.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, national_assembly_majority, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__cohabitation_equilibrium_reading, national_assembly_majority, agenda_setter).

% Whichever party lost the legislative majority the president's party once held. Has formal voice in debate but no lever over the negotiated president/PM boundary; its policy preferences are structurally absent from the informal domain-allocation bargaining between the two executive seats.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, opposition_parliamentary_minority, excluded,
    moderate, biographical, constrained, national).

% Must implement policy while receiving conflicting or ambiguous direction from two executive principals who are actively contesting jurisdiction. Bears the operational cost of the ambiguity — duplicated approval chains, contradictory signals to foreign counterparts, delayed decisions pending informal negotiation between the Élysée and Matignon.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, civil_service_and_diplomatic_corps, payer,
    moderate, biographical, trapped, national).

% Votes separately for president and Assembly, sometimes producing cohabitation deliberately or by drift. Bears the diffuse cost of reduced policy coherence and accountability diffusion — when a domain fails, each executive can plausibly blame the other, and voters cannot cleanly assign responsibility at the next election.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, electorate, payer,
    powerless, generational, trapped, national).

% Adjudicates disputes over the boundary of executive authority when they are formally litigated, but most domain allocation during cohabitation is settled through informal political negotiation and convention rather than judicial ruling, leaving the Council a residual, rarely-invoked role.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, constitutional_council, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fifth_republic_constitution__cohabitation_equilibrium_reading, diffuse).
narrative_ontology:fixing_cost_class(fifth_republic_constitution__cohabitation_equilibrium_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for two executives with independent electoral mandates and no way to remove one another to continue governing the same state without a constitutional crisis, by informally partitioning policy domains (foreign/defense to the president, domestic/budget to the prime minister) rather than forcing either mandate to yield entirely.
% TRANSFER_FUNCTION: Moves effective policymaking authority back and forth between the presidency and the premiership depending on which one commands the Assembly majority at a given moment; in cohabitation periods it moves substantial domestic authority from the president to the prime minister while leaving diplomatic and defense authority with the president, at the cost of policy coherence and clear public accountability.
% ABSENT_VOICES: The opposition parliamentary minority and ordinary voters have no seat at the informal negotiation table where domain boundaries are actually drawn; the civil service, which absorbs the operational friction of dual and sometimes contradictory direction, is not a party to the bargain either.
% DISAPPEARANCE_RATIONALE: If the negotiated dual-executive equilibrium disappeared — for instance, if the constitution were amended to make the presidency purely ceremonial or purely dominant — the entire pattern of French governance would reorganize: the reserved-domain convention, cohabitation crisis politics, and the semi-presidential balance that shapes party strategy and coalition formation would all cease to structure political life as they currently do.
% FOUNDING_PROBLEM: The Fourth Republic collapsed under a purely parliamentary system seen as producing chronic cabinet instability and executive paralysis (Algeria crisis, revolving governments); the 1958 constitution was built to give the executive enough independent authority to govern decisively while still requiring legislative confidence, without recreating either Bonapartist autocracy or parliamentary chaos.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars and comparative political scientists outside government (e.g., analyses of the 1958 founding debates and subsequent cohabitation episodes of 1986, 1993, and 1997) attest the instability problem was real and the dual-mandate structure was a genuine response to it; they also document that the informal domain-partition convention emerged ad hoc during cohabitation rather than being specified in the original text, and that it now sometimes serves to shield both executives from accountability rather than to solve the original stability problem — a reading the executives themselves do not volunteer.
narrative_ontology:disappearance_verdict(fifth_republic_constitution__cohabitation_equilibrium_reading, world_rearranges).
narrative_ontology:founding_problem_status(fifth_republic_constitution__cohabitation_equilibrium_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__cohabitation_equilibrium_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fifth_republic_constitution__cohabitation_equilibrium_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.52, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored as moderate (0.52) and explicitly unstable over time — the measurement series shows it rising sharply during the three historical cohabitation periods (1986, 1993 not separately plotted but reflected in the 1986-1997 climb, 1997) and falling during unified-government periods (2008, post-2002 quinquennat alignment), then rising again toward 2024 as electoral fragmentation reintroduced coalition uncertainty. Suppression is moderate (0.4): the constraint does not suppress alternatives through coercion so much as through the structural unavailability of a clean mechanism to resolve jurisdictional disputes — the Constitutional Council is rarely invoked, so the informal bargain itself becomes the only available resolution channel, which is a soft form of alternative-foreclosure. Theater ratio rises during cohabitation (0.32-0.40) as both executives perform decisiveness in their reserved domains partly to compensate for the constraint's genuine instability in domains of ambiguous jurisdiction.
 *
 * PERSPECTIVAL GAP:
 *   From the president's seat, the arrangement looks like principled respect for the popular mandate in reserved domains — a genuine constitutional achievement. From the prime minister's seat during cohabitation, it looks like a hard-won expansion of domestic authority against a president who would otherwise dominate. From the civil service and electorate's seats, the same structure looks like an unresolved jurisdictional contest whose costs (delay, contradictory signals, diffused accountability) are borne by parties who did not negotiate the boundary and cannot appeal it except at the ballot box, years later, for a different office.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary status is domain-contingent by design: the president benefits (low d) in foreign/defense matters regardless of cohabitation status, and the prime minister benefits (low d) in domestic/budget matters when backed by an Assembly majority. Neither is a stable, unconditional beneficiary across all domains and all electoral configurations — this is the defining feature of the reading and is why it is authored as tangled_rope rather than a straightforward rope: there is a genuine coordination function (avoiding executive paralysis) bundled with asymmetric extraction (policy coherence and administrative clarity are sacrificed to sustain the negotiated boundary, and the electorate bears diffused accountability costs it did not choose). The civil service and electorate are victims not because either executive intends to extract from them, but because the negotiated-boundary structure itself, by design, produces ambiguity that these actors must absorb.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — Fourth Republic parliamentary paralysis — is genuinely dead in the sense that no cabinet has fallen at the pre-1958 rate; the dual-executive structure demonstrably solved that specific instability. But the informal domain-partition convention that emerged to manage cohabitation was never in the original 1958 text and has itself become a site of ongoing renegotiation rather than a settled solution — it is contested rather than fully live or fully dead. Classifying this as tangled_rope rather than mountain or pure rope prevents two mislabeling errors: treating the arrangement as inevitable constitutional bedrock (it is a negotiated, historically contingent equilibrium that shifted content substantially between 1958 and 1986), and treating it as pure extraction with no coordination function (it did solve a real governability crisis and continues to prevent executive-legislative deadlock from producing constitutional breakdown).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cohabitation_frequency_contingency,
    'Is the cohabitation-equilibrium pattern a stable structural feature of the Fifth Republic, or was it contingent on electoral-calendar misalignment that the 2000 quinquennat reform (synchronizing presidential and legislative terms) has substantially eliminated?',
    'Track cohabitation frequency and duration pre- and post-2000 reform; if no cohabitation occurs across several subsequent electoral cycles, the equilibrium reading describes a historical phase rather than an enduring structural feature.',
    'If cohabitation is now rare, this reading''s claim of ongoing mutual constraint weakens relative to the hyper_presidential_reading, which would better describe the post-2000 default state; if fragmentation (as in 2022, 2024) makes cohabitation-like coalition bargaining recurrent again, this reading''s claim strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cohabitation_frequency_contingency, empirical, 'Whether the cohabitation equilibrium is structural or a historically bounded electoral-calendar artifact.').

omega_variable(
    reading_boundary_location,
    'Where exactly does the disagreement between the three kernel readings live — is it about which domains are ''reserved,'' about whether the president can be meaningfully constrained at all absent cohabitation, or about whether legislative authorization is required as a matter of convention versus formal law?',
    'Compare the three readings'' treatment of a single hard case (e.g., presidential military deployment decisions without prior parliamentary vote) — each reading predicts a different verdict on whether this is within bounds.',
    'Clarifies whether the three readings are genuinely incompatible (forecloses) or represent different emphases within a compatible framework (coexists_with/influences) — currently authored as coexists_with because all three remain live in constitutional scholarship and political practice depending on electoral alignment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_location, conceptual, 'Locating the precise structural disagreement among the three fifth_republic_constitution kernel readings.').

omega_variable(
    beneficiary_or_victim_of_ambiguity,
    'Is the informal domain-partition convention itself a genuine coordination achievement that happens to impose incidental costs, or is the persistent ambiguity actively useful to both executives as a way to evade accountability by each blaming the other?',
    'Examine whether either executive has proposed formalizing the domain boundary (e.g., through constitutional amendment) when they held the political capital to do so; persistent non-formalization despite opportunity would support the accountability-evasion reading.',
    'If ambiguity is actively preserved for accountability-evasion purposes, the constraint shifts further toward snare-like dynamics for the electorate; if ambiguity is simply an unavoidable byproduct of two independent mandates, the tangled_rope classification with moderate extraction stands as authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_or_victim_of_ambiguity, conceptual, 'Whether jurisdictional ambiguity is incidental cost or preserved accountability shield.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__cohabitation_equilibrium_reading, 1958, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fift_tr_t1958, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 1958, 0.15).
narrative_ontology:measurement(fift_tr_t1970, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 1970, 0.18).
narrative_ontology:measurement(fift_tr_t1986, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 1986, 0.32).
narrative_ontology:measurement(fift_tr_t1997, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 1997, 0.4).
narrative_ontology:measurement(fift_tr_t2008, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 2008, 0.3).
narrative_ontology:measurement(fift_tr_t2024, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(fift_be_t1958, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 1958, 0.28).
narrative_ontology:measurement(fift_be_t1970, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 1970, 0.32).
narrative_ontology:measurement(fift_be_t1986, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 1986, 0.48).
narrative_ontology:measurement(fift_be_t1997, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 1997, 0.55).
narrative_ontology:measurement(fift_be_t2008, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 2008, 0.42).
narrative_ontology:measurement(fift_be_t2024, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 2024, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(fift_su_t1958, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 1958, 0.22).
narrative_ontology:measurement(fift_su_t1970, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 1970, 0.25).
narrative_ontology:measurement(fift_su_t1986, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 1986, 0.38).
narrative_ontology:measurement(fift_su_t1997, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 1997, 0.42).
narrative_ontology:measurement(fift_su_t2008, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 2008, 0.35).
narrative_ontology:measurement(fift_su_t2024, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_republic_constitution__cohabitation_equilibrium_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.12).
narrative_ontology:affects_constraint(fifth_republic_constitution__cohabitation_equilibrium_reading, hyper_presidential_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__cohabitation_equilibrium_reading, parliamentary_constraint_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the fifth_republic_constitution kernel. hyper_presidential_reading authors the president as a near-sovereign minimally checked by the legislature (lower authored suppression from the Assembly's perspective, higher concentrated extraction toward the president as sole beneficiary). parliamentary_constraint_reading authors the president as a coordinated executive requiring legislative authorization (extraction concentrated toward Assembly control, president as constrained implementer). This cohabitation_equilibrium_reading sits between them: both seats constrain each other, extraction is moderate and time-varying, and the victim is policy coherence rather than a single disempowered institutional actor. All three share the same constitutional text but are authored as separate constraints because their ε values, beneficiary sets, and victim sets differ structurally, not merely by observer standpoint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fifth_republic_constitution__cohabitation_equilibrium_reading, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
