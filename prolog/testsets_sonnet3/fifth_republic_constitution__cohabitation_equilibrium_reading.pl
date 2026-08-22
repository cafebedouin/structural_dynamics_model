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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   This story instantiates the cohabitation-equilibrium reading of the Fifth
 *   Republic's dual executive: a structural claim that the constitution's
 *   ambiguous allocation of authority between president and prime minister is
 *   not resolved in favor of either pole, but produces a genuinely
 *   negotiated, unstable equilibrium whenever the two elected mandates
 *   diverge (cohabitation). Under this reading, extraction is moderate and
 *   unstable rather than concentrated — whichever pole controls the contested
 *   domain in a given period 'wins' that slice of authority, at the cost of a
 *   coherent, accountable national policy. The rising
 *   extractiveness/theater/suppression values around t=16 in the measurement
 *   series represent a cohabitation episode (analogous to 1986-88, 1993-95,
 *   or 1997-2002): friction, competing claims to legitimacy, and public
 *   disputes over who speaks for France abroad or sets the domestic agenda
 *   all spike, then partially settle as convention re-stabilizes divided
 *   authority until the next election realigns or re-splits the mandates.
 *
 * KEY AGENTS:
 *   - president_of_the_republic: institutional/constrained — retains foreign/defense domain, loses domestic primacy during cohabitation
 *   - prime_minister_and_cabinet: institutional/constrained — controls domestic policy when Assembly majority opposes president
 *   - national_assembly_majority: organized/mobile — the electoral mechanism that can trigger cohabitation
 *   - electorate_seeking_accountable_governance: powerless/trapped — bears the accountability confusion cohabitation produces
 *   - policy_coherence: non-agent abstract good, degraded by the split-authority structure
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
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__cohabitation_equilibrium_reading, tangled_rope).
narrative_ontology:human_readable(fifth_republic_constitution__cohabitation_equilibrium_reading, "Fifth Republic Dual Executive — Cohabitation Equilibrium Reading").
narrative_ontology:topic_domain(fifth_republic_constitution__cohabitation_equilibrium_reading, "constitutional_law/political_systems/comparative_government").

domain_priors:requires_active_enforcement(fifth_republic_constitution__cohabitation_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__cohabitation_equilibrium_reading, 'cc66c8df-0308-4033-ab49-b668a2bd76f9').
narrative_ontology:cs_kernel_codification('cc66c8df-0308-4033-ab49-b668a2bd76f9', formalized).
narrative_ontology:cs_authority_grounding('cc66c8df-0308-4033-ab49-b668a2bd76f9', distributed).
narrative_ontology:cs_reading_relation('cc66c8df-0308-4033-ab49-b668a2bd76f9', fifth_republic_constitution__hyper_presidential_reading, coexists_with).
narrative_ontology:cs_reading_relation('cc66c8df-0308-4033-ab49-b668a2bd76f9', fifth_republic_constitution__parliamentary_constraint_reading, coexists_with).
narrative_ontology:cs_axiom('cc66c8df-0308-4033-ab49-b668a2bd76f9', foundational, dual_mandate_requires_negotiated_domain_allocation).
narrative_ontology:cs_axiom_status(dual_mandate_requires_negotiated_domain_allocation, holdable).
narrative_ontology:cs_axiom_grounding('cc66c8df-0308-4033-ab49-b668a2bd76f9', dual_mandate_requires_negotiated_domain_allocation, conventional).
narrative_ontology:cs_axiom('cc66c8df-0308-4033-ab49-b668a2bd76f9', secondary, policy_coherence_is_legitimately_sacrificeable_to_dual_legitimacy).
narrative_ontology:cs_axiom_status(policy_coherence_is_legitimately_sacrificeable_to_dual_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('cc66c8df-0308-4033-ab49-b668a2bd76f9', policy_coherence_is_legitimately_sacrificeable_to_dual_legitimacy, instrumental).
narrative_ontology:cs_reference_frame('cc66c8df-0308-4033-ab49-b668a2bd76f9', de_gaulle_1958_rationalized_parliamentarism).
narrative_ontology:cs_drift_state('cc66c8df-0308-4033-ab49-b668a2bd76f9', post_1986_first_cohabitation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('cc66c8df-0308-4033-ab49-b668a2bd76f9', '').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__cohabitation_equilibrium_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, domain_controlling_executive_actor).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, foreign_policy_presidential_apparatus).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, policy_coherence).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, electorate_seeking_accountable_governance).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, president_of_the_republic).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, prime_minister_and_cabinet).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, national_assembly_majority).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, civil_service_and_diplomatic_corps).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds constitutionally reserved authority over foreign affairs, defense, and the 'domaine reservé' regardless of parliamentary majority. During cohabitation (when the Assembly majority opposes the president's party), the president must negotiate domestic authority with a prime minister drawn from the opposing majority; retains only the domains the constitution's ambiguous text and convention have carved out. Cannot dissolve this arrangement at will once cohabitation exists — must wait out the term or force elections at high political cost.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, president_of_the_republic, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__cohabitation_equilibrium_reading, president_of_the_republic, beneficiary).

% Derives authority from Assembly majority and constitutionally 'determines and conducts the policy of the nation' domestically. During cohabitation, controls domestic legislation, budget, and administration but must coexist with a president who retains ceremonial and substantive foreign-policy prerogatives, complicating unified national positions. Exit from the arrangement means losing office at the next election or through a no-confidence vote the PM's own majority would have to trigger.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, prime_minister_and_cabinet, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__cohabitation_equilibrium_reading, prime_minister_and_cabinet, beneficiary).

% Elects and sustains the prime minister; can force cohabitation by electing a majority opposed to the sitting president. Its members benefit from domestic policy control when their majority forms the government, but the arrangement subjects them to negotiated compromises whenever their government must present a unified international face alongside a rival president.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, national_assembly_majority, agenda_setter,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__cohabitation_equilibrium_reading, national_assembly_majority, beneficiary).

% Votes separately for president and Assembly, occasionally producing opposed majorities. Bears the cost of unclear accountability during cohabitation: when foreign policy stumbles or domestic reform stalls, voters cannot cleanly identify which executive actor to hold responsible, since authority is split and contested rather than unified.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, electorate_seeking_accountable_governance, payer,
    powerless, biographical, trapped, national).

% Must implement policy while receiving potentially conflicting signals from the presidential and prime ministerial offices during cohabitation, particularly at the seams (European affairs, defense procurement, trade) where domain boundaries are genuinely contested rather than settled.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, civil_service_and_diplomatic_corps, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__cohabitation_equilibrium_reading, civil_service_and_diplomatic_corps, observer).

% Occasionally arbitrates disputes over which executive actor holds authority in a contested domain, but has historically been reluctant to draw bright lines, leaving much of the allocation to political negotiation and precedent rather than adjudicated rule.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, constitutional_council, observer,
    institutional, generational, analytical, national).

% Not an actor but the abstract good that is degraded whenever the two executive poles pursue incompatible strategies (e.g., presidential foreign policy initiatives undercut by a domestically-focused PM's budget priorities, or vice versa) — listed for completeness as the diffuse cost-bearer of the negotiated-allocation structure.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, policy_coherence, payer,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_non_agent(fifth_republic_constitution__cohabitation_equilibrium_reading, policy_coherence).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fifth_republic_constitution__cohabitation_equilibrium_reading, diffuse).
narrative_ontology:fixing_cost_class(fifth_republic_constitution__cohabitation_equilibrium_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Divides executive authority between a directly-elected president and an Assembly-accountable prime minister so that both the plebiscitary and parliamentary sources of legitimacy have a genuine executive foothold, preventing either from being wholly eclipsed by the other when they diverge politically.
% TRANSFER_FUNCTION: Moves effective policy control between the presidential and prime ministerial poles depending on which pole's party controls the Assembly majority at a given moment; during cohabitation, moves domestic authority toward the PM and preserves foreign/defense authority with the president, with contested territory at the seams.
% ABSENT_VOICES: The electorate that split its ticket (voting for one party's president and another's Assembly majority) rarely intends the resulting governance friction as an outcome; their preference for divided government as a check is real but they have no seat in the negotiated allocation of authority that follows — that negotiation happens entirely between the two institutional poles and is not something voters ratify directly.
% DISAPPEARANCE_RATIONALE: If the dual executive were replaced by a unified executive (pure presidential or pure parliamentary system), the president's camp would say governance clarity improves; the Assembly's camp would say a check on plebiscitary overreach disappears. Both institutional poles and constitutional scholars dispute which arrangement the world would 'rearrange' toward, which is why this reading treats the cohabitation equilibrium itself as a distinct, contested constitutional fact rather than settled convention.
% FOUNDING_PROBLEM: The 1958 constitution was built to end the perceived instability of the Fourth Republic's pure parliamentarism (frequent government collapse, weak executive authority) by installing a strong president, while retaining a prime minister and Assembly to preserve parliamentary legitimacy and avoid pure presidential autocracy — cohabitation was not designed for but emerged as the equilibrium mechanism when the two elected mandates diverge.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars outside both the presidential and prime ministerial camps (e.g., academic commentary on the 1958 constitutional debates and subsequent cohabitation episodes of 1986, 1993, 1997) attest that the framers anticipated strong presidential leadership as the normal case and that cohabitation was a later-discovered equilibrium, not an intended design feature — this is corroborated independently of either executive pole's self-interested account of how authority 'should' be allocated.
narrative_ontology:disappearance_verdict(fifth_republic_constitution__cohabitation_equilibrium_reading, contested).
narrative_ontology:founding_problem_status(fifth_republic_constitution__cohabitation_equilibrium_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__cohabitation_equilibrium_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.48) is authored as moderate because under this reading neither pole permanently captures the other's authority — the coordination function (accommodating both plebiscitary and parliamentary legitimacy sources) is genuine, but so is the extraction: each pole, when it holds the advantaged domain, extracts governing primacy from the other and from policy coherence generally. Suppression (0.42) reflects that neither actor can simply exit the arrangement — the president cannot dissolve cohabitation without new elections, the PM cannot act outside domestic domains without presidential friction — but this is lower than a pure snare because both retain meaningful counter-moves (dissolution power, no-confidence votes, public appeals). Theater ratio (0.38) captures the substantial performative element: joint appearances, competing claims to represent 'France' internationally, and ceremonial coordination that masks the underlying contest for domain control. Accessibility collapse is low-moderate (0.35) because constitutional alternatives (clarifying amendments, term synchronization via the 2000 quinquennat reform) remain visible and have partially been pursued. Resistance (0.58) is comparatively high because both institutional poles actively contest domain boundaries rather than passively accepting either's claim.
 *
 * PERSPECTIVAL GAP:
 *   From the presidential seat, the arrangement looks like a temporary, regrettable constraint on otherwise-plenary authority — cohabitation is an aberration to be endured until alignment returns. From the prime ministerial/Assembly seat, the same structure looks like the constitution finally operating as parliamentary check on presidential overreach — a feature, not a bug. The engine should compute different per-seat types from the same structural data precisely because this reading holds that BOTH readings of the moment are locally true from their respective institutional vantage points; that is the substance of calling this the 'equilibrium' reading rather than either the hyper-presidential or parliamentary-constraint reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Under this reading there is no single fixed beneficiary — directionality is domain-contingent and time-varying, which is itself the reading's defining structural claim. Whichever pole controls the contested domain in a given cohabitation period sits nearer the beneficiary end (d low) for that domain; the other sits nearer the target end (d high) for the same domain. Policy coherence and the electorate are consistent net payers across all configurations because the instability itself, not any single actor's capture, is what degrades their position. This is why 'policy_coherence' and 'electorate_seeking_accountable_governance' are declared as victims/payers regardless of which human institutional actor currently holds advantage.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Fourth Republic instability) is genuinely dead in the narrow sense — no one seriously proposes returning to pure multi-party parliamentarism — but the cohabitation equilibrium itself was never the designed solution; it is an emergent property of split-ticket outcomes under the amended electoral calendar. This reading resists mandatrophy mislabeling in both directions: it does not call the entire dual-executive structure obsolete extraction (it still performs real coordination between two legitimate mandates) nor does it certify it as costless natural law (the instability and coherence costs are real and recurring, not merely rhetorical).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cohabitation_as_design_or_emergent_pathology,
    'Is the cohabitation equilibrium a legitimate, intended feature of the 1958 constitutional design, or an emergent pathology the framers did not anticipate and that later conventions merely papered over?',
    'Close textual and historical analysis of the 1958 constitutional debates (de Gaulle, Debré) compared against actual practice across the three historical cohabitation episodes (1986-88, 1993-95, 1997-2002) and the post-2000 quinquennat reform''s effect on cohabitation frequency.',
    'If cohabitation is genuinely designed-for, this reading''s coordination claim strengthens and the constraint reads closer to a tangled_rope with real, durable coordination function. If it is an unanticipated emergent pathology merely tolerated, the reading shifts closer to a scaffold that has outlived any sunset (the quinquennat reform being an attempted, incomplete sunset) or toward a piton if the friction it produces is mostly performative rather than substantively contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cohabitation_as_design_or_emergent_pathology, conceptual, 'Whether the equilibrium is designed coordination or emergent, tolerated pathology.').

omega_variable(
    domain_boundary_stability,
    'How stable and predictable is the actual boundary between presidential (domaine reservé) and prime ministerial domestic authority across different cohabitation episodes — is it a settled convention or renegotiated each time?',
    'Comparative case analysis of domain disputes across the three historical cohabitation periods (defense procurement decisions, EU summit representation, domestic reform initiatives) to assess whether precedent accumulated into stable convention or each episode re-litigated the boundary from scratch.',
    'A stable, precedent-governed boundary would lower the effective extractiveness and suppression measures (the arrangement functions more like settled coordination); a boundary re-litigated each time would support higher instability-driven extraction and validate the ''unstable equilibrium'' characterization central to this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_boundary_stability, empirical, 'Whether domain boundaries between the two executive poles are convention-stabilized or contest-renegotiated.').

omega_variable(
    sibling_reading_selection_ambiguity,
    'Given that the same constitutional text supports all three readings (hyper-presidential, parliamentary-constraint, and this cohabitation-equilibrium reading), what determines which reading is operative at a given moment — is it purely a function of whether the president''s party controls the Assembly, or does one reading have interpretive priority as the ''default'' state?',
    'Track which reading dominant constitutional commentary and institutional self-description invoke during unified-government periods versus cohabitation periods; assess whether either non-cohabitation reading (hyper-presidential) is treated as the ''true'' baseline with cohabitation as deviation, or whether all three are treated as equally live depending on electoral configuration.',
    'If the hyper-presidential reading is treated as baseline/default with this reading as an occasional deviation, this reading''s claimed_type and extractiveness should be understood as episodic rather than as an equally-weighted permanent feature of the constitutional order — affecting how much weight this constraint''s classification should carry relative to its siblings in any composite assessment of ''the Fifth Republic constitution.''',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_selection_ambiguity, conceptual, 'Which reading holds interpretive priority as the constitution''s ''default'' self-understanding, and how that bears on this reading''s relative weight.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__cohabitation_equilibrium_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fift_tr_t0, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(fift_tr_t8, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(fift_tr_t16, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 16, 0.45).
narrative_ontology:measurement(fift_tr_t24, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 24, 0.36).
narrative_ontology:measurement(fift_tr_t32, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 32, 0.3).
narrative_ontology:measurement(fift_tr_t40, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 40, 0.38).

% Extraction over time
narrative_ontology:measurement(fift_be_t0, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(fift_be_t8, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 8, 0.35).
narrative_ontology:measurement(fift_be_t16, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 16, 0.52).
narrative_ontology:measurement(fift_be_t24, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 24, 0.44).
narrative_ontology:measurement(fift_be_t32, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 32, 0.4).
narrative_ontology:measurement(fift_be_t40, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 40, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(fift_su_t0, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(fift_su_t8, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 8, 0.28).
narrative_ontology:measurement(fift_su_t16, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 16, 0.5).
narrative_ontology:measurement(fift_su_t24, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 24, 0.42).
narrative_ontology:measurement(fift_su_t32, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 32, 0.35).
narrative_ontology:measurement(fift_su_t40, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 40, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_republic_constitution__cohabitation_equilibrium_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fifth_republic_constitution__cohabitation_equilibrium_reading, hyper_presidential_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__cohabitation_equilibrium_reading, parliamentary_constraint_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the fifth_republic_constitution kernel, decomposed per the ε-invariance principle: the same constitutional text produces structurally distinct claims about where executive authority actually sits, with different beneficiary/victim structures and different ε values depending on which reading is instantiated. hyper_presidential_reading claims near-total presidential authority with minimal Assembly constraint (lower contested extraction, single clear beneficiary — the presidency); parliamentary_constraint_reading claims the president is fundamentally coordinated and requires legislative authorization (extraction runs toward the Assembly/PM pole); this cohabitation_equilibrium_reading claims neither pole permanently prevails and extraction is moderate, unstable, and domain-contingent, with policy coherence itself as the consistent victim. All three should be read together as a constraint family, not reconciled into one averaged ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
