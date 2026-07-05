% ============================================================================
% CONSTRAINT STORY: fifth_republic_constitution__hyper_presidential_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: fifth_republic_constitution__hyper_presidential_reading
 *   human_readable: Fifth Republic Constitution — Hyper-Presidential Reading
 *   domain: constitutional_law/political_systems/comparative_government
 *
 * SUMMARY:
 *   This story instantiates the hyper-presidential reading of the Fifth
 *   Republic constitution: the president as direct sovereign whose
 *   universal-suffrage election constitutes an unmediated national mandate,
 *   with Article 49.3 and Article 16 as the operative mechanisms by which
 *   legislative process is bypassed rather than merely expedited. Under this
 *   reading the National Assembly is not a co-equal coordinate branch but a
 *   body whose deliberative function is structurally subordinated whenever
 *   the executive elects to invoke these articles — which, over the measured
 *   interval, has become more frequent even absent the emergency conditions
 *   the mechanisms were designed for. This is a decomposition of the
 *   natural-language 'Fifth Republic constitution' label per the ε-invariance
 *   principle: the parliamentary_constraint_reading treats the same textual
 *   provisions as requiring genuine legislative authorization and would
 *   compute a low-extraction Rope or Scaffold; the
 *   cohabitation_equilibrium_reading treats the dual executive as a
 *   negotiated power-sharing arrangement activated whenever president and
 *   Assembly majority diverge, and would compute closer to a Tangled Rope
 *   with more balanced beneficiary/victim distribution. These are not the
 *   same constraint measured three ways — they are three constitutionally
 *   live readings with different victim sets, different ε, and different
 *   enforcement pictures, linked here only through the kernel and the network
 *   edges below.
 *
 * KEY AGENTS:
 *   - incumbent_president: Primary beneficiary and agenda-setter (institutional/arbitrage) — invokes 49.3/16, insulated from direct censure
 *   - presidency_as_institution: Structural beneficiary across incumbents (institutional/arbitrage) — accretes precedent
 *   - national_assembly: Primary victim (organized/constrained) — bypassed via 49.3, censure threshold near-unreachable
 *   - opposition_parties: Secondary victim (organized/constrained) — structurally unable to compel majority censure
 *   - electorate_seeking_deliberation: Diffuse victim (powerless/trapped) — loses deliberative voice, retains only deferred electoral recourse
 *   - prime_minister: Instrument and cost-absorber (powerful/constrained) — formal user of 49.3, bears political cost
 *   - constitutional_council: Analytical observer (institutional/analytical) — declines to check political legitimacy of frequency
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__hyper_presidential_reading, 0.71).
domain_priors:suppression_score(fifth_republic_constitution__hyper_presidential_reading, 0.68).
domain_priors:theater_ratio(fifth_republic_constitution__hyper_presidential_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__hyper_presidential_reading, tangled_rope).
narrative_ontology:human_readable(fifth_republic_constitution__hyper_presidential_reading, "Fifth Republic Constitution — Hyper-Presidential Reading").
narrative_ontology:topic_domain(fifth_republic_constitution__hyper_presidential_reading, "constitutional_law/political_systems/comparative_government").

domain_priors:requires_active_enforcement(fifth_republic_constitution__hyper_presidential_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__hyper_presidential_reading, '23e61a97-db6f-4109-85ec-ebde64212d22').
narrative_ontology:cs_kernel_codification('23e61a97-db6f-4109-85ec-ebde64212d22', formalized).
narrative_ontology:cs_authority_grounding('23e61a97-db6f-4109-85ec-ebde64212d22', lineage).
narrative_ontology:cs_interpretation_layer_present('23e61a97-db6f-4109-85ec-ebde64212d22').
narrative_ontology:cs_reading_relation('23e61a97-db6f-4109-85ec-ebde64212d22', fifth_republic_constitution__parliamentary_constraint_reading, coexists_with).
narrative_ontology:cs_reading_relation('23e61a97-db6f-4109-85ec-ebde64212d22', fifth_republic_constitution__cohabitation_equilibrium_reading, influences).
narrative_ontology:cs_axiom('23e61a97-db6f-4109-85ec-ebde64212d22', foundational, direct_election_constitutes_unmediated_mandate).
narrative_ontology:cs_axiom_status(direct_election_constitutes_unmediated_mandate, holdable).
narrative_ontology:cs_axiom_grounding('23e61a97-db6f-4109-85ec-ebde64212d22', direct_election_constitutes_unmediated_mandate, conventional).
narrative_ontology:cs_axiom('23e61a97-db6f-4109-85ec-ebde64212d22', foundational, legislative_negotiation_is_dispensable_friction_not_constitutional_requirement).
narrative_ontology:cs_axiom_status(legislative_negotiation_is_dispensable_friction_not_constitutional_requirement, holdable).
narrative_ontology:cs_axiom_grounding('23e61a97-db6f-4109-85ec-ebde64212d22', legislative_negotiation_is_dispensable_friction_not_constitutional_requirement, instrumental).
narrative_ontology:cs_reference_frame('23e61a97-db6f-4109-85ec-ebde64212d22', gaullist_direct_mandate_founding).
narrative_ontology:cs_drift_state('23e61a97-db6f-4109-85ec-ebde64212d22', contemporary_minority_government_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('23e61a97-db6f-4109-85ec-ebde64212d22', '').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__hyper_presidential_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__hyper_presidential_reading, incumbent_president).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__hyper_presidential_reading, presidency_as_institution).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, national_assembly).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, opposition_parties).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, electorate_seeking_deliberation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, prime_minister).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__hyper_presidential_reading, direct_national_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__hyper_presidential_reading, presidential_election_as_plebiscitary_mandate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Elected by direct universal suffrage since 1962, the president invokes Article 49.3 to force legislation through without a vote and Article 16 to assume emergency powers, treating the popular mandate as direct authorization that bypasses ordinary legislative negotiation. Sets the government's program, appoints the prime minister, and can dissolve the Assembly, but is never dissolved in return. Frames every use of these powers as executing the will of the people expressed at the ballot box.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, incumbent_president, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__hyper_presidential_reading, incumbent_president, beneficiary).

% The office itself accumulates precedent each time 49.3 or Article 16 is invoked without successful censure or reversal; each unchallenged use widens the interpretive latitude available to the next occupant regardless of who holds it. Institutional memory and constitutional custom entrench the reading independent of any single incumbent.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, presidency_as_institution, beneficiary,
    institutional, civilizational, arbitrage, national).

% Can be bypassed entirely on a bill via Article 49.3 unless it can muster an absolute majority for a motion of censure, a threshold deliberately hard to reach because censuring collapses the government and often threatens deputies' own seats via dissolution. Debates and amendments become theater when the executive signals it will use 49.3 regardless of the vote count. Its only real lever — censure — is a blunt, high-stakes instrument rarely usable in practice.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, national_assembly, payer,
    organized, immediate, constrained, national).

% Mobilize public opposition and file censure motions but structurally cannot compel policy change without an absolute majority coalition that fractured multi-party assemblies rarely produce. Their exit is electoral (waiting for the next presidential term) rather than institutional (blocking this term's exercise of power).
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, opposition_parties, payer,
    organized, biographical, constrained, national).

% Citizens who expect legislative deliberation and amendment on major bills — pension reform, budget laws — instead see them enacted by presidential decree-equivalent mechanism after minimal or no floor debate. They retain only the vote at the next election as recourse, a five-year-delayed and diffuse instrument against a specific policy grievance.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, electorate_seeking_deliberation, payer,
    powerless, generational, trapped, national).

% Nominally head of government and the formal user of Article 49.3, but selected by and serving at the president's pleasure in this reading; functions as the instrument through which presidential will is executed and absorbs political cost (resignation, unpopularity) that the president is insulated from by fixed-term direct election.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, prime_minister, agenda_setter,
    powerful, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__hyper_presidential_reading, prime_minister, payer).

% Reviews the constitutionality of laws and procedures after the fact but has historically declined to check the political legitimacy of 49.3 usage itself, treating it as a valid procedural mechanism rather than adjudicating whether its frequency undermines legislative function.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, constitutional_council, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fifth_republic_constitution__hyper_presidential_reading, incumbent_president).
narrative_ontology:fixing_cost_class(fifth_republic_constitution__hyper_presidential_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism to pass a government program even when no stable legislative majority exists, avoiding the paralysis that destabilized the Fourth Republic's assembly-dominated system.
% TRANSFER_FUNCTION: Moves effective lawmaking authority from the elected Assembly to the president and the prime minister acting as presidential instrument, and moves political accountability for unpopular measures away from the president (who cannot be censured directly) onto the prime minister and the Assembly's censure mechanism.
% ABSENT_VOICES: Backbench deputies and minority coalition partners who would negotiate amendments in ordinary process are foreclosed once 49.3 is announced; citizens affected by bypassed bills (pension changes, budget allocations) have no procedural voice in this reading, only the deferred electoral one.
% DISAPPEARANCE_RATIONALE: If the hyper-presidential reading were repudiated — if courts or amendment stripped 49.3/16 of this scope — governments without absolute majorities would need genuine coalition-building on every bill, legislative negotiation would resume as the actual site of policy formation, and the presidency would lose its capacity to govern unilaterally through minority governments.
% FOUNDING_PROBLEM: The Fourth Republic collapsed under chronic cabinet instability and an assembly that could topple governments without producing durable majorities; de Gaulle's 1958 constitution was built to give the executive enough independent authority to govern despite legislative fragmentation.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars and comparative-government analysts outside the presidency attest that legislative fragmentation persists in some periods (supporting a live founding problem) but that 49.3/Article 16 usage has expanded well past crisis governance into routine budget and reform passage even under presidents with working majorities — a pattern documented by parliamentary procedure researchers and by the Assembly's own censure-motion records, neither of which are beneficiaries of the practice.
narrative_ontology:disappearance_verdict(fifth_republic_constitution__hyper_presidential_reading, world_rearranges).
narrative_ontology:founding_problem_status(fifth_republic_constitution__hyper_presidential_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__hyper_presidential_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fifth_republic_constitution__hyper_presidential_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fifth_republic_constitution__hyper_presidential_reading, 0.71, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored high (0.71 at interval end) and rising because the delta being modeled is real: 49.3 usage has trended from an emergency-adjacent tool toward routine passage of contested budget and pension legislation, transferring effective lawmaking authority from the elected chamber to the executive on a widening set of ordinary (non-crisis) matters. Suppression (0.68) reflects the structural difficulty of the absolute-majority censure threshold — not merely political reluctance but an engineered asymmetry where deputies risk their own seats (via dissolution) to check the executive, and where fractured multi-party assemblies structurally cannot assemble the required coalition. Theater ratio (0.42) is moderate: parliamentary debate on bills still occurs and is not wholly performative, but its capacity to alter outcomes collapses once 49.3 is invoked, so a rising share of debate time is expressively rather than functionally consequential. Accessibility collapse (0.58) and resistance (0.55) are mid-range rather than mountain-grade: this is a contested constitutional practice, not settled natural law — opposition parties, scholars, and periodic mass mobilization (e.g. pension-reform protests) constitute real, organized resistance, and alternative readings of the same text remain constitutionally live, which is precisely why this is authored as a reading rather than a mountain.
 *
 * PERSPECTIVAL GAP:
 *   From the presidency's seat, 49.3 and Article 16 are legitimate expressions of the direct mandate voters conferred, and the Assembly's inability to censure is evidence of that mandate's durability, not of a broken check. From the Assembly's seat, the same mechanism is a structural override of the deliberative function the constitution nominally assigns it. The engine should compute these as genuinely different per-seat classifications from the same structural facts — that divergence is the reading's central claim, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   The incumbent president and the presidency as an institution sit at the beneficiary end: the president collects policy outcomes without bearing the accountability cost of a parliamentary vote, and the institution accretes durable interpretive latitude with each unchallenged invocation. The National Assembly, opposition parties, and the electorate seeking deliberation sit toward the target end: their nominal legislative authority is structurally bypassed by a mechanism they cannot reliably counter. The prime minister is a genuinely mixed seat — nominal co-agenda-setter (the constitutional text names the PM, not the president, as the user of 49.3) but functionally an instrument absorbing cost on the president's behalf in this reading; this is exactly the seat divergence the engine should register differently from the Assembly's.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — Fourth Republic legislative paralysis — was real in 1958 and remains contestable as 'live' in periods of genuine fragmentation. But the founding_problem_status is authored as contested rather than dead precisely because this reading's own defenders can point to ongoing coalition fragility as justification, while critics point to routine use even under presidents with working majorities as evidence the mechanism has drifted from crisis tool to default governance tool. Classifying this as tangled_rope rather than snare preserves the genuine coordination function (avoiding governmental collapse) that a pure-extraction reading would erase, while the enforcement requirement and named victim set (Assembly, opposition, electorate) prevent the coordination story from laundering what is, on this reading's own metrics, a substantial and growing transfer of lawmaking authority away from the elected chamber.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_fifth_republic,
    'Is the hyper-presidential reading the structurally correct account of the Fifth Republic constitution, or is it one politically motivated reading among three (alongside parliamentary_constraint_reading and cohabitation_equilibrium_reading) that the constitutional text itself underdetermines?',
    'No single resolution exists — the French constitutional text has sustained all three readings across different presidencies and cohabitation periods since 1958; frequency and context of Article 49.3/16 invocation across administrations is the closest empirical proxy, but the underlying interpretive question is not resolvable by data alone.',
    'If the parliamentary_constraint_reading is treated as authoritative, the same textual provisions would compute as Rope or Scaffold with the Assembly as co-equal beneficiary rather than victim. If the cohabitation_equilibrium_reading is authoritative, the beneficiary/victim structure shifts to a negotiated dual-executive with more balanced extraction. This story deliberately instantiates only the hyper-presidential reading per the ε-invariance principle; the sibling readings are separate constraint files linked via network.affects_constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_fifth_republic, conceptual, 'Which of three live constitutional readings governs the Fifth Republic''s actual operation is itself contested and not resolvable within a single reading''s structural data.').

omega_variable(
    fsm_direct_sovereignty_doctrine,
    'Is the president''s claim to embody ''direct national sovereignty'' via universal suffrage a genuine constitutional-democratic principle (a natural extension of popular sovereignty) or a constructed doctrine that benefits the presidency by foreclosing legislative negotiation as illegitimate friction?',
    'Comparative analysis against other directly-elected presidencies with stronger legislative checks (e.g., the US presidency''s veto-override structure) would show whether direct election alone entails minimal legislative constraint, or whether the Fifth Republic''s specific configuration (49.3, Article 16, no direct presidential censure) is a separable design choice riding on the sovereignty rhetoric.',
    'If direct sovereignty doctrine is genuinely entailed by universal suffrage, the beneficiary structure is less an extraction and more a feature of the democratic design itself. If it is a constructed cover story, the beneficiary declaration on presidency_as_institution is evidence of extraction dressed as popular will.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fsm_direct_sovereignty_doctrine, conceptual, 'Whether the plebiscitary-mandate doctrine underlying the hyper-presidential reading is natural to direct election or a constructed extraction-justifying frame.').

omega_variable(
    censure_threshold_design_intent,
    'Was the absolute-majority censure threshold designed as a genuine stability safeguard against Fourth-Republic-style paralysis, or was it calibrated (deliberately or through drift) to be functionally unreachable in a fragmented multi-party system, converting a nominal check into a dead letter?',
    'Historical analysis of 1958 constitutional drafting debates and comparison of actual censure motion success rates across the Fifth Republic''s history against the fragmentation levels of each Assembly.',
    'If the threshold was calibrated to be nearly unreachable by design, suppression is better characterized as intentional rather than incidental, strengthening the snare-adjacent reading. If it reflects a genuine, once-workable stability mechanism that became harder to clear only as party systems fragmented later, the suppression is more incidental drift than designed extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(censure_threshold_design_intent, empirical, 'Whether the near-unreachable censure threshold was designed extraction or incidental drift from party-system fragmentation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__hyper_presidential_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fift_tr_t0, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(fift_tr_t10, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement(fift_tr_t20, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(fift_tr_t30, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 30, 0.32).
narrative_ontology:measurement(fift_tr_t40, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 40, 0.36).
narrative_ontology:measurement(fift_tr_t50, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 50, 0.39).
narrative_ontology:measurement(fift_tr_t60, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 60, 0.42).

% Extraction over time
narrative_ontology:measurement(fift_be_t0, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(fift_be_t10, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(fift_be_t20, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 20, 0.53).
narrative_ontology:measurement(fift_be_t30, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(fift_be_t40, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 40, 0.63).
narrative_ontology:measurement(fift_be_t50, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement(fift_be_t60, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 60, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(fift_su_t0, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(fift_su_t10, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 10, 0.46).
narrative_ontology:measurement(fift_su_t20, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(fift_su_t30, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 30, 0.57).
narrative_ontology:measurement(fift_su_t40, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 40, 0.61).
narrative_ontology:measurement(fift_su_t50, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 50, 0.65).
narrative_ontology:measurement(fift_su_t60, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 60, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_republic_constitution__hyper_presidential_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fifth_republic_constitution__hyper_presidential_reading, fifth_republic_constitution__parliamentary_constraint_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__hyper_presidential_reading, fifth_republic_constitution__cohabitation_equilibrium_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the kernel fifth_republic_constitution. hyper_presidential_reading (this file) treats presidential authority as minimally constrained direct sovereignty and computes high extractiveness with the Assembly as primary victim. parliamentary_constraint_reading treats the same textual provisions as requiring genuine legislative authorization for implementation and would compute substantially lower extraction with the Assembly as co-beneficiary of the coordination function. cohabitation_equilibrium_reading treats the dual executive as a negotiated allocation activated during periods of presidential/parliamentary majority divergence, and computes a more balanced tangled-rope structure. All three share the same constitutional text (1958 Constitution, as amended, particularly Articles 20, 21, 49.3, and 16) but diverge in which structural facts (frequency of unilateral invocation vs. frequency of negotiated cohabitation vs. baseline legislative authorization requirement) they treat as dispositive. Per the ε-invariance principle, these are three separate constraints, not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
