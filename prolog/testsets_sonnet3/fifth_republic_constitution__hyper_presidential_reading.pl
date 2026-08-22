% ============================================================================
% CONSTRAINT STORY: fifth_republic_constitution__hyper_presidential_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: Fifth Republic Constitution — Hyper-Presidential Reading (President as Direct Sovereign)
 *   domain: constitutional_law/political_systems
 *
 * SUMMARY:
 *   This story instantiates the hyper-presidential reading of the Fifth
 *   Republic constitutional kernel: the president as direct sovereign
 *   embodying the national will, constrained only minimally by the
 *   legislature. On this reading, Article 49.3 (forcing bills through absent
 *   a successful censure motion) and Article 16 (emergency powers) are the
 *   constitution's true center of gravity — tools that let a directly-elected
 *   president govern despite, or through, a fragmented or uncooperative
 *   Assembly. The Assembly's ordinary lawmaking role is read as structurally
 *   subordinate. This is a distinct constraint from the
 *   parliamentary_constraint_reading (which reads the president as a
 *   coordinated executive requiring legislative authorization) and the
 *   cohabitation_equilibrium_reading (which reads the system as a negotiated
 *   dual executive). Each reading has its own epsilon, its own
 *   beneficiary/victim structure, and its own type — they are linked, not
 *   merged.
 *
 * KEY AGENTS:
 *   - incumbent_president: primary agenda-setter and beneficiary (institutional/arbitrage) — invokes 49.3/16, sets agenda
 *   - presidency_as_institution: durable beneficiary across incumbents — accumulates precedent
 *   - national_assembly: primary target (organized/constrained) — bypassed lawmaking function
 *   - opposition_parties: secondary target (organized/constrained) — foreclosed substantive input
 *   - civil_society_organizations: diffuse target (moderate/constrained) — lost deliberative channel
 *   - constitutional_council: analytical observer with limited binding effect
 *   - electorate: excluded voice — split mandate not honored symmetrically
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__hyper_presidential_reading, 0.71).
domain_priors:suppression_score(fifth_republic_constitution__hyper_presidential_reading, 0.62).
domain_priors:theater_ratio(fifth_republic_constitution__hyper_presidential_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__hyper_presidential_reading, tangled_rope).
narrative_ontology:human_readable(fifth_republic_constitution__hyper_presidential_reading, "Fifth Republic Constitution — Hyper-Presidential Reading (President as Direct Sovereign)").
narrative_ontology:topic_domain(fifth_republic_constitution__hyper_presidential_reading, "constitutional_law/political_systems").

domain_priors:requires_active_enforcement(fifth_republic_constitution__hyper_presidential_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__hyper_presidential_reading, '779d2437-dafe-4e2a-80f6-6da294152052').
narrative_ontology:cs_kernel_codification('779d2437-dafe-4e2a-80f6-6da294152052', formalized).
narrative_ontology:cs_authority_grounding('779d2437-dafe-4e2a-80f6-6da294152052', extraction).
narrative_ontology:cs_interpretation_layer_present('779d2437-dafe-4e2a-80f6-6da294152052').
narrative_ontology:cs_reading_relation('779d2437-dafe-4e2a-80f6-6da294152052', fifth_republic_constitution__parliamentary_constraint_reading, coexists_with).
narrative_ontology:cs_reading_relation('779d2437-dafe-4e2a-80f6-6da294152052', fifth_republic_constitution__cohabitation_equilibrium_reading, influences).
narrative_ontology:cs_axiom('779d2437-dafe-4e2a-80f6-6da294152052', foundational, direct_mandate_supersedes_legislative_mandate).
narrative_ontology:cs_axiom_status(direct_mandate_supersedes_legislative_mandate, holdable).
narrative_ontology:cs_axiom_grounding('779d2437-dafe-4e2a-80f6-6da294152052', direct_mandate_supersedes_legislative_mandate, conventional).
narrative_ontology:cs_axiom('779d2437-dafe-4e2a-80f6-6da294152052', secondary, presidential_primacy_required_for_governability).
narrative_ontology:cs_axiom_status(presidential_primacy_required_for_governability, holdable).
narrative_ontology:cs_axiom_grounding('779d2437-dafe-4e2a-80f6-6da294152052', presidential_primacy_required_for_governability, instrumental).
narrative_ontology:cs_reference_frame('779d2437-dafe-4e2a-80f6-6da294152052', de_gaulle_founding_settlement).
narrative_ontology:cs_drift_state('779d2437-dafe-4e2a-80f6-6da294152052', contemporary_routine_49_3_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('779d2437-dafe-4e2a-80f6-6da294152052', '').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__hyper_presidential_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__hyper_presidential_reading, presidency_as_institution).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__hyper_presidential_reading, incumbent_president).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, national_assembly).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, opposition_parties).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, civil_society_organizations).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__hyper_presidential_reading, direct_popular_mandate_supremacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Elected by direct national suffrage, invokes Article 49.3 to force legislation through without a vote and Article 16 to assume near-total emergency powers when the president judges institutions threatened. Appoints the prime minister, can dissolve the Assembly, and sets the legislative agenda via government bills. Frames these powers as the direct expression of the people's will, bypassing what is characterized as fragmented parliamentary factionalism.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, incumbent_president, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__hyper_presidential_reading, incumbent_president, beneficiary).

% Accumulates constitutional practice and precedent each time 49.3 or Article 16 is invoked without successful censure; each use normalizes the next, embedding hyper-presidential authority into institutional memory independent of which individual holds the office.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, presidency_as_institution, beneficiary,
    institutional, civilizational, analytical, national).

% Elected legislature whose ordinary lawmaking function is bypassed when the government invokes 49.3, which deems a bill passed unless a censure motion succeeds — a high-threshold reversal that structurally favors the executive. Its only recourse against Article 16 is largely advisory (Constitutional Council consultation after 30 days). Cannot force votes on presidential priorities without cooperation from a government that a hyper-presidential reading holds does not owe it substantive deference.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, national_assembly, payer,
    organized, biographical, constrained, national).

% Hold seats in the Assembly proportional to electoral support but find substantive legislative input foreclosed when the government routes contested bills through 49.3. Their remaining tools — no-confidence motions, public mobilization, Constitutional Council referral — face high procedural or political barriers under this reading of presidential primacy.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, opposition_parties, payer,
    organized, biographical, constrained, national).

% Advocacy groups, unions, and public commentators who would ordinarily engage the deliberative process attached to ordinary legislative debate lose that channel when bills are pushed through without a vote; their only remaining leverage is street mobilization or litigation after the fact.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, civil_society_organizations, payer,
    moderate, biographical, constrained, national).

% Reviews the constitutionality of legislation and, for Article 16, is consulted on the emergency declaration and, after 30/60 days, on its continuation. Under the hyper-presidential reading its role is advisory and deferential rather than a binding check — it can annul specific provisions but has not historically blocked invocation of executive-primacy powers outright.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, constitutional_council, observer,
    institutional, generational, analytical, national).

% Voted for both a president and an Assembly, expecting both mandates to carry weight; under this reading, only the presidential mandate is treated as authoritative between elections, and their Assembly votes exercise diminished practical effect on contested policy. They have no standing mechanism to object to how the two mandates are weighted against each other except at the next election.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, electorate, excluded,
    powerless, biographical, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for the executive to break legislative deadlock and govern decisively when Assembly coalitions are fragmented or obstructive, avoiding the paralysis that afflicted the Fourth Republic.
% TRANSFER_FUNCTION: Moves effective lawmaking authority from the elected Assembly to the president and the government the president appoints, converting a bill's normal passage-by-vote into passage-by-non-censure, and in emergencies concentrating near-total governing authority in the presidency.
% ABSENT_VOICES: The National Assembly's ordinary members, opposition blocs, and the civil-society constituencies who rely on legislative debate are structurally bypassed each time 49.3 or Article 16 is invoked; they are formally represented but substantively excluded from the specific decision.
% DISAPPEARANCE_RATIONALE: If the hyper-presidential reading were displaced (e.g., by binding judicial limits on 49.3/16 or a constitutional amendment requiring affirmative legislative votes), contested legislation would require actual coalition-building in the Assembly, no-confidence dynamics would carry real weight, and governing majorities would need genuine parliamentary support rather than the ability to dare a censure vote.
% FOUNDING_PROBLEM: The Fourth Republic's parliamentary system produced chronic cabinet instability and legislative paralysis; the 1958 constitution was built to give the executive tools to govern decisively despite a fragmented or hostile Assembly.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars and comparative-government researchers outside the presidency attest that legislative fragmentation of Fourth Republic severity has not recurred, and that repeated 49.3 use under stable majorities reflects executive convenience rather than genuine deadlock-breaking; incumbents and their governments attest the tools remain necessary for governability. Independent political science literature on Article 49.3 usage frequency supports the contested framing rather than a resolved one.
narrative_ontology:disappearance_verdict(fifth_republic_constitution__hyper_presidential_reading, world_rearranges).
narrative_ontology:founding_problem_status(fifth_republic_constitution__hyper_presidential_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__hyper_presidential_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored high (0.71) and rising over the interval because repeated, routine use of 49.3 (rather than exceptional use limited to genuine deadlock) converts an emergency tool into a standing method of legislating without a vote — this is extraction of legislative authority from the Assembly's ordinary members and the electorate's parliamentary mandate. Suppression (0.62) reflects the structural difficulty of reversing a 49.3-forced bill (a successful absolute-majority censure motion, which also risks dissolving the government) and the near-unreviewable character of an Article 16 declaration in practice. Theater ratio is moderate-low (0.28): the underlying function — breaking genuine deadlock — is real and was clearly needed at points in the Republic's history, but a rising share of invocations occur under stable, disciplined majorities where deadlock is not really present, which is the theatrical residue this reading is tracking. Accessibility collapse (0.58) and resistance (0.55) are mid-range: alternatives (coalition negotiation, ordinary votes) remain constitutionally available and are actively fought for by opposition and civil society, so the constraint has not achieved the near-total collapse a mountain would show.
 *
 * PERSPECTIVAL GAP:
 *   From the incumbent president's seat, this reading experiences the constitution as functioning exactly as designed — a mandate to govern decisively that the Assembly's own fragmentation makes necessary. From the National Assembly's seat, the identical structure computes as extraction: their electoral mandate is real but structurally subordinated whenever the executive judges it convenient to invoke 49.3 or 16. The engine computes these as different seat-classifications from the same structural data; this reading does not average them — it authors ONE reading's epsilon (the hyper-presidential one) and lets the divergence with sibling readings live in separate files.
 *
 * DIRECTIONALITY LOGIC:
 *   The incumbent president and the presidency as an enduring institution are declared beneficiaries: they collect governing capacity, agenda control, and durable constitutional precedent from every successful invocation, with the president individually holding arbitrage-grade exit (control of dissolution timing, appointment powers) and the institution itself operating on a civilizational time horizon that outlasts any single incumbent. The National Assembly, opposition parties, and civil society are declared victims: they bear the cost of bypassed deliberation with only constrained exit (a censure motion that risks their own dissolution, or extra-institutional mobilization). The electorate is excluded rather than victimized directly in the metric sense — their parliamentary votes are structurally discounted, which the six-questions layer captures as an absent voice rather than a beneficiary/victim declaration, since the electorate is not a party running the arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — Fourth Republic legislative paralysis — is genuinely dead in its acute historical form (no return to multi-government-per-year instability has recurred), yet the tools built to solve it (49.3, Article 16) have been retained and, on this reading, expanded in routine use. This is close to a mandatrophy pattern: a mandate whose founding crisis has resolved but whose emergency architecture persists and hardens. The founding_problem_status is authored as 'contested' rather than flatly 'dead' because the hyper-presidential reading's own defenders argue coalition fragmentation risk remains latent and the tools remain a standing insurance policy — that defense is exactly the self-interested corroboration the mismatch-consumer is designed to flag against outside political-science attestation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hyper_presidential_vs_parliamentary_framing,
    'Is the Fifth Republic constitution''s true center of gravity the presidency''s Article 49.3/Article 16 powers (hyper-presidential reading) or the ordinary legislative process that those articles interrupt only exceptionally (parliamentary_constraint_reading)?',
    'Empirical frequency analysis of 49.3/Article 16 invocations relative to genuine legislative deadlock (measured by coalition fragmentation indices and failed-vote counts) across the Republic''s history; a pattern of routine use under stable majorities supports the hyper-presidential reading, a pattern confined to genuine crises supports the parliamentary reading.',
    'If invocation frequency tracks deadlock, this reading''s high extractiveness score overstates the constraint''s actual operation and the parliamentary_constraint_reading is the better-fitting account for most of the interval; if invocation is substantially decoupled from deadlock (as the rising measurement series here asserts), the hyper-presidential reading is vindicated as the operative pattern in recent decades.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hyper_presidential_vs_parliamentary_framing, conceptual, 'Whether the hyper-presidential or parliamentary-constraint reading better characterizes the constitution''s operative center of gravity.').

omega_variable(
    cohabitation_boundary_condition,
    'Does the hyper-presidential reading hold uniformly across the Republic''s history, or does it collapse into the cohabitation_equilibrium_reading whenever the president''s party lacks an Assembly majority?',
    'Comparative case analysis of presidential power exercise during the Republic''s historical cohabitation periods versus unified-majority periods — if presidential dominance measurably weakens during cohabitation, the hyper-presidential reading is conditional rather than general.',
    'If conditional, this story''s epsilon should be understood as an upper bound applicable only to unified-majority periods, and a time-varying reading (switching between this constraint and cohabitation_equilibrium_reading) would more accurately model the full interval than a single continuous series.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cohabitation_boundary_condition, empirical, 'Whether hyper-presidential dominance is a constant or conditional on unified government.').

omega_variable(
    article_16_binding_review_ambiguity,
    'Is the Constitutional Council''s post-60-day review of an Article 16 emergency a genuine binding check or a purely advisory formality under actual practice?',
    'Review of the historical record of Article 16 invocations and whether Council consultation has ever materially altered or terminated an emergency declaration against presidential wishes.',
    'If genuinely advisory only, the suppression score for this reading is likely understated; if it has functioned as a real check in practice, this reading''s suppression score may overstate the constraint''s coercive character relative to the parliamentary reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(article_16_binding_review_ambiguity, empirical, 'Whether Constitutional Council review of Article 16 functions as a real or merely formal check.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__hyper_presidential_reading, 1958, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fift_tr_t1958, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 1958, 0.12).
narrative_ontology:measurement(fift_tr_t1970, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(fift_tr_t1986, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 1986, 0.18).
narrative_ontology:measurement(fift_tr_t2000, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(fift_tr_t2015, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 2015, 0.24).
narrative_ontology:measurement(fift_tr_t2024, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(fift_be_t1958, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 1958, 0.42).
narrative_ontology:measurement(fift_be_t1970, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 1970, 0.48).
narrative_ontology:measurement(fift_be_t1986, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 1986, 0.51).
narrative_ontology:measurement(fift_be_t2000, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(fift_be_t2015, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 2015, 0.65).
narrative_ontology:measurement(fift_be_t2024, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 2024, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(fift_su_t1958, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 1958, 0.4).
narrative_ontology:measurement(fift_su_t1970, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 1970, 0.45).
narrative_ontology:measurement(fift_su_t1986, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 1986, 0.5).
narrative_ontology:measurement(fift_su_t2000, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 2000, 0.53).
narrative_ontology:measurement(fift_su_t2015, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 2015, 0.58).
narrative_ontology:measurement(fift_su_t2024, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_republic_constitution__hyper_presidential_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fifth_republic_constitution__hyper_presidential_reading, fifth_republic_constitution__parliamentary_constraint_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__hyper_presidential_reading, fifth_republic_constitution__cohabitation_equilibrium_reading).

% DUAL FORMULATION NOTE:
% Three constraint stories decompose the natural-language 'Fifth Republic Constitution' kernel: this story (hyper_presidential_reading, high extraction, tangled_rope), fifth_republic_constitution__parliamentary_constraint_reading (president as coordinated executive requiring legislative authorization, materially lower extraction, likely rope or scaffold), and fifth_republic_constitution__cohabitation_equilibrium_reading (dual executive requiring negotiated allocation, conditional on divided government, likely rope-leaning tangled_rope). Each carries its own epsilon and beneficiary/victim structure per the ε-invariance principle; they are linked via affects_constraints rather than merged into one story with an observable parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
