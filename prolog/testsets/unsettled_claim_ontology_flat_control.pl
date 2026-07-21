% ============================================================================
% CONSTRAINT STORY: unsettled_claim_ontology_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unsettled_claim_ontology_flat_control, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: unsettled_claim_ontology_flat_control
 *   human_readable: Social Pricing of Untested Self-Assertive Claims (Ability, Nerve, Capacity for Violence)
 *   domain: social epistemology / signaling theory / conflict economics
 *
 * SUMMARY:
 *   An untested or barely-tested self-assertive claim — 'I can do this,' 'I
 *   won't back down,' 'I would hurt you if pushed' — circulates as social
 *   currency before anyone verifies it. The framework treats this as a single
 *   constraint (flat construction, no decomposition into readings): a
 *   mechanism that both solves a genuine coordination problem (avoiding
 *   constant costly tests of everyone's actual capacity) and enables
 *   asymmetric extraction (those whose claims circulate favorably collect
 *   deference that those who decline to claim, or whose claims are
 *   disbelieved, must pay for). The rising theater_ratio over the interval
 *   reflects a mechanism increasingly dominated by delivery and volume rather
 *   than underlying accuracy — claims get more elaborate and
 *   confident-sounding even as the gap between claim and tested capacity
 *   widens, because testing remains costly and rare relative to claiming.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsettled_claim_ontology_flat_control, 0.52).
domain_priors:suppression_score(unsettled_claim_ontology_flat_control, 0.44).
domain_priors:theater_ratio(unsettled_claim_ontology_flat_control, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsettled_claim_ontology_flat_control, extractiveness, 0.52).
narrative_ontology:constraint_metric(unsettled_claim_ontology_flat_control, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(unsettled_claim_ontology_flat_control, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unsettled_claim_ontology_flat_control, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(unsettled_claim_ontology_flat_control, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsettled_claim_ontology_flat_control, tangled_rope).
narrative_ontology:human_readable(unsettled_claim_ontology_flat_control, "Social Pricing of Untested Self-Assertive Claims (Ability, Nerve, Capacity for Violence)").
narrative_ontology:topic_domain(unsettled_claim_ontology_flat_control, "social epistemology / signaling theory / conflict economics").

domain_priors:requires_active_enforcement(unsettled_claim_ontology_flat_control).

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(unsettled_claim_ontology_flat_control, unsettled_claim_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsettled_claim_ontology_flat_control, confident_claimants).
narrative_ontology:constraint_beneficiary(unsettled_claim_ontology_flat_control, status_hierarchy_incumbents).
narrative_ontology:constraint_beneficiary(unsettled_claim_ontology_flat_control, reputation_intermediaries).
narrative_ontology:constraint_victim(unsettled_claim_ontology_flat_control, accurate_low_confidence_signalers).
narrative_ontology:constraint_victim(unsettled_claim_ontology_flat_control, untested_challengers).
narrative_ontology:constraint_victim(unsettled_claim_ontology_flat_control, bystanders_forced_to_adjudicate).
narrative_ontology:constraint_vindicates(unsettled_claim_ontology_flat_control, cheap_talk_has_social_value).
narrative_ontology:constraint_vindicates(unsettled_claim_ontology_flat_control, reputation_substitutes_for_verification).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Makes an assertion of ability, nerve, or willingness to escalate that has not been put to a real test. Collects deference, status, or avoidance-of-challenge on the strength of the claim circulating uncontested. Benefits precisely because the claim remains untested; a real test could resolve the claim downward as easily as upward, so the claimant's rational strategy is to keep circulation high and testing low.
narrative_ontology:constraint_stakeholder(unsettled_claim_ontology_flat_control, confident_claimants, beneficiary,
    moderate, biographical, constrained, local).

% Existing high-status actors administer the informal rules for which claims get challenged, ignored, or accepted at face value. They set the local norm about how much testing a claim requires before it is treated as settled, and that norm-setting power lets them ratify claims that preserve the existing pecking order while subjecting challengers' claims to harsher scrutiny.
narrative_ontology:constraint_stakeholder(unsettled_claim_ontology_flat_control, status_hierarchy_incumbents, beneficiary,
    organized, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(unsettled_claim_ontology_flat_control, status_hierarchy_incumbents, agenda_setter).

% Third parties (gossips, seconds, witnesses, rating systems) who relay and amplify claims without direct exposure to the underlying risk. They profit in attention, standing, or brokerage fees from being the ones who know or transmit the story, and have no incentive to force costly verification that would collapse their intermediary role.
narrative_ontology:constraint_stakeholder(unsettled_claim_ontology_flat_control, reputation_intermediaries, beneficiary,
    moderate, biographical, mobile, local).

% Individuals whose honest self-assessment is modest, or who decline to make an untested claim at all, are read as weaker than their actual capability relative to the loud claimants around them. They pay in status, access, and safety for refusing to participate in the same cheap-signal market, and have no low-cost way to prove their accuracy without triggering the very tests they were trying to avoid.
narrative_ontology:constraint_stakeholder(unsettled_claim_ontology_flat_control, accurate_low_confidence_signalers, payer,
    powerless, immediate, trapped, local).

% Anyone who contests a circulating claim must either back down (losing standing) or force a real test (bearing the actual physical, social, or legal risk of resolving what the claim asserted). The claimant who made the original assertion faces no symmetric cost for having made it; the burden of resolution falls disproportionately on whoever calls it.
narrative_ontology:constraint_stakeholder(unsettled_claim_ontology_flat_control, untested_challengers, payer,
    powerless, immediate, trapped, local).

% Community members, employers, or onlookers must act on the claim's apparent validity (who to hire, who to defer to, who to avoid a confrontation with) without being consulted on how much testing should have been required before the claim was allowed to circulate as settled fact. Their practical decisions ratify the claim's social force regardless of its truth.
narrative_ontology:constraint_stakeholder(unsettled_claim_ontology_flat_control, bystanders_forced_to_adjudicate, excluded,
    powerless, immediate, constrained, local).

% Study the pricing mechanism itself: whether the circulating claim functions as a genuine (if noisy) signal of underlying capacity, a pure bluff sustained by mutual avoidance of costly verification, or a status commodity whose relationship to the underlying capacity is incidental. They have no stake in any particular claim's resolution.
narrative_ontology:constraint_stakeholder(unsettled_claim_ontology_flat_control, conflict_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unsettled_claim_ontology_flat_control, confident_claimants).
narrative_ontology:fixing_cost_class(unsettled_claim_ontology_flat_control, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Absent perfect information about who can actually do what, a circulating claim of ability or nerve lets a community allocate deference, avoid costly physical tests, and coordinate around a provisional ranking without everyone fighting everyone to find out who is strongest — a real, if crude, coordination problem the claim-economy solves.
% TRANSFER_FUNCTION: Moves deference, safety, access, and standing from those who decline to make unverified self-assertive claims (or whose claims are disbelieved) to those whose claims circulate successfully, regardless of whether the underlying ability or nerve the claim asserts is ever actually present.
% ABSENT_VOICES: Whoever eventually loses a forced real test (the challenger who calls the bluff and is beaten, or the claimant who is exposed as bluffing) is retroactively written out of the story as having 'obviously' been on the wrong side of the claim all along; their prior, unresolved position — that the claim was genuinely uncertain — is erased by the outcome and never gets to argue that the pricing mechanism itself was unfair before the resolution.
% DISAPPEARANCE_RATIONALE: If untested self-assertive claims could no longer circulate as social currency — if every claim of ability or nerve required immediate, low-cost, low-stakes verification — status hierarchies built on unverified reputation would collapse, confident claimants would lose their principal lever, reputation intermediaries would lose their brokerage function, and accurate low-confidence signalers would gain relative standing. The entire local status economy reorganizes.
% FOUNDING_PROBLEM: Communities need a fast, low-cost way to estimate who can be trusted with authority, who is dangerous to cross, and who should be deferred to, without constantly running actual physical or competence tests that are costly, risky, or destructive to run on everyone.
% FOUNDING_PROBLEM_CORROBORATION: Conflict economists and game theorists outside the claim-making population attest the coordination problem (avoiding costly tests) is real and persists; but they also attest, from the same outside seat, that the mechanism as currently priced systematically rewards volume and delivery confidence over accuracy, meaning the 'signal' component has substantially decoupled from the underlying capacity it is nominally about — no one inside the beneficiary group (confident claimants, incumbents, intermediaries) corroborates this decoupling, since acknowledging it would devalue their own position.
narrative_ontology:disappearance_verdict(unsettled_claim_ontology_flat_control, world_rearranges).
narrative_ontology:founding_problem_status(unsettled_claim_ontology_flat_control, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unsettled_claim_ontology_flat_control, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-21',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(unsettled_claim_ontology_flat_control, 'none', 1).
narrative_ontology:epsilon_provenance(unsettled_claim_ontology_flat_control, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unsettled_claim_ontology_flat_control_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unsettled_claim_ontology_flat_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unsettled_claim_ontology_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts moderate (0.34) and rises to 0.52 as the claim-economy matures and confident claimants learn the mechanism rewards assertion over accuracy, without anyone bearing symmetric cost for having claimed falsely. Suppression is present but not dominant (0.44 at end) — it operates less through direct coercion than through the social cost of challenging a claim (loss of face if the challenge fails, real risk if it succeeds in provoking a test). Theater ratio rises fastest (0.30 to 0.58) because performative confidence — tone, posture, reputation-by-association — increasingly substitutes for the actual tested capacity it purports to represent; this is the Goodhart signature of a signal decoupling from its referent. Accessibility collapse is moderate (0.40): alternatives to participating in the claim-economy exist (declining to claim, seeking third-party verification, building track record slowly) but are systematically disadvantaged, not eliminated. Resistance is substantial (0.61) because untested challengers and accurate low-confidence signalers have real incentive to contest a mechanism that undervalues them, even though contesting it individually is costly.
 *
 * PERSPECTIVAL GAP:
 *   From the confident claimant's seat, the mechanism looks like an efficient coordination device — a way to communicate ability or resolve without constant costly fighting, and their own claims are simply accurate assertions of real capacity. From the untested challenger's or accurate low-confidence signaler's seat, the same mechanism looks like an extraction racket that prices bluster over substance and forces the marginal-cost party (whoever calls the bluff) to subsidize the claimant's free-riding on unverified reputation. The engine should compute these as genuinely different seat-level classifications from the same structural data, not as one side simply being wrong about the facts — both descriptions can be locally accurate depending on whether a given claimant's underlying capacity happens to match the claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Confident claimants, status incumbents, and reputation intermediaries sit near the beneficiary end: they collect deference, standing, or brokerage value from claims circulating without being tested, and bear no symmetric cost when a claim goes untested indefinitely. Accurate low-confidence signalers and untested challengers sit near the target end: they either forgo the currency entirely (paying in relative status) or bear the concentrated cost of forcing resolution (paying in actual risk) while the original claimant risks comparatively little for having made the claim. Bystanders are excluded from setting the verification standard but are structurally forced to act on the claim's apparent validity, which is why they are marked excluded rather than payer or beneficiary — their cost is indirect, borne through bad allocation decisions rather than direct extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding coordination problem (avoid constant, costly, mutually destructive testing of who can do what) remains genuinely live in any community without perfect information — this prevents the constraint from being mislabeled a pure snare. But the specific pricing mechanism observed here has drifted: rising theater_ratio and rising extractiveness together show the mechanism increasingly rewarding delivery confidence and claim volume divorced from tested accuracy, which is the signature of extraction riding on a real coordination shell. Tangled Rope, not Rope: the coordination function is real (bystanders and communities do benefit from having SOME provisional ranking to act on rather than none) and the extraction is real (accurate signalers pay disproportionately for a signal system biased toward loud, cheap assertion) — both must be true simultaneously for this classification, and the temporal data shows exactly that co-occurrence rather than one crowding out the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    signal_referent_ambiguity,
    'Underneath the circulating claim, is there a real (if noisy) correlation between the claim and the speaker''s actual tested capacity, or has the claim decoupled entirely into a pure status commodity priced on delivery and confidence alone?',
    'Compare claim content against outcomes in the subset of cases where claims are eventually tested (forced confrontations, competence trials, verified track records over time); measure correlation between claim confidence and test outcome.',
    'If correlation remains substantial, the mechanism functions closer to a genuine noisy signal and the classification should weight toward the coordination pole (rope-adjacent); if correlation has collapsed toward zero, the extraction pole dominates and the constraint is closer to a pure snare wearing a coordination costume.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(signal_referent_ambiguity, empirical, 'Whether the claim still tracks the underlying capacity it asserts or has decoupled into pure signaling theater.').

omega_variable(
    verification_cost_asymmetry,
    'Is the asymmetric cost of verification (challenger bears the real risk, claimant bears little) an inherent structural feature of testing capacity/nerve claims, or is it a constructed norm that could be redistributed (e.g., via third-party arbitration, reputation staking, or graduated low-stakes tests)?',
    'Examine communities or institutions that have implemented alternative verification mechanisms (arbitrated duels, credentialing bodies, staged low-stakes trials) and compare extraction and suppression metrics against communities using pure informal claim-circulation.',
    'If the asymmetry is inherent to the domain (violence and physical ability are genuinely hard to verify cheaply), the tangled_rope classification is close to unavoidable; if institutional alternatives demonstrably rebalance the cost, the current arrangement looks more like an unnecessary extraction layer that persists through incumbent preference rather than necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_cost_asymmetry, conceptual, 'Whether the challenger-bears-cost asymmetry is structurally necessary or a maintained choice.').

omega_variable(
    what_object_is_priced,
    'What kind of object is actually being priced when the claim circulates — a probabilistic estimate of true capacity, a commitment device (the claimant now has reputational skin in the game), or a pure status token disconnected from any underlying referent?',
    'This is the substrate''s central interpretive fork (deliberately left open in this flat authoring — the source material names it as the thing all readings disagree about); resolving it would require decomposing into separate reading-stories per the ε-invariance principle, each with its own ε and stakeholder structure, rather than forcing one measurement onto three candidate ontologies.',
    'Each candidate answer implies a different constraint with a different ε: a probability-estimate reading trends toward rope/mountain-adjacent (genuine if imperfect information transfer), a commitment-device reading trends toward tangled_rope (real coordination value plus enforcement cost), and a pure-status-token reading trends toward snare (extraction with no informational content at all). This story deliberately averages none of these and instead measures the flat, undecomposed substrate as authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(what_object_is_priced, conceptual, 'The undecomposed ontological fork the flat-construction control is designed to leave unresolved.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsettled_claim_ontology_flat_control, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unse_tr_t0, unsettled_claim_ontology_flat_control, theater_ratio, 0, 0.3).
narrative_ontology:measurement(unse_tr_t4, unsettled_claim_ontology_flat_control, theater_ratio, 4, 0.37).
narrative_ontology:measurement(unse_tr_t8, unsettled_claim_ontology_flat_control, theater_ratio, 8, 0.44).
narrative_ontology:measurement(unse_tr_t12, unsettled_claim_ontology_flat_control, theater_ratio, 12, 0.49).
narrative_ontology:measurement(unse_tr_t16, unsettled_claim_ontology_flat_control, theater_ratio, 16, 0.53).
narrative_ontology:measurement(unse_tr_t20, unsettled_claim_ontology_flat_control, theater_ratio, 20, 0.56).
narrative_ontology:measurement(unse_tr_t24, unsettled_claim_ontology_flat_control, theater_ratio, 24, 0.58).

% Extraction over time
narrative_ontology:measurement(unse_be_t0, unsettled_claim_ontology_flat_control, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(unse_be_t4, unsettled_claim_ontology_flat_control, base_extractiveness, 4, 0.39).
narrative_ontology:measurement(unse_be_t8, unsettled_claim_ontology_flat_control, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(unse_be_t12, unsettled_claim_ontology_flat_control, base_extractiveness, 12, 0.47).
narrative_ontology:measurement(unse_be_t16, unsettled_claim_ontology_flat_control, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(unse_be_t20, unsettled_claim_ontology_flat_control, base_extractiveness, 20, 0.51).
narrative_ontology:measurement(unse_be_t24, unsettled_claim_ontology_flat_control, base_extractiveness, 24, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(unse_su_t0, unsettled_claim_ontology_flat_control, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(unse_su_t4, unsettled_claim_ontology_flat_control, suppression_requirement, 4, 0.33).
narrative_ontology:measurement(unse_su_t8, unsettled_claim_ontology_flat_control, suppression_requirement, 8, 0.36).
narrative_ontology:measurement(unse_su_t12, unsettled_claim_ontology_flat_control, suppression_requirement, 12, 0.39).
narrative_ontology:measurement(unse_su_t16, unsettled_claim_ontology_flat_control, suppression_requirement, 16, 0.41).
narrative_ontology:measurement(unse_su_t20, unsettled_claim_ontology_flat_control, suppression_requirement, 20, 0.43).
narrative_ontology:measurement(unse_su_t24, unsettled_claim_ontology_flat_control, suppression_requirement, 24, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unsettled_claim_ontology_flat_control, identity_coordination).
narrative_ontology:boltzmann_floor_override(unsettled_claim_ontology_flat_control, 0.08).

% DUAL FORMULATION NOTE:
% This is the flat (undecomposed) control construction for the unsettled-claim-ontology substrate. Per the ε-invariance principle, the three candidate readings implicit in the source material (probability-estimate reading, commitment-device reading, pure-status-token reading) would ordinarily be decomposed into separate linked stories, each with its own stable ε. This story deliberately does NOT perform that decomposition — it measures the substrate as one constraint with one ε, to serve as the comparison baseline against any reading-decomposed variant of the same substrate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
