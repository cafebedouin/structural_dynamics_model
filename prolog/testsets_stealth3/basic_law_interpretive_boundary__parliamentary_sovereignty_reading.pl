% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_boundary__parliamentary_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_boundary__parliamentary_sovereignty_reading, []).

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
 *   constraint_id: basic_law_interpretive_boundary__parliamentary_sovereignty_reading
 *   human_readable: Parliamentary Sovereignty Reading — Knesset Interpretive Supremacy over Basic Laws
 *   domain: constitutional law/comparative constitutionalism/judicial review theory
 *
 * SUMMARY:
 *   'Who holds the last word on Israel's Basic Laws' is a colloquial label
 *   covering three structurally distinct arrangements sharing one kernel
 *   (basic_law_interpretive_boundary). This file instantiates ONE of them
 *   cleanly, per epsilon-invariance: the parliamentary sovereignty reading,
 *   under which the Knesset — whatever coalition commands 61 seats — is the
 *   ultimate interpreter and amender of Basic Laws by simple majority, with
 *   binding judicial review replaced by advisory consultation and a
 *   legislative override. The standing arrangement under contest is this
 *   supremacy arrangement itself; epsilon is authored for it by this
 *   reading's own lights (OQ-26): low, because the reading locates legitimacy
 *   in electoral mandate rather than extraction, with residual extraction the
 *   reading itself concedes at the margins (coalition-insulation uses of the
 *   override, minority exposure). The sibling files instantiate different
 *   constraints, not alternative views of this one:
 *   judicial_supremacy_reading makes binding court invalidation the operative
 *   rule (the victim set shifts to legislative majorities; epsilon rises for
 *   legislative seats), and balanced_contestation_reading partitions
 *   authority between bounded court interpretation and treaty-constrained
 *   legislative sovereignty (both victim sets shrink). Their epsilon values
 *   differ by construction; the stories are linked, not merged. Historically
 *   the arrangement was near-operative from statehood until the 1990s
 *   constitutional revolution, was pushed back by court assertions through
 *   the 2000s and 2010s, and entered open revival contest in 2023-2026 — the
 *   interval and the shared-grid measurement series trace that arc.
 *
 * KEY AGENTS:
 *   - knesset_governing_coalitions: agenda-setting seat (institutional/arbitrage) — writes, amends, and overrides; collects the arrangement's gains
 *   - incumbent_sectoral_factions: beneficiary seat (powerful/arbitrage) — converts pivotal votes into override-proof statutes
 *   - parliamentary_opposition_parties: payer seat (powerful/constrained) — bears depreciation of every procedural weapon it builds
 *   - arab_minority_citizens: payer seat (organized/constrained) — permanent out-coalition; loses the judicial protection channel
 *   - judicial_review_dependents: payer seat (moderate/constrained) — diffuse rights-holders whose claims become overridable
 *   - israeli_supreme_court_judiciary: payer seat (institutional/identity_locked) — guardian identity fused with authority the arrangement strips
 *   - future_knesset_majorities: payer seat (powerful/constrained) — inherit the ratchet their rivals armed
 *   - international_treaty_bodies: excluded seat (institutional/trapped, global) — the residual external limit the reading concedes
 *   - comparative_constitutional_observers: analytical seat — no stakes; feeds the sibling legitimacy contests
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.28).
domain_priors:suppression_score(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.72).
domain_priors:theater_ratio(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, "Parliamentary Sovereignty Reading — Knesset Interpretive Supremacy over Basic Laws").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, "constitutional law/comparative constitutionalism/judicial review theory").

domain_priors:requires_active_enforcement(basic_law_interpretive_boundary__parliamentary_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, '83e92ecc-d85b-4597-bd3e-b045d2f722f4').
narrative_ontology:cs_kernel_codification('83e92ecc-d85b-4597-bd3e-b045d2f722f4', fixed_text).
narrative_ontology:cs_authority_grounding('83e92ecc-d85b-4597-bd3e-b045d2f722f4', self_enforcing).
narrative_ontology:cs_reading_relation('83e92ecc-d85b-4597-bd3e-b045d2f722f4', basic_law_interpretive_boundary__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('83e92ecc-d85b-4597-bd3e-b045d2f722f4', basic_law_interpretive_boundary__balanced_contestation_reading, forecloses).
narrative_ontology:cs_axiom('83e92ecc-d85b-4597-bd3e-b045d2f722f4', foundational, indivisible_elective_sovereignty).
narrative_ontology:cs_axiom_status(indivisible_elective_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('83e92ecc-d85b-4597-bd3e-b045d2f722f4', indivisible_elective_sovereignty, deontological).
narrative_ontology:cs_axiom('83e92ecc-d85b-4597-bd3e-b045d2f722f4', foundational, simple_majority_constitutional_revision).
narrative_ontology:cs_axiom_status(simple_majority_constitutional_revision, holdable).
narrative_ontology:cs_axiom_grounding('83e92ecc-d85b-4597-bd3e-b045d2f722f4', simple_majority_constitutional_revision, conventional).
narrative_ontology:cs_reference_frame('83e92ecc-d85b-4597-bd3e-b045d2f722f4', elective_majority_last_word).
narrative_ontology:cs_drift_state('83e92ecc-d85b-4597-bd3e-b045d2f722f4', post_override_clause_campaign, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('83e92ecc-d85b-4597-bd3e-b045d2f722f4', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, knesset_governing_coalitions).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, incumbent_sectoral_factions).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, parliamentary_opposition_parties).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, arab_minority_citizens).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, judicial_review_dependents).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, future_knesset_majorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, israeli_supreme_court_judiciary).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Commands 61-plus seats; writes and rewrites Basic Laws by simple majority; drafts override clauses that convert judicial invalidations into rubber stamps; distributes committee chairs and coalition funds. Its exit is not leaving the arrangement but losing an election — until then it can amend any rule that binds it, including rules written by its predecessors.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, knesset_governing_coalitions, agenda_setter,
    institutional, biographical, arbitrage, national).

% Smaller coalition parties — ultra-Orthodox, settler-linked, and other sectoral blocs — trade pivotal votes for exemption laws, budget allocations, and settlement-policy guarantees. The override mechanism converts those concessions into statutes that later courts cannot unwind. They collect durable policy goods without administering the constitutional machinery themselves.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, incumbent_sectoral_factions, beneficiary,
    powerful, biographical, arbitrage, national).

% Hold seats, staff committees, and contest elections, but every procedural weapon they rely on — precedent, legal challenge, committee scrutiny — can be erased by the next simple-majority vote. Laws they pass today can be gutted tomorrow; their investment in institutional craft depreciates with each override.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, parliamentary_opposition_parties, payer,
    powerful, biographical, constrained, national).

% Roughly a fifth of citizens, represented by parties excluded from every governing coalition since statehood apart from brief external-support episodes. Their rights protection has historically run through Supreme Court litigation — land-allocation challenges, admissibility rulings, anti-discrimination petitions. Parliamentary supremacy closes that channel with no realistic electoral path to majority status; exit via emigration is possible but costly and stigmatizing.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, arab_minority_citizens, payer,
    organized, biographical, constrained, national).

% Diffuse rights-holders — women relying on access rulings, LGBTQ couples relying on court-recognized registration, disabled litigants, conscription-policy challengers — whose protection strategy assumes courts can invalidate legislation. Under an override-capable Knesset their claims become petitionable-but-overridable; their fallback is lobbying a coalition that does not need their votes.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, judicial_review_dependents, payer,
    moderate, biographical, constrained, national).

% Judges selected through a committee the governing coalition increasingly dominates, sworn to uphold Basic Laws they may no longer bind anyone with. Their professional identity fused with the post-1995 guardian role; retirement, resignation, or public defiance are the only exits, and each damages the institution they identify with. Advisory status reduces them to consultants on legislation they once could stop.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, israeli_supreme_court_judiciary, payer,
    institutional, generational, identity_locked, national).

% Tomorrow's coalitions inherit today's precedents: each simple-majority amendment demonstrates the method, lowers its cost, and arms successors — including ideological rivals — with the same tool. They pay in reduced option-value: any future majority inherits a ratchet it did not choose and cannot easily reverse without appearing to attack democracy itself.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, future_knesset_majorities, payer,
    powerful, generational, constrained, national).

% UN treaty bodies, international legal proceedings, and association-framework partners monitor conduct that domestic override clauses cannot touch. They hold no seat in the Knesset and no domestic veto; their leverage is reporting, treaty-body review, and legal processes outside Israeli jurisdiction. The reading concedes their residual constraint — the one limit parliamentary supremacy does not purport to dissolve.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, international_treaty_bodies, excluded,
    institutional, generational, trapped, global).

% Scholars comparing Israel's entrenchment and override debates with Westminster, Canada, New Zealand, and Denmark track whether simple-majority constitutional amendment produces instability or accountability. They cast no votes and bear no extraction; their analyses feed the legitimacy contests among the sibling readings.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, comparative_constitutional_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, knesset_governing_coalitions).
narrative_ontology:fixing_cost_class(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves divided-sovereignty coordination in a state with no formal constitution: gives the elected body an uncontested last word so that voter mandates translate into legislation without inter-institutional deadlock, keeps fundamental-law revision continuous with ordinary politics (no constitutional moments required), and preserves reversibility — any generation can undo its predecessors' amendments by the same simple route.
% TRANSFER_FUNCTION: Moves interpretive and amending authority over Basic Laws from courts and from any supermajority or entrenchment requirement to whichever coalition holds 61 seats; moves immunity-from-legal-challenge to coalition-specific statutes (exemption laws, settlement guarantees, budget frameworks); moves the burden of rights protection from judicial review onto electoral competition — a burden that lands hardest on groups excluded from every coalition.
% ABSENT_VOICES: Arab-minority citizens and their legal-rights organizations are present only as objects: they litigate the arrangements the coalition passes but hold no seat in any coalition that writes them. Future cohorts bound by precedents adopted in their absence have no seat at all. International treaty bodies observe from outside the domestic conversation. Under this reading their objections receive the answer 'win an election' — an answer structurally unavailable to permanent out-groups, which is the arrangement's sharpest absent-voice problem.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight — if the Knesset lost the simple-majority last word and judicial invalidations became binding — the world rearranges immediately: coalition bargains lose their enforceability guarantee (every sectoral concession becomes court-contestable), the legislative pipeline stalls pending review, appointment politics inverts as the judicial selection committee becomes the decisive constitutional actor, and the center of constitutional gravity migrates from the Knesset to the Supreme Court. Entrenchment politics erupts as both sides race to lock in rules while they still can.
% FOUNDING_PROBLEM: Israel founded without a formal constitution because religious-secular and ideological blocs could not agree on content; the Harari resolution (1950) proposed incremental Basic Laws, each passable by ordinary majority, deferring the constitution indefinitely. The arrangement solved: how to give a new state constitutional form without constitutional consensus, keeping fundamental-law revision continuous with ordinary politics.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration exists from outside the benefiting parties but is split along the kernel's fault line. Constitutional-historical scholarship on the founding-era deadlock and the Harari resolution, the Knesset's own archival record, and Israel Democracy Institute analyses attest the original problem and document that a formal constitution remains unreachable (supporting liveness). Former attorneys general, retired justices, and opposition jurists attest from outside the beneficiary set that the arrangement's present use — override clauses shielding coalition-specific policy — no longer serves that founding rationale. Coalition legal advisers, inside the beneficiary set, attest liveness. No single corroborator outside the benefiting parties attests the strong claim that the founding problem is fully live; the attestation record itself mirrors the three-reading contest.
narrative_ontology:disappearance_verdict(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.28, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_boundary__parliamentary_sovereignty_reading_tests).
:- end_tests(basic_law_interpretive_boundary__parliamentary_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. Claimed type is tangled_rope on structural evidence: the arrangement solves a real coordination problem (divided sovereignty in a constitution-less state — voter mandates must be able to become law without inter-institutional deadlock), AND it carries asymmetric extraction (any temporary 61-seat coalition can rewrite fundamental law and immunize its specific policies against later challenge, while permanent out-groups have no reciprocal path), AND it requires active enforcement (the Court does not accept advisory status voluntarily; the 2023-2026 override campaign is the enforcement machinery in motion). Metrics are descriptive of the arrangement's actual operation across the interval. Extractiveness is authored low-to-moderate (0.28 at interval end) because this reading's own lights locate the flows in democratic self-government, not extraction — but not near-zero, because even sympathetic authors of the reading concede the override clause's coalition-insulation motivations and the minority-exposure externality. Suppression (0.72) is a raw structural property, unscaled by power or scope: holding the arrangement against a court that claims binding review and a public that protested en masse takes real coercive-political force. Theater ratio (0.36) has risen with the widening gap between 'will of the people' rhetoric and narrow coalition arithmetic. Accessibility collapse is low (0.20): the sibling readings, supermajority-entrenchment designs, and constitutional-convention proposals are all alive — nothing about this arrangement forecloses alternatives in practice, which is precisely why enforcement is needed. Resistance is high (0.78): the 2023 protest wave, judicial pushback, and professional-body refusal constitute the largest constitutional mobilization in the country's history. The three measurement series share one 12-point grid (alignment rule). The recent segment shows a crisis-cycle shape — assertion, backlash, partial retreat (2023 to 2026) — and the escalation episodes themselves function as coalition-consolidation mechanisms (each crisis disciplines the bloc and hardens the override coalition), an intermittent-reinforcement analogue documented here rather than treated as noise.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and should. From the governing-coalition seat the arrangement is empowering coordination: it delivered every mandate the electorate issued and keeps constitutional evolution reversible — a rope experience. From the opposition and minority seats the same structure is exposure: every protection they rely on is one simple-majority vote from erasure, with no electoral route to reciprocity — a snare-flavored experience. From the judiciary's seat it is existential demotion: an institution whose post-1995 identity is guardianship of the Basic Laws is told its invalidations are suggestions. The engine derives these divergent per-seat classifications from the structural data (role, power, exit); this story does not adjudicate which experience is 'true' — the divergence is the finding. Same-level dynamics: opposition parties and governing factions hold nominally equal institutional standing (Knesset membership), differentiated entirely by the constraint-specific factor of coalition arithmetic — 61 seats versus not — which flips exit from arbitrage to constrained and separates their directionalities despite equal nominal power.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive the derivation. knesset_governing_coalitions and incumbent_sectoral_factions sit at the beneficiary end (d near 0.0): the arrangement subsidizes them with amendability and override-proof policy goods. parliamentary_opposition_parties, judicial_review_dependents, and future_knesset_majorities sit toward the target end (d high): they pay in depreciated procedural capital, overridable rights claims, and inherited ratchets. arab_minority_citizens sit nearest the full-target pole among citizen seats: extraction concentrates on a group with no coalition path (see omega permanent_minority_exit_answer). israeli_supreme_court_judiciary is a high-d payer whose identity_locked exit amplifies effective extraction — trapped guardians pay more than mobile ones. international_treaty_bodies are excluded rather than coordinated; their exclusion marks the enforcement object's outer boundary. No directionality_overrides are declared: the structural declarations plus exit options already produce the correct qualitative d for every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — giving a new state constitutional form without constitutional consensus (the Harari resolution of 1950 and the incremental Basic Laws strategy) — is authored as contested, not dead: a formal constitution remains unreachable, so the original rationale retains defenders, while opposition jurists and former attorneys general attest that the arrangement's present use (coalition insulation via override) no longer serves that rationale. The R5 mismatch consumer reads founding_problem_status (contested) against disappearance_verdict (world_rearranges): the arrangement is load-bearing today regardless of whether its founding justification is live — exactly the signature the mandatrophy apparatus exists to catch before it hardens into piton. The classification prevents mislabeling in both directions: calling this a snare would erase the genuine coordination function (deadlock avoidance, mandate translation, reversibility) that even critics rely on; calling it a rope would erase the asymmetric extraction that permanent minorities and future majorities demonstrably bear. Tangled rope is the honest middle, and the temporal series shows which way it is drifting — enforcement intensity and theater both rose as the arrangement's operative justification shifted from consensus-avoidance to coalition-protection.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This story instantiates the parliamentary_sovereignty_reading of the basic_law_interpretive_boundary kernel; how would the judicial_supremacy_reading and balanced_contestation_reading instantiate structurally different constraints from the same Basic Laws corpus?',
    'Adoption events: enactment of a binding override clause, formal court-entrenchment legislation, or a negotiated authority-partition framework would each shift the operative constraint to a sibling reading with a different victim set and different epsilon.',
    'Under judicial supremacy the constrained party becomes the Knesset majority itself (epsilon rises for legislative seats, falls for court-dependent seats); under balanced contestation authority partitions and both prior victim sets shrink. The classification in this file applies only while this reading is operative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer-frame routing: this constraint is one reading of a three-reading kernel; sibling readings are other files, not alternative views of this one.').

omega_variable(
    treaty_residual_constraint_weight,
    'How much constraining weight do international treaty obligations retain under operative parliamentary supremacy, given that the reading concedes them as the sole external limit?',
    'Track compliance behavior under an operative override regime: do coalition legislatures price treaty breach into majoritarian bills, or treat treaty obligations as overrideable like domestic law?',
    'If treaties prove overrideable in practice, the reading''s residual-limit concession collapses and effective extraction rises toward the judicial-supremacy baseline; if they bind, the arrangement keeps a genuine external floor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_residual_constraint_weight, empirical, 'Whether the conceded treaty-obligation exception is a real structural limit or a rhetorical reservation.').

omega_variable(
    precedent_ratchet_boomerang,
    'Does simple-majority constitutional amendment self-undermine as each coalition arms the mechanism for its successors, or does governmental alternation discipline majorities into restraint?',
    'Compare amendment rates and reversal patterns across alternating governments in Israel and comparable parliamentary systems (Denmark, New Zealand, pre-1997 Canada).',
    'A ratchet dynamic raises long-run extraction above the reading''s near-zero self-assessment and pushes classification toward snare characteristics; alternation discipline supports the rope-side coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precedent_ratchet_boomerang, empirical, 'Whether the amendment mechanism is self-limiting through alternation or self-amplifying through precedent.').

omega_variable(
    permanent_minority_exit_answer,
    'Is the reading''s implicit answer to minority objection — ''become the majority'' — actually available to permanently excluded blocs such as Arab citizens?',
    'Coalition-formation analysis across the full historical record: has any Arab-list participation ever converted into veto-bearing coalition membership, and is any such path visible under current fragmenting arithmetic?',
    'If no path exists, the arrangement''s extraction concentrates on a fixed group with no electoral recourse, contradicting the reading''s low-epsilon self-assessment and sharply raising effective chi for that seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(permanent_minority_exit_answer, empirical, 'Whether electoral-reciprocity logic holds for groups structurally outside every coalition.').

omega_variable(
    judiciary_identity_lock_depth,
    'How deep does the Supreme Court''s identity lock run — would it comply with advisory status if formally imposed, or resist at the cost of institutional rupture?',
    'Behavioral evidence from the 2023-2026 episode: judicial selection-committee battles, public statements by sitting justices, compliance with the first override enactments.',
    'Deep identity lock sustains enforcement costs (keeps suppression high, supporting the tangled_rope classification); shallow lock would let the arrangement stabilize cheaply and drift toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judiciary_identity_lock_depth, empirical, 'Depth of professional and institutional identity fusion binding the judiciary to its guardian role.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 1948, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(blib_psr_tr_t1948, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(blib_psr_tr_t1958, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 1958, 0.12).
narrative_ontology:measurement(blib_psr_tr_t1968, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 1968, 0.14).
narrative_ontology:measurement(blib_psr_tr_t1978, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 1978, 0.16).
narrative_ontology:measurement(blib_psr_tr_t1988, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 1988, 0.18).
narrative_ontology:measurement(blib_psr_tr_t1992, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 1992, 0.2).
narrative_ontology:measurement(blib_psr_tr_t1995, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 1995, 0.24).
narrative_ontology:measurement(blib_psr_tr_t2003, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 2003, 0.28).
narrative_ontology:measurement(blib_psr_tr_t2011, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 2011, 0.3).
narrative_ontology:measurement(blib_psr_tr_t2018, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 2018, 0.33).
narrative_ontology:measurement(blib_psr_tr_t2023, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 2023, 0.4).
narrative_ontology:measurement(blib_psr_tr_t2026, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 2026, 0.36).

% Extraction over time
narrative_ontology:measurement(blib_psr_be_t1948, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 1948, 0.18).
narrative_ontology:measurement(blib_psr_be_t1958, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 1958, 0.24).
narrative_ontology:measurement(blib_psr_be_t1968, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 1968, 0.27).
narrative_ontology:measurement(blib_psr_be_t1978, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 1978, 0.29).
narrative_ontology:measurement(blib_psr_be_t1988, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 1988, 0.31).
narrative_ontology:measurement(blib_psr_be_t1992, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 1992, 0.29).
narrative_ontology:measurement(blib_psr_be_t1995, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 1995, 0.24).
narrative_ontology:measurement(blib_psr_be_t2003, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 2003, 0.22).
narrative_ontology:measurement(blib_psr_be_t2011, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 2011, 0.24).
narrative_ontology:measurement(blib_psr_be_t2018, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 2018, 0.26).
narrative_ontology:measurement(blib_psr_be_t2023, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 2023, 0.33).
narrative_ontology:measurement(blib_psr_be_t2026, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 2026, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(blib_psr_su_t1948, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 1948, 0.15).
narrative_ontology:measurement(blib_psr_su_t1958, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 1958, 0.18).
narrative_ontology:measurement(blib_psr_su_t1968, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 1968, 0.2).
narrative_ontology:measurement(blib_psr_su_t1978, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 1978, 0.22).
narrative_ontology:measurement(blib_psr_su_t1988, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 1988, 0.28).
narrative_ontology:measurement(blib_psr_su_t1992, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 1992, 0.35).
narrative_ontology:measurement(blib_psr_su_t1995, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 1995, 0.45).
narrative_ontology:measurement(blib_psr_su_t2003, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 2003, 0.52).
narrative_ontology:measurement(blib_psr_su_t2011, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 2011, 0.58).
narrative_ontology:measurement(blib_psr_su_t2018, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 2018, 0.62).
narrative_ontology:measurement(blib_psr_su_t2023, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 2023, 0.8).
narrative_ontology:measurement(blib_psr_su_t2026, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 2026, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, balanced_contestation_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'who controls Basic Law interpretation' decomposes into three structurally distinct constraints sharing one kernel. This file (parliamentary sovereignty) authors low epsilon from its own seat with victims = opposition, permanent minorities, court-dependent rights-holders, and future majorities. judicial_supremacy_reading makes binding court invalidation operative — the victim set shifts to legislative majorities and epsilon rises for legislative seats. balanced_contestation_reading partitions authority — both prior victim sets shrink. Upstream/downstream: the judicial_supremacy reading currently shapes the operating environment this reading pushes against (modeled as revival_pressure in drift_state); all three files link mutually via affects_constraints so contamination and adoption events propagate across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
