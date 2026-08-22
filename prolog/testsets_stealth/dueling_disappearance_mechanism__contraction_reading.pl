% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dueling_disappearance_mechanism__contraction_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: dueling_disappearance_mechanism__contraction_reading
 *   human_readable: Dignity-Culture Displacement Constraint on Honor Violence (Contraction Reading)
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   This story instantiates the contraction reading of the kernel
 *   dueling_disappearance_mechanism: the claim that dueling became culturally
 *   unthinkable because dignity-culture axioms displaced honor-culture
 *   axioms, so that challenge-violence lost semantic coherence rather than
 *   being outcompeted or suppressed. The constraint under contest is the
 *   standing dignity-culture settlement that makes violent honor-vindication
 *   unthinkable — an arrangement that holds without enforcement, collects
 *   nothing for anyone, and yet has a real victim class: the honor
 *   practitioners whose framework became illegible. The colloquial question
 *   'why did dueling die' decomposes, per the epsilon-invariance principle,
 *   into three structurally distinct constraints — this axiomatic-substrate
 *   story, the sibling institutional-substitution story (courts, banking,
 *   libel law outcompeting the duel), and the sibling overdetermination story
 *   — each with its own epsilon, victim structure, and classification; this
 *   file authors only the first and links the others via
 *   network.affects_constraints. The claimed type and the authored metrics
 *   are independent authored facts: the claim asserts substrate status; the
 *   metrics describe what the arrangement actually did to the people it
 *   governed, including extraction that accumulated on a shrinking honor
 *   class across the interval.
 *
 * KEY AGENTS:
 *   - honor_culture_practitioners: primary target (organized/identity_locked) — the gentry-officer-politician class whose challenge-and-vindication framework became first scandalous, then criminal, then unsayable; bears the settlement's entire extraction
 *   - military_officer_corps: secondary target (organized/constrained) — the duel's last institutional stronghold; same honor framework as the gentry but with emerging official channels as a partial, honor-costly exit
 *   - dignity_culture_majority: protected class (moderate/constrained) — the population that internalized intrinsic-worth axioms; bears no cost, collects no transfer, and cannot state the alternative
 *   - anti_dueling_reformers: agenda authors (organized/mobile) — wrote the dignity critique and the dead-letter statutes; exited to other reforms once the axioms held
 *   - state_legal_apparatus: nominal enforcer (institutional/mobile) — a century of anti-dueling statutes that juries would not enforce; its failure is this reading's central evidence
 *   - historical_sociologists: analytical observer (analytical/analytical) — adjudicates the mechanism debate among the three readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__contraction_reading, 0.53).
domain_priors:suppression_score(dueling_disappearance_mechanism__contraction_reading, 0.15).
domain_priors:theater_ratio(dueling_disappearance_mechanism__contraction_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, extractiveness, 0.53).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__contraction_reading, mountain).
narrative_ontology:human_readable(dueling_disappearance_mechanism__contraction_reading, "Dignity-Culture Displacement Constraint on Honor Violence (Contraction Reading)").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__contraction_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:emerges_naturally(dueling_disappearance_mechanism__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__contraction_reading, '66a692e0-2999-40ea-bb84-e3a5017f3987').
narrative_ontology:cs_kernel_codification('66a692e0-2999-40ea-bb84-e3a5017f3987', distributed).
narrative_ontology:cs_authority_grounding('66a692e0-2999-40ea-bb84-e3a5017f3987', expertise).
narrative_ontology:cs_interpretation_layer_present('66a692e0-2999-40ea-bb84-e3a5017f3987').
narrative_ontology:cs_reading_relation('66a692e0-2999-40ea-bb84-e3a5017f3987', dueling_disappearance_mechanism__institutional_displacement_reading, coexists_with).
narrative_ontology:cs_reading_relation('66a692e0-2999-40ea-bb84-e3a5017f3987', dueling_disappearance_mechanism__overdetermined_composite_reading, coexists_with).
narrative_ontology:cs_axiom('66a692e0-2999-40ea-bb84-e3a5017f3987', foundational, axiomatic_displacement_sufficiency).
narrative_ontology:cs_axiom_status(axiomatic_displacement_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('66a692e0-2999-40ea-bb84-e3a5017f3987', axiomatic_displacement_sufficiency, empirically_contingent).
narrative_ontology:cs_axiom('66a692e0-2999-40ea-bb84-e3a5017f3987', foundational, honor_framework_illegibility).
narrative_ontology:cs_axiom_status(honor_framework_illegibility, holdable).
narrative_ontology:cs_axiom_grounding('66a692e0-2999-40ea-bb84-e3a5017f3987', honor_framework_illegibility, empirically_contingent).
narrative_ontology:cs_reference_frame('66a692e0-2999-40ea-bb84-e3a5017f3987', honor_axiomatic_order).
narrative_ontology:cs_drift_state('66a692e0-2999-40ea-bb84-e3a5017f3987', contemporary, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('66a692e0-2999-40ea-bb84-e3a5017f3987', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__contraction_reading, honor_culture_practitioners).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__contraction_reading, military_officer_corps).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__contraction_reading, dignity_culture_majority).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__contraction_reading, state_legal_apparatus).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gentry, officers, politicians, and professionals of the eighteenth- and nineteenth-century Atlantic world whose social standing ran on challenge and vindication: worth was publicly contingent, a slight left unanswered meant social death, and the duel was the settled technology for settling the question. As the dignity settlement spread, their framework passed from respectable to scandalous to criminal to unsayable; memoirs of the last generation of duelists describe the code as a compulsion they could no longer publicly defend. Leaving the framework meant renouncing the identity that constituted them — by the code's own terms, the gentleman who declined the code was no gentleman.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, honor_culture_practitioners, payer,
    organized, biographical, identity_locked, continental).

% The duel's last institutional stronghold. Officer honor codes on both sides of the Atlantic made satisfaction a condition of command; refusal meant resignation or ruin within the corps. Official substitutes — courts of inquiry, courts-martial, departmental regulations (the United States Navy outlawed dueling in 1862 after a string of deaths) — gave officers a channel civilian gentlemen lacked, but using it carried honor-cost inside the corps' own code for another generation. The corps' self-concept was fused with the code even as its institutions built the exit.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, military_officer_corps, payer,
    organized, biographical, constrained, continental).

% The mass population that came to hold worth as intrinsic and inviolable rather than reputation-contingent. They never face a challenge economy: no encounter must be read for status threat, no grievance must be answered in blood, and the protection is invisible because it is constitutive. The honor alternative is not forbidden to them so much as unstatable — they do not hold the premises in which a challenge-claim could be framed. Adopting the honor framework would require acquiring a self-understanding their upbringing never supplied.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, dignity_culture_majority, beneficiary,
    moderate, generational, constrained, continental).

% Evangelical societies, Enlightenment publicists, peace advocates, and state legislators who authored the dignity critique and a wave of anti-dueling statutes from the 1780s onward. Their durable work was the axiomatic argument rather than the statutes: the laws sat as dead letters for a century while the argument reorganized what a person could claim to be. Once the axioms held, the movement had nothing left to administer and dissolved into other reforms — abolition, temperance, international arbitration.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, anti_dueling_reformers, agenda_setter,
    organized, generational, mobile, continental).

% Legislatures and courts that prohibited dueling across the Atlantic world for a century before dueling faded. Prosecutions were rare and convictions rarer; juries of gentlemen declined to convict, and officers evaded the bans by crossing state lines or resigning commissions. The apparatus spent real enforcement capacity against a practice its own personnel often held, and the statutes it passed functioned as declarations rather than rules. Its century of failure before the practice died is the evidence this account leans on.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, state_legal_apparatus, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(dueling_disappearance_mechanism__contraction_reading, state_legal_apparatus, payer).

% The analytical seat over the mechanism debate. Working from duel statistics, statute books, military regulations, memoirs, and the comparative ethnography of honor societies, they adjudicate among axiomatic displacement, institutional substitution, and overdetermination — and their verdicts feed back into how the disappearance is taught, memorialized, and generalized to other honor institutions.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dueling_disappearance_mechanism__contraction_reading, diffuse).
narrative_ontology:fixing_cost_class(dueling_disappearance_mechanism__contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a shared settlement of the terms of personal worth: once intrinsic-worth axioms are common knowledge, no one need maintain challenge-readiness or read every encounter for status threat, and disputes route by default to non-violent channels.
% TRANSFER_FUNCTION: Moves nothing to anyone: the honor framework's social technology — challenge, vindication, satisfaction — is decommissioned, and the standing and identity capital embedded in it is destroyed rather than transferred. The closest thing to a transfer is the honor class's loss of disputing precedence to courts and markets, which under this reading is a consequence of the axiomatic shift, not its mechanism.
% ABSENT_VOICES: The honor practitioners objected and were present — but their objection progressively lost statability: 'my worth requires violent vindication' became, in dignity terms, not a claim but a symptom, so the last defenders argued as anachronisms in a language that could no longer parse their position. Also absent: those the honor economy fixed beneath its challenges — women, the poor, the enslaved in the American case — whose fixed standing was the background the duel presupposed and who had no seat in either framework's self-description; and the dead and maimed of the transition generations, whose testimony survives only as bereaved correspondence.
% DISAPPEARANCE_RATIONALE: If the dignity-axiom settlement vanished overnight, the modern arrangement of personal worth, dispute routing, and the unsayability of challenge-violence would lose its substrate: honor logic would become thinkable again, and agents with reputational stakes and unresolved grievances — the position the officer corps and gentry occupied — would have standing incentives to rebuild challenge practice. Every institution that now assumes grievances route to courts, credit, and conversation rather than to seconds and pistols would need to reconstitute its assumptions. Nothing about the settlement is self-executing in its absence; it is load-bearing.
% FOUNDING_PROBLEM: How a commercial society of strangers can settle the terms of personal worth without perpetual armed vigilance: honor's challenge economy required every gentleman to read every encounter for status threat and to answer slights with a technology that killed over trivia, which became untenable as credit, commerce, travel, and religious-humanitarian accounts of the person spread.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside any beneficiary set (this reading declares none): the honor practitioners' own testimony — duelist memoirs and last-generation apologies describing the code as a compulsion and cataloguing its burdens — attests the founding problem from inside the losing class; comparative ethnography of surviving honor societies attests the vigilance burden independently of the Atlantic record; and the contemporaneous religious-humanitarian and commercial critiques (peace-society tracts, merchant testimony against the duel's interference with credit and travel) attest it from the seats that authored the displacement. No source inside a beneficiary set is available to self-serve, because none exists.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dueling_disappearance_mechanism__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dueling_disappearance_mechanism__contraction_reading, 0.53, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dueling_disappearance_mechanism__contraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dueling_disappearance_mechanism__contraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(dueling_disappearance_mechanism__contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(dueling_disappearance_mechanism__contraction_reading),
    narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(dueling_disappearance_mechanism__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.53 at interval end) rises through the transition and plateaus: the settlement strips the honor class of legibility, standing, and its dispute technology; extraction intensifies on each remaining holder as the class shrinks, until the honor capital available to extract is exhausted. Suppression (0.15) is authored as a raw structural property, unscaled by power or scope: the settlement's own coercive surface is small — its force is semantic closure, not enforcement — and the era's coercive anti-dueling statutes belong to the legal apparatus's failed project, not to this arrangement's operation. Theater (0.20) stays low throughout: a substrate performs almost nothing, and what theatricality the record shows (dead-letter statutes, commemorative rhetoric) is the legal apparatus's activity, not this constraint's. Accessibility collapse (0.90) is the reading's signature claim: once dignity axioms are internalized the alternative does not become costly, it becomes incoherent — a duel cannot vindicate a worth that is no longer held to be challenge-contingent. Resistance (0.18 at end) was real mid-transition — Southern honor apologists, officer-corps defiance of bans — and faded not because its bearers were outcoordinated (they were an organized class) but because they lost the language in which a defense could be stated. The measurement series run on one shared seven-point grid (1770-1920); suppression_requirement is deliberately not tracked as a series because this arrangement's enforcement picture is static by hypothesis — enforcement-capacity dynamics are the sibling institutional reading's subject. Coordination type identity_coordination is declared because the settlement's coordination function is the culture's identity settlement itself (what a person is, what counts as standing); the known cover-story risk of that type is tracked by the dignity_exclusion_shadow omega rather than by inflating the floor.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from one structure. For the honor practitioners the settlement is a destroying arrangement: it took their framework, standing, and dispute technology, and identity-lock meant exit was self-erasure — from that seat the arrangement computes as uncompensated extraction. For the dignity majority it is indistinguishable from common sense: no cost, invisible protection, no statable alternative. For the reformers it is a finished project they no longer touch. For the legal apparatus it was a century-long failed mandate. The claimed type is authored from the structural whole — a substrate with a victim class and no collector class — and the engine's per-seat computation is expected to diverge across these seats; that divergence, not the claim, is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Victims: honor_culture_practitioners and military_officer_corps are declared victims; both hold identity-fused or honor-costly exit, which places them near the full-target pole, most extremely for the identity-locked gentry class — effective extraction is amplified for them by the derivation. Beneficiaries: none are declared, deliberately — under this reading no one collects; the settlement destroyed the honor framework rather than transferring it, and the majority's protection is a benefit-from, not a receipt. The dignity_culture_majority stakeholder therefore carries an explicit directionality override (moderate to 0.12): left to the power-atom fallback it would sit near symmetric, but its structural position is near the beneficiary pole — it pays nothing and the settlement is constitutive of its self-understanding. The override is needed because the beneficiaries array is empty by design, not by omission. Reformers and the legal apparatus sit near symmetric: they authored and spent on the arrangement without collecting from it. The historians' seat is analytical and outside the derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   Two mislabels are guarded against. Reading the disappearance as pure coordination among honor peers would erase the victims: the honor settlement coordinated violence among equals by fixing it on subordinates, and its displacement had losers. Reading it as pure civilizational progress with no losers would erase the extraction: the per-seat computation keeps the honor class's loss visible even while the substrate claim stands. The founding problem — ordering personal worth among strangers without mutual armed vigilance — remains live and the settlement still addresses it, so there is no zombie-mandate signature here; the mandatrophy residue in this history (dead-letter anti-dueling statutes) belongs to the legal apparatus's project and to the sibling institutional reading's file, not to this constraint. One receipt-surface caution is recorded: the cell this story occupies (diffuse gains, prohibitive fixing) is shared with the piton signature, but the mechanism differs — the settlement is not an atrophied function maintained by performance but a live substrate that performs almost nothing, and the low theater ratio together with the live founding problem is the discriminating evidence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the mechanism that made dueling culturally unthinkable the dignity-axiom displacement this contraction reading posits, or the institutional substitution of the sibling reading, or an overdetermined multiplicity — and does this story''s referent (an unenforced axiomatic substrate) survive the adjudication?',
    'Historiographical adjudication with counterfactual leverage: cases where institutions modernized without axiomatic shift (dueling persisted — e.g., parts of the American South after comparable legal modernization) versus axiomatic shift without effective institutional substitution (anti-dueling norms holding where statutes were dead letters); plus the relative timing of statutes, institutional substitutes, and practice decline.',
    'If the institutional reading carries, this file''s referent is misdescribed — the standing arrangement is an enforced substitution with identifiable beneficiaries (courts, credit, press institutions), and classification moves from mountain toward rope or tangled_rope. If the composite reading carries, no single constraint holds the classification and this file''s epsilon must be re-authored as one component among several. If this reading carries, the mountain claim stands with the victim class as its extraction record.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Which of the three readings of the dueling-disappearance kernel names the operative constraint.').

omega_variable(
    cultural_substrate_reversibility,
    'Can a cultural axiomatic substrate be a mountain in the strong sense, or is every cultural constraint revisable — could dignity axioms be un-thought and honor logic become thinkable again?',
    'Monitor revival pressure: subcultural honor codes, challenge-revival movements, and any population re-acquiring challenge-contingent worth; examine whether historical revival attempts (post-bellum Southern code nostalgia, ritualized Mensur persistence) reconstituted the full framework or borrowed only its costume.',
    'If the substrate is reversible under sufficient pressure, the constraint is a maximally stable coordination settlement rather than a mountain, and classification drifts toward rope; if irreversible, the mountain claim holds and the victim class''s loss is permanent rather than deferred.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_substrate_reversibility, empirical, 'Whether the dignity substrate is irreversible (mountain-eligible) or a maximally stable cultural settlement.').

omega_variable(
    honor_class_extraction_weighting,
    'How should the settlement''s extraction be weighted when it is total for a shrinking honor minority and near-zero for everyone else — is destroying a framework extraction when almost no one remaining holds it?',
    'Explicit weighting analysis over the historical population: extraction intensity per honor-class member against class share at each time point; sensitivity of the classification to population-weighted versus per-seat epsilon.',
    'A population-weighted reading lowers aggregate epsilon and strengthens the mountain computation; a per-seat reading keeps epsilon high for the honor class and makes seat divergence the primary output. The choice moves the corpus-level verdict on whether progress mechanisms with losers are mountains or extraction-bearing hybrids.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honor_class_extraction_weighting, conceptual, 'Weighting convention for extraction concentrated on a vanishing victim class.').

omega_variable(
    illegibility_suppression_status,
    'Is the honor framework''s progressive unsayability a suppression mechanism belonging to this constraint, or the victim class''s own framework failure — does semantic closure coerce?',
    'Test for counterfactual articulability: could a mid-transition honor defender state the code''s claims in terms the dignity framework could process and contest (as early apologists could), or had the terms of art themselves become unavailable (as late apologists report)? Distinguish stigma (sayable, contested) from illegibility (unstatable).',
    'If illegibility is constraint-side suppression, effective suppression is materially higher than the 0.15 scalar and the victim seats compute closer to a snare experience; if it is framework failure, the settlement''s suppression stays negligible and the mountain computation stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(illegibility_suppression_status, conceptual, 'Whether semantic closure counts as suppression or as the victim framework''s own collapse.').

omega_variable(
    ritualized_residual_dueling,
    'Do the ritualized residues — German student Mensur, dueling codes in a few European militaries into the early twentieth century, consensual honor rituals — falsify the unthinkability claim or mark it as class- and region-scoped?',
    'Scope the claim: map where and among whom challenge-violence remained articulable after 1890, and whether those enclaves held the full honor axioms or retained only the practice''s costume under dignity rules (consent, medical supervision, non-lethality).',
    'If full-framework enclaves persisted, accessibility collapse is regional rather than civilizational and the mountain claim narrows; if the residues kept only the costume (as the Mensur''s transformation suggests), the unthinkability claim holds and the residues are drift evidence rather than counterexamples.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ritualized_residual_dueling, empirical, 'Whether ritualized dueling residues refute the substrate claim or confirm its scoping.').

omega_variable(
    dignity_exclusion_shadow,
    'Do the dignity settlement''s own exclusions — who counted as possessing intrinsic worth (the enslaved, women, the colonized were long outside its protection) — belong to this constraint''s epsilon referent, or are they a separate constraint?',
    'Epsilon-invariance decomposition test: if measuring the settlement with and without its exclusion record yields materially different epsilon and different victim sets, author a separate constraint story for the exclusion structure and link it here via the network.',
    'If the exclusions are inside the referent, this file''s epsilon rises, the victim set expands beyond honor practitioners, and the mountain claim is materially weakened; if they are a separate constraint, this file''s epsilon and victim set stand as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dignity_exclusion_shadow, conceptual, 'Whether the dignity settlement''s historical exclusions are part of this constraint or a sibling constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__contraction_reading, 1770, 1920).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dueling_contraction_tr_t1770, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1770, 0.05).
narrative_ontology:measurement(dueling_contraction_tr_t1810, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1810, 0.08).
narrative_ontology:measurement(dueling_contraction_tr_t1830, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1830, 0.1).
narrative_ontology:measurement(dueling_contraction_tr_t1850, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1850, 0.12).
narrative_ontology:measurement(dueling_contraction_tr_t1870, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1870, 0.15).
narrative_ontology:measurement(dueling_contraction_tr_t1890, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1890, 0.18).
narrative_ontology:measurement(dueling_contraction_tr_t1920, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1920, 0.2).

% Extraction over time
narrative_ontology:measurement(dueling_contraction_be_t1770, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1770, 0.06).
narrative_ontology:measurement(dueling_contraction_be_t1810, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1810, 0.15).
narrative_ontology:measurement(dueling_contraction_be_t1830, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1830, 0.26).
narrative_ontology:measurement(dueling_contraction_be_t1850, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1850, 0.36).
narrative_ontology:measurement(dueling_contraction_be_t1870, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1870, 0.44).
narrative_ontology:measurement(dueling_contraction_be_t1890, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1890, 0.5).
narrative_ontology:measurement(dueling_contraction_be_t1920, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1920, 0.53).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(dueling_disappearance_mechanism__contraction_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dueling_disappearance_mechanism__contraction_reading, identity_coordination).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism__institutional_displacement_reading).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism__overdetermined_composite_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'why dueling died' covers three structurally distinct claims with different epsilon values and different failure modes: (1) this file — an axiomatic substrate that makes challenge-violence incoherent (claimed mountain; victims: the illegible honor class; no collector class); (2) dueling_disappearance_mechanism__institutional_displacement_reading — institutional substitutes outcompeting the duel (an arrangement with identifiable beneficiaries: courts, credit institutions, libel-law and banking professions); (3) dueling_disappearance_mechanism__overdetermined_composite_reading — multiple independently sufficient causes, under which no single constraint carries the classification. The upstream story in the literature is the institutional reading (older, better established); this contraction reading is downstream (the culturalist turn) and cites the statutes' century of failure as its wedge against it. All three files are linked via network.affects_constraints; each holds one stable epsilon over its own referent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dueling_disappearance_mechanism__contraction_reading, moderate, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
