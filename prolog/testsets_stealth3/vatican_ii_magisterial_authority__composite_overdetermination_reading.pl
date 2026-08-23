% ============================================================================
% CONSTRAINT STORY: vatican_ii_magisterial_authority__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_magisterial_authority__composite_overdetermination_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: vatican_ii_magisterial_authority__composite_overdetermination_reading
 *   human_readable: Conciliar Composite Corpus and Interpretive Adjudication Regime (Overdetermination Reading)
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   The standing arrangement under contest is the post-conciliar magisterial
 *   authority structure as it operates through the conciliar corpus: a set of
 *   documents whose disputed passages were worded to admit two incompatible
 *   systematic ecclesiologies at once, so that a supermajority including both
 *   factions could affix signatures, plus the adjudicating office that
 *   decides which reading becomes operative law. On this reading the dual
 *   readability is not an exegetical accident but the designed product of
 *   supermajority-seeking compromise; the ten-to-twelve percent of non placet
 *   votes marked incompatibilities that the final texts embedded rather than
 *   resolved; implementation divergence across pontificates is therefore a
 *   structural property of the arrangement, not administrative noise; and the
 *   operative locus of authority is hermeneutical control — whoever
 *   adjudicates the unsettled corpus holds the real teaching power. KEY
 *   AGENTS (by structural relationship): papal_magisterium: adjudicating
 *   office and net collector (institutional/constrained);
 *   conciliar_progressive_periti: founding beneficiary
 *   (powerful/constrained); pre_conciliar_traditionalists: primary
 *   disciplined flank (organized/identity_locked);
 *   progressive_liberation_theologians: secondarily disciplined flank
 *   (moderate/constrained); conciliar_minority_fathers: excluded founders
 *   whose objections were archived (organized/identity_locked);
 *   catholic_laity: diffuse bearers with no ratifying seat
 *   (powerless/trapped); council_historians: analytical observers holding the
 *   drafting record (analytical/analytical). Epsilon is authored for this
 *   standing arrangement, assessed by this reading's own lights — not for any
 *   alternative arrangement, and not averaged across other ways of reading
 *   the corpus. The claim/metric gap is deliberate: the arrangement is
 *   CLAIMED as tangled_rope (genuine coordination function plus asymmetric
 *   extraction, actively enforced) while the metrics are authored as
 *   independent descriptive facts; the engine computes per-seat
 *   classifications and measures any divergence.
 *
 * KEY AGENTS:
 *   - papal_magisterium: adjudicating office and net collector (institutional/constrained) — decides which reading of the corpus is operative
 *   - conciliar_progressive_periti: founding beneficiary (powerful/constrained) — drafted the compromise wordings that secured the votes
 *   - pre_conciliar_traditionalists: primary disciplined flank (organized/identity_locked) — bears the alternating penalty-and-license cycle
 *   - progressive_liberation_theologians: secondarily disciplined flank (moderate/constrained) — censured when their reading outruns the office
 *   - conciliar_minority_fathers: excluded founders (organized/identity_locked) — non placet votes archived unanswered
 *   - catholic_laity: diffuse bearers (powerless/trapped) — implementation whiplash with no ratifying voice
 *   - council_historians: analytical observer (analytical/analytical) — holds the drafting record both wings cite selectively
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.62).
domain_priors:suppression_score(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.66).
domain_priors:theater_ratio(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_magisterial_authority__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_magisterial_authority__composite_overdetermination_reading, "Conciliar Composite Corpus and Interpretive Adjudication Regime (Overdetermination Reading)").
narrative_ontology:topic_domain(vatican_ii_magisterial_authority__composite_overdetermination_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_magisterial_authority__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_magisterial_authority__composite_overdetermination_reading, '22d20c16-f554-467c-b752-6970f865982d').
narrative_ontology:cs_kernel_codification('22d20c16-f554-467c-b752-6970f865982d', fixed_text).
narrative_ontology:cs_authority_grounding('22d20c16-f554-467c-b752-6970f865982d', extraction).
narrative_ontology:cs_interpretation_layer_present('22d20c16-f554-467c-b752-6970f865982d').
narrative_ontology:cs_reading_relation('22d20c16-f554-467c-b752-6970f865982d', vatican_ii_magisterial_authority__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('22d20c16-f554-467c-b752-6970f865982d', vatican_ii_magisterial_authority__rupture_reading, coexists_with).
narrative_ontology:cs_axiom('22d20c16-f554-467c-b752-6970f865982d', foundational, interpretive_office_constitutes_doctrine).
narrative_ontology:cs_axiom_status(interpretive_office_constitutes_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('22d20c16-f554-467c-b752-6970f865982d', interpretive_office_constitutes_doctrine, conventional).
narrative_ontology:cs_axiom('22d20c16-f554-467c-b752-6970f865982d', foundational, composite_unity_precedes_resolution).
narrative_ontology:cs_axiom_status(composite_unity_precedes_resolution, holdable).
narrative_ontology:cs_axiom_grounding('22d20c16-f554-467c-b752-6970f865982d', composite_unity_precedes_resolution, instrumental).
narrative_ontology:cs_reference_frame('22d20c16-f554-467c-b752-6970f865982d', compromise_engineered_vote_corpus).
narrative_ontology:cs_drift_state('22d20c16-f554-467c-b752-6970f865982d', contemporary, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('22d20c16-f554-467c-b752-6970f865982d', '').
narrative_ontology:cs_kernel_id(vatican_ii_magisterial_authority__composite_overdetermination_reading, vatican_ii_magisterial_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__composite_overdetermination_reading, conciliar_progressive_periti).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__composite_overdetermination_reading, papal_magisterium).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, pre_conciliar_traditionalists).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, progressive_liberation_theologians).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, catholic_laity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__composite_overdetermination_reading, catholic_laity).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__composite_overdetermination_reading, hermeneutical_control_as_authority_locus).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__composite_overdetermination_reading, textual_ambiguity_as_vote_instrument).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the office that adjudicates what the conciliar documents mean. Issues interpretations, disciplines teachers whose readings stray from the favored line, and regulates which liturgical and pastoral implementations are lawful. Collects the deference and obedience that flow to the office able to say what the texts settle. Cannot resign its way out of the interpretive question; each successor inherits the unresolved corpus intact.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, papal_magisterium, agenda_setter,
    institutional, generational, constrained, global).

% The theologians who drafted the compromise wordings that secured supermajority votes on the disputed schemas. Saw their renewal program enacted under cover of traditional formulas; many rose to bishoprics and curial posts afterward. Their personal stakes faded with their generation, but the drafting strategy they pioneered set the template their students still follow.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, conciliar_progressive_periti, beneficiary,
    powerful, biographical, constrained, global).

% Communities and clergy organized around the pre-conciliar liturgy and neo-scholastic theology. Their formation was displaced by implementations authorized under the same texts they had opposed on the floor; successive pontificates alternately disciplined them (suspensions, the 1988 penalties on the Lefebvre line, restriction of the old rite) and licensed them (indults, the 2007 widening of permission). Leaving the communion would forfeit the sacramental life their identity is built on; staying means accepting whichever regime currently governs the old rite.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, pre_conciliar_traditionalists, payer,
    organized, generational, identity_locked, continental).

% Theologians who read the conciliar opening to the modern world as license for social critique and structural reform. When their readings outran the interpretive office's preferred line they faced censures, silenced publications, and removal from chairs — figures such as Boff, Curran, and Häring among the prominent cases. Their alternative, secular academia, costs them the ecclesial audience their work addresses.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, progressive_liberation_theologians, payer,
    moderate, biographical, constrained, regional).

% The council bishops who voted non placet on the disputed schemas — roughly a tenth of the floor. Their written reservations were archived in the acts rather than answered; the final texts kept the wordings they objected to. After the council their position had no institutional address: the synod and curia absorbed their opponents' reading, and the channel for their objection narrowed into the traditionalist movement.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, conciliar_minority_fathers, excluded,
    organized, generational, identity_locked, global).

% Attended no council session and ratified nothing, yet received the reordered liturgy, revised catechesis, and restructured parish life that the implementations carried. Gained vernacular worship and broader lay roles; bore the whiplash of successive reversals in language, music, and devotional practice as regimes changed. Exit means leaving a community most cannot leave without losing family and sacramental ties.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, catholic_laity, payer,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__composite_overdetermination_reading, catholic_laity, beneficiary).

% Scholars of the council's drafting history — the Bologna school, O'Malley, and their critics — who reconstruct how the compromise formulations were assembled and what the minority objections contained. Hold the diaries, relaciones, and modi records that the benefiting parties cite selectively. Analytical seat: neither collects from the arrangement nor answers to its discipline.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, council_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_magisterial_authority__composite_overdetermination_reading, papal_magisterium).
narrative_ontology:fixing_cost_class(vatican_ii_magisterial_authority__composite_overdetermination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Produced, and now maintains, a single authoritative doctrinal corpus that two irreconcilable ecclesiological factions could simultaneously affirm — solving the council's vote problem and continuing to hold incompatible constituencies inside one communion.
% TRANSFER_FUNCTION: Moves interpretive authority and doctrinal legitimacy to the office that adjudicates the corpus (papacy, curia, doctrinal congregation); moves implementation costs — liturgical reversal, disciplinary action, career consequence — onto whichever constituency's reading is currently disfavored; moves liturgical and pastoral change onto clergy and laity irrespective of their own position.
% ABSENT_VOICES: The minority fathers' objections sit archived in the council acts; lay Catholics deliberated and ratified nothing; the Orthodox and Protestant interlocutors the texts address had no seat in drafting or adjudication; theologians censured after the fact had no standing forum when the wordings were fixed.
% DISAPPEARANCE_RATIONALE: Without the corpus and its interpretive regime the Church loses its operative doctrinal constitution overnight: no shared baseline for ordination, liturgy, ecumenism, or religious liberty; the factions currently held inside one communion by the composite texts would institutionalize as separate bodies; every office whose authority flows from adjudicating the texts forfeits its instrument. Rearrangement, not stasis.
% FOUNDING_PROBLEM: How to produce authoritative conciliar documents at all when the episcopal floor held two incompatible ecclesiologies — neo-scholastic integralism and ressourcement renewal — either of which, stated plainly, would have driven the other from the chamber.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the council's procedural record (relaciones explaining deliberate wording strategy, the modi and footnote negotiations, diaries of fathers and periti on both wings) attests the vote-coordination origin; the minority fathers' published post-conciliar commentaries attest that their objections were noted, not resolved; historians of the council (Alberigo's Bologna school; O'Malley) independently reconstruct the compromise mechanics. No participant attests that the dual readability of the final texts was accidental.
narrative_ontology:disappearance_verdict(vatican_ii_magisterial_authority__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_magisterial_authority__composite_overdetermination_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_magisterial_authority__composite_overdetermination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_magisterial_authority__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_magisterial_authority__composite_overdetermination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_magisterial_authority__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_magisterial_authority__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.62) is substantial but bounded: the composite corpus genuinely delivered the coordination it was built for — supermajority promulgation and continued communion — while transferring interpretive authority to the adjudicating office and imposing unresolved costs on whichever flank is currently disfavored. Suppression (0.66) reflects the present enforcement peak: canonical penalties, congregational censures, and rite restrictions alternate with licensing windows rather than disappearing. Theater (0.38) rises steadily as consensus maintenance — anniversaries, unity rhetoric, invocations of the council's spirit detached from its letters — consumes a growing share of activity relative to actual adjudication. Accessibility collapse is low (0.42): grasping the overdetermination does not close alternatives; it reveals that the text cannot arbitrate, which keeps interpretive alternatives alive. Resistance is high (0.70): the hermeneutical contest is permanent and organized on both flanks. Enforcement follows a pontificate cycle — hardening (the doctrinal congregation's era, the 1988 penalties, the 2021 rite restrictions) alternating with relaxation (indults, the 2007 widening) — and the oscillation is itself part of the mechanism: each relaxation builds expectations in the disfavored flank, each crackdown harvests the return to the interpreter, an intermittent-reinforcement schedule that keeps both constituencies bidding for hermeneutical favor instead of exiting. All three metric series share one time grid (t = 0,10,20,30,40,50,60 years from the council's opening) so no metric's end-state value is silently substituted into earlier rows. Suppression mixes structural machinery (penalties, bans, restrictions) with internalized clerical self-censorship; the omega variable carries the proportion question rather than the scalar pretending to resolve it.
 *
 * PERSPECTIVAL GAP:
 *   From the adjudicating seat the arrangement is the teaching office legitimately exercising its charism over documents that require interpretation; from the traditionalist seat it is displacement enforced by texts that were presented as preserving what they dismantled; from the censured-theologian seat it is an opening granted and then selectively revoked; from the pew it is liturgical whiplash without a ratifying voice. These are not perceptual errors — the engine computes divergent per-seat classifications from the same structural data because the seats hold different directionalities toward one arrangement. Identity lock operates on two seats: the traditionalist communities are relationally and ideologically fused with the pre-conciliar form (exit equals schism equals self-loss, which is why the 1988 break cost so few exits relative to the grievance), and the adjudicating office is institutionally fused with its adjudication (the office has become the function of settling what the texts cannot). If the traditionalist identity frame broke — if the old rite were conceded as permanently lawful rather than contingently tolerated — the flank's exit option would shift from identity_locked toward constrained and its extracted share would fall; if the office's frame broke — if it conceded that the corpus cannot arbitrate — its authority claim would have to relocate from text to office outright, which it resists acknowledging.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation: the adjudicating office and the drafting periti sit near the beneficiary pole; the traditionalist communities, the censured theologians, and the laity sit toward the target pole. Four overrides correct places where the derivation alone would misfire. The office (institutional) is corrected to 0.18: role alone would miss that it is the net collector of the deference the arrangement generates, though it also bears real management costs. The organized atom is corrected to 0.78 because it covers two seats with no beneficiary/victim declaration — the excluded minority fathers would otherwise fall back to the power-atom default — and both organized seats sit near the full-target end. The moderate atom is corrected to 0.68 for the censured theologians, whose exit to secular academia is real but costly. The powerless atom is corrected to 0.5: the laity appear in the victim declaration for the implementation costs they bear, but they also received the renewal goods those implementations carried — the documented indirect-beneficiary case for overriding downward — so their net position is near-symmetric. The periti (powerful) need no override: their beneficiary declaration already derives a low d.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — producing authoritative documents from an irreconcilable floor — died with the council's final session; the arrangement persists governing interpretation of what it produced. Authored honestly, founding_problem_status=dead combined with disappearance_verdict=world_rearranges is the mismatch signature the battery's consumer cross-checks against the theater trajectory, and it is the correct reading here: the corpus now functions primarily as the instrument through which interpretive authority is exercised, not as the answer to the problem that built it. Classifying the arrangement as tangled_rope rather than rope prevents crediting the unity dividend without pricing the flank costs; classifying it as snare would erase the genuine vote-coordination and communion-holding function that made the ambiguity rational for its builders. The rising theater ratio tracks the drift from adjudication toward consensus performance — a symptom worth watching, though the cost-asymmetry that defines inertial neglect does not yet hold: the office could resolve the ambiguity and declines because resolution would cost it half its communion, which is extraction-preserving maintenance rather than neglect. The receipt surface records this: gains accrue to a named seat (the adjudicating office), and fixing is prohibitive for exactly that seat — the combination that keeps the arrangement actively maintained rather than drifting into performance alone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story instantiates the composite_overdetermination_reading of kernel vatican_ii_magisterial_authority; the sibling readings (continuity_reading, rupture_reading) are separate constraints with their own epsilon, beneficiary structures, and classifications. Does this reading''s structural diagnosis survive contact with the siblings'' compiled data?',
    'Cross-read the three family stories'' epsilon values and victim structures; convergence or divergence across the family maps the kernel''s contest space and tests whether the composite diagnosis subsumes or merely parallels the siblings.',
    'If the continuity reading''s epsilon is negligible, this story''s extraction attribution narrows to the interpretive regime rather than the texts themselves; if the rupture reading''s epsilon and victim structure match this story''s, the composite and rupture readings may collapse into one constraint and the family should be reduced to two members.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame routing: which reading of the conciliar corpus governs classification, and how the siblings relate.').

omega_variable(
    ambiguity_design_vs_emergence,
    'Were the dual-vision wordings deliberately engineered to secure supermajority votes, or did they accumulate through ordinary committee compromise with no single actor intending the overdetermination?',
    'Drafting-history archaeology: the relatio explanations, modi edits, footnote negotiations, and father diaries, read for the difference between strategic wording choices and accretion.',
    'Deliberate design supports attributing the ambiguity to the drafting coalition as a governance instrument and weights epsilon toward the founding beneficiaries; emergence shifts the extraction attribution forward to the interpretive regime that later exploited the inherited irresolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ambiguity_design_vs_emergence, empirical, 'Whether the overdetermination was engineered or accumulated.').

omega_variable(
    hermeneutical_rent_magnitude,
    'How much of the arrangement''s extraction is the adjudicating office''s rent — authority flowing from settling an unsettled corpus — versus costs intrinsic to any doctrinal transition of this magnitude?',
    'Counterfactual comparison with councils that issued unambiguous definitions (Trent, Vatican I): measure downstream disciplinary volume and faction persistence relative to the magnitude of doctrinal change.',
    'If comparable councils show similar conflict at far lower textual ambiguity, the excess is attributable to the composite structure; if not, part of the measured epsilon is the price of any transition and the extraction attribution shrinks.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hermeneutical_rent_magnitude, empirical, 'Separating interpretive rent from generic transition cost.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression carried by enforcement machinery (canonical penalties, publication bans, rite restrictions) or by internalized clerical self-censorship that persists where enforcement relaxes?',
    'Post-relaxation trajectory: after the 2007 widening of permission, did disfavored readings resume freely (structural dominance) or stay muted (internalized dominance)? Compare seminary curricula, publication patterns, and petition volumes across relaxation windows.',
    'An internalized share raises effective suppression above the structural measure and predicts persistence across regime changes; a structural share predicts the oscillation tracking enforcement capacity that the measurement series shows.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Mechanism composition of the arrangement''s suppressive force.').

omega_variable(
    implementation_divergence_valence,
    'Is implementation divergence a pressure valve that has prevented formal schism (functional) or a governance failure that institutionalizes contradiction (dysfunctional)?',
    'Compare communion integrity against pre-conciliar schism baselines and against confessional bodies that resolved equivalent disputes by textual definition (confessional-church comparisons).',
    'A functional valence supports the coordination half of the classification and lengthens expected persistence; a dysfunctional valence pushes the arrangement toward the extractive pole and predicts eventual formal division.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implementation_divergence_valence, conceptual, 'Feature-or-failure reading of divergent implementation across regimes.').

omega_variable(
    minority_vote_signal_status,
    'Do the non placet votes — roughly a tenth of the floor on the disputed schemas — signal theological incompatibility embedded unresolved in the final texts, as this reading holds, or ordinary conservative scruple subsequently satisfied by the official hermeneutic?',
    'Content-match the minority objections recorded in the acts against the magisterial confirmations and reversals of the following decades; persistent recurrence of the same objections across regimes indicates embedded incompatibility.',
    'Confirmation strengthens this reading''s foundational premise and raises the attributed extraction; satisfaction of the objections would weaken the embedded-incompatibility claim and pull the family toward the continuity reading''s profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_vote_signal_status, empirical, 'What the rejection-vote record actually signals about the final texts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t0, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement_basis(vati_tr_t0, observed).
narrative_ontology:measurement(vati_tr_t10, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement_basis(vati_tr_t10, observed).
narrative_ontology:measurement(vati_tr_t20, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement_basis(vati_tr_t20, observed).
narrative_ontology:measurement(vati_tr_t30, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 30, 0.29).
narrative_ontology:measurement_basis(vati_tr_t30, observed).
narrative_ontology:measurement(vati_tr_t40, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 40, 0.33).
narrative_ontology:measurement_basis(vati_tr_t40, observed).
narrative_ontology:measurement(vati_tr_t50, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 50, 0.36).
narrative_ontology:measurement_basis(vati_tr_t50, observed).
narrative_ontology:measurement(vati_tr_t60, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 60, 0.38).
narrative_ontology:measurement_basis(vati_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(vati_be_t0, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement_basis(vati_be_t0, observed).
narrative_ontology:measurement(vati_be_t10, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement_basis(vati_be_t10, observed).
narrative_ontology:measurement(vati_be_t20, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 20, 0.54).
narrative_ontology:measurement_basis(vati_be_t20, observed).
narrative_ontology:measurement(vati_be_t30, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 30, 0.57).
narrative_ontology:measurement_basis(vati_be_t30, observed).
narrative_ontology:measurement(vati_be_t40, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 40, 0.59).
narrative_ontology:measurement_basis(vati_be_t40, observed).
narrative_ontology:measurement(vati_be_t50, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 50, 0.61).
narrative_ontology:measurement_basis(vati_be_t50, observed).
narrative_ontology:measurement(vati_be_t60, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 60, 0.62).
narrative_ontology:measurement_basis(vati_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t0, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(vati_su_t0, observed).
narrative_ontology:measurement(vati_su_t10, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 10, 0.46).
narrative_ontology:measurement_basis(vati_su_t10, observed).
narrative_ontology:measurement(vati_su_t20, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 20, 0.56).
narrative_ontology:measurement_basis(vati_su_t20, observed).
narrative_ontology:measurement(vati_su_t30, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 30, 0.64).
narrative_ontology:measurement_basis(vati_su_t30, observed).
narrative_ontology:measurement(vati_su_t40, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement_basis(vati_su_t40, observed).
narrative_ontology:measurement(vati_su_t50, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 50, 0.5).
narrative_ontology:measurement_basis(vati_su_t50, observed).
narrative_ontology:measurement(vati_su_t60, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 60, 0.66).
narrative_ontology:measurement_basis(vati_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_magisterial_authority__composite_overdetermination_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__composite_overdetermination_reading, vatican_ii_magisterial_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__composite_overdetermination_reading, vatican_ii_magisterial_authority__rupture_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Vatican II' names one kernel instantiated by three structurally distinct constraints, decomposed per the epsilon-invariance principle: measuring the corpus as organic development, as fundamental break, or as engineered composite yields different epsilon values, different victim structures, and different classifications — so they are three files, not one. This story authors epsilon for the standing post-conciliar arrangement as the composite reading assesses it (engineered ambiguity, interpretive-rent collection, cyclical flank discipline); the continuity reading authors epsilon for the same corpus as organic development within unbroken tradition (expected negligible extraction); the rupture reading authors epsilon for it as fundamental break (expected high extraction with a different victim structure). Upstream/downstream: the continuity reading is the official citation source invoked to legitimize implementations, and this composite reading influences both siblings by explaining why their incompatible conclusions coexist in one corpus. Each family file links the others via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_magisterial_authority__composite_overdetermination_reading, powerless, 0.5).
constraint_indexing:directionality_override(vatican_ii_magisterial_authority__composite_overdetermination_reading, organized, 0.78).
constraint_indexing:directionality_override(vatican_ii_magisterial_authority__composite_overdetermination_reading, moderate, 0.68).
constraint_indexing:directionality_override(vatican_ii_magisterial_authority__composite_overdetermination_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
