% ============================================================================
% CONSTRAINT STORY: ost_article_ii_non_appropriation__international_regime
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ost_article_ii_non_appropriation__international_regime, []).

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
 *   constraint_id: ost_article_ii_non_appropriation__international_regime
 *   human_readable: Article II Non-Appropriation: Deferred-Regime Reading (Appropriation Question Held Open Pending Multilateral Settlement)
 *   domain: international law/commons governance/space resources
 *
 * SUMMARY:
 *   Under the international_regime reading, Article II of the Outer Space
 *   Treaty does not settle whether extracting and owning celestially-sourced
 *   materials counts as forbidden appropriation; it HOLDS THE QUESTION OPEN,
 *   deferring it to a future multilateral regime in the way the Moon
 *   Agreement's Article XI later made explicit for its parties. The standing
 *   arrangement this reading describes is therefore an engineered limbo: no
 *   actor possesses treaty-authoritative title to extracted resources, no
 *   actor is treaty-prohibited from extracting them, and the settlement
 *   instrument (a regime) does not yet exist. Fifty-plus years on, the
 *   deferral still lacks a codified sunset; its justification is entirely
 *   transitional - keep the question unprejudiced until collective settlement
 *   capacity exists. Meanwhile national authorization statutes and bloc
 *   instruments layer practice on top of the open question, and COPUOS
 *   deliberation continues without convergence. KEY AGENTS (by structural
 *   relationship): - copuos_member_states: agenda-setting body
 *   ([institutional]/[constrained]) — administers the deferral without being
 *   able to impose its resolution - established_spacefaring_states: principal
 *   beneficiary ([institutional]/[arbitrage]) — retains optionality, exits
 *   into parallel governance - artemis_accords_states: beneficiary with payer
 *   residue ([powerful]/[mobile]) — gains operating legality, absorbs
 *   consistency criticism - first_mover_extraction_firms: dual-positioned
 *   payer/beneficiary ([organized]/[constrained]) — bears title-risk premium,
 *   collects royalty-free operation - space_resource_investors: payer
 *   ([organized]/[mobile]) — prices uncertainty into every round -
 *   latecomer_spacefaring_states: payer ([moderate]/[trapped]) — watches the
 *   window close on positional advantage - moon_agreement_states_parties:
 *   identity-locked beneficiary ([organized]/[identity_locked]) — upholds the
 *   Article XI template - planetary_protection_advocates: excluded
 *   ([organized]/[trapped]) — would demand environmental safeguards inside
 *   any regime - space_law_scholarship: analytical observer
 *   ([analytical]/[analytical]) — maps the reading contest without
 *   adjudicating it
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ost_article_ii_non_appropriation__international_regime, 0.52).
domain_priors:suppression_score(ost_article_ii_non_appropriation__international_regime, 0.45).
domain_priors:theater_ratio(ost_article_ii_non_appropriation__international_regime, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, extractiveness, 0.52).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__international_regime, scaffold).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__international_regime, "Article II Non-Appropriation: Deferred-Regime Reading (Appropriation Question Held Open Pending Multilateral Settlement)").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__international_regime, "international law/commons governance/space resources").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__international_regime, 'c1b25410-94e6-4e30-9339-6eeda7685b67').
narrative_ontology:cs_kernel_codification('c1b25410-94e6-4e30-9339-6eeda7685b67', fixed_text).
narrative_ontology:cs_authority_grounding('c1b25410-94e6-4e30-9339-6eeda7685b67', distributed).
narrative_ontology:cs_reading_relation('c1b25410-94e6-4e30-9339-6eeda7685b67', ost_article_ii_non_appropriation__extraction_permissive, influences).
narrative_ontology:cs_reading_relation('c1b25410-94e6-4e30-9339-6eeda7685b67', ost_article_ii_non_appropriation__commons_conservation, influences).
narrative_ontology:cs_axiom('c1b25410-94e6-4e30-9339-6eeda7685b67', foundational, appropriation_question_deferred_to_future_international_regime).
narrative_ontology:cs_axiom_status(appropriation_question_deferred_to_future_international_regime, holdable).
narrative_ontology:cs_axiom_grounding('c1b25410-94e6-4e30-9339-6eeda7685b67', appropriation_question_deferred_to_future_international_regime, conventional).
narrative_ontology:cs_axiom('c1b25410-94e6-4e30-9339-6eeda7685b67', secondary, no_unilateral_reading_authoritative_absent_multilateral_framework).
narrative_ontology:cs_axiom_status(no_unilateral_reading_authoritative_absent_multilateral_framework, holdable).
narrative_ontology:cs_axiom_grounding('c1b25410-94e6-4e30-9339-6eeda7685b67', no_unilateral_reading_authoritative_absent_multilateral_framework, conventional).
narrative_ontology:cs_reference_frame('c1b25410-94e6-4e30-9339-6eeda7685b67', deferred_regime_settlement).
narrative_ontology:cs_drift_state('c1b25410-94e6-4e30-9339-6eeda7685b67', commercial_extraction_era_post_cslca, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c1b25410-94e6-4e30-9339-6eeda7685b67', '').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__international_regime, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__international_regime, established_spacefaring_states).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__international_regime, artemis_accords_states).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__international_regime, first_mover_extraction_firms).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__international_regime, first_mover_extraction_firms).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__international_regime, space_resource_investors).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__international_regime, latecomer_spacefaring_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__international_regime, moon_agreement_states_parties).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__international_regime, artemis_accords_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convene the consensus process in which any resource regime would be negotiated. Individually each member can block text; collectively none can impose settlement, so the committee administers the open question by keeping it on the agenda session after session without producing agreement. Exiting the process would mean abandoning the only universally legitimate venue, which no member wants to forfeit even while frustrated by it.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, copuos_member_states, agenda_setter,
    institutional, generational, constrained, global).

% Retain full optionality under the open question: bound by no common-heritage obligation, committed to no regime design, and free to sponsor national authorization frameworks for their industries while reaffirming Article II's sovereignty ban. When multilateral terms look unfavorable they route around the forum through parallel instruments; when they look favorable they return to negotiate from a position strengthened by accumulated practice.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, established_spacefaring_states, beneficiary,
    institutional, generational, arbitrage, global).

% Layer bloc-level understandings (including 'safety zones') atop the unresolved treaty question, gaining immediate operating legality for nationally licensed firms. They absorb recurring criticism that such zones function as quasi-appropriation inconsistent with Article II, a reputational and treaty-consistency cost paid in exchange for the operating freedom the deferral otherwise leaves unsecured.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, artemis_accords_states, beneficiary,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(ost_article_ii_non_appropriation__international_regime, artemis_accords_states, payer).

% Invest mission-specific capital in extraction hardware under national licenses while possessing no multilaterally recognized title to anything recovered. They pay the uncertainty premium in insurance rates, financing terms, and valuation discounts; simultaneously they operate royalty-free in the grey zone, owing nothing to any regime because no regime exists. Leaving is costly: launch manifests, flight hardware, and demonstration missions are sunk and non-fungible.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, first_mover_extraction_firms, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ost_article_ii_non_appropriation__international_regime, first_mover_extraction_firms, beneficiary).

% Price unresolved title into every term sheet: longer diligence, higher hurdle rates, structured downside protection. Unlike the firms they fund, their capital is portable - when uncertainty spikes they can rotate into government-contracted work, terrestrial analogues, or other sectors entirely, taking the scale-up funding the sector needs with them.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, space_resource_investors, payer,
    organized, biographical, mobile, global).

% Lack current extraction capability and therefore cannot manufacture the positional facts that first movers are accumulating. Each year of continued deferral converts waiting time into others' advantage: launch windows, landing sites, and operational know-how concentrate where capability already sits. Their realistic path to any share of celestial resources runs through an eventual regime, so their bargaining position erodes the longer settlement waits - and they have no alternative venue in which to alter that arithmetic.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, latecomer_spacefaring_states, payer,
    moderate, generational, trapped, global).

% A small ratifying group lacking every major space power, upholding the common-heritage regime of Article XI as the template toward which the deferral points. The principle is constitutive of the coalition's shared identity rather than merely instrumental to its interests; maintaining the Article XI lineage as living precedent is the group's defining activity. Abandoning the frame would dissolve the bloc into ordinary interest-group politics.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, moon_agreement_states_parties, beneficiary,
    organized, generational, identity_locked, global).

% Argue that all three readings of the appropriation question leave environmental protection unaddressed - extraction under ANY settlement regime could degrade scientifically pristine environments absent binding safeguards. They hold no formal seat in the negotiation process and reach it only indirectly through advocacy; the substantive design conversations proceed without them.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, planetary_protection_advocates, excluded,
    organized, generational, trapped, global).

% Map the competing readings, document the accumulation of national-practice precedent, and supply the interpretive authority that neither bloc fully controls. Produces the doctrinal analyses, symposium literature, and IISL position papers through which the reading contest is conducted; adjudicates nothing and collects nothing.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, space_law_scholarship, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ost_article_ii_non_appropriation__international_regime, first_mover_extraction_firms).
narrative_ontology:fixing_cost_class(ost_article_ii_non_appropriation__international_regime, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Holds the celestial-resource appropriation question open during technological immaturity, preventing premature unilateral or bloc-level settlement that would prejudice a multilateral outcome, while preserving Article II's sovereignty ban as the fixed floor beneath the unresolved question.
% TRANSFER_FUNCTION: Moves title-certainty risk onto first-mover extraction firms and their investors (paid as insurance premiums, financing discounts, and forgone scale), and converts elapsed deferral time into de facto positional advantage for actors capable of unilateral action - a slow transfer of effective control over celestial resources from absent future participants to present capable ones.
% ABSENT_VOICES: Planetary-protection advocates and prospective non-spacefaring stakeholders would object that the question is being framed purely as a distribution fight among capable parties, with environmental safeguards treated as an afterthought to be added post-settlement; they are outside the room because COPUOS seats are state-held and agenda-setting is consensus-gated. Private operators held no formal voice for most of the interval, entering only through national delegations sympathetic to their position.
% DISAPPEARANCE_RATIONALE: If the deferral ended overnight - either because a binding multilateral regime appeared or because unilateral appropriation became normalized - the entire cislunar resource economy would reorganize within months: financing structures, insurance products, mission targeting, and site-staking behavior all key off current title expectations. Either resolution redistributes billions in anticipated asset value and rewrites which actors can commit capital.
% FOUNDING_PROBLEM: At the 1967 drafting, the immediate problem was preventing the Cold War superpowers from extending territorial sovereignty to the Moon and celestial bodies; the drafters deliberately left resource-utilization rules unsettled, anticipating that a future regime would be negotiated once technology and institutions matured (a structure the Moon Agreement's Article XI later codified for its parties).
% FOUNDING_PROBLEM_CORROBORATION: The anti-sovereignty half of the founding problem is corroborated from well outside any beneficiary set: the published negotiation record (Outer Space Treaty drafting history), the UN secretariat's own documentation, and the scholarly consensus of the space-law literature all attest it. The resource-settlement half is attested as still-open by academic commentators and civil-society submissions to COPUOS; no major-power government currently attests that the question remains genuinely open, and the benefiting blocs assert opposite statuses - the Artemis-bloc states treat it as practically settled by national frameworks, the Moon Agreement parties treat it as awaiting their template. Corroboration for the 'contested' status itself rests on academic and intergovernmental-secretariat sources, not on the benefiting parties.
narrative_ontology:disappearance_verdict(ost_article_ii_non_appropriation__international_regime, world_rearranges).
narrative_ontology:founding_problem_status(ost_article_ii_non_appropriation__international_regime, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ost_article_ii_non_appropriation__international_regime, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ost_article_ii_non_appropriation__international_regime, 'none', 1).
narrative_ontology:epsilon_provenance(ost_article_ii_non_appropriation__international_regime, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ost_article_ii_non_appropriation__international_regime_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ost_article_ii_non_appropriation__international_regime, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ost_article_ii_non_appropriation__international_regime_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52, matching the 2026 measurement) reflects the uncertainty premium the deferral imposes on governed actors: title-less operation raises insurance costs, depresses valuation multiples, and chills scale investment - a real cost, but diffuse and indirect rather than a collected levy, hence below the snare band. Suppression (0.45) is denial-of-recognition rather than denial-of-action: firms can launch under national licenses, so the constraint suppresses the core good (multilaterally recognized title) while leaving operation formally open; that asymmetry places it just under the midpoint. Theater ratio (0.55, crossing 0.5 after 2020) tracks the growing share of COPUOS and allied activity that is procedural performance - working papers, building-blocks documents, anniversary symposia - relative to convergent output; the underlying non-sovereignty compliance remains genuinely functional, so the metric is elevated but nowhere near piton-grade theatricality. Accessibility collapse (0.25): once actors understand the deferral, alternatives are abundantly available - national frameworks, bloc instruments, bilateral arrangements - which is precisely why the arrangement erodes rather than commands. Resistance (0.62) is sustained and organized: national resource statutes, the Artemis instrument set, and counter-bloc positioning all constitute active resistance to letting the deferral resolve on multilateral terms alone. All three temporal series are authored on ONE shared seven-point grid (1967-2026) per the alignment rule; suppression_requirement is tracked because the story specifically traces enforcement-capacity change: maintaining the deferral was nearly effortless when extraction was science fiction (1967-1991) and requires progressively more active diplomatic defense as hardware approaches revenue. Claim/metric independence is preserved: scaffold is claimed from the transition-justification structure, not tuned to the metrics; where the engine's per-seat computation diverges, that divergence is the datum.
 *
 * PERSPECTIVAL GAP:
 *   Seats should classify differently. From the agenda-setter seat (copuos_member_states), the deferral is patient statecraft: nothing is collected, nothing is seized, and the forum's consensual paralysis is experienced as prudence. From the payer seats (first_mover_extraction_firms, space_resource_investors), the identical structure is an unresolved-risk tax levied on exactly the actors doing the capital-intensive work. From the identity-locked beneficiary seat (moon_agreement_states_parties), it is a fiduciary window-hold whose continuation is constitutive of the bloc's self-conception - professional-relational identity fusion in which abandoning the deferral would dissolve the coalition's reason to exist. From the trapped payer seat (latecomer_spacefaring_states), it is a closing door. Same nominal institutional tier, radically different constraint experience: differentiation runs on capability to create faits accomplis, not on treaty rank. The identity-lock mechanism here is ideological-institutional: the common-heritage principle is not merely preferred by these parties, it defines them; if the frame broke, they would migrate to ordinary interest-group bargaining and the deferral would face a differently-composed opposition.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive derivation without overrides. established_spacefaring_states sit near the beneficiary pole (d low): they collect optionality and face zero extraction, and their arbitrage-grade exit into parallel governance pushes them further toward the subsidized end. artemis_accords_states derive low-but-not-zero d: they benefit from grey-zone legality while carrying reputational and treaty-consistency costs (their secondary payer role). moon_agreement_states_parties derive low d as beneficiaries, though their identity-lock means the subsidy is partly psychological rather than material. first_mover_extraction_firms are the structural hinge: the victims declaration drives d toward the target pole, tempered by the secondary beneficiary role (royalty-free operation today); net d lands high-but-not-maximal - they are the arrangement's principal funders of risk and principal collectors of unpriced access simultaneously. space_resource_investors derive near-target d with mobile exit damping effective extraction somewhat. latecomer_spacefaring_states derive near-full-target d: trapped exit, generational stakes, and a shrinking claim on the eventual settlement. copuos_member_states sit approximately symmetric as administrators who neither collect nor materially pay. No directionality_overrides were needed: exit-option differentiation (arbitrage/mobile/constrained/trapped/identity_locked) cleanly separates same-power actors that a power-atom-keyed override could not distinguish.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold classification does distinct preventive work against two misreadings. Read as pure coordination (rope), the deferral's fifty-year persistence looks like successful peace-preservation and its growing proceduralism goes unexamined; read as pure extraction (snare), the uncertainty premium looks like deliberate incumbent protection and the genuine anti-sovereignty function disappears. Naming it scaffold forces the transition question: what is this FOR, and when does it end? The answer exposes the arrangement's central vulnerability - unlike a properly drafted scaffold, this one carries NO CODIFIED SUNSET CLAUSE. The transition justification lives in interpretive tradition and the Moon Agreement's Article XI aspiration, not in any terminating instrument; the founding problem's territorial half (prevent sovereignty claims) remains live and genuinely served, but the resource-settlement half has been aging since 1979 with no completion mechanism. The measurements document the resulting drift signature: theater crossing 0.5 as negotiation becomes performance, extractiveness climbing as uncertainty compounds on approaching revenue, suppression requirements rising as defending the deferral starts costing diplomatic effort. The founding_problem_status x disappearance_verdict pair (contested x world_rearranges) does not yet trip the capture/zombie mismatch flag, but the trajectory - a sunset-less scaffold with rising theater - is the canonical pre-piton profile, and the omegas regime_before_consolidation and grey_zone_capital_tolerance are the instruments watching for it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates ONLY the international_regime reading of kernel ost_article_ii_non_appropriation. What would the sibling readings change structurally, and where exactly is the disagreement located?',
    'Comparative compilation of the three sibling stories (extraction_permissive, commons_conservation) with this one: the disagreement is located in two textual elements of Article II - whether ''use or occupation'' extends to resource removal (not merely territorial claim), and whether the treaty''s silence on resource title DEFERS the question to a future regime or settles it implicitly.',
    'Under extraction_permissive, first_mover_extraction_firms stop being payers (title risk vanishes) and victims concentrate in latecomer states; under commons_conservation, first movers become violators rather than grey-zone actors and future generations enter the beneficiary set; epsilon differs sharply across all three. Each sibling is a separate file, not a parameter of this one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: one reading of a contested kernel; sibling readings are distinct constraints.').

omega_variable(
    regime_before_consolidation,
    'Will a binding multilateral regime arrive before de facto extraction norms consolidate through accumulated national-authorization practice?',
    'Track COPUOS working-group output for convergence on draft principles with scheduled adoption versus indefinite consensus items; track cumulative count of national resource-authorization statutes and executed extractions.',
    'If consolidation wins the race, this scaffold loses its transition justification and decays toward a piton (inertial deferral maintained theatrically) or a captured structure favoring first movers; if a regime lands first, the scaffold completes its purpose and dissolves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regime_before_consolidation, empirical, 'Race between regime formation and fait-accompli accumulation.').

omega_variable(
    moon_agreement_anchor_strength,
    'Does Moon Agreement Article XI give the deferral reading any binding anchor, given roughly twenty ratifications excluding every major spacefaring power?',
    'Doctrinal analysis of customary-international-law uptake of the common-heritage formula alongside ratification trends; observation of whether Artemis-bloc instruments cite or distinguish Article XI.',
    'A strong customary anchor supports the deferral reading''s coherence (a destination exists for the transition); a weak anchor leaves the scaffold pointing at a destination no one is obligated to build, accelerating piton drift.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(moon_agreement_anchor_strength, empirical, 'Whether the Article XI analogue binds anyone who matters.').

omega_variable(
    zero_sum_distribution_frame,
    'Is the regime-distributional conflict genuinely zero-sum, or zero-sum mainly because bloc positions frame it that way?',
    'Test candidate positive-sum designs (pooled royalties, orbital-slot-analogous licensing, phased capability-weighted shares) against stated red lines of each bloc; identify whether objections survive design changes.',
    'If positive-sum designs become credible, the stall breaks, the scaffold''s transition completes on schedule, and measured theater falls; if the zero-sum frame is constitutive, deferral persists indefinitely regardless of design ingenuity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(zero_sum_distribution_frame, conceptual, 'Whether the negotiation deadlock reflects the problem''s structure or the parties'' framing.').

omega_variable(
    grey_zone_capital_tolerance,
    'How much title uncertainty can space-resource capital underwrite before it routes entirely around multilateralism by stacking national authorizations?',
    'Observe insurance pricing, investment-round language, and corporate disclosure of title-risk treatment as extraction missions approach revenue; compare capital flows into jurisdictions offering stronger domestic title guarantees.',
    'High tolerance extends the deferral''s viable life as scaffold; low tolerance collapses it from below as actors exit the multilateral frame entirely, converting the deferral into dead letter.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(grey_zone_capital_tolerance, empirical, 'Deferral lifespan as a function of capital''s tolerance for unresolved title.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__international_regime, 1967, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ost__tr_t1967, ost_article_ii_non_appropriation__international_regime, theater_ratio, 1967, 0.1).
narrative_ontology:measurement(ost__tr_t1979, ost_article_ii_non_appropriation__international_regime, theater_ratio, 1979, 0.2).
narrative_ontology:measurement(ost__tr_t1991, ost_article_ii_non_appropriation__international_regime, theater_ratio, 1991, 0.28).
narrative_ontology:measurement(ost__tr_t2004, ost_article_ii_non_appropriation__international_regime, theater_ratio, 2004, 0.33).
narrative_ontology:measurement(ost__tr_t2015, ost_article_ii_non_appropriation__international_regime, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(ost__tr_t2020, ost_article_ii_non_appropriation__international_regime, theater_ratio, 2020, 0.47).
narrative_ontology:measurement(ost__tr_t2026, ost_article_ii_non_appropriation__international_regime, theater_ratio, 2026, 0.55).

% Extraction over time
narrative_ontology:measurement(ost__be_t1967, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 1967, 0.1).
narrative_ontology:measurement(ost__be_t1979, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 1979, 0.18).
narrative_ontology:measurement(ost__be_t1991, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 1991, 0.22).
narrative_ontology:measurement(ost__be_t2004, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 2004, 0.28).
narrative_ontology:measurement(ost__be_t2015, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 2015, 0.38).
narrative_ontology:measurement(ost__be_t2020, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 2020, 0.46).
narrative_ontology:measurement(ost__be_t2026, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 2026, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(ost__su_t1967, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 1967, 0.1).
narrative_ontology:measurement(ost__su_t1979, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 1979, 0.14).
narrative_ontology:measurement(ost__su_t1991, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 1991, 0.17).
narrative_ontology:measurement(ost__su_t2004, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 2004, 0.24).
narrative_ontology:measurement(ost__su_t2015, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 2015, 0.32).
narrative_ontology:measurement(ost__su_t2020, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 2020, 0.39).
narrative_ontology:measurement(ost__su_t2026, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 2026, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ost_article_ii_non_appropriation__international_regime, resource_allocation).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__international_regime, ost_article_ii_non_appropriation__extraction_permissive).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__international_regime, ost_article_ii_non_appropriation__commons_conservation).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'Article II non-appropriation' covers three structurally distinct claims that cannot share one story. The extraction_permissive reading yields low epsilon (title certainty, minimal extraction beyond ordinary licensing friction); the commons_conservation reading yields high epsilon assessed against would-be extractors and assigns future generations a beneficiary stake; THIS reading (international_regime) yields intermediate epsilon (0.52) - the uncertainty premium - with a dual-positioned first-mover seat and a trapped latecomer seat. Upstream/downstream structure: the deferral reading is causally UPSTREAM of both siblings' viability - every year of successful deferral accumulates operational precedent feeding the permissive reading while consuming the window the conservation reading depends on; hence this story's edges run TO both siblings. Each sibling file reciprocates the edge and documents its own delta in narrative_context.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
