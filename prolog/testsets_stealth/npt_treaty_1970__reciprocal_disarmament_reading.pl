% ============================================================================
% CONSTRAINT STORY: npt_treaty_1970__reciprocal_disarmament_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_1970__reciprocal_disarmament_reading, []).

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
 *   constraint_id: npt_treaty_1970__reciprocal_disarmament_reading
 *   human_readable: NPT Reciprocal Bargain Reading - Article VI as Binding Disarmament Obligation
 *   domain: international_law/nuclear_nonproliferation/regime_theory
 *
 * SUMMARY:
 *   This story instantiates the reciprocal_disarmament_reading of the 1970
 *   Non-Proliferation Treaty: Article VI as a binding legal obligation
 *   carrying temporal urgency, and horizontal abstention by non-weapon states
 *   as consideration for vertical disarmament by the weapon states. On this
 *   reading the standing arrangement is a bargain with one side performed and
 *   the other deferred indefinitely: the non-weapon majority pays continuous
 *   abstention, the weapon states collect the freeze, and the clause that was
 *   supposed to redeem the exchange carries no deadline, no verification, and
 *   no remedy. The claim/metric gap is deliberate: the claimed type is stated
 *   from the authoring seat as what is structurally true of the arrangement
 *   under this reading, while the metrics describe its actual operation - the
 *   engine computes per-seat classifications from the structural data, and
 *   divergence between claim and computed type is the measurement the corpus
 *   exists to take. Per the epsilon-invariance principle, the colloquial
 *   label 'the NPT' is decomposed into separate stories per reading; this
 *   file authors only the reciprocal reading's constraint, whose epsilon is
 *   indexed to the unverified-reciprocity injustice over the fixed referent
 *   of the standing arrangement.
 *
 * KEY AGENTS:
 *   - - p5_nuclear_weapon_states: Agenda-setting beneficiary (institutional/identity_locked) - collects the horizontal freeze; carries the unverified vertical promise
 *   - - nnws_nonaligned_coalition: Primary target (organized/constrained) - pays permanent abstention; holds procedural and moral leverage only
 *   - - former_arsenal_rollback_states: Maximum-cost target (moderate/trapped) - delivered actual rollback, received assurances short of treaties
 *   - - nnws_alliance_dependents: Dual-positioned (powerful/constrained) - collects extended deterrence, pays abstention and hosting burdens
 *   - - iaea_safeguards_secretariat: Administrator with a bounded mandate (institutional/constrained) - verifies Article III, structurally silent on Article VI
 *   - - threshold_outlier_states: Excluded counterexample (powerful/arbitrage) - kept arsenals by never joining
 *   - - withdrawn_breakout_state: Exited precedent (moderate/arbitrage) - exercised Article X at maximal cost
 *   - - humanitarian_initiative_coalition: Excluded challenger bloc (organized/mobile) - built a parallel instrument after decades of blocked agendas
 *   - - arms_control_regime_analysts: Analytical observer - sees which obligations verify and which dissolve into communique language
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__reciprocal_disarmament_reading, 0.66).
domain_priors:suppression_score(npt_treaty_1970__reciprocal_disarmament_reading, 0.53).
domain_priors:theater_ratio(npt_treaty_1970__reciprocal_disarmament_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 0.53).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__reciprocal_disarmament_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__reciprocal_disarmament_reading, "NPT Reciprocal Bargain Reading - Article VI as Binding Disarmament Obligation").
narrative_ontology:topic_domain(npt_treaty_1970__reciprocal_disarmament_reading, "international_law/nuclear_nonproliferation/regime_theory").

domain_priors:requires_active_enforcement(npt_treaty_1970__reciprocal_disarmament_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__reciprocal_disarmament_reading, 'bb508bad-20b3-4046-ad0f-3b7635ee29c5').
narrative_ontology:cs_kernel_codification('bb508bad-20b3-4046-ad0f-3b7635ee29c5', fixed_text).
narrative_ontology:cs_authority_grounding('bb508bad-20b3-4046-ad0f-3b7635ee29c5', distributed).
narrative_ontology:cs_reading_relation('bb508bad-20b3-4046-ad0f-3b7635ee29c5', npt_treaty_1970__oligopoly_enforcement_reading, forecloses).
narrative_ontology:cs_reading_relation('bb508bad-20b3-4046-ad0f-3b7635ee29c5', npt_treaty_1970__withdrawal_sovereignty_reading, influences).
narrative_ontology:cs_axiom('bb508bad-20b3-4046-ad0f-3b7635ee29c5', foundational, article_vi_binding_temporal_obligation).
narrative_ontology:cs_axiom_status(article_vi_binding_temporal_obligation, holdable).
narrative_ontology:cs_axiom_grounding('bb508bad-20b3-4046-ad0f-3b7635ee29c5', article_vi_binding_temporal_obligation, conventional).
narrative_ontology:cs_axiom('bb508bad-20b3-4046-ad0f-3b7635ee29c5', foundational, vertical_horizontal_reciprocity_requirement).
narrative_ontology:cs_axiom_status(vertical_horizontal_reciprocity_requirement, holdable).
narrative_ontology:cs_axiom_grounding('bb508bad-20b3-4046-ad0f-3b7635ee29c5', vertical_horizontal_reciprocity_requirement, deontological).
narrative_ontology:cs_reference_frame('bb508bad-20b3-4046-ad0f-3b7635ee29c5', reciprocal_grand_bargain).
narrative_ontology:cs_drift_state('bb508bad-20b3-4046-ad0f-3b7635ee29c5', post_2022_review_cycle_failure, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bb508bad-20b3-4046-ad0f-3b7635ee29c5', '').
narrative_ontology:cs_kernel_id(npt_treaty_1970__reciprocal_disarmament_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__reciprocal_disarmament_reading, p5_nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__reciprocal_disarmament_reading, nnws_alliance_dependents).
narrative_ontology:constraint_victim(npt_treaty_1970__reciprocal_disarmament_reading, nnws_nonaligned_coalition).
narrative_ontology:constraint_victim(npt_treaty_1970__reciprocal_disarmament_reading, former_arsenal_rollback_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__reciprocal_disarmament_reading, nnws_nonaligned_coalition).
narrative_ontology:constraint_victim(npt_treaty_1970__reciprocal_disarmament_reading, nnws_alliance_dependents).
narrative_ontology:constraint_vindicates(npt_treaty_1970__reciprocal_disarmament_reading, icj_1996_article_vi_good_faith_interpretation).
narrative_ontology:constraint_vindicates(npt_treaty_1970__reciprocal_disarmament_reading, pacta_sunt_servanda_treaty_good_faith).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Five states that tested weapons before 1967 hold permanent recognized arsenal status under the treaty. They dominate review conferences, hold Security Council vetoes that gate any enforcement referral, and set the export-control rules that police technology transfer. They collect the treaty's central return: no new nuclear state has emerged among parties since 1970 except by treaty exit. At the same time they carry the treaty's open-ended promise to negotiate away their own arsenals - a promise they interpret as hortatory, decline to schedule, and route around by modernizing warheads and delivery systems. Leaving the arrangement would cost them the legal recognition that freezes everyone else out.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, p5_nuclear_weapon_states, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__reciprocal_disarmament_reading, p5_nuclear_weapon_states, beneficiary).

% Verifies that non-weapon parties do not divert declared material: inspections, material accounting, the Additional Protocol. Its mandate ends at the fuel cycle - the treaty gives it no role in measuring whether the weapon states are honoring their disarmament undertakings, so the verification asymmetry is written into its charter. It reports diversion findings to the Security Council, where the weapon states' vetoes decide what happens next.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, iaea_safeguards_secretariat, agenda_setter,
    institutional, generational, constrained, global).

% Industrial democracies under United States or allied extended deterrence - Japan, South Korea, NATO's non-weapon members. They receive security guarantees that substitute for national arsenals and host forward-based capabilities. They pay by forgoing indigenous weapons permanently and by lending legitimacy to a review process their protectors dominate. Their security dependence makes exit unthinkable short of alliance collapse, and their protectors' arsenal modernization is visible to them from inside.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, nnws_alliance_dependents, beneficiary,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__reciprocal_disarmament_reading, nnws_alliance_dependents, payer).

% The Non-Aligned and like-minded bloc - Mexico, Egypt, Indonesia, South Africa and dozens more - that supplies the treaty's numerical weight. They surrendered the weapons option permanently and accept second-class legal status codified in the text; in exchange they hold the review-conference floor, where they block consensus documents to register the unfulfilled disarmament side of the bargain. Their leverage is procedural and moral rather than material: they can embarrass, delay, and withhold consent, but not compel.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, nnws_nonaligned_coalition, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__reciprocal_disarmament_reading, nnws_nonaligned_coalition, beneficiary).

% States that actually dismantled inherited or indigenous arsenals - South Africa, Kazakhstan, Ukraine, Belarus. They paid the bargain's maximum price and received the thinnest consideration: security assurances short of treaties, and in one case a memorandum whose principal guarantor later invaded. Reconstitution is prohibitively expensive and diplomatically closed; the assurances they hold are weaker than the abstention they delivered.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, former_arsenal_rollback_states, payer,
    moderate, biographical, trapped, regional).

% India, Pakistan, and Israel never signed and retain arsenals outside the treaty's legal map. They would renegotiate the bargain's terms - regional and capability-based rather than date-based - but have no seat in a conference of parties. Their existence is the standing counterexample the weapon states cite against disarmament urgency and the non-aligned cite against the regime's universality claims.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, threshold_outlier_states, excluded,
    powerful, generational, arbitrage, regional).

% North Korea exercised the treaty's withdrawal clause, tested weapons, and now sits outside the arrangement under maximal sanctions. Its trajectory is the live demonstration of what the exit path costs and what it yields: it kept the program and lost the economy. Every review conference debates its precedent; no party wants to follow it, and several quietly study it.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, withdrawn_breakout_state, excluded,
    moderate, biographical, arbitrage, regional).

% The roughly 120 states and civil-society campaigns behind the Humanitarian Initiative and the 2017 Ban Treaty. Locked out of meaningful agenda-setting inside review conferences by the consensus rule, they built a parallel instrument that prohibits the weapons outright. Their exit is partial and moral rather than material - most remain treaty parties - but they have stopped treating the review process as the venue where the bargain will be honored.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, humanitarian_initiative_coalition, excluded,
    organized, biographical, mobile, global).

% Regime theorists, verification specialists, and diplomatic historians who track the bargain's ledgers - stockpiles, delivery systems, review-conference outcomes, safeguard coverage. They see the whole structure: which obligations verify, which dissolve into communique language, and where the two accounts diverge.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, arms_control_regime_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_1970__reciprocal_disarmament_reading, p5_nuclear_weapon_states).
narrative_ontology:fixing_cost_class(npt_treaty_1970__reciprocal_disarmament_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the cascade-proliferation collective-action problem: each non-weapon state abstains only if others abstain too, and the treaty converts mutual abstention into a verifiable, near-universal commitment backed by common safeguards, so no party faces the neighborhood insecurity of arming alone.
% TRANSFER_FUNCTION: Moves permanent abstention and foregone weapons-option value from the non-weapon parties to the weapon states (preserved exclusivity and strategic stability); moves monitoring authority from national governments to the international inspectorate; and promises - without any verification machinery - a return flow of dismantled arsenals back to the general security of all parties.
% ABSENT_VOICES: The threshold outsiders (India, Pakistan, Israel) never accepted the bargain's terms and have no seat in a conference of parties; the populations downwind of test sites (the Marshallese, Semipalatinsk-region communities) bear costs the bargain priced at zero; and the Ban Treaty majority spent decades muted inside the consensus rule before building a venue of their own.
% DISAPPEARANCE_RATIONALE: Safeguards agreements, export-control lists, alliance deterrence planning, and five decades of abstention decisions all presuppose the treaty. Overnight disappearance would reopen the weapons question in Tokyo, Seoul, Berlin, Riyadh, and Cairo within a decade, and the Security Council's nonproliferation resolutions would lose their legal anchor.
% FOUNDING_PROBLEM: The 1960s fear of a proliferation cascade - forecasts of fifteen to twenty-five nuclear powers by the 1970s - and the desire of the existing weapon states to stabilize the club by freezing membership while managing the superpower arms race.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: IAEA annual safeguards reports document the diversion-detection function the arrangement performs; the Non-Aligned Movement's review-conference working papers and the Humanitarian Initiative's conference record attest, from outside the weapon states, both that the cascade problem remains live and that the disarmament side has gone undelivered. The weapon states themselves corroborate the cascade problem while disputing this reading's account of their own obligation; no neutral arbiter attests that the reciprocity has been honored, and the absence of such attestation is itself the finding.
narrative_ontology:disappearance_verdict(npt_treaty_1970__reciprocal_disarmament_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_1970__reciprocal_disarmament_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_1970__reciprocal_disarmament_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(npt_treaty_1970__reciprocal_disarmament_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_1970__reciprocal_disarmament_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_1970__reciprocal_disarmament_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_1970__reciprocal_disarmament_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_1970__reciprocal_disarmament_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.66 at interval end) because the arrangement's principal flow - permanent abstention from the many to exclusivity for the few - has run for fifty-five years while the redeeming flow remains promissory and unverified. Suppression (0.53) reflects the mix holding parties in place: export-control denial, sanction exposure, Security Council referral risk, and alliance dependency, layered over genuine consent. Theater (0.52) captures the review-conference ritual layer - reaffirmations, action plans, consensus failures - that has grown atop a real safeguards function; the ratio crosses 0.5 as successive disarmament plans (1995 Principles, 2000 Thirteen Steps, 2010 Action Plan) go unexecuted and the performative share of regime activity rises. Accessibility_collapse is low-moderate (0.38): alternatives partially exist and are known - the Ban Treaty, regional weapon-free zones, hedging, withdrawal - which is characteristic of a construct that must be defended rather than a natural limit. Resistance (0.55) is correspondingly real: bloc blockades, the humanitarian initiative, and one completed exit. The measurement series run on one shared nine-point grid anchored to review-cycle years. The review cycle imposes a quasi-periodic rhythm - each five-year cycle accumulates grievance, peaks at a consensus crisis (1995 extension, 2000 steps, 2015 and 2022 failures), and closes with partial recommission; the theater dip at 2000 marks the one cycle in which the disarmament ledger briefly became substantive. The oscillation is not itself the extraction mechanism here - extraction compounds steadily beneath the cycle - but the cycle launders it, converting each failure into fresh reaffirmation language.
 *
 * PERSPECTIVAL GAP:
 *   From the weapon-state seat the arrangement is a success it administers: proliferation prevented, stability managed, the disarmament clause a direction rather than a deadline. From the non-aligned and rollback seats the same structure reads as a permanent exchange of real abstention for promissory language - the enforcement gap is not an implementation detail but the precise point at which the bargain fails. The inspectorate seat experiences a working verification machine beside a clause it is forbidden to touch. The alliance-dependent seat sees both faces at once: it collects the deterrence the freeze makes possible and watches its protector modernize the arsenal the promise covers. These are computed divergences from the structural data; the authored claim does not reconcile them.
 *
 * DIRECTIONALITY LOGIC:
 *   The weapon states are declared beneficiaries - they collect the freeze - but they also carry the formal Article VI burden this reading treats as binding, so their position sits beneficiary-side without reaching the zero-cost pole: reputational exposure and modernization friction are real, if evadable. The non-aligned coalition and the rollback states are declared victims: the first pays ongoing abstention with only procedural recourse, the second paid in full and holds paper assurances. Alliance dependents straddle - declared beneficiaries collecting deterrence while paying abstention and hosting costs. Threshold outliers and the withdrawn state sit near the beneficiary pole by arbitrage: the arrangement's costs reach them chiefly as exclusion from legitimacy, not as foregone capability. No directionality_overrides are authored, and deliberately so: the override mechanism keys on power atoms, and every atom in this story contains agents whose true directionalities differ (institutional holds both the weapon states and the inspectorate; moderate holds both the rollback states and the withdrawn state; powerful holds both alliance dependents and threshold outliers), so any single-atom override would cross-contaminate. Differentiation rides on the beneficiary/victim declarations and exit atoms instead.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - preventing a proliferation cascade - remains live, so the arrangement is not a vestige; but the reciprocity half of the mandate has decayed toward ritual: five decades of reaffirmation without a negotiated instrument, review conferences that fail by consensus rather than deliver by it. The classification resists two symmetrical errors. Reading the arrangement as pure coordination erases the unpaid side of the bargain and launders the enforcement gap as implementation shortfall; reading it as pure extraction erases the real cascade-prevention function that even the Ban Treaty bloc implicitly relies on. The concentrated beneficiary that profits from the gap is also the actor that would bear the cost of closing it, which is why the arrangement persists in its lopsided form rather than completing into either a fulfilled bargain or an abandoned one - and why the theater ratio can climb past 0.5 without the structure collapsing into inert performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the npt_treaty_1970 kernel. Would instantiating the oligopoly_enforcement_reading instead change the structural verdict?',
    'Compare the compiled sibling stories: the oligopoly reading removes the weapon states from the victim set, lowers epsilon toward the coordination floor, and computes the arrangement as nearer a working coordination mechanism; whichever reading a compliance body adopts determines which ledger governs assessment.',
    'If the oligopoly reading governs, the Article VI gap is an implementation shortfall rather than an injustice, and the non-weapon coalition''s leverage claim loses its contractual basis; if this reading governs, the arrangement carries an unresolved debt that compounds with each review cycle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Reading choice determines whether the Article VI gap is structural injustice or implementation detail.').

omega_variable(
    article_vi_verifiability,
    'Can compliance with an obligation to negotiate disarmament be specified and measured at all, or is the standard inherently indeterminate?',
    'Comparative analysis of candidate compliance indicators - stockpile trajectories, doctrine declarations, fissile-material cutoff progress - against state practice in other treaty regimes with negotiation obligations.',
    'If verifiable, the enforcement gap becomes a measurable breach accumulating since 1970 and the injustice claim hardens; if inherently indeterminate, the reading''s temporal-urgency claim rests on an unoperationalizable standard and softens into a fairness complaint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_verifiability, empirical, 'Whether the binding-obligation reading can ground a measurable compliance standard.').

omega_variable(
    indefinite_extension_consent_effect,
    'Did the 1995 indefinite extension renew the original reciprocal terms, or discharge the weapon states'' side of the bargain by making the treaty permanent without conditions?',
    'Travaux of the 1995 Conference, the Decision documents'' Principles and Objectives, and subsequent state practice across review conferences.',
    'Determines whether post-1995 abstention is continuously exchanged consideration (this reading''s premise) or a settled gift whose reciprocity expired - the latter collapses the injustice claim into ordinary regret.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indefinite_extension_consent_effect, conceptual, 'Whether the 1995 extension renewed or extinguished the bargain''s reciprocity.').

omega_variable(
    nnws_leverage_conversion,
    'Can the non-weapon-state coalition convert normative leverage - Ban Treaty stigma, review-conference blockage - into material pressure that alters weapon-state calculus?',
    'Track whether TPNW-era stigma correlates with budget, posture, or doctrinal change in any weapon state over the coming decade.',
    'If conversion succeeds, the victim seat''s effective power rises and the arrangement may renegotiate toward a transitional compact with a real sunset; if it fails, entrenchment deepens and the theater ratio continues climbing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nnws_leverage_conversion, empirical, 'Whether the coalition''s normative leverage can become material leverage.').

omega_variable(
    retention_mechanism_composition,
    'Is non-weapon-state retention held by structural coercion (export-control denial, sanction exposure, alliance dependency) or by internalized security belief that would survive barrier removal?',
    'Post-exit and near-exit trajectories: the withdrawn state''s sanction experience measures the structural arm; alliance-dependents'' behavior during protector-unreliability episodes measures the internalized arm.',
    'If predominantly structural, suppression is external and addressable by institutional redesign; if substantially internalized, the arrangement persists even if enforcement relaxes, and the effective suppression exceeds the structural measure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retention_mechanism_composition, empirical, 'Structural versus internalized composition of the retention mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_1970__reciprocal_disarmament_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt_recip_tr_t1970, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 1970, 0.18).
narrative_ontology:measurement_basis(npt_recip_tr_t1970, observed).
narrative_ontology:measurement(npt_recip_tr_t1978, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 1978, 0.21).
narrative_ontology:measurement_basis(npt_recip_tr_t1978, observed).
narrative_ontology:measurement(npt_recip_tr_t1985, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 1985, 0.26).
narrative_ontology:measurement_basis(npt_recip_tr_t1985, observed).
narrative_ontology:measurement(npt_recip_tr_t1995, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 1995, 0.36).
narrative_ontology:measurement_basis(npt_recip_tr_t1995, observed).
narrative_ontology:measurement(npt_recip_tr_t2000, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 2000, 0.33).
narrative_ontology:measurement_basis(npt_recip_tr_t2000, observed).
narrative_ontology:measurement(npt_recip_tr_t2010, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 2010, 0.43).
narrative_ontology:measurement_basis(npt_recip_tr_t2010, observed).
narrative_ontology:measurement(npt_recip_tr_t2015, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 2015, 0.49).
narrative_ontology:measurement_basis(npt_recip_tr_t2015, observed).
narrative_ontology:measurement(npt_recip_tr_t2022, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 2022, 0.51).
narrative_ontology:measurement_basis(npt_recip_tr_t2022, observed).
narrative_ontology:measurement(npt_recip_tr_t2025, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 2025, 0.52).
narrative_ontology:measurement_basis(npt_recip_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(npt_recip_be_t1970, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 1970, 0.34).
narrative_ontology:measurement_basis(npt_recip_be_t1970, observed).
narrative_ontology:measurement(npt_recip_be_t1978, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 1978, 0.37).
narrative_ontology:measurement_basis(npt_recip_be_t1978, observed).
narrative_ontology:measurement(npt_recip_be_t1985, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 1985, 0.41).
narrative_ontology:measurement_basis(npt_recip_be_t1985, observed).
narrative_ontology:measurement(npt_recip_be_t1995, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 1995, 0.5).
narrative_ontology:measurement_basis(npt_recip_be_t1995, observed).
narrative_ontology:measurement(npt_recip_be_t2000, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 2000, 0.53).
narrative_ontology:measurement_basis(npt_recip_be_t2000, observed).
narrative_ontology:measurement(npt_recip_be_t2010, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement_basis(npt_recip_be_t2010, observed).
narrative_ontology:measurement(npt_recip_be_t2015, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 2015, 0.61).
narrative_ontology:measurement_basis(npt_recip_be_t2015, observed).
narrative_ontology:measurement(npt_recip_be_t2022, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 2022, 0.64).
narrative_ontology:measurement_basis(npt_recip_be_t2022, observed).
narrative_ontology:measurement(npt_recip_be_t2025, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 2025, 0.66).
narrative_ontology:measurement_basis(npt_recip_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(npt_recip_su_t1970, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 1970, 0.3).
narrative_ontology:measurement_basis(npt_recip_su_t1970, observed).
narrative_ontology:measurement(npt_recip_su_t1978, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 1978, 0.32).
narrative_ontology:measurement_basis(npt_recip_su_t1978, observed).
narrative_ontology:measurement(npt_recip_su_t1985, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 1985, 0.35).
narrative_ontology:measurement_basis(npt_recip_su_t1985, observed).
narrative_ontology:measurement(npt_recip_su_t1995, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 1995, 0.48).
narrative_ontology:measurement_basis(npt_recip_su_t1995, observed).
narrative_ontology:measurement(npt_recip_su_t2000, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement_basis(npt_recip_su_t2000, observed).
narrative_ontology:measurement(npt_recip_su_t2010, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 2010, 0.6).
narrative_ontology:measurement_basis(npt_recip_su_t2010, observed).
narrative_ontology:measurement(npt_recip_su_t2015, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 2015, 0.58).
narrative_ontology:measurement_basis(npt_recip_su_t2015, observed).
narrative_ontology:measurement(npt_recip_su_t2022, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 2022, 0.55).
narrative_ontology:measurement_basis(npt_recip_su_t2022, observed).
narrative_ontology:measurement(npt_recip_su_t2025, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 2025, 0.53).
narrative_ontology:measurement_basis(npt_recip_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__reciprocal_disarmament_reading, resource_allocation).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, npt_treaty_1970__oligopoly_enforcement_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, npt_treaty_1970__withdrawal_sovereignty_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the NPT' decomposes into at least three structurally distinct constraints - one per dominant reading of the same text. This file authors the reciprocal_disarmament_reading, whose epsilon is indexed to the unverified Article VI reciprocity over the fixed referent of the standing arrangement. The oligopoly_enforcement_reading authors a low-epsilon constraint over the same arrangement (Articles I-II operative, Article VI aspirational), and the withdrawal_sovereignty_reading authors a constraint centered on the Article X exit path. The upstream/downstream edge runs from this reading to the withdrawal reading: the justice argument this reading generates ('abstention was sold, delivery refused') is the raw material of withdrawal-threat politics, so this story structurally influences its sibling's operating environment. Family members link via affects_constraints per the epsilon-invariance rule; the confusion lives in the shared label, not in the mathematics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
