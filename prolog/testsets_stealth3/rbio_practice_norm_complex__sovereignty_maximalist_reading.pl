% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__sovereignty_maximalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rbio_practice_norm_complex__sovereignty_maximalist_reading, []).

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
 *   constraint_id: rbio_practice_norm_complex__sovereignty_maximalist_reading
 *   human_readable: Absolute Sovereignty Shield - RBIO Sovereignty-Maximalist Reading
 *   domain: international_relations/international_law/political_economy
 *
 * SUMMARY:
 *   This story instantiates ONE reading - the sovereignty-maximalist reading
 *   - of the contested kernel rbio_practice_norm_complex (the Rules-Based
 *   International Order norm complex). Under this reading the operative
 *   constraint is: state sovereignty is absolute; RBIO norms are legitimate
 *   only insofar as they protect sovereignty against external interference;
 *   humanitarian exceptions are pretexts for regime change. The epsilon
 *   referent is the standing absolute-sovereignty arrangement itself - the
 *   arrangement this reading endorses and defends - described by this
 *   reading's own structural data: it coordinates interstate peace and
 *   shields vulnerable states from predation, and it simultaneously
 *   forecloses external recourse for populations living under repressive
 *   governments. Claimed type and metrics are authored independently: the
 *   claim is tangled_rope (a genuine war-prevention coordination function
 *   fused with asymmetric extraction from trapped populations, held in place
 *   by active enforcement), while the metrics describe substantially
 *   extractive, increasingly theatrical, actively enforced operation. The
 *   sibling readings (liberal_institutional_reading,
 *   hegemonic_extraction_reading) are separate constraints with their own
 *   epsilon values, beneficiary structures, and classifications; they are
 *   linked through the network, not averaged into this one. KEY AGENTS (by
 *   structural relationship): - authoritarian_regimes: Primary beneficiary
 *   (institutional/arbitrage) - collects immunity from external
 *   accountability - trapped_populations_repressive_states: Primary target
 *   (powerless/trapped) - bears the costs of foreclosed recourse -
 *   small_vulnerable_states: Secondary beneficiary (moderate/constrained) -
 *   collects protection from great-power predation -
 *   great_power_norm_defenders: Enforcement administrator
 *   (institutional/arbitrage) - wields vetoes and diplomatic weight to hold
 *   the shield - humanitarian_advocacy_coalition: Excluded claimant
 *   (organized/constrained) - would authorize recourse; kept outside the
 *   conversation - international_legal_community: Analytical observer
 *   (institutional/analytical) - sees the full structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.65).
domain_priors:suppression_score(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.7).
domain_priors:theater_ratio(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__sovereignty_maximalist_reading, tangled_rope).
narrative_ontology:human_readable(rbio_practice_norm_complex__sovereignty_maximalist_reading, "Absolute Sovereignty Shield - RBIO Sovereignty-Maximalist Reading").
narrative_ontology:topic_domain(rbio_practice_norm_complex__sovereignty_maximalist_reading, "international_relations/international_law/political_economy").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__sovereignty_maximalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__sovereignty_maximalist_reading, '8f6dde69-4ee6-4a48-91ba-0dd68518eba2').
narrative_ontology:cs_kernel_codification('8f6dde69-4ee6-4a48-91ba-0dd68518eba2', fixed_text).
narrative_ontology:cs_authority_grounding('8f6dde69-4ee6-4a48-91ba-0dd68518eba2', lineage).
narrative_ontology:cs_interpretation_layer_present('8f6dde69-4ee6-4a48-91ba-0dd68518eba2').
narrative_ontology:cs_reading_relation('8f6dde69-4ee6-4a48-91ba-0dd68518eba2', rbio_practice_norm_complex__liberal_institutional_reading, forecloses).
narrative_ontology:cs_reading_relation('8f6dde69-4ee6-4a48-91ba-0dd68518eba2', rbio_practice_norm_complex__hegemonic_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('8f6dde69-4ee6-4a48-91ba-0dd68518eba2', foundational, sovereignty_absolutism_no_intervention_authority).
narrative_ontology:cs_axiom_status(sovereignty_absolutism_no_intervention_authority, holdable).
narrative_ontology:cs_axiom_grounding('8f6dde69-4ee6-4a48-91ba-0dd68518eba2', sovereignty_absolutism_no_intervention_authority, conventional).
narrative_ontology:cs_axiom('8f6dde69-4ee6-4a48-91ba-0dd68518eba2', foundational, humanitarian_justifications_are_regime_change_pretexts).
narrative_ontology:cs_axiom_status(humanitarian_justifications_are_regime_change_pretexts, holdable).
narrative_ontology:cs_axiom_grounding('8f6dde69-4ee6-4a48-91ba-0dd68518eba2', humanitarian_justifications_are_regime_change_pretexts, empirically_contingent).
narrative_ontology:cs_reference_frame('8f6dde69-4ee6-4a48-91ba-0dd68518eba2', westphalian_absolute_sovereignty_settlement).
narrative_ontology:cs_drift_state('8f6dde69-4ee6-4a48-91ba-0dd68518eba2', contemporary_multipolar_contest, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('8f6dde69-4ee6-4a48-91ba-0dd68518eba2', '').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__sovereignty_maximalist_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__sovereignty_maximalist_reading, authoritarian_regimes).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__sovereignty_maximalist_reading, small_vulnerable_states).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__sovereignty_maximalist_reading, trapped_populations_repressive_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run states where political competition, press freedom, and judicial independence are suppressed. The norm shields them from external accountability: no intervention, minimal conditionality, and humanitarian appeals reframed as pretexts. They invoke non-interference selectively - demanding it for themselves while backing allies' cross-border operations - and move fluidly among patrons, voting blocs, and veto relationships. Nothing about their position requires leaving the arrangement; their survival strategy is built on it.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, authoritarian_regimes, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__sovereignty_maximalist_reading, authoritarian_regimes, agenda_setter).

% Live under governments that jail dissidents, close courts, and at the extreme commit mass atrocities. Every external recourse channel is closed to them: intervention is barred, conditionality is barred unless leaving it costs nothing, and their advocates' testimony is discounted as pretext. Emigration is restricted, dangerous, or unavailable to the poorest; inside, there is no independent forum. Their exposure deepens with each crisis the shield covers, and the class spans dozens of states with no common assembly in which to act.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, trapped_populations_repressive_states, payer,
    powerless, biographical, trapped, global).

% Weak states without power projection whose main protection against stronger neighbors and great-power predation is the non-intervention rule itself. They defend the norm in general assemblies and regional bodies and receive real security from it. The same rule occasionally blocks them from welcoming external pressure against atrocities next door or against spillover from collapsed neighbors, a cost they absorb rather than shape.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, small_vulnerable_states, beneficiary,
    moderate, generational, constrained, global).

% Permanent Security Council members and allied blocs that block intervention-authorizing resolutions by veto and argue sovereignty doctrine in every forum. They determine which crises receive any recourse and which do not, and their patronage ties to client regimes depend on the shield holding. When the doctrine is challenged they escalate blocking activity rather than concede ground, and they retain the option of invoking the same norms selectively elsewhere.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, great_power_norm_defenders, agenda_setter,
    institutional, generational, arbitrage, global).

% NGOs, responsibility-to-protect proponents, exile networks, and several middle powers pressing for humanitarian exceptions, atrocity referrals, and targeted conditionality. Under this reading their claims are classified in advance as pretexts, so they lobby, document, and litigate from outside the councils where the norm is actually enforced; host states can deregister or expel them, and their access depends on the goodwill of the very governments they criticize.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, humanitarian_advocacy_coalition, excluded,
    organized, biographical, constrained, global).

% Scholars, special rapporteurs, and judges who interpret the line the Charter draws between domestic jurisdiction and international concern. They observe the widening distance between the absolute-sovereignty claim and actual state practice - sanctions regimes, conditional lending, criminal referrals - and publish analyses of the gap without holding any vote on resolving it.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, international_legal_community, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rbio_practice_norm_complex__sovereignty_maximalist_reading, authoritarian_regimes).
narrative_ontology:fixing_cost_class(rbio_practice_norm_complex__sovereignty_maximalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the interstate collective-action problem of preventive war, border revisionism, and pretext-driven intervention: by making sovereignty inviolable and external interference presumptively illegitimate, it stabilizes borders, lowers security dilemmas, and gives weak states predictable protection against predation by stronger ones.
% TRANSFER_FUNCTION: Moves immunity from external accountability to incumbent regimes (and security assurance to all states fearing intervention precedent), while moving the costs of unchecked internal repression - foreclosed recourse, unremedied atrocity, suppressed dissent - onto the populations inside repressive states.
% ABSENT_VOICES: Populations under repressive governments have no seat: the reading dismisses their advocates in advance as instruments of interventionism, so the people bearing the arrangement's costs are represented only by parties with an incentive to minimize their claims. Exiled dissidents, atrocity survivors, and humanitarian NGOs would object that protection of sovereignty and protection of persons are being conflated; they are outside the room by doctrinal design, not by accident.
% DISAPPEARANCE_RATIONALE: If the absolute-sovereignty norm vanished overnight, intervention decisions would proliferate wherever capability met motive, alliance structures and nuclear deterrence postures would be rebuilt around the new free-for-all, dozens of regimes would redesign their survival strategies around anticipated external action, and weak states would scramble for alternative protectors - the entire architecture of interstate restraint would have to be renegotiated from scratch.
% FOUNDING_PROBLEM: Prevent a recurrence of great-power war and imperial intervention by mutual renunciation of interference: the Westphalian settlement ended confessional intervention, and the UN Charter settlement ended aggressive war and sphere-of-interest predation after 1945 by making sovereignty the bedrock of interstate order.
% FOUNDING_PROBLEM_CORROBORATION: Small vulnerable states and the historical record of the post-1945 decline in interstate war corroborate from outside the regime-beneficiary set that the war-prevention core solves a still-live problem. However, no neutral seat attests that ABSOLUTE sovereignty was the founding term: the Charter's own drafting and text include Chapter VII enforcement authority and human-rights obligations that this reading discounts, and humanitarian-law scholars corroborate that the founders did not intend blanket immunity for internal repression. The reading's extension of the founding problem to total foreclosure of recourse is attested mainly by its beneficiaries.
narrative_ontology:disappearance_verdict(rbio_practice_norm_complex__sovereignty_maximalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(rbio_practice_norm_complex__sovereignty_maximalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rbio_practice_norm_complex__sovereignty_maximalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rbio_practice_norm_complex__sovereignty_maximalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.65, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rbio_practice_norm_complex__sovereignty_maximalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rbio_practice_norm_complex__sovereignty_maximalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rbio_practice_norm_complex__sovereignty_maximalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is 0.65 because the arrangement's costs fall on a seat with no compensating return: populations under repressive governments lose every external recourse channel precisely when internal channels are also closed, while the benefit side (war prevention, border stability) is real but diffusely shared. Suppression is 0.70 and unscaled by construction - it is the raw structural fact that recourse channels (intervention, coercive conditionality, atrocity referral) are actively foreclosed and that emigration, the remaining exit, is restricted or lethal for exactly the populations worst off. Theater is 0.38 and rising: the formal equality of states and the uniform non-interference vocabulary increasingly mask selective application in which allies' interventions are welcomed and adversaries' sovereignty is invoked instrumentally. Accessibility_collapse is 0.55 - alternatives (R2P, ICC referral, targeted conditionality) exist on paper and partially in practice but collapse for the trapped seat once the veto-and-doctrine shield is understood. Resistance is 0.60 - the arrangement meets sustained, organized pushback from the humanitarian coalition and parts of the legal community, which is why enforcement effort keeps climbing. The measurement series run on one shared time grid (t=0..30, step 5) so every tracked metric is authored at every examined time point; the trajectories trace the post-Cold War arc: opening (t=0), the R2P settlement and its watering-down (t~15), the Libya backlash and veto-hardening (t~20), and the contemporary multipolar contest in which defending the norm requires visibly more active blocking (suppression_requirement 0.52 to 0.72). Identity-lock dynamics bind the regime seat: anti-colonial and regime-survival identity fuse with the sovereignty doctrine so that accepting any external review authority feels like existential surrender, not policy adjustment; if that frame broke, regime seats would compute far less beneficiary-positioned. On coalition potential: the trapped-population seat is individually powerless, and the same norm that traps them also blocks the transnational coalition channels (cross-border advocacy, external sponsorship) through which powerless classes otherwise build leverage - the constraint suppresses the coalition path itself.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and the engine derives that divergence from the structural data. From the authoritarian-regime seat the arrangement is a load-bearing protection it actively maintains - closer to a valued coordination structure it helps administer. From the trapped-population seat the identical structure operates as a sealed container: every escalation of internal repression finds no external counterweight, which reads as pure extraction. From the small-vulnerable-state seat the arrangement is a cheap insurance policy against predation by stronger neighbors - genuine benefit, paid for in occasional foreclosed recourse against atrocities next door. The great-power defender seat experiences the norm as discretionary influence: it decides which crises get recourse, which is a rent of its own. No single-seat verdict is authoritative; the per-seat classifications are computed, not asserted.
 *
 * DIRECTIONALITY LOGIC:
 *   Authoritarian_regimes sit nearest the beneficiary pole (d near 0.0): the constraint subsidizes them with immunity, their exit is arbitrage-grade (forum-shopping among patrons, selective invocation), and they help administer the norm. Trapped_populations_repressive_states sit nearest the target pole (d near 1.0): they bear the transfer (foreclosed recourse, unchecked repression) with trapped exit - no working internal channel, restricted emigration, no external forum. Small_vulnerable_states derive low-to-moderate d as declared beneficiaries with constrained exit: real protection received, occasional costs borne. Great_power_norm_defenders are not declared beneficiaries in the structural arrays because their gain is derivative - patronage leverage over shielded clients - but qualitatively they sit well below symmetric, nearer the beneficiary end than the target end. The humanitarian coalition is excluded rather than coordinated: its exclusion is the enforcement object. Because the constraint's spatial scope is global, verification of compliance is hard and effective extraction amplifies accordingly - the engine owns that scaling; the authored scope atoms feed it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - mutual renunciation of interference to prevent great-power war and imperial predation - remains live, so this is not a mandatrophy case and mandatrophy_resolved is deliberately not declared. The classification discipline cuts both ways here. Against the mislabel-as-snare error: the war-prevention and weak-state-protection functions are genuine, widely cited from outside the beneficiary set, and would be missed by a pure-extraction verdict. Against the mislabel-as-rope error: the extraction is not incidental overhead - it is concentrated on a seat that receives nothing back, and the arrangement's persistence depends on actively suppressing that seat's recourse and its coalition channels, which is why requires_active_enforcement is true and the tangled_rope claim, not rope, is what the structure supports. If the founding problem ever died (durable great-power peace without the shield), the arrangement would decay toward piton - ceremonial sovereignty invocations over a dead function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is ONE reading (sovereignty_maximalist_reading) of the kernel rbio_practice_norm_complex. What would the sibling readings change structurally if instantiated instead?',
    'Comparative classification across the three reading-stories of the same kernel: the liberal_institutional_reading would authorize multilateral intervention authority and shrink the victim set to non-consented targets; the hegemonic_extraction_reading would raise epsilon further and relocate beneficiaries to the hegemonic core rather than authoritarian regimes.',
    'If the liberal reading prevailed, part of the measured extraction converts into authorized recourse and the arrangement trends toward rope with a narrower victim set; if the extraction reading prevailed, the arrangement trends toward snare with the hegemonic core as capturer. The disagreement is located in whether any external intervention authority can be legitimate and who captures the shield.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: one reading of a contested kernel; sibling readings instantiate different constraints.').

omega_variable(
    humanitarian_pretext_empirical_rate,
    'What fraction of historically invoked humanitarian justifications for external intervention were pretexts for regime change versus genuine protective operations?',
    'Systematic coding of intervention cases since 1945 on stated motive, intervening-party conduct, and post-intervention outcomes, with blinding to the coding analysts'' normative commitments.',
    'A high pretext rate strengthens the reading''s core axiom and raises the justification for foreclosed recourse; a low rate undermines the axiom and shifts the measured extraction onto the reading itself as the mechanism that blocks genuine rescue.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(humanitarian_pretext_empirical_rate, empirical, 'Empirical base rate of pretextual versus genuine humanitarian justification.').

omega_variable(
    weak_state_protection_vs_regime_immunity,
    'Does the non-intervention core protect weak and vulnerable states against great-power predation enough that the arrangement''s coordination benefit is distributed broadly, or does the practical effect concentrate on regime immunity for authoritarian incumbents?',
    'Comparative analysis of instances where non-intervention norms deterred predation on weak states versus instances where the same norms shielded internal repression, weighted by affected population size.',
    'If broad protection dominates, the arrangement''s coordination function is widely shared and classification trends toward rope; if regime immunity dominates, the extraction is concentrated and classification trends toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(weak_state_protection_vs_regime_immunity, empirical, 'Distribution of the arrangement''s protective benefit across weak states versus authoritarian incumbents.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the suppression experienced by trapped populations structural (legal bars on recourse, veto-blocked channels, emigration restrictions) or internalized (learned absence of any expectation that external recourse exists)?',
    'Post-exit trajectory studies of exile and diaspora communities: if political mobilization toward external recourse revives shortly after physical exit, the suppression was predominantly structural; if exiles continue not to seek recourse, a substantial internalized component persists.',
    'If internalized, effective suppression exceeds the structural measure because the target carries the closed-horizon expectation across borders; remedies aimed only at legal channels would underperform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized mechanism of recourse suppression for trapped populations.').

omega_variable(
    conditionality_zero_exit_cost_coherence,
    'Is the reading''s own permission structure coherent: it tolerates conditionality only when the target state can exit without cost, but does a costless exit option ever actually exist for a state whose economy and elite survival are entangled with the conditioned relationship?',
    'Conceptual analysis plus case review of conditionality episodes: identify any case where the target exited the conditioned arrangement at genuinely zero cost; absence of such cases indicates the permission is vacuous.',
    'If costless exit is never real, the reading''s concession to conditionality is empty and the effective constraint is stricter than its stated terms, raising measured suppression; if real cases exist, the reading retains a genuine liberalizing edge.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(conditionality_zero_exit_cost_coherence, conceptual, 'Internal coherence of the zero-exit-cost condition on permissible conditionality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_tr_t0, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 0, 0.26).
narrative_ontology:measurement(rbio_tr_t5, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(rbio_tr_t10, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(rbio_tr_t15, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 15, 0.32).
narrative_ontology:measurement(rbio_tr_t20, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 20, 0.34).
narrative_ontology:measurement(rbio_tr_t25, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 25, 0.36).
narrative_ontology:measurement(rbio_tr_t30, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 30, 0.38).

% Extraction over time
narrative_ontology:measurement(rbio_be_t0, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(rbio_be_t5, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 5, 0.53).
narrative_ontology:measurement(rbio_be_t10, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement(rbio_be_t15, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 15, 0.59).
narrative_ontology:measurement(rbio_be_t20, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(rbio_be_t25, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 25, 0.63).
narrative_ontology:measurement(rbio_be_t30, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 30, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(rbio_su_t0, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(rbio_su_t5, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 5, 0.56).
narrative_ontology:measurement(rbio_su_t10, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(rbio_su_t15, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 15, 0.64).
narrative_ontology:measurement(rbio_su_t20, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 20, 0.67).
narrative_ontology:measurement(rbio_su_t25, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(rbio_su_t30, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rbio_practice_norm_complex__sovereignty_maximalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__sovereignty_maximalist_reading, rbio_practice_norm_complex__liberal_institutional_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__sovereignty_maximalist_reading, rbio_practice_norm_complex__hegemonic_extraction_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'the RBIO'. The natural-language concept covers at least three structurally distinct claims that cannot share one epsilon: (1) this story's absolute-sovereignty arrangement (genuine war-prevention coordination fused with extraction from trapped populations); (2) the liberal-institutional arrangement (consent-based revisable norms with authorized multilateral recourse); (3) the hegemonic-extraction arrangement (frozen hegemonic project capturing the norm complex). The upstream member with the highest empirical anchoring is the liberal-institutional reading (Charter text and doctrine), which the other two readings cite and contest; this reading and the extraction reading both position themselves against it. Every family member links the others via affects_constraints; each carries its own epsilon, beneficiaries, victims, and claimed type.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
