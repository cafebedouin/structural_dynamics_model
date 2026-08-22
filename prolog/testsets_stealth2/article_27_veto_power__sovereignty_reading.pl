% ============================================================================
% CONSTRAINT STORY: article_27_veto_power__sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_27_veto_power__sovereignty_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: article_27_veto_power__sovereignty_reading
 *   human_readable: P5 Veto as Westphalian Consent Principle (Sovereignty Reading)
 *   domain: international_relations/institutional_design/constitutional_law
 *
 * SUMMARY:
 *   This story instantiates the sovereignty reading of the P5 veto: the veto
 *   is the codification, at the enforcement apex of the international system,
 *   of the Westphalian principle that no state can be bound by international
 *   law without its consent. On this reading the constraint is not a rule
 *   imposed on anyone; it is the legal registration of a pre-existing
 *   structural fact — a state with global-reach enforcement capacity and
 *   unacceptable-cost retaliation cannot be compelled by any collective body,
 *   and every institutional attempt to build such compulsion has failed the
 *   same way (the League of Nations lost Japan, Italy, and Germany rather
 *   than restraining them; the US Senate rejected Versailles rather than join
 *   a body that could bind it). Article 27(3) therefore does not create
 *   great-power immunity; it records it. The constraint's operation is
 *   self-executing: it requires no enforcement machinery, meets no meaningful
 *   structural resistance, and collects no rents — what the permanent five
 *   'receive' is the retention of an autonomy they would possess in the
 *   veto's absence. Time mapping: t=0 is 1945 (San Francisco), t=80 is 2025.
 *   The claim/metric gap is deliberate: the constraint is CLAIMED as mountain
 *   (structural inevitability) while the metrics are authored descriptively,
 *   including a theater ratio that has risen as ritual veto-condemnation
 *   machinery has grown around a functional core. KEY AGENTS (by structural
 *   relationship): - permanent_five_members: agenda-setting seat
 *   (institutional/arbitrage) — hold and administer the veto; their consent
 *   gates Charter revision - elected_council_members: formally present in the
 *   Council, structurally nullified on any P5-blocked question
 *   (organized/constrained) - general_un_membership: the ratifying majority,
 *   with no path into the revision conversation (organized/trapped) -
 *   sanctioned_small_states: bear the Council's enforceable side, which
 *   passes where P5 interests are absent (powerless/trapped) -
 *   g4_aspiring_permanent_members: excluded from the veto club; the visible
 *   distributive face of the arrangement (powerful/constrained) -
 *   reform_coalitions_act_group: organized restraint campaigns that cannot
 *   touch Article 27 itself (organized/trapped) -
 *   international_legal_scholars: analytical observer — document doctrine and
 *   practice drift (analytical/analytical)
 *
 * KEY AGENTS:
 *   - permanent_five_members: agenda_setter (institutional/arbitrage) — wield the veto; their ratification is the effective amendment gate
 *   - elected_council_members: excluded (organized/constrained) — vote and draft but cannot overcome a veto
 *   - general_un_membership: excluded (organized/trapped) — consented collectively at ratification; no individual or collective path to revision
 *   - sanctioned_small_states: excluded (powerless/trapped) — absorb the Council's enforceable coercion, which flows where P5 interests are absent
 *   - g4_aspiring_permanent_members: excluded (powerful/constrained) — campaign for seats inside an arrangement only insiders can amend
 *   - reform_coalitions_act_group: excluded (organized/trapped) — pursue restraint codes and GA scrutiny that bypass but never amend Article 27
 *   - international_legal_scholars: observer (analytical/analytical) — trace doctrine from Namibia onward
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__sovereignty_reading, 0.08).
domain_priors:suppression_score(article_27_veto_power__sovereignty_reading, 0.1).
domain_priors:theater_ratio(article_27_veto_power__sovereignty_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__sovereignty_reading, mountain).
narrative_ontology:human_readable(article_27_veto_power__sovereignty_reading, "P5 Veto as Westphalian Consent Principle (Sovereignty Reading)").
narrative_ontology:topic_domain(article_27_veto_power__sovereignty_reading, "international_relations/institutional_design/constitutional_law").

domain_priors:emerges_naturally(article_27_veto_power__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__sovereignty_reading, '06426e0c-8795-488a-bb34-96b5d6c6384a').
narrative_ontology:cs_kernel_codification('06426e0c-8795-488a-bb34-96b5d6c6384a', fixed_text).
narrative_ontology:cs_authority_grounding('06426e0c-8795-488a-bb34-96b5d6c6384a', lineage).
narrative_ontology:cs_interpretation_layer_present('06426e0c-8795-488a-bb34-96b5d6c6384a').
narrative_ontology:cs_reading_relation('06426e0c-8795-488a-bb34-96b5d6c6384a', article_27_veto_power__coordination_reading, influences).
narrative_ontology:cs_reading_relation('06426e0c-8795-488a-bb34-96b5d6c6384a', article_27_veto_power__oligopoly_reading, coexists_with).
narrative_ontology:cs_axiom('06426e0c-8795-488a-bb34-96b5d6c6384a', foundational, consent_as_source_of_legal_obligation).
narrative_ontology:cs_axiom_status(consent_as_source_of_legal_obligation, holdable).
narrative_ontology:cs_axiom_grounding('06426e0c-8795-488a-bb34-96b5d6c6384a', consent_as_source_of_legal_obligation, conventional).
narrative_ontology:cs_axiom('06426e0c-8795-488a-bb34-96b5d6c6384a', foundational, war_peace_prerogative_non_delegable).
narrative_ontology:cs_axiom_status(war_peace_prerogative_non_delegable, holdable).
narrative_ontology:cs_axiom_grounding('06426e0c-8795-488a-bb34-96b5d6c6384a', war_peace_prerogative_non_delegable, deontological).
narrative_ontology:cs_reference_frame('06426e0c-8795-488a-bb34-96b5d6c6384a', westphalian_consent_sovereignty).
narrative_ontology:cs_drift_state('06426e0c-8795-488a-bb34-96b5d6c6384a', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('06426e0c-8795-488a-bb34-96b5d6c6384a', '').
narrative_ontology:cs_kernel_id(article_27_veto_power__sovereignty_reading, article_27_veto_power).

% --- Structural relationships ---
narrative_ontology:constraint_vindicates(article_27_veto_power__sovereignty_reading, westphalian_consent_principle).
narrative_ontology:constraint_vindicates(article_27_veto_power__sovereignty_reading, great_power_unanimity_requirement).
narrative_ontology:constraint_vindicates(article_27_veto_power__sovereignty_reading, unenforceable_command_credibility_cost).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the veto and administer the Council's agenda, including blocking agenda items outright. They experience the veto not as a privilege conferred but as the legal recognition of a capacity they would retain in its absence: no resolution can direct force or binding obligation against them, and their ratification is the effective gate on any Charter amendment. Their exit options are effectively unlimited — they can withdraw, disregard, or rebuild the institution, as the League's departing members demonstrated.
narrative_ontology:constraint_stakeholder(article_27_veto_power__sovereignty_reading, permanent_five_members, agenda_setter,
    institutional, generational, arbitrage, global).

% Serve two-year terms, draft resolutions, and vote, but cannot overcome a veto and increasingly negotiate texts through informal pre-clearance with the permanent five before formal debate begins. They are present in the room and structurally nullified on any question a permanent member blocks; their formal participation is the visible surface of decisions made elsewhere.
narrative_ontology:constraint_stakeholder(article_27_veto_power__sovereignty_reading, elected_council_members, excluded,
    organized, biographical, constrained, global).

% Consented collectively to the Charter at ratification and periodically objects to specific vetoes through General Assembly votes, but has no path into the revision conversation: Article 108 amendment requires the concurrence of every permanent member's domestic ratification process. They cannot exit the international system, and their collective organs can condemn the veto but not touch it.
narrative_ontology:constraint_stakeholder(article_27_veto_power__sovereignty_reading, general_un_membership, excluded,
    organized, generational, trapped, global).

% Bear the Council's enforceable side — sanctions regimes, arms embargoes, ad hoc tribunals — which passes routinely where permanent-member interests are absent and stalls wherever they are engaged. Their exposure to binding collective coercion is the mirror image of permanent-member immunity, and their claim that the consent principle should shield them equally has no procedural address.
narrative_ontology:constraint_stakeholder(article_27_veto_power__sovereignty_reading, sanctioned_small_states, excluded,
    powerless, biographical, trapped, regional).

% Major regional powers campaigning for permanent seats and veto rights, or for constraints on the existing veto. They operate inside the Council game they cannot change: enlargement requires the same unanimous permanent-member consent that the arrangement reserves to incumbents, so their candidacy is hostage to the structure they seek to join or reform.
narrative_ontology:constraint_stakeholder(article_27_veto_power__sovereignty_reading, g4_aspiring_permanent_members, excluded,
    powerful, generational, constrained, global).

% Cross-regional coalitions (the ACT group, the French-Mexican restraint initiative, the Liechtenstein veto-initiative campaign) pursuing codes of conduct and Assembly scrutiny of veto use. Their instruments deliberately bypass Article 27 because amending it requires the consent of the states whose conduct they seek to constrain; they accumulate signatures and moral pressure with no formal lever.
narrative_ontology:constraint_stakeholder(article_27_veto_power__sovereignty_reading, reform_coalitions_act_group, excluded,
    organized, generational, trapped, global).

% Trace the veto's doctrinal basis from the San Francisco negotiations through the ICJ's Namibia advisory opinion establishing that abstention does not defeat adoption, and document practice drift such as penholder arrangements and pre-clearance norms. They hold no seat in the decision and collect nothing from the arrangement; their product is the record against which any structural claim about the veto must be checked.
narrative_ontology:constraint_stakeholder(article_27_veto_power__sovereignty_reading, international_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_27_veto_power__sovereignty_reading, diffuse).
narrative_ontology:fixing_cost_class(article_27_veto_power__sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns the Security Council's formal outputs with what will actually be complied with: by guaranteeing that no resolution can direct binding action against a permanent member's will, the veto keeps the Council from issuing commands it lacks the capacity to enforce, preserving the institution's credibility and keeping the great powers inside it rather than driving them out as the League's unanimity rule drove theirs out.
% TRANSFER_FUNCTION: Moves nothing. The arrangement blocks the transfer of coercive-decision authority from national capitals to the collective body: each permanent member retains final say over its own war, peace, and sanctions exposure, and no money, work, or obligation flows from any state to any other by virtue of the veto itself.
% ABSENT_VOICES: The general membership, small states under Council sanctions, aspiring permanent members, and organized reform coalitions would all object to the veto's unequal application of the consent principle, and all are structurally outside the conversation that matters: Charter revision runs through Article 108, which requires the concurrence of the very states the objection targets. Their voices exist loudly in the Assembly and are procedurally weightless at the amendment gate.
% DISAPPEARANCE_RATIONALE: If Article 27(3) vanished overnight, the underlying power distribution would not move with it: adverse resolutions against a great power would be defied as they were under the League, compliance in the Council has always been voluntary, and the institution would either revert to informal unanimity practice within months or destroy its own credibility issuing unenforceable commands. The formal deletion changes the legal surface, not the enforcement arithmetic — which is precisely the reading's claim that the veto registers a fact rather than producing one.
% FOUNDING_PROBLEM: After the League of Nations failed — paralyzed by unanimity, then abandoned by Japan, Italy, and Germany rather than obeyed — the Charter's designers faced the problem of building a security institution that the great powers would join and stay inside, given that none would accept being bound against its will. The veto at Dumbarton Oaks and Yalta was the answer: guarantee each great power that the new Council could never direct force or binding obligation against it, and it would lend the institution its power instead of exiting it.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the permanent five: the diplomatic record of the San Francisco conference and histories of the Charter's drafting show the founders stating the problem and the solution in exactly these terms; the League's collapse sequence (Japanese withdrawal after the Lytton Report, Italian defiance of Abyssinia sanctions, German exit) is attested by diplomatic historians independent of any P5 government; the US Senate's rejection of the Versailles Treaty independently demonstrates that no great-power polity would ratify an institution empowered to bind it; and the observed pattern of great-power non-compliance with adverse ICJ judgments and Assembly resolutions corroborates the underlying uncompellability claim without reference to the veto at all.
narrative_ontology:disappearance_verdict(article_27_veto_power__sovereignty_reading, world_unchanged).
narrative_ontology:founding_problem_status(article_27_veto_power__sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_27_veto_power__sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_27_veto_power__sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_27_veto_power__sovereignty_reading, 0.08, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_27_veto_power__sovereignty_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, ExtMetricName, E),
    domain_priors:suppression_score(article_27_veto_power__sovereignty_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(article_27_veto_power__sovereignty_reading),
    narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(article_27_veto_power__sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored near-zero (0.08) because the sovereignty reading's referent is the standing arrangement assessed by its own lights: the veto takes nothing from anyone — it declines to compel, and the reading holds that the declined compulsion was never available. The gentle upward drift in the series (0.05 to 0.08) prices a modest, rising opportunity cost borne by the general membership as the Council's formal authority diverges from its enforceable scope (blocked action in Rwanda 1994, Syria, Ukraine, Gaza), which the reading values lightly because the blocked actions were unenforceable in any case. Suppression is low (0.10) and is authored as a raw structural property, unscaled by power or scope: nothing enforces the veto; alternatives such as General Assembly uniting-for-peace resolutions and ICJ opinions exist unsuppressed and simply lack teeth naturally. Accessibility collapse is high (0.90): once the enforcement-capacity arithmetic is understood, alternative designs collapse almost completely — the League precedent forecloses the obvious rivals. Resistance is low (0.15): normative objection is abundant and loud, but structural resistance is minimal because objectors overwhelmingly concede the underlying arithmetic even while deploring specific vetoes. Theater ratio (0.30 at interval end) tracks the growth of a ritual layer — annual GA veto debates, the ACT code of conduct, the Franco-Mexican initiative, the Liechtenstein veto-initiative resolution — around a functional core of pre-clearance and agenda control; it rises monotonically as open Council deliberation increasingly performs debate over outcomes already fixed by P5 positions. Both metric series run on one shared nine-point grid so every metric is authored at every examined time point; no suppression_requirement series is authored because the veto's enforcement picture is static by nature — it is self-executing, with no enforcement capacity to build up or decay.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setting seat and the membership seats should compute differently. From the permanent-five position the veto is not a grant but a recognition: the arrangement merely declines to paper over a capacity they would retain regardless, so the seat experiences the constraint as congruence between legal form and material fact, with effective extraction near the coordination floor. From the general-membership and small-state seats the same instrument reads as unequal application of a principle those states are told governs everyone: they are bound by Council decisions passed over their individual dissent while five states are not, and the sanction of that inequality lands hardest on the weakest. The engine computes this per-seat divergence from the structural data; the authored mountain claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries or victims are declared, because the sovereignty reading holds that no actor extracts and no actor is extracted from: the veto's effect on the permanent five is retention of a pre-existing autonomy, not a subsidy, and its effect on the rest is the absence of a compulsion that was never available. With no beneficiary/victim data, the derivation chain would fall to canonical per-power-atom fallbacks that carry no information about this reading's symmetry claim, so explicit overrides encode it: institutional (the permanent five) at d=0.50 — the constraint constitutes their position rather than taxing or subsidizing it; moderate (general membership) at d=0.55 — marginally target-side insofar as the Council's blocked capacity would nominally have been theirs to use; powerless (small states exposed to Council sanctions) at d=0.60 — the veto-shaped Council channels enforceable coercion toward the weak and away from the strong, placing them somewhat target-side even under this reading. All values sit near the symmetric midpoint, keeping aggregate effective extraction near the coordination floor, which is the quantitative expression of the reading's near-zero epsilon claim.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — building a security institution that great powers will join and remain inside, given that they will not accept being bound against their will — is live under this reading, so no mandate atrophy is declared: the arrangement has not outlived its function because its function is coextensive with a permanent feature of the power distribution. The classification discipline cuts both ways. Against the snare mislabel: locating the veto's persistence in structural necessity rather than rent defense prevents the oligopoly-style reading of every P5-blocked decision as theft, and the near-zero epsilon with sub-floor Boltzmann extraction marks the arrangement's costs as coordination cost rather than extractive overhead. Against the rope mislabel: the high accessibility collapse and negligible resistance distinguish this from a mere coordination convention that participants could renegotiate — the constraint is not held up by agreement but by arithmetic. The omega variables carry the residual risk in both directions: if the counterfactual institutional analysis finds a viable compulsion design, the same structural data flips to entrenchment, and the mountain claim was the cover story.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_entrenchment,
    'Is the P5 veto a genuine structural inevitability — the codification of the fact that no collective body can compel states with global-reach enforcement capacity — or a constructed legal entrenchment whose persistence serves identifiable state interests?',
    'Counterfactual institutional analysis: test whether any design lacking the veto (League-style unanimity with exit rights, qualified majority with withdrawal clauses, ad hoc enforcement coalitions) has ever sustained compulsion of a great power against its will; if a viable design exists or can be specified, the veto is constructed rather than necessary.',
    'If constructed, the constraint reclassifies toward tangled_rope or snare with identifiable beneficiaries (the permanent five as holders of a legal entitlement exceeding their bare capacity) and victims (the general membership denied majoritarian recourse); if genuine, it certifies as mountain and reform efforts aimed at Article 27 are misdirected at the codification rather than the structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_entrenchment, conceptual, 'Whether the veto reflects enforcement-capacity necessity or legal construction serving P5 interests.').

omega_variable(
    kernel_reading_committer_structure,
    'This constraint instantiates the sovereignty_reading of kernel article_27_veto_power; how would classification shift under the sibling readings (coordination_reading, oligopoly_reading), and where exactly is the disagreement located?',
    'Cross-reading structural comparison: the coordination_reading locates the veto''s function in great-power war prevention (introducing a universal beneficiary structure); the oligopoly_reading locates it in authority-rent extraction via Charter immutability (introducing a victim structure: the general membership); the disagreement is located precisely in whether the consent principle''s application to asymmetric powers is a neutral registration of enforcement capacity or a distributive choice converting incapacity into legal entitlement.',
    'Under coordination_reading, epsilon rises above zero (all states pay a coordination premium for guaranteed P5 participation) while the type may remain mountain-like; under oligopoly_reading, epsilon becomes substantially positive with the permanent five as beneficiaries and the general membership as victims, reclassifying toward snare or tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one of three readings of the Article 27 kernel; siblings alter the beneficiary/victim structure this reading declares absent.').

omega_variable(
    enforcement_capacity_physical_basis,
    'Does the impossibility of compelling great powers rest on physical facts (nuclear weapons making defeat unacceptable, global reach making isolation ineffective) or on political facts (domestic ratification constraints, alliance structures) that could change?',
    'Track whether technological or political change alters compulsion feasibility: nuclear abolition scenarios, strategic defense maturation, fragmentation of great-power domestic consensus on force projection, or demonstrated willingness of a great power to accept adverse collective enforcement.',
    'If the physical basis erodes, the veto''s mountain character decays and the arrangement becomes a legacy convention on a piton trajectory; if robust, the mountain classification is stable across any institutional redesign.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_capacity_physical_basis, empirical, 'Physical versus political basis of great-power uncompellability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__sovereignty_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(veto_sovereignty_reading_tr_t0, article_27_veto_power__sovereignty_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(veto_sovereignty_reading_tr_t0, observed).
narrative_ontology:measurement(veto_sovereignty_reading_tr_t10, article_27_veto_power__sovereignty_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement_basis(veto_sovereignty_reading_tr_t10, observed).
narrative_ontology:measurement(veto_sovereignty_reading_tr_t20, article_27_veto_power__sovereignty_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement_basis(veto_sovereignty_reading_tr_t20, observed).
narrative_ontology:measurement(veto_sovereignty_reading_tr_t30, article_27_veto_power__sovereignty_reading, theater_ratio, 30, 0.14).
narrative_ontology:measurement_basis(veto_sovereignty_reading_tr_t30, observed).
narrative_ontology:measurement(veto_sovereignty_reading_tr_t40, article_27_veto_power__sovereignty_reading, theater_ratio, 40, 0.16).
narrative_ontology:measurement_basis(veto_sovereignty_reading_tr_t40, observed).
narrative_ontology:measurement(veto_sovereignty_reading_tr_t50, article_27_veto_power__sovereignty_reading, theater_ratio, 50, 0.2).
narrative_ontology:measurement_basis(veto_sovereignty_reading_tr_t50, observed).
narrative_ontology:measurement(veto_sovereignty_reading_tr_t60, article_27_veto_power__sovereignty_reading, theater_ratio, 60, 0.24).
narrative_ontology:measurement_basis(veto_sovereignty_reading_tr_t60, observed).
narrative_ontology:measurement(veto_sovereignty_reading_tr_t70, article_27_veto_power__sovereignty_reading, theater_ratio, 70, 0.27).
narrative_ontology:measurement_basis(veto_sovereignty_reading_tr_t70, observed).
narrative_ontology:measurement(veto_sovereignty_reading_tr_t80, article_27_veto_power__sovereignty_reading, theater_ratio, 80, 0.3).
narrative_ontology:measurement_basis(veto_sovereignty_reading_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(veto_sovereignty_reading_be_t0, article_27_veto_power__sovereignty_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement_basis(veto_sovereignty_reading_be_t0, observed).
narrative_ontology:measurement(veto_sovereignty_reading_be_t10, article_27_veto_power__sovereignty_reading, base_extractiveness, 10, 0.05).
narrative_ontology:measurement_basis(veto_sovereignty_reading_be_t10, observed).
narrative_ontology:measurement(veto_sovereignty_reading_be_t20, article_27_veto_power__sovereignty_reading, base_extractiveness, 20, 0.06).
narrative_ontology:measurement_basis(veto_sovereignty_reading_be_t20, observed).
narrative_ontology:measurement(veto_sovereignty_reading_be_t30, article_27_veto_power__sovereignty_reading, base_extractiveness, 30, 0.06).
narrative_ontology:measurement_basis(veto_sovereignty_reading_be_t30, observed).
narrative_ontology:measurement(veto_sovereignty_reading_be_t40, article_27_veto_power__sovereignty_reading, base_extractiveness, 40, 0.07).
narrative_ontology:measurement_basis(veto_sovereignty_reading_be_t40, observed).
narrative_ontology:measurement(veto_sovereignty_reading_be_t50, article_27_veto_power__sovereignty_reading, base_extractiveness, 50, 0.07).
narrative_ontology:measurement_basis(veto_sovereignty_reading_be_t50, observed).
narrative_ontology:measurement(veto_sovereignty_reading_be_t60, article_27_veto_power__sovereignty_reading, base_extractiveness, 60, 0.08).
narrative_ontology:measurement_basis(veto_sovereignty_reading_be_t60, observed).
narrative_ontology:measurement(veto_sovereignty_reading_be_t70, article_27_veto_power__sovereignty_reading, base_extractiveness, 70, 0.08).
narrative_ontology:measurement_basis(veto_sovereignty_reading_be_t70, observed).
narrative_ontology:measurement(veto_sovereignty_reading_be_t80, article_27_veto_power__sovereignty_reading, base_extractiveness, 80, 0.08).
narrative_ontology:measurement_basis(veto_sovereignty_reading_be_t80, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(article_27_veto_power__sovereignty_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_27_veto_power__sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_27_veto_power__sovereignty_reading, article_27_veto_power__coordination_reading).
narrative_ontology:affects_constraint(article_27_veto_power__sovereignty_reading, article_27_veto_power__oligopoly_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the P5 veto' decomposes into three structurally distinct claims per the epsilon-invariance principle. This story (sovereignty_reading) authors the veto as neutral instantiation of the consent principle: epsilon near-zero, no beneficiary/victim structure, mountain. The sibling coordination_reading authors it as a war-prevention mechanism with a universal beneficiary structure and a positive coordination premium. The sibling oligopoly_reading authors it as entrenchment of a geopolitical oligopoly with the permanent five as beneficiaries, the general membership as victims, and substantially positive epsilon. The stories are linked pairwise through affects_constraints; the upstream sovereignty claim (highest empirical confidence — the League record) is cited as evidence by the coordination reading and contested as cover by the oligopoly reading, so this story sits upstream of both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_27_veto_power__sovereignty_reading, institutional, 0.5).
constraint_indexing:directionality_override(article_27_veto_power__sovereignty_reading, moderate, 0.55).
constraint_indexing:directionality_override(article_27_veto_power__sovereignty_reading, powerless, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
