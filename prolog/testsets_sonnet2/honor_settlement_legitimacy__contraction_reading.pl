% ============================================================================
% CONSTRAINT STORY: honor_settlement_legitimacy__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_settlement_legitimacy__contraction_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: honor_settlement_legitimacy__contraction_reading
 *   human_readable: Honor Settlement Legitimacy — Contraction Reading (Dueling as Cognitively Unthinkable)
 *   domain: historical_sociology/legal_history/cultural_anthropology
 *
 * SUMMARY:
 *   This story instantiates the CONTRACTION reading of the
 *   honor_settlement_legitimacy kernel: the historical decline of dueling is
 *   read not as a policy suppressed by law and enforcement (drop_reading —
 *   honor culture persists at the margins) and not as a multi-causal
 *   overdetermined decline with a residual honor-edge (composite_reading),
 *   but as a genuine cultural-framework transformation in which
 *   honor-as-legitimate-grounds-for-lethal-combat exits the space of
 *   intelligible options altogether. On this reading, by the end of the
 *   interval, dueling is not merely illegal and suppressed but literally
 *   incomprehensible as a legitimate response to insult in the same way trial
 *   by combat is incomprehensible to a modern litigant. The measurement grid
 *   shows suppression_requirement falling sharply and then flattening (0.70
 *   -> 0.35) precisely because active suppression apparatus becomes
 *   progressively unnecessary once the framework itself has exited
 *   circulation — this is the signature this reading stakes its claim on,
 *   distinguishing it from drop_reading (which would show suppression
 *   persisting because the practice persists at the margins) and from
 *   composite_reading (which would show a shallower, noisier decline
 *   reflecting multiple simultaneous causal mechanisms).
 *
 * KEY AGENTS:
 *   - professional_class_reputational_regime: institutional beneficiary of the new reputation-management framework
 *   - bureaucratic_state_monopoly_on_violence: institutional agenda-setter administering the replacement legal/military codes
 *   - commercial_credit_economy_participants: organized beneficiary group requiring predictable non-lethal dispute resolution
 *   - former_dueling_class_aristocracy_and_officer_corps: powerful but trapped payer whose status-mechanism disappears
 *   - residual_honor_culture_adherents: excluded voice the contraction reading holds has no institutionally legible standing left
 *   - legal_and_cultural_historians: analytical observers adjudicating among the three kernel readings from primary sources
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__contraction_reading, 0.28).
domain_priors:suppression_score(honor_settlement_legitimacy__contraction_reading, 0.35).
domain_priors:theater_ratio(honor_settlement_legitimacy__contraction_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__contraction_reading, mountain).
narrative_ontology:human_readable(honor_settlement_legitimacy__contraction_reading, "Honor Settlement Legitimacy — Contraction Reading (Dueling as Cognitively Unthinkable)").
narrative_ontology:topic_domain(honor_settlement_legitimacy__contraction_reading, "historical_sociology/legal_history/cultural_anthropology").

domain_priors:emerges_naturally(honor_settlement_legitimacy__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__contraction_reading, '40166097-60ea-43f9-a70b-4df12f826e0b').
narrative_ontology:cs_kernel_codification('40166097-60ea-43f9-a70b-4df12f826e0b', implicit).
narrative_ontology:cs_authority_grounding('40166097-60ea-43f9-a70b-4df12f826e0b', practice).
narrative_ontology:cs_interpretation_layer_present('40166097-60ea-43f9-a70b-4df12f826e0b').
narrative_ontology:cs_reading_relation('40166097-60ea-43f9-a70b-4df12f826e0b', honor_settlement_legitimacy__drop_reading, forecloses).
narrative_ontology:cs_reading_relation('40166097-60ea-43f9-a70b-4df12f826e0b', honor_settlement_legitimacy__composite_reading, influences).
narrative_ontology:cs_axiom('40166097-60ea-43f9-a70b-4df12f826e0b', foundational, honor_violence_categorically_unavailable_post_transition).
narrative_ontology:cs_axiom_status(honor_violence_categorically_unavailable_post_transition, holdable).
narrative_ontology:cs_axiom_grounding('40166097-60ea-43f9-a70b-4df12f826e0b', honor_violence_categorically_unavailable_post_transition, empirically_contingent).
narrative_ontology:cs_axiom('40166097-60ea-43f9-a70b-4df12f826e0b', secondary, reputational_injury_repairable_only_through_institutional_channels).
narrative_ontology:cs_axiom_status(reputational_injury_repairable_only_through_institutional_channels, holdable).
narrative_ontology:cs_axiom_grounding('40166097-60ea-43f9-a70b-4df12f826e0b', reputational_injury_repairable_only_through_institutional_channels, conventional).
narrative_ontology:cs_reference_frame('40166097-60ea-43f9-a70b-4df12f826e0b', aristocratic_honor_code_as_legitimate_dispute_resolution).
narrative_ontology:cs_drift_state('40166097-60ea-43f9-a70b-4df12f826e0b', post_transition_bureaucratic_legal_order, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('40166097-60ea-43f9-a70b-4df12f826e0b', '').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__contraction_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__contraction_reading, professional_class_reputational_regime).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__contraction_reading, bureaucratic_state_monopoly_on_violence).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__contraction_reading, commercial_credit_economy_participants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__contraction_reading, former_dueling_class_aristocracy_and_officer_corps).
narrative_ontology:constraint_vindicates(honor_settlement_legitimacy__contraction_reading, state_monopoly_on_legitimate_violence_doctrine).
narrative_ontology:constraint_vindicates(honor_settlement_legitimacy__contraction_reading, reputation_as_administrable_property_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The emergent professional and commercial middle class whose status depends on credit rating, bureaucratic office, and public reputation rather than blood honor. This class's entire status-production apparatus (credentialing, credit bureaus, professional societies) required that reputational injury be repairable through non-violent institutional means. Under the contraction reading, this class does not merely win a policy fight against dueling — the concept of settling an insult by combat becomes as unavailable to this class as trial by ordeal, not a suppressed option but a non-option.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, professional_class_reputational_regime, beneficiary,
    institutional, generational, analytical, national).

% The consolidating nation-state apparatus (courts, police, army officer corps under central command) whose Weberian claim to sole legitimate violence is incompatible with private combat as a recognized dispute-resolution channel. Administers criminal codes, military discipline codes, and civil libel law that replace the duel's function. Under this reading, the state did not simply criminalize an ongoing practice — the practice ceased to register as an intelligible institution to reason about at all, which is a stronger and more totalizing outcome than mere enforcement.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, bureaucratic_state_monopoly_on_violence, agenda_setter,
    institutional, civilizational, analytical, national).

% Merchants, bankers, and tradesmen whose economic life depends on predictable, survivable business relationships. A dueling culture that could remove a creditor or debtor from the world over a perceived slight was structurally incompatible with impersonal contract-based commerce. This group benefits from honor's exit from the normative field because it removes a standing risk to capital and personnel, not merely a nuisance.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, commercial_credit_economy_participants, beneficiary,
    organized, generational, analytical, national).

% The aristocratic and military-officer stratum for whom the duel had been the primary mechanism of honor vindication and status maintenance. Under the contraction reading, this class does not experience a mere loss of a legal option — they experience the disappearance of the entire cognitive category through which their forebears' conduct was intelligible as honorable. Their grandfathers' duels become retrospectively unintelligible rather than merely illegal, which forecloses even nostalgic or fringe revival as a coherent project.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, former_dueling_class_aristocracy_and_officer_corps, payer,
    powerful, biographical, trapped, national).

% Individuals and pockets of society (rural gentry, certain military subcultures, immigrant enclaves carrying transplanted codes) who might wish to continue treating dueling as a legitimate response to insult. On the contraction reading their position is not merely marginalized but rendered structurally unspeakable in mainstream discourse — they are excluded from the conversation because the conversation's terms no longer contain the category they would need to make their case. They are named here to register their absence, not to credit their continued practice, which the drop_reading (a sibling, not this constraint) would instead affirm.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, residual_honor_culture_adherents, excluded,
    moderate, biographical, trapped, regional).

% Scholars examining court records, dueling codes, newspaper commentary, and military tribunal proceedings across the transition period. They adjudicate among the contraction, drop, and composite readings by examining whether post-transition sources treat dueling as a suppressed-but-comprehensible option or as an incoherent category error — the empirical test this reading stakes itself on.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, legal_and_cultural_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_settlement_legitimacy__contraction_reading, diffuse).
narrative_ontology:fixing_cost_class(honor_settlement_legitimacy__contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None, on this reading — there is no coordination problem being solved by 'honor culture no longer existing as a category.' The prior dueling system did solve a coordination problem (credible commitment to defend reputation without state adjudication), but the contraction reading holds that the REPLACEMENT arrangement is not honor-culture-with-a-patch; it is a genuinely different normative framework (bureaucratic/legal reputation management) that makes the old coordination problem itself no longer arise in its original form.
% TRANSFER_FUNCTION: Nothing is transferred from a victim to a beneficiary in the ordinary extraction sense on this reading — the honor-culture normative framework does not persist anywhere to be extracted from; it exits the space of live options entirely, the way trial-by-combat is not 'suppressed' in a modern legal system but literally outside its adjudicatory imagination. To the extent anything is 'transferred,' it is status-production capacity moving from blood-honor mechanisms to credentialing/legal mechanisms, benefiting the professional and commercial classes.
% ABSENT_VOICES: Descendants and cultural sympathizers of the aristocratic dueling class, and any residual honor-culture adherents, would object that this reading overclaims — that honor logic persisted longer and more coherently than 'cognitive unthinkability' allows. They are structurally absent from mainstream legal-historical discourse because the contraction reading's own thesis is that their framework is no longer available as a lens through which even they can articulate a rebuttal in institutionally legible terms.
% DISAPPEARANCE_RATIONALE: Because this reading holds that honor-culture-as-normative-option has already fully exited the space, there is nothing left for 'the constraint' to disappear FROM in the present tense — the modern world is already the world without it. The disappearance question is retrospective: if the transformation had NOT occurred, dueling would still register as a live, comprehensible option in disputes among status-conscious elites, and courts/military codes would need active suppression machinery rather than simple non-recognition. Under the actual (contraction) state of affairs, removing residual legal prohibitions on dueling today would change essentially nothing, because no one holds the cognitive framework needed to act on the option even if it were legalized.
% FOUNDING_PROBLEM: Elite reputation management and credible commitment to defend one's word or standing without recourse to a (weak or absent) centralized judicial authority, in a social order where personal honor was inseparable from political and economic standing.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians examining post-transition court and military tribunal records (an outside-the-beneficiary-class source) attest that by the late 19th/early 20th century in the jurisdictions studied, dueling references in institutional documents shift from 'crime to be punished' to 'anachronism requiring explanation,' consistent with the categorical-exit claim rather than mere successful suppression. No advocate for the former dueling class is available to corroborate persistence of the framework in institutionally legible terms, which the contraction reading treats as itself evidence for its thesis, though a partisan of the drop_reading would dispute that absence-of-legible-advocacy equals absence-of-the-framework.
narrative_ontology:disappearance_verdict(honor_settlement_legitimacy__contraction_reading, world_unchanged).
narrative_ontology:founding_problem_status(honor_settlement_legitimacy__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_settlement_legitimacy__contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_settlement_legitimacy__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_settlement_legitimacy__contraction_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_settlement_legitimacy__contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(honor_settlement_legitimacy__contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(honor_settlement_legitimacy__contraction_reading),
    narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(honor_settlement_legitimacy__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate-low (0.28 at interval end) and falling, because on the contraction reading there is decreasingly any active party extracting benefit from suppressing a live alternative — the alternative itself stops being live, so what looks like extraction (state power crushing an aristocratic practice) resolves instead into something closer to a mountain-like disappearance: the old framework is not being defeated repeatedly, it has stopped needing to be defeated. Accessibility_collapse is authored very high (0.88) precisely because this reading's defining claim is near-total collapse of the honor-dueling alternative as a cognitively available option, not merely a legally blocked one. Resistance is authored low (0.10) for the same reason: a category that has become unthinkable does not generate ongoing resistance from those it constrains, because there is no one left holding the framework as a felt loss requiring active resistance — this is the sharpest structural distinguishing feature from the drop_reading, which would author resistance as ongoing (residual adherents actively resisting suppression).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (professional class, state, commercial economy) are coded near the low-directionality/subsidized end because the transformation directly enables their status and economic mechanisms to function without the standing risk of lethal reputational combat. The former dueling class is coded as payer/trapped because their entire status-vindication mechanism vanishes with no substitute available to them in their own terms — but note the extraction they 'suffer' is not being actively imposed by an ongoing enforcement apparatus by the end of the interval (suppression_requirement falls to 0.35 and flattens); it is a structural loss following a completed cognitive transformation, which is why this constraint reads closer to mountain than to snare or tangled_rope despite naming beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (credible reputational defense absent strong central judicial authority) is authored as dead, and disappearance_verdict is world_unchanged, precisely because on this reading there is no live mandate left to have outlived its function — the mandate itself dissolved along with the cognitive category, rather than persisting as an empty institutional shell (which would be the piton pattern) or persisting as active suppression of a live alternative (which would be tangled_rope or snare). This is what distinguishes contraction from mere successful policy suppression: a piton or snare reading would require the old framework to still exist somewhere to be suppressed or performed; contraction denies that residuum exists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cognitive_unthinkability_vs_successful_suppression,
    'Is the observed disappearance of dueling from institutional records evidence that the honor-dueling framework became genuinely cognitively unavailable (contraction_reading), or evidence merely that suppression was successful enough that adherents stopped leaving a documentary trace (drop_reading)?',
    'Close reading of private correspondence, memoirs, and regional/subcultural records (military messes, immigrant enclaves, rural gentry) from the late transition period for evidence that honor logic remained articulable in first-person terms, even if not practiced or publicly defensible. Persistent private articulability would favor drop_reading; near-total absence even in private records would favor contraction_reading.',
    'If drop_reading is empirically favored, this constraint''s claimed_type and metrics (very high accessibility_collapse, very low resistance) would be substantially overclaimed, and the honor-dueling framework should be modeled as a suppressed-but-surviving snare/tangled_rope rather than a mountain-like categorical exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_unthinkability_vs_successful_suppression, empirical, 'Whether dueling''s decline reflects genuine cognitive-framework exit or merely effective long-term suppression leaving no trace.').

omega_variable(
    honor_culture_natural_vs_constructed_ambiguity,
    'Is treating the post-transformation reputational order as a mountain (naturally emergent, near-total accessibility collapse) itself a false summit — i.e., does the professional/commercial/bureaucratic beneficiary class have an interest in narrating the transformation as complete and natural, when a residual honor-logic substrate in fact persists and could be reactivated under different structural conditions (e.g., state collapse, frontier settings, organized-crime contexts where duels-in-substance recur)?',
    'Comparative examination of contexts where state monopoly on violence weakens (failed states, prison systems, organized crime) for reversion to duel-like honor combat; recurrence would suggest the framework is latent and situationally suppressed rather than genuinely extinct, supporting the FSM concern.',
    'If honor-logic reliably re-emerges whenever state/commercial structural supports weaken, the contraction reading''s mountain-like claim (categorical exit) is a false summit masking an ongoing, structurally-dependent suppression that benefits the professional/commercial/state classes named as beneficiaries here — reclassification toward tangled_rope would follow.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(honor_culture_natural_vs_constructed_ambiguity, conceptual, 'Whether the apparent naturalness/completeness of honor culture''s exit is itself contestable given identifiable beneficiaries of that narrative.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly do the three kernel readings (contraction, drop, composite) locate their disagreement — is it about the EXTENT of decline (quantitative), the MECHANISM of decline (causal), or the COGNITIVE STATUS of the residual (categorical availability vs. mere practice frequency)?',
    'This is a conceptual/framing question rather than an empirically resolvable one in the ordinary sense; it would be addressed by making explicit, in each sibling story''s commentary, exactly which observable would falsify that reading and confirming the three readings are not simply relabeling the same evidence.',
    'Clarifies that this constraint''s ε (0.28, falling, flattening) is not directly comparable to the siblings'' ε values as a matter of degree on a shared scale — each reading''s ε is assessed by that reading''s own lights about a differently-individuated claim, per the kernel-reading ε referent rule.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Locating precisely what the three sibling readings of the honor_settlement_legitimacy kernel disagree about.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__contraction_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_settlement_legitimacy__contraction_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(hono_tr_t20, honor_settlement_legitimacy__contraction_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(hono_tr_t40, honor_settlement_legitimacy__contraction_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(hono_tr_t60, honor_settlement_legitimacy__contraction_reading, theater_ratio, 60, 0.15).
narrative_ontology:measurement(hono_tr_t80, honor_settlement_legitimacy__contraction_reading, theater_ratio, 80, 0.15).
narrative_ontology:measurement(hono_tr_t100, honor_settlement_legitimacy__contraction_reading, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(hono_be_t20, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(hono_be_t40, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 40, 0.32).
narrative_ontology:measurement(hono_be_t60, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 60, 0.28).
narrative_ontology:measurement(hono_be_t80, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 80, 0.28).
narrative_ontology:measurement(hono_be_t100, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 100, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(hono_su_t20, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(hono_su_t40, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 40, 0.45).
narrative_ontology:measurement(hono_su_t60, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 60, 0.35).
narrative_ontology:measurement(hono_su_t80, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 80, 0.35).
narrative_ontology:measurement(hono_su_t100, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 100, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(honor_settlement_legitimacy__contraction_reading, honor_settlement_legitimacy__drop_reading).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__contraction_reading, honor_settlement_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the honor_settlement_legitimacy kernel (contraction, drop, composite). Each is authored as a separate ε-invariant constraint per the ε-invariance principle: this reading claims mountain with low-and-falling extraction (0.28) and very high accessibility_collapse (0.88) reflecting a categorical-exit thesis; drop_reading is expected to author higher persistent suppression/resistance reflecting ongoing marginal practice; composite_reading is expected to author a shallower, noisier decline trajectory reflecting multiple overlapping causal mechanisms rather than a single categorical transformation. All three link to each other via affects_constraints; none subsumes another.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
