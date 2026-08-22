% ============================================================================
% CONSTRAINT STORY: honor_settlement_legitimacy__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: honor_settlement_legitimacy__contraction_reading
 *   human_readable: Honor-Settlement Legitimacy — Contraction Reading (Dueling as Cognitively Unthinkable)
 *   domain: historical_sociology/legal_history/cultural_anthropology
 *
 * SUMMARY:
 *   This story instantiates the contraction reading of the
 *   honor_settlement_legitimacy kernel: the claim that dueling's decline was
 *   not a matter of increasing prohibition against a stable,
 *   persistently-thinkable practice (the drop_reading) nor a case of multiple
 *   independently-sufficient mechanisms operating alongside a residual honor
 *   grammar (the composite_reading), but rather a wholesale exit of
 *   honor-violence vocabulary from the space of cognitively available
 *   responses to insult. Under this reading, by the late 19th century a
 *   gentleman insulted in public did not experience dueling as 'illegal but
 *   comprehensible' — he experienced it as unthinkable, on a par with
 *   resolving a business dispute by ordeal. The suppression_requirement
 *   series falls sharply across the interval precisely because this reading
 *   holds that active suppression became progressively less necessary as the
 *   underlying cognitive framework degraded — a rule that no longer needs
 *   enforcing because no one is trying to break it is the diagnostic
 *   signature the contraction reading predicts and the drop_reading does not.
 *
 * KEY AGENTS:
 *   - bourgeois_professional_class: beneficiary (organized/analytical) — inherits the now-exclusive legal/reputational channels
 *   - centralized_state_legal_monopoly: agenda_setter (institutional/analytical) — reshaped honor's vocabulary, not merely its legality
 *   - insurance_and_credit_institutions: beneficiary (institutional/analytical) — require calculable, non-violent dispute resolution
 *   - former_dueling_class_descendants: payer (moderate/identity_locked) — lost an entire framework for self-respect without replacement
 *   - military_honor_subculture: excluded (organized/constrained) — retained residual honor-code vocabulary, treated as friction not counter-evidence
 *   - social_and_cultural_historians: observer (analytical/analytical) — adjudicate between readings via textual evidence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__contraction_reading, 0.28).
domain_priors:suppression_score(honor_settlement_legitimacy__contraction_reading, 0.35).
domain_priors:theater_ratio(honor_settlement_legitimacy__contraction_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__contraction_reading, mountain).
narrative_ontology:human_readable(honor_settlement_legitimacy__contraction_reading, "Honor-Settlement Legitimacy — Contraction Reading (Dueling as Cognitively Unthinkable)").
narrative_ontology:topic_domain(honor_settlement_legitimacy__contraction_reading, "historical_sociology/legal_history/cultural_anthropology").

domain_priors:emerges_naturally(honor_settlement_legitimacy__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__contraction_reading, 'bf641561-5492-47c2-b467-0ac4d5aedf3c').
narrative_ontology:cs_kernel_codification('bf641561-5492-47c2-b467-0ac4d5aedf3c', distributed).
narrative_ontology:cs_authority_grounding('bf641561-5492-47c2-b467-0ac4d5aedf3c', distributed).
narrative_ontology:cs_reading_relation('bf641561-5492-47c2-b467-0ac4d5aedf3c', honor_settlement_legitimacy__drop_reading, forecloses).
narrative_ontology:cs_reading_relation('bf641561-5492-47c2-b467-0ac4d5aedf3c', honor_settlement_legitimacy__composite_reading, influences).
narrative_ontology:cs_axiom('bf641561-5492-47c2-b467-0ac4d5aedf3c', foundational, honor_violence_vocabulary_wholly_exited_possibility_space).
narrative_ontology:cs_axiom_status(honor_violence_vocabulary_wholly_exited_possibility_space, holdable).
narrative_ontology:cs_axiom_grounding('bf641561-5492-47c2-b467-0ac4d5aedf3c', honor_violence_vocabulary_wholly_exited_possibility_space, empirically_contingent).
narrative_ontology:cs_axiom('bf641561-5492-47c2-b467-0ac4d5aedf3c', secondary, residual_practice_is_marginal_friction_not_counterevidence).
narrative_ontology:cs_axiom_status(residual_practice_is_marginal_friction_not_counterevidence, holdable).
narrative_ontology:cs_axiom_grounding('bf641561-5492-47c2-b467-0ac4d5aedf3c', residual_practice_is_marginal_friction_not_counterevidence, conventional).
narrative_ontology:cs_reference_frame('bf641561-5492-47c2-b467-0ac4d5aedf3c', ancien_regime_honor_grammar).
narrative_ontology:cs_drift_state('bf641561-5492-47c2-b467-0ac4d5aedf3c', post_ww1_professionalized_state, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('bf641561-5492-47c2-b467-0ac4d5aedf3c', '').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__contraction_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__contraction_reading, bourgeois_professional_class).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__contraction_reading, centralized_state_legal_monopoly).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__contraction_reading, insurance_and_credit_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__contraction_reading, former_dueling_class_descendants).
narrative_ontology:constraint_vindicates(honor_settlement_legitimacy__contraction_reading, legal_monopoly_on_violence_doctrine).
narrative_ontology:constraint_vindicates(honor_settlement_legitimacy__contraction_reading, civilizing_process_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rising professional and commercial classes whose status depended on contractual reliability, credit, and institutional reputation rather than blood honor. As dueling became unintelligible as a status-repair mechanism, disputes moved into courts, licensing bodies, and print libel actions — arenas where this class already held structural advantage. They do not administer the transformation; they simply find their preferred dispute-resolution channels now the only cognitively available ones.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, bourgeois_professional_class, beneficiary,
    organized, generational, analytical, national).

% The state's claim to sole legitimate authority over violence and over the adjudication of insult required that private violent self-help be rendered not merely illegal but unthinkable — a rule that must be actively policed produces martyrs and folk heroes, whereas a rule that has become common sense produces silence. State institutions (courts, army codes of conduct, press) reshaped the vocabulary of honor itself so dueling could no longer be narrated as honorable.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, centralized_state_legal_monopoly, agenda_setter,
    institutional, civilizational, analytical, national).

% Actuarial and credit-rating institutions require predictable, non-violent settlement of disputes among the propertied classes they insure and lend to. The reframing of honor as reputational-legal rather than blood-debt made individuals' economic and physical risk profiles calculable in ways duels had made volatile.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, insurance_and_credit_institutions, beneficiary,
    institutional, generational, analytical, national).

% Descendants of the aristocratic and officer classes for whom personal honor once had a specific, actionable grammar. Under the contraction reading, this vocabulary does not persist as an option they choose not to exercise — it has become genuinely unavailable to them as a way of thinking about insult, so any residual impulse toward it registers as embarrassment or pathology rather than principled restraint. They bear the cost of losing an entire framework for self-respect without any replacement they authored.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, former_dueling_class_descendants, payer,
    moderate, generational, identity_locked, national).

% Officer corps and military academies retained honor-code vocabulary (codes of conduct, courts of honor) longer than civilian society and would object that honor-based conflict resolution remains coherent and functional within bounded institutional contexts. Their continued institutional practice is treated by the contraction reading as residual friction rather than as evidence the framework never fully exited the possibility space — they are structurally outside the historians' consensus this reading represents.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, military_honor_subculture, excluded,
    organized, biographical, constrained, national).

% Scholars who reconstruct the shift in newspaper coverage, legal argument, and private correspondence to trace when dueling stopped being narratable as honorable and became narratable only as criminal, absurd, or psychologically disordered. They adjudicate between this reading and the drop/composite readings using textual and statistical evidence of vocabulary change, not merely incidence decline.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, social_and_cultural_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A shared cultural vocabulary for status, insult, and legitimate response allows disputes to be resolved (or escalated) predictably within a community; the contraction reading holds that this function was not merely redirected but structurally replaced — the entire honor-violence grammar was retired from the set of thinkable responses, not merely outcompeted by an alternative in continued competition.
% TRANSFER_FUNCTION: Moves the authority to define legitimate response to insult from a decentralized honor-code grammar (available to any gentleman) to centralized legal and reputational institutions (courts, press, credit bureaus) controlled by professional and state actors — a transfer of interpretive monopoly, not merely of enforcement.
% ABSENT_VOICES: The military honor subculture and any surviving fringe practitioners of honor-code dueling would object that the framework remains coherent and available to them; under this reading their objection is treated as evidence of incomplete diffusion rather than evidence against contraction, which the drop_reading would weigh very differently.
% DISAPPEARANCE_RATIONALE: As a genuine cognitive-framework mountain (per this reading), if the constraint 'disappeared' the honor-violence vocabulary would not simply become available again by removing a rule — it would have to be relearned or reconstructed as a coherent way of thinking, because ex hypothesi no living cognitive framework currently holds the concepts needed to resurrect it as legitimate. This is what distinguishes contraction from drop: under drop, removing suppression would let dormant practice resurface immediately.
% FOUNDING_PROBLEM: Honor culture originally solved a real coordination problem in societies with weak or partial state monopolies on violence: reputational credibility needed a self-enforcing mechanism because courts could not or would not adjudicate matters of personal insult, especially among elites exempt from ordinary criminal process.
% FOUNDING_PROBLEM_CORROBORATION: Independent legal historians (outside the beneficiary classes) corroborate via court records and press archives that by the late 19th century the state's monopoly on violence and functioning civil defamation law had displaced the coordination function that honor culture once served; no surviving institution outside the residual military honor subculture treats the founding problem as unsolved, and even that subculture channels it through formal disciplinary codes rather than lethal combat, which the corroborating historians read as confirming the underlying framework, not merely its enforcement, has changed.
narrative_ontology:disappearance_verdict(honor_settlement_legitimacy__contraction_reading, world_unchanged).
narrative_ontology:founding_problem_status(honor_settlement_legitimacy__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_settlement_legitimacy__contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored low-to-moderate (0.10 rising to 0.28) because the contraction reading treats the framework shift as a genuine cognitive/normative mountain — no one 'extracts' from a framework that has become common sense, though the beneficiary classes cited do capture the diffuse gains of channel-exclusivity (courts, credit, licensing) that emerged once honor-violence exited the possibility space. This is exactly the FSM-candidate pattern: a mountain claim with declared beneficiaries, hence the omega below. Accessibility_collapse is authored very high (0.88) because the defining feature of this reading is that alternatives (dueling as legitimate response) did not merely become costly — they became incoherent. Resistance is authored very low (0.08) because a genuinely contracted framework meets almost no active resistance; what resistance existed (military honor subculture) is treated within this reading as marginal residue, not counter-evidence. The suppression_requirement trajectory falling from 0.55 to 0.10 embodies the reading's core empirical signature: declining need for active enforcement as the practice becomes self-evidently absurd rather than merely forbidden.
 *
 * PERSPECTIVAL GAP:
 *   The military honor subculture's seat and the historians' observer seat compute this constraint very differently: from inside the subculture, honor-code dispute resolution remains a live, coherent option they exercise within bounded institutional forms (courts of honor, codes of conduct) — evidence against total contraction. From the observer seat adjudicating the kernel contest, this persistence is read as residual friction consistent with partial, non-total contraction, which is exactly the structural tension the drop_reading and composite_reading exist to capture as separate constraints.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (bourgeois professionals, the state's legal monopoly, credit institutions) are institutional/analytical actors who did not need to fight for this outcome — the framework shift simply handed them exclusive jurisdiction over dispute resolution they were already positioned to exploit, so directionality sits near the subsidized end. The payer class (former dueling-class descendants) is identity_locked: their cost is not a transfer of resources but the loss of an entire self-respect vocabulary with no substitute they authored, which the derivation chain would otherwise under-weight if treated as an ordinary economic victim group — hence the identity_locked exit option rather than merely constrained.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (self-enforcing status-repair where state courts were absent or exempted the elite) is corroborated as dead by legal historians outside the beneficiary set, and the disappearance_verdict is world_unchanged specifically because a genuine framework-contraction, unlike a mere rule, cannot be un-vanished by removing a law — this is what prevents the contraction reading from being mistaken for ordinary mandatrophy (an extractive rule that outlived its function but persists through inertia). Here nothing persists to be mislabeled: the reading's claim is precisely that the apparatus IS gone, not merely idle.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mountain_or_constructed_consensus,
    'Is the disappearance of honor-violence vocabulary a genuine cognitive-framework mountain (no longer thinkable, full stop), or a constructed consensus maintained by institutions (state legal monopoly, credit/insurance apparatus, professional classes) that benefit from its disappearance and actively police its cultural memory?',
    'Comparative analysis of subcultures (military honor codes, certain diaspora communities, some prison social systems) where honor-violence vocabulary demonstrably persists as a coherent, thinkable option; if such vocabulary can be reactivated readily by motivated actors, contraction is overstated and the constraint is closer to constructed suppression with declared beneficiaries than to natural cognitive change.',
    'If genuinely a mountain, the declared beneficiaries collect a windfall from a framework shift they did not cause, which is unusual but not disqualifying for a mountain classification. If constructed, FSM fires and the correct classification is closer to tangled_rope — a framework change actively maintained by institutions that benefit from honor-violence''s unthinkability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mountain_or_constructed_consensus, conceptual, 'Whether cognitive unthinkability is natural framework drift or institutionally maintained construction.').

omega_variable(
    kernel_reading_disambiguation,
    'This constraint is one reading (contraction_reading) of the contested honor_settlement_legitimacy kernel. The sibling readings — drop_reading (persistence as fringe practice) and composite_reading (overdetermined decline with a contraction edge) — would structurally change the victim set, the accessibility_collapse value, and the suppression_requirement trajectory. Which reading best fits the specific empirical record (vocabulary in press/court/private correspondence vs. mere incidence statistics)?',
    'Systematic corpus analysis of 19th-century press, legal argument, and private correspondence coding whether dueling references treat the practice as (a) illegal-but-comprehensible, (b) fringe/practiced by identifiable residual groups, or (c) narratively unavailable/absurd — distinguishing drop from contraction requires linguistic-framework evidence, not incidence counts alone.',
    'If the record shows (b) — clear residual practicing subcultures treated by contemporaries as continuing a live, if declining, tradition — the drop_reading is the better-fitting constraint and this contraction_reading overstates accessibility_collapse. If the record shows (c) pervasively, contraction is well-supported and the military honor subculture becomes the anomalous residue this story already treats it as.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disambiguation, empirical, 'Which kernel reading (contraction vs. drop vs. composite) best fits the historical vocabulary record; the disagreement is located in whether residual honor-code practice constitutes counter-evidence or marginal friction.').

omega_variable(
    military_subculture_boundary_case,
    'Does the persistence of honor-code vocabulary within bounded military institutional contexts (courts of honor, codes of conduct) constitute a counterexample to total contraction, or is it a formally transformed and functionally different institution that no longer instantiates the same framework?',
    'Compare the semantic content and enforcement logic of 20th-century military honor codes against 18th-century dueling codes — if the former have been substantively reformed to exclude lethal violence and route through disciplinary bureaucracy, they may represent a different (successor) framework rather than survival of the original.',
    'If the military subculture retains the original framework, contraction is not total and this story''s accessibility_collapse (0.88) is too high. If the military subculture''s honor code is a distinct successor framework, the boundary case does not undermine contraction and the excluded-voice treatment in six_questions.absent_voices is justified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(military_subculture_boundary_case, conceptual, 'Whether military honor-code persistence is a survival or a successor framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__contraction_reading, 1750, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1750, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1750, 0.05).
narrative_ontology:measurement(hono_tr_t1790, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1790, 0.06).
narrative_ontology:measurement(hono_tr_t1830, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1830, 0.08).
narrative_ontology:measurement(hono_tr_t1870, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1870, 0.1).
narrative_ontology:measurement(hono_tr_t1910, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1910, 0.11).
narrative_ontology:measurement(hono_tr_t1950, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1950, 0.12).

% Extraction over time
narrative_ontology:measurement(hono_be_t1750, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1750, 0.1).
narrative_ontology:measurement(hono_be_t1790, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1790, 0.14).
narrative_ontology:measurement(hono_be_t1830, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1830, 0.2).
narrative_ontology:measurement(hono_be_t1870, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1870, 0.25).
narrative_ontology:measurement(hono_be_t1910, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1910, 0.27).
narrative_ontology:measurement(hono_be_t1950, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1950, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1750, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1750, 0.55).
narrative_ontology:measurement(hono_su_t1790, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1790, 0.5).
narrative_ontology:measurement(hono_su_t1830, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1830, 0.4).
narrative_ontology:measurement(hono_su_t1870, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1870, 0.28).
narrative_ontology:measurement(hono_su_t1910, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1910, 0.18).
narrative_ontology:measurement(hono_su_t1950, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1950, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(honor_settlement_legitimacy__contraction_reading, drop_reading).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__contraction_reading, composite_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the honor_settlement_legitimacy kernel (contraction_reading, drop_reading, composite_reading), each authored as a separate constraint per the epsilon-invariance principle. contraction_reading claims near-total accessibility_collapse (0.88) and a sharply falling suppression_requirement trajectory (0.55->0.10), reflecting its core claim that active enforcement became progressively unnecessary as the framework itself dissolved. drop_reading (sibling, not authored here) would instead claim a persistently practiced fringe tradition requiring ongoing (if declining) suppression against identifiable adherents — lower accessibility_collapse, a victim group of residual honor-culture practitioners, and a flatter or differently-shaped suppression curve. composite_reading (sibling, not authored here) would claim multiple independently-sufficient mechanisms (legal, economic, cultural) operating in parallel with only partial cognitive contraction, producing an intermediate accessibility_collapse value and a mixed beneficiary/victim structure drawing on both this story's institutional beneficiaries and drop_reading's residual-practitioner victims. The three stories share the same historical interval and underlying empirical record but instantiate structurally distinct claims about mechanism and completeness, hence distinct epsilon values and distinct classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
