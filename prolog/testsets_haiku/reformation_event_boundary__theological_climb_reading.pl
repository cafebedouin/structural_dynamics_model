% ============================================================================
% CONSTRAINT STORY: reformation_event_boundary__theological_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_event_boundary__theological_climb_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: reformation_event_boundary__theological_climb_reading
 *   human_readable: Reformation as Theological Doctrinal Breakthrough (Justification by Faith Alone)
 *   domain: religious_history/epistemology
 *
 * SUMMARY:
 *   The theological_climb_reading frames the Reformation as a genuine
 *   doctrinal innovation: Martin Luther's intensive study of Scripture led
 *   him to articulate justification by faith alone (sola fide) as the heart
 *   of Christian salvation doctrine, displacing late medieval
 *   sacramental-works theology that had accumulated over centuries. The
 *   doctrine is claimed to be a recovery of Pauline and Augustinian truth,
 *   not a new invention; its institutional separation from Catholicism is
 *   presented as a necessary consequence of doctrinal incompatibility, not as
 *   politically motivated rupture. This reading makes the theological content
 *   itself—the scriptural exegesis and logical force of the sola fide
 *   case—the primary explanatory lever for why the Reformation happened, how
 *   it spread, and why institutional separation was non-negotiable. Political
 *   actors (German princes, English monarchs) enter the story as secondary
 *   riders who exploited the genuine theological rupture for asset seizure;
 *   their interests explain the speed and geography of confessionalization,
 *   not the doctrine itself. This reading is one of three structurally
 *   distinct understandings of the Reformation; it stands in coexistence with
 *   the political_swap_reading (which makes political rupture primary and
 *   theology post-hoc rationalization) and the
 *   composite_overdetermination_reading (which denies any single causal
 *   driver). The theological_climb reading's distinction turns on whether the
 *   doctrinal breakthrough is a genuine climb (discovery of suppressed truth)
 *   or is constructed as natural while serving institutional interests (false
 *   summit). The omegas document the irreducible ambiguity.
 *
 * KEY AGENTS:
 *   - Martin Luther: moderate power, identity-locked to the doctrine via monastic vocation and Scripture study; articulates and defends sola fide despite institutional opposition; cannot abandon without abandoning his understanding of Scripture.
 *   - Protestant believers: powerless individually, gain liberation from works-anxiety and direct access to Scripture; mobile exit (can migrate to Protestant communities or remain nominally Catholic).
 *   - Catholic institutional authority: institutional power, trapped (cannot abandon mediatorial claims without losing its core authority); victim of theological correction under this reading.
 *   - Sacred Scripture corpus: non-agent, vindicated proposition; the textual foundation privileged by the Lutheran reading over scholastic synthesis.
 *   - Secular rulers: powerful, excluded from the theological conversation but opportunistically riders on the rupture; their political interests are secondary to the doctrinal breakthrough.
 *   - Analytical observer (modern historians): can assess whether the theological innovation is genuine or constructed, whether suppression is structural or internalized, whether beneficiaries are authentic or aspirational.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_event_boundary__theological_climb_reading, 0.31).
domain_priors:suppression_score(reformation_event_boundary__theological_climb_reading, 0.18).
domain_priors:theater_ratio(reformation_event_boundary__theological_climb_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, accessibility_collapse, 0.89).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_event_boundary__theological_climb_reading, mountain).
narrative_ontology:human_readable(reformation_event_boundary__theological_climb_reading, "Reformation as Theological Doctrinal Breakthrough (Justification by Faith Alone)").
narrative_ontology:topic_domain(reformation_event_boundary__theological_climb_reading, "religious_history/epistemology").

domain_priors:emerges_naturally(reformation_event_boundary__theological_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_event_boundary__theological_climb_reading, 'e88574b1-654f-4528-aaee-beaefdd198df').
narrative_ontology:cs_kernel_codification('e88574b1-654f-4528-aaee-beaefdd198df', fixed_text).
narrative_ontology:cs_authority_grounding('e88574b1-654f-4528-aaee-beaefdd198df', lineage).
narrative_ontology:cs_interpretation_layer_present('e88574b1-654f-4528-aaee-beaefdd198df').
narrative_ontology:cs_reading_relation('e88574b1-654f-4528-aaee-beaefdd198df', reformation_event_boundary__political_swap_reading, coexists_with).
narrative_ontology:cs_reading_relation('e88574b1-654f-4528-aaee-beaefdd198df', reformation_event_boundary__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('e88574b1-654f-4528-aaee-beaefdd198df', foundational, sola_fide_scriptural_recovery).
narrative_ontology:cs_axiom_status(sola_fide_scriptural_recovery, holdable).
narrative_ontology:cs_axiom_grounding('e88574b1-654f-4528-aaee-beaefdd198df', sola_fide_scriptural_recovery, empirically_contingent).
narrative_ontology:cs_axiom('e88574b1-654f-4528-aaee-beaefdd198df', foundational, doctrine_primacy_over_politics).
narrative_ontology:cs_axiom_status(doctrine_primacy_over_politics, holdable).
narrative_ontology:cs_axiom_grounding('e88574b1-654f-4528-aaee-beaefdd198df', doctrine_primacy_over_politics, instrumental).
narrative_ontology:cs_reference_frame('e88574b1-654f-4528-aaee-beaefdd198df', scripture_primacy_framework).
narrative_ontology:cs_drift_state('e88574b1-654f-4528-aaee-beaefdd198df', early_modern_confessionalization_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e88574b1-654f-4528-aaee-beaefdd198df', '').
narrative_ontology:cs_kernel_id(reformation_event_boundary__theological_climb_reading, reformation_event_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_event_boundary__theological_climb_reading, protestant_believers).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__theological_climb_reading, evangelical_theology_tradition).
narrative_ontology:constraint_victim(reformation_event_boundary__theological_climb_reading, catholic_institutional_authority).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_event_boundary__theological_climb_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(reformation_event_boundary__theological_climb_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_event_boundary__theological_climb_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, ExtMetricName, E),
    domain_priors:suppression_score(reformation_event_boundary__theological_climb_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(reformation_event_boundary__theological_climb_reading),
    narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(reformation_event_boundary__theological_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The theological_climb_reading is claimed to be a Mountain—a naturally-necessary doctrine recovered from Scripture. The authored metrics reflect this: accessibility_collapse is very high (0.89) because once the sola fide argument is understood, the medieval works-system becomes incoherent and collapse is near-complete; resistance is low (0.12) because intellectual assent to a well-formed exegetical argument carries less resistance than coercive extraction would. Extractiveness is very low (0.31) for the interval 1517–1555 because the doctrine itself is not extractive—it liberates believers from anxiety. However, a low non-zero extractiveness remains because the doctrine beneficiaries (Protestant believers and the Reformation tradition itself) exclude Catholic institutional actors from its truth-claim, and some institutional extraction does accrue as Protestant denominations consolidate and establish their own hierarchies. Suppression is also low (0.18) because the constraint persists primarily through doctrinal persuasion and community formation, not through coercion; the Catholic suppression of Luther's teaching is real but operates against the growing intellectual and spiritual plausibility of the doctrine. Theater_ratio is very low (0.08) because the Reformation's primary function is doctrinal innovation and spiritual reorientation, not performative maintenance; the institutional apparatus that develops around Protestant confessions is a later phenomenon outside the tight periodization. The measurements show modest rise in all three metrics from 1517 (Luther's breakthrough moment, when the doctrine is purely intellectual) to 1555 (Peace of Augsburg, when institutional confessionalization is established). The rise in extractiveness reflects the transition from pure theological claim to institutional consolidation; the rise in suppression reflects increasing Catholic institutional opposition; the rise in theater reflects the emerging need for institutional maintenance of the doctrine once political actors have seized and weaponized it. But all three remain low relative to extractive or performative constraints, consistent with the mountain claim.
 *
 * PERSPECTIVAL GAP:
 *   The doctrinal/theological seat (Luther, reform theologians) and the institutional believer seats experience this constraint as liberation and truth. The Catholic institutional seat experiences it as attack and heresy. From the theological seat, the constraint is a recovery of suppressed truth and imposes no extraction—it only corrects error. From the Catholic institutional seat, the constraint is a schismatic attack on legitimate authority and does impose extraction (loss of institutional reach, erosion of mediatorial claims, seizure of assets by opportunistic rulers). The engine computes per-seat types from the structural data: the theological seat is a beneficiary (d near 0.0), the Catholic institutional seat is a victim (d near 1.0). The claimed_type (mountain) asserts the doctrine is naturally true; the metrics allow the engine to assess whether the constraint operates as purely doctrinal (low metrics, genuine mountain) or whether institutional extraction has contaminated the innovation (higher metrics, false summit). The perspectival gap is not a failure of the story—it is the exact point the theological_climb reading insists on: from the seat that understands the doctrine, it is a climb; from the seat whose institutional claims are overturned, it is an attack.
 *
 * DIRECTIONALITY LOGIC:
 *   Martin Luther has identity-locked exit (cannot abandon his monastic vocation or his conviction about Scripture); he is the agenda-setter (articulates and defends the doctrine). His directionality (d) is close to 0.5 (symmetric)—he faces institutional opposition and personal risk, but he is defending what he believes is truth, not extracting benefit. Protestant believers are beneficiaries (low d, toward 0.0) and have mobile exit—they can adopt the doctrine, migrate to Protestant communities, or remain Catholic. Their directionality is low because the doctrine benefits them without trap. Catholic institutional authority is a victim (high d, toward 1.0) with trapped exit—it cannot abandon its mediatorial claims without ceasing to be Catholic in any historically recognizable sense. Its directionality is high because the doctrine undermines its institutional foundation and it has no exit. Secular rulers are excluded (not seated in the doctrinal conversation); their subsequent institutional opportunism is a secondary rider that does not alter their excluded status from the theological_climb frame. The structural beneficiaries (Protestant believers, the Reformation theology tradition) are real—they benefit from the doctrine's truth-claim (if true) or from institutional realignment (if constructed). The victims are real—the Catholic institution bears costs from doctrinal correction (if true) or from theological attack (if constructed). The ambiguity about whether the doctrine is naturally true (mountain) or constructed (false summit) is where the omegas come in.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (late medieval contradiction between doctrinal promise and institutional practice) is live at the Reformation's start and structurally addressed by sola fide (eliminating the contradiction by privileging faith). Under the theological_climb reading, the founding problem status remains 'live' at the interval end (1555) because the doctrinal contradiction is never fully resolved—Catholic theology continues to insist on the necessity of sacramental works, and the Peace of Augsburg merely establishes territorial confessional boundaries without resolving the theological dispute. This prevents mandatrophy from triggering (mandatrophy would require the founding problem to be dead while the constraint persists). The disappearance_verdict is 'world_rearranges' because if sola fide had been universally accepted as false, denominational Christendom would not have split. The constraint (the theological doctrine + its institutional separation consequence) is necessary to the historical outcome. No mandatrophy is present in this reading: the constraint persists because the doctrinal dispute is unsettled and believers continue to find one side or the other persuasive, not because institutional inertia maintains a defunct function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_reading,
    'Is justification by faith alone a re-discovered eternal truth about salvation (natural law of Christian doctrine) or a theological interpretation that benefits Protestant institutional actors and is therefore constructed as natural?',
    'Textual analysis of pre-Reformation Augustine and Paul against Luther''s reading; independent assessment by scholars outside both Protestant and Catholic institutional frameworks (modern exegetical consensus); comparison of theological necessity (would Christianity collapse without sola fide?) against institutional benefit (does the doctrine serve Protestant institutional interests?).',
    'If the doctrine is discovered natural law: the constraint is Mountain and the beneficiary list is merely observational (noting who benefits from truth). If the doctrine is constructed reading: the constraint is Tangled Rope (theological innovation + institutional extraction riding on it), and the beneficiaries become evidence of institutional capture masquerading as truth.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_reading, conceptual, 'Whether the Reformation doctrine is a genuine scriptural recovery or a constructed reading that serves Protestant institutional interests.').

omega_variable(
    theological_primacy_vs_political_overdetermination,
    'Would the institutional Reformation have occurred at the scale and speed it did if the theological dispute had remained confined to academic debate without exploitation by secular political actors?',
    'Counterfactual historical analysis: comparison of Reformation spread in regions with strong secular rulers (rapid, institutional) vs. regions without secular political interest (slower, spiritual only); analysis of whether doctrinal adoption correlates with theological persuasion or with political-economic benefit to rulers; examination of confessional uniformity in regions where rulers mandated theology vs. regions of believer choice.',
    'If theology alone would have driven comparable institutional change: the theological_climb reading holds and politics are secondary riders. If institutional change is driven by political rulers'' interests and theology follows: the political_swap reading gains plausibility, and the theological_climb reading describes belief-community formation but not historical causation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_primacy_vs_political_overdetermination, empirical, 'Whether the theological breakthrough is the primary driver of Reformation institutions or a necessary-but-not-sufficient cause whose institutional scale depends on political opportunism.').

omega_variable(
    suppression_internalization_mechanism,
    'Is the Catholic institutional suppression of sola fide teaching (prohibition of Luther''s works, Council of Trent counter-doctrine, inquisitorial enforcement) structural coercion, or does Catholic teaching internalize the resistance to the Protestant doctrine as part of its own truth-claim?',
    'Post-suppression trajectory analysis: do Catholic believers who encounter Protestant theology find the arguments rationally compelling and resist via choice, or merely obey institutional prohibition? If the former, suppression is structural and Catholic doctrine stands on its epistemic merits. If the latter, suppression is internalized into doctrinal identity and the measured suppression underestimates the constraint''s true coercive force.',
    'If suppression is structural: the measured suppression (0.18) captures the enforcement cost and the Catholic institutional response is rationally considered counter-argument. If suppression is internalized: the constraint''s effective suppression is higher and the Catholic victim-position is more trapped than the structural measure suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_mechanism, empirical, 'Whether Catholic institutional resistance to sola fide is rational counter-argument or internalized identity-locked rejection.').

omega_variable(
    beneficiary_claim_authenticity,
    'Do Protestant believers genuinely experience sola fide doctrine as liberating from scrupulosity and works-anxiety, or do they experience it as cover for institutional rebellion and social realignment?',
    'Testimonial analysis of early Protestant converts: conscience-testimony (diaries, spiritual writings, letters) from believers converted for stated theological reasons vs. political/economic reasons; long-term psychological outcomes in regions of believer choice vs. imposed confessionalization; attention to whether believers report relief from conscience-anxiety or adoption of new doctrinal identities.',
    'If believers experience genuine liberation: sola fide is correctly identified as a coordination solution to a real conscience problem, and the beneficiary claim is accurate. If believers'' stated theological reasons are post-hoc rationalization: the theological_climb reading mislabels what is primarily institutional realignment and identity-adoption, and the beneficiary list is aspirational rather than descriptive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_claim_authenticity, empirical, 'Whether Protestant beneficiaries genuinely gain from theological doctrine or from institutional opportunity masked as doctrine.').

omega_variable(
    periodization_boundary_stability,
    'Is the tight periodization (1517–1555) stable, or does the theological_climb event extend backward (medieval proto-Reformation theology) or forward (ongoing doctrinal elaboration to present day)?',
    'Genealogy of justification doctrine: trace sola fide back through medieval theology (Scotus, Ockham, Bradwardine) to assess whether Luther''s reading is a recovery of dormant theology or a genuine innovation; assess whether post-Trent Catholic theology fundamentally rebuts the doctrine or merely re-emphasizes alternative elements; examine whether sola fide doctrine remains generative in contemporary theology or is vestigial.',
    'If periodization is tight and stable: the Reformation is a contained historical event with a clear causal locus (Luther''s reading, 1517–1555). If the doctrine extends backward: Luther is recovering what was always latent, and the innovation claim weakens. If the doctrine extends forward: the Reformation is an ongoing reconfiguration rather than a completed event, and the endpoint (1555) is arbitrary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(periodization_boundary_stability, empirical, 'Whether the theological innovation has a tight historical boundary or extends beyond the marked interval.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_event_boundary__theological_climb_reading, 1517, 1555).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1517, reformation_event_boundary__theological_climb_reading, theater_ratio, 1517, 0.02).
narrative_ontology:measurement(refo_tr_t1525, reformation_event_boundary__theological_climb_reading, theater_ratio, 1525, 0.04).
narrative_ontology:measurement(refo_tr_t1535, reformation_event_boundary__theological_climb_reading, theater_ratio, 1535, 0.06).
narrative_ontology:measurement(refo_tr_t1545, reformation_event_boundary__theological_climb_reading, theater_ratio, 1545, 0.08).
narrative_ontology:measurement(refo_tr_t1555, reformation_event_boundary__theological_climb_reading, theater_ratio, 1555, 0.08).

% Extraction over time
narrative_ontology:measurement(refo_be_t1517, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1517, 0.12).
narrative_ontology:measurement(refo_be_t1525, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1525, 0.18).
narrative_ontology:measurement(refo_be_t1535, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1535, 0.28).
narrative_ontology:measurement(refo_be_t1545, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1545, 0.31).
narrative_ontology:measurement(refo_be_t1555, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1555, 0.31).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1517, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1517, 0.08).
narrative_ontology:measurement(refo_su_t1525, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1525, 0.12).
narrative_ontology:measurement(refo_su_t1535, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1535, 0.15).
narrative_ontology:measurement(refo_su_t1545, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1545, 0.18).
narrative_ontology:measurement(refo_su_t1555, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1555, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_event_boundary__theological_climb_reading, information_standard).
narrative_ontology:boltzmann_floor_override(reformation_event_boundary__theological_climb_reading, 0.03).
narrative_ontology:affects_constraint(reformation_event_boundary__theological_climb_reading, reformation_event_boundary__political_swap_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__theological_climb_reading, reformation_event_boundary__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% The Reformation is a contested kernel instantiated by three structurally distinct constraint stories: theological_climb_reading (this story, emphasizing doctrinal innovation and recovery), political_swap_reading (emphasizing political rupture and asset seizure), and composite_overdetermination_reading (denying single primary driver). Each reading instantiates a different constraint with different ε, different victim/beneficiary structure, and different periodization. The readings coexist as live positions held by different scholarly communities; none logically forecloses the others within a single interpretive framework, though each creates structural pressure on the others. All three affect one another through scholarly debate, institutional legitimacy claims, and historical interpretation. This story (theological_climb) links to the sibling readings via network.affects_constraints to enable contamination propagation analysis and cross-reading comparison.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
