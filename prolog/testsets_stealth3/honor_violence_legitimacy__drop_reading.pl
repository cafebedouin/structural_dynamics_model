% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__drop_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_violence_legitimacy__drop_reading, []).

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
 *   constraint_id: honor_violence_legitimacy__drop_reading
 *   human_readable: Honor-Violence Code: Retained Legitimacy, Collapsed Practice (Drop Reading)
 *   domain: historical sociology/legal anthropology/commitment systems
 *
 * SUMMARY:
 *   Between roughly 1780 and 1900 (mapped to interval 0-120) the European and
 *   American code of honor retained its full conceptual and institutional
 *   apparatus — written codes, courts of honor, seconds' protocols, the
 *   posted-coward sanction — while the frequency of actual meetings
 *   collapsed. This story instantiates the drop reading of the
 *   honor-violence-legitimacy kernel: the code stayed structurally legitimate
 *   and available, and external costs — anti-dueling statutes, prosecutions,
 *   changing military regulation, shifting economic incentives — priced
 *   practice out faster than belief retired it. What remained by the
 *   interval's end is an idle but formally live institution: challenges still
 *   issuable, boards still convenable, manuals still printed, meetings seldom
 *   fought. Constraint-family note: the colloquial label 'the decline of
 *   dueling' fails epsilon-invariance as a single story — measuring it by
 *   conceptual availability yields a live-but-idle code (this file),
 *   measuring it by the semantic content of honor yields a redefined notion
 *   excluding violence (the contraction reading), and the record supports
 *   elements of both (the composite reading). Each is authored separately
 *   with its own epsilon, victims, and classification, linked via
 *   network.affects_constraints. KEY AGENTS (by structural relationship): -
 *   court_of_honor_senior_officers: Agenda-setting custodian
 *   (institutional/constrained) — administers the code, could retire it -
 *   aristocratic_gentry_class: Primary beneficiary (powerful/identity_locked)
 *   — collects order and boundary definition, buries its own sons -
 *   junior_officers_challenged_gentlemen: Primary target (moderate/trapped) —
 *   bears compelled risk between posting and the meeting-ground -
 *   bereaved_families_of_duelists: Secondary target (moderate/constrained) —
 *   bear irrecoverable losses with no seat in the settlement -
 *   anti_dueling_movement: Excluded opposition (organized/mobile) — imposes
 *   the external costs from outside the adjudicating frame -
 *   fencing_masters_manual_authors: Incidental beneficiary (moderate/mobile)
 *   — sells the code's technique without bearing its demands -
 *   legal_historians_anthropologists: Analytical observer
 *   (analytical/analytical) — sees the full structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__drop_reading, 0.3).
domain_priors:suppression_score(honor_violence_legitimacy__drop_reading, 0.34).
domain_priors:theater_ratio(honor_violence_legitimacy__drop_reading, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, suppression_requirement, 0.34).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, theater_ratio, 0.72).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__drop_reading, piton).
narrative_ontology:human_readable(honor_violence_legitimacy__drop_reading, "Honor-Violence Code: Retained Legitimacy, Collapsed Practice (Drop Reading)").
narrative_ontology:topic_domain(honor_violence_legitimacy__drop_reading, "historical sociology/legal anthropology/commitment systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__drop_reading, '52159d1f-fa77-4bf4-895f-10e5b6cf48f8').
narrative_ontology:cs_kernel_codification('52159d1f-fa77-4bf4-895f-10e5b6cf48f8', fixed_text).
narrative_ontology:cs_authority_grounding('52159d1f-fa77-4bf4-895f-10e5b6cf48f8', lineage).
narrative_ontology:cs_interpretation_layer_present('52159d1f-fa77-4bf4-895f-10e5b6cf48f8').
narrative_ontology:cs_reading_relation('52159d1f-fa77-4bf4-895f-10e5b6cf48f8', honor_violence_legitimacy__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('52159d1f-fa77-4bf4-895f-10e5b6cf48f8', honor_violence_legitimacy__composite_reading, influences).
narrative_ontology:cs_axiom('52159d1f-fa77-4bf4-895f-10e5b6cf48f8', foundational, honor_satisfaction_requires_personal_risk).
narrative_ontology:cs_axiom_status(honor_satisfaction_requires_personal_risk, holdable).
narrative_ontology:cs_axiom_grounding('52159d1f-fa77-4bf4-895f-10e5b6cf48f8', honor_satisfaction_requires_personal_risk, conventional).
narrative_ontology:cs_axiom('52159d1f-fa77-4bf4-895f-10e5b6cf48f8', foundational, disuse_does_not_repeal_honor_obligation).
narrative_ontology:cs_axiom_status(disuse_does_not_repeal_honor_obligation, holdable).
narrative_ontology:cs_axiom_grounding('52159d1f-fa77-4bf4-895f-10e5b6cf48f8', disuse_does_not_repeal_honor_obligation, conventional).
narrative_ontology:cs_reference_frame('52159d1f-fa77-4bf4-895f-10e5b6cf48f8', operative_code_duello_regime).
narrative_ontology:cs_drift_state('52159d1f-fa77-4bf4-895f-10e5b6cf48f8', fin_de_siecle_practice_collapse, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('52159d1f-fa77-4bf4-895f-10e5b6cf48f8', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__drop_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__drop_reading, aristocratic_gentry_class).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__drop_reading, fencing_masters_manual_authors).
narrative_ontology:constraint_victim(honor_violence_legitimacy__drop_reading, junior_officers_challenged_gentlemen).
narrative_ontology:constraint_victim(honor_violence_legitimacy__drop_reading, bereaved_families_of_duelists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Senior officers and committee men who convene boards to hear affairs of honor, rule on whether an insult warrants a meeting, supervise exchanges between seconds, and post men who refuse without adequate apology. Their rank authority rests on administering this jurisdiction. By the late interval their caseload is thin and largely ceremonial: most affairs end in negotiated apology before any meeting is arranged. They could retire the jurisdiction by regulation — several armies eventually did — but doing so surrenders the disciplinary instrument and the adjudicating role their standing inside the corps depends on.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, court_of_honor_senior_officers, agenda_setter,
    institutional, generational, constrained, national).

% The class whose internal order the code governs. It collects orderly dispute settlement among peers who scorn the common-law courts, clear boundaries around membership claims, and the deterrent shadow a possible meeting casts over insults. It also buries sons: meetings kill or maim a share of participants, prosecutions touch others, and the code's demands fall on its own households. Membership in the class is constituted by code observance; a man who renounces it places himself outside gentility, so the class as a whole cannot easily set the code down without dissolving what it is.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, aristocratic_gentry_class, beneficiary,
    powerful, generational, identity_locked, continental).

% Subalterns, younger sons, and any gentleman on the receiving end of a challenge. Institutional hierarchy leaves them little leverage: seniors sit on the boards that rule their affairs, custom fixes the choice between accepting a meeting — with its risk of death, wound, or prosecution under the anti-dueling statutes — and refusing, which posts them as cowards and ends commissions, engagements, and club memberships. Resigning the army or emigrating forfeits livelihood and standing together. Most pay in risk and anxiety; some pay in blood or ruin.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, junior_officers_challenged_gentlemen, payer,
    moderate, biographical, trapped, national).

% Wives, parents, and children of men killed or ruined in meetings, and of men prosecuted for giving or taking them. They inherit the loss without any seat in the code that produced it: no board hears them, no apology restores the household's earner, and the honor settlement that justified the meeting allocates them nothing. Some campaign against the practice afterward; most simply bear the cost.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, bereaved_families_of_duelists, payer,
    moderate, generational, constrained, national).

% Clergymen, radical pamphleteers, utilitarian reformers, satirists, and the legislators and prosecutors they moved. They press from outside the adjudicating frame: no court of honor seats them, no club committee hears them, and their objections enter the affair only as external facts — statutes, sermons, mockeries, prosecutions — that raise the price of every meeting. Their pressure is the chief reason meetings grew rare; they seek the code's abolition outright, not reform of its terms.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, anti_dueling_movement, excluded,
    organized, generational, mobile, national).

% Masters of arms, swordsmen, pistol instructors, and the publishers and authors of dueling codes and manuals. They sell the code's technique: lessons, weapons, treatises on etiquette and firearms accuracy. Their trade follows the code's fortunes — thriving when meetings are frequent, thinning as they grow rare — and they collect fees for service rendered rather than any share in what the code's settlements produce.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, fencing_masters_manual_authors, beneficiary,
    moderate, biographical, mobile, national).

% Analysts reconstructing the practice's rise and fall from court records, regimental archives, correspondence, and the manual literature. They stand outside every seat: they trace how often meetings occurred, what the boards ruled, who refused and at what price, and how the code's texts and its practice diverged over the interval.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, legal_historians_anthropologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_violence_legitimacy__drop_reading, diffuse).
narrative_ontology:fixing_cost_class(honor_violence_legitimacy__drop_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gave the honor class a regulated procedure for settling status injuries — challenge, seconds' negotiation, agreed terms, satisfaction — containing disputes among armed peers who disdained the common-law courts, and drawing a public line around who counted as a gentleman and who did not.
% TRANSFER_FUNCTION: Moved bodily risk and blood from challenged and challenging gentlemen into the satisfaction account of class honor: the challenged staked his life to restore standing, the refuser paid in status, and the class collected order and boundary definition. Late in the interval the transfers shrink to symbolic compliance — apologies negotiated by seconds — plus fees to the code's tradesmen and occasional prosecutions under the statutes.
% ABSENT_VOICES: The men who wanted out but could not say so: a challenged gentleman's refusal had to be dressed as apology or counter-charge, never as unwillingness, so their voice enters the record only through seconds' euphemisms. Outside the class, clergymen, radical reformers, and the widows of the killed objected loudly but held no seat in any court of honor or club committee — the rooms where affairs were actually adjudicated. Their opposition reached the practice only as external price: statutes, prosecutions, ridicule.
% DISAPPEARANCE_RATIONALE: Remove the code overnight and the honor class loses its dispute-settlement protocol: pending affairs reroute to litigation, ostracism, or private vengeance; courts of honor lose their jurisdiction and the officer corps its disciplinary centerpiece; fencing masters and manual publishers lose their trade; the class's boundary language loses its referent. Because meetings were already rare, the material shock is modest — but the conceptual rearrangement, honor without violent satisfaction, is exactly the ground the sibling readings contest.
% FOUNDING_PROBLEM: Uncontainable feud among armed equals: in a class where every gentleman went armed and disdained the common-law courts as beneath a man of honor, an unpunished insult invited escalating private violence between houses. The code substituted a bounded, regulated satisfaction — one meeting, chosen weapons, seconds supervising terms — for open feud.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and the quantitative record corroborate the dissolution: challenge frequency collapses even where statutes were lax or unenforced, armed-peer feud disappears into state courts and policing during the interval, and the class disarms itself. Only the code's own custodians — court-of-honor officers and traditionalist memoirists — attest the problem live, and their attestation is contradicted by their own shrinking caseloads. Outside the benefiting parties, no source attests it.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__drop_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__drop_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__drop_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_violence_legitimacy__drop_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_violence_legitimacy__drop_reading, 0.3, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__drop_reading_tests).
:- end_tests(honor_violence_legitimacy__drop_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored from the standing arrangement's actual operation, not its formal status. At interval start the code compels widespread participation under ruin-or-death terms and takes a steady toll of killed and ruined men (0.72); as external costs bite, incidence falls and with it the toll (0.30 at end) — but the compulsion mechanism never repeals itself, so extraction declines without vanishing. Suppression (0.34 at end) tracks the code's own coercive machinery — posting, boards, expulsion — which attrits across the interval as the state absorbs dispute settlement; the suppression_requirement series records that enforcement decay explicitly, which is why it is authored here rather than left static. Theater rises monotonically (0.18 to 0.72): by the late interval most activity conducted under the code's banner is seconds negotiating apologies, boards ruling on technicalities, and manuals rehearsing technique for meetings that will not occur — performance substituting for function. Accessibility_collapse (0.55) is moderate: alternatives to a meeting — apology, litigation, public explanation, quiet endurance — always existed and grew steadily cheaper, which is precisely the drop reading's mechanism; the code never closed the exit door, it priced the stay. Resistance (0.60) is substantial and organized: refusals, religious objection, satire, statute, prosecution. All three series run on one shared grid (points every 20 years across 0-120) so every metric is authored at every examined point. The drift is a ratchet, not a cycle — wars briefly revive practice (Napoleonic-era officer meetings) but the envelope moves one way, so no oscillation is modeled. Claim and metrics are independent: claimed_type records my structural judgment that the end-state arrangement is an atrophied institution held up by inertia and ceremony, administered by custodians who could retire it at a cost to themselves exceeding anything it now costs them to keep; the engine computes per-seat types from the structural data.
 *
 * PERSPECTIVAL GAP:
 *   Three seats inhabit the same code and should compute different types. From the custodian seat the code is a sacred trust in persecuted retreat: every abandoned meeting confirms persecution, validating the drop reading from inside. From the payer seat it is a rigged gamble: rank staffs the boards, custom fixes the choice, and the class collects the order regardless of who falls. From the beneficiary class seat it is constitutive: the code is not something gentility uses but something gentility is, which is why a class that no longer fights will not say so. The engine derives these divergences from power, exit, and role data; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries sit near the subsidized end: the gentry class collects order, boundary definition, and the deterrent shadow of a possible meeting (its directionality sits low, tempered by its own buried sons and legal exposure — the beneficiary declaration plus identity-locked exit carry that mix without needing an override); fencing masters collect fees without bearing the code's demands. Declared victims sit near the target end, amplified by exit: junior officers are trapped between posting and the meeting-ground, approaching the full-target pole; bereaved families bear the settlement's irrecoverable losses with no recourse. The anti-dueling movement stands outside the transfer economy entirely — it neither collects nor pays under the code; it taxes it. Receipt is checked and diffuse: no named seat captures what the code's settlements produce — order and boundary accrue class-wide, custodial authority is administrative prestige rather than captured yield, and the masters' fees are payment for service, not receipt of the compelled compliance.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — containing feud among armed equals who disdained the courts — died during the interval: states monopolized dispute settlement, the class disarmed itself, and challenge frequency collapsed even where law was lax. The arrangement outlived its mandate and persists as ceremony and inertia. Reading the whole interval through its extractive early phase would mislabel a decaying institution as a going concern; reading its genuine coordination function as enduring virtue would miss the compulsion that did the extracting. The end-state reading turns on the cost asymmetry: the custodians could retire the code by regulation, but the cost to their authority exceeds anything the idle code now costs them, and no seat captures its yield (receipt diffuse, fixing prohibitive for the seats that could fix it). With founding_problem_status dead against a world_rearranges disappearance verdict, the mismatch consumer should flag this arrangement as a zombie — which is the honest terminal state of the drop reading's retained legitimacy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This story instantiates the drop_reading of kernel honor_violence_legitimacy — that dueling stayed structurally legitimate while external costs collapsed its practice. Is that the right reading of the kernel''s residual structure, or does the evidence favor contraction_reading (honor redefined until violent satisfaction became unthinkable) or composite_reading (both mechanisms operating simultaneously)?',
    'Comparative archival test: track whether code texts, challenge protocols, and courts of honor remained operative and were invoked after meetings grew rare (supports this reading), whether honor vocabulary itself shifted to exclude violent satisfaction (supports contraction), or whether both signatures appear (supports composite).',
    'If contraction holds, this story''s epsilon referent — a live-but-idle code — misdescribes the arrangement, and the victim set belongs to a differently structured constraint; if composite holds, epsilon decomposes into two linked stories rather than one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of the honor-violence-legitimacy kernel this story''s structure instantiates.').

omega_variable(
    residual_threat_value_vs_vestigiality,
    'Does the surviving availability of the challenge retain coercive threat-value — disciplining conduct inside the honor class because a meeting remains possible — or is the idle code purely vestigial performance?',
    'Trace insult-handling episodes late in the interval: if alleged insults still trigger formal preliminary exchanges and extracted apologies specifically because a meeting remains possible, threat-value persists; if episodes route to litigation, ridicule, or indifference, the code is vestigial.',
    'Persistent threat-value adds a diffuse speech-and-conduct toll the scalar metrics undercount and pulls the end-state back toward a coordination-plus-compulsion hybrid; pure vestigiality confirms the atrophied reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_threat_value_vs_vestigiality, empirical, 'Whether the idle code retains deterrent function or is purely ceremonial.').

omega_variable(
    counterfactual_resumption_test,
    'Was the code''s retained legitimacy substantive — would meetings resume where external costs lifted — or nominal, a form kept because nobody tested it?',
    'Natural experiments where enforcement relaxed: amnesty periods, colonial postings beyond easy statute reach, regiments slow to prosecute — did challenge frequency rebound where the price fell?',
    'Resumption validates the drop reading''s central claim and a higher underlying willingness to fight; non-resumption indicates the reading overstates retained legitimacy and the arrangement was already contracting from inside.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_resumption_test, empirical, 'Whether retained legitimacy was substantive or nominal.').

omega_variable(
    payer_coalition_potential,
    'Could the compelled participants — junior officers and challenged gentlemen — have broken the sanction by acting together, since a coordinated mass refusal would make posting unenforceable, and what stopped them?',
    'Examine refusal waves and their aftermaths: regimental petitions, mass refusals after notorious deaths, and whether collective refusal drew sanction or collapsed enforcement.',
    'Viable coalition power cuts the effective price of exit for the payer seat and hastens the atrophy; its absence shows identity-lock and hierarchy doing enforcement work the boards could no longer do alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(payer_coalition_potential, empirical, 'Whether payers held latent coalition power against the code''s sanctions.').

omega_variable(
    honor_identity_internalization_depth,
    'Was the code''s hold on its subjects structural — career, club, and marriage sanctions administered by boards and committees — or internalized, with honor fused into identity so deeply that exit was unthinkable even where sanctions had faded?',
    'Post-exit trajectories: men who left the army or emigrated and then faced challenges or insults — did the code''s demands follow them where no board could reach, or lapse with the sanctions?',
    'If internalized, the code''s effective hold exceeds what its enforcement machinery alone explains, and the end-state carries more live compulsion than the suppression scalar registers; if structural, the machinery''s decay measures the hold''s decay directly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honor_identity_internalization_depth, empirical, 'Structural versus internalized mechanism behind the code''s hold on its subjects.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__drop_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(honor_drop_tr_t0, honor_violence_legitimacy__drop_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(honor_drop_tr_t20, honor_violence_legitimacy__drop_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement(honor_drop_tr_t40, honor_violence_legitimacy__drop_reading, theater_ratio, 40, 0.34).
narrative_ontology:measurement(honor_drop_tr_t60, honor_violence_legitimacy__drop_reading, theater_ratio, 60, 0.47).
narrative_ontology:measurement(honor_drop_tr_t80, honor_violence_legitimacy__drop_reading, theater_ratio, 80, 0.57).
narrative_ontology:measurement(honor_drop_tr_t100, honor_violence_legitimacy__drop_reading, theater_ratio, 100, 0.66).
narrative_ontology:measurement(honor_drop_tr_t120, honor_violence_legitimacy__drop_reading, theater_ratio, 120, 0.72).

% Extraction over time
narrative_ontology:measurement(honor_drop_be_t0, honor_violence_legitimacy__drop_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(honor_drop_be_t20, honor_violence_legitimacy__drop_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(honor_drop_be_t40, honor_violence_legitimacy__drop_reading, base_extractiveness, 40, 0.61).
narrative_ontology:measurement(honor_drop_be_t60, honor_violence_legitimacy__drop_reading, base_extractiveness, 60, 0.53).
narrative_ontology:measurement(honor_drop_be_t80, honor_violence_legitimacy__drop_reading, base_extractiveness, 80, 0.44).
narrative_ontology:measurement(honor_drop_be_t100, honor_violence_legitimacy__drop_reading, base_extractiveness, 100, 0.36).
narrative_ontology:measurement(honor_drop_be_t120, honor_violence_legitimacy__drop_reading, base_extractiveness, 120, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(honor_drop_su_t0, honor_violence_legitimacy__drop_reading, suppression_requirement, 0, 0.76).
narrative_ontology:measurement(honor_drop_su_t20, honor_violence_legitimacy__drop_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(honor_drop_su_t40, honor_violence_legitimacy__drop_reading, suppression_requirement, 40, 0.63).
narrative_ontology:measurement(honor_drop_su_t60, honor_violence_legitimacy__drop_reading, suppression_requirement, 60, 0.55).
narrative_ontology:measurement(honor_drop_su_t80, honor_violence_legitimacy__drop_reading, suppression_requirement, 80, 0.47).
narrative_ontology:measurement(honor_drop_su_t100, honor_violence_legitimacy__drop_reading, suppression_requirement, 100, 0.4).
narrative_ontology:measurement(honor_drop_su_t120, honor_violence_legitimacy__drop_reading, suppression_requirement, 120, 0.34).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__drop_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_violence_legitimacy__drop_reading, honor_violence_legitimacy__contraction_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__drop_reading, honor_violence_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% Decomposition of the colloquial label 'the decline of dueling' per the epsilon-invariance principle: the drop reading (this file) locates the decline in price — the code's structure survives, practice is priced out; the contraction reading locates it in meaning — honor is redefined until violent satisfaction is excluded, yielding a different victim set and a different epsilon referent; the composite reading asserts both mechanisms ran simultaneously. The stories are linked pairwise through network.affects_constraints; the drop reading is the empirically conservative member, changing the least about the code's structure and attributing the decline to external cost rather than internal redefinition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
