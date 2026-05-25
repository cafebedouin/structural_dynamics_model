% ============================================================================
% CONSTRAINT STORY: media_trial_contamination
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_media_trial_contamination, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: media_trial_contamination
 *   human_readable: Media Trial Contamination of Judicial Fairness
 *   domain: legal/media/institutional
 *
 * SUMMARY:
 *   Media trial contamination describes the structural constraint in which
 *   pretrial media coverage of high-profile cases creates cognitive
 *   contamination of judge and jury, undermining the fair trial guarantee.
 *   The constraint exhibits genuine coordination function (public right to
 *   information about judicial proceedings) alongside extractive mechanisms
 *   (prosecution narrative control, media sensationalism, attention economy
 *   incentives). The defendant and jury are trapped — they cannot exit
 *   without forfeiting trial rights. Prosecution and media organizations are
 *   beneficiaries constrained by legal rules but actively enforcing narrative
 *   advantage. The judicial system has theoretical exit options through
 *   mistrial and venue change, but these remedies may be insufficient. First
 *   Amendment doctrine performs neutrality while structural incentives
 *   determine outcomes, making it a piton—maintained through institutional
 *   inertia rather than functional effectiveness. The constraint's theater
 *   ratio (0.68) reflects that much trial publicity is performative rather
 *   than genuinely informative: sensationalism, narrative framing, and
 *   selective emphasis replace balanced reporting. The extractiveness value
 *   (0.58) reflects moderate but serious contamination of fair trial
 *   rights—extraction is substantial but not total, because some cases remain
 *   unaffected by media pressure and some mistrial remedies do function.
 *
 * KEY AGENTS:
 *   - Defendant: Primary victim (powerless/trapped) — bears reputation destruction, prejudgment, loss of presumption of innocence; cannot exit
 *   - Jury: Primary victim (powerless/trapped) — conscripted and contaminated; epistemic integrity compromised; cannot unhear prejudicial narratives
 *   - Prosecution Agencies: Primary beneficiary (organized/constrained) — controls information flow to media; benefits from narrative advantage during pretrial period
 *   - Media Organizations: Institutional beneficiary (institutional/constrained) — profits from engagement and sensationalism; gains audience through trial coverage; constrained by defamation law and journalistic ethics
 *   - Judicial System: Institutional actor (institutional/arbitrage) — has theoretical exit options through mistrial, venue change, gag orders; can manage contamination through procedural mechanisms
 *   - First Amendment Doctrine: Institutional framework (institutional/arbitrage) — maintains fiction that free press and fair trial are compatible; enforces through procedural mechanisms that may not work
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(media_trial_contamination, 0.58).
domain_priors:suppression_score(media_trial_contamination, 0.65).
domain_priors:theater_ratio(media_trial_contamination, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(media_trial_contamination, extractiveness, 0.58).
narrative_ontology:constraint_metric(media_trial_contamination, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(media_trial_contamination, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(media_trial_contamination, tangled_rope).
narrative_ontology:human_readable(media_trial_contamination, "Media Trial Contamination of Judicial Fairness").
narrative_ontology:topic_domain(media_trial_contamination, "legal/media/institutional").

domain_priors:requires_active_enforcement(media_trial_contamination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(media_trial_contamination, media_organizations).
narrative_ontology:constraint_beneficiary(media_trial_contamination, prosecution_agencies).
narrative_ontology:constraint_victim(media_trial_contamination, defendant_fair_trial_right).
narrative_ontology:constraint_victim(media_trial_contamination, judicial_impartiality).
narrative_ontology:constraint_victim(media_trial_contamination, jury_epistemic_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE DEFENDANT (SNARE) — Trapped by judicial geography and unavoidable media exposure. Cannot exit the constraint without forfeiting trial. Bears maximum extraction: narrative prejudgment, reputation destruction, and loss of presumption of innocence. No alternatives, no exit routes. Maximum experienced extractiveness.
constraint_indexing:constraint_classification(media_trial_contamination, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE JURY (SNARE) — Conscripted and trapped. Subject to massive pretrial publicity, social media contamination, and community pressure. Cannot exit despite cognitive contamination. Suppressed by the media narrative and unable to unhear prejudicial information. Experiences the constraint as extraction of their epistemic integrity — forced to participate in a trial whose fairness has been undermined.
constraint_indexing:constraint_classification(media_trial_contamination, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: PROSECUTION AGENCIES (TANGLED ROPE) — Constrained by media dynamics but also benefit from narrative advantage. Media coverage can amplify public perception of prosecution strength. Genuine coordination function exists: media coverage communicates case details to public. But asymmetric extraction emerges: prosecution controls information flow to media, shaping narratives before defense can respond. Active enforcement of narrative control required. Suppression operates on media access and timing.
constraint_indexing:constraint_classification(media_trial_contamination, tangled_rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: MEDIA ORGANIZATIONS (TANGLED ROPE) — Constrained by competitive pressure to maximize engagement and first-to-publish advantage. Genuine coordination function: communicating case information to the public enables democratic oversight of trials. But extraction mechanism emerges: sensationalism, narrative framing, and selective emphasis contaminate rather than inform. Active enforcement through editorial decisions required. Beneficiaries of attention economy incentives despite constraints from defamation law.
constraint_indexing:constraint_classification(media_trial_contamination, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: THE JUDICIAL SYSTEM (ROPE) — Institutional actor with significant exit options: judges can declare mistrial, change venue, impose gag orders, manage jury selection. Sees media trial contamination as a coordination problem requiring institutional management. The constraint is a coordination mechanism for public accountability, but judges have tools to enforce boundaries. Can arbitrage between trial publicity and fairness through procedural mechanisms.
constraint_indexing:constraint_classification(media_trial_contamination, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: FIRST AMENDMENT DOCTRINE (PITON) — Maintains a performative commitment to 'free press' and 'fair trial' as compatible values. The doctrine itself has atrophied: empirical research shows media prejudgment affects jury outcomes, yet constitutional law maintains the fiction that jury nullification of media bias is possible. Theater ratio high because doctrine performs neutrality while structural incentives (attention economy, prosecution narrative control) determine outcomes. Maintained through institutional inertia rather than functional effectiveness.
constraint_indexing:constraint_classification(media_trial_contamination, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Risk of naturalizing the constraint as an inevitable feature of democratic justice: 'media coverage is inherent to public trials' or 'jury contamination is inescapable in the information age.' However, structural data contradicts mountain classification — the constraint exists through institutional design choices (prosecution information control, media business model incentives, jury selection limits) not through physical/logical necessity. Engine will compute false summit, revealing that the 'inevitability' framing naturalizes what are contingent institutional arrangements.
constraint_indexing:constraint_classification(media_trial_contamination, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(media_trial_contamination_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(media_trial_contamination, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(media_trial_contamination, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(media_trial_contamination, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(media_trial_contamination, TR),
    TR >= 0.70.

:- end_tests(media_trial_contamination_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Fair trial contamination is substantial and systematic in high-profile cases, but not universal across all trials. Media coverage intensity varies; jury contamination severity varies by community saturation. The trajectory from 0.35 to 0.58 reflects increasing media penetration and social media amplification over the measured period (likely 10-20 years). Suppression (0.65): Moderate-high. Significant barriers to uncontaminated trial: jurors are conscripted and cannot opt out; information environment is saturated; defendant cannot prevent media coverage; jury selection voir dire has limited effectiveness at eliminating narrative effects. Media narrative suppresses defendant's rebuttal capacity—prosecution speaks first and shapes frame before defense can respond. Theater ratio (0.68): Moderate-high. Trial publicity contains substantial performative elements: media dramatization rather than neutral reporting; prosecution press conferences designed to shape public narrative; defense counterspeech is reactive and less resourced. However, theater is not dominant—genuine information about trials does reach the public, and jury decisions do respond to actual case evidence not just media narratives.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates asymmetric extraction visible only through perspectival multiplicity. From the defendant's viewpoint, the constraint is pure extraction (Snare): no coordination benefit, only cognitive contamination and prejudgment. From prosecution's viewpoint, the constraint is coordination with asymmetric benefit (Tangled Rope): genuine information function (communicating case details) paired with systematic narrative advantage. From media's viewpoint, similar tangled rope: coordination function (public information) with embedded sensationalism incentive. From judicial system's viewpoint, rope: a solvable coordination problem. The perspectival gap reveals what each actor cannot see from their position: defendant cannot see that prosecution benefits structurally from the same media environment; prosecution cannot see that their information control creates epistemic contamination rather than fair trial communication; media cannot see that engagement incentives override accuracy incentives; judicial system cannot see that its remedies may be insufficient against structural incentive misalignment.
 *
 * DIRECTIONALITY LOGIC:
 *   Defendant: powerless + trapped → d ≈ 0.95 → f(d) ≈ 1.42 → maximum experienced extractiveness. Cannot exercise agency; bears full cost of narrative contamination. Jury: powerless + trapped → d ≈ 0.95 → f(d) ≈ 1.42 → maximum experienced extractiveness. Conscripted and contaminated; cannot exit. Prosecution: organized + constrained → d ≈ 0.40 → f(d) ≈ 0.40 → moderate extracted benefit. Controls information flow; benefits from narrative advantage; constrained by legal rules preventing direct trial interference. Media: institutional + constrained → d ≈ 0.35 → f(d) ≈ 0.30 → moderate extracted benefit. Profits from engagement; benefits from trial sensationalism; constrained by defamation law and journalistic ethics that are weakly enforced. Judicial system: institutional + arbitrage → d ≈ 0.10 → f(d) ≈ -0.02 → effective subsidy to judicial function. Has exit options and can arbitrage; benefits from legitimate publicity while managing contamination through procedural tools.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY PRESENT BUT UNRESOLVED: This constraint sits at extractiveness 0.58, below the 0.70 threshold requiring mandatrophy resolution. However, it demonstrates the mandatrophy at the conceptual level. The core tension: 'fair trial' requires suppression of prejudicial information (First Amendment restriction) to protect defendant; 'free press' requires publication of trial information to enable democratic accountability. Neither can fully exist without constraining the other. The false mountain (Perspective 7) naturalizes this tension as inevitable, but structural data shows it is contingent on institutional design: information asymmetry favoring prosecution, media business model incentives toward sensationalism, and jury selection procedures with limited contamination-filtering capacity. The mandatrophy resolves toward 'contingent institutional tension' rather than 'natural law': the constraint could be restructured (prosecution information parity, media literacy requirements, alternative jury selection) to reduce contamination while preserving both fairness and press freedom. No such restructuring has been attempted at scale, which is why the piton classification holds—doctrine persists through inertia, maintaining the fiction of compatibility rather than designing for genuine balance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    jury_contamination_measurement,
    'How can contamination be measured objectively when jury deliberation is private and jurors cannot articulate the sources of their beliefs?',
    'Comparison of verdict rates in high-publicity vs low-publicity cases controlling for case strength; mock jury studies; post-verdict juror interviews; change-of-venue analysis showing correlation between media saturation and verdicts',
    'If contamination is severe and systematic: snare classification holds across more perspectives. If contamination is marginal or inconsistent: tangled rope classification becomes dominant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jury_contamination_measurement, empirical, 'Measurement methodology for jury cognitive contamination').

omega_variable(
    media_prosecution_coordination,
    'Is media-prosecution narrative alignment the result of: (a) active information control by prosecution, (b) independent media editorial judgment favoring prosecution, or (c) structural incentive alignment that requires no explicit coordination?',
    'Leak analysis: tracking information asymmetries between prosecution and defense in media coverage timeline; interviews with prosecutors and journalists about information access; analysis of prosecution press releases vs media framing; comparison of convicted vs acquitted defendant media coverage patterns',
    'If active control: prosecution is deliberate beneficiary of extraction. If independent media judgment: media is autonomous beneficiary. If structural incentive alignment: both are beneficiaries of the same extraction mechanism, deepening tangled rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(media_prosecution_coordination, empirical, 'Whether prosecution-media alignment is active coordination or structural incentive').

omega_variable(
    mistrial_remedy_sufficiency,
    'Do mistrial declarations and venue changes actually remove contamination or merely reset the clock without addressing underlying structural incentives?',
    'Longitudinal study of retrials: comparing first-trial contamination severity vs retrial outcomes in cases with long delays; analysis of whether jury selection voir dire can eliminate media narrative effects; mock jury studies testing remedies',
    'If remedies work: judicial exit options are real and rope classification justified. If remedies fail: judicial system lacks genuine exit options and snare classification extends to institutional actors.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mistrial_remedy_sufficiency, empirical, 'Whether mistrial and venue change remedies eliminate contamination').

omega_variable(
    suppression_mechanism_internalization,
    'Is jury suppression of media narratives structural (external information barriers) or internalized (jurors'' own belief frameworks shaped by media exposure before trial)?',
    'Pre-trial and post-trial juror surveys on media exposure and belief formation; analysis of jurors who claim lack of exposure vs their actual media consumption; intervention studies with media literacy training before jury duty',
    'If structural: mistrial and venue change can solve the problem. If internalized: jurors carry contamination regardless of trial environment, and the constraint is categorically more severe.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether jury suppression is structural or internalized').

omega_variable(
    first_amendment_counterfactual,
    'Would restricting pretrial media coverage (gag orders, restricted access to prosecution information) reduce contamination at the cost of reducing public accountability oversight?',
    'Comparative analysis of jurisdictions with strong gag order enforcement vs permissive media access; mock jury studies comparing juror contamination vs perceived fairness across media restriction levels; public opinion surveys on acceptable trade-offs',
    'If restriction works without eliminating oversight: piton classification holds (doctrine persists despite better alternatives). If restriction creates accountability gaps: doctrine may be defending a necessary evil rather than an atrophied practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(first_amendment_counterfactual, preference, 'Trade-off between contamination reduction and democratic oversight').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(media_trial_contamination, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mediatrial_tr_t0, media_trial_contamination, theater_ratio, 0, 0.52).
narrative_ontology:measurement(mediatrial_tr_t5, media_trial_contamination, theater_ratio, 5, 0.62).
narrative_ontology:measurement(mediatrial_tr_t10, media_trial_contamination, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(mediatrial_be_t0, media_trial_contamination, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mediatrial_be_t5, media_trial_contamination, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(mediatrial_be_t10, media_trial_contamination, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(media_trial_contamination, information_standard).
narrative_ontology:affects_constraint(media_trial_contamination, wrongful_conviction_epistemic_closure).
narrative_ontology:affects_constraint(media_trial_contamination, prosecution_narrative_authority).

% DUAL FORMULATION NOTE:
% Media trial contamination is distinct from but causally upstream of wrongful conviction formation. The contamination constraint has its own extractiveness value (0.58) reflecting the severity of fair trial violation; wrongful conviction has higher extractiveness (0.75+) reflecting the finality of false imprisonment. Media narrative authority is a complementary constraint (extraction flowing from media's control of public discourse) with different ε reflecting different harm mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
