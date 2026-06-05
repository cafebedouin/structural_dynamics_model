% ============================================================================
% CONSTRAINT STORY: criminal_procedure_amendments__seventh_amendment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_seventh_amendment, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: criminal_procedure_amendments__seventh_amendment
 *   human_readable: Seventh Amendment Civil Jury Preservation
 *   domain: political/legal
 *
 * SUMMARY:
 *   The Seventh Amendment preserves the civil jury as a structural check on
 *   judicial consolidation of power in private disputes. This reading of the
 *   criminal-procedure-amendments kernel instantiates one foundational
 *   commitment: that lay fact-finding is a legitimate and necessary
 *   constraint on judicial authority in civil matters. The constraint
 *   exhibits the full range of DR types from different structural positions.
 *   Civil litigants denied jury trial face pure extraction (snare); corporate
 *   repeat litigants experience it as coordination (rope); ordinary litigants
 *   face a mix (tangled rope); trial courts experience it as institutional
 *   theater (piton); civil justice reformers see it as a temporary solution
 *   being eroded by procedure (scaffold); and the civilizational view risks
 *   naturalizing it as an immutable law of legitimate governance (false
 *   summit). The key structural dynamic is that the jury's salience and
 *   legitimacy are declining: jury trial rates have fallen from ~15% of civil
 *   cases in 1960 to ~2% in 2020, driven by summary judgment doctrine,
 *   settlement incentives, and procedural gatekeeping (Daubert expert
 *   exclusions, fee-shifting rules). The constitutional preserve is formally
 *   intact but functionally eroding through doctrinal innovation rather than
 *   formal amendment. This makes the constraint a diagnostic exemplar of how
 *   procedures can sunset constitutional rights without repealing them.
 *
 * KEY AGENTS:
 *   - Civil Litigants (Plaintiffs and Defendants): Beneficiaries (nominal), victims (actual when denied jury access) — structural position depends on whether they have arbitrage (corporate) or are trapped in bench trial.
 *   - Trial Court Judiciary: Institutional actor (institutional/arbitrage) — experiences jury administration as procedural overhead and theater; benefits from summary judgment gatekeeping that narrows jury access.
 *   - Civil Justice Reform Movement: Organized agents (organized/mobile) — legal scholars, procedural innovators, ADR advocates building alternative fact-finding mechanisms (experts, special masters) that substitute for jury judgment.
 *   - Judicial Economy: Victim (powerless/trapped) — abstract collective good; jury trials consume resources (time, jury administration costs); beneficiaries of summary judgment and efficiency doctrine.
 *   - Lay Fact-Finding Institutions: Beneficiary (moderate/constrained) — juries as repositories of community judgment; their legitimacy and role are under pressure from expert-dominated adjudication in complex cases.
 *   - Analytical Observer: Perspective holder (analytical/analytical) — at civilizational scope, risks naturalizing the jury as immutable rather than recognizing it as a contingent institutional choice.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(criminal_procedure_amendments__seventh_amendment, 0.38).
domain_priors:suppression_score(criminal_procedure_amendments__seventh_amendment, 0.52).
domain_priors:theater_ratio(criminal_procedure_amendments__seventh_amendment, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(criminal_procedure_amendments__seventh_amendment, extractiveness, 0.38).
narrative_ontology:constraint_metric(criminal_procedure_amendments__seventh_amendment, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(criminal_procedure_amendments__seventh_amendment, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(criminal_procedure_amendments__seventh_amendment, tangled_rope).
narrative_ontology:human_readable(criminal_procedure_amendments__seventh_amendment, "Seventh Amendment Civil Jury Preservation").
narrative_ontology:topic_domain(criminal_procedure_amendments__seventh_amendment, "political/legal").

domain_priors:requires_active_enforcement(criminal_procedure_amendments__seventh_amendment).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(criminal_procedure_amendments__seventh_amendment, '59cb8690-8a21-49fc-ab03-d2c645d08be4').
narrative_ontology:cs_kernel_codification('59cb8690-8a21-49fc-ab03-d2c645d08be4', formalized).
narrative_ontology:cs_authority_grounding('59cb8690-8a21-49fc-ab03-d2c645d08be4', lineage).
narrative_ontology:cs_interpretation_layer_present('59cb8690-8a21-49fc-ab03-d2c645d08be4').
narrative_ontology:cs_reading_relation('59cb8690-8a21-49fc-ab03-d2c645d08be4', criminal_procedure_amendments__fourth_amendment, coexists_with).
narrative_ontology:cs_reading_relation('59cb8690-8a21-49fc-ab03-d2c645d08be4', criminal_procedure_amendments__fifth_amendment, coexists_with).
narrative_ontology:cs_reading_relation('59cb8690-8a21-49fc-ab03-d2c645d08be4', criminal_procedure_amendments__sixth_amendment, influences).
narrative_ontology:cs_reading_relation('59cb8690-8a21-49fc-ab03-d2c645d08be4', criminal_procedure_amendments__eighth_amendment, coexists_with).
narrative_ontology:cs_axiom('59cb8690-8a21-49fc-ab03-d2c645d08be4', foundational, lay_fact_finding_legitimacy).
narrative_ontology:cs_axiom_status(lay_fact_finding_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('59cb8690-8a21-49fc-ab03-d2c645d08be4', lay_fact_finding_legitimacy, deontological).
narrative_ontology:cs_axiom('59cb8690-8a21-49fc-ab03-d2c645d08be4', foundational, jury_trial_check_on_consolidation).
narrative_ontology:cs_axiom_status(jury_trial_check_on_consolidation, holdable).
narrative_ontology:cs_axiom_grounding('59cb8690-8a21-49fc-ab03-d2c645d08be4', jury_trial_check_on_consolidation, deontological).
narrative_ontology:cs_axiom('59cb8690-8a21-49fc-ab03-d2c645d08be4', secondary, civil_jury_scope_noncontractible).
narrative_ontology:cs_axiom_status(civil_jury_scope_noncontractible, overridden).
narrative_ontology:cs_axiom_grounding('59cb8690-8a21-49fc-ab03-d2c645d08be4', civil_jury_scope_noncontractible, conventional).
narrative_ontology:cs_reference_frame('59cb8690-8a21-49fc-ab03-d2c645d08be4', constitutional_jury_preservation).
narrative_ontology:cs_drift_state('59cb8690-8a21-49fc-ab03-d2c645d08be4', contemporary_procedural_erosion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('59cb8690-8a21-49fc-ab03-d2c645d08be4', '').
narrative_ontology:cs_kernel_id(criminal_procedure_amendments__seventh_amendment, criminal_procedure_amendments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(criminal_procedure_amendments__seventh_amendment, civil_litigants).
narrative_ontology:constraint_beneficiary(criminal_procedure_amendments__seventh_amendment, lay_fact_finding_institutions).
narrative_ontology:constraint_victim(criminal_procedure_amendments__seventh_amendment, judicial_economy).
narrative_ontology:constraint_victim(criminal_procedure_amendments__seventh_amendment, bench_adjudication_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEFENDANT DENIED JURY (SNARE) — A civil defendant denied jury trial where the Seventh Amendment nominally preserves it faces judicial consolidation of factfinding power. The judge acts as sole arbiter of law and fact, with no external check on bias or error. Exit is trapped: the right is statutory/constitutional, not contractible away in litigation once commenced. Maximum experienced extraction for those locked into bench proceedings.
constraint_indexing:constraint_classification(criminal_procedure_amendments__seventh_amendment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CORPORATE REPEAT LITIGANT (ROPE) — Large corporations with repeated litigation experience can navigate jury vs bench choice strategically: retain elite counsel, request bench trials in complex commercial disputes, leverage specialized judges in commercial dockets. The jury constraint is coordination (predictability, standardization) rather than pure extraction. Arbitrage exit: pay for counsel to exploit doctrinal edges.
constraint_indexing:constraint_classification(criminal_procedure_amendments__seventh_amendment, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: ORDINARY LITIGANT (TANGLED ROPE) — Civil parties with moderate resources experience the jury requirement as both coordination (legitimacy, lay perspective on facts) and extraction (cost of jury trial, unpredictability, delay). Cannot fully exit (constitutional right/duty) but can partially avoid through settlement or procedural choice. Constrained exit: can negotiate jury waivers in some contexts, but constitutional preservation limits this.
constraint_indexing:constraint_classification(criminal_procedure_amendments__seventh_amendment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: TRIAL COURT JUDICIARY (PITON) — Federal and state trial judges experience jury administration as performative overhead: jury selection theater, jury instructions (often formulaic), jury deliberation black boxes that reverse outcomes on appeal. The judiciary has largely internalized jury trial as procedural ritual, not as a check on judicial power. Sees the requirement as institutional inertia: maintained because the Constitution says so and because alternatives haven't fully replaced it, not because juries reliably improve factfinding. The judge's power is checked formally but the lived experience is often theater.
constraint_indexing:constraint_classification(criminal_procedure_amendments__seventh_amendment, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CIVIL JUSTICE REFORM MOVEMENT (SCAFFOLD) — Organized legal reformers (empirical researchers, procedural scholars, judge advocates) view jury trial as a temporary institutional solution to a coordination problem—lay fact-finding—that is being replaced by alternative mechanisms: court-appointed experts, special masters, ADR, summary judgment doctrine. The jury preservation requirement is being functionally eroded through procedural rules (summary judgment gates, Daubert expert gatekeeping) without formal amendment. This is a sunset in progress: the jury's salience as a fact-finder is declining, and the constraint's enforcement is weakening through doctrinal innovation.
constraint_indexing:constraint_classification(criminal_procedure_amendments__seventh_amendment, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the jury trial represents an immutable structural limit on consolidated state power: lay citizens in fact-finding roles are a logical/natural-law constraint on judicial monopoly. No coherent legal system can function without some check on adjudicative power; the jury is the irreducible minimum such check. This perspective naturalizes the jury as a foundational feature of legitimate law. However, the structural data contradicts this: the jury is a contingent institutional choice with identifiable beneficiaries and victims, making it a false summit candidate.
constraint_indexing:constraint_classification(criminal_procedure_amendments__seventh_amendment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(criminal_procedure_amendments__seventh_amendment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(criminal_procedure_amendments__seventh_amendment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(criminal_procedure_amendments__seventh_amendment, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(criminal_procedure_amendments__seventh_amendment, TR),
    TR >= 0.70.

:- end_tests(criminal_procedure_amendments__seventh_amendment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-low. The core extractiveness comes from the asymmetry between judicial consolidation (judge as sole fact-finder) and the constitutional preservation of lay judgment. Where juries are actually empaneled and function, extractiveness is low—the jury checks judicial power. But extractiveness rises where jury trials are procedurally gatekept (summary judgment, Daubert) or where litigants cannot afford to invoke jury rights (fee-shifting, resource barriers). The rising trajectory (0.22 → 0.38) reflects the doctrinal erosion of jury trial access over 100 years without formal amendment. Suppression (0.52): Moderate. Formal barriers to jury trial are modest (constitutional preserve is clear), but practical barriers are rising: summary judgment motion success rates, Daubert gatekeeping of experts, settlement pressure, cost of jury trial (jury selection, bifurcated discovery, jury instructions). These barriers suppress jury trial access without formally suppressing the constitutional right. Theater ratio (0.58): Moderate-high. Modern jury procedures are substantially performative: jury selection follows formulaic patterns; jury instructions are often boilerplate language with limited cognitive impact on actual fact-finding; judges constrain jury scope through directed verdict and judgment as a matter of law; expert testimony gatekeeping (Daubert) means lay juries are often deciding disputes about expert qualification rather than underlying facts. The theater has increased over time as judges internalized jury administration as overhead rather than as a genuine check on judicial power.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates divergent classification across structural positions. The powerless defendant denied jury faces snare (pure extraction with no exit). The corporate litigant faces rope (coordination with exit options). The ordinary litigant faces tangled rope (mixed coordination and extraction). The trial judge faces piton (theater and institutional inertia). The reform movement faces scaffold (temporary, sunsetting mechanism being replaced by alternatives). The civilizational analytical view risks mountain (naturalizing jury as immutable) but the structural data reveals false summit: beneficiaries and victims are identifiable, extractiveness has risen with procedural erosion, and the suppression is not unchangeable—it is maintained through doctrine, not through physical law. The perspectival gap reflects that what appears as 'lay fact-finding' to civil litigants appears as 'inefficiency overhead' to judges and 'outdated procedure' to reformers. No single type captures the constraint; the presheaf of classifications across contexts is the answer.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is derived from structural position: civil litigants denied jury access (powerless/trapped) experience high d (they are targets of the extraction; judicial consolidation runs toward them). Corporate repeat litigants (powerful/arbitrage) experience low d (they are beneficiaries of jury navigation and arbitrage; they can game the system). Trial judges (institutional/arbitrage) experience low d (they benefit from procedural gatekeeping that narrows jury access, even if formal Constitution preserves jury trial). Ordinary litigants (moderate/constrained) experience moderate d (constrained exit—they can settle but cannot avoid the jury requirement if they litigate; costs and benefits are mixed). Reform advocates (organized/mobile) experience low d (they have agency to shift the constraint toward expert alternatives; they are not trapped). The analytical observer (analytical/analytical) experiences moderate d as a derived value (observer sees full structure without being extracted from or extracted to). The chi formula χ = ε × f(d) × σ(S) produces higher effective extractiveness for trapped agents (high f(d)) and lower for beneficiaries (low/negative f(d)). Scope modifier for national is 1.0, so no scalar adjustment.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED through perspectival differentiation. The mandatrophy question is: Is the jury trial constraint a check on judicial power (coordination function, rope), an extraction mechanism (snare), a degraded ritual (piton), or an immutable natural law (mountain)? The answer is context-dependent. For the defendant locked into bench trial, it is snare (the check failed, and they experience extraction). For the corporate litigant with counsel, it is rope (the jury is a standard to navigate). For the judiciary, it is piton (maintained as ritual, not as effective constraint). For reformers, it is scaffold (temporary, being replaced). For the civilizational observer, it is false-summit mountain (naturalized but actually contingent). The constraint resolves mandatrophy by instantiating all six types coherently across its observation site. No type is wrong; all are structurally accurate from their respective positions. The unified answer is the presheaf: the constraint is a tangled_rope at the analytical level (moderate extractiveness, moderate suppression, mixed coordination and extraction), with perspectival deviation into snare, rope, piton, scaffold, and false-summit mountain depending on agent power, exit options, and time horizon.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    jury_efficacy_vs_symbolism,
    'Does the modern jury trial actually constrain judicial power through fact-finding independence, or does it serve primarily as a symbolic legitimation of outcomes the judge has already shaped through procedural gatekeeping (summary judgment, Daubert, jury instructions)?',
    'Empirical analysis of jury verdict rates pre/post-summary judgment motion; comparison of judge and jury outcomes on identical fact patterns; analysis of reversal rates on appeal; jury comprehension studies of instructions.',
    'If juries meaningfully constrain outcomes: the constraint prevents judicial consolidation (primary function). If juries are largely symbolic: the constraint''s extractiveness shifts upward (ineffective check becomes performative obligation), and the piton classification becomes dominant.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(jury_efficacy_vs_symbolism, empirical, 'Whether jury trial meaningfully constrains judicial power or serves primarily symbolic function').

omega_variable(
    doctrinal_erosion_sunset,
    'Is the Seventh Amendment constraint functionally sunsetting through procedural doctrine (summary judgment expansion, Daubert expert gatekeeping, fee-shifting rules) without formal amendment?',
    'Historical trend analysis: jury trial rates over 50 years; proportion of cases reaching jury vs. resolved on summary judgment motion; doctrinal shifts that gate jury access; empirical tracking of whether procedural rules are intentionally narrowing jury trial scope.',
    'If sunset is real and intentional: the scaffold perspective is correct, and the constraint is experiencing deliberate functional erosion. If procedural rules are doctrine-neutral: the constraint remains structurally active despite reduced jury trial rates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(doctrinal_erosion_sunset, empirical, 'Whether procedural doctrine is intentionally narrowing jury trial access without formal amendment').

omega_variable(
    lay_judgment_legitimacy_shift,
    'Has the normative legitimacy of lay fact-finding declined as litigation has become more complex (securities, patent, technical torts), making expert-dominated adjudication (special masters, court-appointed experts, magistrate judges) more legitimate than jury judgment?',
    'Doctrinal analysis of expert testimony gatekeeping (Daubert evolution); empirical tracking of judge vs. expert vs. jury role in complex litigation; survey data on lawyer and judge attitudes toward jury competence; analysis of complexity exceptions to jury trial rights.',
    'If legitimacy has shifted: the Seventh Amendment constraint is subject to credibility erosion (the axiom that lay judgment is legitimate is being overridden by practice). If lay judgment remains legitimate: the constraint''s authority grounding is intact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lay_judgment_legitimacy_shift, conceptual, 'Whether lay fact-finding legitimacy has declined relative to expert adjudication in complex disputes').

omega_variable(
    amendment_reading_contest,
    'Does the Seventh Amendment reading coexist with, foreclose, or influence sibling readings (Fourth, Fifth, Sixth, Eighth)? Do these readings share a common theory of lay participation in criminal/civil justice, or are they fundamentally distinct checks on state power?',
    'Doctrinal analysis of how judges apply Seventh Amendment jury trial rights in light of parallel rights in Fourth/Fifth/Sixth/Eighth; empirical tracking of whether expansion in one amendment correlates with expansion in others; constitutional interpretation case law on whether amendments are unified theory or distinct mechanisms.',
    'If readings coexist: each amendment independently entrenches a distinct procedural check. If one foreclose another: constitutional coherence may require choosing a unified theory. If they influence each other: expansions in one amendment (e.g., Sixth Amendment jury scope) pressure others.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_reading_contest, conceptual, 'Structural relationship between Seventh Amendment reading and sibling Bill of Rights readings').

omega_variable(
    false_summit_benign_naturalization,
    'Is the civilizational/analytical mountain perspective a benign recognition of a genuine structural limit on state power, or a dangerous naturalization of a contingent institutional choice that obscures ongoing contestation about jury scope and legitimacy?',
    'Historical analysis: is jury trial competence actively defended in doctrinal debate, or treated as settled? Are there current movements to abolish/restrict jury trial rights (explicit or implicit through procedure)? Does naturalizing jury trial as ''law of nature'' prevent recognition of its declining real-world salience?',
    'If benign naturalization: the mountain perspective captures a true structural limit. If dangerous: the false summit detector correctly identifies that the ''naturalness'' of jury trial masks ongoing institutional power struggles and procedural erosion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_benign_naturalization, conceptual, 'Whether civilizational mountain perspective naturalizes a contingent institutional choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(criminal_procedure_amendments__seventh_amendment, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seventh_theater_t0, criminal_procedure_amendments__seventh_amendment, theater_ratio, 0, 0.35).
narrative_ontology:measurement(seventh_theater_t50, criminal_procedure_amendments__seventh_amendment, theater_ratio, 50, 0.48).
narrative_ontology:measurement(seventh_theater_t100, criminal_procedure_amendments__seventh_amendment, theater_ratio, 100, 0.58).

% Extraction over time
narrative_ontology:measurement(seventh_extractiveness_t0, criminal_procedure_amendments__seventh_amendment, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(seventh_extractiveness_t50, criminal_procedure_amendments__seventh_amendment, base_extractiveness, 50, 0.32).
narrative_ontology:measurement(seventh_extractiveness_t100, criminal_procedure_amendments__seventh_amendment, base_extractiveness, 100, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(seventh_suppression_t0, criminal_procedure_amendments__seventh_amendment, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(seventh_suppression_t50, criminal_procedure_amendments__seventh_amendment, suppression_requirement, 50, 0.5).
narrative_ontology:measurement(seventh_suppression_t100, criminal_procedure_amendments__seventh_amendment, suppression_requirement, 100, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(criminal_procedure_amendments__seventh_amendment, enforcement_mechanism).
narrative_ontology:affects_constraint(criminal_procedure_amendments__seventh_amendment, fourth_amendment).
narrative_ontology:affects_constraint(criminal_procedure_amendments__seventh_amendment, fifth_amendment).
narrative_ontology:affects_constraint(criminal_procedure_amendments__seventh_amendment, sixth_amendment).
narrative_ontology:affects_constraint(criminal_procedure_amendments__seventh_amendment, eighth_amendment).

% DUAL FORMULATION NOTE:
% The seventh_amendment reading is part of the criminal_procedure_amendments kernel family. Each sibling reading (fourth, fifth, sixth, eighth) instantiates a distinct structural check on state power. The seventh_amendment is distinguished by its focus on lay fact-finding distribution in civil disputes; the sibling readings address intrusion (fourth), prosecution power (fifth), trial machinery (sixth), and punishment severity (eighth). These are not the same constraint viewed from different angles—they are distinct mechanisms serving different functions. However, they are unified by the commitment-system kernel: a constitutional order in which lay participation and procedural constraints limit state power. The network links indicate structural influence: expansion of jury rights in one amendment (e.g., broader Sixth Amendment jury scope) may pressure or influence the scope of Seventh Amendment jury rights in civil cases.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(criminal_procedure_amendments__seventh_amendment, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
