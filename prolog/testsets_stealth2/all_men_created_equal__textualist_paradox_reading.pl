% ============================================================================
% CONSTRAINT STORY: all_men_created_equal__textualist_paradox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_all_men_created_equal__textualist_paradox_reading, []).

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
 *   constraint_id: all_men_created_equal__textualist_paradox_reading
 *   human_readable: Textualist Paradox Reading of the Equality Clause
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the textualist_paradox_reading of the kernel
 *   all_men_created_equal: the claim that a polity announcing universal
 *   equality in its founding text while applying the principle on restricted
 *   scope is caught in a performative contradiction, and that the
 *   contradiction transfers authority from the restrictors to the excluded.
 *   The standing arrangement under contest is the regime of founder-intent
 *   authority claims — invocations of the founding texts' universal language
 *   in support of narrowed application — and epsilon is authored for THAT
 *   arrangement as this reading sees it: moderately extractive, because the
 *   arrangement funds contemporary authority with a universality it declines
 *   to honor, and self-exposing, because each invocation supplies the
 *   material for its own impeachment. FAMILY NOTE (epsilon-invariance): the
 *   kernel decomposes into three readings —
 *   all_men_created_equal__originalist_reading, this
 *   textualist_paradox_reading, and
 *   all_men_created_equal__universalist_reading — each a separate constraint
 *   file with its own epsilon, victim set, and classification. This file's
 *   epsilon is indexed to the paradox's operation on originalist authority
 *   claims, not to the bounded-application arrangement the originalist
 *   reading defends nor to the expansion program the universalist reading
 *   endorses; the sibling files are linked through
 *   network.affects_constraints. KEY AGENTS (by structural relationship): -
 *   founders_intent_interpreters: Primary target (institutional/constrained)
 *   — bears the contradiction charge on each restricted invocation -
 *   restrictionist_text_invokers: Secondary target (powerful/constrained) —
 *   pays in rhetorical capital - abolitionist_advocates: Historical primary
 *   beneficiary (organized/constrained) - civil_rights_litigants: Recurring
 *   beneficiary (moderate/constrained) - excluded_class_members: Principal
 *   beneficiary (powerless/trapped) — whose exclusion the contradiction names
 *   - constitutional_law_professoriate: Agenda-setter and residual
 *   beneficiary (institutional/mobile) - legal_historians: Analytical
 *   observer (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(all_men_created_equal__textualist_paradox_reading, 0.55).
domain_priors:suppression_score(all_men_created_equal__textualist_paradox_reading, 0.4).
domain_priors:theater_ratio(all_men_created_equal__textualist_paradox_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(all_men_created_equal__textualist_paradox_reading, tangled_rope).
narrative_ontology:human_readable(all_men_created_equal__textualist_paradox_reading, "Textualist Paradox Reading of the Equality Clause").
narrative_ontology:topic_domain(all_men_created_equal__textualist_paradox_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(all_men_created_equal__textualist_paradox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(all_men_created_equal__textualist_paradox_reading, '3bb1b8e4-a2df-48f6-8f1d-fc2c932390d9').
narrative_ontology:cs_kernel_codification('3bb1b8e4-a2df-48f6-8f1d-fc2c932390d9', fixed_text).
narrative_ontology:cs_authority_grounding('3bb1b8e4-a2df-48f6-8f1d-fc2c932390d9', lineage).
narrative_ontology:cs_interpretation_layer_present('3bb1b8e4-a2df-48f6-8f1d-fc2c932390d9').
narrative_ontology:cs_reading_relation('3bb1b8e4-a2df-48f6-8f1d-fc2c932390d9', all_men_created_equal__originalist_reading, influences).
narrative_ontology:cs_reading_relation('3bb1b8e4-a2df-48f6-8f1d-fc2c932390d9', all_men_created_equal__universalist_reading, coexists_with).
narrative_ontology:cs_axiom('3bb1b8e4-a2df-48f6-8f1d-fc2c932390d9', foundational, universal_language_self_condemns_restriction).
narrative_ontology:cs_axiom_status(universal_language_self_condemns_restriction, holdable).
narrative_ontology:cs_axiom_grounding('3bb1b8e4-a2df-48f6-8f1d-fc2c932390d9', universal_language_self_condemns_restriction, conventional).
narrative_ontology:cs_axiom('3bb1b8e4-a2df-48f6-8f1d-fc2c932390d9', secondary, textual_standing_for_excluded_claimants).
narrative_ontology:cs_axiom_status(textual_standing_for_excluded_claimants, holdable).
narrative_ontology:cs_axiom_grounding('3bb1b8e4-a2df-48f6-8f1d-fc2c932390d9', textual_standing_for_excluded_claimants, conventional).
narrative_ontology:cs_reference_frame('3bb1b8e4-a2df-48f6-8f1d-fc2c932390d9', face_value_universal_text).
narrative_ontology:cs_drift_state('3bb1b8e4-a2df-48f6-8f1d-fc2c932390d9', contemporary_restricted_application_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3bb1b8e4-a2df-48f6-8f1d-fc2c932390d9', '').
narrative_ontology:cs_kernel_id(all_men_created_equal__textualist_paradox_reading, all_men_created_equal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(all_men_created_equal__textualist_paradox_reading, abolitionist_advocates).
narrative_ontology:constraint_beneficiary(all_men_created_equal__textualist_paradox_reading, civil_rights_litigants).
narrative_ontology:constraint_beneficiary(all_men_created_equal__textualist_paradox_reading, excluded_class_members).
narrative_ontology:constraint_victim(all_men_created_equal__textualist_paradox_reading, founders_intent_interpreters).
narrative_ontology:constraint_victim(all_men_created_equal__textualist_paradox_reading, restrictionist_text_invokers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(all_men_created_equal__textualist_paradox_reading, constitutional_law_professoriate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Judges, justices, and scholars who ground constitutional authority in the framers' intentions and defend scope limits drawn from eighteenth-century social categories. Each time they invoke the founding texts' universal language in support of a narrowed application, opponents charge that the invocation refutes itself, and the charge lands in print, in oral argument, and in confirmation hearings. Leaving the school means surrendering institutional position built over a career; staying means either migrating the school's grounding, as the shift from subjective intent to public meaning did, or absorbing the charge repeatedly.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, founders_intent_interpreters, payer,
    institutional, biographical, constrained, national).

% Officeholders, parties, and movements that trade on founding reverence — anniversary oratory, oath-taking, invocation of the founders — while defending policies that narrow the promise's reach. Their rhetorical capital depends on continued reverence for the same documents that indict the narrowing. Abandoning either the reverence or the narrowing costs them a constituency, so they typically absorb the contradiction charge when it is pressed and wait for public attention to move elsewhere.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, restrictionist_text_invokers, payer,
    powerful, immediate, constrained, national).

% Nineteenth-century organizers, writers, and orators who insisted that the Declaration and Constitution, read on their face, condemned slavery. They operated as an organized minority without access to formal power, unable to leave the republic whose texts they contested, and they gained argumentative ground precisely at the moments their opponents appealed to those same documents.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, abolitionist_advocates, beneficiary,
    organized, generational, constrained, national).

% People placed outside the announced equality — enslaved persons and their descendants, then legally segregated citizens, then those narrowed out in each subsequent scope fight. They cannot exit the polity that excludes them, and their standing to invoke the founding text was long denied in the very forums where the scope question was decided. When the gap between the text's words and their treatment is named in court or in print, their claims acquire a standing they could not obtain from any other quarter.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, excluded_class_members, beneficiary,
    powerless, generational, trapped, national).

% Twentieth- and twenty-first-century plaintiffs and counsel organizations pressing equality claims through the courts. They depend on doctrinal receptivity they do not control; when courts accept that a restricted application cannot be squared with the text's plain words, their filings convert that acceptance into remedies, and when courts decline, they regroup and refile under new theories.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, civil_rights_litigants, beneficiary,
    moderate, biographical, constrained, national).

% Produces the casebook treatments, law-review symposia, and confirmation-hearing questioning that determine when the contradiction charge is pressed and how much weight it carries. Administers the argument's currency across generations of students. The running dispute over the founding texts' scope supplies a large share of the field's publications and career milestones; individual members can write in any methodological register and face no barrier to changing sides.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, constitutional_law_professoriate, agenda_setter,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(all_men_created_equal__textualist_paradox_reading, constitutional_law_professoriate, beneficiary).

% Scholars of the founding, Reconstruction, and the interpretive tradition who document when the contradiction argument was made, against whom, and with what effect. They hold no stake in which interpretive method prevails and corroborate the record from outside the disputing camps.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, legal_historians, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(all_men_created_equal__textualist_paradox_reading, diffuse).
narrative_ontology:fixing_cost_class(all_men_created_equal__textualist_paradox_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The argument supplies a shared standard for evaluating gaps between a polity's revered founding commitments and their applied scope: it lets interpreters call out text-practice inconsistencies on a common criterion, and it locates a lever for excluded claimants inside the authoritative text rather than outside it.
% TRANSFER_FUNCTION: Moves interpretive authority and standing from actors who invoke the founding texts' universal language while defending restricted application to the actors who press the texts' universal reading; each successful deployment converts the invoker's borrowed textual majesty into the deployer's argumentative position.
% ABSENT_VOICES: The excluded themselves were absent from every forum where the scope question was originally settled — enslaved persons, women, and the non-propertied had no seat where 'all men' was given content, and their objection reaches the record only retroactively, through later deployments of the argument on their behalf. Contemporary claimants affected by narrowed applications but lacking standing or resources to litigate remain similarly unseated; the argument's force depends on seats they were historically denied.
% DISAPPEARANCE_RATIONALE: If the contradiction argument vanished overnight, founder-intent authority claims would regain unrestricted access to the founding texts' prestige without contradiction cost, restriction-maintaining invocations would shed their recurring impeachment, and expansion-seeking movements would lose their inside-the-text lever and be pushed onto extra-textual grounds — natural-law argument, pure political mobilization — with materially lower yield. The interpretive field would reorganize around the boundary between textual reverence and statutory scope with no standing mechanism for charging the gap.
% FOUNDING_PROBLEM: How can those excluded by a polity that reveres a universal-equality text claim that text's authority without conceding the exclusion? Douglass's formulation: the abolitionist who rejects the founding documents as irremediably compromised leaves them to the slaveholders, whereas the documents read at face value indict the practice — reclaiming the founding tradition for the excluded rather than abandoning it.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties on two sides: legal historians of the founding and Reconstruction (an observer seat with no stake in the method dispute) document the recurring recurrence of the text-practice gap in each era; and the targeted school's own methodological history attests it — the documented migration from subjective-intent grounding to public-meaning grounding is conceded in the originalist literature itself as a response to exactly this line of attack. No attestation from the argument's beneficiaries is relied upon.
narrative_ontology:disappearance_verdict(all_men_created_equal__textualist_paradox_reading, world_rearranges).
narrative_ontology:founding_problem_status(all_men_created_equal__textualist_paradox_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(all_men_created_equal__textualist_paradox_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(all_men_created_equal__textualist_paradox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(all_men_created_equal__textualist_paradox_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(all_men_created_equal__textualist_paradox_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(all_men_created_equal__textualist_paradox_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(all_men_created_equal__textualist_paradox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.55 at interval end) because the argument takes real, recurring argumentative authority from restricted-application invocations, but its targets retain substantial resources and have twice adapted rather than collapsed. Suppression is moderate-low (0.40) and purely argumentative: the argument imprisons no one and bars no forum; what it suppresses is a specific combination — reverent textual invocation joined to restricted application — rendering that combination costly to hold in print. Suppression is a raw structural property here, unscaled by power or scope; only extractiveness is scaled downstream. Theater ratio (0.30) reflects a mostly functional argument with a growing ceremonial fringe: anniversary oratory rehearses the universal language without pressing the contradiction, while the operative deployments run through opinions, briefs, and scholarship. Accessibility collapse is low (0.35): the targeted school adapted twice (Reconstruction-era reframing, then the intent-to-public-meaning migration) and rival readings remain fully live, so alternatives nowhere near collapse. Resistance is substantial (0.60): an organized counter-scholarship, judicial pushback, and political dismissal meet the argument wherever it is pressed. The measurement series run on ONE shared time grid (1852, 1865, 1896, 1954, 1971, 1985, 2005, 2026) with every tracked metric authored at every point; the base_properties scalars are the end-state (2026) values. CYCLICAL PATTERN: the series oscillate on a roughly generational cycle — deployment, doctrinal concession or absorption, quiet re-accumulation of restricted practices, renewed deployment (1852-65 peak, Plessy-era trough, Brown-era peak, new-originalism absorption, current resurgence). The oscillation is plausibly part of the operating mechanism rather than noise: each crisis deployment re-funds the argument's authority (intermittent reinforcement), which is flagged as an open omega rather than assumed. IDENTITY-LOCK DYNAMICS: the payer seat's exit is constrained less by economics than by professional identity fused with founder-fidelity — an originalist who concedes the paradox wholesale does not merely change methods but dissolves the school's warrant; the observed exits are therefore intra-school migrations (intent to public meaning) that preserve identity while conceding ground. Were that identity frame to break — the school reframing itself as method rather than loyalty — exit would loosen and the argument's taxing power over it would fall.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats compute differently and should. From the founders_intent_interpreters seat, the argument operates as a recurring bad-faith impeachment: an anachronism hunt that punishes a legitimate method for the sins of the founding generation and taxes every good-faith invocation of the tradition. From the excluded_class_members and abolitionist_advocates seats, the same structure is the text keeping faith with itself — the only lever that works from inside a tradition that otherwise locks them out. The constitutional_law_professoriate seat experiences it as a productive research program that organizes the field. The legal_historians seat sees a consistency norm with sharply asymmetric incidence: cheap to deploy, expensive to absorb, and falling almost entirely on one methodological camp. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations map directly onto directional positions. Excluded_class_members (powerless, trapped, beneficiary) sit nearest the full-beneficiary end: the argument subsidizes claims they can advance nowhere else, at zero cost to them. Abolitionist_advocates (organized, constrained) and civil_rights_litigants (moderate, constrained) sit nearby — they collect standing from each successful deployment and bear little of its cost. Founders_intent_interpreters (institutional, constrained, payer) sit near the full-target end: the charge lands on every restricted invocation, their exit is identity-priced, and their best moves are costly adaptations. Restrictionist_text_invokers (powerful, constrained, payer) also sit high: they need the texts' prestige and therefore cannot cleanly exit the combination being taxed. The professoriate (agenda_setter with beneficiary secondary role, mobile) sits low — it administers the standard and collects residual career capital regardless of which side wins a given round. No directionality overrides are authored: the derivation chain from declared roles, power atoms, and exit options already produces these positions, and no seat's derived directionality misdescribes its structural relationship. RECEIPT NOTE: gain_flow is authored as 'diffuse' as an affirmative checked claim — receipts cycle across deployer seats by era (abolitionist_advocates before 1865, civil_rights_litigants at mid-century), with only a steady residual to the professoriate; no single named seat captures the argument's yields durably across the interval. Fixing_cost is 'prohibitive': removing the argument requires either universalizing application (a wholesale constitutional transformation) or demoting the founding texts' bindingness (abandoning the reverence the entire tradition trades on) — both beyond any seat positioned to attempt them.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents two opposite mislabelings. Read from the beneficiary seats, the argument presents as pure coordination — a costless consistency service, truth-telling at no one's expense — which would license a rope verdict and hide the systematic authority tax on one methodological camp. Read from the target seat, it presents as pure extraction — a career-punishment machine dressed as interpretation — which would license a snare verdict and erase the genuine coordination function: the shared standard for text-practice consistency, and the inside-the-text lever that has repeatedly converted exclusion into standing. The structural data hold both facts at once: real coordination service, asymmetric incidence, active enforcement through deployment in opinions, scholarship, and hearings. On the R5 mismatch check, founding_problem_status (live) and disappearance_verdict (world_rearranges) are aligned — the founding problem recurs with each new scope fight, the arrangement does load-bearing work, and no zombie flag is indicated; the mandate has not outlived its function, so no mandatrophy resolution is declared.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    declaration_binding_status,
    'Is the Declaration''s equality clause a binding constitutional commitment whose violation is a legitimacy defect, or an aspirational preamble whose scope is fixed entirely by subsequently enacted provisions?',
    'Doctrinal and historical analysis of how the tradition itself treats inconsistency with the equality clause — whether courts and statesmen have ever counted it as a legality or legitimacy defect independent of enacted text.',
    'If the clause is non-binding, the paradox extracts nothing and this constraint collapses toward an inert rhetorical artifact; if binding, the moderate extraction measured here stands and the originalist escape route of denying the text''s authority closes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(declaration_binding_status, conceptual, 'Whether the kernel text binds, which determines whether invoking it while restricting scope costs anything.').

omega_variable(
    public_meaning_absorption,
    'Has the migration from original-intent to original-public-meaning methodology genuinely absorbed the contradiction charge, or merely relocated it to the level of word meaning?',
    'Test whether public-meaning accounts of the founding generation''s understanding of ''all'' and ''equal'' reproduce the same gap the intent-based account produced; track scholarly and judicial treatment of the equality clause under the newer methodology.',
    'If absorbed, this constraint''s extractiveness decays toward coordination-only levels over coming decades; if relocated, moderate extraction persists and the target school remains taxable at each scope fight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_meaning_absorption, empirical, 'Whether the target school''s methodological adaptation neutralized the paradox or deferred it.').

omega_variable(
    deployment_function_mix,
    'What share of contemporary deployments of the contradiction argument impose real costs on restricted-application claims, versus rehearse the founding ceremonially without argumentative consequence?',
    'Corpus study weighting deployments (opinions, briefs, scholarship versus ceremonial address) by measurable effect on doctrinal outcomes.',
    'A rising ceremonial share signals piton-direction drift — the argument persisting as performance; a stable functional share supports the coordination-plus-tax structure claimed here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deployment_function_mix, empirical, 'Functional versus performative composition of current deployments.').

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the kernel all_men_created_equal; how would classification change under the sibling readings, and where exactly does the disagreement bind?',
    'No resolution is available inside this story — the ambiguity resolves only at the kernel level, through which reading''s scope-semantics the tradition eventually stabilizes; compare the sibling files'' victim sets and epsilon values directly.',
    'Under the originalist reading the burdened seats are expansion-seeking interpreters and this file''s payer seats become beneficiaries; under the universalist reading the burdened seats are restriction-maintaining institutions and extraction indexes higher; the disagreement is located in what governs the universal quantifier''s scope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame omega: the reading-indexed identity of this constraint within the all_men_created_equal kernel family.').

omega_variable(
    oscillation_driver_ambiguity,
    'Is the century-scale oscillation in the argument''s force driven by intermittent reinforcement — each crisis deployment re-funding its authority — or by external doctrinal shocks (war, amendment, court turnover) that would cycle regardless?',
    'Comparative timing analysis: whether deployments cluster predictably after absorption periods (a reinforcement signature) or track exogenous constitutional events only.',
    'If intermittent reinforcement drives the cycle, the oscillation is part of the constraint''s operating mechanism and extraction should be read as pulsing rather than declining; if exogenous, the series is noise around a stable moderate level.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(oscillation_driver_ambiguity, empirical, 'Driver of the observed rise-collapse-recycle pattern in the argument''s force.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(all_men_created_equal__textualist_paradox_reading, 1852, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(all__tr_t1852, all_men_created_equal__textualist_paradox_reading, theater_ratio, 1852, 0.15).
narrative_ontology:measurement(all__tr_t1865, all_men_created_equal__textualist_paradox_reading, theater_ratio, 1865, 0.12).
narrative_ontology:measurement(all__tr_t1896, all_men_created_equal__textualist_paradox_reading, theater_ratio, 1896, 0.44).
narrative_ontology:measurement(all__tr_t1954, all_men_created_equal__textualist_paradox_reading, theater_ratio, 1954, 0.22).
narrative_ontology:measurement(all__tr_t1971, all_men_created_equal__textualist_paradox_reading, theater_ratio, 1971, 0.27).
narrative_ontology:measurement(all__tr_t1985, all_men_created_equal__textualist_paradox_reading, theater_ratio, 1985, 0.32).
narrative_ontology:measurement(all__tr_t2005, all_men_created_equal__textualist_paradox_reading, theater_ratio, 2005, 0.36).
narrative_ontology:measurement(all__tr_t2026, all_men_created_equal__textualist_paradox_reading, theater_ratio, 2026, 0.3).

% Extraction over time
narrative_ontology:measurement(all__be_t1852, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 1852, 0.62).
narrative_ontology:measurement(all__be_t1865, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 1865, 0.58).
narrative_ontology:measurement(all__be_t1896, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 1896, 0.3).
narrative_ontology:measurement(all__be_t1954, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 1954, 0.66).
narrative_ontology:measurement(all__be_t1971, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 1971, 0.6).
narrative_ontology:measurement(all__be_t1985, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 1985, 0.55).
narrative_ontology:measurement(all__be_t2005, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 2005, 0.48).
narrative_ontology:measurement(all__be_t2026, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 2026, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(all__su_t1852, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 1852, 0.35).
narrative_ontology:measurement(all__su_t1865, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 1865, 0.25).
narrative_ontology:measurement(all__su_t1896, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 1896, 0.7).
narrative_ontology:measurement(all__su_t1954, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 1954, 0.3).
narrative_ontology:measurement(all__su_t1971, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 1971, 0.38).
narrative_ontology:measurement(all__su_t1985, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 1985, 0.42).
narrative_ontology:measurement(all__su_t2005, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 2005, 0.35).
narrative_ontology:measurement(all__su_t2026, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 2026, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(all_men_created_equal__textualist_paradox_reading, information_standard).
narrative_ontology:affects_constraint(all_men_created_equal__textualist_paradox_reading, all_men_created_equal__originalist_reading).
narrative_ontology:affects_constraint(all_men_created_equal__textualist_paradox_reading, all_men_created_equal__universalist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the equality principle' covers three structurally distinct claims that decompose per the epsilon-invariance principle. The originalist reading instantiates a constraint whose referent is the bounded-application arrangement it defends; the universalist reading instantiates one whose referent is the restriction-maintaining arrangements it contests; this textualist_paradox_reading instantiates one whose referent is the regime of founder-intent authority claims as encountered by the contradiction argument. The three epsilons differ because the referents differ — not because one constraint is measured three ways. Upstream/downstream structure: this reading creates structural pressure on the originalist sibling (its documented intent-to-public-meaning migration is a response to exactly this attack) while coexisting with the universalist sibling as a rival strategy within the expansion camp. All three files link one another through network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
