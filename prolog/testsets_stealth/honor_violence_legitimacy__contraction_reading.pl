% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_violence_legitimacy__contraction_reading, []).

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
 *   constraint_id: honor_violence_legitimacy__contraction_reading
 *   human_readable: Redefined Honor Regime: Violence Excluded from Legitimate Response (Contraction Reading)
 *   domain: historical sociology / legal anthropology / commitment systems
 *
 * SUMMARY:
 *   Between the mid-eighteenth and early twentieth centuries the honor
 *   regimes of European elites inverted: conduct that had been obligatory —
 *   answering insult with arranged combat — became dishonorable, then
 *   unthinkable. This story instantiates the contraction_reading of the
 *   honor_violence_legitimacy kernel: the decisive mechanism was the
 *   redefinition of honor itself, from a public standing maintained by
 *   displayed courage and the right to give satisfaction, into an internal
 *   moral character (integrity, veracity, self-command) to which private
 *   violence is categorically foreign. On this reading dueling did not become
 *   expensive while remaining legitimate; legitimacy itself dissolved, and
 *   with it the option. The epsilon referent is the standing honor-legitimacy
 *   arrangement across the transition, assessed by this reading's own lights:
 *   the end-state regime takes little from those it governs (identity
 *   discipline, devalued status capital for the old gentry), suppresses
 *   almost nothing visibly (its force is conceptual closure), and coordinates
 *   genuinely (peaceful status competition aligned with the state's
 *   consolidation of the violence monopoly). Sibling readings — drop_reading
 *   and composite_reading — are separate constraints with separate epsilon;
 *   see network.dual_formulation_note. KEY AGENTS (by structural
 *   relationship): - central_state_authorities: agenda-setter and principal
 *   beneficiary (institutional / identity_locked) — completed its violence
 *   monopoly as private honor combat receded -
 *   commercial_professional_classes: primary beneficiary (organized / mobile)
 *   — supplied the redefinition's social base and inherited the domesticated
 *   honor vocabulary - evangelical_moral_reformers: beneficiary (organized /
 *   constrained) — collected cultural authority from the anti-dueling
 *   campaign - traditionalist_gentry: payer (moderate / identity_locked) —
 *   status capital denominated in the old code, expropriated by the
 *   redefinition - professional_officer_corps: payer with secondary
 *   beneficiary position (organized / constrained) — the duel's last
 *   stronghold; traded it for professional discipline -
 *   status_conscious_gentlemen: beneficiary (moderate / constrained) — kept
 *   the status vocabulary without the blood-price -
 *   historical_sociologists_of_the_duel: analytical observer — sees the full
 *   structure and the sibling readings
 *
 * KEY AGENTS:
 *   - central_state_authorities: agenda_setter (institutional / identity_locked) — administers the legal order absorbing private honor violence; the gains of the redefinition demonstrably land here
 *   - commercial_professional_classes: beneficiary (organized / mobile) — rising bourgeoisie whose transactions and careers favored lawful, predictable conduct
 *   - evangelical_moral_reformers: beneficiary (organized / constrained) — moral entrepreneurs whose authority grew with the campaign that redefined honor
 *   - traditionalist_gentry: payer (moderate / identity_locked) — holders of the old honor economy's status capital
 *   - professional_officer_corps: payer, secondary beneficiary (organized / constrained) — last institutional stronghold of the duel
 *   - status_conscious_gentlemen: beneficiary (moderate / constrained) — ordinary honor-bearing men relieved of the forced choice
 *   - historical_sociologists_of_the_duel: observer (analytical / analytical) — comparative analytical seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__contraction_reading, 0.11).
domain_priors:suppression_score(honor_violence_legitimacy__contraction_reading, 0.06).
domain_priors:theater_ratio(honor_violence_legitimacy__contraction_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, extractiveness, 0.11).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, suppression_requirement, 0.06).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__contraction_reading, rope).
narrative_ontology:human_readable(honor_violence_legitimacy__contraction_reading, "Redefined Honor Regime: Violence Excluded from Legitimate Response (Contraction Reading)").
narrative_ontology:topic_domain(honor_violence_legitimacy__contraction_reading, "historical sociology / legal anthropology / commitment systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__contraction_reading, 'f3f56d7c-b951-470d-9da4-18e3a3454df2').
narrative_ontology:cs_kernel_codification('f3f56d7c-b951-470d-9da4-18e3a3454df2', distributed).
narrative_ontology:cs_authority_grounding('f3f56d7c-b951-470d-9da4-18e3a3454df2', self_enforcing).
narrative_ontology:cs_reading_relation('f3f56d7c-b951-470d-9da4-18e3a3454df2', honor_violence_legitimacy__drop_reading, forecloses).
narrative_ontology:cs_reading_relation('f3f56d7c-b951-470d-9da4-18e3a3454df2', honor_violence_legitimacy__composite_reading, coexists_with).
narrative_ontology:cs_axiom('f3f56d7c-b951-470d-9da4-18e3a3454df2', foundational, honor_is_virtue_not_standing).
narrative_ontology:cs_axiom_status(honor_is_virtue_not_standing, holdable).
narrative_ontology:cs_axiom_grounding('f3f56d7c-b951-470d-9da4-18e3a3454df2', honor_is_virtue_not_standing, deontological).
narrative_ontology:cs_axiom('f3f56d7c-b951-470d-9da4-18e3a3454df2', secondary, conceptual_closure_needs_no_enforcement).
narrative_ontology:cs_axiom_status(conceptual_closure_needs_no_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('f3f56d7c-b951-470d-9da4-18e3a3454df2', conceptual_closure_needs_no_enforcement, conventional).
narrative_ontology:cs_reference_frame('f3f56d7c-b951-470d-9da4-18e3a3454df2', honor_as_internal_virtue).
narrative_ontology:cs_drift_state('f3f56d7c-b951-470d-9da4-18e3a3454df2', contemporary_post_dueling_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f3f56d7c-b951-470d-9da4-18e3a3454df2', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, central_state_authorities).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, commercial_professional_classes).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, evangelical_moral_reformers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, professional_officer_corps).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, status_conscious_gentlemen).
narrative_ontology:constraint_victim(honor_violence_legitimacy__contraction_reading, traditionalist_gentry).
narrative_ontology:constraint_victim(honor_violence_legitimacy__contraction_reading, professional_officer_corps).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims and administers the monopoly on legitimate violence: dueling prosecutions, military regulations barring officers from giving or accepting challenges, court systems that absorb disputes once settled by combat. The redefinition handed it the practical completion of that monopoly without a frontal battle against the armed classes. It cannot retreat from the position without contradicting its own foundation — a state that readmitted private lethal settlement would dissolve the claim on which its legal order rests.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, central_state_authorities, agenda_setter,
    institutional, generational, identity_locked, national).

% Merchants, lawyers, physicians, and clerks whose livelihoods run on predictable bodies and enforceable contracts. Supplied the social base of the redefinition — newspapers, periodicals, and associations that ridiculed the duel as barbaric — and inherited the honor vocabulary in domesticated form: creditworthiness, professional integrity, respectability. Capital and careers can move; adoption of whichever conduct code serves advancement is always available.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, commercial_professional_classes, beneficiary,
    organized, generational, mobile, national).

% Clergy and lay campaigners whose cultural authority grew with the anti-dueling cause; sermons, tract societies, and reform candidacies were built on it. Their standing is tied to the moral economy they constructed — the redefinition holding is what their authority consists in — so pivoting away from it would forfeit the position they occupy.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, evangelical_moral_reformers, beneficiary,
    organized, generational, constrained, national).

% Hereditary families whose status capital was denominated in the old code — swordsmanship, displayed courage, the recognized right to give satisfaction. The redefinition rendered their competencies worthless and their honor claims eccentric; their sons were schooled into respectability. Memoirs from these circles lament that honor was corrupted into mere creditworthiness. Leaving the code would mean disowning the self it formed; most assimilated in silence, a minority died in the last duels.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, traditionalist_gentry, payer,
    moderate, biographical, identity_locked, regional).

% Officer classes were the duel's last stronghold; armies regulated the practice over decades and then prohibited it. They paid in the loss of the duel as a vindication and advancement mechanism and gained in its replacement by professional discipline, merit records, and tribunals. Career-bound: rejecting the service's code means leaving the service, and the code is the service.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, professional_officer_corps, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(honor_violence_legitimacy__contraction_reading, professional_officer_corps, beneficiary).

% Ordinary honor-bearing men who once faced the forced choice — answer a challenge or be posted as cowards. Under the redefined code they keep the status vocabulary without the blood-price: an insult is answered in print, in court, or with contempt, and no one of standing may demand pistols. They remain embedded in the society whose code disciplines them; withdrawal from respectable society was social death then and remains so now, but the code now asks far less.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, status_conscious_gentlemen, beneficiary,
    moderate, biographical, constrained, national).

% Analytical seat: reconstruct the decline from prosecution files, memoirs, press campaigns, and military regulations; compare national trajectories — England's early extinction without statute, France's persistence under penal law to 1914, Germany's ritualized Mensur — to separate conceptual from cost mechanisms and to adjudicate between the sibling readings of this history.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, historical_sociologists_of_the_duel, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_violence_legitimacy__contraction_reading, central_state_authorities).
narrative_ontology:fixing_cost_class(honor_violence_legitimacy__contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared standard of honorable conduct among elites that requires no enforcement machinery: disputes over reputation route to law, print, and professional mediation instead of private lethal combat, and status competition continues without a blood-price, in alignment with the state's claim to the violence monopoly.
% TRANSFER_FUNCTION: Moves the authority to define honor — and with it the power to sanction status — from hereditary warrior-gentry self-help institutions to state courts, professions, churches, and mass print opinion; along the way it moved the risks of private violence from gentlemen's bodies to state-administered justice, and demonetized the old gentry's status capital without compensation.
% ABSENT_VOICES: Those killed under the old code cannot testify; their place is taken by surviving kin testimony only. Colonized subjects whose honor systems remained violence-central would object that 'honor excludes violence' operated as a racial boundary — European restraint counted as civilization while the same conduct elsewhere counted as savagery — and that the redefinition armed a civilizing discourse applied to them. Present but marginalized: unrepentant dueling traditionalists in officer subcultures, audible only as nostalgia.
% DISAPPEARANCE_RATIONALE: If the redefined honor regime vanished overnight, the question it closed — whether violence answers insult — would reopen. Professional ethics codes, military discipline regulations, defamation and reputation law, and the entire respectability vocabulary of commercial society presuppose the exclusion; every institution built on the closed answer would have to renegotiate its treatment of insult, challenge, and retaliation, and the state's violence monopoly would face its first private competitor in two centuries.
% FOUNDING_PROBLEM: How to make elite self-help violence illegitimate without igniting a status war: a centralized state integrating a status-conscious armed elite into a commercial empire needed those bodies alive and their violence delegated, but a direct ban on dueling had repeatedly failed wherever honor still demanded it. The solution adopted was to change what honor meant rather than to police what gentlemen did.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the losing side's own testimony — traditionalist memoirs lamenting that honor had been corrupted into creditworthiness — attests both the founding problem and the mechanism of its solution; historians of the duel working from prosecution records, military regulations, and press archives corroborate the sequence from analytical seats; and the failed-ban record (penal statutes that did not stop the practice where the concept still held) independently documents that enforcement alone was not the operative variable.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_violence_legitimacy__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_violence_legitimacy__contraction_reading, 0.11, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__contraction_reading_tests).
:- end_tests(honor_violence_legitimacy__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   End-state extraction is low (0.11): the redefined regime takes chiefly identity discipline — conformity to restraint norms — and the residual devaluation of old status capital; the blood-price, the compelled risk, and the challenge-or-ruin choice are gone. Suppression is near-zero (0.06) because the regime's persistence mechanism is conceptual closure rather than coercion: this is the contraction reading's core claim, and it is authored as a structural fact, not inferred from the low score. Suppression is a raw property left unscaled; only extractiveness is scaled by directionality and scope in the engine's computation. Accessibility_collapse is high (0.78) but conceptual rather than punitive: once the redefinition is absorbed, the alternative is not forbidden, it is inconceivable — an honorable man defending honor by pistol is a contradiction in terms. The value sits below the ~0.85+ typical of genuine natural laws deliberately: the collapse is contingent, culturally bounded, and datable. Resistance is low (0.15) at end state, though transition-era resistance was substantial (officer subcultures, the German Mensur, French dueling's persistence) — the scalar describes the standing arrangement, the series carries the transition. Theater_ratio (0.30) is the end of a hump, not a plateau: the displaced code's rituals passed through a purely performative phase (bloodless French formalities, scar-cutting as fashion) peaking near 1880 before the residue itself faded, leaving a functional core with modest ceremonial survival. The metric profile is mountain-shaped (high conceptual collapse, low resistance, self-enforcing) resting on a constructed, datable, beneficiary-bearing arrangement — that mismatch is the story's point, not an error to reconcile: the reading's thesis is that successful redefinition manufactures natural-law phenomenology without natural-law substance. All three tracked series run on one shared seven-point grid (1760–1920) so every metric is authored at every examined time point; suppression_requirement is tracked because the story's subject IS enforcement decay — the machinery of compulsion (posting cowards, compelled challenges, ostracism) dismantling itself as the concept closed.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute very differently. From the central_state_authorities seat the arrangement is a civilizational achievement so self-evident that its constructedness is invisible — the state experiences no enforcement burden because nothing needs enforcing. From the traditionalist_gentry seat the same arrangement is expropriation: a lifetime's accumulated status currency demonetized by a definition change, with no compensation and no appeal, and exit indistinguishable from self-erasure. Between generations of status_conscious_gentlemen the gap is starker still: the transition generation experienced the arrangement as coercion (prohibition where they desired action), while their grandsons experience it as the shape of thought itself. The engine computes these divergences per seat from the structural data; the authored rope claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries cluster at the low-d end: central_state_authorities derives near-beneficiary directionality despite its agenda-setting role because the gains demonstrably land there (the violence monopoly's completion is the arrangement's principal product — recorded in gain_flow); commercial_professional_classes and evangelical_moral_reformers collect status, authority, and a favorable operating environment with mobile or constrained but real exits. traditionalist_gentry sits near the full-target end, amplified by identity lock: their selves are constituted by the displaced code, so the arrangement's costs are borne without possibility of exit-priced bargaining. professional_officer_corps is genuinely dual-positioned — it paid the loss of the duel as a vindication-and-promotion mechanism and gained professional discipline — placing it near symmetric. status_conscious_gentlemen benefit (no more forced choice) but sit slightly off the beneficiary pole because they remain embedded in and disciplined by the code. No directionality overrides were needed: the beneficiary declarations plus exit options produce the correct relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — managing lethal private violence among a status-conscious armed elite — remains live in mutated form, and the arrangement persists because it still solves it: disputes route to courts, print, and professional mediation; status competition continues without blood-price. No zombie flag: status is live, verdict is world_rearranges, and the mismatch consumer finds status-live x rearranges, the healthy cell. The temporal series nonetheless preserves the displaced code's piton-shaped passage — theater_ratio peaks at 0.61 around 1880, when the old rituals were performed without function — before the residue dissolved; this prevents two misreadings. First, it blocks the inference from low enforcement to naturalness: the arrangement behaves like a natural law from inside while carrying identifiable beneficiaries and a traceable birthdate, which is exactly the false-summit configuration the omegas document. Second, it blocks the symmetrical error of reading the transition as pure liberation: the displaced regime had a real coordination function (feud suppression among armed elites), so the redefinition reformed a working if costly institution rather than abolishing a mere swindle.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates the contraction_reading of kernel honor_violence_legitimacy; the drop_reading and composite_reading instantiate different constraints over the same history. Which reading''s structure does the evidence bind?',
    'Compile all three sibling stories and compare their computed classifications against the same evidentiary base: prosecution records, the timing of dueling''s extinction relative to the timing of redefinition rhetoric, and cross-national trajectories.',
    'Under drop_reading the standing arrangement is a cost-enforced regime with high suppression and the redefinition is epiphenomenal; under composite_reading the explanatory variance splits between mechanisms. This story''s epsilon, beneficiary structure, and classification are valid only within the contraction frame.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame indexicality: one of three readings of the honor-violence-legitimacy kernel.').

omega_variable(
    mechanism_attribution_ambiguity,
    'Was the conceptual redefinition of honor decisive in ending dueling, or did external costs (criminal liability, changing military organization, market integration) drive extinction with the redefinition as retrospective rationalization?',
    'Natural experiments across jurisdictions: France retained penal prohibitions for decades while dueling persisted to 1914 (costs rose, legitimacy held); Britain needed no statute after mid-century because the concept had already closed (legitimacy gone, costs irrelevant). Timing asymmetries of this kind attribute the binding force.',
    'If costs were decisive, this reading overstates the concept''s causal role and the arrangement''s stability rests on enforcement after all; classification shifts toward the drop_reading story''s profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mechanism_attribution_ambiguity, empirical, 'Whether conceptual contraction or external cost did the causal work.').

omega_variable(
    naturalness_presentation_ambiguity,
    'Is the exclusion of violence a conceptual necessity of honor properly understood, or a contingent nineteenth-century redefinition that succeeded in presenting itself as constitutive of honor as such?',
    'Cross-cultural comparison with honor systems in which violence remains honor-central (Mediterranean, Caucasus, pastoralist codes): if honor-with-violence is internally coherent elsewhere, the European exclusion is contingent rather than analytic.',
    'A necessity-reading would push the frame toward natural-law certification; a contingency-reading keeps the arrangement a constructed coordination norm with identifiable beneficiaries — the false-summit question for this regime.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_presentation_ambiguity, conceptual, 'Constitutive necessity versus contingent construction of the violence-exclusion.').

omega_variable(
    internalized_suppression_ambiguity,
    'Does the near-absence of measurable suppression at end state reflect genuine consent or complete internalization — the transition generation experienced prohibition where they desired action, while their grandsons desire nothing to prohibit?',
    'Compare transition-generation memoirs (reported temptation, shame at declining challenges, relief at face-saving exits) with post-redefinition cohorts (no reported temptation). Persistence of felt conflict after the enforcement machinery vanished indicates internalized suppression.',
    'If internalized, effective suppression exceeds the structural measure: the arrangement carries its force inside agents, and the low suppression score understates its grip on those formed mid-transition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_ambiguity, empirical, 'Structural versus internalized suppression mechanism across the transition.').

omega_variable(
    old_regime_character_ambiguity,
    'Was the pre-redefinition honor-duel regime primarily coordination (rule-governed single combat suppressing feud spirals among armed elites) or primarily extraction (compelled life-risk as the price of class membership)?',
    'Analyze duel frequency against feud frequency before and after codified dueling codes; examine compulsion directly through challenged parties'' documented reluctance, coerced challenges, and the treatment of refusers.',
    'Coordination-dominance reads the transition as reform of a working institution; extraction-dominance reads it as liberation — changing whether the redefinition dismantled a benefit or a burden, and coloring the moral valence of every seat''s position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(old_regime_character_ambiguity, empirical, 'Character of the displaced regime: coordination or extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__contraction_reading, 1760, 1920).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hvlg_contraction_tr_t1760, honor_violence_legitimacy__contraction_reading, theater_ratio, 1760, 0.2).
narrative_ontology:measurement(hvlg_contraction_tr_t1790, honor_violence_legitimacy__contraction_reading, theater_ratio, 1790, 0.23).
narrative_ontology:measurement(hvlg_contraction_tr_t1820, honor_violence_legitimacy__contraction_reading, theater_ratio, 1820, 0.33).
narrative_ontology:measurement(hvlg_contraction_tr_t1850, honor_violence_legitimacy__contraction_reading, theater_ratio, 1850, 0.52).
narrative_ontology:measurement(hvlg_contraction_tr_t1880, honor_violence_legitimacy__contraction_reading, theater_ratio, 1880, 0.61).
narrative_ontology:measurement(hvlg_contraction_tr_t1910, honor_violence_legitimacy__contraction_reading, theater_ratio, 1910, 0.48).
narrative_ontology:measurement(hvlg_contraction_tr_t1920, honor_violence_legitimacy__contraction_reading, theater_ratio, 1920, 0.3).

% Extraction over time
narrative_ontology:measurement(hvlg_contraction_be_t1760, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1760, 0.72).
narrative_ontology:measurement(hvlg_contraction_be_t1790, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1790, 0.68).
narrative_ontology:measurement(hvlg_contraction_be_t1820, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1820, 0.55).
narrative_ontology:measurement(hvlg_contraction_be_t1850, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1850, 0.38).
narrative_ontology:measurement(hvlg_contraction_be_t1880, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1880, 0.24).
narrative_ontology:measurement(hvlg_contraction_be_t1910, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1910, 0.14).
narrative_ontology:measurement(hvlg_contraction_be_t1920, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1920, 0.11).

% Suppression requirement over time
narrative_ontology:measurement(hvlg_contraction_su_t1760, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1760, 0.74).
narrative_ontology:measurement(hvlg_contraction_su_t1790, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1790, 0.71).
narrative_ontology:measurement(hvlg_contraction_su_t1820, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1820, 0.58).
narrative_ontology:measurement(hvlg_contraction_su_t1850, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1850, 0.34).
narrative_ontology:measurement(hvlg_contraction_su_t1880, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1880, 0.17).
narrative_ontology:measurement(hvlg_contraction_su_t1910, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1910, 0.09).
narrative_ontology:measurement(hvlg_contraction_su_t1920, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1920, 0.06).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__contraction_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy__drop_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the decline of dueling' conflates three structurally distinct claims about one history: (1) legitimacy persisted while external costs rose (drop_reading); (2) honor itself was redefined so violence exited the legitimate set (contraction_reading, this file); (3) both mechanisms operated simultaneously (composite_reading). Per the epsilon-invariance principle these are separate constraints with separate epsilon, beneficiary structures, and classifications, linked as a constraint family. Upstream/downstream structure: each single-mechanism reading's evidence is cited by composite_reading; contraction and drop mutually foreclose on the location of legitimacy, while neither forecloses the inclusive composite. This file's epsilon (0.11 end-state) is authored for the contraction frame only; the drop_reading story should author materially higher suppression and an enforcement-dependent stability profile over the same referent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
