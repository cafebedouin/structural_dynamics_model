% ============================================================================
% CONSTRAINT STORY: constitutional_text_authority__living_constitutionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text_authority__living_constitutionalist_reading, []).

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
 *   constraint_id: constitutional_text_authority__living_constitutionalist_reading
 *   human_readable: Living Constitutionalist Reading of Constitutional Text Authority
 *   domain: legal/jurisprudential/political
 *
 * SUMMARY:
 *   The living constitutionalist reading holds that constitutional meaning
 *   evolves with social attitudes and values, and that interpretive authority
 *   derives from contemporary moral principles and enduring values applied to
 *   changed circumstances. Operationally this is a practice, not a slogan: a
 *   standing arrangement in which life-tenured judges gate legislative
 *   outcomes through doctrines of evolving standards, recognize unenumerated
 *   rights through changing understanding, and treat Brown v. Board as proof
 *   that the Constitution can change without Article V. The arrangement
 *   solves a real coordination problem (an unamendable charter governing a
 *   transformed society) while transferring substantial decision authority to
 *   an insulated branch — hence the tangled-rope structure. This story is ONE
 *   READING of the kernel constitutional_text_authority; the originalist and
 *   positivist siblings are separate constraints with their own epsilon
 *   values and victim sets, linked via network.affects_constraints. KEY
 *   AGENTS (by structural relationship): see commentary.key_agents — the
 *   agenda-setting seat (justices) administers the updating regime;
 *   beneficiary seats (appellate judges, claimant litigants, the
 *   professoriate) collect discretion, protections, and scholarly authority;
 *   payer seats (state legislatures, congress, fixed-meaning citizens, the
 *   originalist movement) bear displaced law, rewritten expectations, and
 *   doctrinal defeat; the excluded seat (future generations) inherits the
 *   compounded result without voice.
 *
 * KEY AGENTS:
 *   - - supreme_court_justices: Primary agenda setter (institutional/identity_locked) — decides which contemporary values gate outcomes; each update compounds the office's authority
 *   - - federal_appellate_judges: Secondary beneficiary with payer residue (institutional/constrained) — extends updated doctrine, absorbs doctrinal churn
 *   - - rights_claimant_litigants: Episodic beneficiary (organized/constrained) — collects protections unavailable legislatively at the moment of suit
 *   - - constitutional_law_professoriate: Beneficiary (organized/identity_locked) — supplies the moral-historical syntheses the practice consumes
 *   - - state_legislatures: Primary payer (organized/constrained) — loses enacted law to judicially updated meaning
 *   - - congress: Payer with secondary beneficiary position (institutional/constrained) — statutes struck down; also receives nationalized rights it could not amend into being
 *   - - fixed_meaning_citizens: Diffuse payer (moderate/trapped) — bears rewritten rules without ratification-era consent or cheap exit
 *   - - originalist_jurisprudential_movement: Organized payer-resister (organized/constrained) — bears doctrinal defeat and mounts the principal resistance
 *   - - future_generations: Excluded seat (powerless/trapped) — inherits accumulated re-authorings without voice
 *   - - constitutional_politics_scholars: Analytical observer (analytical/analytical) — measures legitimacy and backlash cycles
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text_authority__living_constitutionalist_reading, 0.48).
domain_priors:suppression_score(constitutional_text_authority__living_constitutionalist_reading, 0.64).
domain_priors:theater_ratio(constitutional_text_authority__living_constitutionalist_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text_authority__living_constitutionalist_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text_authority__living_constitutionalist_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(constitutional_text_authority__living_constitutionalist_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text_authority__living_constitutionalist_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text_authority__living_constitutionalist_reading, "Living Constitutionalist Reading of Constitutional Text Authority").
narrative_ontology:topic_domain(constitutional_text_authority__living_constitutionalist_reading, "legal/jurisprudential/political").

domain_priors:requires_active_enforcement(constitutional_text_authority__living_constitutionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text_authority__living_constitutionalist_reading, '384687da-a73e-41b6-9d40-b7effd8b4c06').
narrative_ontology:cs_kernel_codification('384687da-a73e-41b6-9d40-b7effd8b4c06', fixed_text).
narrative_ontology:cs_authority_grounding('384687da-a73e-41b6-9d40-b7effd8b4c06', expertise).
narrative_ontology:cs_interpretation_layer_present('384687da-a73e-41b6-9d40-b7effd8b4c06').
narrative_ontology:cs_reading_relation('384687da-a73e-41b6-9d40-b7effd8b4c06', constitutional_text_authority__originalist_reading, forecloses).
narrative_ontology:cs_reading_relation('384687da-a73e-41b6-9d40-b7effd8b4c06', constitutional_text_authority__positivist_reading, influences).
narrative_ontology:cs_axiom('384687da-a73e-41b6-9d40-b7effd8b4c06', foundational, meaning_adaptation_required_for_legitimate_governance).
narrative_ontology:cs_axiom_status(meaning_adaptation_required_for_legitimate_governance, holdable).
narrative_ontology:cs_axiom_grounding('384687da-a73e-41b6-9d40-b7effd8b4c06', meaning_adaptation_required_for_legitimate_governance, instrumental).
narrative_ontology:cs_axiom('384687da-a73e-41b6-9d40-b7effd8b4c06', foundational, unenumerated_rights_judicially_recognizable).
narrative_ontology:cs_axiom_status(unenumerated_rights_judicially_recognizable, holdable).
narrative_ontology:cs_axiom_grounding('384687da-a73e-41b6-9d40-b7effd8b4c06', unenumerated_rights_judicially_recognizable, deontological).
narrative_ontology:cs_reference_frame('384687da-a73e-41b6-9d40-b7effd8b4c06', evolutionary_moral_continuity_framework).
narrative_ontology:cs_drift_state('384687da-a73e-41b6-9d40-b7effd8b4c06', contemporary_originalist_ascendancy, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('384687da-a73e-41b6-9d40-b7effd8b4c06', '').
narrative_ontology:cs_kernel_id(constitutional_text_authority__living_constitutionalist_reading, constitutional_text_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text_authority__living_constitutionalist_reading, supreme_court_justices).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__living_constitutionalist_reading, federal_appellate_judges).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__living_constitutionalist_reading, rights_claimant_litigants).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__living_constitutionalist_reading, constitutional_law_professoriate).
narrative_ontology:constraint_victim(constitutional_text_authority__living_constitutionalist_reading, state_legislatures).
narrative_ontology:constraint_victim(constitutional_text_authority__living_constitutionalist_reading, congress).
narrative_ontology:constraint_victim(constitutional_text_authority__living_constitutionalist_reading, fixed_meaning_citizens).
narrative_ontology:constraint_victim(constitutional_text_authority__living_constitutionalist_reading, originalist_jurisprudential_movement).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__living_constitutionalist_reading, congress).
narrative_ontology:constraint_victim(constitutional_text_authority__living_constitutionalist_reading, federal_appellate_judges).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Nine life-tenured justices decide which contemporary values gate constitutional outcomes and which traditions anchor them. Each update exercises and compounds the office's interpretive authority; their jurisprudential commitments fuse with their institutional legacies, and retirement is the only exit from a seat whose method they have publicly defended.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, supreme_court_justices, agenda_setter,
    institutional, generational, identity_locked, national).

% Lower-court judges gain discretion to read values into open-textured clauses when extending Supreme Court doctrine, but pay in doctrinal churn: every fresh update obliges panels to re-litigate meaning they had treated as settled, and reversal risk disciplines how far they reach.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, federal_appellate_judges, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text_authority__living_constitutionalist_reading, federal_appellate_judges, payer).

% Litigants collect protections — privacy, dignity, incorporation of rights against the states — that were unavailable through legislatures at the moment of suit. Wins are episodic and reversible by later benches; declining to litigate forfeits the protection entirely, so the channel is valuable precisely where it is least controllable.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, rights_claimant_litigants, beneficiary,
    organized, biographical, constrained, national).

% Academic constitutional lawyers supply the moral-historical syntheses that opinions cite and train the clerks who draft them. Their scholarly authority and career capital are invested in the legitimacy of value-based interpretation; pivoting to a purely historical or procedural method would strand decades of accumulated work.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, constitutional_law_professoriate, beneficiary,
    organized, biographical, identity_locked, national).

% State legislatures enact policies on religion, criminal procedure, marriage, education, and regulation that are later invalidated under judicially updated meaning. Their remedies run through expensive litigation or an Article V amendment process that has succeeded only twenty-seven times; they keep legislating inside a space that contracts without their consent.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, state_legislatures, payer,
    organized, biographical, constrained, regional).

% Congress sees statutes struck down under evolving doctrine, yet also benefits when judicial updates accomplish what amendment politics cannot — nationalizing rights against the states, retiring archaic provisions. Its recourse against unwanted updates is limited to confirmation fights, curbing proposals, and jurisdictional maneuvers, all slow and uncertain.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, congress, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text_authority__living_constitutionalist_reading, congress, beneficiary).

% Citizens who order their affairs, votes, and expectations around the ratified text's stability find the operative rules rewritten by benches they never voted for directly. Emigration is the only individual exit; collective recourse runs through appointment politics delayed by years and mediated by chance vacancies.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, fixed_meaning_citizens, payer,
    moderate, biographical, trapped, national).

% Judges, scholars, and lawyers committed to ratification-era public meaning bear repeated doctrinal defeat and fight nomination battles over method. They are simultaneously the constraint's principal organized resistance; abandoning the contest would mean surrendering the interpretive enterprise their professional identities are built on.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, originalist_jurisprudential_movement, payer,
    organized, biographical, constrained, national).

% People not yet born inherit whichever meanings each era's bench accumulates. They cannot object to updates made before they could speak, cannot ratify or reject them, and will live inside the compounded result; their only representation is the restraint of current interpreters.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, future_generations, excluded,
    powerless, civilizational, trapped, national).

% Political scientists measure Court legitimacy, backlash cycles, and the diffusion of interpretive methods across benches. They take no side in the contest and collect nothing from its outcome; their data series are the principal external check on the practice's self-description.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, constitutional_politics_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text_authority__living_constitutionalist_reading, supreme_court_justices).
narrative_ontology:fixing_cost_class(constitutional_text_authority__living_constitutionalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps a single durable constitutional framework governing a society whose circumstances and values change across centuries: it solves the problem of how an eighteenth-century text, amendable only through a process that has succeeded twenty-seven times in 235 years, can continue to coordinate a modern polity without either obsolescence or perpetual constitutional convention.
% TRANSFER_FUNCTION: Moves interpretive authority over contested moral and political questions from the ratifying generation's fixed public understanding, and from currently seated legislatures, to the federal judiciary; concretely, it moves decision power on rights questions from electoral processes to life-tenured benches, and moves the costs of each update onto those whose enacted law or settled expectations are displaced.
% ABSENT_VOICES: Future generations who will inherit the accumulated re-authorings are structurally absent — no seat speaks for them. Citizens whose values lose at each update are present only vicariously, through senators voting on confirmations years before the decisive cases arrive. State and local majorities whose enactments are overridden appear only as litigation opponents after the fact. All three would demand either supermajority ratification of updates or a fixed-meaning guarantee; they are outside the courtroom, in the ratification past and the appointment future.
% DISAPPEARANCE_RATIONALE: If judicial updating vanished overnight — if the Court tomorrow bound itself to fixed ratification-era meaning — the rearrangement would be immediate and vast: incorporation of the Bill of Rights against the states, the substantive due process line, equal-protection applications beyond the four corners of the text, and the entire reliance architecture built on evolved doctrine would unravel or demand a quarter-century of amendments. Enacted law previously invalidated would revive; every stakeholder seat listed here would have to reposition.
% FOUNDING_PROBLEM: How to keep a rigid, deliberately hard-to-amend eighteenth-century charter authoritative for a changing nation — posed acutely when Lochner-era rigidity blocked democratic legislation, and crystallized when Brown v. Board (1954) required overturning Plessy without waiting for Article V to move.
% FOUNDING_PROBLEM_CORROBORATION: The problem is attested from outside the benefiting parties: constitutional historians across the interpretive divide document the amendment-rate failure and the New Deal crisis; originalist scholars themselves concede Article V's extreme difficulty even while rejecting the judicial solution; several comparative jurisdictions adopted explicit living-tree instruments for the same rigidity problem. No corroborating source depends on judicial updating power for its livelihood or authority.
narrative_ontology:disappearance_verdict(constitutional_text_authority__living_constitutionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text_authority__living_constitutionalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text_authority__living_constitutionalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_text_authority__living_constitutionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text_authority__living_constitutionalist_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text_authority__living_constitutionalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text_authority__living_constitutionalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_text_authority__living_constitutionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.48: the arrangement transfers real decision authority from electorates and legislatures to an insulated bench, but the transfer is tempered by a genuine adaptive function and by episodic benefits flowing to claimant seats — this is the signature of a hybrid, not a pure extraction. Suppression (0.64) is a raw structural property, unscaled: judicial supremacy plus an Article V process that effectively closes the formal alternative, tempered by the electoral route to reshaping the bench. Theater ratio (0.46) reflects the erosion of candid value-reasoning (the early Warren Court announced that it was updating) into discovery-rhetoric in which constructed values are presented as found ones. Accessibility collapse (0.42) is honestly moderate-low: the rival readings remain live — that contest IS the kernel — so alternatives do not collapse upon understanding. Resistance (0.62) is high and organized: a forty-year counter-movement, confirmation warfare, and explicit repudiation from the current bench. The temporal series run on ONE shared grid (points 0-70, mapped to 1954-2024) with all three metrics authored at every point. Base extractiveness is deliberately NON-monotonic: it climbs through the Warren and Burger expansions (peak 0.55 at t=20, the Roe era, when updates were frequent and each displaced more settled expectation), plateaus through Rehnquist retrenchment, and gently declines as the originalist program displaces the method — the constraint's extraction contracts as its domain contracts. Suppression requirement rises monotonically across the same grid: enforcement intensity grows as legitimacy erodes, because defending accumulated doctrine against repudiation costs more than issuing updates did. Theater rises throughout: the gap between what the practice does (author values) and what its rhetoric claims (discovers them) widens as the method comes under scrutiny.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently, and the structural data explains why. From the agenda-setter seat, the arrangement is stewardship: the justices experience updating as fidelity to a charter whose point is endurance. From the payer seats, the identical structure operates as enforced extraction: state legislatures watch enacted law die in court, fixed-meaning citizens carry rules no one they elected wrote, and the originalist movement absorbs serial defeat. Identity-lock dynamics bind two seats specifically: for the justices, institutional identity fuses with jurisprudential legacy (repudiating one's own method mid-tenure destroys the legacy); for the professoriate, professional identity fuses with the moral-reading paradigm (career capital is denominated in it). If either identity frame broke — justices treating method as swappable, academics repricing their capital — the constraint's enforcement costs would drop sharply. Coalition potential among the diffuse payers exists but is weak: fixed-meaning citizens can coordinate only through the slow appointment channel, and the originalist movement's coalition strength is precisely what the rising suppression_requirement series records.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place the justices, appellate judges, claimant litigants, and professoriate near the beneficiary end of d: the arrangement subsidizes their discretion, protections, and authority, and their exits are constrained or identity-locked rather than arbitrage-grade. Victim declarations place state legislatures, congress, fixed-meaning citizens, and the originalist movement near the target end: they bear the transfer directly, with trapped or constrained exits that amplify effective extraction. Congress's dual position (payer with secondary beneficiary) is authored rather than overridden because the structural data — statutes struck down AND rights nationalized — already encodes the ambivalence. No directionality overrides are used: the derivation from declared beneficiaries, victims, power atoms, and exit options captures the relationships, and the coarse per-power-atom override surface could not distinguish same-atom agents on opposite sides (institutional payers versus institutional beneficiaries) anyway. Future generations sit outside the d computation as an excluded seat: they collect nothing and pay later, which is exactly why they appear in absent_voices rather than as a directional party.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — an unamendable charter facing a changing society — is live, so this is NOT a mandatrophy case: the arrangement has not outlived its function, and the R5 mismatch consumer should find status=live paired with verdict=world_rearranges, producing no zombie flag. The classification work the type performs is preventing symmetric mislabeling. Reading the arrangement as pure snare (judicial usurpation) ignores the coordination function even its fiercest critics concede — Brown solved a problem Article V demonstrably would not have solved in time. Reading it as pure rope (necessary adaptation) ignores the concentrated authority capture: the gains land on a specific seat, the bench, which is why gain_flow names supreme_court_justices rather than diffuse. Tangled rope holds both truths: genuine coordination AND asymmetric extraction through the same structure, sustained by active enforcement (judicial supremacy, stare decisis, confirmation politics) — which is why requires_active_enforcement is true and why the suppression series trends upward rather than decaying.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is ONE reading of the kernel constitutional_text_authority (the living constitutionalist reading). What structurally changes if a sibling reading is instantiated instead?',
    'Generate the sibling stories (constitutional_text_authority__originalist_reading, constitutional_text_authority__positivist_reading) and compare victim sets, epsilon, and seat classifications across the family.',
    'The originalist sibling shifts the victim set toward those relying on accumulated evolved doctrine (incorporation, substantive due process) and authors epsilon for the same standing practice from fixed-meaning lights; the positivist sibling removes moral-content gatekeeping entirely, dissolving this reading''s beneficiary structure. Per-seat classifications computed here do not transfer across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame position: one reading of a three-reading kernel; disagreement located in the determinants-of-meaning premise.').

omega_variable(
    whose_values_are_tracked,
    'Whose attitudes does ''evolving social values'' actually track in operation — national consensus, elite legal-class opinion, or the ideological composition of the sitting bench?',
    'Correlate the timing of doctrinal updates with contemporaneous mass polling, elite legal-journal opinion, and appointment-turnover sequences; test lag structures.',
    'If updates track bench composition or elite opinion, extraction concentrates on out-of-coalition citizens and the constraint skews snare-flavored; if updates track genuine national consensus, the coordination reading strengthens and effective extraction falls toward the coordination floor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(whose_values_are_tracked, empirical, 'Whether the values-gate tracks diffuse consensus or concentrated elite/bench preference.').

omega_variable(
    countermajoritarian_price_or_rent,
    'Is the transfer of interpretive authority to an insulated judiciary the unavoidable price of rights protection under a nearly unamendable charter, or rent captured by a branch accountable to no electorate?',
    'Compare rights-protection outcomes and update legitimacy across jurisdictions differing in formal amendment difficulty (easy-amendment state constitutions versus the Article V regime; comparative living-tree jurisdictions).',
    'If comparable protection is achievable through cheaper formal channels, the authority transfer is rent above coordination cost; if no cheaper channel protects the same interests, much of the measured extraction is the price of the coordination itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(countermajoritarian_price_or_rent, empirical, 'Whether judicialized updating is coordination cost or branch-specific rent.').

omega_variable(
    brown_exceptionalism_question,
    'Does Brown v. Board (1954) — constitutional change without Article V — vindicate a general warrant for value-driven updating, or stand as a singular correction of a founding-era moral catastrophe that later updates cannot generalize from?',
    'Test whether subsequent updates exhibit Brown''s structure (overwhelming retrospective moral clarity, correction of an original sin) or ordinary political valence (contested, coalition-dependent outcomes).',
    'The exceptionalist reading caps this constraint''s warrant and lowers sustainable epsilon; the generalizing reading sustains broad updating authority and entrenches the tangled-rope structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(brown_exceptionalism_question, conceptual, 'Scope of the warrant drawn from the reading''s canonical success case.').

omega_variable(
    displacement_vs_stabilization,
    'Is the living-constitutionalist practice being terminally displaced by the organized originalist program, or stabilizing as a hybrid in which stare decisis preserves accumulated doctrine while new merits decisions reason historically?',
    'Track the share of constitutional merits decisions employing historical-tradition tests versus contemporary-moral-principle tests over the coming decade, plus appointment pipelines.',
    'Terminal displacement dates this constraint''s transition toward piton (doctrine preserved theatrically, method abandoned); stabilization entrenches the tangled_rope structure indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displacement_vs_stabilization, empirical, 'Trajectory of the reading under sibling-reading ascendancy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text_authority__living_constitutionalist_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(cons_tr_t10, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 10, 0.27).
narrative_ontology:measurement(cons_tr_t20, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement(cons_tr_t30, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(cons_tr_t40, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(cons_tr_t50, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 50, 0.41).
narrative_ontology:measurement(cons_tr_t60, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 60, 0.44).
narrative_ontology:measurement(cons_tr_t70, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 70, 0.46).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(cons_be_t10, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(cons_be_t20, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(cons_be_t30, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 30, 0.54).
narrative_ontology:measurement(cons_be_t40, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 40, 0.52).
narrative_ontology:measurement(cons_be_t50, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 50, 0.5).
narrative_ontology:measurement(cons_be_t60, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 60, 0.49).
narrative_ontology:measurement(cons_be_t70, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 70, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(cons_su_t10, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(cons_su_t20, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(cons_su_t30, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement(cons_su_t40, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 40, 0.55).
narrative_ontology:measurement(cons_su_t50, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 50, 0.58).
narrative_ontology:measurement(cons_su_t60, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 60, 0.61).
narrative_ontology:measurement(cons_su_t70, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 70, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text_authority__living_constitutionalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_text_authority__living_constitutionalist_reading, constitutional_text_authority__originalist_reading).
narrative_ontology:affects_constraint(constitutional_text_authority__living_constitutionalist_reading, constitutional_text_authority__positivist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'constitutional authority' conflates three structurally distinct claims about what fixes constitutional meaning. This story instantiates the living constitutionalist reading (meaning evolves with social attitudes; contemporary moral principles gate outcomes). The originalist sibling instantiates fixed ratification-era meaning; the positivist sibling grounds validity in enactment procedure with law/morality distinction intact. Each member carries its own epsilon, beneficiary/victim structure, and classification; they are linked here because each reading's operation changes the legitimacy conditions and resource availability of the others (appointment politics, doctrinal reliance interests, academic authority markets).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
