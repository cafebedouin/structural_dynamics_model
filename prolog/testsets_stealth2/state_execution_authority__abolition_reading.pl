% ============================================================================
% CONSTRAINT STORY: state_execution_authority__abolition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_execution_authority__abolition_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: state_execution_authority__abolition_reading
 *   human_readable: State Execution Authority — Abolition Reading (Categorical Prohibition of Capital Punishment)
 *   domain: criminal justice/political philosophy/constitutional law
 *
 * SUMMARY:
 *   This story instantiates the abolition reading of the state execution
 *   kernel: the claim that state execution is categorically impermissible
 *   regardless of crime severity or procedural safeguards. The constraint
 *   under classification is the standing capital-punishment arrangement of a
 *   retentionist jurisdiction (modeled on the United States, the largest
 *   Western retentionist democracy; national scope), assessed by this
 *   reading's own lights per the epsilon-referent rule: epsilon is authored
 *   for the arrangement as it stands — the state's taking of life as
 *   punishment — never for the abolitionist's endorsed alternative, where
 *   epsilon would be trivially low by construction. Under this reading the
 *   arrangement is a snare: its coordination story (ultimate justice for the
 *   worst crimes, deterrence, closure for victims' families) is cover — each
 *   element is either satisfiable without killing (life imprisonment provides
 *   finality and incapacitation), empirically unsupported as a distinguishing
 *   benefit (deterrence), or rejected as a justification (retributive
 *   satisfaction) — while the cost borne by the executed (all of them, guilty
 *   and innocent alike) is total and irreversible. The declared beneficiaries
 *   are the arrangement's structural collectors, not legitimate
 *   justification-holders; the reading denies that any of their collections
 *   could offset the victim set. Wrongful execution is treated not as an
 *   administrative defect but as proof of systemic illegitimacy: a system
 *   that demonstrably convicts and sentences innocents cannot hold
 *   irreversibility as a safe instrument. The sibling readings (retributive,
 *   deterrence) are separate constraint files linked through
 *   cs_structure.reading_relations and the network edge set; they are not
 *   folded into this story's epsilon, beneficiary/victim structure, or
 *   classification.
 *
 * KEY AGENTS:
 *   - state_sovereign_authority: agenda-setter and structural collector (institutional/constrained) — runs the machinery end to end and collects the expressive output of the execution itself
 *   - electoral_politicians: beneficiary (powerful/mobile) — collects electoral capital from capital-punishment positions
 *   - prosecutorial_offices: beneficiary (institutional/mobile) — collects plea leverage and career advancement from capital charging power
 *   - executed_persons: primary payer (powerless/trapped) — bears the full cost of the arrangement, guilty and innocent alike; no exit exists
 *   - death_row_inmates: standing payer (powerless/trapped) — bears years-to-decades of confinement under sentence while the machinery processes appeals
 *   - wrongfully_convicted_capital_defendants: proof-of-illegitimacy payer (powerless/trapped) — the exonerated subset demonstrating that the system's errors are real and its sanction irreversible
 *   - murder_victims_families: contested payer (moderate/constrained) — conscripted as the arrangement's moral justification; many report the process prevents the closure it promises
 *   - death_penalty_opposing_citizens: excluded (moderate/constrained) — disqualified from capital juries by death qualification, absent from the sentencing decision taken in the community's name
 *   - international_human_rights_bodies: analytical observer (institutional/analytical) — documents the global abolition trend and attests from outside the domestic collector set
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__abolition_reading, 0.95).
domain_priors:suppression_score(state_execution_authority__abolition_reading, 0.88).
domain_priors:theater_ratio(state_execution_authority__abolition_reading, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__abolition_reading, snare).
narrative_ontology:human_readable(state_execution_authority__abolition_reading, "State Execution Authority — Abolition Reading (Categorical Prohibition of Capital Punishment)").
narrative_ontology:topic_domain(state_execution_authority__abolition_reading, "criminal justice/political philosophy/constitutional law").

domain_priors:requires_active_enforcement(state_execution_authority__abolition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__abolition_reading, 'dbca1f53-c2e3-4550-8489-a425f1c750a2').
narrative_ontology:cs_kernel_codification('dbca1f53-c2e3-4550-8489-a425f1c750a2', formalized).
narrative_ontology:cs_authority_grounding('dbca1f53-c2e3-4550-8489-a425f1c750a2', lineage).
narrative_ontology:cs_interpretation_layer_present('dbca1f53-c2e3-4550-8489-a425f1c750a2').
narrative_ontology:cs_reading_relation('dbca1f53-c2e3-4550-8489-a425f1c750a2', state_execution_authority__retributive_reading, forecloses).
narrative_ontology:cs_reading_relation('dbca1f53-c2e3-4550-8489-a425f1c750a2', state_execution_authority__deterrence_reading, coexists_with).
narrative_ontology:cs_axiom('dbca1f53-c2e3-4550-8489-a425f1c750a2', foundational, state_execution_categorically_impermissible).
narrative_ontology:cs_axiom_status(state_execution_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('dbca1f53-c2e3-4550-8489-a425f1c750a2', state_execution_categorically_impermissible, deontological).
narrative_ontology:cs_axiom('dbca1f53-c2e3-4550-8489-a425f1c750a2', secondary, wrongful_execution_proves_systemic_illegitimacy).
narrative_ontology:cs_axiom_status(wrongful_execution_proves_systemic_illegitimacy, holdable).
narrative_ontology:cs_axiom_grounding('dbca1f53-c2e3-4550-8489-a425f1c750a2', wrongful_execution_proves_systemic_illegitimacy, empirically_contingent).
narrative_ontology:cs_reference_frame('dbca1f53-c2e3-4550-8489-a425f1c750a2', state_without_execution_authority).
narrative_ontology:cs_drift_state('dbca1f53-c2e3-4550-8489-a425f1c750a2', contemporary, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('dbca1f53-c2e3-4550-8489-a425f1c750a2', '').
narrative_ontology:cs_kernel_id(state_execution_authority__abolition_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_execution_authority__abolition_reading, state_sovereign_authority).
narrative_ontology:constraint_beneficiary(state_execution_authority__abolition_reading, electoral_politicians).
narrative_ontology:constraint_beneficiary(state_execution_authority__abolition_reading, prosecutorial_offices).
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, executed_persons).
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, death_row_inmates).
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, wrongfully_convicted_capital_defendants).
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, murder_victims_families).
narrative_ontology:constraint_vindicates(state_execution_authority__abolition_reading, sovereign_expressive_authority_doctrine).
narrative_ontology:constraint_vindicates(state_execution_authority__abolition_reading, procedural_finality_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts the capital statutes and administers the machinery end to end — charging, sentencing, appellate review, clemency, and the execution itself. Collects the arrangement's central output: the demonstrated capacity to impose death as law, which anchors the sanction hierarchy and the state's expressive claim to final authority. Repeal is formally available at any legislative session; the expressive and political investment built around the arrangement makes the seat that could repeal it the seat most reluctant to.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, state_sovereign_authority, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(state_execution_authority__abolition_reading, state_sovereign_authority, beneficiary).

% Campaign on capital-punishment positions and collect electoral returns from tough-on-crime stances and high-profile capital prosecutions. Their stake tracks the election cycle; they can drop the issue or pivot when the electoral weather shifts, at little personal cost, and their support for the arrangement costs them nothing they do not choose to spend.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, electoral_politicians, beneficiary,
    powerful, immediate, mobile, national).

% Hold the charging decision that makes death available as leverage: the threat of capital prosecution shapes plea outcomes in thousands of cases that never reach a capital trial, and high-profile capital cases advance careers. Individual prosecutors can decline capital charging or move offices; the office-level practice outlasts any single incumbent.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, prosecutorial_offices, beneficiary,
    institutional, biographical, mobile, regional).

% Persons put to death under the arrangement — under this reading, all of them, guilty and innocent alike, bear its full cost: their lives, taken by the state and unrecoverable. There is no exit from the sanction; post-conviction voice is narrowed to counsel-mediated filings, and the sentence, once carried out, ends the person's standing to object permanently.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, executed_persons, payer,
    powerless, immediate, trapped, local).

% Persons under sentence of death awaiting execution, typically confined for years to decades under restrictive conditions while the appellate machinery processes their cases. Commutation and exoneration occur but are rare exceptions rather than exits; the default trajectory of the sentence runs to the execution chamber.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, death_row_inmates, payer,
    powerless, biographical, trapped, local).

% The subset convicted and sentenced in error — identified by post-conviction exonerations, which demonstrate that the system's errors are real while its sanction is irreversible. Those executed bear the full cost with no remedy; those exonerated after decades bear the lost years and carry the demonstration with them.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, wrongfully_convicted_capital_defendants, payer,
    powerless, biographical, trapped, local).

% Families of homicide victims, conscripted by the arrangement as its moral justification: executions are carried out in their name. Many report the decades-long process — retrials, appeals, stays, reset execution dates, media cycles — prevents the closure the arrangement promises them; a minority report satisfaction when executions proceed. The satisfaction does not enter this story's benefit accounting; the process-harm does, as widely attested from this seat.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, murder_victims_families, payer,
    moderate, biographical, constrained, local).

% Citizens whose conscientious opposition to capital punishment disqualifies them from serving on capital juries under death qualification, removing them from the sentencing decisions taken in the community's name. They object through abolition politics, amicus participation, and referenda, but are structurally absent from the one seat — the jury — where the sanction is actually imposed.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, death_penalty_opposing_citizens, excluded,
    moderate, generational, constrained, national).

% Treaty bodies and regional institutions that document the global abolition trend, review retentionist jurisdictions, and attest from outside the arrangement's domestic political set that the founding problem is answered without execution in the large majority of states. They hold no enforcement power over the home jurisdiction; their instruments are documentation, treaty review, and norm articulation.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_execution_authority__abolition_reading, state_sovereign_authority).
narrative_ontology:fixing_cost_class(state_execution_authority__abolition_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Channels the community's response to its most severe crimes into a single terminal, state-administered sanction: it defines a legal endpoint beyond which no further state response exists, concentrates the expressive force of criminal law in one act, and monopolizes retaliatory response that might otherwise revert to private vengeance.
% TRANSFER_FUNCTION: Moves life itself from condemned persons to the state's expressive account; moves a fiscal premium from taxpayers to the legal apparatus (capital prosecution, death-row confinement, and appellate machinery cost substantially more than life imprisonment); moves attention and electoral capital to politicians and prosecutors; and moves moral legitimation from the grief of victims' families to the state's justification of the act done in their name.
% ABSENT_VOICES: The executed are permanently silenced — the arrangement's primary victims cannot object after the fact, and post-conviction rules narrow the condemned's voice to counsel-mediated filings. Death-skeptical citizens are filtered out of capital juries by death qualification, so the community's moral minority is structurally absent from the sentencing decision taken in its name. The populations deterrence claims to protect are hypothetical and voiceless. All three absences are load-bearing: the unanimity of the arrangement's internal voice is produced by removing these seats from the room.
% DISAPPEARANCE_RATIONALE: The criminal-sanction stack compresses onto life imprisonment, as abolitionist jurisdictions demonstrate in practice: prosecutorial charging strategy loses its terminal lever, death qualification and the clemency machinery become vestigial, appellate capital doctrine dissolves into ordinary criminal appeals, and tough-on-crime electoral positioning loses its sharpest instrument. Nothing collapses — the substitution exists and operates elsewhere — but every named collector loses its instrument and the condemned seats' exposure ends.
% FOUNDING_PROBLEM: How should the political community answer its most severe crimes: with a sanction expressing ultimate condemnation, deterring the worst offenses, and monopolizing vengeance that would otherwise revert to private blood feud? Capital punishment was the sovereign's historical answer — the terminal sanction.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the abolitionist jurisdictions and Second Optional Protocol state parties attest in practice that the founding problem is answered without execution; UN and regional human rights bodies attest the same from the treaty seat; Murder Victims' Families for Human Rights and comparable organizations attest from the victim seat that the process fails the closure it promises. Retentionist legislatures and prosecutors' associations assert the problem remains live and execution-specific, but they sit inside the beneficiary set and their attestation is discounted accordingly.
narrative_ontology:disappearance_verdict(state_execution_authority__abolition_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_execution_authority__abolition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__abolition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_execution_authority__abolition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_execution_authority__abolition_reading, 0.95, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_execution_authority__abolition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_execution_authority__abolition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_execution_authority__abolition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.95 because the reading's referent makes the cost total: the sanction takes life, is irreversible, admits no substitution (life imprisonment is qualitatively different, so the 'same punishment short of death' defense fails), and admits no offsetting legitimate benefit under this reading. Suppression is 0.88 and structural rather than internalized: finality doctrine bars post-conviction innocence claims, death qualification filters the jury pool, execution-method secrecy statutes shield the machinery from scrutiny, and the condemned's exit set is empty by definition. Theater ratio 0.70: the procedural safeguards (proportionality review, clemency, habeas) perform legitimation more than they alter outcomes — clemency is rarely granted, certiorari is routinely denied, executions proceed through decades of 'review' — but the share is not higher because the process does exonerate a real subset. Accessibility collapse is low (0.25) because the functional alternative — life imprisonment — is fully available, cheaper, and operating across abolitionist jurisdictions; the arrangement persists by political and expressive commitment, not by necessity, which is the snare signature rather than a mountain's. Resistance is high (0.75): sustained abolition litigation, exoneration-driven innocence advocacy, moratoria, international treaty pressure, and declining death sentences and executions across the interval. The measurement trajectories track the machinery's hardening and the accumulation of demonstrated wrongful convictions rather than execution volume: per this reading, each exoneration raises the proven cost of irreversibility, and each finality doctrine raises the suppression needed to keep objections from stopping the process — so extractiveness, theater, and suppression requirement all rise together even as headline execution counts fall after their 1990s peak. All three tracked metrics run on one shared six-point grid (t=0..50, mapping to approximately 1975–2025), with end-state values equal to the base_properties scalars; the drift is monotonic, with no oscillation requiring cyclical modeling.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute divergently by construction. From the state seat the arrangement is sovereign prerogative functioning as designed; from the electoral and prosecutorial seats it is an asset — career capital and plea leverage — whose costs are borne elsewhere; from the condemned seats it is total loss with an empty exit set; from the victims'-families seat it splits, a minority reporting satisfaction when executions proceed and many attesting the decades-long process harms the closure it promises; from the international observer seat it is a rights violation in global retreat. The engine computes per-seat classifications from the structural data; the abolition claim in this file does not adjudicate them — it supplies the reading-indexed beneficiary/victim declarations the computation reads. Coalition note: the condemned cannot coalize individually — isolated, silenced, processed singly — which is what keeps their seats powerless despite their numbers; the effective coalition forms at the class level (exoneree networks, abolition movements, victims'-family organizations opposed to the process) and is what the resistance score registers.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (state_sovereign_authority, electoral_politicians, prosecutorial_offices) drive those seats toward the beneficiary end of d: the arrangement subsidizes them with expressive power, electoral capital, and charging leverage. Victim declarations (executed_persons, death_row_inmates, wrongfully_convicted_capital_defendants, murder_victims_families) drive those seats toward the target end; trapped exit options push the condemned seats to the full-target end, since no mobility or arbitrage exists against a terminal sanction. The state's exit is formally open — repeal is legislatively cheap and functions in abolitionist jurisdictions — but politically locked by the expressive investment, keeping it at the subsidized end. No directionality overrides are used: the derivation from beneficiary/victim declarations, power atoms, and exit options reproduces the intended structure without correction. As required, suppression is treated as an unscaled structural property of the arrangement; only extractiveness is scaled, by directionality and spatial scope (national scope modestly amplifies effective extraction by raising verification difficulty).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how the community answers its worst crimes with ultimate condemnation — is authored contested, not dead: retentionist parties still assert it is live, and the arrangement persists on that live, disputed demand rather than on atrophied function. The status-times-verdict pair (contested, world_rearranges) therefore does not trip the dead-mandate capture flag, and the classification is not a mandatrophy case in the classic sense. The reading's distinctive move is adjacent: it argues the founding problem is satisfiable without the arrangement's cost — finality, incapacitation, and expressive condemnation are all available through life imprisonment — so the persistence of the killing itself is not the residue of a dead mandate but the active extraction the snare classification records. If the founding problem were later resolved as dead (terminal-sanction demand collapsing as abolition normalizes), this story would drift toward piton dynamics within a rump retentionist apparatus; the persistence_source_ambiguity omega tracks that branch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is the abolition_reading of the state_execution_authority kernel; how would the retributive and deterrence sibling readings restructure the beneficiary/victim sets, epsilon, and classification over the same standing arrangement?',
    'Author the sibling stories: the retributive reading admits executed-but-guilty persons as proportionately sanctioned rather than victimized and admits retributive satisfaction on the benefit side; the deterrence reading admits potential murder victims as beneficiaries of the deterrent effect and conditions legitimacy on the empirical deterrence literature. Each sibling yields its own epsilon over the identical referent.',
    'The classification here is reading-indexed, not topic-indexed: abolition_reading computes a snare over the standing arrangement; the siblings compute different structures over the identical referent. Cross-reading comparison requires the sibling files — re-measuring this story with a different observable would violate epsilon-invariance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed classification over a shared kernel referent; sibling readings are separate constraints.').

omega_variable(
    deterrence_evidence_invariance,
    'Does execution produce a marginal deterrent effect on homicide, and does the answer move this story''s classification?',
    'Panel econometrics and natural experiments (moratoria, execution-publicity studies), in the lineage of the National Research Council''s 2012 finding that the deterrence evidence is inconclusive.',
    'None on this story: the categorical axiom makes deterrence justificatorily irrelevant regardless of the evidence, which is precisely the deontological structure distinguishing this reading from the deterrence sibling. The answer changes only the sibling''s structure — this invariance is itself diagnostic data about the reading''s grounding type.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deterrence_evidence_invariance, empirical, 'Deterrence evidence is classification-invariant for this reading; diagnostic of deontological grounding.').

omega_variable(
    wrongful_conviction_rate,
    'What is the true rate of wrongful conviction among death-sentenced persons?',
    'Survival-analysis of capital exoneration rates (the ~4%+ estimate from Gross et al. 2014 is a floor, since many exonerations are censored by execution or death in custody), expanded post-conviction testing access.',
    'Any nonzero rate combined with irreversibility supports the secondary axiom (wrongful execution proves systemic illegitimacy); a materially higher rate would accelerate the revival_pressure drift and strengthen abolition pressure; a near-zero rate would weaken the secondary axiom''s empirical grounding without touching the foundational categorical axiom.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wrongful_conviction_rate, empirical, 'True wrongful-conviction rate among the death-sentenced; grounds the systemic-illegitimacy axiom.').

omega_variable(
    persistence_source_ambiguity,
    'Does the arrangement persist because the founding problem (demand for a terminal sanction) is live, or by institutional inertia and expressive sunk cost within the retentionist apparatus?',
    'Comparative analysis of abolition transitions (do retentionist functions collapse when abolition lands?) and legislative-history analysis of retention votes: live-demand persistence shows as affirmative reauthorization; inertia persistence shows as default retention absent affirmative defense.',
    'If live demand dominates, the snare classification is stable. If inertia dominates, the arrangement acquires piton dynamics inside a rump apparatus — maintained theatrically by an agenda-setter whose cost to fix (repeal) is lower than what it bears — and the classification trajectory bends toward piton as executions wind down.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persistence_source_ambiguity, empirical, 'Live-demand versus inertia persistence; governs snare-versus-piton drift as practice declines.').

omega_variable(
    lwop_substitution_status,
    'Is life without parole a qualitatively different sanction from execution (this reading''s substitution premise), or is ''death by incarceration'' a slow-death equivalent that would extend the categorical prohibition''s victim set?',
    'Conceptual and empirical analysis of LWOP conditions, mortality, and the abolitionist movement''s internal split over LWOP as the alternative sanction.',
    'If LWOP is judged equivalent, the substitution story collapses, the victim set extends to LWOP populations, and the categorical axiom''s reach expands beyond judicial killing — restructuring the victim declarations and raising epsilon further. If qualitatively different (the authored premise), the story stands as written.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(lwop_substitution_status, conceptual, 'Status of the LWOP substitution premise inside the abolition reading itself.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__abolition_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(state_execution_abolition_tr_t0, state_execution_authority__abolition_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement_basis(state_execution_abolition_tr_t0, observed).
narrative_ontology:measurement(state_execution_abolition_tr_t10, state_execution_authority__abolition_reading, theater_ratio, 10, 0.52).
narrative_ontology:measurement_basis(state_execution_abolition_tr_t10, observed).
narrative_ontology:measurement(state_execution_abolition_tr_t20, state_execution_authority__abolition_reading, theater_ratio, 20, 0.58).
narrative_ontology:measurement_basis(state_execution_abolition_tr_t20, observed).
narrative_ontology:measurement(state_execution_abolition_tr_t30, state_execution_authority__abolition_reading, theater_ratio, 30, 0.62).
narrative_ontology:measurement_basis(state_execution_abolition_tr_t30, observed).
narrative_ontology:measurement(state_execution_abolition_tr_t40, state_execution_authority__abolition_reading, theater_ratio, 40, 0.66).
narrative_ontology:measurement_basis(state_execution_abolition_tr_t40, observed).
narrative_ontology:measurement(state_execution_abolition_tr_t50, state_execution_authority__abolition_reading, theater_ratio, 50, 0.7).
narrative_ontology:measurement_basis(state_execution_abolition_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(state_execution_abolition_be_t0, state_execution_authority__abolition_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement_basis(state_execution_abolition_be_t0, observed).
narrative_ontology:measurement(state_execution_abolition_be_t10, state_execution_authority__abolition_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement_basis(state_execution_abolition_be_t10, observed).
narrative_ontology:measurement(state_execution_abolition_be_t20, state_execution_authority__abolition_reading, base_extractiveness, 20, 0.78).
narrative_ontology:measurement_basis(state_execution_abolition_be_t20, observed).
narrative_ontology:measurement(state_execution_abolition_be_t30, state_execution_authority__abolition_reading, base_extractiveness, 30, 0.85).
narrative_ontology:measurement_basis(state_execution_abolition_be_t30, observed).
narrative_ontology:measurement(state_execution_abolition_be_t40, state_execution_authority__abolition_reading, base_extractiveness, 40, 0.9).
narrative_ontology:measurement_basis(state_execution_abolition_be_t40, observed).
narrative_ontology:measurement(state_execution_abolition_be_t50, state_execution_authority__abolition_reading, base_extractiveness, 50, 0.95).
narrative_ontology:measurement_basis(state_execution_abolition_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(state_execution_abolition_su_t0, state_execution_authority__abolition_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(state_execution_abolition_su_t0, observed).
narrative_ontology:measurement(state_execution_abolition_su_t10, state_execution_authority__abolition_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement_basis(state_execution_abolition_su_t10, observed).
narrative_ontology:measurement(state_execution_abolition_su_t20, state_execution_authority__abolition_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(state_execution_abolition_su_t20, observed).
narrative_ontology:measurement(state_execution_abolition_su_t30, state_execution_authority__abolition_reading, suppression_requirement, 30, 0.78).
narrative_ontology:measurement_basis(state_execution_abolition_su_t30, observed).
narrative_ontology:measurement(state_execution_abolition_su_t40, state_execution_authority__abolition_reading, suppression_requirement, 40, 0.84).
narrative_ontology:measurement_basis(state_execution_abolition_su_t40, observed).
narrative_ontology:measurement(state_execution_abolition_su_t50, state_execution_authority__abolition_reading, suppression_requirement, 50, 0.88).
narrative_ontology:measurement_basis(state_execution_abolition_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_execution_authority__abolition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_execution_authority__abolition_reading, state_execution_authority__retributive_reading).
narrative_ontology:affects_constraint(state_execution_authority__abolition_reading, state_execution_authority__deterrence_reading).

% DUAL FORMULATION NOTE:
% The colloquial 'death penalty debate' decomposes, per the epsilon-invariance principle, into three structurally distinct readings of one kernel (state_execution_authority): this file (abolition_reading — categorical prohibition, all executed persons victimized including the guilty, no legitimate beneficiaries), state_execution_authority__retributive_reading (execution as proportionate desert), and state_execution_authority__deterrence_reading (execution as cost-raising deterrence). Each reading authors its own epsilon over the same standing arrangement; they are separate constraints linked through reading_relations and this network edge set, not one constraint with a measurement parameter. The upstream/downstream structure: the empirical deterrence literature feeds the deterrence sibling; the exoneration record feeds this reading's systemic-illegitimacy axiom; neither sibling's file can absorb the others' epsilon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
