% ============================================================================
% CONSTRAINT STORY: versailles_reparations_clauses__punitive_liability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_versailles_reparations_clauses__punitive_liability_reading, []).

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
 *   constraint_id: versailles_reparations_clauses__punitive_liability_reading
 *   human_readable: Versailles War-Guilt Liability Regime (Punitive Liability Reading)
 *   domain: international_relations/legal_history/political_economy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of a contested kernel. Kernel: the
 *   Versailles reparations clauses (Articles 231-232 and the Reparations
 *   Commission machinery). Reading instantiated here: the
 *   punitive_liability_reading - that Germany bears unique moral and
 *   financial responsibility for total war costs, and that Article 231
 *   therefore grounds quasi-unlimited reparations claims. The epsilon
 *   referent is the standing arrangement this reading produced and governed:
 *   the 1919-1924 regime of open-ended claims assessed by an inter-allied
 *   commission and enforced by sanctions culminating in the Ruhr occupation.
 *   Sibling readings (limited_responsibility_reading, repudiation_reading)
 *   are separate constraints with their own files, epsilon values, and
 *   stakeholder structures; they enter this story only through network links
 *   and omega variables, never averaged into this classification. KEY AGENTS
 *   (by structural relationship): - allied_creditor_states: Primary
 *   beneficiary (institutional/mobile) - holds the treaty claims, receives
 *   deliveries, recycles receipts into war-debt service -
 *   reparation_commission_allied_control: Agenda setter
 *   (institutional/constrained) - fixes claim totals, schedules, and
 *   sanctions; interprets the liability article - german_workers_taxpayers:
 *   Primary target (powerless/trapped) - bears the transfer through taxation
 *   and currency depreciation - german_reich_finance_ministry:
 *   Dual-positioned intermediary (organized/constrained) - administers the
 *   domestic side of the transfer, oscillates between fulfillment and
 *   resistance - french_belgian_reconstruction_interests: Secondary
 *   beneficiary (organized/constrained) - receives coal deliveries and
 *   reconstruction funding - german_negotiating_delegation_1919: Excluded
 *   voice (powerless/trapped) - objections to the liability article overruled
 *   before signature - keynesian_neutral_economists: Analytical observer
 *   (analytical/analytical) - documents the gap between claim measure and
 *   productive capacity
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(versailles_reparations_clauses__punitive_liability_reading, 0.84).
domain_priors:suppression_score(versailles_reparations_clauses__punitive_liability_reading, 0.86).
domain_priors:theater_ratio(versailles_reparations_clauses__punitive_liability_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, extractiveness, 0.84).
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 0.86).
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(versailles_reparations_clauses__punitive_liability_reading, snare).
narrative_ontology:human_readable(versailles_reparations_clauses__punitive_liability_reading, "Versailles War-Guilt Liability Regime (Punitive Liability Reading)").
narrative_ontology:topic_domain(versailles_reparations_clauses__punitive_liability_reading, "international_relations/legal_history/political_economy").

domain_priors:requires_active_enforcement(versailles_reparations_clauses__punitive_liability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(versailles_reparations_clauses__punitive_liability_reading, '228470f0-1e9c-42d4-9440-72e072f7bd1b').
narrative_ontology:cs_kernel_codification('228470f0-1e9c-42d4-9440-72e072f7bd1b', fixed_text).
narrative_ontology:cs_authority_grounding('228470f0-1e9c-42d4-9440-72e072f7bd1b', extraction).
narrative_ontology:cs_interpretation_layer_present('228470f0-1e9c-42d4-9440-72e072f7bd1b').
narrative_ontology:cs_reading_relation('228470f0-1e9c-42d4-9440-72e072f7bd1b', versailles_reparations_clauses__limited_responsibility_reading, influences).
narrative_ontology:cs_reading_relation('228470f0-1e9c-42d4-9440-72e072f7bd1b', versailles_reparations_clauses__repudiation_reading, influences).
narrative_ontology:cs_axiom('228470f0-1e9c-42d4-9440-72e072f7bd1b', foundational, unique_german_war_guilt_moral_verdict).
narrative_ontology:cs_axiom_status(unique_german_war_guilt_moral_verdict, holdable).
narrative_ontology:cs_axiom_grounding('228470f0-1e9c-42d4-9440-72e072f7bd1b', unique_german_war_guilt_moral_verdict, empirically_contingent).
narrative_ontology:cs_axiom('228470f0-1e9c-42d4-9440-72e072f7bd1b', foundational, article_231_grounds_unlimited_claim_measure).
narrative_ontology:cs_axiom_status(article_231_grounds_unlimited_claim_measure, holdable).
narrative_ontology:cs_axiom_grounding('228470f0-1e9c-42d4-9440-72e072f7bd1b', article_231_grounds_unlimited_claim_measure, conventional).
narrative_ontology:cs_reference_frame('228470f0-1e9c-42d4-9440-72e072f7bd1b', unconditional_total_cost_liability).
narrative_ontology:cs_drift_state('228470f0-1e9c-42d4-9440-72e072f7bd1b', dawes_plan_inquiry, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('228470f0-1e9c-42d4-9440-72e072f7bd1b', '').
narrative_ontology:cs_kernel_id(versailles_reparations_clauses__punitive_liability_reading, versailles_reparations_clauses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__punitive_liability_reading, allied_creditor_states).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__punitive_liability_reading, french_belgian_reconstruction_interests).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, german_workers_taxpayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, german_reich_finance_ministry).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__punitive_liability_reading, article_231_war_guilt_finding).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__punitive_liability_reading, interallied_debt_pooling_formula).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% France, Belgium, and Britain hold the treaty-established claims. They receive cash deliveries, coal shipments, timber, and merchant shipping under Reparations Commission schedules, and recycle much of what they collect into war-debt service owed to the United States. Their governments can moderate or harden collection terms by political decision; no external power compels them to keep collecting.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, allied_creditor_states, beneficiary,
    institutional, generational, mobile, continental).

% Industrial and agricultural interests in northern France and Belgium receive coal deliveries and reconstruction funding sourced from German payments. Their regions absorbed the war's physical destruction; they press their governments for maximum collection and treat any capacity-based reduction as a direct transfer away from them.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, french_belgian_reconstruction_interests, beneficiary,
    organized, biographical, constrained, regional).

% An inter-allied body seated in Paris determines the claim total, fixes annual payment schedules, certifies German defaults, and authorizes sanctions up to military occupation. It interprets the treaty text - deciding, for instance, that pensions and separation allowances count as compensable damage. It exists only so long as the claims regime it administers persists.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, reparation_commission_allied_control, agenda_setter,
    institutional, generational, constrained, continental).

% Bear the transfer through income taxes, forced levies, and - decisively - currency depreciation as the Reich monetizes deficits to meet external demands. Mark-denominated savings are destroyed. Emigration is available to a few; the population as a whole has nowhere to move its tax burden, and refusing payment invites occupation of German territory.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, german_workers_taxpayers, payer,
    powerless, biographical, trapped, national).

% Administers the domestic side of the arrangement: raises the taxes and loans that fund deliveries, negotiates schedule revisions, and implements or withholds compliance. Caught between external claimants who can occupy its territory and a domestic electorate that punishes fulfillment politics, its policy oscillates between fulfillment and passive resistance.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, german_reich_finance_ministry, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(versailles_reparations_clauses__punitive_liability_reading, german_reich_finance_ministry, agenda_setter).

% Received the treaty terms as a finished draft in May 1919, permitted to submit written objections but not to negotiate the liability article. Signed under threat of resumed invasion. Would have contested both the moral premise and the open-ended claim measure; its objections were noted and overruled.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, german_negotiating_delegation_1919, excluded,
    powerless, immediate, trapped, national).

% Analytical seat outside the belligerent governments: Keynes resigned from the British Treasury delegation and published a capacity-based critique arguing the claim total exceeded any feasible German surplus; later joined by the Dawes Committee's international experts. They take no side in collection politics but document the gap between the claim measure and German productive capacity.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, keynesian_neutral_economists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(versailles_reparations_clauses__punitive_liability_reading, allied_creditor_states).
narrative_ontology:fixing_cost_class(versailles_reparations_clauses__punitive_liability_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ranks and pools the Allies' competing claims against a single debtor so each creditor state's share is set by an agreed formula rather than a bilateral scramble, and provides one common legal instrument (the liability article) on which every claim can be written.
% TRANSFER_FUNCTION: Moves gold, foreign exchange, coal, timber, chemical products, shipping tonnage, and fiscal claims from German taxpayers and workers to the treasuries and reconstruction programs of France, Belgium, and Britain, with part recycled onward to the United States as war-debt service.
% ABSENT_VOICES: The German delegation was handed the liability article as a non-negotiable draft; the United States, whose arbitration the Germans had been promised under the Fourteen Points, left the conversation after Senate ratification failure; neutral economists spoke only from outside. The seats that would have contested the moral premise and the open-ended claim measure were absent from the room where both were fixed.
% DISAPPEARANCE_RATIONALE: If the punitive-liability arrangement vanished overnight, Allied budget plans built on German receipts collapse, the occupation and sanctions machinery dissolves, German fiscal sovereignty is restored immediately, and the inter-allied debt chain loses its planned settlement source - the European financial order of the early 1920s reorganizes around whatever capacity-bounded or repudiated settlement replaces it.
% FOUNDING_PROBLEM: After a war of unprecedented destruction, who bears the cost - the defeated state alone, the victors themselves, or neutrals - and on what legal and moral basis are claims against a sovereign state established?
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: Keynes's contemporary capacity analysis, the British Treasury's post-1921 revisionism, and above all the Dawes Committee (1924) - an international body including American and neutral experts convened by the claimants' own commission - attest that the founding problem of cost allocation remained live while the unlimited-claim answer had failed. No corroborating source outside the creditor bloc attests the quasi-unlimited claim measure itself.
narrative_ontology:disappearance_verdict(versailles_reparations_clauses__punitive_liability_reading, world_rearranges).
narrative_ontology:founding_problem_status(versailles_reparations_clauses__punitive_liability_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(versailles_reparations_clauses__punitive_liability_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(versailles_reparations_clauses__punitive_liability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(versailles_reparations_clauses__punitive_liability_reading, 0.84, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(versailles_reparations_clauses__punitive_liability_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(versailles_reparations_clauses__punitive_liability_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(versailles_reparations_clauses__punitive_liability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.84 at interval end) because the claim measure is decoupled from any capacity test: the assessed stock and annual demands were set by the creditors' own commission, and the London Schedule of 1921 fixed demands no independent analysis judged collectable. Suppression is higher still (0.86) because the arrangement's persistence depends on coercive machinery - default certification, product liens, customs controls, and finally military occupation of German territory - not on participant assent; suppression is authored as a raw structural property and is deliberately NOT scaled by power or scope. Theater rises to 0.42 because by 1923-24 a growing share of the machinery performs liability rather than collecting it: assessments, note ceremonies, and occupation administration continued while net receipts fell below the cost of enforcement itself. Accessibility_collapse is 0.62: from the German seat alternatives collapsed nearly completely (pay, be occupied, or monetary collapse - there was no bankruptcy procedure for a sovereign state inside this reading), but capacity-based alternative readings stayed live at the system level and ultimately displaced this arrangement, so collapse is high but not natural-law grade. Resistance is 0.72: Ruhr passive resistance, hyperinflation functioning as de facto default, and British drift toward revisionism. The claimed type (snare) is authored from structure - a moral-premise cover story over coercion-dependent extraction with identifiable victims - independently of these metric values; the engine computes per-seat classifications from the structural data. All three metric series run on one shared time grid (t = years since June 1919; t=5 is the Dawes Report moment) so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter/beneficiary seats should compute differently. From the creditor and commission seats the arrangement presents as the settlement of a legitimate debt: they experience the coordination half (pooling and ranking claims) and read German objections as evasion. From the trapped German seats the same structure operates as open-ended expropriation enforced by invasion threat. Same-level divergence matters too: Britain and France hold the same nominal creditor power, but France borders the debtor and can occupy, while Britain's collection leverage is diplomatic only - so their experienced constraint and their exit options differ despite equal standing. The Reich finance ministry is genuinely dual-positioned: it administers the extraction domestically while bearing it externally, and its seat should compute as a target with an administrator's view of the machinery.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive the derivation and no overrides are needed. allied_creditor_states sits near the beneficiary end (d low): the arrangement subsidizes their treasuries and their exit is mobile - they can moderate terms by political decision, as Britain repeatedly urged. french_belgian_reconstruction_interests is beneficiary but constrained: their receipts depend on the regime continuing, pulling d slightly up from the pure-beneficiary end. reparation_commission_allied_control is the agenda-setter seat: it administers and interprets but collects no rents itself - gains pass through to creditor treasuries - so its extraction exposure is minimal while its enforcement role is maximal. german_workers_taxpayers sits nearest the full-target end (d high): trapped targets with no arbitrage exit absorb the scaled extraction, amplified by the continental scope that makes verification of German performance hard and suspicion cheap. german_reich_finance_ministry derives high d as a payer even though it administers: its administration serves the transfer without capturing it. Suppression of alternatives is concentrated on the German seats; the creditor seats face no suppression at all.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification discipline cuts both ways here. Calling the whole reparations complex pure coordination would erase the German victims and launder a coercion-backed transfer as burden-sharing; calling every reparations arrangement a snare would smear the limited_responsibility_reading, in which the same kernel supports a genuine capacity-bounded settlement with a real inter-allied coordination function. Decomposing the kernel into three readings is what prevents both errors. Within THIS reading, the coordination function (claim pooling) is real but subordinate: the quasi-unlimited claim measure is the defining feature, and it survives only under active enforcement - hence snare rather than tangled_rope. On mandatrophy: the founding problem (who bears the costs of unprecedented destruction) was live throughout the interval, so this is not a zombie mandate; the arrangement's demise came through political displacement by a sibling reading (the Dawes capacity settlement), not through internal decay of a function nobody needed. The mismatch consumer should find status=live x verdict=world_rearranges - consistent, no capture/zombie flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is the punitive_liability_reading of kernel versailles_reparations_clauses; how would instantiating the sibling readings change the structural classification?',
    'Generate and classify the sibling files (limited_responsibility_reading, repudiation_reading) and compare computed types, beneficiary/victim sets, and epsilon across the family.',
    'The limited reading should compute with a genuine coordination component (inter-allied burden-sharing) and bounded extraction, i.e. a tangled-rope shape; the repudiation reading inverts the beneficiary/victim structure entirely. The classification of THIS file must not be generalized to the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: one of three readings of a shared kernel; classification is reading-indexed.').

omega_variable(
    unique_guilt_empirical_status,
    'Does the unique-German-war-guilt premise underlying the moral verdict survive historiographic testing?',
    'Archival historiography of war origins (Kautsky documents; later Fischer-school research on German war aims and responsibility).',
    'If the empirical premise fails, the moral foundation of the unlimited claim measure collapses, accelerating displacement toward the limited or repudiation readings and dating the axiom_overriding drift earlier than the Dawes moment recorded here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unique_guilt_empirical_status, empirical, 'Empirical status of the war-guilt premise on which the reading''s authority rests.').

omega_variable(
    liability_measure_vs_collectability,
    'Does the arrangement conflate the measure of liability (quasi-unlimited claim stock) with the schedule of payment (annual capacity)? How much of the measured extraction is paper claim versus effective transfer?',
    'Compare the nominal claim stock and assessed schedules against actually delivered transfers (cash, coal, timber, in-kind deliveries) year by year; the gap isolates paper-liability extraction.',
    'If most measured extraction is uncollectable claim stock, effective extraction is lower than the headline epsilon and the arrangement''s harm runs more through monetary destabilization of the German economy than through net resource transfer to creditors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liability_measure_vs_collectability, conceptual, 'Whether epsilon reflects realized transfer or uncollectable paper claims.').

omega_variable(
    duress_validity_doctrine,
    'Does the reading''s validity premise - that a signed treaty binds notwithstanding that its terms were presented as a finished draft under threat of resumed invasion - withstand contemporaneous and later consent doctrine?',
    'Doctrinal analysis of treaty-consent standards (pre-Vienna-Convention practice, later VCLT art. 52 analogues) applied to the 1919 signature conditions.',
    'If duress invalidates, the reading''s legal foundation collapses into the repudiation_reading and the beneficiary/victim structure inverts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(duress_validity_doctrine, conceptual, 'Legal-validity ambiguity at the reading''s foundation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(versailles_reparations_clauses__punitive_liability_reading, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(versailles_punitive_tr_t0, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(versailles_punitive_tr_t0, observed).
narrative_ontology:measurement(versailles_punitive_tr_t1, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1, 0.22).
narrative_ontology:measurement_basis(versailles_punitive_tr_t1, observed).
narrative_ontology:measurement(versailles_punitive_tr_t2, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 2, 0.25).
narrative_ontology:measurement_basis(versailles_punitive_tr_t2, observed).
narrative_ontology:measurement(versailles_punitive_tr_t3, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 3, 0.3).
narrative_ontology:measurement_basis(versailles_punitive_tr_t3, observed).
narrative_ontology:measurement(versailles_punitive_tr_t4, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 4, 0.38).
narrative_ontology:measurement_basis(versailles_punitive_tr_t4, observed).
narrative_ontology:measurement(versailles_punitive_tr_t5, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 5, 0.42).
narrative_ontology:measurement_basis(versailles_punitive_tr_t5, observed).

% Extraction over time
narrative_ontology:measurement(versailles_punitive_be_t0, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement_basis(versailles_punitive_be_t0, observed).
narrative_ontology:measurement(versailles_punitive_be_t1, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1, 0.75).
narrative_ontology:measurement_basis(versailles_punitive_be_t1, observed).
narrative_ontology:measurement(versailles_punitive_be_t2, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 2, 0.8).
narrative_ontology:measurement_basis(versailles_punitive_be_t2, observed).
narrative_ontology:measurement(versailles_punitive_be_t3, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 3, 0.8).
narrative_ontology:measurement_basis(versailles_punitive_be_t3, observed).
narrative_ontology:measurement(versailles_punitive_be_t4, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 4, 0.86).
narrative_ontology:measurement_basis(versailles_punitive_be_t4, observed).
narrative_ontology:measurement(versailles_punitive_be_t5, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 5, 0.84).
narrative_ontology:measurement_basis(versailles_punitive_be_t5, observed).

% Suppression requirement over time
narrative_ontology:measurement(versailles_punitive_su_t0, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(versailles_punitive_su_t0, observed).
narrative_ontology:measurement(versailles_punitive_su_t1, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1, 0.6).
narrative_ontology:measurement_basis(versailles_punitive_su_t1, observed).
narrative_ontology:measurement(versailles_punitive_su_t2, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 2, 0.68).
narrative_ontology:measurement_basis(versailles_punitive_su_t2, observed).
narrative_ontology:measurement(versailles_punitive_su_t3, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 3, 0.74).
narrative_ontology:measurement_basis(versailles_punitive_su_t3, observed).
narrative_ontology:measurement(versailles_punitive_su_t4, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 4, 0.88).
narrative_ontology:measurement_basis(versailles_punitive_su_t4, observed).
narrative_ontology:measurement(versailles_punitive_su_t5, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 5, 0.86).
narrative_ontology:measurement_basis(versailles_punitive_su_t5, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(versailles_reparations_clauses__punitive_liability_reading, resource_allocation).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, versailles_reparations_clauses__limited_responsibility_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, versailles_reparations_clauses__repudiation_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'German reparations under the Treaty of Versailles' decomposes into three structurally distinct readings of one kernel, per the epsilon-invariance principle. This story carries the punitive_liability_reading (high epsilon, coercion-enforced, dominant 1919-1924). The limited_responsibility_reading carries the capacity-bounded settlement (Dawes/Young era): real inter-allied coordination, bounded extraction, tangled-rope-shaped. The repudiation_reading carries the duress-invalidity position with an inverted claim structure. Causal structure across the family: the punitive reading's enforcement failure (Ruhr occupation, hyperinflation) generated the evidentiary and political conditions for the limited reading's adoption, while its harshness simultaneously fed repudiationist mobilization inside Germany. Each family member links the others via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
