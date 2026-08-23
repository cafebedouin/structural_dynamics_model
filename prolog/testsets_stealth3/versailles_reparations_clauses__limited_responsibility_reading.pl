% ============================================================================
% CONSTRAINT STORY: versailles_reparations_clauses__limited_responsibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_versailles_reparations_clauses__limited_responsibility_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: versailles_reparations_clauses__limited_responsibility_reading
 *   human_readable: Versailles Reparations - Capacity-Bounded Liability Reading (Article 231 as Legal Formality)
 *   domain: international relations/legal history/political economy
 *
 * SUMMARY:
 *   The Treaty of Versailles fixed a reparations claim on Germany: Article
 *   231 establishing the liability basis, annexed schedules fixing sums,
 *   delivery obligations, and sanction threats. The colloquial label
 *   'Versailles reparations' covers one contested kernel read three ways;
 *   this file instantiates the limited_responsibility_reading - reparations
 *   must track German economic capacity, Article 231 is a legal-liability
 *   formality rather than a moral judgment, and payments are bounded by
 *   viability. The referent for epsilon is the standing arrangement under
 *   contest - the imposed claim structure as it operated from 1919 - assessed
 *   by this reading's own lights: the reading concedes that a genuine
 *   compensation obligation exists (Belgium's devastation, civilian damages)
 *   while denying that the headline sum or its moralized framing is binding,
 *   and its own operation revised the schedules repeatedly downward (London
 *   Schedule 1921, Dawes 1924, Young 1929, effective abolition at Lausanne
 *   1932). Assumptions stated: the interval runs from the treaty's signing
 *   (1919) to the Lausanne termination (1932); base-property values are
 *   end-state judgments reflecting the post-Lausanne phase; historical actors
 *   are mapped to seats at the power levels they actually held; the punitive
 *   and repudiation siblings are separate constraint files and are not
 *   averaged into this one. KEY AGENTS (by structural relationship): -
 *   german_industrial_financial_elites: primary beneficiary (powerful/mobile)
 *   - retains capital, supplies the official capacity figures, converts every
 *   schedule ceiling into preserved assets - german_diplomatic_establishment:
 *   beneficiary with administrative hand (institutional/identity_locked) -
 *   runs treaty revision by legal means - american_transatlantic_lenders:
 *   structural beneficiary (institutional/arbitrage) - finances the circular
 *   flow, enforces serviceability through loan conditions -
 *   french_reparations_claimants: primary payer among creditors
 *   (powerful/constrained) - booked claims shrink to capacity-sized schedules
 *   - belgian_reconstruction_claimants: payer with weakest leverage
 *   (moderate/trapped) - reconstruction shortfall becomes permanent -
 *   german_taxpaying_classes: internal payer (powerless/trapped) - carries
 *   inflation loss, annuity taxation, deflationary adjustment -
 *   dawes_young_administrative_machinery: agenda setter
 *   (institutional/constrained) - draws schedules, certifies capacity,
 *   supervises German finances - keynesian_institutional_analysts: analytical
 *   observer (analytical/analytical) - establishes the transfer-feasibility
 *   critique the reading rests on
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(versailles_reparations_clauses__limited_responsibility_reading, 0.52).
domain_priors:suppression_score(versailles_reparations_clauses__limited_responsibility_reading, 0.5).
domain_priors:theater_ratio(versailles_reparations_clauses__limited_responsibility_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(versailles_reparations_clauses__limited_responsibility_reading, tangled_rope).
narrative_ontology:human_readable(versailles_reparations_clauses__limited_responsibility_reading, "Versailles Reparations - Capacity-Bounded Liability Reading (Article 231 as Legal Formality)").
narrative_ontology:topic_domain(versailles_reparations_clauses__limited_responsibility_reading, "international relations/legal history/political economy").

domain_priors:requires_active_enforcement(versailles_reparations_clauses__limited_responsibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(versailles_reparations_clauses__limited_responsibility_reading, '7885f53d-8727-4b66-b59e-fbe65ba5abda').
narrative_ontology:cs_kernel_codification('7885f53d-8727-4b66-b59e-fbe65ba5abda', fixed_text).
narrative_ontology:cs_authority_grounding('7885f53d-8727-4b66-b59e-fbe65ba5abda', expertise).
narrative_ontology:cs_interpretation_layer_present('7885f53d-8727-4b66-b59e-fbe65ba5abda').
narrative_ontology:cs_reading_relation('7885f53d-8727-4b66-b59e-fbe65ba5abda', versailles_reparations_clauses__punitive_liability_reading, coexists_with).
narrative_ontology:cs_reading_relation('7885f53d-8727-4b66-b59e-fbe65ba5abda', versailles_reparations_clauses__repudiation_reading, influences).
narrative_ontology:cs_axiom('7885f53d-8727-4b66-b59e-fbe65ba5abda', foundational, article231_is_legal_instrument_not_moral_verdict).
narrative_ontology:cs_axiom_status(article231_is_legal_instrument_not_moral_verdict, holdable).
narrative_ontology:cs_axiom_grounding('7885f53d-8727-4b66-b59e-fbe65ba5abda', article231_is_legal_instrument_not_moral_verdict, conventional).
narrative_ontology:cs_axiom('7885f53d-8727-4b66-b59e-fbe65ba5abda', foundational, annuities_bounded_by_verified_payment_capacity).
narrative_ontology:cs_axiom_status(annuities_bounded_by_verified_payment_capacity, holdable).
narrative_ontology:cs_axiom_grounding('7885f53d-8727-4b66-b59e-fbe65ba5abda', annuities_bounded_by_verified_payment_capacity, instrumental).
narrative_ontology:cs_reference_frame('7885f53d-8727-4b66-b59e-fbe65ba5abda', capacity_indexed_liability_framework).
narrative_ontology:cs_drift_state('7885f53d-8727-4b66-b59e-fbe65ba5abda', lausanne_abolition_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7885f53d-8727-4b66-b59e-fbe65ba5abda', '').
narrative_ontology:cs_kernel_id(versailles_reparations_clauses__limited_responsibility_reading, versailles_reparations_clauses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__limited_responsibility_reading, german_industrial_financial_elites).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__limited_responsibility_reading, german_diplomatic_establishment).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__limited_responsibility_reading, american_transatlantic_lenders).
narrative_ontology:constraint_victim(versailles_reparations_clauses__limited_responsibility_reading, french_reparations_claimants).
narrative_ontology:constraint_victim(versailles_reparations_clauses__limited_responsibility_reading, belgian_reconstruction_claimants).
narrative_ontology:constraint_victim(versailles_reparations_clauses__limited_responsibility_reading, german_taxpaying_classes).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__limited_responsibility_reading, transfer_problem_theory).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__limited_responsibility_reading, keynes_economic_consequences_analysis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own and direct German heavy industry and banking. Between 1919 and 1924 the reparations claim threatened their asset base; the argument that payments must fit productive capacity let them keep factories, rail links, and capital at home while presenting every reduction as technical necessity rather than defiance. Capital moved freely across borders during the inflation years, and after stabilization they staffed the bodies supplying the official capacity figures. What flows to them: preserved industrial assets, protected margins, and effective veto power over any schedule drawn above their own estimates.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, german_industrial_financial_elites, beneficiary,
    powerful, biographical, mobile, continental).

% Foreign-ministry and chancellery careers built around treaty revision by legal means. They supply the capacity studies, negotiate the schedule adjustments from London through Dawes and Young, and carry abroad the argument that Germany intends to pay whatever it verifiably can. Professional standing rests on being the respectable alternative to both open repudiation at home and renewed coercion from abroad; abandoning that stance means defecting to positions their careers defined themselves against.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, german_diplomatic_establishment, beneficiary,
    institutional, biographical, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(versailles_reparations_clauses__limited_responsibility_reading, german_diplomatic_establishment, agenda_setter).

% Banking houses led by J.P. Morgan & Co. finance the circular flow: loans to German states and municipalities, reparations remittances to Allied governments, war-debt installments back to Washington, and interest returning at every hop. Their lending conditions require the payment schedule to look serviceable; they place personnel on the Dawes Committee and fund the Agent-General's office. Exposure is hedged by diversification and scale, and capital can be withdrawn or redirected faster than any single government can react.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, american_transatlantic_lenders, beneficiary,
    institutional, biographical, arbitrage, global).

% The French treasury, the devastated-department regions, and millions of booked pension and widow claims counted against expected German transfers. Governments had sold the war effort partly on the promise that Germany would pay; when schedules shrank to verified capacity, the gap landed on French taxpayers and the franc. Unilateral pressure - the Ruhr occupation - raised costs faster than collections, leaving continuation dependent on British and American goodwill Paris could not command.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, french_reparations_claimants, payer,
    powerful, biographical, constrained, national).

% A small, heavily damaged country whose reconstruction bill exceeds everything Germany will ultimately pay. Its delegation anchors the original claim structure, but its leverage depends entirely on the larger allies; once the schedule is capacity-bounded, Belgium has no independent path to reopen the amount and absorbs the shortfall permanently in unbuilt infrastructure and uncompensated households.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, belgian_reconstruction_claimants, payer,
    moderate, biographical, trapped, national).

% German wage earners and small savers. They live through the inflation that erases life savings in 1923, then carry the disciplined taxation and municipal charges servicing the annuities, then the deflationary wage cuts of the early 1930s. The same official documents that tell the Allies Germany can pay little are used to hold domestic social spending down. Emigration is the main individual exit and is out of reach for most.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, german_taxpaying_classes, payer,
    powerless, immediate, trapped, national).

% The international apparatus assembled to operate the settlement: the Reparation Commission, the Dawes Committee's expert panels, the Agent-General for Reparation Payments, the Transfer Committee, and later the Bank for International Settlements. It draws the annual schedules, supervises German budgets and the Reichsbank, certifies capacity, and arbitrates between payers and claimants. Its staffing and continuation depend on the settlement remaining in force; it cannot step away from its mandate without ending itself.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, dawes_young_administrative_machinery, agenda_setter,
    institutional, generational, constrained, continental).

% Economists and public intellectuals - Keynes foremost - arguing from 1919 onward that a claim sized beyond transferable surplus would destroy the payer and the claim alike. They publish, testify, and advise delegations; their reward is reputational, and their access to decision tables rises as official predictions fail.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, keynesian_institutional_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(versailles_reparations_clauses__limited_responsibility_reading, german_industrial_financial_elites).
narrative_ontology:fixing_cost_class(versailles_reparations_clauses__limited_responsibility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Sustains any reparations transfer at all without collapsing the payer: sizes annuities to independently reviewed capacity, supervises German finances to reassure lenders, and cycles American credit through Germany to the Allied treasuries so that payment remains physically possible.
% TRANSFER_FUNCTION: Moves gold-mark annuities and in-kind deliveries (coal, timber, chemical patents) from German taxpayers and industry to Belgian, French, Italian, and British treasuries; moves interest and service payments from every node of the circuit back to American lenders; and leaves foregone claim value and negotiating time with German elites, who retain the underlying assets.
% ABSENT_VOICES: Belgian devastated-zone civilians and French widows' and orphans' associations, whose individual claims anchored the original entitlement but enter the room only as consolidated state line items; German small savers wiped out in the 1923 inflation, voiced by neither the elites pleading incapacity nor the creditors counting capacity; and colonial subjects of the Allied powers, excluded from compensation accounting altogether. They would object both to the shrinking of the claim and to the domestic burden-shifting done in its name.
% DISAPPEARANCE_RATIONALE: If capacity-bounding vanished overnight in the mid-1920s, the 132-billion-gold-mark headline claim revives with its moralized framing: France resumes coercive collection, German default and currency collapse follow, the American loan cycle halts, and the Locarno security architecture and Germany's League admission never form. Every major arrangement of 1924-1929 depends on this reading having won.
% FOUNDING_PROBLEM: After the Ruhr occupation demonstrated that forced extraction beyond capacity destroys the source of payment, the problem was to design a schedule Germany could verifiably service while giving creditors assurance that payment would actually continue - converting a political claim into a payable one without either side losing face.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: Keynes's 'The Economic Consequences of the Peace' (1919) argued the claim exceeded transferable surplus before any German government advanced the argument; the 1922 Balfour Note records British Treasury doubt about collectability; the Dawes Committee itself concluded the 1921 schedule was unserviceable. Against this, German nationalist testimony insisted the problem was permanent and unpayable in principle, while French hardliners insisted Germany could pay but would not - the status is contested because the parties never agreed on which failure mode was real.
narrative_ontology:disappearance_verdict(versailles_reparations_clauses__limited_responsibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(versailles_reparations_clauses__limited_responsibility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(versailles_reparations_clauses__limited_responsibility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(versailles_reparations_clauses__limited_responsibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(versailles_reparations_clauses__limited_responsibility_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(versailles_reparations_clauses__limited_responsibility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(versailles_reparations_clauses__limited_responsibility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(versailles_reparations_clauses__limited_responsibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored at 0.52. The reading's operation strips real value from the Allied claim structure - the 132-billion-gold-mark headline never survived contact with capacity certification, actual net German payments ran near a tenth of it, and the gap landed on French and Belgian budgets - while a genuine coordination core remains: a payer kept solvent is the only configuration in which anyone is paid at all, and the American credit cycle that kept Europe liquid was built on the capacity frame. Suppression 0.50 is the end-state of a deliberately non-monotonic enforcement history: installing the reading required heavy enforcement (foreign control of the Reichsbank, railway mortgages, loan conditionality), the Locarno detente relaxed it, the Young Plan ratcheted it back up, and the Depression plus Lausanne dissolved its object. Theater ratio 0.35: the expert-committee format performed neutral technicality over political bargains and German budget presentations were staged, but schedules were drawn, money moved, and supervision was real. Accessibility collapse 0.40: the punitive alternative largely died at the Ruhr, yet the repudiation alternative stayed alive underground and surfaced in the 1929 Young Plan referendum - alternatives were narrowed, not extinguished. Resistance 0.65: French nationalism fought the reading openly until 1924; German nationalists fought its concessions from the right throughout. All three temporal series share one seven-point grid (1919, 1921, 1923, 1924, 1926, 1929, 1932); the suppression oscillation tracks enforcement crises (Ruhr 1923, Locarno 1926, Depression 1929-1931), not intermittent reinforcement - the cycle is enforcement-cost response, not an extraction mechanism. Coalition check: the weaker creditor seats attempted coordination (Franco-Belgian alignment at London 1921 and in the occupation) and were defeated by Anglo-American financial leverage, which is why trapped Belgium sits structurally below powerful-but-constrained France. Claim and metrics are authored independently: tangled_rope is my structural belief - both a genuine coordination function and asymmetric extraction running through the same machinery, actively enforced - and the metric values are descriptive judgments, not tuned toward any classifier output.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the German industrial-financial seat the arrangement is a shield that worked: assets preserved, ceilings negotiated, every reduction framed as arithmetic. From the French creditor seat the same structure is dispossession of booked entitlement - a moral claim converted into a budget line and then shrunk. From the American lender seat it is a profitable stability machine whose risks were diversified away. From the German taxpayer seat it is a shell game in which elites invoked the nation's poverty while personal fortunes crossed borders. The diplomatic establishment sits closest to the reading's self-image; its exit is identity_locked because abandoning the fulfillment path meant defecting to the extremes its careers were built against - break that professional identity frame and the seat's effective position shifts toward mobile, changing what the arrangement costs it. Inter-institutionally, the French state and the American banks hold comparable nominal power but face opposite exit structures: France's enforcement option burned out at the Ruhr, while Morgan's capital was never trapped anywhere. These are computed divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low d: german_industrial_financial_elites sit near the subsidized end (assets retained, leverage gained, capital demonstrably mobile), german_diplomatic_establishment slightly higher (prestige and agenda access offset by compliance obligations), american_transatlantic_lenders low (interest inflows at every hop exceed hedged exposure). Victim declarations drive high d: french_reparations_claimants near full target (booked claims stripped, exit constrained by alliance dependence), belgian_reconstruction_claimants nearest full target (trapped, no independent reopening path, existential dependence on the settlement). One override is declared: german_taxpaying_classes are declared victims and trapped, which the derivation would push toward full target (~0.9), but the same arrangement lowered their external burden relative to the punitive alternative and financed recovery employment after 1924 - the override sets d to 0.70 to correct that overshoot for the indirect subsidy channel. The administrative machinery sits mid-range (salary and purpose flow from the settlement; enforcement costs flow back), and the analytical observer is symmetric by construction. Suppression here is structural - foreign financial control, loan conditionality, occupation threat - not internalized; no interpersonal suppression ambiguity arises.
 *
 * MANDATROPHY ANALYSIS:
 *   Misclassification risks run in both directions. Reading this as pure coordination erases the identifiable losers - Belgian reconstruction, French pension claims, French taxpayers - who bore real, uncompensated costs through the very schedules that kept Germany solvent. Reading it as pure extraction erases why the maximalist alternative failed on its own terms: the Ruhr occupation yielded less net value than it consumed and destroyed the payer's capacity to pay anyone. The tangled_rope classification holds both truths in one structure, which is exactly why it prevents mislabeling here. On obsolescence: the founding problem - size a claim a broken economy could verifiably service - was declared dead by the reading's own triumph, since Lausanne 1932 abolished the object rather than resizing it once more; yet the reading's rhetoric survived intact as nationalist ammunition, and the parties never agreed the problem had been solved, only that the ledger was closed. Hence founding_problem_status is authored contested rather than dead, and the status-times-disappearance pairing (contested x world_rearranges) is the honest signal: the arrangements genuinely depended on this reading, while its own genealogy was never settled by consensus.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_structure,
    'This constraint instantiates the limited_responsibility_reading of the versailles_reparations_clauses kernel; how would the punitive_liability_reading and repudiation_reading siblings change the structural picture?',
    'Generate and compare the sibling stories: the punitive reading converts German elites from subsidized negotiators into full targets and drives the headline claim toward quasi-unlimited liability; the repudiation reading deletes the payment obligation entirely, leaving no coordination function and no machinery.',
    'The same treaty text supports a moderate-extraction coordination regime, a high-extraction liability regime, or no regime at all depending on which reading is seated; cross-reading comparison, not this file alone, is the measurement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_structure, conceptual, 'Committer structure: one kernel, three readings, structurally distinct constraints.').

omega_variable(
    article231_semantic_status,
    'Is Article 231''s operative meaning settled as a liability-scheduling formality, or does its wording carry moral-verdict force that limits how far capacity bounds may legitimately revise the claim?',
    'Drafting-record analysis (Commission on Responsibilities minutes, the clause''s insertion history and the framers'' documented intent that practical consequences not flow from the moral language) combined with subsequent legal reception under the Hague agreements and arbitration practice.',
    'If the clause carries binding moral-verdict force, capacity bounds weaken creditor entitlement and the reading leans toward taking value from Allied claimants; if purely formal, the bounds are interpretive clarification and the reading''s coordination side strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article231_semantic_status, conceptual, 'Where the readings disagree: the semantic status of the guilt clause itself.').

omega_variable(
    capacity_measurement_strategic_manipulation,
    'Were the German capacity figures anchoring the schedules objective measurements, or strategically manufactured demonstrations of insolvency (deliberate monetary collapse in 1923, staged budget presentations, German-supplied data certified by committees reliant on it)?',
    'Archival comparison of Reichsbank internal deliberations against published capacity submissions, plus independent econometric reconstruction of maximum sustainable transfer that bypasses German-reported figures.',
    'If capacity was fabricated, the coordination surface thins toward an elite shield and effective extraction concentrates on Allied creditors plus German savers; if genuine, the coordination component strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_measurement_strategic_manipulation, empirical, 'Objectivity of the measured capacity bound.').

omega_variable(
    burden_distribution_within_germany,
    'Did capacity-bounded reparations protect the German population broadly, or protect elite wealth while shifting costs onto wage earners and small savers (the 1918-1924 inflation transfer, then deflationary adjustment 1929-1932)?',
    'Distributional wealth-transfer studies of the German inflation era and incidence analysis of Young Plan taxation and Bruening-era austerity.',
    'Determines whether german_taxpaying_classes are net beneficiaries (lower d) or net targets (higher d), and therefore whether the reading''s extraction is narrowly captured by elites or broadly diffused.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(burden_distribution_within_germany, empirical, 'Internal distribution of the arrangement''s costs and protections.').

omega_variable(
    transfer_limit_natural_or_constructed,
    'Does the capacity bound approximate a hard economic limit on transferable surplus that no enforcement could lift, or is it a politically chosen bound that sustained coercion could have raised?',
    'Counterfactual economic-history estimation of maximum transferable German surplus under sustained coercion, benchmarked against the Ruhr occupation''s measured negative yield.',
    'A hard-limit finding pushes the constraint toward natural-law-like status (the bound would persist regardless of defenders); a soft finding confirms a constructed, enforced choice, keeping the tangled-rope profile or pushing toward the extractive side.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transfer_limit_natural_or_constructed, empirical, 'Whether the viability bound is a discovered limit or an authored one.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(versailles_reparations_clauses__limited_responsibility_reading, 1919, 1932).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vers_tr_t1919, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1919, 0.1).
narrative_ontology:measurement_basis(vers_tr_t1919, observed).
narrative_ontology:measurement(vers_tr_t1921, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1921, 0.13).
narrative_ontology:measurement_basis(vers_tr_t1921, observed).
narrative_ontology:measurement(vers_tr_t1923, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1923, 0.18).
narrative_ontology:measurement_basis(vers_tr_t1923, observed).
narrative_ontology:measurement(vers_tr_t1924, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1924, 0.24).
narrative_ontology:measurement_basis(vers_tr_t1924, observed).
narrative_ontology:measurement(vers_tr_t1926, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1926, 0.27).
narrative_ontology:measurement_basis(vers_tr_t1926, observed).
narrative_ontology:measurement(vers_tr_t1929, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1929, 0.31).
narrative_ontology:measurement_basis(vers_tr_t1929, observed).
narrative_ontology:measurement(vers_tr_t1932, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1932, 0.35).
narrative_ontology:measurement_basis(vers_tr_t1932, observed).

% Extraction over time
narrative_ontology:measurement(vers_be_t1919, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1919, 0.22).
narrative_ontology:measurement_basis(vers_be_t1919, observed).
narrative_ontology:measurement(vers_be_t1921, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1921, 0.28).
narrative_ontology:measurement_basis(vers_be_t1921, observed).
narrative_ontology:measurement(vers_be_t1923, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1923, 0.34).
narrative_ontology:measurement_basis(vers_be_t1923, observed).
narrative_ontology:measurement(vers_be_t1924, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1924, 0.46).
narrative_ontology:measurement_basis(vers_be_t1924, observed).
narrative_ontology:measurement(vers_be_t1926, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1926, 0.49).
narrative_ontology:measurement_basis(vers_be_t1926, observed).
narrative_ontology:measurement(vers_be_t1929, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1929, 0.51).
narrative_ontology:measurement_basis(vers_be_t1929, observed).
narrative_ontology:measurement(vers_be_t1932, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1932, 0.52).
narrative_ontology:measurement_basis(vers_be_t1932, observed).

% Suppression requirement over time
narrative_ontology:measurement(vers_su_t1919, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1919, 0.35).
narrative_ontology:measurement_basis(vers_su_t1919, observed).
narrative_ontology:measurement(vers_su_t1921, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1921, 0.42).
narrative_ontology:measurement_basis(vers_su_t1921, observed).
narrative_ontology:measurement(vers_su_t1923, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1923, 0.55).
narrative_ontology:measurement_basis(vers_su_t1923, observed).
narrative_ontology:measurement(vers_su_t1924, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1924, 0.68).
narrative_ontology:measurement_basis(vers_su_t1924, observed).
narrative_ontology:measurement(vers_su_t1926, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1926, 0.6).
narrative_ontology:measurement_basis(vers_su_t1926, observed).
narrative_ontology:measurement(vers_su_t1929, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1929, 0.64).
narrative_ontology:measurement_basis(vers_su_t1929, observed).
narrative_ontology:measurement(vers_su_t1932, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1932, 0.5).
narrative_ontology:measurement_basis(vers_su_t1932, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(versailles_reparations_clauses__limited_responsibility_reading, resource_allocation).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, versailles_reparations_clauses__punitive_liability_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, versailles_reparations_clauses__repudiation_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Versailles reparations' conflates three structurally distinct claims over one kernel text (Articles 231-232 plus the payment annexes). Epsilon differs across the family: the punitive liability reading authors high extraction (quasi-unlimited moralized claims), this limited responsibility reading authors moderate extraction (core liability conceded, maximalism bounded), and the repudiation reading deletes the obligation structure entirely. The punitive reading is upstream (the enacted 1919 frame that this reading revised); this reading is downstream of it and structurally upstream of the repudiation reading, whose rhetoric it armed. Each file stands alone with its own stable epsilon; the family is linked through affects_constraints edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(versailles_reparations_clauses__limited_responsibility_reading, powerless, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
