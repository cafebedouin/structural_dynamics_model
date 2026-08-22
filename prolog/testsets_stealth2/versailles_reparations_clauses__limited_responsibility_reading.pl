% ============================================================================
% CONSTRAINT STORY: versailles_reparations_clauses__limited_responsibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: versailles_reparations_clauses__limited_responsibility_reading
 *   human_readable: Versailles Reparations — Capacity-Bounded Settlement Reading
 *   domain: international_relations/legal_history/political_economy
 *
 * SUMMARY:
 *   Between the Versailles signature (June 1919, t=0) and the Lausanne
 *   extinction (July 1932, t=13; one time unit equals one year), the war-cost
 *   obligation fixed by Articles 231-247 was progressively reinterpreted and
 *   re-scheduled around a capacity principle: Germany owed real money, but
 *   only what its economy could transfer without collapse, and the war-guilt
 *   clause was to be read as the legal hinge of liability rather than a moral
 *   verdict. This story assesses that capacity-bounded arrangement as the
 *   limited_responsibility_reading sees it — the standing arrangement under
 *   contest is the reparations regime itself, not the settlement this reading
 *   would have preferred. The reading won institutionally (London Schedule
 *   1921, Dawes 1924, Young 1929) and then watched its own machinery drain
 *   away (Hoover Moratorium 1931, Lausanne 1932). KEY AGENTS (by structural
 *   relationship): - reparation_commission_interallied: administering seat
 *   (institutional/constrained) — converts the treaty clauses into annual
 *   schedules and apportions receipts - german_fiscal_authorities: primary
 *   protected party (organized/constrained) — gains leverage from every
 *   viability finding - german_industrialists: protected party
 *   (powerful/mobile) — paid substantially in kind, financed by stabilization
 *   loans - american_transatlantic_lenders: circuit financier
 *   (institutional/arbitrage) — collects on the loan round trip while
 *   formally outside the settlement - allied_creditor_states: bearing seat
 *   with residual collection (institutional/constrained) — accepts
 *   written-down claims as the price of collecting anything -
 *   french_belgian_reconstruction_claimants: diffuse bearing seat
 *   (moderate/trapped) — recoveries shrink with each revision -
 *   german_taxpaying_households: ultimate bearing seat (powerless/trapped) —
 *   funds the annuities through taxes and wage restraint -
 *   dawes_expert_mediators: analytical seat (institutional/analytical) — sees
 *   both the solvency arithmetic and the politics around it -
 *   german_nationalist_movements: excluded seat (organized/identity_locked) —
 *   rejects the obligation entire, pressures from outside the room
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(versailles_reparations_clauses__limited_responsibility_reading, 0.48).
domain_priors:suppression_score(versailles_reparations_clauses__limited_responsibility_reading, 0.44).
domain_priors:theater_ratio(versailles_reparations_clauses__limited_responsibility_reading, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(versailles_reparations_clauses__limited_responsibility_reading, tangled_rope).
narrative_ontology:human_readable(versailles_reparations_clauses__limited_responsibility_reading, "Versailles Reparations — Capacity-Bounded Settlement Reading").
narrative_ontology:topic_domain(versailles_reparations_clauses__limited_responsibility_reading, "international_relations/legal_history/political_economy").

domain_priors:requires_active_enforcement(versailles_reparations_clauses__limited_responsibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(versailles_reparations_clauses__limited_responsibility_reading, '758e76f0-7d57-49d2-b143-b49f7154611b').
narrative_ontology:cs_kernel_codification('758e76f0-7d57-49d2-b143-b49f7154611b', fixed_text).
narrative_ontology:cs_authority_grounding('758e76f0-7d57-49d2-b143-b49f7154611b', lineage).
narrative_ontology:cs_interpretation_layer_present('758e76f0-7d57-49d2-b143-b49f7154611b').
narrative_ontology:cs_reading_relation('758e76f0-7d57-49d2-b143-b49f7154611b', versailles_reparations_clauses__punitive_liability_reading, forecloses).
narrative_ontology:cs_reading_relation('758e76f0-7d57-49d2-b143-b49f7154611b', versailles_reparations_clauses__repudiation_reading, forecloses).
narrative_ontology:cs_axiom('758e76f0-7d57-49d2-b143-b49f7154611b', foundational, reparation_obligation_bounded_by_capacity).
narrative_ontology:cs_axiom_status(reparation_obligation_bounded_by_capacity, holdable).
narrative_ontology:cs_axiom_grounding('758e76f0-7d57-49d2-b143-b49f7154611b', reparation_obligation_bounded_by_capacity, instrumental).
narrative_ontology:cs_axiom('758e76f0-7d57-49d2-b143-b49f7154611b', foundational, article_231_carries_no_moral_stigma).
narrative_ontology:cs_axiom_status(article_231_carries_no_moral_stigma, holdable).
narrative_ontology:cs_axiom_grounding('758e76f0-7d57-49d2-b143-b49f7154611b', article_231_carries_no_moral_stigma, conventional).
narrative_ontology:cs_reference_frame('758e76f0-7d57-49d2-b143-b49f7154611b', viability_bounded_compensation_regime).
narrative_ontology:cs_drift_state('758e76f0-7d57-49d2-b143-b49f7154611b', lausanne_extinction_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('758e76f0-7d57-49d2-b143-b49f7154611b', '').
narrative_ontology:cs_kernel_id(versailles_reparations_clauses__limited_responsibility_reading, versailles_reparations_clauses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__limited_responsibility_reading, german_fiscal_authorities).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__limited_responsibility_reading, german_industrialists).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__limited_responsibility_reading, american_transatlantic_lenders).
narrative_ontology:constraint_victim(versailles_reparations_clauses__limited_responsibility_reading, allied_creditor_states).
narrative_ontology:constraint_victim(versailles_reparations_clauses__limited_responsibility_reading, french_belgian_reconstruction_claimants).
narrative_ontology:constraint_victim(versailles_reparations_clauses__limited_responsibility_reading, german_taxpaying_households).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__limited_responsibility_reading, reparation_commission_interallied).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__limited_responsibility_reading, allied_creditor_states).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__limited_responsibility_reading, german_taxpaying_households).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__limited_responsibility_reading, ability_to_pay_doctrine).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__limited_responsibility_reading, article_231_legal_formality_reading).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__limited_responsibility_reading, economic_viability_transfer_limit).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A multi-government body seated in Paris that translates the treaty's payment clauses into annual schedules: it assesses what the German budget and trade balance can carry, hears moratorium petitions, fixes annuities and in-kind deliveries, and apportions each year's receipts among the creditor governments that staff it. Its members vote under instructions from home capitals, so its technical findings move with the diplomatic weather.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, reparation_commission_interallied, agenda_setter,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(versailles_reparations_clauses__limited_responsibility_reading, reparation_commission_interallied, beneficiary).

% The Reich ministries and chancellery teams that negotiate every schedule: they compile capacity statistics, plead shortfalls, trade political concessions for payment relief, and administer the domestic taxes that fund whatever is agreed. Each downward revision enlarges their room to maneuver at home; walking out of the treaty system entirely would invite occupation and financial strangulation, so they bargain inside it.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, german_fiscal_authorities, beneficiary,
    organized, biographical, constrained, national).

% Heavy industry in the Ruhr and Upper Silesia delivers a large share of payments in coal, steel, timber, dyestuffs, and ships rather than cash, which routes orders back to their own works; the stabilization loans of the mid-1920s finance plant modernization alongside the payment stream. Their associations lobby the commission over valuations and delivery quotas and organize work stoppages when enforcement tightens.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, german_industrialists, beneficiary,
    powerful, biographical, mobile, national).

% Private banks and investors in New York, with a government that has formally stepped outside the settlement, float the stabilization and settlement loans that refill German coffers between annuity payments, collecting interest and fees on the round trip. Their continued lending is the quiet condition on which the whole payment circuit keeps turning.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, american_transatlantic_lenders, beneficiary,
    institutional, biographical, arbitrage, global).

% The French, Belgian, British, Italian, and other governments holding assessed war-damage claims. Each schedule revision writes down what they recover relative to their initial assessments, and each moratorium postpones them; yet the same machinery is the only route by which anything is collected at all, since seizing more than the German economy can carry yields a default worth zero. They police the schedules, threaten sanctions over shortfall, and divide between those pressing for more and those preferring a solvent lesser sum.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, allied_creditor_states, payer,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(versailles_reparations_clauses__limited_responsibility_reading, allied_creditor_states, beneficiary).

% War-devastated departments, municipal rebuilding authorities, and pension funds in northern France and Belgium whose compensation claims are the raw material the schedules apportion. They have no separate collection channel: when an annuity is cut or postponed, their recoveries shrink with it, and they petition their own governments rather than the commission.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, french_belgian_reconstruction_claimants, payer,
    moderate, biographical, trapped, regional).

% Urban and rural households carrying the taxes, wage restraint, and currency wreckage through which the annuities are actually raised. The viability principle caps how much can be pulled from them in a given year, which spares them the open-ended claims of the early schedules, but within that cap they are the ones who pay.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, german_taxpaying_households, payer,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(versailles_reparations_clauses__limited_responsibility_reading, german_taxpaying_households, beneficiary).

% The American-led committees of financiers and economists, and later the Agent-General for Reparations Payments and the Basel institution that succeeds him, convert the capacity principle into workable schedules, monitor German fiscal administration, and report annually on both the solvency arithmetic and the political maneuvering around it. They sit inside the machine without collecting from it.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, dawes_expert_mediators, observer,
    institutional, biographical, analytical, continental).

% Parties and veterans' leagues that reject any payment as national dishonor and campaign against every government that signs a schedule. They hold no seat in the commission or in the conference diplomacy that produces the schedules; their pressure operates from outside the room, eroding the domestic position of whoever negotiates.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, german_nationalist_movements, excluded,
    organized, generational, identity_locked, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(versailles_reparations_clauses__limited_responsibility_reading, allied_creditor_states).
narrative_ontology:fixing_cost_class(versailles_reparations_clauses__limited_responsibility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Scales an international damage-compensation settlement to what the debtor economy can actually transfer, giving creditor governments a predictable if reduced revenue stream and the debtor a survivable obligation — solving the collective problem that unlimited claims produce a default worth zero while zero claims leave assessed damage permanently uncompensated.
% TRANSFER_FUNCTION: Moves annual payments — gold-mark annuities, coal, timber, dyestuffs, shipping, and later service charges — from German taxpayers and industry to Allied state treasuries, which apportion them to reconstruction and pension claims; American credit flows the opposite way into Germany to keep the circuit liquid.
% ABSENT_VOICES: German nationalist opinion would repudiate the entire obligation and held no seat in the conference diplomacy; French and Belgian local claimants were represented only indirectly through their governments; Soviet Russia, excluded from the settlement altogether, would contest the whole architecture. Dissent entered chiefly as street pressure and electoral punishment after schedules were signed, not as votes inside the room.
% DISAPPEARANCE_RATIONALE: Overnight removal of the capacity bound reopens the very choice the regime existed to manage: either the early full-assessment schedules revive, bringing default, renewed occupation, and a collapsed German currency, or the obligation lapses entirely, stranding creditor budgets built on expected receipts and unwinding the inter-allied debt settlements chained to them. Every treasury in the chain rearranges around whichever branch prevails.
% FOUNDING_PROBLEM: After 1918: how to make the states damaged by the war whole out of the defeated economy without destroying that economy's ability to pay, and without triggering the enforcement spiral of occupation and default that all sides could see coming.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: J. M. Keynes's 1919 attack on the impossibly sized claim predates German negotiating leverage; the 1924 Dawes Committee's own findings state the insolvency arithmetic; and later economic historians reconstructing German national accounts conclude the early schedules exceeded any plausible transfer capacity. No corroborating source depends on German payments for its position.
narrative_ontology:disappearance_verdict(versailles_reparations_clauses__limited_responsibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(versailles_reparations_clauses__limited_responsibility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(versailles_reparations_clauses__limited_responsibility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(versailles_reparations_clauses__limited_responsibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(versailles_reparations_clauses__limited_responsibility_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

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
 *   Epsilon 0.48 is the interval-characteristic value of the bounded regime: the early punitive-weighted years (paper liability of 132 billion gold marks, Ruhr seizure) run 0.70-0.78, the mature Dawes-Young years run 0.44-0.56, and the terminal extinction year runs 0.14. Suppression 0.44 is authored as a raw structural property, unscaled: military occupation, fiscal oversight, and sanction threat were real coercive instruments, but lighter than the machinery the punitive reading would have required to hold an unviable schedule. Theater 0.36 mixes genuine actuarial work (the expert committees' capacity studies were real analysis) with ritual recitation of Article 231 and face-saving conference choreography; it crosses 0.5 only in the moratorium years, when the machinery outlived the collections. Accessibility_collapse 0.52: once the viability principle is adopted, unlimited-schedule alternatives close off for the parties inside the regime, but default and repudiation remain imaginable exits, so collapse is partial. Resistance 0.58: French obstruction of every reduction, German nationalist agitation against every signature, and American congressional detachment met the regime continuously. All three tracked metric series are authored on one shared grid (t = 0, 2, 4, 5, 7, 9, 11, 13) so the engine samples a complete row at every point; the trajectory shows the reading rising from marginal to dominant by t=5 while the regime's functional content drains after t=9 — theater rising as extraction falls is the signature of a settlement outliving its collections. Receipt surface: collections demonstrably accrued to the allied_creditor_states treasuries, which apportioned them onward; fixing the arrangement in either direction — reviving full-assessment claims or cancelling outright — was prohibitively costly relative to benefit for anyone positioned to attempt it, as the Ruhr experiment priced one branch and the Depression priced the other.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From german_fiscal_authorities the capacity bound is protection won by negotiation — each viability finding enlarges domestic room. From allied_creditor_states the same bound is confiscation of an assessed entitlement, tolerated only because the alternative collection path yields zero. From german_taxpaying_households it is a ceiling on what is taken, not an exemption from taking. The dawes_expert_mediators see both faces at once. One treaty text, four experiences; the engine derives this divergence from the role and exit asymmetries declared on the stakeholder surface, not from the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (german_fiscal_authorities, german_industrialists, american_transatlantic_lenders) derive directionality near the subsidized end: the bound shields their balance sheets, channels in-kind orders back to their works, and generates lending fees. Declared victims (allied_creditor_states, french_belgian_reconstruction_claimants, german_taxpaying_households) derive directionality near the target end: each revision writes down recoveries they hold no alternative route to collect. The two dual-positioned seats are the ones a naive derivation would flatten: allied_creditor_states (payer with secondary beneficiary) both lose claims and depend on the machinery for any recovery at all, and german_taxpaying_households (payer with secondary beneficiary) both fund the annuities and are spared the open-ended early schedules — both sit mid-range, which is why their secondary roles are declared rather than left implicit. No directionality overrides are used: role plus exit separation carries the differentiation.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two mislabels. Reading the regime as pure Allied plunder imports the punitive sibling's story and erases the real solvency coordination the bound performed — without it, collections were zero and the enforcement spiral everyone foresaw arrived early. Reading it as benign bookkeeping erases that every downward revision wrote down identifiable claimants' recoveries while shielding identifiable German and American balance sheets through the same schedules. Mandate trajectory: the founding problem (managing an insolvent settlement) was arguably stabilized by 1929, after which the machinery persisted on momentum — theater_ratio crossing 0.5 at t=11 marks the performative tail — until Lausanne closed it formally. Because the regime terminated rather than persisting as a zombie, the corpus records resolution-by-extinction rather than a living vestige; the R5 mismatch consumer should find status=contested crossed with verdict=world_rearranges consistent with a settlement whose problem faded before its form did.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_sibling_punitive_delta,
    'This constraint instantiates the limited_responsibility_reading of the versailles_reparations_clauses kernel; what structural differences follow if the punitive_liability_reading governs the same treaty text instead?',
    'Comparative classification against the sibling story versailles_reparations_clauses__punitive_liability_reading: identical Articles 231-247 referent, opposing axiom on the clause''s normative status.',
    'Under the punitive reading, epsilon rises well above the authored 0.48, the bearing set expands toward the German economy as a whole, enforcement intensifies, and the computed type shifts toward pure extraction; the disagreement is located in the normative status of Article 231 and in whether ''capacity'' is a constitutive bound or a negotiating posture.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_sibling_punitive_delta, conceptual, 'Committer structure: structural delta versus the punitive sibling reading of the same kernel.').

omega_variable(
    kernel_sibling_repudiation_delta,
    'What structural differences follow if the repudiation_reading governs the kernel instead — the position that duress vitiates the obligation?',
    'Comparative classification against the sibling story versailles_reparations_clauses__repudiation_reading.',
    'Under the repudiation reading the obligation dissolves, the protected-party seats empty out, and the arrangement reduces to a dead letter maintained by rhetoric rather than by collection machinery; the classification collapses toward inertial-theatrical residue rather than enforced transfer.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_sibling_repudiation_delta, conceptual, 'Committer structure: structural delta versus the repudiation sibling reading of the same kernel.').

omega_variable(
    capacity_assessment_objectivity,
    'Was ''German economic capacity'' an objective economic quantity that the schedules tracked, or a politically constructed figure produced by bargaining between pleading debtors and skeptical creditors?',
    'Compare contemporaneous Reparation Commission and Dawes Committee assessments with retrospective macroeconomic reconstructions of German national accounts and the transfer-problem literature.',
    'Systematic understatement by German negotiators raises the true extraction floor of the bounded regime above the authored value; systematic overstatement by creditors lowers it and strengthens the punitive sibling''s claim that capacity talk was evasion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_assessment_objectivity, empirical, 'Whether the viability bound measured a real economic limit or encoded bargaining power.').

omega_variable(
    transfer_circuit_sustainability,
    'Was the 1924-1929 circuit of American loans into Germany, reparations out of Germany, and Allied debt service back to the United States a solvent coordination mechanism or circular financing that deferred insolvency?',
    'Balance-of-payments reconstruction of the circuit; counterfactual solvency of German annuity payments absent continuing new American lending.',
    'If the circuit was circular, the coordination function is weaker than authored, the theater_ratio understates performative content in the stable middle years, and the eventual collapse reads as built-in rather than exogenous.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transfer_circuit_sustainability, empirical, 'Whether the stabilized settlement rested on real transfer capacity or on recycling.').

omega_variable(
    enforcement_endogeneity_of_collapse,
    'Did the regime terminate at Lausanne because its founding problem was solved, or because the Depression destroyed the enforcement capacity and fiscal space that kept it running?',
    'Counterfactual analysis: absent the 1929 crash and the accompanying fiscal crisis, were Young Plan annuities payable from German national income without renewed destabilization?',
    'Resolves founding_problem_status between dead and contested, and determines whether the 1932 extinction records successful completion of a transitional settlement or abandonment of a still-live obligation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_endogeneity_of_collapse, empirical, 'Whether termination reflected problem resolution or enforcement collapse.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(versailles_reparations_clauses__limited_responsibility_reading, 0, 13).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vrc_limited_resp_tr_t0, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(vrc_limited_resp_tr_t2, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 2, 0.22).
narrative_ontology:measurement(vrc_limited_resp_tr_t4, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 4, 0.25).
narrative_ontology:measurement(vrc_limited_resp_tr_t5, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 5, 0.34).
narrative_ontology:measurement(vrc_limited_resp_tr_t7, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 7, 0.36).
narrative_ontology:measurement(vrc_limited_resp_tr_t9, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 9, 0.4).
narrative_ontology:measurement(vrc_limited_resp_tr_t11, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 11, 0.55).
narrative_ontology:measurement(vrc_limited_resp_tr_t13, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 13, 0.68).

% Extraction over time
narrative_ontology:measurement(vrc_limited_resp_be_t0, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(vrc_limited_resp_be_t2, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 2, 0.74).
narrative_ontology:measurement(vrc_limited_resp_be_t4, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 4, 0.78).
narrative_ontology:measurement(vrc_limited_resp_be_t5, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 5, 0.56).
narrative_ontology:measurement(vrc_limited_resp_be_t7, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 7, 0.5).
narrative_ontology:measurement(vrc_limited_resp_be_t9, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 9, 0.44).
narrative_ontology:measurement(vrc_limited_resp_be_t11, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 11, 0.28).
narrative_ontology:measurement(vrc_limited_resp_be_t13, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 13, 0.14).

% Suppression requirement over time
narrative_ontology:measurement(vrc_limited_resp_su_t0, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(vrc_limited_resp_su_t2, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 2, 0.58).
narrative_ontology:measurement(vrc_limited_resp_su_t4, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 4, 0.76).
narrative_ontology:measurement(vrc_limited_resp_su_t5, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(vrc_limited_resp_su_t7, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 7, 0.46).
narrative_ontology:measurement(vrc_limited_resp_su_t9, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 9, 0.4).
narrative_ontology:measurement(vrc_limited_resp_su_t11, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 11, 0.22).
narrative_ontology:measurement(vrc_limited_resp_su_t13, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 13, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(versailles_reparations_clauses__limited_responsibility_reading, resource_allocation).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, versailles_reparations_clauses__punitive_liability_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, versailles_reparations_clauses__repudiation_reading).

% DUAL FORMULATION NOTE:
% 'Versailles reparations' is a colloquial label covering at least three structurally distinct claims: what the clause morally asserts (punitive_liability_reading), what it legally obligates at viable scale (this reading), and whether it obligates at all (repudiation_reading). The readings share one text but diverge on epsilon, bearing sets, and failure modes, so they are modeled as a linked constraint family rather than one story with a measurement parameter. Upstream/downstream structure: the punitive reading supplies the t0 textual baseline that the limited reading revises downward; the limited reading's successive concessions supplied the legitimacy erosion on which the repudiation reading fed as radical flank.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
