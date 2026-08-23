% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__hegemonic_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rbio_practice_norm_complex__hegemonic_extraction_reading, []).

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
 *   constraint_id: rbio_practice_norm_complex__hegemonic_extraction_reading
 *   human_readable: RBIO Practice Norm Complex as Frozen Hegemonic Project (Hegemonic Extraction Reading)
 *   domain: international relations/international law/political economy
 *
 * SUMMARY:
 *   This story instantiates the hegemonic-extraction reading of the
 *   rules-based international order: an arrangement presented as universal,
 *   consent-based law that is in fact a settlement frozen at its founding —
 *   formally revisable on paper, practically un-amendable because every
 *   revision touching great-power prerogatives requires the concurrence of
 *   the very actors whose prerogatives are at issue, and enforced selectively
 *   along the interest lines of its architects. On this reading the
 *   selectivity is not administrative noise but the tell: it reveals whose
 *   order it is. The same architecture that genuinely coordinates maritime
 *   commerce, diplomatic recognition, and crisis communication also transmits
 *   structural adjustment — liquidity against creditor-drafted policy —
 *   moving resources and policy autonomy from the debtor South to Northern
 *   capital. KEY AGENTS (by structural relationship): -
 *   us_european_capital_interests: Primary beneficiary (powerful/arbitrage) —
 *   collects debt service, market openings, compliant policy environments -
 *   p5_permanent_members: Agenda setter (institutional/arbitrage) —
 *   administers enforcement, holds the amendment veto -
 *   international_financial_institutions: Administrator-beneficiary
 *   (institutional/identity_locked) — drafts and polices conditionality -
 *   global_south_debtor_states: Primary target (organized/constrained) —
 *   bears conditionality and selective enforcement -
 *   structural_adjustment_populations: Deepest target (powerless/trapped) —
 *   bears austerity with no negotiating seat - small_open_economies: Mixed
 *   seat (moderate/constrained) — consumes coordination goods, pays selective
 *   costs - emerging_alternative_blocs: Excluded challenger (powerful/mobile)
 *   — locked out of reform, building parallel structures -
 *   twail_dependency_scholars: Analytical observer — supplies the structural
 *   account
 *
 * KEY AGENTS:
 *   - us_european_capital_interests: primary beneficiary, powerful/arbitrage, collects the transfer flows
 *   - p5_permanent_members: agenda setter, institutional/arbitrage, holds veto over amendment and enforcement agenda
 *   - international_financial_institutions: administrator-beneficiary, institutional/identity_locked, operates conditionality
 *   - global_south_debtor_states: primary target, organized/constrained, G77-coordinated but veto-blocked
 *   - structural_adjustment_populations: deepest target, powerless/trapped, bears domestic incidence without a seat
 *   - small_open_economies: mixed beneficiary-payer, moderate/constrained, near-symmetric net position
 *   - emerging_alternative_blocs: excluded challenger, powerful/mobile, exit via parallel institution-building
 *   - twail_dependency_scholars: analytical observer, documents the enforcement-interest correlation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.76).
domain_priors:suppression_score(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.68).
domain_priors:theater_ratio(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, extractiveness, 0.76).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__hegemonic_extraction_reading, tangled_rope).
narrative_ontology:human_readable(rbio_practice_norm_complex__hegemonic_extraction_reading, "RBIO Practice Norm Complex as Frozen Hegemonic Project (Hegemonic Extraction Reading)").
narrative_ontology:topic_domain(rbio_practice_norm_complex__hegemonic_extraction_reading, "international relations/international law/political economy").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__hegemonic_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__hegemonic_extraction_reading, 'd9f2c39e-e1bf-403e-85bb-577ded3ff05b').
narrative_ontology:cs_kernel_codification('d9f2c39e-e1bf-403e-85bb-577ded3ff05b', fixed_text).
narrative_ontology:cs_authority_grounding('d9f2c39e-e1bf-403e-85bb-577ded3ff05b', extraction).
narrative_ontology:cs_interpretation_layer_present('d9f2c39e-e1bf-403e-85bb-577ded3ff05b').
narrative_ontology:cs_reading_relation('d9f2c39e-e1bf-403e-85bb-577ded3ff05b', rbio_practice_norm_complex__liberal_institutional_reading, forecloses).
narrative_ontology:cs_reading_relation('d9f2c39e-e1bf-403e-85bb-577ded3ff05b', rbio_practice_norm_complex__sovereignty_maximalist_reading, coexists_with).
narrative_ontology:cs_axiom('d9f2c39e-e1bf-403e-85bb-577ded3ff05b', foundational, selective_enforcement_reveals_extractive_intent).
narrative_ontology:cs_axiom_status(selective_enforcement_reveals_extractive_intent, holdable).
narrative_ontology:cs_axiom_grounding('d9f2c39e-e1bf-403e-85bb-577ded3ff05b', selective_enforcement_reveals_extractive_intent, empirically_contingent).
narrative_ontology:cs_axiom('d9f2c39e-e1bf-403e-85bb-577ded3ff05b', foundational, conditionality_under_distress_is_coerced_consent).
narrative_ontology:cs_axiom_status(conditionality_under_distress_is_coerced_consent, holdable).
narrative_ontology:cs_axiom_grounding('d9f2c39e-e1bf-403e-85bb-577ded3ff05b', conditionality_under_distress_is_coerced_consent, empirically_contingent).
narrative_ontology:cs_reference_frame('d9f2c39e-e1bf-403e-85bb-577ded3ff05b', institutionalized_hegemonic_settlement).
narrative_ontology:cs_drift_state('d9f2c39e-e1bf-403e-85bb-577ded3ff05b', contemporary_multipolar_contestation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d9f2c39e-e1bf-403e-85bb-577ded3ff05b', '').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__hegemonic_extraction_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__hegemonic_extraction_reading, us_european_capital_interests).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__hegemonic_extraction_reading, p5_permanent_members).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__hegemonic_extraction_reading, global_south_debtor_states).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__hegemonic_extraction_reading, structural_adjustment_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__hegemonic_extraction_reading, international_financial_institutions).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__hegemonic_extraction_reading, small_open_economies).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__hegemonic_extraction_reading, small_open_economies).
narrative_ontology:constraint_vindicates(rbio_practice_norm_complex__hegemonic_extraction_reading, washington_consensus_doctrine).
narrative_ontology:constraint_vindicates(rbio_practice_norm_complex__hegemonic_extraction_reading, formal_sovereign_equality_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold creditor claims, equity positions, and market-access interests across the debtor world. Receive debt-service flows, concessionary asset transfers, and policy environments shaped by conditionality. Capital is mobile across jurisdictions and can reprice or relocate exposure when any single regime becomes unfavorable; the order's rules bind others far more than they bind portfolio allocation.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, us_european_capital_interests, beneficiary,
    powerful, generational, arbitrage, global).

% Hold veto power over Security Council action and Charter amendment, administer sanctions authorization, and decide which norm violations receive enforcement attention. They wrote the founding settlement and remain its gatekeepers: no revision touching their prerogatives can pass without their own consent. Selective application of the rules they police is available to them as a standing option.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, p5_permanent_members, agenda_setter,
    institutional, generational, arbitrage, global).

% Draft, negotiate, and monitor structural adjustment programs; disburse liquidity against policy benchmarks; and certify debtor states' creditworthiness to private markets. Their staffing, intellectual tradition, and shareholder governance are fused with the policy model they administer, so the institution experiences the model as its own identity rather than as a chosen instrument.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, international_financial_institutions, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__hegemonic_extraction_reading, international_financial_institutions, beneficiary).

% Face balance-of-payments crises in which the price of liquidity is acceptance of creditor-drafted policy programs. Collectively they coordinate through the G77 and Non-Aligned Movement and have repeatedly won General Assembly majorities for reform, but assembly votes cannot amend the Charter or the IFI quota structure. Default, autarky, or full alignment with alternative lenders each carry severe and demonstrably enforced costs.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, global_south_debtor_states, payer,
    organized, generational, constrained, continental).

% Bear the domestic incidence of adjustment: subsidy removal, public-sector contraction, user fees for health and education, and labor-market restructuring. They were absent from program negotiation, which proceeds between finance ministries and fund staff, and migration — the classic individual exit — is restricted by the very destination states that shape the order.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, structural_adjustment_populations, payer,
    powerless, biographical, trapped, continental).

% Depend heavily on the order's public goods — maritime freedom, standardized trade rules, dispute fora, recognized currency arrangements — for market access they could never secure bilaterally. They pay in compliance costs, occasional selective enforcement touching their exports, and zero voice in governance reform; their net position sits close to break-even and varies with proximity to great-power disputes.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, small_open_economies, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__hegemonic_extraction_reading, small_open_economies, payer).

% States and coalitions whose weight has grown faster than their institutional share. Their governance-reform proposals have stalled for decades against veto arithmetic and incumbent shareholder lock-in, so they increasingly build parallel structures — new development banks, swap lines, settlement systems — outside the arrangement they were unable to amend from within.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, emerging_alternative_blocs, excluded,
    powerful, generational, mobile, global).

% Third World Approaches to International Law jurists, dependency theorists, and heterodox economists who document the correlation between enforcement patterns and great-power interest, the genealogy of the founding settlement, and the distributional record of adjustment lending. They hold no enforcement power; their product is the structural account this reading systematizes.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, twail_dependency_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rbio_practice_norm_complex__hegemonic_extraction_reading, us_european_capital_interests).
narrative_ontology:fixing_cost_class(rbio_practice_norm_complex__hegemonic_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves real multilateral coordination problems once instead of per-dyad: diplomatic recognition, maritime and airspace rules, trade and customs standards, dispute-resolution fora, crisis communication channels among nuclear powers, and a common accounting frame for sovereign credit.
% TRANSFER_FUNCTION: Moves debt-service payments, policy autonomy (via loan conditionality), preferential market-access terms, resource-concession rights, and enforcement attention itself from Global South states and their populations toward Northern creditor capitals and P5-administered priorities; enforcement effort is allocated by the interest profile of the enforcing powers rather than by violation severity.
% ABSENT_VOICES: Structural adjustment populations had no seat at program negotiation, which ran between finance ministries and fund staff; most of Africa and colonial Asia were absent from the 1944-45 founding conferences where the settlement's architecture was fixed; and reform-proposing majorities in the General Assembly hold voice without vote where it matters. All three groups would object to the arrangement's current terms if seated.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, sovereign credit would reprice chaotically, maritime and trade intercourse would fall back on bilateral gunboat bargaining, dozens of payment and settlement chains would sever, and every actor from central banks to shipping insurers would scramble to rebuild coordination from scratch — while the extraction channels (conditionality, selective enforcement) would dissolve with the machinery that carries them.
% FOUNDING_PROBLEM: Built to solve the interwar breakdown: great-power war, beggar-thy-neighbor tariff wars, competitive devaluation, contagious sovereign default, and the absence of any managed forum for great-power crisis bargaining — solved, by design, under the stewardship of the victorious powers.
% FOUNDING_PROBLEM_CORROBORATION: TWAIL scholarship, G77 ministerial communiques, and UNCTAD analyses from outside the benefiting parties attest that the war-prevention problem persists in mutated form while a dependence-management function accreted around it; independent diplomatic historians corroborate that the founding settlement was designed by and for the wartime allies. The benefiting parties themselves attest only the war-prevention half and deny the extraction characterization — no corroboration for the extraction reading comes from within the beneficiary set, which is itself signal.
narrative_ontology:disappearance_verdict(rbio_practice_norm_complex__hegemonic_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(rbio_practice_norm_complex__hegemonic_extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rbio_practice_norm_complex__hegemonic_extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rbio_practice_norm_complex__hegemonic_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.76, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rbio_practice_norm_complex__hegemonic_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rbio_practice_norm_complex__hegemonic_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rbio_practice_norm_complex__hegemonic_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.76 at interval end) because the transfer channels are structural, not incidental: conditionality converts a liquidity need into a policy-transfer event, and the resulting flows (debt service, concessionary asset sales, opened sectors) run persistently northward. Suppression (0.68) is a raw structural property, deliberately unscaled: it measures the enforcement machinery itself — sanctions regimes, credit blacklisting, secondary-sanctions reach, the demonstrated punishment of default and of forum-shopping — not its amplification by scope. Theater (0.45) tracks the widening gap between performed universalism (General Assembly procedure, formal sovereign equality, reform commissions) and operative hierarchy; roughly half the visible activity of the arrangement is process that cannot alter the distribution it decorates. Accessibility_collapse (0.55) is partial: the NIEO program was tried and crushed, but BRICS-era parallel institutions show alternatives are buildable at rising cost — alternatives narrow, they do not vanish. Resistance (0.60) is sustained and organized (G77, debt jubilee campaigns, non-alignment, parallel-bloc construction) yet arithmetically futile against the veto, which is precisely why suppression concentrates on blocking exits rather than silencing voices. The temporal series run on one shared nine-point grid (every tracked metric authored at every point, 1945-2025): extractiveness climbs steeply into the 1980s debt-crisis/structural-adjustment peak, dips modestly in the mid-2000s (HIPC relief, commodity boom easing financing constraints), then re-tightens; theater and suppression follow the same broad arc, with the mid-2000s relaxation visible in all three series. The trajectory is monotone-drifting with one dip, not cyclical — the oscillation is a commodity-and-credit cycle passing through a ratchet, not intermittent reinforcement.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute sharply different types from identical structural data. From the us_european_capital_interests seat (arbitrage-grade exit, beneficiary position) the arrangement presents as a rope it helped weave: predictable rules it largely writes, costs it can reprice or relocate away from. From the structural_adjustment_populations seat (powerless, trapped, no seat in negotiation) the same structure presents as a snare: extraction with no exit and no voice. The global_south_debtor_states seat sits between — organized enough to resist, constrained enough to lose, experiencing a tangled rope whose coordination half it genuinely consumes. Small open economies compute near-symmetric. The p5 and IFI seats experience legitimate stewardship. The engine derives this divergence from power, exit, and role data; nothing in the authored claim adjudicates it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: us_european_capital_interests and p5_permanent_members sit near the beneficiary pole (low d), amplified toward capture by their arbitrage-grade exit — they are above the rules they administer. The victim declarations place global_south_debtor_states and structural_adjustment_populations near the target pole (high d); trapped exit pushes the populations seat to the full-target end, while organized-but-constrained debtor states sit slightly back from it. International_financial_institutions derive low d from their beneficiary role, but their identity_locked exit marks them as administratively fused with the arrangement rather than merely advantaged by it. Small_open_economies derive near-symmetric d from their dual role. Emerging_alternative_blocs warrant note: despite powerful ratings, exclusion places them on the target side — their exclusion is the enforcement object, mirroring the rival-payment-network pattern in platform cases. Scope amplification applies to the global-scale seats: verification of compliance across planetary scope is hard, which scales effective extraction upward for targets operating at that scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — managing great-power rivalry and interwar-style economic breakdown — is contested rather than dead: war-prevention among nuclear powers is plausibly still live, which is exactly why a clean mandatrophy verdict is unavailable and the mismatch consumer reads status=contested x verdict=world_rearranges without firing a zombie flag. The classification discipline matters here in both directions. Reading the arrangement as pure snare would erase the coordination goods the Global South demonstrably consumes (maritime rules, dispute fora, credit standardization) and license abolitionism that would hurt the victims too; reading it as pure rope would launder the transfer channels as coordination cost. Tangled rope keeps both halves structurally present and locates the remedy correctly: not dissolution but channel reform — conditionality governance, quota realignment, veto-circle accountability — aimed at the extraction riding on the coordination. The piton question is also live and worth monitoring: if enforcement selectivity ever becomes pure ritual (sanctions theater uncorrelated with interest) while the transfer continues on inertia alone, the arrangement degrades toward piton; the theater series' plateau after 2005 is the early signature to watch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint is the hegemonic_extraction_reading of the rbio_practice_norm_complex kernel — what would change structurally if a sibling reading were instantiated instead?',
    'Not resolvable by data within this story: the choice of reading is the contest. The liberal_institutional_reading relocates enforcement selectivity from intent to capacity, collapsing the victim set to empty and epsilon toward coordination cost; the sovereignty_maximalist_reading narrows the victim set to directly intervened states and drops the economic-conditionality core entirely. The disagreement is located in two structural elements: (a) whether selective enforcement evidences design or budget constraint, and (b) whether consent given under fiscal distress is valid consent.',
    'Adopting the liberal reading converts this tangled_rope toward rope with negligible extraction; adopting the sovereignty_maximalist reading converts it toward a narrower snare focused on intervention rather than finance. Cross-reading comparison is valid only at the kernel level, never by averaging epsilons.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Committer-frame locator: one reading of a three-reading kernel; sibling instantiations change victim sets and epsilon.').

omega_variable(
    selectivity_intent_vs_capacity,
    'Does enforcement selectivity track great-power interest beyond what neutral capacity constraints (distance, caseload, information) independently predict?',
    'Systematic coding of enforcement episodes (sanctions, referral, conditionality triggers) against a capacity model fitted to non-political predictors; residual correlation with sponsor interest profiles is the test. Comparable natural experiments: enforcement behavior shifts when the violating state''s alliance posture changes while its violation profile does not.',
    'If residuals are near zero, the liberal reading''s capacity account stands and measured extraction falls toward coordination cost; if interest-tracking survives capacity controls, the extraction reading is confirmed and the tangled_rope classification hardens toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selectivity_intent_vs_capacity, empirical, 'Whether the selectivity signature is design or budget constraint.').

omega_variable(
    distress_consent_validity,
    'Is consent to conditionality given under balance-of-payments emergency valid consent, such that the resulting agreements are contracts rather than coerced transfers?',
    'Conceptual analysis disciplined by comparative doctrine (duress, unconscionability, necessity in contract law) plus behavioral evidence: do debtor states renegotiate the same terms when liquidity constraints relax, and do they comply when enforcement lapses?',
    'If distress consent is invalid, the conditionality channel is extraction by construction and epsilon stays high regardless of program outcomes; if valid, part of the measured transfer is priced cooperation and the rope component strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distress_consent_validity, conceptual, 'Validity of consent under fiscal duress — the hinge between contract and coercion framings.').

omega_variable(
    exit_viability_counterfactual,
    'How viable are the exits — strategic default, de-dollarized settlement, full alignment with alternative lenders — that would convert the trapped and constrained target seats toward mobile?',
    'Observe realized exits (Argentina''s serial defaults, sanctioned-economy adaptation, BRICS settlement mechanisms) for cost curves and punishment latency; model counterfactual welfare under coordinated Southern exit using trade and financial-network data.',
    'High viable-exit rates would lower effective suppression and pull target-seat directionality back from the full-target end, softening the extraction reading; demonstrated punishment of every attempted exit confirms the suppression measure and the reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_viability_counterfactual, empirical, 'Counterfactual viability of the exits whose closure constitutes suppression.').

omega_variable(
    internalized_tina_layer,
    'What share of the arrangement''s persistence runs through internalized belief — policymaker conviction that no alternative exists — rather than through active coercive enforcement?',
    'Compare policy behavior across regimes with identical external constraints but different elite formation histories (technocrats trained in Northern programs versus heterodox formations); survey and archival work on reform episodes abandoned before external pressure was applied.',
    'If a large share of persistence is internalized, effective suppression exceeds the structural measure — the arrangement would survive enforcement decay longer than the machinery implies, and reform strategy must target belief formation, not just veto arithmetic.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalized_tina_layer, conceptual, 'Structural versus internalized component of the suppression sustaining the arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__hegemonic_extraction_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_hegemonic_tr_t1945, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 1945, 0.25).
narrative_ontology:measurement(rbio_hegemonic_tr_t1955, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 1955, 0.28).
narrative_ontology:measurement(rbio_hegemonic_tr_t1965, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 1965, 0.33).
narrative_ontology:measurement(rbio_hegemonic_tr_t1975, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 1975, 0.38).
narrative_ontology:measurement(rbio_hegemonic_tr_t1985, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 1985, 0.44).
narrative_ontology:measurement(rbio_hegemonic_tr_t1995, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 1995, 0.47).
narrative_ontology:measurement(rbio_hegemonic_tr_t2005, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 2005, 0.42).
narrative_ontology:measurement(rbio_hegemonic_tr_t2015, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 2015, 0.45).
narrative_ontology:measurement(rbio_hegemonic_tr_t2025, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(rbio_hegemonic_be_t1945, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 1945, 0.45).
narrative_ontology:measurement(rbio_hegemonic_be_t1955, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 1955, 0.48).
narrative_ontology:measurement(rbio_hegemonic_be_t1965, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 1965, 0.52).
narrative_ontology:measurement(rbio_hegemonic_be_t1975, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 1975, 0.58).
narrative_ontology:measurement(rbio_hegemonic_be_t1985, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 1985, 0.72).
narrative_ontology:measurement(rbio_hegemonic_be_t1995, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 1995, 0.74).
narrative_ontology:measurement(rbio_hegemonic_be_t2005, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 2005, 0.7).
narrative_ontology:measurement(rbio_hegemonic_be_t2015, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 2015, 0.73).
narrative_ontology:measurement(rbio_hegemonic_be_t2025, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 2025, 0.76).

% Suppression requirement over time
narrative_ontology:measurement(rbio_hegemonic_su_t1945, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 1945, 0.35).
narrative_ontology:measurement(rbio_hegemonic_su_t1955, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 1955, 0.38).
narrative_ontology:measurement(rbio_hegemonic_su_t1965, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 1965, 0.4).
narrative_ontology:measurement(rbio_hegemonic_su_t1975, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 1975, 0.45).
narrative_ontology:measurement(rbio_hegemonic_su_t1985, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 1985, 0.58).
narrative_ontology:measurement(rbio_hegemonic_su_t1995, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 1995, 0.62).
narrative_ontology:measurement(rbio_hegemonic_su_t2005, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 2005, 0.6).
narrative_ontology:measurement(rbio_hegemonic_su_t2015, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 2015, 0.66).
narrative_ontology:measurement(rbio_hegemonic_su_t2025, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 2025, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rbio_practice_norm_complex__hegemonic_extraction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__hegemonic_extraction_reading, liberal_institutional_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__hegemonic_extraction_reading, sovereignty_maximalist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the rules-based international order' decomposes into three structurally distinct readings of one kernel (rbio_practice_norm_complex), each with its own epsilon, beneficiary/victim structure, and classification. This member (hegemonic_extraction_reading) authors high epsilon over the standing arrangement with capital-north beneficiaries and Global South victims; liberal_institutional_reading authors near-zero extraction with a capacity-account of selectivity; sovereignty_maximalist_reading authors a narrower victim set (directly intervened states) and drops the economic-conditionality channel. The upstream/downstream structure runs from the liberal reading (the official self-description, cited as legitimating cover) toward this reading (which treats that self-description as the thing to be explained). All three files link one another via network.affects_constraints; cross-reading comparison happens at the kernel level, never by reconciling epsilons within one file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
