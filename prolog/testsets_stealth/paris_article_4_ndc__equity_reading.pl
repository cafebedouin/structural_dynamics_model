% ============================================================================
% CONSTRAINT STORY: paris_article_4_ndc__equity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_paris_article_4_ndc__equity_reading, []).

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
 *   constraint_id: paris_article_4_ndc__equity_reading
 *   human_readable: CBDR-Differentiated Interpretation of Paris Article 4 NDCs (Equity Reading)
 *   domain: international_climate_governance/treaty_law/political_economy
 *
 * SUMMARY:
 *   This story authors ONE reading of the paris_article_4_ndc kernel: the
 *   equity reading, under which Article 4's nationally determined
 *   contributions are interpreted through Common But Differentiated
 *   Responsibilities and Respective Capabilities, making the
 *   developed/developing distinction a structural feature of the obligation
 *   architecture rather than a self-selected posture. Under this reading the
 *   arrangement coordinates near-universal participation by scaling
 *   obligations with responsibility and capability, while codifying transfer
 *   duties on developed states, reserving regulatory discretion for
 *   developing states, and concentrating enforcement-design veto power in
 *   equity coalitions. Per the constraint-family rule, the sibling readings
 *   are separate stories with separate epsilons: the sovereigntist reading
 *   authors a low-extraction voluntary-pledge coordination story; the
 *   supranational reading authors a high-suppression binding-trajectory story
 *   with symmetric obligations on major emitters. This file's epsilon (0.55)
 *   refers only to the CBDR-differentiated arrangement as this reading
 *   assesses it — moderate, with the burden concentrated on developed-state
 *   seats. KEY AGENTS (by structural relationship): -
 *   large_developing_emitters: Primary beneficiary (powerful/constrained) —
 *   retain policy space, collect transfer claims - g77_negotiating_bloc:
 *   Agenda-setting beneficiary (organized/mobile) — holds the consensus veto
 *   over enforcement design - developed_state_treasuries: Primary payer
 *   (institutional/constrained) — bears codified finance obligations -
 *   developed_carbon_intensive_industries: Secondary payer
 *   (powerful/arbitrage) — bears asymmetric stringency, dampened by
 *   relocation options - least_developed_countries: Protected beneficiary
 *   (organized/trapped) — priority claims on finance windows -
 *   small_island_states: Dual-positioned beneficiary/payer
 *   (organized/trapped) — collects through funds, pays in delayed global
 *   ambition - unfccc_secretariat: Administrator (institutional/constrained)
 *   — operates the differentiated transparency machinery -
 *   ipcc_assessment_community: Analytical observer (institutional/analytical)
 *   - future_generations: Absent voice (powerless/trapped) — bear the
 *   atmospheric residue of the bargain
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paris_article_4_ndc__equity_reading, 0.55).
domain_priors:suppression_score(paris_article_4_ndc__equity_reading, 0.55).
domain_priors:theater_ratio(paris_article_4_ndc__equity_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paris_article_4_ndc__equity_reading, tangled_rope).
narrative_ontology:human_readable(paris_article_4_ndc__equity_reading, "CBDR-Differentiated Interpretation of Paris Article 4 NDCs (Equity Reading)").
narrative_ontology:topic_domain(paris_article_4_ndc__equity_reading, "international_climate_governance/treaty_law/political_economy").

domain_priors:requires_active_enforcement(paris_article_4_ndc__equity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(paris_article_4_ndc__equity_reading, 'f4e7bcfa-5026-4f7d-b92c-4935cd4e9577').
narrative_ontology:cs_kernel_codification('f4e7bcfa-5026-4f7d-b92c-4935cd4e9577', fixed_text).
narrative_ontology:cs_authority_grounding('f4e7bcfa-5026-4f7d-b92c-4935cd4e9577', lineage).
narrative_ontology:cs_interpretation_layer_present('f4e7bcfa-5026-4f7d-b92c-4935cd4e9577').
narrative_ontology:cs_reading_relation('f4e7bcfa-5026-4f7d-b92c-4935cd4e9577', paris_article_4_ndc__sovereigntist_reading, coexists_with).
narrative_ontology:cs_reading_relation('f4e7bcfa-5026-4f7d-b92c-4935cd4e9577', paris_article_4_ndc__supranational_reading, influences).
narrative_ontology:cs_axiom('f4e7bcfa-5026-4f7d-b92c-4935cd4e9577', foundational, obligations_scale_with_responsibility_and_capability).
narrative_ontology:cs_axiom_status(obligations_scale_with_responsibility_and_capability, holdable).
narrative_ontology:cs_axiom_grounding('f4e7bcfa-5026-4f7d-b92c-4935cd4e9577', obligations_scale_with_responsibility_and_capability, deontological).
narrative_ontology:cs_axiom('f4e7bcfa-5026-4f7d-b92c-4935cd4e9577', foundational, state_category_distinctions_are_structural_not_elected).
narrative_ontology:cs_axiom_status(state_category_distinctions_are_structural_not_elected, holdable).
narrative_ontology:cs_axiom_grounding('f4e7bcfa-5026-4f7d-b92c-4935cd4e9577', state_category_distinctions_are_structural_not_elected, conventional).
narrative_ontology:cs_reference_frame('f4e7bcfa-5026-4f7d-b92c-4935cd4e9577', rio_cbdr_differentiated_bargain).
narrative_ontology:cs_drift_state('f4e7bcfa-5026-4f7d-b92c-4935cd4e9577', post_paris_global_stocktake_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('f4e7bcfa-5026-4f7d-b92c-4935cd4e9577', '').
narrative_ontology:cs_kernel_id(paris_article_4_ndc__equity_reading, paris_article_4_ndc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__equity_reading, large_developing_emitters).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__equity_reading, least_developed_countries).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__equity_reading, g77_negotiating_bloc).
narrative_ontology:constraint_victim(paris_article_4_ndc__equity_reading, developed_state_treasuries).
narrative_ontology:constraint_victim(paris_article_4_ndc__equity_reading, developed_carbon_intensive_industries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__equity_reading, small_island_states).
narrative_ontology:constraint_victim(paris_article_4_ndc__equity_reading, small_island_states).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__equity_reading, cbdr_rc_principle).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__equity_reading, historical_responsibility_doctrine).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__equity_reading, respective_capabilities_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Submit NDCs framed around development priorities rather than economy-wide absolute caps, set peak-emissions timelines domestically, and condition a large share of stated ambition on delivered finance and technology. Collect climate finance, technology-transfer claims, and market-mechanism access. Cannot be compelled to raise targets; their principal exposure is reputational, plus the long-run expectation that the categories will eventually catch up with their emissions.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, large_developing_emitters, beneficiary,
    powerful, generational, constrained, global).

% Hold formal priority claims on adaptation finance, capacity-building, and loss-and-damage windows. Their NDCs are frequently conditional documents awaiting funding. Exposure to warming is direct and territorial; leverage inside the talks comes almost entirely from bloc membership, and leaving the regime would forfeit the finance channels they depend on.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, least_developed_countries, beneficiary,
    organized, immediate, trapped, national).

% Coordinates roughly 130 developing-country parties into a common negotiating position. Controls the consensus chokepoint: no decision on transparency, review, or the finance contributor base passes without its assent. Walkouts and floor fights are its recurring instruments. It both defends the category system and draws procedural power from defending it.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, g77_negotiating_bloc, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(paris_article_4_ndc__equity_reading, g77_negotiating_bloc, agenda_setter).

% Appropriate and disburse climate finance against competing domestic claims, report on delivery, and negotiate the contributor base. Bound by ratified treaty language ('shall provide') while facing legislatures that treat the sums as discretionary. Withdrawal from the treaty is the only clean exit and carries severe diplomatic cost, as the repeated United States cycle demonstrates.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, developed_state_treasuries, payer,
    institutional, biographical, constrained, national).

% Face decarbonization mandates at home while competitors in shielded jurisdictions operate under lighter expectations. Production relocation, border-adjustment lobbying, and litigation are their standard responses; their mobility dampens how tightly the asymmetry binds them compared to fiscally locked treasuries.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, developed_carbon_intensive_industries, payer,
    powerful, immediate, arbitrage, global).

% Advocate maximal global ambition while drawing on the category system for finance and special treatment. Their exposure is existential and immediate: they collect through dedicated funds when differentiation channels money their way, and they pay in delayed worldwide mitigation whenever it shields large emitters.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, small_island_states, beneficiary,
    organized, immediate, trapped, regional).
narrative_ontology:stakeholder_secondary_role(paris_article_4_ndc__equity_reading, small_island_states, payer).

% Administers the transparency framework, compiles NDC syntheses, and services the global stocktake. Sets procedural agendas under party direction; it operates the differentiated reporting machinery but cannot alter the category structure it administers.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, unfccc_secretariat, agenda_setter,
    institutional, generational, constrained, global).

% Produces the assessment cycles that quantify responsibility and capability distributions. Its findings feed the stocktake but carry no decision rights; it observes the gap between pledged and delivered from outside the negotiation.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, ipcc_assessment_community, observer,
    institutional, generational, analytical, global).

% Bear the atmospheric consequences of whatever stringency the bargain permits. Invoked in the preamble language of every COP decision and absent from every negotiating room; no delegation holds their proxy.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, future_generations, excluded,
    powerless, civilizational, trapped, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(paris_article_4_ndc__equity_reading, large_developing_emitters).
narrative_ontology:fixing_cost_class(paris_article_4_ndc__equity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the participation problem of global climate governance: near-universal membership in a mitigation regime across radically unequal states is achievable only if obligations scale with responsibility and capability. Category-based differentiation is the burden-sharing formula that buys universality, and the consensus veto is the mechanism that keeps the formula amendable only by agreement.
% TRANSFER_FUNCTION: Moves finance from developed-state treasuries to developing-state programs; moves stringency burdens onto developed-state industries while reserving regulatory discretion for developing states; and moves agenda control over enforcement design to the equity coalitions that hold the consensus chokepoint.
% ABSENT_VOICES: Future generations and frontline exposed communities hold no seat; developed-state electorates who ultimately fund the transfers appear only indirectly through treasury positions; subnational governments and non-party actors observe without consent rights. Their objection — that the bargain trades their exposure for participation — is voiced only by proxy, chiefly through island-state interventions and assessment-community findings.
% DISAPPEARANCE_RATIONALE: Without category-based differentiation, the grand bargain unwinds: developing-state parties withdraw consent to economy-wide expectations, the G77 fractures or exits, finance loses its normative anchor and shrinks toward ordinary aid, and the near-universal regime reverts to minilateral clubs among willing states — a wholesale rearrangement of climate governance.
% FOUNDING_PROBLEM: The 1992 bargain had to reconcile universal participation with radically unequal responsibility and capacity: no climate regime could bind the South on Northern terms, and none could work without the South. CBDR was the formula; Article 4's NDC provision inherited it after the Kyoto-era firewall proved politically unsustainable in its hard form.
% FOUNDING_PROBLEM_CORROBORATION: IPCC assessment cycles and independent economic series (OECD, World Bank) attest the continuing responsibility and capability divergence from outside the benefiting parties; developed-state negotiators' own insistence on contributor-base evolution attests the problem's persistence from the paying side. No attestation rests solely on the bloc that benefits from the arrangement.
narrative_ontology:disappearance_verdict(paris_article_4_ndc__equity_reading, world_rearranges).
narrative_ontology:founding_problem_status(paris_article_4_ndc__equity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(paris_article_4_ndc__equity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(paris_article_4_ndc__equity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(paris_article_4_ndc__equity_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(paris_article_4_ndc__equity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(paris_article_4_ndc__equity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(paris_article_4_ndc__equity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.55: the arrangement levies codified transfer duties and tighter target expectations on developed-state seats while purchasing genuine universality — the burden is real but bounded by the coordination value it buys. Suppression (0.55) is authored as a raw structural property, unscaled: consensus rules give equity coalitions a working veto and reputational discipline raises the cost of defection, yet the regime has no hard compliance branch, so alternatives are narrowed rather than closed. Theater (0.50) tracks the pledge-announcement cycle — conditional NDCs, re-announced finance, net-zero declarations without inventory backing — rising steadily as announcement substitutes for delivery. Accessibility collapse (0.55): inside the UNFCCC forum, harmonized alternatives are effectively foreclosed by the veto; outside it, minilateral clubs and bilateral deals keep partial alternatives alive. Resistance (0.65) is continuous and documented: contributor-base expansion fights, delivery disputes, open 'evolution of differentiation' proposals from the paying seats, plus island-state resistance to large-emitter shielding. The suppression_requirement series is authored because enforcement capacity is the traced dynamic: the hard Kyoto-era firewall (rising to 0.48), post-Copenhagen enforcement decay (dip to 0.40), then post-Paris re-consolidation of veto practice over transparency and review design (back to 0.55). All three series run on one shared seven-point grid spanning 1992-2024. The claimed type (tangled_rope) is asserted from structure — a genuine coordination function joined to asymmetric, actively maintained extraction — independently of these metric values; the engine computes per-seat types from the structural data.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute sharply different arrangements from one treaty text. A developed-state treasury — institutional power, constrained exit, codified 'shall provide' language — experiences the structure as enforced obligation with no in-forum exit. A developed-state carbon-intensive industry holds arbitrage-grade exit (relocation, border-adjustment lobbying) and therefore meets a looser version of the same asymmetry. A large developing emitter experiences the identical text as protective: it reserves discretion and channels resources inward. Small island states straddle: the category system funds their adaptation while delaying the global mitigation their survival depends on. Same instrument, four different lived arrangements — the engine derives this divergence from power, exit, and role, not from the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the developing-state seats toward the subsidy end: large emitters and least developed countries collect policy space and finance; the G77 bloc additionally collects agenda power through its secondary agenda_setter role, which does not alter its collection position. Victim declarations drive the developed seats toward the target end: treasuries are constrained by ratified language and reputational lock-in, so they sit near the full-target end; industries carry arbitrage-grade exit, which the derivation correctly dampens below trapped-payer levels — carbon leakage is real and measurable. No directionality overrides are authored: role-plus-exit derivation reproduces the structural relationships without correction. Scope amplification applies at the constraint level — the regime's global reach makes delivery verification harder — and is owned by the engine, not authored here.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling universal participation with unequal responsibility and capability — remains live, and disappearance would rearrange the world (verdict: world_rearranges), so the status-by-verdict mismatch consumer finds alignment and no zombie flag. The tangled_rope classification prevents two symmetrical mislabels: the developed-state grievance framing reads the arrangement as pure extraction and misses the universality function that no currently available alternative delivers; the solidarity framing reads it as pure coordination and misses the concentrated, codified payment obligations and the veto rents collected by bloc leadership. Holding coordination and asymmetry together in one structure is precisely what the category requires. Mandatrophy is not resolved and no sunset clause is authored: within this reading, differentiation is a durable structural feature, not transitional support — the transitional hypothesis is carried as an omega instead of a scaffold declaration.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_position,
    'This constraint is one reading of the paris_article_4_ndc kernel; what structural delta would each sibling reading produce if instantiated?',
    'Comparative classification across the three reading-stories in the family: classify the sovereigntist_reading and supranational_reading files and diff beneficiary/victim sets, epsilon, and computed per-seat types against this file.',
    'The sovereigntist sibling dissolves category obligations and transfer duties (low epsilon, coordination-only profile); the supranational sibling binds major emitters symmetrically and concentrates suppression on developing-state discretion. Divergence between the three computed profiles measures how much this reading''s structure depends on the differentiation premise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_position, conceptual, 'Committer-frame membership: one of three readings of the Article 4 NDC kernel.').

omega_variable(
    differentiation_disagreement_location,
    'Where do the readings locate the operative meaning of Article 4 — in category-based obligation structure (this reading), in self-chosen pledge discretion (sovereigntist), or in internationally policed trajectories (supranational)?',
    'Doctrinal analysis of negotiating history (travaux preparatoires, decision texts) plus observed COP behavior on transparency and review design; no further empirical test settles a conceptual dispute over clause meaning.',
    'If the ''national circumstances'' clause is read as mere context, this reading''s structural-distinction requirement loses its textual anchor and the constraint migrates toward the sovereigntist sibling''s profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(differentiation_disagreement_location, conceptual, 'Locates the inter-reading disagreement in the interpretation of the differentiation clauses.').

omega_variable(
    differentiation_transitional_status,
    'Is CBDR differentiation a durable structural feature of the regime or a transitional support that sunsets as capabilities converge?',
    'Capability-convergence series (shares of global GDP, emissions, and finance capacity held by non-Annex-I major economies) tracked against COP precedent on contributor-base and category language.',
    'If convergence forces category obsolescence, the arrangement carries an implicit sunset and drifts toward a transitional profile; if categories persist past convergence, the differentiation hardens into maintained asymmetry with rising theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(differentiation_transitional_status, empirical, 'Whether the differentiation structure is transitional or durable.').

omega_variable(
    shielding_vs_protection_ambiguity,
    'Does the category system protect genuinely low-capability states, or does it shield high-capacity large emitters from proportionate effort?',
    'Decompose NDC ambition gaps by per-capita GDP and per-capita emissions across developing-state parties; compare effort metrics between LDC-class and major-emitter-class members of the same category.',
    'If shielding dominates, the victim set widens beyond developed-state seats — exposed populations and future generations absorb the deferred mitigation — and effective extraction rises above the authored base; if protection dominates, the coordination framing strengthens and the asymmetry reads as corrective rather than extractive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(shielding_vs_protection_ambiguity, empirical, 'Whether differentiation protects the incapable or shields the capable.').

omega_variable(
    finance_additionality_ambiguity,
    'Are transfer flows under the finance obligations additional to existing development assistance, or relabeled from it?',
    'OECD DAC accounting cross-checked against UNFCCC biennial reporting; track overlap between ODA budget lines and climate-finance budget lines.',
    'If flows are largely relabeled, the developed-seat payment burden is smaller than the codified duty suggests and effective extraction on those seats falls; if additional, the transfer is a real resource movement and the asymmetry is material.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(finance_additionality_ambiguity, empirical, 'Whether codified transfers constitute new resources or rebranded aid.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_article_4_ndc__equity_reading, 0, 32).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pari_tr_t0, paris_article_4_ndc__equity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(pari_tr_t0, observed).
narrative_ontology:measurement(pari_tr_t5, paris_article_4_ndc__equity_reading, theater_ratio, 5, 0.25).
narrative_ontology:measurement_basis(pari_tr_t5, observed).
narrative_ontology:measurement(pari_tr_t10, paris_article_4_ndc__equity_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement_basis(pari_tr_t10, observed).
narrative_ontology:measurement(pari_tr_t16, paris_article_4_ndc__equity_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement_basis(pari_tr_t16, observed).
narrative_ontology:measurement(pari_tr_t23, paris_article_4_ndc__equity_reading, theater_ratio, 23, 0.42).
narrative_ontology:measurement_basis(pari_tr_t23, observed).
narrative_ontology:measurement(pari_tr_t28, paris_article_4_ndc__equity_reading, theater_ratio, 28, 0.46).
narrative_ontology:measurement_basis(pari_tr_t28, observed).
narrative_ontology:measurement(pari_tr_t32, paris_article_4_ndc__equity_reading, theater_ratio, 32, 0.5).
narrative_ontology:measurement_basis(pari_tr_t32, observed).

% Extraction over time
narrative_ontology:measurement(pari_be_t0, paris_article_4_ndc__equity_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(pari_be_t0, observed).
narrative_ontology:measurement(pari_be_t5, paris_article_4_ndc__equity_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement_basis(pari_be_t5, observed).
narrative_ontology:measurement(pari_be_t10, paris_article_4_ndc__equity_reading, base_extractiveness, 10, 0.44).
narrative_ontology:measurement_basis(pari_be_t10, observed).
narrative_ontology:measurement(pari_be_t16, paris_article_4_ndc__equity_reading, base_extractiveness, 16, 0.4).
narrative_ontology:measurement_basis(pari_be_t16, observed).
narrative_ontology:measurement(pari_be_t23, paris_article_4_ndc__equity_reading, base_extractiveness, 23, 0.48).
narrative_ontology:measurement_basis(pari_be_t23, observed).
narrative_ontology:measurement(pari_be_t28, paris_article_4_ndc__equity_reading, base_extractiveness, 28, 0.52).
narrative_ontology:measurement_basis(pari_be_t28, observed).
narrative_ontology:measurement(pari_be_t32, paris_article_4_ndc__equity_reading, base_extractiveness, 32, 0.55).
narrative_ontology:measurement_basis(pari_be_t32, observed).

% Suppression requirement over time
narrative_ontology:measurement(pari_su_t0, paris_article_4_ndc__equity_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(pari_su_t0, observed).
narrative_ontology:measurement(pari_su_t5, paris_article_4_ndc__equity_reading, suppression_requirement, 5, 0.45).
narrative_ontology:measurement_basis(pari_su_t5, observed).
narrative_ontology:measurement(pari_su_t10, paris_article_4_ndc__equity_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement_basis(pari_su_t10, observed).
narrative_ontology:measurement(pari_su_t16, paris_article_4_ndc__equity_reading, suppression_requirement, 16, 0.4).
narrative_ontology:measurement_basis(pari_su_t16, observed).
narrative_ontology:measurement(pari_su_t23, paris_article_4_ndc__equity_reading, suppression_requirement, 23, 0.44).
narrative_ontology:measurement_basis(pari_su_t23, observed).
narrative_ontology:measurement(pari_su_t28, paris_article_4_ndc__equity_reading, suppression_requirement, 28, 0.5).
narrative_ontology:measurement_basis(pari_su_t28, observed).
narrative_ontology:measurement(pari_su_t32, paris_article_4_ndc__equity_reading, suppression_requirement, 32, 0.55).
narrative_ontology:measurement_basis(pari_su_t32, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paris_article_4_ndc__equity_reading, resource_allocation).
narrative_ontology:affects_constraint(paris_article_4_ndc__equity_reading, paris_article_4_ndc__sovereigntist_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__equity_reading, paris_article_4_ndc__supranational_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Paris NDC regime' decomposes into three reading-instantiated constraints with distinct epsilons and beneficiary structures, linked as a constraint family. The equity reading sits upstream of the supranational reading: its veto structure changes which enforcement architectures are feasible without foreclosing differentiated-binding hybrids. The sovereigntist reading coexists with both, and parts of the same coalitions hold it simultaneously with this one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
