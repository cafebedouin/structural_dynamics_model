% ============================================================================
% CONSTRAINT STORY: irc_469_material_participation_kernel__strict_gatekeeper_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_irc_469_material_participation_kernel__strict_gatekeeper_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: irc_469_material_participation_kernel__strict_gatekeeper_reading
 *   human_readable: IRC § 469 Material Participation Strict Gatekeeper (Regulatory Reading)
 *   domain: tax_law/real_estate/regulatory_interpretation
 *
 * SUMMARY:
 *   IRC § 469 (passive loss limitation) emerged from the 1986 Tax Reform Act
 *   to prevent high-income individuals from sheltering earned income through
 *   real estate pass-throughs. The statute hinges on 'material participation'
 *   — a concept left undefined in the statute, defined operationally via
 *   regulation, ruling, and litigation. This constraint story instantiates
 *   the STRICT GATEKEEPER READING: material participation requires
 *   verifiable, substantial personal labor (≥750 hours, contemporaneous
 *   records, specific regulatory safe harbors) and a high documentation bar.
 *   This reading narrows the qualifying population, concentrates
 *   loss-deduction access among those who can afford advisory, and creates
 *   compliance friction for small operators. The STRATEGIC SHELTER READING
 *   (sibling constraint) interprets material participation permissively,
 *   allowing aggressive hour-counting and grouping elections. These are not
 *   alternative measurements of the same constraint — they represent
 *   irreconcilable interpretations of the same statutory kernel. Each
 *   produces different extraction patterns, different beneficiary structures,
 *   and different qualification barriers. This story authors the strict
 *   reading as-is, independent of the sibling; the engine compares their
 *   structural predictions.
 *
 * KEY AGENTS:
 *   - high_bracket_wage_earners: Beneficiary (arbitrary beneficiary ~d≈0.1) — protected loss-deduction access via narrow definition that excludes unsophisticated operators; can structure passive real estate investments to claim losses without extensive verification burden.
 *   - passive_investment_sheltering_complex: Beneficiary (institutional beneficiary ~d≈0.15) — large sponsors (syndications, REITs, family offices) that structure investments for high-bracket clients; the strict rule's complexity creates advisory capture and steady demand for structuring services.
 *   - rental_real_estate_active_participants: Target (moderate target ~d≈0.70) — small to mid-scale landlords who actively manage properties and expect loss deductions; the strict reading forces them to prove material participation via 750+ hours and contemporaneous records or accept passive-loss limitation.
 *   - real_estate_professionals: Target (organized target ~d≈0.65) — CPAs, tax attorneys, property managers whose client base must absorb the compliance cost; they are reluctant enforcers of the strict reading, simultaneously victimized and professionally dependent on the constraint.
 *   - small_property_operators: Target (powerless target ~d≈0.85) — individual owners of 1–4 unit rentals; documentation bar is beyond routine practice; cannot afford advisory help; trapped between formal compliance cost and loss-deduction loss.
 *   - IRS_examinations_function: Agenda-setter (institutional agenda-setter ~d≈0.5) — administers and enforces the regulatory interpretation; examination practices and published rulings drive the de-facto standard; administrative capacity determines which positioning gets enforced.
 *   - tax_courts_and_judges: Observer (analytical observer ~d≈0.5) — adjudicates disputes; produces case law that shapes interpretation; sees full-information picture but can only decide cases brought (litigation is expensive and rare).
 *   - congress_tax_authority: Observer/Architect (institutional d≈0.45, secondary role agenda-setter) — enacted the statute; could revise it to narrow or broaden material participation definition; currently inactive (legislative revision is rare).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.71).
domain_priors:suppression_score(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.68).
domain_priors:theater_ratio(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(irc_469_material_participation_kernel__strict_gatekeeper_reading, tangled_rope).
narrative_ontology:human_readable(irc_469_material_participation_kernel__strict_gatekeeper_reading, "IRC § 469 Material Participation Strict Gatekeeper (Regulatory Reading)").
narrative_ontology:topic_domain(irc_469_material_participation_kernel__strict_gatekeeper_reading, "tax_law/real_estate/regulatory_interpretation").

domain_priors:requires_active_enforcement(irc_469_material_participation_kernel__strict_gatekeeper_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(irc_469_material_participation_kernel__strict_gatekeeper_reading, 'd75c2012-98ac-4d39-9069-90f74228178e').
narrative_ontology:cs_kernel_codification('d75c2012-98ac-4d39-9069-90f74228178e', fixed_text).
narrative_ontology:cs_authority_grounding('d75c2012-98ac-4d39-9069-90f74228178e', extraction).
narrative_ontology:cs_interpretation_layer_present('d75c2012-98ac-4d39-9069-90f74228178e').
narrative_ontology:cs_reading_relation('d75c2012-98ac-4d39-9069-90f74228178e', irc_469_material_participation_kernel__strategic_shelter_reading, forecloses).
narrative_ontology:cs_axiom('d75c2012-98ac-4d39-9069-90f74228178e', foundational, material_participation_requires_verifiable_labor).
narrative_ontology:cs_axiom_status(material_participation_requires_verifiable_labor, holdable).
narrative_ontology:cs_axiom_grounding('d75c2012-98ac-4d39-9069-90f74228178e', material_participation_requires_verifiable_labor, deontological).
narrative_ontology:cs_axiom('d75c2012-98ac-4d39-9069-90f74228178e', foundational, documentation_bar_bars_unrecorded_participation).
narrative_ontology:cs_axiom_status(documentation_bar_bars_unrecorded_participation, holdable).
narrative_ontology:cs_axiom_grounding('d75c2012-98ac-4d39-9069-90f74228178e', documentation_bar_bars_unrecorded_participation, instrumental).
narrative_ontology:cs_reference_frame('d75c2012-98ac-4d39-9069-90f74228178e', statutory_passive_loss_limitation_core).
narrative_ontology:cs_drift_state('d75c2012-98ac-4d39-9069-90f74228178e', contemporary_2026, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d75c2012-98ac-4d39-9069-90f74228178e', '').
narrative_ontology:cs_kernel_id(irc_469_material_participation_kernel__strict_gatekeeper_reading, irc_469_material_participation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strict_gatekeeper_reading, high_bracket_wage_earners).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strict_gatekeeper_reading, passive_investment_sheltering_complex).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strict_gatekeeper_reading, rental_real_estate_active_participants).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strict_gatekeeper_reading, real_estate_professionals).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strict_gatekeeper_reading, small_property_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strict_gatekeeper_reading, real_estate_professionals).
narrative_ontology:constraint_vindicates(irc_469_material_participation_kernel__strict_gatekeeper_reading, graduated_income_protection_via_loss_limitation).
narrative_ontology:constraint_vindicates(irc_469_material_participation_kernel__strict_gatekeeper_reading, labor_valued_taxation_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Earn substantial W-2 income in high tax brackets (39.6%+ marginal rate). The strict material participation reading protects their high passive losses from passive-income rules that would otherwise disallow losses against ordinary income. They can structure their real estate investments to qualify as passive, absorb losses without verification burden, and defer loss realization through carried interests. Exit: switch jurisdictions or investment vehicles; arbitrage: invest in passive real estate funds meeting lesser scrutiny, or move income-producing assets to corporate structures.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, high_bracket_wage_earners, beneficiary,
    powerful, biographical, arbitrage, national).

% Large-scale real estate sponsors (syndications, REITs, family offices, institutional fund managers) that package passive real estate investments for high-bracket individuals. The strict reading's complexity and documentation bar creates advisory capture: CPAs, tax counsel, and wealth managers become mandatory intermediaries to navigate qualification, ensuring steady demand for structuring services. The high bar itself is their revenue model. Exit: operate in passive-shelter-permissive jurisdictions or structures; arbitrage: offer both aggressive and conservative strategies to different client cohorts.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, passive_investment_sheltering_complex, beneficiary,
    institutional, generational, arbitrage, national).

% Small to mid-scale landlords and real estate traders who manage properties actively (dealing with tenants, repairs, leasing decisions) and expect to offset ordinary income with real estate operating losses. The strict reading forces them into either (a) proving material participation via 750+ hours or regulatory tests, imposing extensive documentation burden; (b) accepting passive-loss limitation and losing loss deductions against W-2 income; or (c) abandoning active management to qualify as passive. Their identity is often constituted through real estate operation ('I'm a landlord,' 'I fix my own units'). Exit requires unraveling professional identity and portfolio structure built on loss deductibility assumption.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, rental_real_estate_active_participants, payer,
    moderate, biographical, identity_locked, regional).

% CPAs, tax attorneys, property managers, inspectors, and contractors whose bread-and-butter income depends on being retained to meet the strict documentation bar. They are simultaneously victims (their client base must absorb the compliance cost) and reluctant enforcers (their professional role is to navigate the strict reading). The constraint's friction is their job security but also creates client resentment. Exit: reduced to moving between firms or specializing in tax-shelter advisory, which is the same side of the constraint.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, real_estate_professionals, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(irc_469_material_participation_kernel__strict_gatekeeper_reading, real_estate_professionals, beneficiary).

% Individual owners of 1–4 unit rentals or small commercial properties. They work the properties themselves (painting, repairs, tenant screening) and record hours casually, if at all. The strict reading's documentation bar — contemporaneous logs, detailed hour records, reasonable attribution — is beyond routine small-business practice. They cannot afford advisory help and do not understand the regulatory interpretation. The constraint forces them to either formalize operations (high friction cost) or lose expected loss deductions. Exit: liquidate properties, convert to corporate structures, or accept losing deductions they believe are theirs by law.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, small_property_operators, payer,
    powerless, biographical, trapped, local).

% The IRS examination program enforces the strict reading through audit targets on passive-loss limitation positions. It administers the regulatory interpretation, issues Revenue Rulings, and litigates borderline cases. The examination function's position drives the constraint's enforcement: strict examination practices, published rulings, and litigation strategy set the de-facto standard narrower than the statutory text. Administrative capacity to audit determines which positioning gets enforced.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, irs_examinations_function, agenda_setter,
    institutional, generational, analytical, national).

% The U.S. Tax Court and federal district courts adjudicate disputes over material participation qualification. They produce case law that shapes the regulatory interpretation. Their decisions either tighten or relax enforcement, but they respond to cases brought (litigation is expensive and slow, so most disputes never reach court). They see the full-information picture but can only adjudicate cases brought.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, tax_courts_and_judges, observer,
    institutional, generational, analytical, national).

% Congress enacted IRC § 469 (passive loss rules) in 1986 as revenue raising and to prevent high-income individuals from using real estate losses to shelter earned income. The statute is ambiguous on material participation (no statutory definition; regulatory definition exists). Congress could revise the statute to narrow or broaden qualification, but tax legislation is rare. Their role is original architect; agenda-setter power only via legislative revision (low frequency).
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, congress_tax_authority, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(irc_469_material_participation_kernel__strict_gatekeeper_reading, congress_tax_authority, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(irc_469_material_participation_kernel__strict_gatekeeper_reading, passive_investment_sheltering_complex).
narrative_ontology:fixing_cost_class(irc_469_material_participation_kernel__strict_gatekeeper_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents high-bracket individuals from systematically using real estate losses (depreciation, construction deductions, developer shelters) to offset earned income through pass-through investments, ensuring graduated income tax collection and supporting legislative intent to preserve vertical equity in the tax code.
% TRANSFER_FUNCTION: Transfers tax-deduction rights from small and moderate-scale real estate operators (rental landlords, small developers) to high-bracket wage earners and passive-investment structures that can navigate the strict documentation bar. The mechanism: narrow material participation definition concentrates loss-deduction access among those who can afford advisory and implement tight record-keeping.
% ABSENT_VOICES: Real estate developers, contractors, and active small landlords with low documentation sophistication are structurally absent from regulatory interpretation processes. IRS guidance is written for tax professionals; comment letters on proposed regulations come from tax counsel and large sponsors, not from small operators. The power imbalance means restrictive interpretations persist because affected operators cannot afford to litigate.
% DISAPPEARANCE_RATIONALE: If the strict material participation reading and its enforcement disappeared overnight, the real estate tax shelter market would immediately loosen: high-bracket individuals could structure passive real estate investments with aggressive loss deductions; small landlords could claim losses on self-managed properties without extensive documentation; the tax advisor and structuring-service market would shrink. Federal tax revenue would fall in the near term (passive losses claimed), and the distribution of real estate investment returns would shift toward high-bracket beneficiaries.
% FOUNDING_PROBLEM: High-income individuals were using pass-through real estate investments to shelter earned income from taxation through aggressive depreciation and construction-period deductions, undermining vertical equity in the graduated income tax. The Tax Reform Act of 1986 enacted IRC § 469 to prevent this by disallowing passive losses against ordinary income.
% FOUNDING_PROBLEM_CORROBORATION: The IRS examinations function and Treasury Department attests the founding problem remains live: passive-loss shelter strategies evolve to evade the rules and require constant administrative vigilance. Tax practitioners and academic commentators attest the problem is substantially mitigated by modern IRS enforcement and that the strict material participation rule now over-protects passive-loss limitation by excluding legitimate small operators. Congressional testimony on Tax Cuts and Jobs Act (2017) and subsequent tax policy analysis from the Treasury Office of Tax Analysis documents the shift: modern debate is whether the rule prevents abuse or chills legitimate real estate operation.
narrative_ontology:disappearance_verdict(irc_469_material_participation_kernel__strict_gatekeeper_reading, world_rearranges).
narrative_ontology:founding_problem_status(irc_469_material_participation_kernel__strict_gatekeeper_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(irc_469_material_participation_kernel__strict_gatekeeper_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(irc_469_material_participation_kernel__strict_gatekeeper_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(irc_469_material_participation_kernel__strict_gatekeeper_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(irc_469_material_participation_kernel__strict_gatekeeper_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(irc_469_material_participation_kernel__strict_gatekeeper_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The strict gatekeeper reading produces high extractiveness (0.71 at interval end) because it concentrates loss-deduction access among high-bracket individuals and passive-investment structures that can navigate the documentation bar, while excluding small operators from loss deductions they believed were theirs. Suppression is high (0.68) because the constraint persists through active enforcement: IRS examinations, published rulings that narrow safe harbors, litigation strategy that tightens qualification standards. The measurement series shows extractiveness rising from 0.38 (1987, immediately post-enactment) to 0.71 (2026), as regulatory interpretation tightened and advisory-capture deepened. Theater ratio rises from 0.08 to 0.28 over the same period: early enforcement was substantive (real examination of hour records and material participation claims); modern enforcement increasingly relies on form (complex documentation requirements that de-facto exclude small operators regardless of actual participation). Suppression requirement rises from 0.42 to 0.68 as the IRS examination function built capacity and case law tightened safe harbors. All metrics are measured on the same time grid (start=1987, end=2026, measured at 10-year intervals plus endpoints) so temporal analysis can detect drift and divergence.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge dramatically in their experience of the constraint. From the high-bracket wage earner's perspective, the constraint is genuine coordination (preventing abuse of real estate losses) implemented via a reasonable documentation standard that they can easily meet through their advisors. From the small operator's perspective, the constraint is an extraction mechanism that denies loss deductions based on impractical documentation requirements. From the IRS perspective, the constraint is an administrable rule that has tightened over time as interpretation evolved. The engine computes per-seat classification from the structural data (power, exit, beneficiary/victim status); the strict reading's architecture predicts different types from different seats: rope-to-coordination-beneficiary (high-bracket), tangled-rope (small operators, professionals), piton-theater (IRS administrative function).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary seats (high-bracket wage earners, passive-investment complex) derive low d (d≈0.1–0.15) from beneficiary role + powerful/institutional power + arbitrage exit options (they can restructure investments if the rule tightens; switching jurisdictions or fund structures is available). Victim seats divide by power and exit: rental-real-estate-active-participants (moderate power, constrained exit via portfolio structure) derive ~d≈0.70; real-estate professionals (organized power, constrained exit to same advisory market) derive ~d≈0.65; small-property operators (powerless, identity-locked exit because operation is their professional identity) derive high d≈0.85. The strict reading's architecture concentrates extraction on powerless actors (d≈0.85) with identity-lock (they are 'landlords,' unraveling means professional reconstitution) and no arbitrage to other jurisdictions or structures. No directionality overrides are necessary; the structural derivation captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The strict gatekeeper reading is vulnerable to mandatrophy (mandate outliving function): the founding problem (high-income individuals sheltering earned income via aggressive real estate losses) has been substantially mitigated by modern enforcement, case law, and market sophistication. Yet the material participation restriction persists and has tightened, now excluding legitimate small operators and forcing unnecessary compliance costs. The constraint persists not because the founding problem demands it, but because (1) the extraction beneficiaries (high-bracket individuals, advisors) have captured the regulatory interpretation and prefer tight rules that exclude competition; (2) the IRS examination function has built institutional capacity around the tight interpretation; (3) small operators have no legislative voice to revise the rule. A mandatrophy verdict would recognize that the strict reading has outlived its coordination justification and now functions primarily as an extraction mechanism. The measurement series supports this: rising theater ratio (documentation form replacing substance) and rising suppression requirement (administrative tightening unrelated to abuse patterns) indicate theatrical maintenance, not responsive administration.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    documentation_sufficiency_ambiguity,
    'What evidence standard constitutes adequate proof of material participation? Are contemporaneous hour logs required, or can reconstructed records, testimony, and circumstantial evidence suffice?',
    'IRS guidance (published rulings, examination manual updates) and Tax Court precedent establish the sufficiency standard. A shift from contemporaneous-only to reconstructed-acceptable would relax qualification; a shift to strict contemporaneous-required would tighten it.',
    'If reconstructed records become acceptable, extractiveness falls (more operators qualify); small operators'' d decreases (exit becomes less identity-locked as compliance burden drops). If contemporaneous-only hardens, extractiveness rises (fewer qualify, high-bracket advantage amplifies).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(documentation_sufficiency_ambiguity, empirical, 'Whether the strict reading''s documentation bar is contemporaneous-records-only or flexible-reconstruction-acceptable.').

omega_variable(
    regulatory_interpretation_vs_statutory_text,
    'Does the strict material participation reading faithfully implement the statutory text (IRC § 469(h)), or does it exceed the statute by reading in requirements the statute does not contain?',
    'Statutory analysis comparing the regulation''s language and case law doctrine against the statute''s text and legislative history. A court could hold that the strict reading is ultra vires (exceeds delegated authority) and collapse it to the statutory definition.',
    'If the strict reading is found to exceed the statute, the regulatory interpretation could be invalidated, material participation qualification would broaden, and small operators would gain loss-deduction access they currently lack. Extractiveness would fall; small-operator d would decrease.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_interpretation_vs_statutory_text, conceptual, 'Whether the strict reading is a valid regulatory implementation of the statute or an ultra-vires tightening.').

omega_variable(
    separation_of_coordination_and_extraction,
    'Is the material participation requirement structurally necessary to implement the passive-loss limitation (prevent high-income shelter), or is it a sufficient-but-not-necessary mechanism that also extracts from legitimate small operators?',
    'Policy analysis: construct alternative implementations (e.g., income-ceiling passivity triggers, property-type passive presumptions) that achieve the founding problem''s solution with lower friction on small operators. Test whether other countries use narrower or broader definitions.',
    'If the requirement is sufficient-but-not-necessary and extraction can be decoupled, a narrowed or alternative rule would preserve coordination (founding problem solution) while reducing extraction (small operators gain deductions). Current beneficiaries would lose protected access.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(separation_of_coordination_and_extraction, conceptual, 'Whether material participation is structurally necessary to achieve passive-loss limitation or whether extraction is separable from coordination.').

omega_variable(
    identity_lock_mechanism_in_small_operators,
    'Is the exit_options identity_locked assessment accurate for small real estate operators? Or is the lock primarily economic (portfolio structure, sunk costs) rather than identity-constitutive (professional self-concept)?',
    'Post-exit trajectory study: if operators who exit real estate (liquidate, sell, corporate restructure) retain identity as ''real estate people'' and continue involvement through advisory or passive investment, the lock is economic; if identity dissolves with exit, the lock is constitutive.',
    'If identity-lock is primarily economic, d for small operators should decrease (exit becomes more available via restructuring, cost-of-exit decreases). If constitutive, d remains high (exit costs more than restructuring — it costs reconstitution of professional self).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_in_small_operators, empirical, 'Whether small-operator exit_options identity-lock is constitutive or primarily economic.').

omega_variable(
    founding_problem_persistence_empirical,
    'Do modern high-income individuals systematically use real estate pass-throughs to shelter earned income, or has enforcement and market evolution substantially mitigated the founding problem?',
    'IRS examination data (percentage of examined returns with passive-loss limitation adjustments, dollar amounts of disallowed losses, trends over time) and research using tax panel data (SOI data, confidential IRS files).',
    'If founding problem is substantially mitigated (examination data shows declining shelter use, case law is sparse, adjustment amounts are flat or declining), mandatrophy is present and the constraint persists primarily for extraction. If founding problem persists (examination data shows increasing shelter attempts, new avoidance strategies), the constraint remains justified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_persistence_empirical, empirical, 'Whether the founding problem (high-income earned-income sheltering via real estate) persists in modern practice.').

omega_variable(
    kernel_reading_structural_incommensurability,
    'Are the strict gatekeeper reading and the strategic shelter reading structurally incommensurable (cannot coexist in one framework), or do they represent different compliance postures held by different parties within the same framework?',
    'Regulatory specification analysis: does the statute itself contain language that privileges one reading, or are both readings supportable from the statutory text alone? If the statute is agnostic, the readings coexist (different parties'' interpretations, not incommensurable). If the statute favors one, that reading''s premise foreclosed the other.',
    'If incommensurable, one reading will eventually dominate via precedent or legislative change, and the loser''s structural predictions will diverge from actual outcomes. If they coexist, both readings'' structural predictions remain live (the constraint''s actual operation is a mixture).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_structural_incommensurability, conceptual, 'Whether the strict and strategic readings are structurally incommensurable or coexistent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(irc_469_material_participation_kernel__strict_gatekeeper_reading, 1987, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irc469_mp_strict_tr_t1987, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 1987, 0.08).
narrative_ontology:measurement_basis(irc469_mp_strict_tr_t1987, observed).
narrative_ontology:measurement(irc469_mp_strict_tr_t1997, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 1997, 0.12).
narrative_ontology:measurement_basis(irc469_mp_strict_tr_t1997, observed).
narrative_ontology:measurement(irc469_mp_strict_tr_t2007, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 2007, 0.18).
narrative_ontology:measurement_basis(irc469_mp_strict_tr_t2007, observed).
narrative_ontology:measurement(irc469_mp_strict_tr_t2017, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 2017, 0.24).
narrative_ontology:measurement_basis(irc469_mp_strict_tr_t2017, observed).
narrative_ontology:measurement(irc469_mp_strict_tr_t2026, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 2026, 0.28).
narrative_ontology:measurement_basis(irc469_mp_strict_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(irc469_mp_strict_be_t1987, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 1987, 0.38).
narrative_ontology:measurement_basis(irc469_mp_strict_be_t1987, observed).
narrative_ontology:measurement(irc469_mp_strict_be_t1997, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 1997, 0.52).
narrative_ontology:measurement_basis(irc469_mp_strict_be_t1997, observed).
narrative_ontology:measurement(irc469_mp_strict_be_t2007, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 2007, 0.61).
narrative_ontology:measurement_basis(irc469_mp_strict_be_t2007, observed).
narrative_ontology:measurement(irc469_mp_strict_be_t2017, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 2017, 0.68).
narrative_ontology:measurement_basis(irc469_mp_strict_be_t2017, observed).
narrative_ontology:measurement(irc469_mp_strict_be_t2026, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 2026, 0.71).
narrative_ontology:measurement_basis(irc469_mp_strict_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(irc469_mp_strict_su_t1987, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 1987, 0.42).
narrative_ontology:measurement_basis(irc469_mp_strict_su_t1987, observed).
narrative_ontology:measurement(irc469_mp_strict_su_t1997, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 1997, 0.54).
narrative_ontology:measurement_basis(irc469_mp_strict_su_t1997, observed).
narrative_ontology:measurement(irc469_mp_strict_su_t2007, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 2007, 0.63).
narrative_ontology:measurement_basis(irc469_mp_strict_su_t2007, observed).
narrative_ontology:measurement(irc469_mp_strict_su_t2017, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 2017, 0.66).
narrative_ontology:measurement_basis(irc469_mp_strict_su_t2017, observed).
narrative_ontology:measurement(irc469_mp_strict_su_t2026, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 2026, 0.68).
narrative_ontology:measurement_basis(irc469_mp_strict_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(irc_469_material_participation_kernel__strict_gatekeeper_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.12).
narrative_ontology:affects_constraint(irc_469_material_participation_kernel__strict_gatekeeper_reading, irc_469_material_participation_kernel__strategic_shelter_reading).
narrative_ontology:affects_constraint(irc_469_material_participation_kernel__strict_gatekeeper_reading, real_estate_passive_loss_limitation_mechanism).
narrative_ontology:affects_constraint(irc_469_material_participation_kernel__strict_gatekeeper_reading, tax_audit_capacity_constraint).

% DUAL FORMULATION NOTE:
% This constraint (strict gatekeeper reading) and irc_469_material_participation_kernel__strategic_shelter_reading are two readings of the same contested kernel: IRC § 469 material participation definition. They are NOT alternative measurements of the same constraint — they instantiate fundamentally different structural claims about what material participation means, who qualifies, and what extraction follows. The strict reading produces concentrated beneficiary access (high-bracket individuals, advisors); the strategic reading produces broader qualification. Each has its own ε, its own beneficiary/victim structure, its own classification. The engine compares their predictions against each other and against actual litigation outcomes to measure interpretive drift. Both readings remain live in the regulatory system; neither has been foreclosed by statute or precedent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
