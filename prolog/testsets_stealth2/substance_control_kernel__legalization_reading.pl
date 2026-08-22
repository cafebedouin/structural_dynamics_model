% ============================================================================
% CONSTRAINT STORY: substance_control_kernel__legalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_kernel__legalization_reading, []).

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
 *   constraint_id: substance_control_kernel__legalization_reading
 *   human_readable: Liberty-Bounded Substance Control Regime (Legalization Reading)
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the substance_control_kernel: the
 *   legalization reading, under which substance use is self-regarding liberty
 *   and the state's legitimate intervention is confined to preventing
 *   third-party harm and capturing externality costs. The standing
 *   arrangement under contest — and the sole ε referent — is the legalized
 *   regime itself: licensed markets, excise taxation, age gates, impairment
 *   enforcement, and public-use rules. Per the ε-invariance principle, the
 *   colloquial label 'substance policy' decomposes into three structurally
 *   distinct constraints (this reading, the prohibition reading, the
 *   harm-reduction reading), each with its own ε, victim set, and
 *   classification; they are linked as a constraint family through
 *   network.affects_constraints. Under this reading's own lights, users exit
 *   the victim set entirely, third parties enter it through externalities the
 *   regime only partially prices, the legal industry gains market access, and
 *   the state becomes a revenue collector whose fiscal dependence slowly
 *   ratchets extraction upward. The claim (tangled_rope) and the metrics are
 *   authored independently: the metrics describe the regime's actual
 *   operation, and any divergence between claim and computed per-seat types
 *   is the datum, not an error. KEY AGENTS (by structural relationship): -
 *   adult_users_of_legal_substances: Primary beneficiary (moderate/mobile) —
 *   holds the liberty interest the constraint secures -
 *   licensed_substance_industry: Secondary beneficiary (powerful/arbitrage) —
 *   converts legality into durable market access - state_fiscal_authority:
 *   Agenda-setter and revenue collector (institutional/constrained) —
 *   administers the boundary and accrues the tax stream -
 *   road_users_exposed_to_impaired_drivers: Primary payer (powerless/trapped)
 *   — bears the uncompensated crash externality -
 *   bystanders_to_secondhand_exposure: Secondary payer (powerless/trapped) —
 *   bears the exposure externality where use rules lag -
 *   illicit_market_operators: Excluded competitor (organized/mobile) — kept
 *   outside the legal market by the same enforcement that protects it -
 *   public_health_agencies: Analytical observer (institutional/analytical) —
 *   measures where the third-party-harm line sits
 *
 * KEY AGENTS:
 *   - adult_users_of_legal_substances: primary beneficiary (moderate/mobile) — liberty interest holder
 *   - licensed_substance_industry: secondary beneficiary (powerful/arbitrage) — market-access gainer
 *   - state_fiscal_authority: agenda_setter + beneficiary (institutional/constrained) — boundary administrator and tax recipient
 *   - road_users_exposed_to_impaired_drivers: primary payer (powerless/trapped) — crash-externality bearer
 *   - bystanders_to_secondhand_exposure: secondary payer (powerless/trapped) — exposure-externality bearer
 *   - illicit_market_operators: excluded (organized/mobile) — enforced-out gray-market supplier
 *   - public_health_agencies: observer (institutional/analytical) — externality measurer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_kernel__legalization_reading, 0.4).
domain_priors:suppression_score(substance_control_kernel__legalization_reading, 0.32).
domain_priors:theater_ratio(substance_control_kernel__legalization_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__legalization_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_kernel__legalization_reading, "Liberty-Bounded Substance Control Regime (Legalization Reading)").
narrative_ontology:topic_domain(substance_control_kernel__legalization_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_kernel__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__legalization_reading, '5be6ac39-5360-4ecf-b351-7bed450ebf36').
narrative_ontology:cs_kernel_codification('5be6ac39-5360-4ecf-b351-7bed450ebf36', distributed).
narrative_ontology:cs_authority_grounding('5be6ac39-5360-4ecf-b351-7bed450ebf36', distributed).
narrative_ontology:cs_reading_relation('5be6ac39-5360-4ecf-b351-7bed450ebf36', substance_control_kernel__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('5be6ac39-5360-4ecf-b351-7bed450ebf36', substance_control_kernel__harm_reduction_reading, coexists_with).
narrative_ontology:cs_axiom('5be6ac39-5360-4ecf-b351-7bed450ebf36', foundational, self_regarding_use_outside_legitimate_coercion).
narrative_ontology:cs_axiom_status(self_regarding_use_outside_legitimate_coercion, holdable).
narrative_ontology:cs_axiom_grounding('5be6ac39-5360-4ecf-b351-7bed450ebf36', self_regarding_use_outside_legitimate_coercion, deontological).
narrative_ontology:cs_axiom('5be6ac39-5360-4ecf-b351-7bed450ebf36', foundational, externality_capture_sufficient_state_response).
narrative_ontology:cs_axiom_status(externality_capture_sufficient_state_response, holdable).
narrative_ontology:cs_axiom_grounding('5be6ac39-5360-4ecf-b351-7bed450ebf36', externality_capture_sufficient_state_response, instrumental).
narrative_ontology:cs_reference_frame('5be6ac39-5360-4ecf-b351-7bed450ebf36', millian_self_regarding_baseline).
narrative_ontology:cs_drift_state('5be6ac39-5360-4ecf-b351-7bed450ebf36', contemporary_tax_ratchet_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5be6ac39-5360-4ecf-b351-7bed450ebf36', '').
narrative_ontology:cs_kernel_id(substance_control_kernel__legalization_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, adult_users_of_legal_substances).
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, licensed_substance_industry).
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, state_fiscal_authority).
narrative_ontology:constraint_victim(substance_control_kernel__legalization_reading, road_users_exposed_to_impaired_drivers).
narrative_ontology:constraint_victim(substance_control_kernel__legalization_reading, bystanders_to_secondhand_exposure).
narrative_ontology:constraint_vindicates(substance_control_kernel__legalization_reading, millian_harm_principle).
narrative_ontology:constraint_vindicates(substance_control_kernel__legalization_reading, pigouvian_externality_capture_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Purchase and consume regulated substances through legal channels, paying embedded excise taxes and observing age gates and public-use rules. Their liberty interest is the organizing premise of the arrangement. Exit from the cost side is ordinary consumer choice: reduce, substitute, or cease use without legal penalty.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, adult_users_of_legal_substances, beneficiary,
    moderate, biographical, mobile, national).

% Cultivates, manufactures, and retails under license, paying licensing fees and excise taxes that prohibition denied the opportunity to pay legally. Lobbies across jurisdictions for rate relief, advertising latitude, and retail density. Can shift production sites and capital between states or countries if any single regime turns hostile.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, licensed_substance_industry, beneficiary,
    powerful, generational, arbitrage, continental).

% Drafts the age limits, tax schedules, licensing rules, and impairment thresholds that constitute the intervention boundary, and collects the excise revenue the boundary generates. Over successive budget cycles it comes to plan around the revenue stream it administers, which raises the internal political cost of ever recalibrating rates downward. It cannot exit its own territory or stop administering what it has enacted.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, state_fiscal_authority, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__legalization_reading, state_fiscal_authority, beneficiary).

% Share roads with drivers impaired by legally purchased substances. Bear crash risk, injury costs, and insurance premia that rise with aggregate impaired driving. Compensation arrives only through whatever fraction of tax revenue is earmarked for enforcement and trauma care. Cannot opt out of shared road infrastructure.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, road_users_exposed_to_impaired_drivers, payer,
    powerless, biographical, trapped, national).

% Inhale secondhand smoke and vapor in multi-unit housing, patios, and public spaces where use rules lag behind legalization. Bear respiratory and nuisance costs with recourse limited to local ordinances that vary widely by municipality. Moving away from exposure is possible only at personal cost, and exposure follows residential density of permitted use.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, bystanders_to_secondhand_exposure, payer,
    powerless, biographical, trapped, regional).

% Continue supplying untaxed product where tax burdens make legal supply uncompetitive or where licensing caps restrict entry. Are kept outside the legal market by inspection and tax enforcement they cannot join. Would take licenses if the economics allowed; their persistence defines the gray areas the regime's enforcement machinery patrols.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, illicit_market_operators, excluded,
    organized, immediate, mobile, continental).

% Compile epidemiological, road-safety, and emergency-department data on third-party harms, and recommend tax levels and restriction boundaries. Their measurements operationally define where the third-party-harm line sits, but they hold no enforcement power and no revenue stake.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, public_health_agencies, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_kernel__legalization_reading, state_fiscal_authority).
narrative_ontology:fixing_cost_class(substance_control_kernel__legalization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Defines and enforces the boundary of legitimate state action in the substance domain: settles collectively, once, the otherwise endlessly relitigated question of when coercion is permissible (third-party harm) versus impermissible (self-regarding conduct), and routes a dedicated revenue stream toward remediating the externalities the boundary permits.
% TRANSFER_FUNCTION: Moves money from purchasers and licensees (excise taxes, licensing fees) to the state treasury; moves externality risk onto non-consenting third parties to the extent that taxes and remediation under-capture the harm; moves market access from illicit suppliers to licensed firms.
% ABSENT_VOICES: Third parties bearing externalities appear only as aggregate statistics, never as negotiating seats: impaired-driving casualties, exposed bystanders, and residents of high retail-density neighborhoods enter the conversation solely through epidemiological tables compiled after the fact. Future heavy users whose dependency trajectories are not yet visible are likewise unrepresented. They are outside the licensing and taxation negotiations where the boundary is actually drawn.
% DISAPPEARANCE_RATIONALE: If the legalized-regulatory boundary vanished overnight, the world would rearrange around whichever successor arrangement seized the vacuum: re-criminalization would rebuild the arrest, prosecution, and black-market apparatus; ungoverned markets would strip out age gates, product standards, and the remediation revenue stream entirely. Either successor dismantles settled expectations of users, licensees, treasury planners, and third parties alike.
% FOUNDING_PROBLEM: After criminalization proved both costly and liberty-violating, the founding problem was reconciling individual liberty in self-regarding conduct with the real social costs of substance use: how to permit use while preventing and pricing the harms that fall on people who did not consent to bear them.
% FOUNDING_PROBLEM_CORROBORATION: Road-safety statistics, emergency-department surveillance, and insurer actuarial data — all compiled outside the beneficiary set of users, industry, and treasury — attest that third-party harms persist and that the boundary question recurs with every new substance and delivery system. No corroborating source attests the problem is solved.
narrative_ontology:disappearance_verdict(substance_control_kernel__legalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_kernel__legalization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__legalization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(substance_control_kernel__legalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_kernel__legalization_reading, 0.4, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_kernel__legalization_reading_tests).
:- end_tests(substance_control_kernel__legalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.40 at interval end) because the reading's own lights count as extraction only what its principle forbids: third-party costs left uncompensated after taxes and remediation, plus any tax component above true externality cost. The rising series models extraction accumulation: as the treasury plans around sin-tax revenue, rate-setting drifts from Pigouvian calibration toward revenue appetite. Suppression (0.32) is authored as a raw structural property — it is NOT scaled by power or scope; only extractiveness is scaled, by directionality and spatial scope in the engine's computation. The suppression_requirement series tracks a deliberate enforcement-history dynamic: machinery build-out in the first decade (licensing inspection, tax administration, DUI enforcement intensifying as prevalence normalized), peaking near t=16, then mild decay as compliance internalized — hence the story tracks suppression_requirement rather than leaving it static. Theater (0.25) reflects industry-funded responsibility campaigns, symbolic compliance inspections, and public-health messaging that performs more than it prevents, layered over a still-functional core. Accessibility_collapse is moderate (0.45): alternative regulatory designs, rate structures, and repeal movements remain live once the constraint is understood. Resistance (0.50) is bidirectional — prohibitionists press from one flank, tax-weary users and industry from the other, third-party advocates from a third. All three metric series run on one shared time grid (t=0..24 step 4) so every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the user seat the arrangement reads as a rope: liberty secured, taxes paid voluntarily through purchase, exit by abstention. From the payer seats the identical structure operates as imposed risk: non-consenting exposure to crash and inhalation externalities, compensated only partially and ex post. From the fiscal-authority seat the structure is both instrument and income — and the institutional-identity dynamic matters here: the agency has progressively fused with its revenue stream, so recalibration that its own principle would demand feels like self-mutilation rather than adjustment. That identity fusion, not any single actor's preference, is what makes the tax ratchet one-directional. The engine derives these divergent per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (users, industry, treasury) derive low directionality — the constraint subsidizes their liberty, market access, and revenue respectively; the treasury's dual agenda_setter/beneficiary position keeps it near the beneficiary end despite its administrative burden. Declared victims (road users, exposure bystanders) are powerless and trapped — they cannot opt out of shared roads or ambient air — placing them near the full-target end, where effective extraction is amplified. One override is declared: power_atom 'organized' at d=0.70. The derivation chain would leave illicit_market_operators near the canonical symmetric fallback because they appear in neither the beneficiary nor the victim arrays; but their actual structural position is target-of-enforcement — the regime's inspection and tax machinery exists substantially to exclude them, and they bear that enforcement directly. The override corrects the derived d upward to reflect enforced exclusion rather than symmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live and corroborated from outside the beneficiary set, so no mandatrophy is declared and none should be inferred: the arrangement has not outlived its function. The classification discipline cuts both ways here. A pure-rope reading would erase the identifiable victims — diffuse, unorganized, statistically-present third parties whose uncompensated burden flows through the very structure that liberates users; the victim declarations block that erasure. A snare reading would erase the genuine coordination function — the boundary definition solves a real collective problem and participants on the user and industry sides are net beneficiaries; the beneficiary declarations and enforcement requirement block that conflation. Theater is present but the functional core (age gating, impairment enforcement, remediation funding) is intact, blocking a piton reading. The slow danger this story is built to detect is drift, not death: if the tax ratchet continues past externality cost while the founding problem is eventually declared solved, the (status x verdict) mismatch flag would fire against a regime still collecting — the corpus's standard capture signal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the substance_control_kernel: would instantiating a sibling reading structurally change the victim set and epsilon?',
    'Author and evaluate the sibling stories: the prohibition_reading returns users to the victim set via criminal sanction (high epsilon); the harm_reduction_reading reframes users as patients and relocates intervention to treatment access. Compare computed classifications across the linked family.',
    'If the prohibition reading computes as snare while this reading computes as tangled_rope, the kernel contest is a real structural fork rather than rhetorical disagreement, and the cross-reading epsilon deltas quantify what each reading''s victory costs its losers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one of three readings of the substance-control kernel; sibling instantiation would restructure the victim set.').

omega_variable(
    externality_capture_adequacy,
    'Do current tax levels plus remediation spending actually cover the full third-party cost stream — DUI casualties, secondhand-exposure morbidity, treatment spillovers?',
    'Actuarial reconciliation, jurisdiction by jurisdiction, of earmarked revenue against independently estimated attributable third-party costs.',
    'Systematic under-capture concentrates uncompensated burden on the payer seats and pressures reclassification toward snare; systematic over-capture converts the excess into user-side extraction that violates the reading''s own axiom.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_capture_adequacy, empirical, 'Whether the regime''s externality pricing matches the externality burden it permits.').

omega_variable(
    pigouvian_vs_revenue_motive,
    'Is the observed tax ratchet calibrated to marginal externality cost or to general-fund revenue appetite?',
    'Regress successive tax increments against contemporaneous externality-cost estimates; track the earmarked share of revenue over time.',
    'A revenue-driven ratchet confirms extraction accumulation (feeding the T17 abductive hypothesis) and strengthens tangled_rope-to-snare drift; an externality-tracking ratchet is the reading functioning as designed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pigouvian_vs_revenue_motive, empirical, 'Motive ambiguity behind the rising extractiveness trajectory.').

omega_variable(
    gray_market_persistence,
    'Does illicit supply collapse after legalization, or persist in gray areas — high-tax jurisdictions, potency-capped markets, licensing-scarce regions?',
    'Longitudinal market-share surveillance comparing illicit versus licensed supply across jurisdictions with divergent tax and licensing regimes.',
    'Persistent illicit share sustains an enforced-excluded class and keeps suppression elevated above what the liberty framing acknowledges; collapse allows the suppression trajectory to decay further.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gray_market_persistence, empirical, 'Whether the excluded competitor seat dissolves or hardens.').

omega_variable(
    paternalism_boundary_creep,
    'Do public-use bans, potency caps, and flavor restrictions track demonstrated third-party harm pathways, or do they slide into user-side paternalism?',
    'Trace each restriction''s stated justification against measured third-party harm attribution; classify restrictions with no demonstrable third-party pathway as paternalistic residue.',
    'Paternalistic components quietly return users to the victim set, meaning the reading''s authored epsilon understates extraction relative to its own principle — the reading failing by its own lights rather than by its rivals''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(paternalism_boundary_creep, conceptual, 'Whether the intervention boundary is drifting back across the self-regarding line.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__legalization_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_kernel__legalization_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(subs_tr_t4, substance_control_kernel__legalization_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement(subs_tr_t8, substance_control_kernel__legalization_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(subs_tr_t12, substance_control_kernel__legalization_reading, theater_ratio, 12, 0.21).
narrative_ontology:measurement(subs_tr_t16, substance_control_kernel__legalization_reading, theater_ratio, 16, 0.23).
narrative_ontology:measurement(subs_tr_t20, substance_control_kernel__legalization_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement(subs_tr_t24, substance_control_kernel__legalization_reading, theater_ratio, 24, 0.25).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_kernel__legalization_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(subs_be_t4, substance_control_kernel__legalization_reading, base_extractiveness, 4, 0.31).
narrative_ontology:measurement(subs_be_t8, substance_control_kernel__legalization_reading, base_extractiveness, 8, 0.34).
narrative_ontology:measurement(subs_be_t12, substance_control_kernel__legalization_reading, base_extractiveness, 12, 0.36).
narrative_ontology:measurement(subs_be_t16, substance_control_kernel__legalization_reading, base_extractiveness, 16, 0.38).
narrative_ontology:measurement(subs_be_t20, substance_control_kernel__legalization_reading, base_extractiveness, 20, 0.39).
narrative_ontology:measurement(subs_be_t24, substance_control_kernel__legalization_reading, base_extractiveness, 24, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_kernel__legalization_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(subs_su_t4, substance_control_kernel__legalization_reading, suppression_requirement, 4, 0.27).
narrative_ontology:measurement(subs_su_t8, substance_control_kernel__legalization_reading, suppression_requirement, 8, 0.32).
narrative_ontology:measurement(subs_su_t12, substance_control_kernel__legalization_reading, suppression_requirement, 12, 0.35).
narrative_ontology:measurement(subs_su_t16, substance_control_kernel__legalization_reading, suppression_requirement, 16, 0.36).
narrative_ontology:measurement(subs_su_t20, substance_control_kernel__legalization_reading, suppression_requirement, 20, 0.34).
narrative_ontology:measurement(subs_su_t24, substance_control_kernel__legalization_reading, suppression_requirement, 24, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_kernel__legalization_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(substance_control_kernel__legalization_reading, substance_control_kernel__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_kernel__legalization_reading, substance_control_kernel__harm_reduction_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'substance policy' covers three structurally distinct claims and is decomposed per the epsilon-invariance principle. This story (legalization_reading) authors epsilon ~0.40 for the taxed-liberty arrangement with third-party externality victims; the prohibition_reading authors high epsilon for the punitive arrangement with users as victims; the harm_reduction_reading authors epsilon for the clinical-intervention arrangement. Prohibition sits upstream: its documented failure created the legitimacy conditions under which the other two readings became holdable. Each family member links the others via affects_constraints; no single story hedges epsilon across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_kernel__legalization_reading, organized, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
