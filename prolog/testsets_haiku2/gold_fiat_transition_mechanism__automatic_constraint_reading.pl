% ============================================================================
% CONSTRAINT STORY: gold_fiat_transition_mechanism__automatic_constraint_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gold_fiat_transition_mechanism__automatic_constraint_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gold_fiat_transition_mechanism__automatic_constraint_reading
 *   human_readable: Fiat Currency Discretion: Automatic Constraint Elimination (Automatic Constraint Reading)
 *   domain: monetary/political economy
 *
 * SUMMARY:
 *   Between 1944 (Bretton Woods) and 1971 (Nixon Shock), the international
 *   monetary system transitioned from a gold-standard regime where central
 *   bank money creation was automatically constrained by gold reserves, to a
 *   fiat regime where central banks have discretionary control over the money
 *   supply. This reading frames the transition as the replacement of one
 *   constraint type with another: the elimination of a material/physical
 *   constraint (you cannot create more money than you have gold) and its
 *   replacement with an institutional constraint (legal tender laws, central
 *   bank mandate, inflation credibility). The transition is presented as
 *   mechanistic from this seat: gold constrained, fiat enables discretion.
 *   This reading does NOT claim the transition was inevitable (that is the
 *   composite overdetermination reading) or that its primary function was to
 *   eliminate creditor discipline power (that is the creditor discipline
 *   reading). Rather, the automatic constraint reading says: the constraint
 *   that existed (gold reserve limit) was a material fact; the constraint
 *   that replaced it (discretionary central bank authority) is an
 *   institutional fact grounded in law and credibility, not physics. Both
 *   constrain money creation; they constrain it differently.
 *
 * KEY AGENTS:
 *   - Monetary authorities (central banks, treasuries): gained discretionary control; agenda-setter role; previously bound by physical fact, now administering institutional regime
 *   - Creditor class (foreign governments, bondholders, reserve accumulators): lost automatic protection from debasement; payer role; previously could force discipline through redemption threat
 *   - Domestic debtors (governments, borrowers): gained fiscal flexibility; beneficiary role; previously limited by reserve constraint
 *   - Labor and wage earners: dual position; benefit from employment accommodation but bear inflation risk; payer when debasement exceeds wage growth
 *   - Academic economists: observer role; divided on whether this reading is accurate or whether composite/creditor readings better explain the transition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.68).
domain_priors:suppression_score(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.45).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__automatic_constraint_reading, tangled_rope).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__automatic_constraint_reading, "Fiat Currency Discretion: Automatic Constraint Elimination (Automatic Constraint Reading)").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__automatic_constraint_reading, "monetary/political economy").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__automatic_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__automatic_constraint_reading, '9cc0ad21-7f99-4c2c-b231-d4136c65e842').
narrative_ontology:cs_kernel_codification('9cc0ad21-7f99-4c2c-b231-d4136c65e842', formalized).
narrative_ontology:cs_authority_grounding('9cc0ad21-7f99-4c2c-b231-d4136c65e842', extraction).
narrative_ontology:cs_interpretation_layer_present('9cc0ad21-7f99-4c2c-b231-d4136c65e842').
narrative_ontology:cs_reading_relation('9cc0ad21-7f99-4c2c-b231-d4136c65e842', gold_fiat_transition_mechanism__creditor_discipline_reading, coexists_with).
narrative_ontology:cs_reading_relation('9cc0ad21-7f99-4c2c-b231-d4136c65e842', gold_fiat_transition_mechanism__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('9cc0ad21-7f99-4c2c-b231-d4136c65e842', foundational, material_constraint_substitution).
narrative_ontology:cs_axiom_status(material_constraint_substitution, holdable).
narrative_ontology:cs_axiom_grounding('9cc0ad21-7f99-4c2c-b231-d4136c65e842', material_constraint_substitution, deontological).
narrative_ontology:cs_axiom('9cc0ad21-7f99-4c2c-b231-d4136c65e842', secondary, institutional_constraint_requires_credibility).
narrative_ontology:cs_axiom_status(institutional_constraint_requires_credibility, holdable).
narrative_ontology:cs_axiom_grounding('9cc0ad21-7f99-4c2c-b231-d4136c65e842', institutional_constraint_requires_credibility, empirically_contingent).
narrative_ontology:cs_reference_frame('9cc0ad21-7f99-4c2c-b231-d4136c65e842', automatic_physical_constraint).
narrative_ontology:cs_drift_state('9cc0ad21-7f99-4c2c-b231-d4136c65e842', mature_fiat_regime, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9cc0ad21-7f99-4c2c-b231-d4136c65e842', '').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__automatic_constraint_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_authorities).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__automatic_constraint_reading, creditor_class).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__automatic_constraint_reading, domestic_debtors).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__automatic_constraint_reading, labor_and_wage_earners).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__automatic_constraint_reading, labor_and_wage_earners).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__automatic_constraint_reading, foreign_reserve_accumulators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Central banks and treasury departments set monetary policy and administer the fiat regime. They exercise discretionary control over money supply through interest rates, reserve requirements, and open-market operations. They justify discretion as essential for managing business cycles, fighting deflation, and responding to crises. They maintain the legal-tender system that backs fiat and continuously defend central-bank independence from political pressure to inflate.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Creditors (foreign governments, international investors, bond holders, reserve accumulators) hold nominally-denominated claims. Under gold standard, they had automatic protection: if debtor inflated, capital would flee, reserves would drain, and the debtor would be forced to honor the peg or face collapse. Under fiat, they hold claims that can be eroded in real value by monetary expansion without automatic discipline kicking in. They bear long-term purchasing-power risk. Their exit options are constrained because currencies are necessary for trade and portfolio diversification across fiat currencies offers only partial escape.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, creditor_class, payer,
    powerful, generational, constrained, global).

% Governments and domestic borrowers gain fiscal flexibility under fiat. They can borrow and spend without the hard constraint of gold-reserve depletion forcing them to adjust. Monetary authorities can accommodate their borrowing through money creation. The debt burden in real terms can be eroded through controlled inflation. They have mobility because they can raise taxes, restructure spending, or default (though default carries costs).
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, domestic_debtors, beneficiary,
    institutional, generational, mobile, national).

% Workers benefit from full-employment bias of fiat policy: monetary authorities are more willing to accommodate inflation to prevent unemployment. They also bear inflation risk if wage growth lags behind money creation. The purchasing power of savings erodes if inflation is not matched by wage increases. Exit is constrained because employment is location and currency-zone dependent.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, labor_and_wage_earners, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__automatic_constraint_reading, labor_and_wage_earners, beneficiary).

% Central banks and governments holding large foreign-currency reserves (especially US dollars post-WWII) face currency depreciation risk. The transition undermined the gold-backing assumption that made reserve accumulation rational. They cannot redeem reserves for gold at a fixed parity. Their only exit is to diversify into non-reserve currencies or real assets, which destabilizes the entire reserve system.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, foreign_reserve_accumulators, payer,
    powerful, generational, constrained, global).

% Economists analyze the transition and debate its causes, consequences, and implications. Different schools (Keynesian, monetarist, Austrian, MMT) read the mechanism differently. Keynesians emphasize the removal of an irrational constraint on counter-cyclical policy. Monetarists worry about discretion causing inflation. Austrians oppose discretion on principle. Their analyses shape policy debate but they do not set the constraint directly.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, academic_economists, observer,
    institutional, generational, analytical, global).

% Gold miners and commodity interests would have benefited from continued gold-standard constraint (consistent demand, price floor). They were excluded from the transition decision and lost institutional leverage once fiat replaced commodity backing. They periodically lobby for gold-standard restoration but lack political power to force the issue.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, gold_mining_interests, excluded,
    moderate, biographical, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_authorities).
narrative_ontology:fixing_cost_class(gold_fiat_transition_mechanism__automatic_constraint_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Money requires a stable unit of account and medium of exchange. The original gold-standard constraint solved this by tying money supply to a scarce commodity: no one could arbitrarily increase the money supply without physical gold, so everyone knew the currency's value was anchored to a material fact. The fiat constraint solves the same coordination problem differently: instead of material fact, it relies on institutional commitment (central bank independence, inflation targeting, legal tender law). Both solve the problem of preventing unlimited debasement; they do so through different mechanisms.
% TRANSFER_FUNCTION: Moves seigniorage (the real purchasing power from new money creation) from those holding existing money (creditors) to those spending new money first (government, debtors). Under gold, seigniorage was bounded by mint capacity and new gold mining; excess creation triggered redemption demand and reserve loss. Under fiat, seigniorage is potentially unbounded and flows to monetary authorities and their preferred borrowers. Creditors experience erosion of real value in their nominally-denominated holdings.
% ABSENT_VOICES: Gold-mining interests and commodity traders had structural interest in gold backing and were excluded from regime-design decisions. Wage earners and savers who would bear long-term inflation risk were not represented in central-bank policy forums. Future creditors (not yet born) could not voice consequences to their future holdings. Future inflation victims in high-inflation periods (late 1970s, 2020s) did not participate in the initial transition decision. These absences are structural: monetary policy is set by appointed central bankers and elected treasuries, not by workers or savers directly.
% DISAPPEARANCE_RATIONALE: If fiat discretion disappeared tomorrow and gold standard automatically returned: monetary authorities would lose control over money supply and inflation targets; deficits would trigger immediate gold-reserve drain and currency crisis; credit would contract sharply; economies would reorganize around external constraint rather than internal policy. Governments could not run persistent deficits; counter-cyclical policy would be impossible; the institutional architecture of modern central banking would become non-functional. The fiscal and monetary system would rearrange fundamentally.
% FOUNDING_PROBLEM: Under gold standard, the supply of monetary gold was too slow relative to economic growth. Periodic gold shortages constrained credit creation, amplified business cycles, and made counter-cyclical policy impossible. The Great Depression was exacerbated by gold-standard discipline preventing monetary accommodation. Central banks sought to escape the material constraint so they could manage money supply responsively.
% FOUNDING_PROBLEM_CORROBORATION: Monetary authorities and Keynesian economists (outside the beneficiary institutions, but aligned with their interests) attest the founding problem was live and severe. Hard-money and creditor-class economists dispute the framing: they argue the problem was not constraint itself but over-spending by debtors using gold-standard evasion techniques (Bretton Woods peg-gaming, capital controls), or that the real problem was creditors needed subordination and constraint-elimination achieved that. Historians and political economists outside the monetary-authority consensus note the transition faced substantial intellectual resistance (Hayek, von Mises, the gold lobby) and that 'the problem' was differently framed by different interests — no neutral corroborating consensus exists.
narrative_ontology:disappearance_verdict(gold_fiat_transition_mechanism__automatic_constraint_reading, world_rearranges).
narrative_ontology:founding_problem_status(gold_fiat_transition_mechanism__automatic_constraint_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__automatic_constraint_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gold_fiat_transition_mechanism__automatic_constraint_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gold_fiat_transition_mechanism__automatic_constraint_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gold_fiat_transition_mechanism__automatic_constraint_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gold_fiat_transition_mechanism__automatic_constraint_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.35 at time 0 (immediately post-transition, when central banks had not yet exercised full discretion) to 0.68 at interval end (mature fiat regime, seigniorage flowing steadily to debtors and monetary authorities). This trajectory reflects the constraint's increasing effectiveness at capturing value from creditors over time — extractiveness does NOT measure whether the constraint is 'good' or 'necessary', only the degree to which it transfers resources from identified victims to identified beneficiaries. Suppression is moderate (0.45 at end) because maintaining fiat discretion requires legal-tender enforcement and inflation credibility, but does not require the active coercion that a snare would need — creditors are constrained by the system's structural position (they must hold the currency for trade) rather than by overt force. Theater is low (0.22) because the discretionary constraint operates functionally: interest rates, reserve requirements, and open-market operations are the real tools; the narrative around 'central bank independence' and 'inflation targeting' are institutional legitimacy claims, not covers for atrophied function. The measurement grid uses one shared time axis (0, 5, 10, 20, 35, 50), allowing the temporal analysis to show how extractiveness accelerated in the first decade, then plateaued as the regime matured.
 *
 * PERSPECTIVAL GAP:
 *   From the monetary authorities' seat: the transition solved a real coordination problem (money supply needed flexibility), eliminated a material bottleneck (gold mining was too slow), and enabled counter-cyclical policy (impossible under gold). From the creditor seat: the transition was a unilateral rewriting of the rules by the issuer to escape discipline, transferring real value from creditors to debtors indefinitely. Both seats are looking at the same constraint, but the directionality is opposite — authorities at d ≈ 0.2 (beneficiary), creditors at d ≈ 0.8 (target). The engine computes these divergent classifications from the structural data (beneficiary/victim declarations + exit options). The seated divergence is the point: the same institutional arrangement produces opposite type classifications because the positional facts differ. This reading supplies that data; the engine measures the gap.
 *
 * DIRECTIONALITY LOGIC:
 *   Monetary authorities benefit from the constraint (gained discretion they exercise continuously for policy goals). They set the regime and administer it — d near 0.0 (beneficiary end). Creditors lose automatic protection and bear debasement risk — d near 1.0 (target end). Their exit is constrained: they cannot redeem gold (redemption promise ended) and cannot credibly threaten capital flight without losing trade access. Wage earners are dual: benefit from full-employment bias of fiat policy (labor demand rises when money accommodates growth), but bear inflation if debasement exceeds wage indexation — d near 0.5 symmetric, or slightly higher if their labor-bargaining power is weak. Domestic debtors benefit (fiscal flexibility, inflation erodes debt value in real terms) — d near 0.0. The overrides are not needed here; the structural derivation from victim/beneficiary + exit yields appropriate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (gold constraint amplifies cycles, prevents counter-cyclical policy) is live from the monetary authority seat and contested from the creditor seat. A creditor-seat economist would argue the founding problem is misframed: the real problem was not gold constraint but creditors having too much discipline power (creditor discipline reading). The constraint is not a zombie — it remains actively functional. Monetary authorities continuously exercise discretion; the regime requires ongoing institutional maintenance (central bank independence, inflation credibility, legal-tender enforcement). Mandatrophy would apply if the constraint persisted despite having zero functional justification remaining — if monetary authorities maintained discretion while the original counter-cyclical rationale had been superseded by, say, perfectly functioning labor markets that self-stabilize (which has not occurred). The transition itself shows no sign of mandatrophy: it remains contested and actively defended by its beneficiaries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constraint_type_vs_constraint_function,
    'Does the transition represent a swap of constraint types (physical→institutional) while the coordination function remained constant? Or did the coordination function itself fundamentally shift, such that what is being constrained changed?',
    'Comparative analysis: what coordination problems does fiat discretion solve that gold standard could not? Conversely, what coordination problems did gold standard solve that fiat cannot address? If the problem set is identical (both prevent unlimited debasement, both enable trade settlement), then constraint-type replacement is the right frame. If the problem set shifted (gold constrained to enable international credibility; fiat enables domestic flexibility), then the mechanism is a functional shift, not a type swap.',
    'If constraint-type replacement, the automatic constraint reading holds: ε and beneficiary structure follow from the discretion gained. If functional shift, the creditor discipline or composite overdetermination readings better explain causation and beneficiary structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constraint_type_vs_constraint_function, conceptual, 'Whether the constraint mechanism changed type (physical→institutional) or function (international credibility→domestic flexibility).').

omega_variable(
    inevitability_vs_agency,
    'Was the transition caused by mounting pressure from the automatic constraint (gold supply insufficient for growth) such that it was structurally inevitable? Or was it a policy choice by monetary authorities with agency to continue the old regime?',
    'Counterfactual historical analysis: were there technical options for gold standard continuation (e.g., higher official gold price, gold-backed eurodollar system, gold pool expansion) that were foreclosed by independent structural changes, or were they foreclosed by policy choice? If technical options remained live but were rejected, agency dominates inevitability.',
    'High inevitability would support the automatic constraint reading (the constraint eliminated itself via its own structural limit). High agency would support the composite overdetermination or creditor discipline readings (the transition was a choice, and the question is what motivated the choice — which reading of the causal chain is accurate).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(inevitability_vs_agency, conceptual, 'Whether transition was structurally inevitable or a contingent policy choice.').

omega_variable(
    creditor_discipline_causal_weight,
    'How much of the observed extraction (ε=0.68) is attributable to the loss of automatic creditor discipline, versus how much is attributable to the expansion of discretionary monetary accommodation that was impossible under gold?',
    'Decomposition analysis: separate seigniorage gains (who gains from money creation) from discipline losses (who loses automatic constraint). In a purely constraint-replacement scenario, seigniorage gains and discipline losses should move together. If they diverge (e.g., seigniorage captured by debtors but discipline loss primarily benefits monetary authorities), then the readings are measuring different mechanisms.',
    'If discipline loss dominates, creditor discipline reading (this constraint is about creditor veto elimination) is more accurate. If seigniorage and discretion dominate, automatic constraint reading (this constraint is about discretionary authority gaining control) is more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creditor_discipline_causal_weight, empirical, 'Causal decomposition: creditor discipline versus discretionary accommodation in the mechanism.').

omega_variable(
    institutional_constraint_stability,
    'Is the fiat regime''s constraint on money creation (legal tender law + central bank credibility) genuinely equivalent to gold''s constraint in stability and automaticity, or is it fundamentally weaker and requires continuous institutional maintenance?',
    'Comparative institutional analysis: gold constraint operated through brute fact (you cannot pay in gold you do not have). Fiat constraint operates through credibility (investors believe inflation will be controlled). What happens to the fiat constraint if credibility breaks? Historical evidence: 1970s-80s high inflation episodes tested this. Did the constraint re-establish or did it require institutional reform (Volcker era policy shift)?',
    'If fiat constraint is genuinely equivalent to physical constraint in stability, the automatic constraint reading holds — one constraint replaced another. If fiat constraint is substantially weaker and contingent on maintained credibility, then the mechanism is not constraint replacement but delegation to an institutional agent (central bank) whose discipline is not automatic. This would shift weight toward composite overdetermination (multiple independent institutional reforms were necessary) or creditor discipline readings (the constraint was weakened to weaken creditor power).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_constraint_stability, conceptual, 'Whether fiat constraint is materially equivalent to gold constraint or fundamentally weaker.').

omega_variable(
    kernel_reading_contest_witness,
    'Which reading of the gold-fiat transition kernel is most accurate: automatic constraint replacement, creditor discipline elimination, or composite overdetermination?',
    'This omega documents the irreducible contestation internal to the kernel. No single reading can be established as uniquely correct from structural analysis alone, because the transition was causally complex and different framings highlight different causal chains. The engine will compute this constraint (automatic constraint reading) as a distinct structure; sibling readings will compile to separate constraint stories with different ε, beneficiary/victim structures, and CS axioms. Comparative analysis across the three computed structures will reveal which framings the corpus data supports.',
    'This omega feeds the corpus-level mandatrophy and false-summit analysis: if readings that conflict dramatically all compute as live and stable constraints, the kernel is genuinely multivalent. If one reading dominates the others (lower theater, higher consensus corroboration), that reading is the truer frame.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_witness, conceptual, 'Kernel reading contest: which causal framing of the transition is structurally most accurate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__automatic_constraint_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gold_tr_t0, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(gold_tr_t5, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(gold_tr_t10, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement(gold_tr_t20, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(gold_tr_t35, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 35, 0.21).
narrative_ontology:measurement(gold_tr_t50, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 50, 0.22).

% Extraction over time
narrative_ontology:measurement(gold_be_t0, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(gold_be_t5, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(gold_be_t10, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(gold_be_t20, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(gold_be_t35, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 35, 0.65).
narrative_ontology:measurement(gold_be_t50, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(gold_su_t0, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(gold_su_t5, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 5, 0.32).
narrative_ontology:measurement(gold_su_t10, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 10, 0.37).
narrative_ontology:measurement(gold_su_t20, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(gold_su_t35, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 35, 0.44).
narrative_ontology:measurement(gold_su_t50, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 50, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fiat_transition_mechanism__automatic_constraint_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.12).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__automatic_constraint_reading, gold_fiat_transition_mechanism__creditor_discipline_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__automatic_constraint_reading, gold_fiat_transition_mechanism__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the gold-fiat transition kernel. The automatic constraint reading frames the mechanism as the elimination of one constraint type (physical/automatic, enforced by gold reserve limits) and its replacement with another (institutional/discretionary, enforced by law and credibility). Sibling readings (creditor_discipline_reading and composite_overdetermination_reading) instantiate the same historical event as different constraints because they identify different causal structures and beneficiary/victim relationships. All three stories share the same referent (the transition itself) but differ in what they identify as THE constraint operating within that transition. The automatic constraint reading emphasizes constraint-type replacement; the creditor discipline reading emphasizes power redistribution from creditors to debtors; the composite reading emphasizes overdetermined systems change. Comparison across the three computed structures will reveal structural ambiguities in the historical event itself — whether it was driven by automatic constraint failure, power contestation, or systems convergence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
