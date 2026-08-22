% ============================================================================
% CONSTRAINT STORY: versailles_reparations_clauses__limited_responsibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Versailles Reparations — Limited Responsibility Reading (Capacity-Bounded Payments, Article 231 as Legal Formality)
 *   domain: international_relations/legal_history/political_economy
 *
 * SUMMARY:
 *   The Versailles Treaty's reparations clauses (Articles 231-247) generated
 *   three competing readings of Germany's obligation. The
 *   limited_responsibility_reading — championed by German foreign ministers
 *   (Simons, Stresemann, Curtius) and accepted in varying degrees by British
 *   and American diplomats — holds that reparations must be bounded by
 *   Germany's actual economic capacity to pay, that Article 231 ('war guilt')
 *   is a legal basis for liability not a moral condemnation, and that payment
 *   schedules must preserve German viability. This reading gained traction
 *   through the Dawes (1924) and Young (1929) Plans, culminating in the
 *   effective termination at Lausanne (1932). It is a tangled_rope: it
 *   coordinates a viable payment mechanism (rope function) while
 *   asymmetrically extracting reduced liability from Allied creditors and
 *   occupied populations (snare function), requiring active enforcement
 *   through international financial commissions and the threat of sanctions.
 *
 * KEY AGENTS:
 *   - german_elite_negotiators: Primary beneficiary (moderate/organized) — gain reduced liability and negotiating leverage
 *   - weimar_government_officials: Secondary beneficiary (institutional) — gain political breathing room
 *   - german_industrial_complex: Beneficiary (organized) — avoid confiscatory taxation and asset seizures
 *   - allied_creditor_nations: Primary victim (institutional/powerful) — receive reduced compensation for war costs
 *   - occupied_territory_populations: Victim (powerless) — bear occupation costs with diminished reparations
 *   - war_veterans_dependents_allied: Victim (powerless) — pensions and care funded by reparations shrink
 *   - reparations_commission: Agenda setter (institutional) — administers schedules, enforces compliance
 *   - us_treasury_federal_reserve: Observer/agenda setter (institutional) — mediates plans, guarantees loans
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(versailles_reparations_clauses__limited_responsibility_reading, 0.42).
domain_priors:suppression_score(versailles_reparations_clauses__limited_responsibility_reading, 0.38).
domain_priors:theater_ratio(versailles_reparations_clauses__limited_responsibility_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(versailles_reparations_clauses__limited_responsibility_reading, tangled_rope).
narrative_ontology:human_readable(versailles_reparations_clauses__limited_responsibility_reading, "Versailles Reparations — Limited Responsibility Reading (Capacity-Bounded Payments, Article 231 as Legal Formality)").
narrative_ontology:topic_domain(versailles_reparations_clauses__limited_responsibility_reading, "international_relations/legal_history/political_economy").

domain_priors:requires_active_enforcement(versailles_reparations_clauses__limited_responsibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(versailles_reparations_clauses__limited_responsibility_reading, 'd37f3d14-1a2c-4145-8d05-7d9b72730622').
narrative_ontology:cs_kernel_codification('d37f3d14-1a2c-4145-8d05-7d9b72730622', formalized).
narrative_ontology:cs_authority_grounding('d37f3d14-1a2c-4145-8d05-7d9b72730622', lineage).
narrative_ontology:cs_interpretation_layer_present('d37f3d14-1a2c-4145-8d05-7d9b72730622').
narrative_ontology:cs_reading_relation('d37f3d14-1a2c-4145-8d05-7d9b72730622', versailles_reparations_clauses__punitive_liability_reading, influences).
narrative_ontology:cs_reading_relation('d37f3d14-1a2c-4145-8d05-7d9b72730622', versailles_reparations_clauses__repudiation_reading, coexists_with).
narrative_ontology:cs_axiom('d37f3d14-1a2c-4145-8d05-7d9b72730622', foundational, reparations_bounded_by_capacity_to_pay).
narrative_ontology:cs_axiom_status(reparations_bounded_by_capacity_to_pay, holdable).
narrative_ontology:cs_axiom_grounding('d37f3d14-1a2c-4145-8d05-7d9b72730622', reparations_bounded_by_capacity_to_pay, empirically_contingent).
narrative_ontology:cs_axiom('d37f3d14-1a2c-4145-8d05-7d9b72730622', foundational, article_231_legal_basis_not_moral_condemnation).
narrative_ontology:cs_axiom_status(article_231_legal_basis_not_moral_condemnation, holdable).
narrative_ontology:cs_axiom_grounding('d37f3d14-1a2c-4145-8d05-7d9b72730622', article_231_legal_basis_not_moral_condemnation, conventional).
narrative_ontology:cs_reference_frame('d37f3d14-1a2c-4145-8d05-7d9b72730622', versailles_treaty_1919_original_schedule).
narrative_ontology:cs_drift_state('d37f3d14-1a2c-4145-8d05-7d9b72730622', lausanne_1932_effective_termination, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('d37f3d14-1a2c-4145-8d05-7d9b72730622', '').
narrative_ontology:cs_kernel_id(versailles_reparations_clauses__limited_responsibility_reading, versailles_reparations_clauses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__limited_responsibility_reading, german_elite_negotiators).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__limited_responsibility_reading, weimar_government_officials).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__limited_responsibility_reading, german_industrial_complex).
narrative_ontology:constraint_victim(versailles_reparations_clauses__limited_responsibility_reading, allied_creditor_nations).
narrative_ontology:constraint_victim(versailles_reparations_clauses__limited_responsibility_reading, occupied_territory_populations).
narrative_ontology:constraint_victim(versailles_reparations_clauses__limited_responsibility_reading, war_veterans_dependents_allied).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(versailles_reparations_clauses__limited_responsibility_reading, weimar_government_officials).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__limited_responsibility_reading, reparations_bounded_by_economic_capacity).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__limited_responsibility_reading, article_231_legal_not_moral).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__limited_responsibility_reading, viability_over_punitive_justice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% German foreign ministers and diplomats (Simons, Stresemann, Curtius, Brüning) who negotiate reparations revisions. They leverage the 'capacity to pay' argument to reduce liability from 132B to effectively ~20B gold marks paid. Their exit is constrained: they cannot repudiate without inviting occupation, but they can threaten collapse to extract concessions. They personally benefit from political survival and international recognition.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, german_elite_negotiators, beneficiary,
    moderate, biographical, constrained, national).

% Weimar chancellors, finance ministers, and cabinet officials who must implement payment schedules while maintaining domestic legitimacy. They benefit from the reading's political breathing room (avoiding hyperinflation collapse, Ruhr occupation) but also bear the cost of administering a hated system. Their exit is constrained by constitutional oath and Allied enforcement machinery.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, weimar_government_officials, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(versailles_reparations_clauses__limited_responsibility_reading, weimar_government_officials, payer).

% Heavy industry (Krupp, Thyssen, IG Farben, Siemens) and export sectors that would face confiscatory taxation or asset seizure under maximalist reparations. They benefit from capacity-bounded payments that preserve capital stock. Their exit is relatively mobile — capital can flee, production can shift — but they are nationally embedded and politically influential.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, german_industrial_complex, beneficiary,
    organized, biographical, mobile, national).

% France, Britain, Belgium, Italy, and other Allied governments that financed the war through debt and expected German reparations to service that debt. They bear the cost of reduced payments: France faces reconstruction deficits; Britain faces imperial budget strain; Italy faces unpaid war debts. Their exit is constrained by inter-Allied debt obligations and domestic politics — they cannot unilaterally forgive without triggering their own creditor demands.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, allied_creditor_nations, payer,
    institutional, generational, constrained, continental).

% Civilian populations in Rhineland, Ruhr, Saar, and Upper Silesia under Allied occupation (1918-1930). They bear occupation costs (billeting, requisitions, movement restrictions) with the expectation of reparations compensation. When payments are reduced, their suffering is uncompensated. They have no exit — they cannot leave the occupied zone, cannot influence negotiations, and have no political representation.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, occupied_territory_populations, payer,
    powerless, immediate, trapped, local).

% Widows, orphans, disabled veterans, and dependents in Allied nations whose pensions and care were budgeted against expected reparations receipts. When German payments fall short, their benefits are cut or delayed. They have no exit from the dependency and no voice in intergovernmental negotiations.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, war_veterans_dependents_allied, payer,
    powerless, biographical, trapped, national).

% The Inter-Allied Reparations Commission (chaired by French, British, Belgian, Italian, Japanese, US delegates) that sets payment schedules, monitors German compliance, and authorizes sanctions. It administers the constraint but its authority erodes over the interval as Dawes/Young Plans transfer technical control to international bankers and the Bank for International Settlements.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, reparations_commission, agenda_setter,
    institutional, generational, analytical, continental).

% US Treasury (Mellon, Ogden Mills) and Federal Reserve (Strong, Harrison) that mediate Dawes/Young Plans, organize international loans to Germany, and shape the 'capacity to pay' methodology. They are not formal Treaty parties but functionally set the agenda. They gain financial influence and European stability but risk loan losses. Their exit is analytical — they can withdraw mediation but face systemic consequences.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, us_treasury_federal_reserve, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(versailles_reparations_clauses__limited_responsibility_reading, us_treasury_federal_reserve, agenda_setter).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a viable, internationally supervised payment mechanism that prevents German economic collapse (which would yield zero payments) while providing Allied creditors with a predictable, if reduced, revenue stream. Solves the transfer problem: how to move real resources from Germany to Allies without destroying the German export capacity that generates the transfer.
% TRANSFER_FUNCTION: Moves a capped stream of gold marks / foreign currency from German taxpayers and export earnings to Allied treasuries and reconstruction agencies, at a rate calibrated to German capacity rather than Allied claims. The transfer is mediated through the Reparations Commission, the Agent General for Reparations Payments, and later the Bank for International Settlements.
% ABSENT_VOICES: Occupied territory populations (Rhineland, Ruhr, Saar) had no representation at Versailles or in the Reparations Commission. Colonial subjects of Allied empires whose resources financed the war but who received no reparations voice. German working-class taxpayers who bore the inflationary cost of reparations financing but were excluded from 'capacity' calculations (which used industrial/export metrics). Soviet Russia, excluded from Versailles, which repudiated Tsarist debts and claimed German reparations as Allied plunder.
% DISAPPEARANCE_RATIONALE: If the limited_responsibility_reading vanished overnight in 1924, the Dawes Plan would not exist — France would likely maintain Ruhr occupation, Germany would face maximalist schedules, hyperinflation might not have been stabilized, and the Weimar Republic might have collapsed earlier. If it vanished in 1932, the Lausanne Conference's effective termination would be reversed — but by then the reading had already restructured the European financial order. The world rearranged around this reading.
% FOUNDING_PROBLEM: How to extract reparations from a defeated Great Power without destroying its economy (which destroys the capacity to pay) or triggering revolutionary collapse (which destroys the political partner for payment). The 1919 Treaty assumed Germany could pay 132B gold marks; the founding problem of the limited reading is that this assumption was economically false and politically explosive.
% FOUNDING_PROBLEM_CORROBORATION: Keynes (Economic Consequences of the Peace, 1919) independently attested the 132B figure was economically impossible. Schuker (American 'Reparations' to Germany, 1988) documents that net transfers flowed TO Germany (loans > reparations paid) after 1924. Marks (The Illusion of Peace, 1976) shows French policymakers knew the maximalist schedule was unenforceable by 1922. No major historian outside the 1919 French delegation maintains the original schedule was viable.
narrative_ontology:disappearance_verdict(versailles_reparations_clauses__limited_responsibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(versailles_reparations_clauses__limited_responsibility_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(versailles_reparations_clauses__limited_responsibility_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(versailles_reparations_clauses__limited_responsibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(versailles_reparations_clauses__limited_responsibility_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(versailles_reparations_clauses__limited_responsibility_reading_tests).
:- end_tests(versailles_reparations_clauses__limited_responsibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.42 at interval end) reflects the reading's partial constraint on Allied maximalism — Germany pays significantly less than the 1921 London Schedule (132B gold marks) but more than zero. The reading extracts from Allied creditors and occupied populations by capping liability below their claimed damages. Suppression (0.38) is moderate: the reading reduces the coercive apparatus (no Ruhr occupation after 1925, sanctions threat diminishes) but still requires the Reparations Commission and international financial control. Theater ratio (0.55) is high: the 'capacity' discourse and Article 231 reinterpretation perform coordination while the functional outcome is a negotiated extraction ceiling. Accessibility collapse (0.45) reflects that alternatives (full payment, repudiation, moratorium) remain conceptually available but politically constrained. Resistance (0.52) captures French maximalist opposition, German nationalist rejection of any payment, and Allied domestic pressure.
 *
 * PERSPECTIVAL GAP:
 *   From the German elite seat, this is a rope: a genuine coordination mechanism that prevents economic collapse and enables reintegration. From the French creditor seat, it is a snare: a constraint that extracts their legitimate claims through legal reinterpretation. From the US mediator seat, it is a scaffold: a transitional arrangement meant to stabilize Europe until self-sustaining growth returns. The engine computes these seat divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   German elites (beneficiaries) face d near 0.15 — they gain negotiating leverage and reduced payments. Weimar officials (beneficiaries) face d ~0.20 — political survival depends on the reading's success. Allied creditors (victims) face d ~0.75 — they lose ~60% of claimed damages. Occupied populations (victims) face d ~0.80 — they bear occupation costs without full compensation. The Reparations Commission (agenda_setter) faces d ~0.45 — it administers a declining mechanism. US financial actors face d ~0.30 — they gain influence but risk loan exposure.
 *
 * MANDATROPHY ANALYSIS:
 *   The reading resolves the mandatrophy tension by acknowledging the coordination function (viable payments prevent collapse) while exposing the extraction function (Allied claims are capped below moral/legal entitlement). The founding problem — 'how to make Germany pay for the war without destroying it' — was live in 1919 but dead by 1932 (Germany rearmed, Allies defaulted on inter-allied debts). The reading persists as a historical precedent for 'capacity-to-pay' sovereign debt doctrine.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the limited_responsibility_reading a distinct constraint from punitive_liability_reading and repudiation_reading, or are they observables of the same underlying arrangement?',
    'Apply the ε-invariance test: if changing the reading changes the extraction metric ε, the readings instantiate different constraints. Measure extractiveness from each reading''s structural position: punitive reads high extraction from Germany; limited reads partial extraction from Allies; repudiation reads near-zero binding obligation.',
    'If distinct, each reading gets its own constraint story with independent ε, beneficiaries, victims, and classification. If unified, the kernel is one constraint with observer-relative metrics — which violates ε-invariance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the three declared readings are structurally distinct constraints or observer perspectives on one constraint.').

omega_variable(
    naturalness_of_capacity_bound,
    'Is the ''economic capacity'' bound a genuine coordination discovery (what Germany can pay without collapse) or a constructed limit negotiated by German elites to minimize liability?',
    'Compare 1921 London Schedule (132B gold marks) against contemporaneous German national income, export capacity, and reparations transfer problem analyses (Keynes, Schuker, Marks). If the bound tracks independent economic estimates, it is discovered coordination; if it tracks German counter-proposals, it is negotiated extraction limit.',
    'If discovered, the reading has stronger rope-like coordination character; if negotiated, it is a tangled_rope where German elites coordinate to extract a lower bound from Allied creditors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_of_capacity_bound, empirical, 'Whether the capacity bound reflects economic reality or elite negotiation.').

omega_variable(
    article_231_functional_role,
    'Does treating Article 231 as ''legal formality not moral judgment'' functionally reduce Allied extraction, or is it a rhetorical concession that leaves the payment machinery intact?',
    'Track whether the legal-formality framing correlates with downward payment revisions (Dawes 1924, Young 1929, Lausanne 1932) or whether payments continued on schedule despite the framing. Compare Allied domestic rhetoric vs. diplomatic practice.',
    'If rhetorical only, the theater_ratio is higher than measured; if functional, the constraint genuinely shifts toward coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_231_functional_role, empirical, 'Whether the Article 231 reinterpretation has operational effect or is performative.').

omega_variable(
    allied_creditor_coalition_stability,
    'Can Allied creditor nations maintain a unified maximalist position, or does the capacity-bound reading exploit inter-Allied divisions (French security vs. British trade vs. US loans)?',
    'Analyze inter-Allied negotiations 1919-1932: French occupation of Ruhr (1923), British refusal to support, US Dawes/Young plan mediation, Hoover moratorium (1931). If coalition fractures predictably, the reading''s extraction reduction is structural; if coalition holds, the reading is marginal.',
    'If structural, the reading''s tangled_rope character is stable; if marginal, the reading is a transient negotiating position.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(allied_creditor_coalition_stability, empirical, 'Whether Allied disunity is a structural feature the reading exploits or a contingent historical accident.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(versailles_reparations_clauses__limited_responsibility_reading, 1919, 1932).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(versailles_reparations_limited_tr_t1919, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1919, 0.3).
narrative_ontology:measurement(versailles_reparations_limited_tr_t1921, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1921, 0.42).
narrative_ontology:measurement(versailles_reparations_limited_tr_t1924, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1924, 0.5).
narrative_ontology:measurement(versailles_reparations_limited_tr_t1929, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1929, 0.55).
narrative_ontology:measurement(versailles_reparations_limited_tr_t1931, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1931, 0.6).
narrative_ontology:measurement(versailles_reparations_limited_tr_t1932, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1932, 0.55).

% Extraction over time
narrative_ontology:measurement(versailles_reparations_limited_be_t1919, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1919, 0.65).
narrative_ontology:measurement(versailles_reparations_limited_be_t1921, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1921, 0.55).
narrative_ontology:measurement(versailles_reparations_limited_be_t1924, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1924, 0.48).
narrative_ontology:measurement(versailles_reparations_limited_be_t1929, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1929, 0.44).
narrative_ontology:measurement(versailles_reparations_limited_be_t1931, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1931, 0.38).
narrative_ontology:measurement(versailles_reparations_limited_be_t1932, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1932, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(versailles_reparations_limited_su_t1919, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1919, 0.55).
narrative_ontology:measurement(versailles_reparations_limited_su_t1921, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1921, 0.48).
narrative_ontology:measurement(versailles_reparations_limited_su_t1924, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1924, 0.35).
narrative_ontology:measurement(versailles_reparations_limited_su_t1929, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1929, 0.32).
narrative_ontology:measurement(versailles_reparations_limited_su_t1931, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1931, 0.28).
narrative_ontology:measurement(versailles_reparations_limited_su_t1932, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1932, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(versailles_reparations_clauses__limited_responsibility_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(versailles_reparations_clauses__limited_responsibility_reading, 0.12).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, versailles_reparations_clauses__punitive_liability_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, versailles_reparations_clauses__repudiation_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, dawes_plan_1924).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, young_plan_1929).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, inter_allied_debt_settlements).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, weimar_stabilization_1923_24).

% DUAL FORMULATION NOTE:
% The versailles_reparations_clauses kernel decomposes into three constraint stories: punitive_liability_reading (high extraction from Germany, snare/tangled_rope), limited_responsibility_reading (moderate extraction from Allies, tangled_rope), and repudiation_reading (near-zero binding obligation, piton/snare hybrid). The limited reading structurally influences the punitive reading by providing the negotiated alternative that Dawes/Young Plans instantiate, and influences the repudiation reading by offering the 'responsible' middle ground that makes repudiation politically costly. All three share the same Treaty text but instantiate different constraints per ε-invariance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(versailles_reparations_clauses__limited_responsibility_reading, institutional, 0.3).
constraint_indexing:directionality_override(versailles_reparations_clauses__limited_responsibility_reading, powerful, 0.75).
constraint_indexing:directionality_override(versailles_reparations_clauses__limited_responsibility_reading, moderate, 0.15).
constraint_indexing:directionality_override(versailles_reparations_clauses__limited_responsibility_reading, powerless, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
