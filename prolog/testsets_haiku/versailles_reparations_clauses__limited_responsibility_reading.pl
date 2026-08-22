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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: versailles_reparations_clauses__limited_responsibility_reading
 *   human_readable: Versailles Reparations Clauses (Limited Responsibility Reading)
 *   domain: international_relations/legal_history
 *
 * SUMMARY:
 *   The Treaty of Versailles imposes reparation payments on Germany. The
 *   punitive reading interprets Article 231 ('responsibility clause') as
 *   grounds for quasi-unlimited extraction: Germany bears total moral and
 *   financial liability for war costs. The repudiation reading rejects the
 *   entire treaty framework as coercive. The limited-responsibility reading
 *   (instantiated here) claims Article 231 is a legal formality neutral on
 *   causation or moral judgment; reparations payments are justified but must
 *   be bounded by German economic capacity to pay — a constraint that
 *   privileges viability over maximalist extraction. This reading gained
 *   traction in the mid-1920s (Dawes Plan renegotiations) when Allied
 *   creditors themselves recognized that Germany's fiscal exhaustion
 *   threatened their own recovery. From the limited-responsibility
 *   perspective, the constraint solves a genuine coordination problem: it
 *   acknowledges Germany's obligation while preventing fiscal collapse that
 *   would render payments impossible. From the punitive reading, it is a
 *   cover story for insufficient extraction and German elite leverage over
 *   Allied creditors. From the repudiation reading, it legitimizes an
 *   illegitimate coercive framework by accepting any part of it. The story's
 *   claim (tangled_rope) and metrics (0.62 extractiveness, 0.48 suppression
 *   declining over interval) reflect the reading's own characterization:
 *   coordination function (obligation + capacity-bounding = stabilized
 *   reparations schedule) plus asymmetric extraction (German population bears
 *   fiscal burden; Allied creditors retain discretion to renegotiate
 *   downward). The metric trajectory (extractiveness declining, suppression
 *   flat then declining) models the historical Dawes Plan renegotiation
 *   process: initial maximal extraction (t=0, ε=0.78) moderated by
 *   recognition of German incapacity, converging on a revised but still
 *   extractive schedule (t=10, ε=0.62).
 *
 * KEY AGENTS:
 *   - German government & population: bound by treaty obligation, constrained by fiscal capacity — primary targets of extraction and locus of the capacity-bounding principle
 *   - Allied creditor governments (UK, France, US): claim reparations entitlement; negotiate downward when faced with German exhaustion — beneficiaries with power to revise terms
 *   - Occupied territories (Belgium, France): suffered direct territorial/civilian harm; reduced reparations due to capacity-bounding may shift their victim status from war-harmed to constraint-harmed
 *   - Treaty framers & enforcement bodies (Reparations Commission): set schedules, monitor capacity, adjust terms — agenda-setters wielding discretion over 'viability' definition
 *   - Analytical observer (legal/economic historians): assess whether Article 231 is truly capacity-neutral or carries latent liability implications
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(versailles_reparations_clauses__limited_responsibility_reading, 0.62).
domain_priors:suppression_score(versailles_reparations_clauses__limited_responsibility_reading, 0.48).
domain_priors:theater_ratio(versailles_reparations_clauses__limited_responsibility_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(versailles_reparations_clauses__limited_responsibility_reading, tangled_rope).
narrative_ontology:human_readable(versailles_reparations_clauses__limited_responsibility_reading, "Versailles Reparations Clauses (Limited Responsibility Reading)").
narrative_ontology:topic_domain(versailles_reparations_clauses__limited_responsibility_reading, "international_relations/legal_history").

domain_priors:requires_active_enforcement(versailles_reparations_clauses__limited_responsibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(versailles_reparations_clauses__limited_responsibility_reading, '51c032ad-2940-4a63-88fa-94178493aa98').
narrative_ontology:cs_kernel_codification('51c032ad-2940-4a63-88fa-94178493aa98', fixed_text).
narrative_ontology:cs_authority_grounding('51c032ad-2940-4a63-88fa-94178493aa98', lineage).
narrative_ontology:cs_interpretation_layer_present('51c032ad-2940-4a63-88fa-94178493aa98').
narrative_ontology:cs_reading_relation('51c032ad-2940-4a63-88fa-94178493aa98', versailles_reparations_clauses__punitive_liability_reading, coexists_with).
narrative_ontology:cs_reading_relation('51c032ad-2940-4a63-88fa-94178493aa98', versailles_reparations_clauses__repudiation_reading, coexists_with).
narrative_ontology:cs_axiom('51c032ad-2940-4a63-88fa-94178493aa98', foundational, article_231_is_procedure_neutral).
narrative_ontology:cs_axiom_status(article_231_is_procedure_neutral, holdable).
narrative_ontology:cs_axiom_grounding('51c032ad-2940-4a63-88fa-94178493aa98', article_231_is_procedure_neutral, conventional).
narrative_ontology:cs_axiom('51c032ad-2940-4a63-88fa-94178493aa98', foundational, reparations_bounded_by_payment_capacity).
narrative_ontology:cs_axiom_status(reparations_bounded_by_payment_capacity, holdable).
narrative_ontology:cs_axiom_grounding('51c032ad-2940-4a63-88fa-94178493aa98', reparations_bounded_by_payment_capacity, empirically_contingent).
narrative_ontology:cs_reference_frame('51c032ad-2940-4a63-88fa-94178493aa98', capacity_constrained_reparations).
narrative_ontology:cs_drift_state('51c032ad-2940-4a63-88fa-94178493aa98', post_dawes_plan_renegotiation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('51c032ad-2940-4a63-88fa-94178493aa98', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(versailles_reparations_clauses__limited_responsibility_reading, versailles_reparations_clauses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__limited_responsibility_reading, allied_creditor_governments).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__limited_responsibility_reading, occupied_territory_restoration).
narrative_ontology:constraint_victim(versailles_reparations_clauses__limited_responsibility_reading, german_population).
narrative_ontology:constraint_victim(versailles_reparations_clauses__limited_responsibility_reading, german_fiscal_capacity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(versailles_reparations_clauses__limited_responsibility_reading, german_government).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bound by treaty signature to pay reparations; constrained by fiscal capacity and domestic political legitimacy. Must accept revised payment schedules negotiated via the Reparations Commission and implement budget discipline to meet obligations. Cannot exit the payment framework without treaty repudiation (identity-locked to state sovereignty and international treaty system). Capacity-bounding principle is experienced by this seat as both constraint (payment obligation persists) and negotiating leverage (can credibly claim incapacity to force renegotiation downward).
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, german_government, payer,
    institutional, generational, identity_locked, national).

% Bears the fiscal burden of reparations through taxation, inflation, reduced public services, and constrained economic growth. Not consulted in treaty negotiations or Reparations Commission decisions. Their resistance (hyperinflation, reparations strikes, political radicalization) is substantial but structurally constrained — the payment obligation is externally enforced. Capacity-bounding principle means their tax burden remains high (to establish German capacity) but is theoretically more modest than under maximal punitive extraction. Actual experiences vary by class: working class and farmers bear higher proportional burden; industrial elites can offset via export competitiveness.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, german_population, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(versailles_reparations_clauses__limited_responsibility_reading, german_population, excluded).

% Claim reparations entitlement as compensation for war damages and war costs. Set initial maximalist demands (e.g., France's initial claims); renegotiate downward via Reparations Commission when faced with German incapacity and the recognition that excessive extraction prevents payment. Benefit from the constraint (receive reparations flows) but sacrifice maximal extraction for enforceability. Power is substantial (military occupation, asset seizure capability) but moderated by the coordination problem: continued coercion past German capacity leads to zero reparations. Capacity-bounding principle is experienced by this seat as pragmatic adjustment to economic reality and as negotiating framework for inter-allied disputes over burden-sharing.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, allied_creditor_governments, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(versailles_reparations_clauses__limited_responsibility_reading, allied_creditor_governments, agenda_setter).

% Suffered direct territorial invasion, infrastructure destruction, and civilian casualties. Have legitimate claims for reconstruction compensation. Receive reduced reparation flows due to capacity-bounding principle (actual payments are lower than damage-based claims would justify). Not a direct party to payment negotiations; their interests are represented by Allied creditor governments, which privilege reparation distribution according to war costs and creditor-nation interests rather than damage-proportionality. Capacity-bounding shifts the victim status of this seat: they are war-harmed (primary), but also constraint-harmed (secondary, via reduced compensation).
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, occupied_territories_belgium_france, excluded,
    moderate, generational, constrained, regional).

% Technical body appointed by Allied governments to assess German capacity, set payment schedules, and monitor implementation. Holds discretion over what constitutes 'economic viability' — this discretion is the operational implementation of capacity-bounding principle. Charged with balancing maximalist extraction against enforceability. Experiences the constraint as governance problem: high enough extraction to satisfy punitive reading's constituency, low enough to prevent German collapse. The seat is analytical in scope but institutional in power — decisions are enforced via occupation and asset seizure.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, reparations_commission, agenda_setter,
    institutional, generational, analytical, global).

% German nationalist, communist, and right-wing political movements that reject the treaty framework entirely (repudiation reading). Argue that the constraint is illegitimate coercion masked by legalese, that capacity-bounding is a sham that perpetuates extraction, and that Germany has no binding obligation. Hold live political voice within Germany but are excluded from treaty negotiations and Reparations Commission decisions. Their exclusion is structural to the constraint's operation — if they gained power, they would attempt to overturn the entire framework.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, treaty_repudiation_factions, excluded,
    moderate, biographical, constrained, national).

% Allied hardliners (particularly French military/nationalist factions) who argue for maximal extraction grounded in German guilt and responsibility. Hold that Article 231 carries unlimited liability implications and that capacity-bounding is insufficient punishment. Their exclusion from effective policy (via Reparations Commission drift toward limited-responsibility reading) represents the triumph of pragmatism over maximalism. The capacity-bounding principle structurally marginalizes this reading, though it remains live within some Allied constituencies.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, punitive_liability_advocates, excluded,
    moderate, biographical, constrained, regional).

% Economists and historians who assess the reparations constraint from outside the directly affected seats. Examine whether capacity-bounding is workable, whether Article 231's interpretation as 'legal formality not moral judgment' is coherent, and whether the constraint's operation matches its justification. Their role is diagnostic: to detect whether the constraint operates as advertised (stabilized reparations via capacity-respect) or as cover story (extraction masked by legality language).
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, economic_analysts_historical_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(versailles_reparations_clauses__limited_responsibility_reading, allied_creditor_governments).
narrative_ontology:fixing_cost_class(versailles_reparations_clauses__limited_responsibility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable, negotiated reparations payment schedule that allows Germany to service its war-damage obligations while maintaining fiscal viability — solving the collective-action problem of Allied creditors trying to extract maximum reparations from a debtor whose incapacity would render payments impossible. The coordination premise is that a revised but enforceable schedule (capacity-bounded) produces more total reparations revenue than maximal extraction that collapses German fiscal capacity.
% TRANSFER_FUNCTION: Moves reparations payments (money, assets, industrial goods, labor services) from Germany to Allied creditor governments and occupied-territory reconstruction funds. The limited-responsibility reading frames the transfer as compensation for war damages and war costs, calibrated to German economic capacity rather than to maximalist liability. Transfers decline in scale over the measurement interval as capacity-bounding renegotiations (Dawes Plan, 1924) reduce the absolute payment obligations from initial maximal claims.
% ABSENT_VOICES: German population has no formal representation in treaty negotiations or Reparations Commission deliberations; their resistance is registered only as compliance/non-compliance with imposed schedules. Treaty repudiation factions (German nationalists, communists) are excluded from official negotiations — their objection to the entire framework is not heard in deliberative settings. Occupied Belgium and France are represented by Allied creditor governments rather than directly — their distinct interests (reconstruction priority vs. war-cost recovery) are subordinated to inter-allied burden-sharing logic. Independent economists and legal scholars who question whether the capacity-bounding principle is coherently defined are consulted but not decision-makers.
% DISAPPEARANCE_RATIONALE: If the reparations constraint vanished overnight, Germany's fiscal position would improve substantially (no payment obligation), but occupied territories would lose reconstruction compensation, Allied governments would forgo reparations revenue (unless they pursued alternative extraction), and the international legal framework would be destabilized (treaty repudiation sets precedent for other obligations). The world rearranges because the constraint is the primary mechanism through which war damages are being compensated and Allied war costs are being recovered — its absence would require alternative institutional arrangements or acceptance of uncompensated loss.
% FOUNDING_PROBLEM: War damages: Belgium and France suffered territorial invasion, infrastructure destruction, and civilian casualties requiring reconstruction. War costs: Allied governments mobilized massive military resources, sustained casualties, and faced post-war debt — they claim reparations to offset these costs. The founding problem is asymmetric: occupied territories need compensation for direct harm; Allied creditors need compensation for war-mobilization costs. The reparations constraint attempts to solve both via transfer from Germany, bounded by Germany's capacity to pay.
% FOUNDING_PROBLEM_CORROBORATION: War damages in Belgium and France are historically verified: infrastructure destruction, agricultural/industrial loss, civilian casualties. This is corroborated by independent historical documentation (Red Cross records, League of Nations assessments, reconstruction records) outside the benefiting parties' claims. Allied war-cost claims are corroborated by government budget records and military expenditure documentation, though what constitutes 'recoverable' vs. 'political/inherent' war cost remains contested (i.e., do Allied governments legitimately claim repayment for baseline military expenditure, or only exceptional war-mobilization costs?). The founding problem's persistence over the 1919-1924 interval is corroborated by continued discussion of capacity-bounding renegotiations, indicating both parties acknowledged the underlying claims remained live. However, by the mid-1920s, observers outside both benefiting parties (economists, legal scholars) increasingly questioned whether the payment schedule was proportional to either stated founding problem — suggesting the founding problem's status began shifting toward 'contested' or 'partially resolved' even as the constraint persisted.
narrative_ontology:disappearance_verdict(versailles_reparations_clauses__limited_responsibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(versailles_reparations_clauses__limited_responsibility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(versailles_reparations_clauses__limited_responsibility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(versailles_reparations_clauses__limited_responsibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(versailles_reparations_clauses__limited_responsibility_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness declines from 0.78 to 0.62 over the interval because the limited-responsibility reading operationalizes capacity-bounding: initial maximalist claims (t=0) are revised downward by the Dawes Plan (t=4-6) as recognition of incapacity spreads, converging on a sustainable but still asymmetric schedule. Suppression requirement declines from 0.61 to 0.48 because German cooperation is increasingly gained through revised terms rather than coercion — the constraint shifts from enforcement-intensive (high suppression) to negotiation-intensive (lower suppression, higher theater). Theater rises from 0.28 to 0.41 because the capacity-bounding framing increasingly dominates public discourse even as the underlying obligation remains; the 'legality-not-morality' language becomes the visible performance while actual extraction persists via revised schedules. The accessibility_collapse (0.58) reflects that alternatives (outright default, treaty repudiation, unilateral non-payment) remain theoretically available but politically/economically costly for Germany — the constraint's binding force is not structural inevitability but negotiated acceptance. Resistance (0.72) is high because German population, elites, and political movements actively resist payment through political parties (DNVP, Communists, some SPD factions), reparations strikes, and hyperinflation (1923), yet the constraint persists because Allied creditors enforce it through occupation, asset seizure, and credibility threats. All measurements share one time grid (interval [0,10]) so every metric is authored at every examined point, enabling temporal coherence checking.
 *
 * PERSPECTIVAL GAP:
 *   From the German seat: the constraint is experienced as coercive extraction (occupation, asset seizure, payment demands backed by military threat) dressed in the language of legal obligation and capacity-respect. The 'capacity-bounding' framing is either genuine relief (if believed as legal principle) or cynical theater masking continued extraction (if disbelieved). From the Allied creditor seat: the constraint is experienced as necessary stabilization — extraction high enough to recover war costs but bounded by the recognition that Germany cannot pay unlimited sums; capacity-bounding is a pragmatic adjustment to economic reality, not a concession to German rights. From the Reparations Commission seat: the constraint is experienced as governance problem-solving — technical calibration of extraction to enforceability. From the repudiation reading: the constraint is experienced as illegitimate coercion masked by legalese; the entire framework (obligation + capacity-bounding) is a single extractive apparatus. The engine will compute these divergences from the structural data (power atoms, exit_options, time_horizons, roles) and the directionality derivation chain; the authored metrics and commentary establish the facts from which divergence is computed, not the divergences themselves.
 *
 * DIRECTIONALITY LOGIC:
 *   German government/population: d ≈ 0.85 (full target). They bear the extraction (fiscal burden, constrained policy options) via obligatory payment; capacity-bounding is the mechanism that extracts from them by accepting a revised but still substantial schedule rather than releasing them from obligation entirely. Their exit options are trapped or identity_locked (member of international system, bound by treaty sovereignty), not arbitrage. Allied creditors: d ≈ 0.25 (partial beneficiary). They collect reparations (positive flow) but must negotiate terms downward, sacrificing maximal extraction for enforceable viability — they benefit less than the punitive reading would allow, and bear diplomatic/enforcement costs. The capacity-bounding principle structurally favors German interests relative to pure extraction, so Allied agents sit closer to symmetric than to pure extraction (d=0.5 baseline, shifted down by German leverage). Occupied territories: d is contested by this omega (the reading may shift their victim status). Treaty framers/Reparations Commission: d ≈ 0.50 (symmetric). They set the rules but are themselves constrained by the coordination problem: extraction too high leads to German collapse and zero reparations; extraction too low fails to satisfy punitive reading's constituency. They broker the tension rather than sit cleanly at either end.
 *
 * MANDATROPHY ANALYSIS:
 *   The limited-responsibility reading avoids simple mandatrophy because it preserves a genuine (contested but real) coordination function: stabilized reparations payments that prevent German fiscal collapse, which would render payments impossible and leave occupied territories uncompensated. However, three mandatrophy signals are present: (1) the founding problem (war damage compensation to occupied territories and Allied civilian/infrastructure loss) remains live (high ε at t=0 reflects this persistence), but the solution (bounded reparations) increasingly operates as pure extraction from German population rather than recovery for occupied territories (suggesting 'solution' and 'problem' are decoupling); (2) the theater ratio rises as the capacity-bounding framing increasingly dominates discourse while the underlying extraction persists (classic theater drift — the 'legality-not-morality' language is performative, not functional); (3) the reading's internal axioms may be incoherent (see omega on formality/liability boundary), which would indicate the mandate itself is unstable. The classification remains tangled_rope (asymmetric coordination) rather than piton because extraction is still enforced and the beneficiary set (Allied creditors) still profits materially from the constraint. But the mandatrophy risks are substantial: if the reading collapses into incoherence or if occupied territories become net losers from capacity-bounding, the constraint would drift toward snare (pure extraction, no residual coordination).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    viability_vs_maximalism_boundary,
    'What constitutes ''economic viability'' for Germany, and who decides when payment capacity has been exhausted?',
    'Historical examination of Dawes Plan renegotiations (1924) and actual payment schedules; comparison of authored vs. implemented extraction schedules; testimony from German fiscal authorities and Allied creditor assessments contemporaneous with enforcement.',
    'If viability is measured by Germany''s declared capacity, extractiveness moderates and the constraint functions as partial coordination (accepting reduced reparations in exchange for fiscal stability). If viability is negotiated downward from maximal claims, extractiveness remains high but is partially offset by German leverage — the tangled-rope classification holds. If viability becomes a fiction used to justify continued extraction despite genuine exhaustion, the constraint drifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(viability_vs_maximalism_boundary, empirical, 'Whether ''economic capacity'' constraint is binding or ornamental.').

omega_variable(
    article_231_formality_vs_liability,
    'Is Article 231''s ''responsibility clause'' a bare legal formality (causation-neutral payment obligation) or does it carry latent liability implications that the limited-responsibility reading strategically downplays?',
    'Textual analysis of 231 alongside Allied negotiating records; comparison with how Article 231 was invoked in subsequent disputes (reparations revisions, Dawes Plan ratification debates, German counter-claims); examination of whether ''responsibility'' was later re-interpreted as moral/causal guilt (punitive reading''s pathway).',
    'If 231 is truly formalized procedure-neutral, the reading''s premise holds and extractiveness stays moderate (payment obligation bounded by capacity). If 231 carries implicit causal/moral weight that subsequent readings amplified, the limited-responsibility reading is obscuring rather than negating liability — the constraint becomes intellectually unstable and drifts toward snare (cover story for extraction). This omega documents whether the reading''s distinction (legal formality ≠ moral judgment) is sustainable or incoherent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_231_formality_vs_liability, conceptual, 'Whether Article 231 can be meaningfully stripped of liability implications.').

omega_variable(
    occupied_territory_victim_shift,
    'In the limited-responsibility reading, who bears the actual cost of reduced reparations — German population (via fiscal strain) or occupied territories and Allied civilian damages (via reduced compensation for actual harm)?',
    'Historical accounting: compare compensation actually received by occupied Belgium/France vs. maximal claims; compare German fiscal burden under Dawes schedule vs. immediate maximalist reparations; assess which reading''s victimology matches post-1924 implementation.',
    'If occupied territories bear the cost (reduced compensation for real infrastructure damage), the reading shifts victims from Germany to the actually-harmed populations — the constraint''s structure becomes inverted: it protects German economic interests AT THE EXPENSE of those who suffered direct territorial/civilian loss. This would reclassify the constraint from tangled-rope (asymmetric coordination) to snare (one-sided protection). If German population bears the cost (fiscal compression that reduces living standards), the reading''s beneficiary/victim structure holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(occupied_territory_victim_shift, empirical, 'Whether capacity-bounding protects the responsible party or shifts cost to the harmed.').

omega_variable(
    kernel_reading_incoherence_risk,
    'Can Article 231 be coherently read as ''legal formality not moral judgment'' while simultaneously using Germany''s economic capacity as the payment ceiling? If capacity constrains liability, doesn''t that imply a causal/moral relationship between harm-extent and payment-ability?',
    'Logical reconstruction: examine whether ''formality'' and ''capacity-bounding'' are compatible axioms or whether they entail contradictory implications about liability structure. If they are incompatible, the limited-responsibility reading conflates two different positions: (a) Article 231 carries no liability implications (pure legal procedure), and (b) Germany''s payment responsibility should be calibrated to its harm-causing capacity (implicit liability calibration). A coherent limited-responsibility reading would hold only (a), not both.',
    'If the axioms are incoherent, this reading is untenable as stated and must either abandon the ''formality'' framing or abandon the capacity-bounding principle. This affects whether the reading remains a live position or collapses into either the punitive reading (if liability is the actual basis) or a purely procedural-legalist position (if formality is maintained). Incoherence would explain historical drift: reading advocates shifted between legal-formalism and capacity-pragmatism depending on rhetorical context, suggesting the position was never stable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_incoherence_risk, conceptual, 'Internal coherence of the reading''s foundational axioms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(versailles_reparations_clauses__limited_responsibility_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vers_tr_t0, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(vers_tr_t0, observed).
narrative_ontology:measurement(vers_tr_t2, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 2, 0.32).
narrative_ontology:measurement_basis(vers_tr_t2, observed).
narrative_ontology:measurement(vers_tr_t4, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 4, 0.36).
narrative_ontology:measurement_basis(vers_tr_t4, observed).
narrative_ontology:measurement(vers_tr_t6, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 6, 0.39).
narrative_ontology:measurement_basis(vers_tr_t6, observed).
narrative_ontology:measurement(vers_tr_t8, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 8, 0.41).
narrative_ontology:measurement_basis(vers_tr_t8, observed).
narrative_ontology:measurement(vers_tr_t10, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 10, 0.41).
narrative_ontology:measurement_basis(vers_tr_t10, observed).

% Extraction over time
narrative_ontology:measurement(vers_be_t0, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 0, 0.78).
narrative_ontology:measurement_basis(vers_be_t0, observed).
narrative_ontology:measurement(vers_be_t2, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 2, 0.72).
narrative_ontology:measurement_basis(vers_be_t2, observed).
narrative_ontology:measurement(vers_be_t4, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 4, 0.68).
narrative_ontology:measurement_basis(vers_be_t4, observed).
narrative_ontology:measurement(vers_be_t6, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 6, 0.65).
narrative_ontology:measurement_basis(vers_be_t6, observed).
narrative_ontology:measurement(vers_be_t8, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 8, 0.62).
narrative_ontology:measurement_basis(vers_be_t8, observed).
narrative_ontology:measurement(vers_be_t10, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement_basis(vers_be_t10, observed).

% Suppression requirement over time
narrative_ontology:measurement(vers_su_t0, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 0, 0.61).
narrative_ontology:measurement_basis(vers_su_t0, observed).
narrative_ontology:measurement(vers_su_t2, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 2, 0.56).
narrative_ontology:measurement_basis(vers_su_t2, observed).
narrative_ontology:measurement(vers_su_t4, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 4, 0.52).
narrative_ontology:measurement_basis(vers_su_t4, observed).
narrative_ontology:measurement(vers_su_t6, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 6, 0.5).
narrative_ontology:measurement_basis(vers_su_t6, observed).
narrative_ontology:measurement(vers_su_t8, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 8, 0.48).
narrative_ontology:measurement_basis(vers_su_t8, observed).
narrative_ontology:measurement(vers_su_t10, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement_basis(vers_su_t10, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(versailles_reparations_clauses__limited_responsibility_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(versailles_reparations_clauses__limited_responsibility_reading, 0.18).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, versailles_reparations_clauses__punitive_liability_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, versailles_reparations_clauses__repudiation_reading).

% DUAL FORMULATION NOTE:
% The versailles_reparations_clauses kernel decomposes into three structurally distinct constraints, one per reading. Each reading instantiates a different ε, beneficiary/victim structure, and classification. The limited-responsibility reading (this file) claims mid-range extraction (0.62, moderating from maximal punitive claims) and genuine if asymmetric coordination (establishing stable payment terms). The punitive reading would claim high extraction (ε ≈ 0.85+) with minimal coordination function, drifting toward snare. The repudiation reading would claim the entire framework is coercive (high extraction by pure obligation, no coordination justification). All three are live readings held by different institutional seats. They are linked via network.affects_constraints to enable cross-reading analysis of how punishment logic, capacity pragmatism, and legitimacy rejection coexist in the same treaty frame.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
