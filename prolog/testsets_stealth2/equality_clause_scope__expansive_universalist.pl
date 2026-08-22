% ============================================================================
% CONSTRAINT STORY: equality_clause_scope__expansive_universalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equality_clause_scope__expansive_universalist, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: equality_clause_scope__expansive_universalist
 *   human_readable: Equality Clause Scope — Expansive Universalist Reading
 *   domain: constitutional law/political philosophy/civil rights history
 *
 * SUMMARY:
 *   This story instantiates ONE reading — expansive_universalist — of the
 *   contested kernel equality_clause_scope: the commitment that
 *   constitutional equality language denotes a self-evident universal truth
 *   binding on all humans, that historical exclusions are hypocrisy to be
 *   corrected rather than binding precedent, and that judicial interpretation
 *   carries a low legitimacy threshold for rights expansion. Per the fixed
 *   epsilon-referent rule for kernel-reading stories, extractiveness (epsilon
 *   = 0.47 at interval end) is authored for the STANDING ARRANGEMENT UNDER
 *   CONTEST — the selective application of equality with binding exclusions —
 *   as assessed by this reading's own lights, never for the fully
 *   universalized arrangement the reading endorses; the reading-indexed value
 *   over the fixed referent follows OQ-26/OQ-258. The claim/metrics pair is
 *   deliberately independent: the reading itself asserts mountain status
 *   ('self-evident'), so claimed_type = mountain with emerges_naturally =
 *   true is the authentic claim, while the authored metrics describe a
 *   contested, actively enforced, collector-bearing operation. Beneficiaries
 *   are declared intentionally to trigger false-summit evaluation: if the
 *   universalist frame is a constructed interpretive regime rather than
 *   recorded natural law, the FSM chain should detect it. This file is a
 *   member of a three-story constraint family (with
 *   equality_clause_scope__restrictive_originalist and
 *   equality_clause_scope__progressive_textualist); the colloquial label
 *   'equality' decomposes into three structurally distinct constraints with
 *   different beneficiary sets, different legitimacy thresholds, and
 *   different epsilon values.
 *
 * KEY AGENTS:
 *   - historically_excluded_groups: primary covered class (powerless/identity_locked) — bore the standing arrangement's extraction; now the rule's protected set
 *   - constitutional_courts: agenda-setter and institutional collector (institutional/constrained) — administers scope, accrues interpretive authority with each expansion
 *   - exclusion_reliant_jurisdictions: primary payer (powerful/constrained) — arrangements built on exclusionary lines are invalidated
 *   - originalist_interpretive_community: doctrinal payer (organized/identity_locked) — absorbs steady repudiation of its interpretive program
 *   - all_persons_within_jurisdiction: diffuse beneficiary (moderate/constrained) — covered by the universal set, bears costs indirectly
 *   - civil_rights_litigation_organizations: operational beneficiary (organized/constrained) — runs the expansion pipeline the low threshold invites
 *   - constitutional_theorists: analytical observer (analytical/analytical) — sees the full structure across polities
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__expansive_universalist, 0.47).
domain_priors:suppression_score(equality_clause_scope__expansive_universalist, 0.55).
domain_priors:theater_ratio(equality_clause_scope__expansive_universalist, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, extractiveness, 0.47).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__expansive_universalist, mountain).
narrative_ontology:human_readable(equality_clause_scope__expansive_universalist, "Equality Clause Scope — Expansive Universalist Reading").
narrative_ontology:topic_domain(equality_clause_scope__expansive_universalist, "constitutional law/political philosophy/civil rights history").

domain_priors:requires_active_enforcement(equality_clause_scope__expansive_universalist).
domain_priors:emerges_naturally(equality_clause_scope__expansive_universalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_clause_scope__expansive_universalist, 'e4735c8d-8236-4d28-b809-937516a2e5f7').
narrative_ontology:cs_kernel_codification('e4735c8d-8236-4d28-b809-937516a2e5f7', fixed_text).
narrative_ontology:cs_authority_grounding('e4735c8d-8236-4d28-b809-937516a2e5f7', lineage).
narrative_ontology:cs_interpretation_layer_present('e4735c8d-8236-4d28-b809-937516a2e5f7').
narrative_ontology:cs_reading_relation('e4735c8d-8236-4d28-b809-937516a2e5f7', equality_clause_scope__restrictive_originalist, forecloses).
narrative_ontology:cs_reading_relation('e4735c8d-8236-4d28-b809-937516a2e5f7', equality_clause_scope__progressive_textualist, coexists_with).
narrative_ontology:cs_axiom('e4735c8d-8236-4d28-b809-937516a2e5f7', foundational, equality_is_self_evident_universal_truth).
narrative_ontology:cs_axiom_status(equality_is_self_evident_universal_truth, holdable).
narrative_ontology:cs_axiom_grounding('e4735c8d-8236-4d28-b809-937516a2e5f7', equality_is_self_evident_universal_truth, deontological).
narrative_ontology:cs_axiom('e4735c8d-8236-4d28-b809-937516a2e5f7', secondary, historical_exclusions_are_hypocrisy_not_precedent).
narrative_ontology:cs_axiom_status(historical_exclusions_are_hypocrisy_not_precedent, holdable).
narrative_ontology:cs_axiom_grounding('e4735c8d-8236-4d28-b809-937516a2e5f7', historical_exclusions_are_hypocrisy_not_precedent, deontological).
narrative_ontology:cs_axiom('e4735c8d-8236-4d28-b809-937516a2e5f7', secondary, judicial_expansion_presumptively_legitimate).
narrative_ontology:cs_axiom_status(judicial_expansion_presumptively_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('e4735c8d-8236-4d28-b809-937516a2e5f7', judicial_expansion_presumptively_legitimate, instrumental).
narrative_ontology:cs_reference_frame('e4735c8d-8236-4d28-b809-937516a2e5f7', self_evident_universal_equality_baseline).
narrative_ontology:cs_drift_state('e4735c8d-8236-4d28-b809-937516a2e5f7', contemporary_post_civil_rights_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('e4735c8d-8236-4d28-b809-937516a2e5f7', '').
narrative_ontology:cs_kernel_id(equality_clause_scope__expansive_universalist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__expansive_universalist, historically_excluded_groups).
narrative_ontology:constraint_beneficiary(equality_clause_scope__expansive_universalist, all_persons_within_jurisdiction).
narrative_ontology:constraint_beneficiary(equality_clause_scope__expansive_universalist, constitutional_courts).
narrative_ontology:constraint_victim(equality_clause_scope__expansive_universalist, exclusion_reliant_jurisdictions).
narrative_ontology:constraint_victim(equality_clause_scope__expansive_universalist, originalist_interpretive_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equality_clause_scope__expansive_universalist, civil_rights_litigation_organizations).
narrative_ontology:constraint_vindicates(equality_clause_scope__expansive_universalist, self_evident_equality_premise).
narrative_ontology:constraint_vindicates(equality_clause_scope__expansive_universalist, judicial_enforcement_of_universal_rights).
narrative_ontology:constraint_vindicates(equality_clause_scope__expansive_universalist, suspect_classification_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Persons excluded at the founding and afterward — by race, sex, property, and status — from full legal and political standing. Under the prior selective application they bore denial of franchise, personhood, and protection, with no exit available because category membership is involuntary. The universal rule converts them into covered rights-holders; their lock to the category persists, but now binds them to the protection rather than the exclusion.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, historically_excluded_groups, beneficiary,
    powerless, generational, identity_locked, national).

% The universal set the rule covers — everyone the polity's law reaches, including persons never specifically targeted by any historical exclusion. Most experience the guarantee as background assurance rather than daily benefit, and carry its costs diffusely through litigation and compliance. Exit is impractical: renouncing the jurisdiction's equality guarantee means leaving its legal order entirely.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, all_persons_within_jurisdiction, beneficiary,
    moderate, generational, constrained, national).

% Administer the rule: decide what equality requires, which classifications survive scrutiny, which exclusions fall. Each successful expansion widens the court's interpretive jurisdiction and final-say authority — authority the rule routes to courts specifically because the reading sets a low legitimacy threshold for judicial expansion. The courts cannot exit the role; the text commits the function to them.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, constitutional_courts, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(equality_clause_scope__expansive_universalist, constitutional_courts, beneficiary).

% States, localities, and their majorities whose policy arrangements rest on drawing the very lines the rule forbids — segregated schooling, disqualified voters, tiered personhood. Invalidations strip arrangements built over generations; the amendment escape exists only at a threshold few coalitions ever reach. They litigate, delay, and comply in sequence.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, exclusion_reliant_jurisdictions, payer,
    powerful, biographical, constrained, regional).

% Scholars, jurists, and movements committed to the founding-generation scope of the equality language. Under the universal rule their program absorbs steady doctrinal repudiation — precedent accumulates against the scope they defend. Their professional identity is fused with the originalist project; exit would mean abandoning the interpretive identity itself.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, originalist_interpretive_community, payer,
    organized, generational, identity_locked, national).

% Advocacy complexes that bring the expansion cases the rule invites. They convert the low judicial threshold into a standing pipeline: identify an exclusion, frame it as an equality violation, litigate. Funding, membership, and reputation depend on the contest continuing; victories narrow the docket and push the organizations toward new fronts.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, civil_rights_litigation_organizations, beneficiary,
    organized, biographical, constrained, national).

% Analyze the rule's structure across polities and eras — whom it empowers, what it transfers, where its self-description (self-evidence) matches its operation (enforced interpretation). Hold no stake in any single jurisdiction's outcome.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, constitutional_theorists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equality_clause_scope__expansive_universalist, constitutional_courts).
narrative_ontology:fixing_cost_class(equality_clause_scope__expansive_universalist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Settles, once and stably, who counts as an equal member of the polity: a universal membership baseline that does not require each generation, each statute, or each dispute to re-negotiate the boundary of legal and moral standing.
% TRANSFER_FUNCTION: Transfers enforceable legal standing to persons the prior selective application excluded, and transfers decision-authority over the equality boundary from legislatures and local majorities to constitutional courts, whose interpretive jurisdiction expands with each successful claim.
% ABSENT_VOICES: The majorities and jurisdictions whose arrangements are invalidated enter the process only as litigants defending challenged practices — there is no seat where their governance interest is weighed as such, only as a constitutional objection to be overruled. The amendment-path constituency holds that process-legitimacy voices are structurally sidelined by the low judicial threshold. Historically, the excluded themselves were absent at the founding; their present standing before the court is the reading's central correction.
% DISAPPEARANCE_RATIONALE: Overnight removal would strand the legal standing of every person whose equal protection currently rests on the universal reading: anti-discrimination law, franchise equalities, and equal-citizenship doctrines would lose their interpretive foundation, and the prior selective scope — or whatever replacement the surviving institutions negotiated — would have to be rebuilt from scratch across every institution that relies on the current baseline.
% FOUNDING_PROBLEM: The founding contradiction: a polity proclaiming equality as self-evident while its positive law withheld standing from most of its population. The arrangement needed a rule for whether the declaration or the exclusions defined the clause's meaning.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: originalist scholars and jurists attest that the boundary problem recurs (they dispute the solution, not the recurrence); dissenting opinions and recurring court-curbing legislation document continuing contest; comparative constitutional courts confronting new candidate exclusions — citizenship line, carceral status, disability — treat the question as unresolved. No major interpretive school attests that the problem is settled.
narrative_ontology:disappearance_verdict(equality_clause_scope__expansive_universalist, world_rearranges).
narrative_ontology:founding_problem_status(equality_clause_scope__expansive_universalist, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equality_clause_scope__expansive_universalist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equality_clause_scope__expansive_universalist, 'none', 1).
narrative_ontology:epsilon_provenance(equality_clause_scope__expansive_universalist, 0.47, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_clause_scope__expansive_universalist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equality_clause_scope__expansive_universalist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, ExtMetricName, E),
    domain_priors:suppression_score(equality_clause_scope__expansive_universalist, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(equality_clause_scope__expansive_universalist),
    narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(equality_clause_scope__expansive_universalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness declines from 0.93 to 0.47 across the interval: the standing arrangement's extraction from the excluded was near-total at the start (open legal exclusion beneath an intact universal declaration) and has been partially corrected by the rule's own operation, with a residual gap the reading still classifies as hypocrisy (citizenship-line exclusions, territorial inequality, carceral disenfranchisement, precedent fragility). Suppression_requirement is authored because this story specifically tracks enforcement-capacity change, and its trajectory records an OBJECT INVERSION rather than simple decay: coercive machinery held near-constant intensity (0.85-0.88) while defending exclusion, then remained substantial (0.78 falling to 0.55) as the same machinery was redirected to compel inclusion — federal troops, court orders, contempt, funding leverage. Theater_ratio falls from 0.80 to 0.25: at the start the universal declaration was nearly pure performance atop functional exclusion; after mid-interval enforcement arrived, the declaratory layer became mostly functional, retaining only ceremonial residue. Mild backlash oscillation (reconstruction/redemption, expansion/retrenchment cycles) rides on the declining trends but is not itself the extraction mechanism — the wobble tracks electoral turnover, not intermittent reinforcement. All three series share one nine-point grid; the scalar base_properties values equal the series endpoints.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute sharply different types from identical structural data. From the constitutional_courts seat the arrangement is a coordination achievement it administers and from which it legitimately collects authority; from the exclusion_reliant_jurisdictions seat the same structure operates as extraction — generational policy investments invalidated by an institution they did not choose and cannot amend away; from the historically_excluded_groups seat it is liberation whose incompleteness is the salient fact; from the originalist_interpretive_community seat it is doctrinal dispossession. The engine computes this divergence from power, exit, and role data; the authored mountain claim does not adjudicate among the seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation: constitutional_courts (agenda_setter + beneficiary, institutional) derive near the beneficiary end — they genuinely collect the rule's authority yields. historically_excluded_groups appear as beneficiaries, but the derivation's identity-lock adjustment assumes target-position locking; here the lock binds the class TO the protection (they cannot and would not exit the guarantee), so an override sets the powerless atom to d = 0.2, near the beneficiary end. exclusion_reliant_jurisdictions and originalist_interpretive_community are declared victims and derive near the target end — they bear invalidation and repudiation costs respectively, tempered by retained resources and organizational capacity. all_persons_within_jurisdiction and civil_rights_litigation_organizations derive low d from beneficiary listing. Suppression (0.55) is authored as a raw structural property and is deliberately NOT scaled; only extractiveness passes through directionality and scope scaling in the engine.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against mislabeling in both directions. Reading the constraint as pure mountain (taking 'self-evident' at face value) would mask the constructed, collector-bearing structure — courts accrue authority under the low threshold, and the FSM evaluation exists precisely to test whether the summit is natural law or a maintained regime with identifiable collectors. Conversely, reading it as a snare against the payer seats would erase the genuine coordination function: a universal membership baseline solves a real collective-action problem (who counts) that neither per-dispute renegotiation nor amendment-threshold gating solves as stably. On the R5 genealogy interview the founding problem (the membership-boundary contradiction) is corroborated as LIVE from outside the beneficiary set, so no mandatrophy resolution is declared: the constraint has not outlived its function, and the mismatch consumer should find status=live x verdict=world_rearranges — a healthy live constraint, not a zombie.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_regime,
    'Is the equality clause''s universality a discovered moral fact the text records, or a constructed interpretive regime that identifiable agents — courts accruing authority, advocacy complexes running the expansion pipeline — maintain and benefit from?',
    'Comparative-historical analysis testing whether universalist outcomes track the text''s semantic content independently of the enforcing coalition''s interests, plus counterfactual analysis of whether the scope would persist absent active judicial enforcement.',
    'Genuine natural-law status certifies the mountain claim; a constructed regime with concentrated collectors supports FSM reclassification toward tangled_rope, with the courts'' authority accrual as the extraction side.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_regime, conceptual, 'Whether the universalist frame is natural law or a maintained construct with collectors — the false-summit question.').

omega_variable(
    kernel_reading_underdetermination,
    'This constraint is one reading of kernel equality_clause_scope; siblings restrictive_originalist and progressive_textualist instantiate different beneficiary sets and legitimacy thresholds from the same text. Which reading governs is underdetermined by the text alone.',
    'No resolution is available inside the framework — the readings coexist as live constitutional positions; resolution would require amendment-level settlement or doctrinal exhaustion. The omega records the location of the disagreement: beneficiary-set boundary and the institution whose assent legitimates scope change.',
    'If restrictive_originalist governed, the beneficiary set contracts to the propertied political class and epsilon collapses toward ~0.1 from its own lights; if progressive_textualist governed, the universal endpoint is retained but the transfer function changes — no judicial authority accrual, expansion only at amendment threshold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Committer-frame underdetermination: one kernel, three readings, disagreement located in beneficiary boundary and legitimating institution.').

omega_variable(
    residual_hypocrisy_gap_measurement,
    'How large is the residual gap between declared universality and practiced exclusion at interval end — and which exclusions count as inside the arrangement''s scope (citizenship line, territorial status, carceral status, non-citizens)?',
    'Enumerate currently excluded classes within the jurisdiction and measure the legal-standing differential each suffers; sensitivity-test epsilon across inclusion thresholds.',
    'Epsilon could move by roughly +/-0.2 depending on threshold choices; the classification is sensitive near the rope/tangled-rope boundary at the present endpoint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_hypocrisy_gap_measurement, empirical, 'Measurement basis of the residual extraction the reading''s own lights assign to the standing arrangement.').

omega_variable(
    enforcement_object_attribution,
    'The suppression series shows enforcement intensity declining while its OBJECT inverts (from defending exclusion to compelling inclusion). Is post-inversion suppression properly attributed to this constraint, or is it inherited machinery of the standing arrangement?',
    'Isolate enforcement actions initiated under universalist doctrine from those continuing exclusion-era mechanisms; compare jurisdictions where the inversion did not occur.',
    'If the residual suppression is attributed to inherited machinery rather than this rule, this constraint''s suppression drops toward ~0.3, damping extraction amplification for target seats and softening any tangle verdict.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_object_attribution, empirical, 'Attribution of post-inversion coercive force to the rule versus the arrangement it displaced.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__expansive_universalist, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecu_tr_t0, equality_clause_scope__expansive_universalist, theater_ratio, 0, 0.78).
narrative_ontology:measurement_basis(ecu_tr_t0, observed).
narrative_ontology:measurement(ecu_tr_t10, equality_clause_scope__expansive_universalist, theater_ratio, 10, 0.8).
narrative_ontology:measurement_basis(ecu_tr_t10, observed).
narrative_ontology:measurement(ecu_tr_t20, equality_clause_scope__expansive_universalist, theater_ratio, 20, 0.79).
narrative_ontology:measurement_basis(ecu_tr_t20, observed).
narrative_ontology:measurement(ecu_tr_t30, equality_clause_scope__expansive_universalist, theater_ratio, 30, 0.7).
narrative_ontology:measurement_basis(ecu_tr_t30, observed).
narrative_ontology:measurement(ecu_tr_t40, equality_clause_scope__expansive_universalist, theater_ratio, 40, 0.52).
narrative_ontology:measurement_basis(ecu_tr_t40, observed).
narrative_ontology:measurement(ecu_tr_t50, equality_clause_scope__expansive_universalist, theater_ratio, 50, 0.38).
narrative_ontology:measurement_basis(ecu_tr_t50, observed).
narrative_ontology:measurement(ecu_tr_t60, equality_clause_scope__expansive_universalist, theater_ratio, 60, 0.33).
narrative_ontology:measurement_basis(ecu_tr_t60, observed).
narrative_ontology:measurement(ecu_tr_t70, equality_clause_scope__expansive_universalist, theater_ratio, 70, 0.28).
narrative_ontology:measurement_basis(ecu_tr_t70, observed).
narrative_ontology:measurement(ecu_tr_t80, equality_clause_scope__expansive_universalist, theater_ratio, 80, 0.25).
narrative_ontology:measurement_basis(ecu_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(ecu_be_t0, equality_clause_scope__expansive_universalist, base_extractiveness, 0, 0.93).
narrative_ontology:measurement_basis(ecu_be_t0, observed).
narrative_ontology:measurement(ecu_be_t10, equality_clause_scope__expansive_universalist, base_extractiveness, 10, 0.94).
narrative_ontology:measurement_basis(ecu_be_t10, observed).
narrative_ontology:measurement(ecu_be_t20, equality_clause_scope__expansive_universalist, base_extractiveness, 20, 0.93).
narrative_ontology:measurement_basis(ecu_be_t20, observed).
narrative_ontology:measurement(ecu_be_t30, equality_clause_scope__expansive_universalist, base_extractiveness, 30, 0.88).
narrative_ontology:measurement_basis(ecu_be_t30, observed).
narrative_ontology:measurement(ecu_be_t40, equality_clause_scope__expansive_universalist, base_extractiveness, 40, 0.74).
narrative_ontology:measurement_basis(ecu_be_t40, observed).
narrative_ontology:measurement(ecu_be_t50, equality_clause_scope__expansive_universalist, base_extractiveness, 50, 0.58).
narrative_ontology:measurement_basis(ecu_be_t50, observed).
narrative_ontology:measurement(ecu_be_t60, equality_clause_scope__expansive_universalist, base_extractiveness, 60, 0.55).
narrative_ontology:measurement_basis(ecu_be_t60, observed).
narrative_ontology:measurement(ecu_be_t70, equality_clause_scope__expansive_universalist, base_extractiveness, 70, 0.5).
narrative_ontology:measurement_basis(ecu_be_t70, observed).
narrative_ontology:measurement(ecu_be_t80, equality_clause_scope__expansive_universalist, base_extractiveness, 80, 0.47).
narrative_ontology:measurement_basis(ecu_be_t80, observed).

% Suppression requirement over time
narrative_ontology:measurement(ecu_su_t0, equality_clause_scope__expansive_universalist, suppression_requirement, 0, 0.85).
narrative_ontology:measurement_basis(ecu_su_t0, observed).
narrative_ontology:measurement(ecu_su_t10, equality_clause_scope__expansive_universalist, suppression_requirement, 10, 0.88).
narrative_ontology:measurement_basis(ecu_su_t10, observed).
narrative_ontology:measurement(ecu_su_t20, equality_clause_scope__expansive_universalist, suppression_requirement, 20, 0.87).
narrative_ontology:measurement_basis(ecu_su_t20, observed).
narrative_ontology:measurement(ecu_su_t30, equality_clause_scope__expansive_universalist, suppression_requirement, 30, 0.83).
narrative_ontology:measurement_basis(ecu_su_t30, observed).
narrative_ontology:measurement(ecu_su_t40, equality_clause_scope__expansive_universalist, suppression_requirement, 40, 0.78).
narrative_ontology:measurement_basis(ecu_su_t40, observed).
narrative_ontology:measurement(ecu_su_t50, equality_clause_scope__expansive_universalist, suppression_requirement, 50, 0.72).
narrative_ontology:measurement_basis(ecu_su_t50, observed).
narrative_ontology:measurement(ecu_su_t60, equality_clause_scope__expansive_universalist, suppression_requirement, 60, 0.66).
narrative_ontology:measurement_basis(ecu_su_t60, observed).
narrative_ontology:measurement(ecu_su_t70, equality_clause_scope__expansive_universalist, suppression_requirement, 70, 0.6).
narrative_ontology:measurement_basis(ecu_su_t70, observed).
narrative_ontology:measurement(ecu_su_t80, equality_clause_scope__expansive_universalist, suppression_requirement, 80, 0.55).
narrative_ontology:measurement_basis(ecu_su_t80, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=80
narrative_ontology:measurement(ecu_grid_01, equality_clause_scope__expansive_universalist, accessibility_collapse(class), 0, 0.85).
narrative_ontology:measurement(ecu_grid_02, equality_clause_scope__expansive_universalist, accessibility_collapse(class), 80, 0.5).
narrative_ontology:measurement(ecu_grid_03, equality_clause_scope__expansive_universalist, accessibility_collapse(individual), 0, 0.8).
narrative_ontology:measurement(ecu_grid_04, equality_clause_scope__expansive_universalist, accessibility_collapse(individual), 80, 0.45).
narrative_ontology:measurement(ecu_grid_05, equality_clause_scope__expansive_universalist, accessibility_collapse(organizational), 0, 0.6).
narrative_ontology:measurement(ecu_grid_06, equality_clause_scope__expansive_universalist, accessibility_collapse(organizational), 80, 0.7).
narrative_ontology:measurement(ecu_grid_07, equality_clause_scope__expansive_universalist, accessibility_collapse(structural), 0, 0.7).
narrative_ontology:measurement(ecu_grid_08, equality_clause_scope__expansive_universalist, accessibility_collapse(structural), 80, 0.75).
narrative_ontology:measurement(ecu_grid_09, equality_clause_scope__expansive_universalist, resistance(class), 0, 0.5).
narrative_ontology:measurement(ecu_grid_10, equality_clause_scope__expansive_universalist, resistance(class), 80, 0.5).
narrative_ontology:measurement(ecu_grid_11, equality_clause_scope__expansive_universalist, resistance(individual), 0, 0.4).
narrative_ontology:measurement(ecu_grid_12, equality_clause_scope__expansive_universalist, resistance(individual), 80, 0.35).
narrative_ontology:measurement(ecu_grid_13, equality_clause_scope__expansive_universalist, resistance(organizational), 0, 0.2).
narrative_ontology:measurement(ecu_grid_14, equality_clause_scope__expansive_universalist, resistance(organizational), 80, 0.65).
narrative_ontology:measurement(ecu_grid_15, equality_clause_scope__expansive_universalist, resistance(structural), 0, 0.3).
narrative_ontology:measurement(ecu_grid_16, equality_clause_scope__expansive_universalist, resistance(structural), 80, 0.55).
narrative_ontology:measurement(ecu_grid_17, equality_clause_scope__expansive_universalist, stakes_inflation(class), 0, 0.6).
narrative_ontology:measurement(ecu_grid_18, equality_clause_scope__expansive_universalist, stakes_inflation(class), 80, 0.4).
narrative_ontology:measurement(ecu_grid_19, equality_clause_scope__expansive_universalist, stakes_inflation(individual), 0, 0.7).
narrative_ontology:measurement(ecu_grid_20, equality_clause_scope__expansive_universalist, stakes_inflation(individual), 80, 0.35).
narrative_ontology:measurement(ecu_grid_21, equality_clause_scope__expansive_universalist, stakes_inflation(organizational), 0, 0.4).
narrative_ontology:measurement(ecu_grid_22, equality_clause_scope__expansive_universalist, stakes_inflation(organizational), 80, 0.75).
narrative_ontology:measurement(ecu_grid_23, equality_clause_scope__expansive_universalist, stakes_inflation(structural), 0, 0.5).
narrative_ontology:measurement(ecu_grid_24, equality_clause_scope__expansive_universalist, stakes_inflation(structural), 80, 0.7).
narrative_ontology:measurement(ecu_grid_25, equality_clause_scope__expansive_universalist, suppression(class), 0, 0.8).
narrative_ontology:measurement(ecu_grid_26, equality_clause_scope__expansive_universalist, suppression(class), 80, 0.3).
narrative_ontology:measurement(ecu_grid_27, equality_clause_scope__expansive_universalist, suppression(individual), 0, 0.75).
narrative_ontology:measurement(ecu_grid_28, equality_clause_scope__expansive_universalist, suppression(individual), 80, 0.3).
narrative_ontology:measurement(ecu_grid_29, equality_clause_scope__expansive_universalist, suppression(organizational), 0, 0.7).
narrative_ontology:measurement(ecu_grid_30, equality_clause_scope__expansive_universalist, suppression(organizational), 80, 0.6).
narrative_ontology:measurement(ecu_grid_31, equality_clause_scope__expansive_universalist, suppression(structural), 0, 0.85).
narrative_ontology:measurement(ecu_grid_32, equality_clause_scope__expansive_universalist, suppression(structural), 80, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_clause_scope__expansive_universalist, identity_coordination).
narrative_ontology:affects_constraint(equality_clause_scope__expansive_universalist, equality_clause_scope__restrictive_originalist).
narrative_ontology:affects_constraint(equality_clause_scope__expansive_universalist, equality_clause_scope__progressive_textualist).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'equality' conflates three structurally distinct claims that measure differently and must not share one story. This upstream story (expansive_universalist) links to both siblings: restrictive_originalist instantiates a restricted-beneficiary-set constraint with low epsilon from its own lights; progressive_textualist instantiates the same universal endpoint with a different transfer function (amendment-gated, no judicial accrual). The universalist reading influences both siblings' operating environments — its doctrinal accumulations raise the originalists' repudiation costs and lower the marginal payoff of the amendment path — while foreclosing the originalist core premise within any single framework and coexisting with the textualist mechanism dispute.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equality_clause_scope__expansive_universalist, powerless, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
