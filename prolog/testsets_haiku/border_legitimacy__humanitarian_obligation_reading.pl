% ============================================================================
% CONSTRAINT STORY: border_legitimacy__humanitarian_obligation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_legitimacy__humanitarian_obligation_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: border_legitimacy__humanitarian_obligation_reading
 *   human_readable: Humanitarian Obligation Reading: Refuge vs. Economic Migration Distinction
 *   domain: political_philosophy/international_law
 *
 * SUMMARY:
 *   This constraint is one reading of the contested border-legitimacy kernel.
 *   The humanitarian-obligation reading asserts that states have a
 *   categorical duty to admit those fleeing persecution or disaster, while
 *   retaining discretion over economic migrants. The reading creates an
 *   administrative distinction — refugee vs. non-refugee — that bifurcates
 *   the victim set. Persecuted and disaster-displaced populations are
 *   beneficiaries (they have a claim on states' protection resources);
 *   economic migrants are treated as targets of the categorical exclusion
 *   (they lack the protected status). This reading coexists with
 *   sovereignty-reading states that deny any categorical obligation and
 *   freedom-of-movement readings that contest the distinction itself. The
 *   constraint embodies the humanitarian reading's foundational claim: the
 *   persecuted have a survival right that overrides state discretion, but
 *   voluntary economic movement does not.
 *
 * KEY AGENTS:
 *   - persecuted_asylum_seekers — powerless, trapped; the reading's primary beneficiaries
 *   - economic_migrants — powerless, constrained; positioned as non-beneficiaries by the categorical distinction
 *   - receiving_states — institutional, arbitrage-exit; agenda-setters who enforce the boundary
 *   - origin_states — institutional, trapped; excluded from setting the boundary but bear the outflow
 *   - international human rights bodies — institutional, analytical; observe and interpret the constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__humanitarian_obligation_reading, 0.58).
domain_priors:suppression_score(border_legitimacy__humanitarian_obligation_reading, 0.62).
domain_priors:theater_ratio(border_legitimacy__humanitarian_obligation_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__humanitarian_obligation_reading, tangled_rope).
narrative_ontology:human_readable(border_legitimacy__humanitarian_obligation_reading, "Humanitarian Obligation Reading: Refuge vs. Economic Migration Distinction").
narrative_ontology:topic_domain(border_legitimacy__humanitarian_obligation_reading, "political_philosophy/international_law").

domain_priors:requires_active_enforcement(border_legitimacy__humanitarian_obligation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__humanitarian_obligation_reading, '1a013be8-c70c-49f0-a552-b827a42fc053').
narrative_ontology:cs_kernel_codification('1a013be8-c70c-49f0-a552-b827a42fc053', fixed_text).
narrative_ontology:cs_authority_grounding('1a013be8-c70c-49f0-a552-b827a42fc053', lineage).
narrative_ontology:cs_interpretation_layer_present('1a013be8-c70c-49f0-a552-b827a42fc053').
narrative_ontology:cs_reading_relation('1a013be8-c70c-49f0-a552-b827a42fc053', border_legitimacy__border_legitimacy_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('1a013be8-c70c-49f0-a552-b827a42fc053', border_legitimacy__border_legitimacy_freedom_of_movement_reading, influences).
narrative_ontology:cs_axiom('1a013be8-c70c-49f0-a552-b827a42fc053', foundational, protection_duty_on_persecution).
narrative_ontology:cs_axiom_status(protection_duty_on_persecution, holdable).
narrative_ontology:cs_axiom_grounding('1a013be8-c70c-49f0-a552-b827a42fc053', protection_duty_on_persecution, deontological).
narrative_ontology:cs_axiom('1a013be8-c70c-49f0-a552-b827a42fc053', foundational, discretionary_exclusion_for_economic_migration).
narrative_ontology:cs_axiom_status(discretionary_exclusion_for_economic_migration, holdable).
narrative_ontology:cs_axiom_grounding('1a013be8-c70c-49f0-a552-b827a42fc053', discretionary_exclusion_for_economic_migration, deontological).
narrative_ontology:cs_reference_frame('1a013be8-c70c-49f0-a552-b827a42fc053', survival_right_priority_over_discretion).
narrative_ontology:cs_drift_state('1a013be8-c70c-49f0-a552-b827a42fc053', contemporary_enforcement_hardening, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1a013be8-c70c-49f0-a552-b827a42fc053', '').
narrative_ontology:cs_kernel_id(border_legitimacy__humanitarian_obligation_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, persecuted_populations).
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, disaster_displaced_persons).
narrative_ontology:constraint_victim(border_legitimacy__humanitarian_obligation_reading, economic_migrants).
narrative_ontology:constraint_victim(border_legitimacy__humanitarian_obligation_reading, border_enforcement_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, persecuted_asylum_seekers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Flee persecution (political, religious, ethnic) and are entitled under this reading to admission and protection. Exit from the originating jurisdiction is often the only survival strategy. The constraint grants them a legal category of protected status, though enforcement varies by receiving state and jurisdiction.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, persecuted_asylum_seekers, beneficiary,
    powerless, biographical, trapped, global).

% Displaced by natural disaster, environmental collapse, or conflict-related infrastructure destruction. Under this reading, they have a claim to temporary or permanent refuge. Their circumstances are often time-bounded but urgent. Receiving states sometimes recognize this obligation; many do not.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, disaster_displaced_persons, beneficiary,
    powerless, immediate, trapped, global).

% Seek to relocate for employment, income, or standard-of-living improvement. Under this reading, they have NO categorical obligation to be admitted — receiving states retain discretion. This distinction places them outside the humanitarian corridor, often creating perverse incentives to reframe economic migration as persecution or declare false disaster status.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, economic_migrants, payer,
    powerless, biographical, constrained, global).

% Set and enforce the refugee/non-refugee boundary through administrative procedure, judicial review, and border enforcement. The constraint obligates them to admit humanitarian cases but gives them discretion over economic migration, placing the burden of categorical determination on state machinery. They collect sovereignty over who enters and who is excluded.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, receiving_states, agenda_setter,
    institutional, generational, arbitrage, national).

% Lose population through refugee outflows, often their most vulnerable and skilled citizens. The humanitarian reading reduces their policy leverage over who leaves — persecuted populations have a right, rather than requiring state permission or negotiation.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, origin_states, excluded,
    institutional, generational, trapped, national).

% Monitor state compliance with the humanitarian obligation through treaty bodies, courts, and investigative mechanisms. They interpret what counts as persecution and disaster, and produce normative pressure on states to honor the distinction.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_legitimacy__humanitarian_obligation_reading, receiving_states).
narrative_ontology:fixing_cost_class(border_legitimacy__humanitarian_obligation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a categorical framework for international responsibility-sharing: distinguishes cases of genuine flight (where the international system has a duty) from voluntary migration (where individual choice governs). Enables states to coordinate on a unified standard rather than each applying idiosyncratic definitions of 'refugee,' which would create arbitrage and perverse incentives.
% TRANSFER_FUNCTION: Moves the burden of protection and integration from origin states to receiving states for those fleeing persecution and disaster. Also moves the cost-of-exclusion from economic migrants (who lack the categorical claim) to receiving states' labor markets and origin states' human-capital drain. The boundary between protected and unprotected movement determines which state bears the cost.
% ABSENT_VOICES: Economic migrants themselves rarely participate in the refugee-definition process; the distinction is made by state actors and humanitarian organizations. Origin states that benefit from remittances but bear the population loss are largely excluded from setting the boundary. Migrants in mixed-motive flows (fleeing violence AND seeking economic opportunity) find themselves unable to articulate their actual situation within the categorical binary.
% DISAPPEARANCE_RATIONALE: If this obligation vanished, receiving states would have no duty to distinguish humanitarian from economic cases; asylum law as currently structured would collapse. Origin states would regain unilateral control over exit. The humanitarian protection infrastructure would cease. Mixed-motivation flows would be handled through pure discretionary economics rather than rights-based frameworks. The international system would lose the coordination mechanism that defines who states owe protection to.
% FOUNDING_PROBLEM: Post-WWII: the international system lacked a principled way to distinguish those with genuine claims on state protection from those seeking economic opportunity. The Holocaust and partition displaced millions; without a humanitarian category, states could treat all migration as discretionary and exclude indiscriminately. The founding problem was: how can states honor both border authority AND the survival rights of the persecuted?
% FOUNDING_PROBLEM_CORROBORATION: UNHCR and international human rights bodies (UN Committee on the Rights of Migrants, International Court of Justice) attest the founding problem remains live: persecution and disaster still produce mass displacement and demand international response. Sovereignty-reading advocates and receiving states argue the problem is solved by internal displacement law and humanitarian aid to origin regions, making the asylum obligation obsolete. Migration scholarship is split: humanitarian-obligation scholars cite ongoing persecution and climate displacement; sovereignty scholars cite welfare costs and labor-market disruption in receiving states as evidence the founding problem has shifted.
narrative_ontology:disappearance_verdict(border_legitimacy__humanitarian_obligation_reading, world_rearranges).
narrative_ontology:founding_problem_status(border_legitimacy__humanitarian_obligation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_legitimacy__humanitarian_obligation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(border_legitimacy__humanitarian_obligation_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_legitimacy__humanitarian_obligation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_legitimacy__humanitarian_obligation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_legitimacy__humanitarian_obligation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58 at 2024, rising from 0.35 in 1951) because the reading creates categorical closure: economic migrants bear the extraction of having their movement restricted without a protection claim, while persecution/disaster claimants must navigate an increasingly stringent verification process. The extraction rises over time as states develop more sophisticated determination machinery and restrict access (Dublin procedures, safe-third-country rules, biometric systems). Theater is moderate-to-high (0.48): much state activity focuses on managing the refugee/non-refugee boundary administratively rather than on actual protection outcomes — the machinery of determination becomes the performance. Suppression is substantial (0.62) because the constraint requires active enforcement to maintain the distinction: states must police the boundary, deny economic migrants, and distinguish genuine persecution from reframed migration. The measurement series track the post-WWII expansion of refugee determination systems (1951–2000), the subsequent hardening of borders and rise in asymmetric enforcement (2000–2018), and the plateauing of suppression intensity (2018–2024) as enforcement machinery reaches saturation. The temporal pattern shows extraction accumulation driven by enforcement intensification, not by rising refugee flows — theater ratio rising faster than base extractiveness suggests performative expansion (more determination machinery with lower throughput).
 *
 * PERSPECTIVAL GAP:
 *   From the receiving state's seat, the constraint is a coordination mechanism that allocates international responsibility and enables managed movement. From the persecuted seat, it is a lifeline that obligates protection. From the economic migrant's seat, it is an arbitrary exclusion that makes their movement illegal while similar-situated asylum seekers are protected — a structural injustice. The engine should compute these seats as experiencing different types: receiving states may perceive the arrangement as rope or tangled rope (coordination + extraction); persecuted populations as rope (protection) or mountain (an inviolable right); economic migrants as snare (pure exclusion). The divergence is structural, not observational.
 *
 * DIRECTIONALITY LOGIC:
 *   Persecuted and disaster-displaced populations (beneficiaries) derive low directionality (d near 0.0–0.3) — they have escape and protection, though constrained by verification barriers. Economic migrants (victims in the categorical sense) derive high directionality (d near 0.7–1.0) — they face systematic exclusion and constrained exit from origin regions, with no protection claim. Receiving states (agenda-setters) derive mixed directionality: they benefit from the discretion the distinction grants them (d near 0.3–0.5 on the extraction side) but bear integration costs for those admitted (d near 0.5–0.7 on the coordination side). The net state directionality depends on net migration inflows, labor-market conditions, and the ratio of protection claims to economic claims, which varies by receiving state and time period. No single d value captures a receiving state's position — they are simultaneously beneficiary (discretion) and payer (integration cost), hence the tangled-rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The humanitarian-obligation reading faces a mandatrophy hazard. The founding problem was: how do states honor the survival rights of the persecuted while retaining border authority? The reading answers by distinguishing persecution from voluntary migration. But the distinction has become increasingly contested and administratively brittle: modern migration is often mixed-motive (fleeing violence that is also economically motivated); climate displacement blurs the disaster/migration line; states use the categorical distinction to avoid obligations rather than to coordinate them. The rising theater ratio (0.22→0.48) and plateaued suppression (0.41→0.62) suggest the determination machinery is maintaining the boundary through increasingly performative procedures rather than through actual protection outcomes. At what point does the foundational problem (distinguishing survival flight from voluntary movement) become obsolete because modern migration no longer fits the binary? The reading has not yet mandatroped — the founding problem remains live and contested — but the rising theater and stalled suppression suggest drift toward piton status (maintained as institutional theater rather than as a real coordination mechanism).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    persecution_vs_economic_boundary_instability,
    'Is the persecution/disaster vs. economic-motivation boundary stable and administratively legible, or is modern migration systematically mixed-motive in ways that make categorical distinction arbitrary?',
    'Ethnographic and survey studies of actual migrants'' decision-making; administrative data on asylum determination appeals and reversals; longitudinal tracking of rejected claims to see if they reclassify under different rubrics.',
    'If the boundary is arbitrary and often contradicted by migrants'' own accounts, the reading''s categorical foundation collapses — the constraint would devolve to state discretion on all claims (sovereignty reading). If the boundary holds under scrutiny, the tangled-rope classification is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persecution_vs_economic_boundary_instability, empirical, 'Whether the humanitarian reading''s foundational distinction is administratively sustainable.').

omega_variable(
    foundation_problem_death_or_transformation,
    'Is the post-WWII founding problem (how to honor survival rights while retaining border authority) still the operative constraint, or has it been transformed by climate displacement, conflict-driven migration, and global inequality into a different problem (how to manage mass flow while maintaining state capacity)?',
    'Discourse analysis of state and international-body framing of refugee obligation; comparison of founding-era rhetoric to contemporary asylum debates; examination of whether states invoke post-WWII reasoning or new rationales for their policies.',
    'If the founding problem has transformed, the humanitarian reading is a mandatrophied constraint — it persists as institutional form (treaties, definitions, procedures) but no longer addresses the problem it was built to solve. This would reclassify the constraint as piton.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(foundation_problem_death_or_transformation, conceptual, 'Whether the humanitarian obligation''s founding rationale remains the active justification.').

omega_variable(
    verification_cost_as_extractive_mechanism,
    'Are the rising administrative costs of asylum determination (biometrics, background checks, interview procedures, appeals) a necessary function of the humanitarian reading, or a form of suppression that states use to deny valid claims without rejecting the reading''s nominal premises?',
    'Cost-benefit analysis of verification procedures; comparison across jurisdictions of approval rates for similar claims; study of appeal reversal rates as indicator of first-instance decision quality.',
    'If verification is necessary, the suppression metric reflects genuine coordination cost. If verification is performative exclusion, the suppression metric represents state agency capture of the boundary-setting machinery — the constraint converts to snare despite humanitarian framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_cost_as_extractive_mechanism, empirical, 'Whether the humanitarian reading''s administrative machinery serves protection or exclusion.').

omega_variable(
    mixed_motive_migration_capture,
    'When migrants report both persecution and economic motivation, does the humanitarian reading''s categorical distinction require them to suppress one motive in asylum proceedings, leading to perverse incentives (false claims, narrative simplification, identity fusion with the refugee category)?',
    'Interviews with rejected asylum seekers and successful ones; qualitative analysis of asylum hearing transcripts; comparison of migrants'' pre-application self-reports to statements in formal proceedings.',
    'If the reading forces narrative simplification and suppresses mixed-motive truth, it operates as a snare disguised as rope — it extracts compliance with its categorical frame as the cost of protection. If mixed-motive cases are genuinely rare or handled flexibly, the reading sustains its humanitarian framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mixed_motive_migration_capture, empirical, 'Whether the humanitarian reading''s categorical binary suppresses legitimate mixed motivation and creates perverse incentives.').

omega_variable(
    sibling_reading_committer_frame,
    'Which reading of the border-legitimacy kernel is held by which states and international bodies, and on what grounds do they make the choice?',
    'Doctrinal analysis of state asylum law and treaty invocation; comparative constitutional law; interview with policy makers on the reasoning behind their border framing.',
    'Understanding the distribution of readings (which states/bodies adopt humanitarian vs. sovereignty vs. freedom-of-movement) is essential to understanding whether the humanitarian reading is the global norm or one faction''s position. This bears on whether the constraint represents international coordination or imposed doctrine.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_committer_frame, empirical, 'Distribution of border-legitimacy readings across state adoption and the justifications given.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__humanitarian_obligation_reading, 1951, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1951, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 1951, 0.22).
narrative_ontology:measurement_basis(bord_tr_t1951, observed).
narrative_ontology:measurement(bord_tr_t1980, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 1980, 0.32).
narrative_ontology:measurement_basis(bord_tr_t1980, observed).
narrative_ontology:measurement(bord_tr_t2000, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 2000, 0.38).
narrative_ontology:measurement_basis(bord_tr_t2000, observed).
narrative_ontology:measurement(bord_tr_t2010, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 2010, 0.43).
narrative_ontology:measurement_basis(bord_tr_t2010, observed).
narrative_ontology:measurement(bord_tr_t2018, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 2018, 0.47).
narrative_ontology:measurement_basis(bord_tr_t2018, observed).
narrative_ontology:measurement(bord_tr_t2024, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 2024, 0.48).
narrative_ontology:measurement_basis(bord_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(bord_be_t1951, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 1951, 0.35).
narrative_ontology:measurement_basis(bord_be_t1951, observed).
narrative_ontology:measurement(bord_be_t1980, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 1980, 0.42).
narrative_ontology:measurement_basis(bord_be_t1980, observed).
narrative_ontology:measurement(bord_be_t2000, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 2000, 0.48).
narrative_ontology:measurement_basis(bord_be_t2000, observed).
narrative_ontology:measurement(bord_be_t2010, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 2010, 0.54).
narrative_ontology:measurement_basis(bord_be_t2010, observed).
narrative_ontology:measurement(bord_be_t2018, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 2018, 0.58).
narrative_ontology:measurement_basis(bord_be_t2018, observed).
narrative_ontology:measurement(bord_be_t2024, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 2024, 0.58).
narrative_ontology:measurement_basis(bord_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1951, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 1951, 0.41).
narrative_ontology:measurement_basis(bord_su_t1951, observed).
narrative_ontology:measurement(bord_su_t1980, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 1980, 0.48).
narrative_ontology:measurement_basis(bord_su_t1980, observed).
narrative_ontology:measurement(bord_su_t2000, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement_basis(bord_su_t2000, observed).
narrative_ontology:measurement(bord_su_t2010, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 2010, 0.61).
narrative_ontology:measurement_basis(bord_su_t2010, observed).
narrative_ontology:measurement(bord_su_t2018, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 2018, 0.62).
narrative_ontology:measurement_basis(bord_su_t2018, observed).
narrative_ontology:measurement(bord_su_t2024, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 2024, 0.62).
narrative_ontology:measurement_basis(bord_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_legitimacy__humanitarian_obligation_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(border_legitimacy__humanitarian_obligation_reading, 0.18).
narrative_ontology:affects_constraint(border_legitimacy__humanitarian_obligation_reading, border_legitimacy_sovereignty_reading).
narrative_ontology:affects_constraint(border_legitimacy__humanitarian_obligation_reading, border_legitimacy_freedom_of_movement_reading).

% DUAL FORMULATION NOTE:
% The humanitarian-obligation reading is one of three structurally distinct readings of the border-legitimacy kernel. The sovereignty reading asserts states have unilateral right to exclude (ε_sovereignty ~0.15, mountain-candidate); the freedom-of-movement reading asserts movement is a human right and borders are presumptively illegitimate (ε_freedom ~0.65, snare). The humanitarian reading sits between: it grants protection as a duty but retains exclusion discretion (ε_humanitarian ~0.58, tangled rope). These are not observer-relative measurements of one constraint — they are three different constraints instantiated by three different readings of the same kernel. The readings coexist and influence each other: humanitarian pressures sovereignty states to explicit defense; freedom-of-movement pressures humanitarian states to broaden protection; sovereignty pressures humanitarian states to narrow beneficiary categories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(border_legitimacy__humanitarian_obligation_reading, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
