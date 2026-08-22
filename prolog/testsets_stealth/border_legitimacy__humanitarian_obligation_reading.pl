% ============================================================================
% CONSTRAINT STORY: border_legitimacy__humanitarian_obligation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: border_legitimacy__humanitarian_obligation_reading
 *   human_readable: Humanitarian Obligation Reading of Border Legitimacy (Persecution-Keyed Admission Duty)
 *   domain: political philosophy/international law/migration studies
 *
 * SUMMARY:
 *   This story authors the humanitarian-obligation settlement on state
 *   admission powers: border authority is treated as legitimate but bounded —
 *   states owe admission to those fleeing persecution or disaster and may
 *   exclude those moving for economic reasons. The arrangement is
 *   institutionally embodied in the Refugee Convention regime: a protected
 *   category with a (weakly remedied) admission duty attached, and a residual
 *   population whose exclusion the same framework affirmatively licenses. Its
 *   effects bifurcate: for category-fitting refugees it is a lifeline that
 *   solves a real collective-action problem in protection; for everyone
 *   outside the category it converts exclusion from a contested act into an
 *   administratively routine one. The claim and metrics are authored
 *   independently: the claimed type is tangled_rope (coordination and
 *   asymmetric extraction through one structure), while the metrics describe
 *   moderately extractive, increasingly theatrical, actively enforced
 *   operation — the engine computes per-seat classifications from the
 *   structural data, and any divergence between claim and computation is the
 *   datum.
 *
 * KEY AGENTS:
 *   - signatory_states: agenda-setter (institutional/constrained) — drafts and administers the categories, collects legitimacy and labor-market control, bears hosting costs
 *   - unhcr_office: supervisory beneficiary (institutional/identity_locked) — mandate, budget, and identity all flow from the category system it interprets
 *   - recognized_refugees: protected beneficiaries (powerless/trapped) — receive the lifeline where enforcement holds, often in degraded form
 *   - frontline_transit_states: cost-bearing hosts (organized/constrained) — absorb the largest arrival shares under first-asylum rules
 *   - excluded_economic_migrants: primary excluded payers (powerless/constrained) — bear categorical exclusion legitimized by the same framework
 *   - climate_displaced_persons: category-less payers (powerless/trapped) — fit neither branch of the sorting line
 *   - rejected_asylum_seekers: failed-category payers (powerless/trapped) — consumed by determination processes that end in removal or limbo
 *   - migration_law_scholars: analytical observers (analytical/analytical) — document the gap between obligation and delivery
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__humanitarian_obligation_reading, 0.58).
domain_priors:suppression_score(border_legitimacy__humanitarian_obligation_reading, 0.66).
domain_priors:theater_ratio(border_legitimacy__humanitarian_obligation_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__humanitarian_obligation_reading, tangled_rope).
narrative_ontology:human_readable(border_legitimacy__humanitarian_obligation_reading, "Humanitarian Obligation Reading of Border Legitimacy (Persecution-Keyed Admission Duty)").
narrative_ontology:topic_domain(border_legitimacy__humanitarian_obligation_reading, "political philosophy/international law/migration studies").

domain_priors:requires_active_enforcement(border_legitimacy__humanitarian_obligation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__humanitarian_obligation_reading, '445d14cf-2222-42d6-8d28-7616c349b874').
narrative_ontology:cs_kernel_codification('445d14cf-2222-42d6-8d28-7616c349b874', fixed_text).
narrative_ontology:cs_authority_grounding('445d14cf-2222-42d6-8d28-7616c349b874', lineage).
narrative_ontology:cs_interpretation_layer_present('445d14cf-2222-42d6-8d28-7616c349b874').
narrative_ontology:cs_reading_relation('445d14cf-2222-42d6-8d28-7616c349b874', border_legitimacy__sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('445d14cf-2222-42d6-8d28-7616c349b874', border_legitimacy__freedom_of_movement_reading, influences).
narrative_ontology:cs_axiom('445d14cf-2222-42d6-8d28-7616c349b874', foundational, persecution_flight_generates_admission_duty).
narrative_ontology:cs_axiom_status(persecution_flight_generates_admission_duty, holdable).
narrative_ontology:cs_axiom_grounding('445d14cf-2222-42d6-8d28-7616c349b874', persecution_flight_generates_admission_duty, deontological).
narrative_ontology:cs_axiom('445d14cf-2222-42d6-8d28-7616c349b874', foundational, economic_motivation_permissible_ground_for_exclusion).
narrative_ontology:cs_axiom_status(economic_motivation_permissible_ground_for_exclusion, holdable).
narrative_ontology:cs_axiom_grounding('445d14cf-2222-42d6-8d28-7616c349b874', economic_motivation_permissible_ground_for_exclusion, deontological).
narrative_ontology:cs_reference_frame('445d14cf-2222-42d6-8d28-7616c349b874', non_refoulement_bounded_sovereignty).
narrative_ontology:cs_drift_state('445d14cf-2222-42d6-8d28-7616c349b874', contemporary_mixed_movement_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('445d14cf-2222-42d6-8d28-7616c349b874', '').
narrative_ontology:cs_kernel_id(border_legitimacy__humanitarian_obligation_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, signatory_states).
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, recognized_refugees).
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, unhcr_office).
narrative_ontology:constraint_victim(border_legitimacy__humanitarian_obligation_reading, excluded_economic_migrants).
narrative_ontology:constraint_victim(border_legitimacy__humanitarian_obligation_reading, climate_displaced_persons).
narrative_ontology:constraint_victim(border_legitimacy__humanitarian_obligation_reading, rejected_asylum_seekers).
narrative_ontology:constraint_victim(border_legitimacy__humanitarian_obligation_reading, frontline_transit_states).
narrative_ontology:constraint_vindicates(border_legitimacy__humanitarian_obligation_reading, non_refoulement_principle).
narrative_ontology:constraint_vindicates(border_legitimacy__humanitarian_obligation_reading, persecution_economic_distinction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ratified the Refugee Convention and its Protocol and operate national asylum systems that sort arrivals into the protected category and the excludable remainder. They drafted the categories, administer status determination, and decide how generously to interpret them. They gain international standing for honoring admission duties and retain control over labor-market entry for everyone outside the category, while bearing the fiscal and political costs of hosting those they admit. Withdrawing from the regime would carry reputational and legal costs, so participation persists even where day-to-day compliance thins.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, signatory_states, agenda_setter,
    institutional, generational, constrained, national).

% Supervises the convention, issues interpretive guidance, runs field operations for the displaced, and advocates for expanded protection. Its mandate, budget, and diplomatic standing all flow from the category system it interprets; its organizational purpose is constituted by the distinction it administers. Repositioning around a different mission would dissolve the reason the office exists.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, unhcr_office, beneficiary,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(border_legitimacy__humanitarian_obligation_reading, unhcr_office, observer).

% Crossed a border and were found to fit the persecution category. Where recognition works they receive non-deportability, documentation, and sometimes residence and work rights; in practice many spend years or decades in camps or precarious legality, with mobility and employment sharply limited. Their status depends entirely on continuing to fit the category and on the host state's willingness to honor what recognition promises.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, recognized_refugees, beneficiary,
    powerless, biographical, trapped, global).

% Sit along the main flight corridors and receive the largest absolute shares of displaced arrivals under first-asylum rules. They host millions at proportionally greater fiscal strain than wealthier destinations, while resettlement transfers to them remain small and discretionary. Geography removes the option of declining to be a neighbor to crisis.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, frontline_transit_states, payer,
    organized, generational, constrained, regional).

% Move for poverty, joblessness, or livelihood collapse rather than targeted persecution. The arrangement places them wholly outside any admission duty, so states may refuse, detain, and remove them without owing justification beyond their own domestic law. Their options are irregular movement through ever-hardening enforcement or remaining in failing economies; the line that excludes them was drawn without their participation.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, excluded_economic_migrants, payer,
    powerless, biographical, constrained, global).

% Flee floods, drought, storm destruction, and slow-onset environmental collapse. They fit neither the persecution category (there is no persecutor) nor the voluntary economic category (they did not choose to stay), so most fall through the sorting system entirely and hold no admission right anywhere. Their numbers grow as the climate signal strengthens.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, climate_displaced_persons, payer,
    powerless, biographical, trapped, regional).

% Entered the asylum process and were found not to fit the category — claims judged economic, an internal-flight alternative deemed available, or credibility doubted. After rejection they face removal to the conditions they fled, prolonged detention, or life as undocumented residents barred from lawful work. The determination process consumes years of their lives in either outcome.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, rejected_asylum_seekers, payer,
    powerless, biographical, trapped, national).

% Study the doctrine, its drafting history, and its application across jurisdictions. They publish critiques of the category line, document the gap between promised obligation and delivered protection, and supply the arguments that both defenders and opponents of the arrangement rely on. They hold no operational power over any border.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, migration_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_legitimacy__humanitarian_obligation_reading, signatory_states).
narrative_ontology:fixing_cost_class(border_legitimacy__humanitarian_obligation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of forced-displacement protection: no state absorbs refugees unilaterally if others may free-ride, so a shared definition and a mutual (if weakly enforced) admission duty distribute responsibility and stabilize expectations about who must be admitted. It also standardizes status determination so that a finding in one state is legible in others.
% TRANSFER_FUNCTION: Moves hosting obligations and their costs onto whichever state the displaced person reaches first; moves international legitimacy to states that comply visibly; moves the costs of exclusion — detention, removal, family separation — onto migrants outside the protected category; and concentrates interpretive authority in supervisory bodies.
% ABSENT_VOICES: The people sorted by the line were absent from its drawing: no economic migrants or climate-displaced persons sat at the 1951 table, and contemporary compact negotiations consult states and agencies far more than the mobile populations affected. Their objection — that the line tracks the drafters' priorities and destination-state convenience rather than the urgency of need — enters only through advocates and scholars speaking on their behalf.
% DISAPPEARANCE_RATIONALE: If the persecution-keyed admission duty vanished overnight, recognized refugees would lose their principal legal shield against removal, the supervisory office would lose its object, status-determination systems across dozens of states would lose their organizing statute, and exclusion of everyone else would proceed without even the current humanitarian limit — the entire architecture of asylum would have to be rebuilt from nothing.
% FOUNDING_PROBLEM: Post-Second-World-War displacement: tens of millions uprooted in Europe, states unwilling to absorb them unilaterally, and a need for a shared rule keyed to the era's paradigmatic harm — targeted political persecution.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem itself is well corroborated outside the beneficiary set: drafting history, contemporaneous diplomatic records, and demographic documentation of postwar displacement. Its current status is disputed: signatory states attest the persecution problem remains live; supervisory-agency mixed-movement guidance and the climate-displacement literature — voices with partial independence from the benefiting states — attest that the original category no longer covers the dominant displacement drivers; the affected migrant populations themselves, the least consulted seat, overwhelmingly experience the line as serving destinations rather than the displaced.
narrative_ontology:disappearance_verdict(border_legitimacy__humanitarian_obligation_reading, world_rearranges).
narrative_ontology:founding_problem_status(border_legitimacy__humanitarian_obligation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_legitimacy__humanitarian_obligation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(border_legitimacy__humanitarian_obligation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(border_legitimacy__humanitarian_obligation_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is moderate (0.58 at interval end) because the arrangement extracts asymmetrically: the exclusion half imposes categorical costs on a large population while the protection half delivers genuine but thinning benefit. Suppression (0.66) is higher than extraction because persistence depends on an enforcement apparatus aimed at the excluded — visa regimes, carrier sanctions, interdiction, offshore processing, pushbacks — not on participant preference; suppression is authored as a raw structural property and is deliberately left unscaled, since only extractiveness is scaled by directionality and scope in the engine's computation. Theater ratio (0.45) reflects a compliance layer that has grown progressively more symbolic: resettlement pledges unmet, non-binding compacts celebrated, summits substituting for burden-sharing. Accessibility collapse is low (0.35): alternative admission orders — points systems, purely discretionary closure, open admission, bilateral deals — remain live and practiced, so understanding the arrangement does not foreclose its rivals. Resistance (0.55) is real on both flanks: states resist expansion of the duty (reservations, non-ratification, compact abstentions) while migrants and advocates resist the exclusion line itself. The temporal series run on one shared eight-point grid (all three metrics authored at every point) so no end-state value leaks backward into earlier rows; the trajectories show extraction accumulating as the category drifts from its caseload, theater rising as compliance turns performative, and the suppression requirement ratcheting upward as enforcement infrastructure matured across the interval.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute very differently. From the signatory-state seat the arrangement appears as a managed balance: humanitarian duty honored at tolerable cost, labor entry retained as sovereign prerogative. From the excluded-migrant and climate-displaced seats the same structure is a sorting machine whose output was fixed before they arrived in the conversation. From the recognized-refugee seat it is a lifeline with holes — real non-deportability attached to encampment and decades-long limbo. The sharpest same-level divergence runs between frontline transit states and wealthy destination states: nominally equal signatories with radically different cost exposure, because geography traps the former while wealth lets the latter externalize enforcement to neighbors and private carriers. The scholarly seat sees the whole structure and no part of its operation.
 *
 * DIRECTIONALITY LOGIC:
 *   Signatory states sit nearest the beneficiary end (they collect legitimacy and labor-market control) but are not pure beneficiaries — they pay hosting costs, which keeps their derived directionality from reaching zero; the commentary flags this mixed position rather than overriding it, since the override surface is keyed by power atom and would misfire on other institutional seats. Recognized refugees are declared beneficiaries with trapped exit: the derivation places them low-d, but the nominal-versus-delivered-protection omega records that their effective benefit is thinner than their formal position suggests. Excluded economic migrants, climate-displaced persons, and rejected asylum seekers are declared victims with trapped or constrained exit, placing them near the full-target end — trapped or identity-locked targets sit nearer full-target than mobile ones, and none of these seats holds arbitrage-grade exit. UNHCR is an identity-locked beneficiary: its institutional identity is fused with the category system, so its directionality derives low while its incentives favor the system's continuation. Frontline transit states are victims by cost incidence despite being regime insiders — their declaration captures the first-asylum cost transfer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (postwar persecution displacement) is authored as contested, not dead: targeted persecution persists, so the arrangement has not outlived its function outright — but its signature distinction increasingly fails to cover the displacement actually occurring. Classifying the whole as tangled_rope guards against the two symmetrical mislabelings: reading it as pure humanitarian coordination (which erases the excluded populations who pay for the framework's legitimacy) or as pure extraction (which erases the real protection delivered to the recognized). The mismatch consumer reads founding_problem_status x disappearance_verdict: contested-plus-world_rearranges does not trip the zombie flag, correctly, because the protective function is still performed for a subset — but the rising theater_ratio series marks the drift path worth watching: if compliance goes fully symbolic while the exclusion half hardens, the arrangement migrates toward the extractive pole with the humanitarian vocabulary intact.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_membership,
    'This story instantiates the humanitarian_obligation_reading of the border_legitimacy kernel; what structural differences would instantiating the sibling readings (sovereignty_reading, freedom_of_movement_reading) produce?',
    'Author the sibling stories as separate constraints and compare computed victim sets, epsilon, and per-seat classifications across the three files.',
    'Under the sovereignty reading the admission carve-out disappears — all unauthorized entrants share one victim set and epsilon rises; under the freedom-of-movement reading the exclusion license disappears — the categorical line itself becomes the contested object and the refugee/economic bifurcation collapses into a single boundary dispute.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_membership, conceptual, 'Kernel membership: one of three readings of border_legitimacy; sibling readings are separate constraints.').

omega_variable(
    disagreement_location_motive_relevance,
    'Where exactly do the readings disagree — is the legitimacy-relevant variable the migrant''s motive and flight-cause (this reading), the state''s sovereign discretion (sovereignty_reading), or the migrant''s need alone (freedom_of_movement_reading)?',
    'Conceptual analysis of which variable each reading''s axioms treat as decision-relevant, cross-checked against how each reading sorts hard cases such as climate flight and mixed-movement flows.',
    'If motive is not the real locus of disagreement, this reading''s bifurcated victim set is unstable and the three-way decomposition may reduce to a two-way contest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disagreement_location_motive_relevance, conceptual, 'Locating the axis on which the three readings of the kernel actually diverge.').

omega_variable(
    category_line_stability_climate_displacement,
    'Does the persecution/economic distinction remain a coherent sorting line as climate- and disaster-driven displacement grows toward dominance in forced migration?',
    'Track status-determination outcomes and supervisory-agency guidance for climate-displaced applicants over coming decades; watch for doctrinal extension (complementary protection, regional instruments) absorbing the category.',
    'If the line dissolves, the bifurcated victim set collapses into one population excluded by a category that no longer tracks its justification — epsilon rises and the arrangement drifts toward pure exclusion maintenance wearing humanitarian vocabulary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(category_line_stability_climate_displacement, empirical, 'Whether the reading''s signature distinction survives its changing caseload.').

omega_variable(
    nominal_vs_delivered_protection,
    'Is the protection delivered to recognized refugees effective (mobility, work rights, durable residence) or largely nominal (encampment, protracted exile, non-deportability without membership)?',
    'Compare statutory entitlements with outcome data across major host states: camp duration distributions, lawful-work access rates, naturalization rates.',
    'If protection is largely nominal, recognized_refugees shift from effective beneficiaries toward payers, eroding the coordination half of the structure and pushing the computed classification toward the extractive pole.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nominal_vs_delivered_protection, empirical, 'Whether the refugee half of the bifurcation delivers real benefit or formal status only.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__humanitarian_obligation_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(blh_reading_tr_t0, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement_basis(blh_reading_tr_t0, observed).
narrative_ontology:measurement(blh_reading_tr_t10, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement_basis(blh_reading_tr_t10, observed).
narrative_ontology:measurement(blh_reading_tr_t20, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement_basis(blh_reading_tr_t20, observed).
narrative_ontology:measurement(blh_reading_tr_t30, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 30, 0.29).
narrative_ontology:measurement_basis(blh_reading_tr_t30, observed).
narrative_ontology:measurement(blh_reading_tr_t40, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement_basis(blh_reading_tr_t40, observed).
narrative_ontology:measurement(blh_reading_tr_t50, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 50, 0.4).
narrative_ontology:measurement_basis(blh_reading_tr_t50, observed).
narrative_ontology:measurement(blh_reading_tr_t60, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 60, 0.43).
narrative_ontology:measurement_basis(blh_reading_tr_t60, observed).
narrative_ontology:measurement(blh_reading_tr_t70, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 70, 0.45).
narrative_ontology:measurement_basis(blh_reading_tr_t70, observed).

% Extraction over time
narrative_ontology:measurement(blh_reading_be_t0, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement_basis(blh_reading_be_t0, observed).
narrative_ontology:measurement(blh_reading_be_t10, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement_basis(blh_reading_be_t10, observed).
narrative_ontology:measurement(blh_reading_be_t20, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 20, 0.44).
narrative_ontology:measurement_basis(blh_reading_be_t20, observed).
narrative_ontology:measurement(blh_reading_be_t30, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 30, 0.48).
narrative_ontology:measurement_basis(blh_reading_be_t30, observed).
narrative_ontology:measurement(blh_reading_be_t40, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 40, 0.52).
narrative_ontology:measurement_basis(blh_reading_be_t40, observed).
narrative_ontology:measurement(blh_reading_be_t50, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 50, 0.55).
narrative_ontology:measurement_basis(blh_reading_be_t50, observed).
narrative_ontology:measurement(blh_reading_be_t60, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 60, 0.57).
narrative_ontology:measurement_basis(blh_reading_be_t60, observed).
narrative_ontology:measurement(blh_reading_be_t70, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 70, 0.58).
narrative_ontology:measurement_basis(blh_reading_be_t70, observed).

% Suppression requirement over time
narrative_ontology:measurement(blh_reading_su_t0, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(blh_reading_su_t0, observed).
narrative_ontology:measurement(blh_reading_su_t10, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 10, 0.36).
narrative_ontology:measurement_basis(blh_reading_su_t10, observed).
narrative_ontology:measurement(blh_reading_su_t20, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement_basis(blh_reading_su_t20, observed).
narrative_ontology:measurement(blh_reading_su_t30, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 30, 0.48).
narrative_ontology:measurement_basis(blh_reading_su_t30, observed).
narrative_ontology:measurement(blh_reading_su_t40, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 40, 0.55).
narrative_ontology:measurement_basis(blh_reading_su_t40, observed).
narrative_ontology:measurement(blh_reading_su_t50, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 50, 0.61).
narrative_ontology:measurement_basis(blh_reading_su_t50, observed).
narrative_ontology:measurement(blh_reading_su_t60, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 60, 0.64).
narrative_ontology:measurement_basis(blh_reading_su_t60, observed).
narrative_ontology:measurement(blh_reading_su_t70, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 70, 0.66).
narrative_ontology:measurement_basis(blh_reading_su_t70, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_legitimacy__humanitarian_obligation_reading, resource_allocation).
narrative_ontology:affects_constraint(border_legitimacy__humanitarian_obligation_reading, border_legitimacy__sovereignty_reading).
narrative_ontology:affects_constraint(border_legitimacy__humanitarian_obligation_reading, border_legitimacy__freedom_of_movement_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'border legitimacy' decomposes into three structurally distinct constraints — sovereignty_reading (unbounded exclusion authority), humanitarian_obligation_reading (this file: persecution-keyed admission duty with licensed exclusion of the rest), and freedom_of_movement_reading (presumptively illegitimate border restriction). Each carries its own epsilon, victim set, and claimed type; they are linked here because the humanitarian reading structurally mediates between the other two — it inherits the exclusion license from the sovereignty settlement and supplies the carve-out that freedom-of-movement advocates attack as the wedge case. The upstream sovereignty settlement (older, more entrenched) influences this reading; this reading influences the downstream open-borders contest by conceding the state's standing to filter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
