% ============================================================================
% CONSTRAINT STORY: imposition_pathway_kernel__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_pathway_kernel__exogenous_override_reading, []).

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
 *   constraint_id: imposition_pathway_kernel__exogenous_override_reading
 *   human_readable: State-Imposed Commitment Displacement (Exogenous Override Reading)
 *   domain: historical_sociology/state_formation/commitment_systems
 *
 * SUMMARY:
 *   The exogenous_override_reading instantiates one reading of the contested
 *   kernel of commitment displacement mechanisms. The kernel asks: how do
 *   societies shift from one coordinating commitment to another? This reading
 *   asserts that state capacity enables top-down imposition WITHOUT
 *   meaningful fringe adoption pathways — new commitments can be created
 *   through decree and enforcement alone, distinct from the gradual climb
 *   model where commitments percolate from fringe adoptions. The case
 *   exemplar is the Meiji Restoration's calendar change (1873) and dress-code
 *   reforms: no domestic merchant or artisan community had independently
 *   adopted the Gregorian calendar or Western dress before state decree; the
 *   state created new coordination through enforcement machinery, not through
 *   invisible fringe climb. This reading argues the M-set framework (a
 *   mathematical model of commitment selection dynamics) is incomplete
 *   without a distinct cell for exogenous override — the override is not a
 *   compressed climb but a mechanistically different pathway requiring
 *   separate analysis. The claim-metric gap is intentional: the constraint is
 *   claimed as tangled_rope (genuine state coordination function + asymmetric
 *   extraction from those who bear the cost of transition) while the authored
 *   metrics show high suppression and extraction — the engine computes
 *   whether the coordination narrative can sustain the measured suppression;
 *   divergence signals false coordination cover.
 *
 * KEY AGENTS:
 *   - centralizing_state_apparatus: Institutional power, generalizes commitment change at national scope, controls enforcement machinery, benefits from coordination standardization that enables taxation, conscription, and state administration.
 *   - bearers_of_prior_commitment: Moderate-to-powerless agents organized around prior commitments (religious calendar systems, traditional dress, local trade norms), face transition cost and identity disruption, constrained exit due to geographic and class position.
 *   - dissenting_populations: Those who refuse or resist the new commitment, suppressed directly (religious minorities forbidden from calendar observance) or indirectly (penalties for non-compliance embedded in administration, education, market access).
 *   - fringe_early_adopters_counterfactual: The analytical seat investigating whether pre-decree fringe adoption occurred — their absence or presence determines the reading's empirical status.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_pathway_kernel__exogenous_override_reading, 0.68).
domain_priors:suppression_score(imposition_pathway_kernel__exogenous_override_reading, 0.79).
domain_priors:theater_ratio(imposition_pathway_kernel__exogenous_override_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_pathway_kernel__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(imposition_pathway_kernel__exogenous_override_reading, "State-Imposed Commitment Displacement (Exogenous Override Reading)").
narrative_ontology:topic_domain(imposition_pathway_kernel__exogenous_override_reading, "historical_sociology/state_formation/commitment_systems").

domain_priors:requires_active_enforcement(imposition_pathway_kernel__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_pathway_kernel__exogenous_override_reading, 'f270f61f-4d6d-4e5a-89d9-895d8030908b').
narrative_ontology:cs_kernel_codification('f270f61f-4d6d-4e5a-89d9-895d8030908b', distributed).
narrative_ontology:cs_authority_grounding('f270f61f-4d6d-4e5a-89d9-895d8030908b', extraction).
narrative_ontology:cs_reading_relation('f270f61f-4d6d-4e5a-89d9-895d8030908b', imposition_pathway_kernel__endogenous_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('f270f61f-4d6d-4e5a-89d9-895d8030908b', imposition_pathway_kernel__hybrid_cascade_reading, influences).
narrative_ontology:cs_axiom('f270f61f-4d6d-4e5a-89d9-895d8030908b', foundational, state_capacity_enables_commitment_override_without_fringe_adoption).
narrative_ontology:cs_axiom_status(state_capacity_enables_commitment_override_without_fringe_adoption, holdable).
narrative_ontology:cs_axiom_grounding('f270f61f-4d6d-4e5a-89d9-895d8030908b', state_capacity_enables_commitment_override_without_fringe_adoption, empirically_contingent).
narrative_ontology:cs_axiom('f270f61f-4d6d-4e5a-89d9-895d8030908b', foundational, fringe_adoption_was_negligible_pre_decree).
narrative_ontology:cs_axiom_status(fringe_adoption_was_negligible_pre_decree, holdable).
narrative_ontology:cs_axiom_grounding('f270f61f-4d6d-4e5a-89d9-895d8030908b', fringe_adoption_was_negligible_pre_decree, empirically_contingent).
narrative_ontology:cs_reference_frame('f270f61f-4d6d-4e5a-89d9-895d8030908b', prior_commitment_heterogeneity).
narrative_ontology:cs_drift_state('f270f61f-4d6d-4e5a-89d9-895d8030908b', post_decree_stabilization, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('f270f61f-4d6d-4e5a-89d9-895d8030908b', '').
narrative_ontology:cs_kernel_id(imposition_pathway_kernel__exogenous_override_reading, imposition_pathway_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__exogenous_override_reading, centralizing_state_apparatus).
narrative_ontology:constraint_victim(imposition_pathway_kernel__exogenous_override_reading, bearers_of_prior_commitment).
narrative_ontology:constraint_victim(imposition_pathway_kernel__exogenous_override_reading, dissenting_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__exogenous_override_reading, state_employees_and_military).
narrative_ontology:constraint_victim(imposition_pathway_kernel__exogenous_override_reading, state_employees_and_military).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the new commitment (calendar, dress, administrative norm) through decree and enforces it through administrative and military machinery. Controls the timing, content, and enforcement intensity of the override. Benefits from standardized coordination that enables taxation, conscription, and administrative efficiency. Justifies the override as modernization and alignment with international practice. The override is instrumentally chosen to solve state capacity problems, not organically adopted.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, centralizing_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Organized around prior commitment (religious calendar observance, traditional dress, local trade norms). Face transition cost: loss of utility in the prior system (calendar no longer marks religious festivals correctly, dress no longer signals membership), identity disruption (self-conception tied to prior commitment), and suppression for non-compliance (penalties in administration, market access, education). Geographic and class position constrains exit — cannot leave the nation-state jurisdiction. Compliance is forced, not emergent from recognition of coordination benefit.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, bearers_of_prior_commitment, payer,
    moderate, generational, constrained, national).

% Explicitly refuse or resist the new commitment on religious, cultural, or political grounds (e.g., religious minorities forbidden from calendar observance, groups for whom Western dress violates sacred law). Face direct suppression: legal penalties, exclusion from public roles, forced conversion to the new practice. Have no organizational capacity to mount sustained resistance and no geographic exit. Suppression is active and direct — enforcement machinery targets them specifically.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, dissenting_populations, payer,
    powerless, immediate, trapped, national).

% Required to adopt the new commitment as a condition of employment (wear Western dress, use the Gregorian calendar for administrative records). They benefit from the state apparatus's coordination standardization (hierarchical clarity, alignment with international state practice) and from career advancement tied to the new commitment. They also bear transition cost and the psychological cost of mandatory cultural change. The hybrid reading identifies them as the artificial fringe created by state mandate, from which organic climb might emerge; the exogenous reading treats them as enforcement machinery, not as independent adopters.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, state_employees_and_military, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(imposition_pathway_kernel__exogenous_override_reading, state_employees_and_military, payer).

% Would naturally adopt new calendars and dress for competitive advantage in international trade if their adoption were voluntary and pre-decree; the exogenous reading asserts they did NOT do so before the Meiji decree (negligible fringe adoption). They are excluded from the conversation about whether fringe adoption was occurring because the state decree pre-empts their choice. The endogenous reading would require evidence of hidden merchant-community coordination experiments (smuggled foreign calendars, underground dress codes) before decree; the absence of such evidence supports the exogenous reading.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, merchant_communities_and_early_traders, excluded,
    organized, biographical, mobile, regional).

% Examine archival records, merchant documents, religious texts, and personal correspondence to determine whether pre-decree fringe adoption occurred. They test the exogenous reading's core empirical claim (fringe adoption was negligible) against the endogenous reading's claim (fringe adoption was present but compressed or invisible). Their analysis determines which reading is empirically supported and whether the M-set framework requires an override cell.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, comparative_historical_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imposition_pathway_kernel__exogenous_override_reading, centralizing_state_apparatus).
narrative_ontology:fixing_cost_class(imposition_pathway_kernel__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables standardized calendar and dress across the nation-state, solving state administrative problems: reliable inter-regional scheduling, unified legal/military uniform, alignment with international state practice. The coordination problem the state solves is internal (diverse local calendars and dress make nation-wide administration difficult) and external (international trade and diplomacy require shared standards). The reading asserts this coordination is achieved through enforcement without fringe adoption pathways — the state creates the coordination, not fringe populations.
% TRANSFER_FUNCTION: Transfers identity, cultural practice, and administrative cost from prior-commitment bearers to the state apparatus and its beneficiaries. The state extracts the rent of coordination standardization (faster tax collection, more reliable conscription, administrative clarity) without compensating the populations who lose utility in the prior system. Secondary transfer: dissenting populations lose religious practice and face suppression; state employees gain administrative clarity and career advancement but lose cultural autonomy.
% ABSENT_VOICES: Pre-decree fringe communities that might have adopted the new commitment voluntarily (merchant networks experimenting with Gregorian calendar for trade, artisans adopting Western techniques and dress) are excluded from the visible record. The exogenous reading asserts their voice was absent because fringe adoption was negligible; the endogenous reading claims they were present but invisible or compressed. Comparative historians examining archival evidence would have these voices if they existed; their absence supports the exogenous reading.
% DISAPPEARANCE_RATIONALE: If the state override decree disappeared and enforcement ceased, populations would revert to prior commitments (religious calendars, traditional dress) within weeks. The new coordination would collapse because it was never adopted endogenously; it persisted only through enforcement. The state would lose administrative efficiency, conscription would become difficult (local populations timing obligations by religious calendar), and international trade coordination would suffer. The world would rearrange around prior commitments unless suppression became self-sustaining through internalization — the exogenous reading predicts slow internalization (next generation accepts the new commitment as natural) which would make disappearance harder to reverse.
% FOUNDING_PROBLEM: Enable the centralizing state to project power across diverse populations and territories through unified administrative, legal, and military systems. Prior commitment fragmentation (regional calendars, local dress codes, diverse trade norms) makes national-scale taxation, conscription, and command difficult. The state needs a single commitment across the population to standardize timing, dress, and practice.
% FOUNDING_PROBLEM_CORROBORATION: The Meiji state itself attests the problem was live: official histories, policy documents, and reform justifications emphasize the need for modernization and international alignment as prerequisites for state capacity. Historians and comparative sociologists outside the benefiting state apparatus corroborate: analysis of how nation-states function confirms that unified calendars and administrative dress codes are necessary for large-scale coordination. However, the exogenous reading's core claim — that this coordination requires state override WITHOUT fringe adoption pathways — is contested by the endogenous reading. Fringe adoption evidence is the crux: if merchant communities had independently experimented with the Gregorian calendar before 1873 (as occurred in some European countries), the corroboration weakens and the endogenous reading gains support. Archival evidence for Japan consistently shows no evidence of independent Gregorian adoption before the decree, supporting the exogenous reading's claim that the state created the commitment through pure override.
narrative_ontology:disappearance_verdict(imposition_pathway_kernel__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_pathway_kernel__exogenous_override_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_pathway_kernel__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(imposition_pathway_kernel__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_pathway_kernel__exogenous_override_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_pathway_kernel__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imposition_pathway_kernel__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imposition_pathway_kernel__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval midpoint, stabilizing) because the state extracts compliance and transitions behavioral coordination without compensating the bearers of prior commitment; suppression is very high (0.79) because the override depends on enforcement machinery that penalizes non-compliance; theater is moderate-low (0.28) because the coordination benefit is real (standardized calendar enables reliable scheduling across regions, Western dress becomes administrative uniform for state employees) but grows smaller over time relative to the extraction function once compliance is forced. The measurement trajectory shows extraction rising from 0.55 at decree through 0.68 by year 10 (as the state elaborates enforcement to non-compliant populations), then stabilizing as suppression declines — the reading predicts that once internalization occurs (next generation accepts the new commitment as natural), suppression can decrease while extraction persists as the new coordination rent. The trajectory distinguishes this reading from the hybrid reading, which would show suppression declining faster as artificial fringe becomes organic; here, suppression remains high 10+ years post-decree because bearers of prior commitment and dissenting populations continue to be actively suppressed rather than incorporated into climb.
 *
 * PERSPECTIVAL GAP:
 *   From the state apparatus's seat, the override is legitimate coordination modernization (calendar efficiency, dress standardization for administration, alignment with international commerce). From the seat of prior-commitment bearers, it is coerced abandonment of a functional system and identity restructuring. From the dissenting population's seat, it is suppression targeting their religious/cultural practice. The engine computes these seat-specific classifications from power, exit, and the structural assertion that enforcement is required. The authored claim (tangled_rope) is the state apparatus's reading; the victims' seat should compute toward snare. The divergence is where the analysis sits.
 *
 * DIRECTIONALITY LOGIC:
 *   The state apparatus is the full beneficiary (d near 0.0): it controls the new commitment, collects the coordination rent (tax collection becomes possible, conscription becomes feasible, administration unifies), and incurs no transition cost. Bearers of prior commitment are full targets (d near 1.0): they lose the identity/utility of the prior commitment, face suppression for non-compliance, and cannot exit (geographic constraint, class constraint, organizational constraint). Dissenting populations are maximally targeted (d = 1.0): suppression is applied directly to maintain the new commitment against their resistance. The fringe early adopters (the counterfactual seat investigating whether they existed) have analytical directionality — they are the crux of the reading's claims against siblings.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is 'enable national-scale coordination for taxation, conscription, and administration' — distinct from the endogenous reading's founding problem ('solve a coordination problem that fringe populations are already solving'). This reading asserts the founding problem is live: the state continues to benefit from unified calendar, dress, and administrative standards. But contestation arises from the exogenous reading's claim: if the coordination could have emerged from fringe adoption (the endogenous reading's alternative), then the state's override is mandatroph — the founding problem is solved without the state, but the state extracted rent by foreclosing the alternative. The exogenous reading commits to the claim that fringe adoption was negligible (zero or near-zero before decree), which if falsified would move the constraint toward scaffold (temporary override that enabled organic climb) rather than permanent tangled_rope. The measurement trajectory should show whether suppression persists because the commitment hasn't internalized (lives on as enforced) or declines because internalization succeeded (the override was temporary bridge). High suppression persistence suggests mandatroph (override extracted rent from suppression even after the coordination problem was solved).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fringe_visibility_asymmetry,
    'Can pre-decree fringe adoption be ruled out empirically, or is the endogenous_climb_reading''s claim about invisible fringe stages unfalsifiable?',
    'Archival evidence of pre-decree adoption rates, merchant records, religious texts, personal correspondence from the relevant period. The exogenous reading requires evidence that fringe adoption was negligible; the endogenous reading requires evidence that fringe stages occurred but were compressed/unrecorded.',
    'If pre-decree fringe adoption was empirically zero or near-zero (Meiji calendar case: no domestic merchants independently using the Gregorian calendar before 1873 decree), the exogenous reading''s mechanistic claim is supported. If fringe adoption was present but obscured, the endogenous reading retains purchase. This determines whether commitment displacement requires a distinct override cell in the M-set or fits the endogenous climb model.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fringe_visibility_asymmetry, empirical, 'Whether pre-decree fringe adoption was negligible or compressed/invisible.').

omega_variable(
    coercion_vs_internalization_boundary,
    'Does state enforcement of exogenous commitment displacement produce internalized compliance (the commitment becomes ''natural'' within one generation) or sustained structural suppression (compliance persists only under active enforcement)?',
    'Intergenerational compliance trajectories: if post-decree resistance drops sharply and remains low without enforcement intensification, internalization occurred; if resistance is suppressed only by continued enforcement machinery, structural suppression persists. Compare decay of enforcement apparatus vs. compliance trajectories.',
    'If internalization occurs, the exogenous override becomes a transition mechanism (temporary imposition → internalized commitment) and may reclassify to scaffold rather than tangled_rope. If suppression is structural, the override is a permanent extraction mechanism and remains snare or tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_vs_internalization_boundary, empirical, 'Whether exogenous imposition produces internalized commitment or sustained suppression.').

omega_variable(
    alternative_framing_kernel_reading,
    'Is this reading instantiating the kernel of ''commitment displacement mechanism selection'' (exogenous vs. endogenous as empirical fact), or the kernel of ''state authority to override prior commitments'' (exogenous as legitimacy claim)?',
    'Examine the contemporary justification texts (Meiji state ideology, decrees, official histories): if the emphasis is on efficiency/mechanism (''the fastest way to modernize''), the reading is mechanistic; if the emphasis is on state right/authority (''the state''s prerogative to reset the social contract''), the reading is legitimacy-grounded.',
    'A mechanistic reading supports the M-set framework and pure empirical resolution via fringe-adoption evidence. A legitimacy reading shifts the kernel to authority structure and requires analysis of how override authority is grounded and contested. The exogenous_override_reading as authored here assumes the mechanistic framing; a legitimacy-grounded sibling would be a separate constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_framing_kernel_reading, conceptual, 'Whether the kernel is mechanistic (override as distinct pathway) or legitimacy-grounded (override as authority claim).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_pathway_kernel__exogenous_override_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t0, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(impo_tr_t0, observed).
narrative_ontology:measurement(impo_tr_t5, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 5, 0.19).
narrative_ontology:measurement_basis(impo_tr_t5, observed).
narrative_ontology:measurement(impo_tr_t10, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement_basis(impo_tr_t10, observed).
narrative_ontology:measurement(impo_tr_t15, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 15, 0.27).
narrative_ontology:measurement_basis(impo_tr_t15, observed).
narrative_ontology:measurement(impo_tr_t20, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement_basis(impo_tr_t20, observed).
narrative_ontology:measurement(impo_tr_t25, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement_basis(impo_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(impo_be_t0, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(impo_be_t0, observed).
narrative_ontology:measurement(impo_be_t5, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 5, 0.61).
narrative_ontology:measurement_basis(impo_be_t5, observed).
narrative_ontology:measurement(impo_be_t10, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 10, 0.66).
narrative_ontology:measurement_basis(impo_be_t10, observed).
narrative_ontology:measurement(impo_be_t15, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 15, 0.68).
narrative_ontology:measurement_basis(impo_be_t15, observed).
narrative_ontology:measurement(impo_be_t20, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(impo_be_t20, observed).
narrative_ontology:measurement(impo_be_t25, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(impo_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t0, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement_basis(impo_su_t0, observed).
narrative_ontology:measurement(impo_su_t5, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 5, 0.83).
narrative_ontology:measurement_basis(impo_su_t5, observed).
narrative_ontology:measurement(impo_su_t10, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 10, 0.8).
narrative_ontology:measurement_basis(impo_su_t10, observed).
narrative_ontology:measurement(impo_su_t15, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 15, 0.79).
narrative_ontology:measurement_basis(impo_su_t15, observed).
narrative_ontology:measurement(impo_su_t20, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 20, 0.78).
narrative_ontology:measurement_basis(impo_su_t20, observed).
narrative_ontology:measurement(impo_su_t25, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 25, 0.79).
narrative_ontology:measurement_basis(impo_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_pathway_kernel__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(imposition_pathway_kernel__exogenous_override_reading, 0.18).
narrative_ontology:affects_constraint(imposition_pathway_kernel__exogenous_override_reading, imposition_pathway_kernel__endogenous_climb_reading).
narrative_ontology:affects_constraint(imposition_pathway_kernel__exogenous_override_reading, imposition_pathway_kernel__hybrid_cascade_reading).

% DUAL FORMULATION NOTE:
% The imposition_pathway_kernel has three readings corresponding to three distinct mechanistic claims about how commitments displace: (1) exogenous_override_reading (this constraint): state capacity enables pure top-down imposition without fringe adoption; override is mechanistically distinct. (2) endogenous_climb_reading: all displacement is endogenous fringe climb; apparent imposition is compressed climb with invisible fringe stages. (3) hybrid_cascade_reading: override creates artificial fringe (state employees, military) which then undergoes organic climb; override initiates, climb completes. These are NOT three perspectives on one constraint; they are three constraints on a single kernel, with distinct ε values (empirical claim about fringe adoption differs across readings), distinct beneficiary/victim structures (fringe agents appear in hybrid and endogenous but not exogenous), and distinct extinction conditions (fringe-adoption evidence falsifies exogenous reading). Network edges link them because they share the same kernel and their empirical status is interdependent: if pre-decree fringe adoption was negligible (exogenous claim), endogenous reading is falsified; if fringe adoption was present (endogenous claim), exogenous reading is falsified. Hybrid cascade is compatible with either exogenous or endogenous if the state-created artificial fringe successfully converts override to climb. The three readings form a constraint family where the shared kernel is the point of departure and mechanistic alternatives compete for the same empirical record.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(imposition_pathway_kernel__exogenous_override_reading, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
