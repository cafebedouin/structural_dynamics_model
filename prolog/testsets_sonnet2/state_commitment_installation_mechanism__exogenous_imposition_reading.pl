% ============================================================================
% CONSTRAINT STORY: state_commitment_installation_mechanism__exogenous_imposition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_commitment_installation_mechanism__exogenous_imposition_reading, []).

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
 *   constraint_id: state_commitment_installation_mechanism__exogenous_imposition_reading
 *   human_readable: Top-Down Decree Installation of New Institutional Commitments
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   A central state, holding or claiming a mandate for civilizational
 *   transformation (post-revolutionary, post-independence, or
 *   reformist-monarchical), issues a decree installing a new institutional
 *   commitment — a legal code, an official religious or ideological doctrine,
 *   a standardized administrative or educational curriculum — displacing
 *   customary arrangements that previously held local legitimacy. The decree
 *   is not the product of demonstrated superiority contested and won at
 *   institutional fringes; it is installed by fiat and enforced downward
 *   through courts, police, and provincial administration, with resistance
 *   treated as evidence of the old order's illegitimacy rather than as a
 *   stakeholder position to be reconciled. Historically this pattern recurs
 *   across post-revolutionary legal code impositions, colonial and
 *   post-colonial modernization decrees, and state-led religious or
 *   linguistic standardization campaigns.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.68).
domain_priors:suppression_score(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.79).
domain_priors:theater_ratio(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_commitment_installation_mechanism__exogenous_imposition_reading, tangled_rope).
narrative_ontology:human_readable(state_commitment_installation_mechanism__exogenous_imposition_reading, "Top-Down Decree Installation of New Institutional Commitments").
narrative_ontology:topic_domain(state_commitment_installation_mechanism__exogenous_imposition_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(state_commitment_installation_mechanism__exogenous_imposition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_commitment_installation_mechanism__exogenous_imposition_reading, 'aa0566c8-7282-4fb6-bcd5-187a0a4173ef').
narrative_ontology:cs_kernel_codification('aa0566c8-7282-4fb6-bcd5-187a0a4173ef', formalized).
narrative_ontology:cs_authority_grounding('aa0566c8-7282-4fb6-bcd5-187a0a4173ef', extraction).
narrative_ontology:cs_interpretation_layer_present('aa0566c8-7282-4fb6-bcd5-187a0a4173ef').
narrative_ontology:cs_reading_relation('aa0566c8-7282-4fb6-bcd5-187a0a4173ef', state_commitment_installation_mechanism__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('aa0566c8-7282-4fb6-bcd5-187a0a4173ef', state_commitment_installation_mechanism__hybrid_cascade_reading, influences).
narrative_ontology:cs_axiom('aa0566c8-7282-4fb6-bcd5-187a0a4173ef', foundational, mandate_holding_authority_may_install_without_consent).
narrative_ontology:cs_axiom_status(mandate_holding_authority_may_install_without_consent, holdable).
narrative_ontology:cs_axiom_grounding('aa0566c8-7282-4fb6-bcd5-187a0a4173ef', mandate_holding_authority_may_install_without_consent, conventional).
narrative_ontology:cs_axiom('aa0566c8-7282-4fb6-bcd5-187a0a4173ef', secondary, demonstrated_superiority_is_not_a_precondition_for_legitimacy).
narrative_ontology:cs_axiom_status(demonstrated_superiority_is_not_a_precondition_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('aa0566c8-7282-4fb6-bcd5-187a0a4173ef', demonstrated_superiority_is_not_a_precondition_for_legitimacy, instrumental).
narrative_ontology:cs_reference_frame('aa0566c8-7282-4fb6-bcd5-187a0a4173ef', transformation_mandate_apex_authority).
narrative_ontology:cs_drift_state('aa0566c8-7282-4fb6-bcd5-187a0a4173ef', post_consolidation_administrative_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('aa0566c8-7282-4fb6-bcd5-187a0a4173ef', '').
narrative_ontology:cs_kernel_id(state_commitment_installation_mechanism__exogenous_imposition_reading, state_commitment_installation_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__exogenous_imposition_reading, central_state_apparatus).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__exogenous_imposition_reading, reform_aligned_elite_faction).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__exogenous_imposition_reading, local_customary_authorities).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__exogenous_imposition_reading, peasant_and_provincial_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__exogenous_imposition_reading, provincial_administrators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the transformation mandate — a claimed authority to remake institutional commitments from above, justified by a modernizing or civilizational project. Issues decree, drafts the new code or standard, and deploys administrative and coercive machinery (courts, police, tax registries, licensing) to install it. Collects legitimacy, centralized administrative capacity, and often direct fiscal or resource benefit from the new arrangement it authored.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, central_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(state_commitment_installation_mechanism__exogenous_imposition_reading, central_state_apparatus, beneficiary).

% A faction of urban, often foreign-educated or metropolitan-aligned elites whose social position is elevated by the new commitment (a legal code, an official religion, a national curriculum). They did not build the new order from below; they receive it as validation of a position they already held and staff its administration.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, reform_aligned_elite_faction, beneficiary,
    powerful, biographical, mobile, national).

% Chiefs, clerics, guild heads, or local judges whose prior authority derived from the arrangement now being overwritten. The decree strips their adjudicative or ceremonial function without negotiation; they can comply, go underground, or be prosecuted as obstacles to the transformation. Exit means loss of the very role that gave them standing.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, local_customary_authorities, payer,
    moderate, generational, constrained, regional).

% Bear the disruption of daily practice — new courts, new taxes, new registration requirements, new prohibitions on customary practice — with no channel through which their preferences shaped the change and no meaningful capacity to relocate outside state reach. Compliance is enforced through fines, confiscation, or violence; the new commitment arrives as an event, not a negotiation.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, peasant_and_provincial_populations, payer,
    powerless, biographical, trapped, regional).

% Mid-level officials tasked with enforcing the decree in territory they do not fully control. They benefit from career advancement tied to compliance metrics but bear the operational cost and personal risk of imposing an unpopular order on a resistant population; failure to enforce risks their own position.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, provincial_administrators, agenda_setter,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(state_commitment_installation_mechanism__exogenous_imposition_reading, provincial_administrators, payer).

% Study the installation event after the fact — decree texts, resistance records, implementation gaps — to assess whether the new commitment was genuinely functional coordination or primarily an extraction and legitimation exercise for the installing authority.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, historians_of_state_formation, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_commitment_installation_mechanism__exogenous_imposition_reading, central_state_apparatus).
narrative_ontology:fixing_cost_class(state_commitment_installation_mechanism__exogenous_imposition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A unified legal, religious, or administrative standard replaces a patchwork of customary arrangements, in principle solving genuine coordination problems: inconsistent local justice, incompatible weights/measures or legal codes across a expanding polity, absence of a common status marker for the new state's subjects.
% TRANSFER_FUNCTION: Moves adjudicative authority, ceremonial legitimacy, and often direct resource control from local customary holders to the central state and its aligned elite faction; moves compliance costs, disruption, and enforcement risk onto local authorities and ordinary provincial populations who had no part in designing the new commitment.
% ABSENT_VOICES: Local customary authorities and the populations they served were not consulted in drafting the decree; petitions and delegations sent after the fact are characteristically received as evidence of backwardness rather than as legitimate input, and provincial resistance movements are excluded from the legislative record entirely.
% DISAPPEARANCE_RATIONALE: If the installed commitment (code, standard, official doctrine) were withdrawn overnight, local customary authority structures would resume adjudicative function within a generation, the reform-aligned elite faction would lose its administrative rents and status marker, and the central state would lose a major instrument of territorial and cultural integration — the arrangement is load-bearing for the state's claimed sovereignty over that domain, not incidental to it.
% FOUNDING_PROBLEM: The founding problem as stated by the installing authority is civilizational lag or institutional fragmentation: the old order is diagnosed as backward, inconsistent, or an obstacle to the state's transformation project, and only a mandate-holding center is positioned to solve it quickly.
% FOUNDING_PROBLEM_CORROBORATION: The central state apparatus and the reform-aligned elite faction both attest the founding problem was real and urgent. Independent corroboration from outside the beneficiary set is thin: some contemporary foreign observers and later social historians document genuine coordination gaps in the pre-decree order, but the same historical record also documents that the diagnosed lag was frequently overstated or fabricated to license expropriation of customary resources and authority — no unanimous outside corroboration exists, and this ambiguity is itself the structural feature this reading names.
narrative_ontology:disappearance_verdict(state_commitment_installation_mechanism__exogenous_imposition_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_commitment_installation_mechanism__exogenous_imposition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_commitment_installation_mechanism__exogenous_imposition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_commitment_installation_mechanism__exogenous_imposition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_commitment_installation_mechanism__exogenous_imposition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_commitment_installation_mechanism__exogenous_imposition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_commitment_installation_mechanism__exogenous_imposition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (ε ≈ 0.68 by interval end) reflects genuine transfer: authority, resource control, and status accrue to the center and its aligned elite while local customary authorities lose standing and provincial populations bear compliance costs. Suppression is authored high and front-loaded (0.85 at t0, easing modestly as the new order beds in) because abrupt top-down installation requires maximal coercive force at the moment of imposition, when resistance is least normalized and alternatives are most visible; suppression eases somewhat over decades as the old order recedes from living memory but never falls to a low baseline because the arrangement's legitimacy remains contested at the base throughout. Theater ratio rises over time (0.20 → 0.42) as the state increasingly performs consultation, commissions, and legitimation rituals around a settlement it never actually negotiated — retrospective legitimation theater, not the original coordination function.
 *
 * PERSPECTIVAL GAP:
 *   From the central state's seat, the installation is coordination — a fragmented, backward institutional landscape replaced by a coherent, modern standard, justified by the transformation mandate. From the seat of local customary authorities and provincial populations, the same act is extraction: authority and resources moved to the center and its allies, with no input, no negotiated terms, and active suppression of the prior arrangement. The engine's per-seat computation should register this divergence structurally — the state's own analytical seat, if honest, would need to concede that no coordination-benefit test was ever applied to the population that bore the cost.
 *
 * DIRECTIONALITY LOGIC:
 *   The central state apparatus is the clearest structural beneficiary: it authors the decree, collects the administrative and legitimation gains, and retains arbitrage-grade exit (it can revise, repeal, or selectively enforce the commitment as its interests shift). The reform-aligned elite faction benefits without having built the new order from below — they receive elevated status as a byproduct of a transformation they did not have to earn through contest. Local customary authorities and provincial populations are targets: their prior standing or daily practice is overwritten without consultation, and their exit options are constrained or trapped respectively — customary authorities lose their very basis for standing if they exit the old role, and populations have no meaningful capacity to relocate outside state reach.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (institutional fragmentation, civilizational lag) may have been genuinely live at the moment of imposition or may have been substantially fabricated or exaggerated to license the transfer — the founding_problem_status is authored as contested precisely because corroboration from outside the beneficiary set is thin and mixed. Whether or not the founding problem was ever real, the installation mechanism itself persists as a template: subsequent decrees reuse the same top-down machinery regardless of whether the underlying coordination problem remains live, which is the mandatrophy risk this reading is built to expose — an installation process justified by an urgent founding problem outlives scrutiny of whether that problem was ever accurately diagnosed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_authenticity,
    'Was the diagnosed institutional fragmentation or civilizational lag that justified the decree an accurate description of the pre-existing customary order, or was it substantially fabricated/exaggerated to license the transfer of authority and resources to the center?',
    'Comparative institutional history: assess documented functioning of the customary order immediately prior to imposition against contemporary independent accounts (foreign observers, trade records, dispute-resolution outcomes) rather than the installing authority''s own diagnostic rhetoric.',
    'If the fragmentation diagnosis was substantially accurate, the coordination function claimed by the central state has more genuine weight and the constraint sits closer to tangled_rope with a real (if unequally distributed) coordination gain; if fabricated, the coordination story is closer to pure cover and the constraint drifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_authenticity, empirical, 'Whether the stated founding problem was real or a legitimation device.').

omega_variable(
    kernel_reading_boundary_location,
    'Is the exogenous-imposition pattern this reading describes a structurally distinct installation mechanism from endogenous_climb and hybrid_cascade, or are all three simply different phases/moments within a single longer installation process that this story is arbitrarily slicing at the imposition moment?',
    'Trace individual historical cases (e.g., a specific legal code adoption) across the full multi-decade arc: does the same case exhibit exogenous imposition at inception and then endogenous or cascade dynamics at consolidation, suggesting one process with phases, or does the case remain purely top-down throughout its stabilization, supporting genuinely distinct mechanisms?',
    'If the readings are phases of one process, the kernel decomposition into three sibling constraints risks double-counting extraction across the family; if they are genuinely distinct mechanisms observed in different historical cases, the three-story decomposition is structurally sound and each ε stands independently.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary_location, conceptual, 'Whether the three kernel readings are distinct mechanisms or phases of one process — the located disagreement between this reading and its siblings.').

omega_variable(
    elite_faction_capture_degree,
    'Is the reform-aligned elite faction a genuine beneficiary distinct from the state apparatus, or is it merely an administrative arm of the state with no independent interest — i.e., should it be collapsed into central_state_apparatus for directionality purposes?',
    'Examine whether the elite faction''s status gains persist or are contested independently of the state''s own fortunes (e.g., does the faction retain elevated status if the decree is later partially reversed, suggesting independent capture rather than pure administrative dependency).',
    'If independent, the current two-beneficiary structure is correct; if merely administrative, directionality for that seat should shift closer to the state''s own d-value rather than being treated as a separately capturing party.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_faction_capture_degree, empirical, 'Whether the elite faction is an independent beneficiary or an extension of the state seat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_commitment_installation_mechanism__exogenous_imposition_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(stat_tr_t8, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(stat_tr_t16, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 16, 0.33).
narrative_ontology:measurement(stat_tr_t24, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 24, 0.37).
narrative_ontology:measurement(stat_tr_t32, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement(stat_tr_t40, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(stat_be_t8, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(stat_be_t16, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 16, 0.63).
narrative_ontology:measurement(stat_be_t24, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement(stat_be_t32, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 32, 0.68).
narrative_ontology:measurement(stat_be_t40, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(stat_su_t8, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 8, 0.82).
narrative_ontology:measurement(stat_su_t16, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 16, 0.78).
narrative_ontology:measurement(stat_su_t24, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 24, 0.76).
narrative_ontology:measurement(stat_su_t32, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 32, 0.79).
narrative_ontology:measurement(stat_su_t40, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 40, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_commitment_installation_mechanism__exogenous_imposition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__exogenous_imposition_reading, state_commitment_installation_mechanism__endogenous_climb_reading).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__exogenous_imposition_reading, state_commitment_installation_mechanism__hybrid_cascade_reading).

% DUAL FORMULATION NOTE:
% Three sibling constraints decompose the natural-language concept 'how new state commitments gain legitimacy': this story (exogenous_imposition_reading) authors legitimacy as manufactured top-down via decree and coercive installation, with the state and an aligned elite faction as beneficiaries and local customary authorities/populations as victims bearing suppressed resistance. endogenous_climb_reading authors legitimacy as earned bottom-up through demonstrated institutional superiority with no comparable top-down beneficiary structure. hybrid_cascade_reading authors an apex-installed commitment that nonetheless requires fringe validation to stabilize, producing a different, more contested beneficiary/victim profile and lower sustained suppression than this reading's front-loaded coercion. Each carries its own ε and its own claimed_type; none is a measurement of the same constraint under different observables.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
