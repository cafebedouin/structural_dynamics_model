% ============================================================================
% CONSTRAINT STORY: state_commitment_installation_mechanism__endogenous_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_commitment_installation_mechanism__endogenous_climb_reading, []).

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
 *   constraint_id: state_commitment_installation_mechanism__endogenous_climb_reading
 *   human_readable: Endogenous Climb: Legitimacy Ascent of New Commitments from Institutional Fringes
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This story instantiates the endogenous-climb reading of the
 *   state-commitment-installation kernel: legitimacy for a new institutional
 *   commitment (a legal procedure, fiscal technique, or administrative form)
 *   accrues not because an apex authority decrees it, but because it
 *   demonstrably outperforms the incumbent method at the periphery and climbs
 *   through guild codification and provincial adoption until central
 *   ratification becomes politically unavoidable. Incumbent apex
 *   officeholders resist because their standing is tied to the older
 *   commitment; resistance is real and falls as the climbing practice's track
 *   record accumulates. This is a distinct constraint from the
 *   exogenous_imposition_reading (where the center originates and installs
 *   top-down) and the hybrid_cascade_reading (where apex installation
 *   requires fringe validation to stabilize) — those are separate constraints
 *   with their own ε values, not alternate measurements of this one.
 *
 * KEY AGENTS:
 *   - fringe_reform_networks: Primary originator/beneficiary (moderate/mobile) — builds the practice and the evidentiary track record that drives the climb
 *   - provincial_administrators: Early adopter/beneficiary (moderate/constrained) — implements the practice locally, converts outcomes into career capital
 *   - emergent_professional_guilds: Codifying beneficiary (organized/mobile) — transforms local innovation into portable, licensable standard
 *   - incumbent_apex_officeholders: Primary payer (institutional/constrained) — loses relative standing as the older commitment is displaced
 *   - entrenched_patronage_networks: Secondary payer (powerful/trapped) — depreciated as the currency of the old commitment falls
 *   - central_ratifying_authority: Observer/late agenda-setter (institutional/analytical) — ratifies the climb once undeniable, capturing partial credit
 *   - unrecognized_dissenting_communities: Excluded (powerless/trapped) — comparable innovations that never reached the channels needed to climb
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_commitment_installation_mechanism__endogenous_climb_reading, 0.38).
domain_priors:suppression_score(state_commitment_installation_mechanism__endogenous_climb_reading, 0.52).
domain_priors:theater_ratio(state_commitment_installation_mechanism__endogenous_climb_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_commitment_installation_mechanism__endogenous_climb_reading, rope).
narrative_ontology:human_readable(state_commitment_installation_mechanism__endogenous_climb_reading, "Endogenous Climb: Legitimacy Ascent of New Commitments from Institutional Fringes").
narrative_ontology:topic_domain(state_commitment_installation_mechanism__endogenous_climb_reading, "historical_sociology/state_formation/cultural_authority").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_commitment_installation_mechanism__endogenous_climb_reading, 'd15896f1-a997-4119-bdf8-3f0a297c6cf9').
narrative_ontology:cs_kernel_codification('d15896f1-a997-4119-bdf8-3f0a297c6cf9', distributed).
narrative_ontology:cs_authority_grounding('d15896f1-a997-4119-bdf8-3f0a297c6cf9', practice).
narrative_ontology:cs_interpretation_layer_present('d15896f1-a997-4119-bdf8-3f0a297c6cf9').
narrative_ontology:cs_reading_relation('d15896f1-a997-4119-bdf8-3f0a297c6cf9', state_commitment_installation_mechanism__exogenous_imposition_reading, coexists_with).
narrative_ontology:cs_reading_relation('d15896f1-a997-4119-bdf8-3f0a297c6cf9', state_commitment_installation_mechanism__hybrid_cascade_reading, influences).
narrative_ontology:cs_axiom('d15896f1-a997-4119-bdf8-3f0a297c6cf9', foundational, legitimacy_tracks_demonstrated_local_superiority).
narrative_ontology:cs_axiom_status(legitimacy_tracks_demonstrated_local_superiority, holdable).
narrative_ontology:cs_axiom_grounding('d15896f1-a997-4119-bdf8-3f0a297c6cf9', legitimacy_tracks_demonstrated_local_superiority, empirically_contingent).
narrative_ontology:cs_axiom('d15896f1-a997-4119-bdf8-3f0a297c6cf9', secondary, institutional_selection_operates_bottom_up).
narrative_ontology:cs_axiom_status(institutional_selection_operates_bottom_up, holdable).
narrative_ontology:cs_axiom_grounding('d15896f1-a997-4119-bdf8-3f0a297c6cf9', institutional_selection_operates_bottom_up, conventional).
narrative_ontology:cs_reference_frame('d15896f1-a997-4119-bdf8-3f0a297c6cf9', provincial_demonstration_precedes_central_recognition).
narrative_ontology:cs_drift_state('d15896f1-a997-4119-bdf8-3f0a297c6cf9', post_guild_professionalization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d15896f1-a997-4119-bdf8-3f0a297c6cf9', '').
narrative_ontology:cs_kernel_id(state_commitment_installation_mechanism__endogenous_climb_reading, state_commitment_installation_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__endogenous_climb_reading, fringe_reform_networks).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__endogenous_climb_reading, provincial_administrators).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__endogenous_climb_reading, emergent_professional_guilds).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__endogenous_climb_reading, incumbent_apex_officeholders).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__endogenous_climb_reading, entrenched_patronage_networks).
narrative_ontology:constraint_vindicates(state_commitment_installation_mechanism__endogenous_climb_reading, demonstrated_superiority_confers_legitimacy).
narrative_ontology:constraint_vindicates(state_commitment_installation_mechanism__endogenous_climb_reading, institutional_selection_is_bottom_up).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate outside the formal apparatus of state authority — provincial reformers, guild innovators, dissenting clergy, or municipal experimenters who develop a superior practice (accounting method, dispute-resolution procedure, tax-farming alternative) and demonstrate it locally before it is noticed by the center. They set the agenda by originating the practice and accumulating a track record; they benefit when the practice climbs and their networks gain standing.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, fringe_reform_networks, agenda_setter,
    moderate, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(state_commitment_installation_mechanism__endogenous_climb_reading, fringe_reform_networks, beneficiary).

% Mid-level officials who adopt the fringe practice early because it solves a real local problem (revenue shortfall, litigation backlog, succession dispute) more effectively than the sanctioned method. They benefit from improved outcomes and can parlay early adoption into career advancement once the practice is later ratified centrally.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, provincial_administrators, beneficiary,
    moderate, biographical, constrained, regional).

% Craft, legal, or commercial associations that codify and transmit the new practice, turning a local innovation into a portable standard. They gain jurisdictional standing and licensing authority as the practice climbs, converting technical superiority into institutional position.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, emergent_professional_guilds, beneficiary,
    organized, generational, mobile, national).

% Hold formal authority under the prior commitment structure and lose relative standing as the climbing practice gains legitimacy, since their expertise and patronage networks were built around the older method. They resist ratification, delay recognition, or attempt to co-opt the innovation's credit; exit is constrained because their authority is defined by the very apparatus being displaced.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, incumbent_apex_officeholders, payer,
    institutional, biographical, constrained, national).

% Clients and dependents whose access to office, revenue, or protection runs through the incumbent apparatus. As the fringe practice climbs and displaces the old commitment, the currency of their patronage relationships depreciates. They are trapped because their entire social position is constituted by the network being superseded.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, entrenched_patronage_networks, payer,
    powerful, biographical, trapped, national).

% The crown, council, or supreme court that eventually recognizes and formalizes the climbing practice, converting demonstrated local superiority into binding state commitment. Watches the ascent from above, intervenes to ratify (or block) once the practice's track record becomes politically undeniable, and thereby captures some credit for what fringe actors built.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, central_ratifying_authority, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(state_commitment_installation_mechanism__endogenous_climb_reading, central_ratifying_authority, agenda_setter).

% Groups whose alternative practices demonstrated comparable or superior results but lacked the network access, literacy, or proximity to guild/administrative channels needed to climb. Their innovations are absorbed, ignored, or attributed to later adopters; they have no forum to contest the ascent narrative once it consolidates.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, unrecognized_dissenting_communities, excluded,
    powerless, biographical, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_commitment_installation_mechanism__endogenous_climb_reading, diffuse).
narrative_ontology:fixing_cost_class(state_commitment_installation_mechanism__endogenous_climb_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism by which locally-proven practices can be tested at low stakes, observed, and selectively scaled — allowing the state to adopt superior administrative, legal, or economic techniques without centrally commissioning experimentation it cannot itself originate or verify.
% TRANSFER_FUNCTION: Moves legitimacy, jurisdictional standing, and eventually formal authority from the apex incumbents and their patronage networks toward the fringe originators and the guilds/administrators who scaled the practice; moves practical governance capacity from the center (which did not invent the solution) to the periphery (which did).
% ABSENT_VOICES: Dissenting communities whose comparable innovations never reached guild or administrative attention are structurally absent from the ascent narrative; their alternative practices are neither refuted nor credited, simply outcompeted for visibility rather than merit alone.
% DISAPPEARANCE_RATIONALE: If the climb mechanism vanished, novel administrative and legal practices would have no pathway to central recognition except direct petition to apex authority or violent rupture; provincial innovation would either stagnate under incumbent suppression or fragment into permanently unratified regional variants, and guilds would lose their primary route to jurisdictional standing.
% FOUNDING_PROBLEM: Centralized authorities could not generate, test, or verify administrative and legal innovations fast enough to keep pace with changing economic and social conditions in the provinces; a channel was needed for locally-validated superiority to reach the center without requiring the center to have invented it.
% FOUNDING_PROBLEM_CORROBORATION: Provincial administrators and guild records attest the problem remains live — local innovation continues to outpace central capacity for invention. Incumbent apex officeholders and their historians characterize the problem as substantially solved by standing bureaucratic review processes, framing continued fringe ascent as redundant disruption rather than necessary correction; independent institutional historians outside both camps document persistent lag in central innovation capacity, supporting the fringe/administrator reading.
narrative_ontology:disappearance_verdict(state_commitment_installation_mechanism__endogenous_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_commitment_installation_mechanism__endogenous_climb_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_commitment_installation_mechanism__endogenous_climb_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_commitment_installation_mechanism__endogenous_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_commitment_installation_mechanism__endogenous_climb_reading, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_commitment_installation_mechanism__endogenous_climb_reading_tests).
:- end_tests(state_commitment_installation_mechanism__endogenous_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate and rises slowly (0.22 to 0.38) because the mechanism is genuinely coordinative at origin — it lets superior local practice propagate — but accumulates extraction as guilds and provincial administrators who scaled the practice begin converting their gatekeeping position into rent (licensing fees, credentialing barriers) once their codified version becomes the only recognized path to ratification. Suppression is initially fairly high (0.62) reflecting incumbent resistance and gatekeeping against unrecognized fringe innovators, and falls over the interval (to 0.52) as the climbing practice displaces the old guard and its own institutional position stabilizes. Theater ratio is low throughout but rises modestly as guild codification increasingly performs rigor it does not always possess. Accessibility collapse is moderate (0.35): alternative practices are not fully foreclosed, but once a guild-codified version becomes the recognized standard, non-guild-affiliated innovators face a materially harder path. Resistance is high (0.68) reflecting sustained incumbent pushback at the apex — this is the reading's diagnostic signature: resistance concentrated at the top, not at the bottom.
 *
 * DIRECTIONALITY LOGIC:
 *   Fringe reform networks, provincial administrators, and guilds are declared beneficiaries — they gain standing, career capital, and jurisdictional authority as the climb succeeds, so the engine should derive low-to-moderate d for these seats. Incumbent apex officeholders and entrenched patronage networks are declared victims — their authority and social position are constituted by the commitment being displaced, and their exit options are constrained-to-trapped because leaving the apparatus means abandoning the position that gives them standing at all, so d should sit near the target end for these seats. Unrecognized dissenting communities are excluded rather than victimized in the direct-payer sense — they neither benefit from nor are extracted from by the operating mechanism; they are simply outside its evidentiary channel entirely, which is why they are marked excluded rather than payer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (centralized authority cannot originate innovation fast enough) remains substantially live per provincial and guild corroboration, which is why this reading resists a clean mandatrophy verdict — the climb mechanism is still solving a real problem, not merely persisting on inertia. Where mandatrophy risk enters is at the guild layer: once a guild's codified version of a once-fringe practice becomes the sole recognized channel to ratification, the guild's continued gatekeeping can outlive the coordination need it originally served, converting from evidentiary aggregator to rent-collecting licensing body. The rising extractiveness series models exactly this drift without asserting it has completed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    climb_reading_is_the_true_mechanism_or_survivorship_narrative,
    'Is the endogenous-climb pattern the actual causal mechanism by which this commitment gained legitimacy, or is it a survivorship narrative constructed after the fact by the fringe actors and guilds who benefited from being credited with the ascent?',
    'Comparative archival analysis of contemporaneous correspondence between apex authorities and provincial administrators during the period before ratification — if the apex authority was already actively monitoring and preparing to install the practice regardless of provincial demonstration, the climb narrative is retrospective credit-assignment rather than the true mechanism, supporting the hybrid_cascade or exogenous_imposition reading instead.',
    'If the archival record shows apex-initiated preparation predating the claimed ''demonstrated superiority'' climb, this story''s claimed_type and beneficiary structure would need to be substantially revised toward the hybrid_cascade_reading, and the vindicated proposition ''institutional_selection_is_bottom_up'' would be undermined for this specific historical case.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(climb_reading_is_the_true_mechanism_or_survivorship_narrative, conceptual, 'Whether the endogenous-climb framing reflects genuine mechanism or retrospective narrative construction by beneficiaries of the ascent.').

omega_variable(
    guild_codification_rent_capture_threshold,
    'At what point does guild codification of the climbing practice convert from a genuine coordination service (verification, standardization, transmission) into rent extraction via licensing and credentialing barriers?',
    'Track licensing fee growth and credential-denial rates for non-guild-affiliated practitioners of the codified technique over the post-ratification period; a sustained rise in denial rates without corresponding quality differentiation indicates rent capture.',
    'If rent capture is confirmed and substantial, the guild seat''s classification would shift from pure beneficiary toward a tangled-rope or snare-adjacent position, and the rising extractiveness series in this story would need to be attributed specifically to guild behavior rather than the climb mechanism as a whole.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(guild_codification_rent_capture_threshold, empirical, 'Whether guild codification of the climbing practice degrades from coordination into extraction over time.').

omega_variable(
    excluded_innovator_selection_bias,
    'Is the set of dissenting communities whose innovations never climbed a random sample of comparable-quality alternatives, or were they systematically excluded on grounds unrelated to demonstrated superiority (geographic isolation, lack of literacy, exclusion from guild membership on identity grounds)?',
    'Comparative case study of a sample of non-climbing local innovations against climbing ones, controlling for measurable outcome quality, to isolate whether network access rather than superiority predicts ascent.',
    'If access rather than merit predominantly predicts which innovations climb, the core normative claim of this reading — that legitimacy tracks demonstrated superiority — is substantially weakened, and the constraint would need reclassification toward tangled_rope (coordination function real, but selection systematically favors already-networked actors).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(excluded_innovator_selection_bias, empirical, 'Whether the climb mechanism selects on genuine superiority or on pre-existing network access.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_commitment_installation_mechanism__endogenous_climb_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(stat_tr_t12, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 12, 0.13).
narrative_ontology:measurement(stat_tr_t24, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 24, 0.16).
narrative_ontology:measurement(stat_tr_t36, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 36, 0.18).
narrative_ontology:measurement(stat_tr_t48, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 48, 0.2).
narrative_ontology:measurement(stat_tr_t60, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 60, 0.22).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(stat_be_t12, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 12, 0.26).
narrative_ontology:measurement(stat_be_t24, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 24, 0.31).
narrative_ontology:measurement(stat_be_t36, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 36, 0.34).
narrative_ontology:measurement(stat_be_t48, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 48, 0.36).
narrative_ontology:measurement(stat_be_t60, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 60, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(stat_su_t12, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 12, 0.6).
narrative_ontology:measurement(stat_su_t24, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 24, 0.57).
narrative_ontology:measurement(stat_su_t36, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 36, 0.55).
narrative_ontology:measurement(stat_su_t48, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 48, 0.53).
narrative_ontology:measurement(stat_su_t60, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 60, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(state_commitment_installation_mechanism__endogenous_climb_reading, state_commitment_installation_mechanism__exogenous_imposition_reading).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__endogenous_climb_reading, state_commitment_installation_mechanism__hybrid_cascade_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the state_commitment_installation_mechanism kernel, each authored as a structurally distinct constraint with its own ε: endogenous_climb_reading (this story — bottom-up ascent, fringe beneficiaries, apex resistance, gradual adoption), exogenous_imposition_reading (top-down installation, apex beneficiaries, provincial/local resistance), and hybrid_cascade_reading (apex-installed but requiring fringe validation to stabilize, mixed beneficiary structure). The three do not average into one composite ε; they are linked here so contamination or purity-drift analysis can propagate across the family when one reading's empirical status shifts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
