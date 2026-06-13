% ============================================================================
% CONSTRAINT STORY: state_commitment_installation_mechanism__endogenous_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: state_commitment_installation_mechanism__endogenous_climb_reading
 *   human_readable: Endogenous Climb: Fringe-to-Apex Legitimacy Pathway
 *   domain: historical_sociology/state_formation
 *
 * SUMMARY:
 *   This constraint captures the historical mechanism by which NEW
 *   institutional commitments (new doctrines, practices, organizational
 *   forms, technologies) achieve legitimacy and become installed as
 *   state-level or apex-level institutions. The endogenous-climb reading
 *   frames this as a ROPE-type coordination mechanism: fringe actors
 *   innovate, practitioner communities recognize superiority through visible
 *   results, adoption cascades through intermediate institutions, and apex
 *   gatekeepers eventually adapt or are displaced. The mechanism is not
 *   top-down mandate (exogenous imposition) nor
 *   apex-installation-with-fringe-ratification (hybrid cascade), but
 *   bottom-up climb driven by demonstrated efficacy. Legitimacy shifts from
 *   inherited authority to earned authority. The claim-metric gap is
 *   intentional and structural: the constraint is CLAIMED as rope
 *   (coordination mechanism benefiting the system as a whole through adaptive
 *   institutional learning) while the authored metrics show moderate
 *   extractiveness (apex gatekeepers pay the cost of eroding authority) and
 *   real resistance (they defend against displacement). The engine will
 *   measure this gap; the narrative explains why both claim and metrics are
 *   defensible readings of the same structural phenomenon.
 *
 * KEY AGENTS:
 *   - Fringe institutional actors: operate at the margins, develop superior innovations, benefit from a legitimacy mechanism that rewards efficacy over inheritance
 *   - Innovation advocates: recognize and promote superiority, operate through evidence and consensus-building rather than authority claims
 *   - Early adopters: hold enough authority to confer legitimacy on innovations they endorse; their adoption cascades
 *   - Apex gatekeepers: institutional authorities whose legitimacy is eroded by the climb; they bear the cost of adaptation or displacement
 *   - Intermediate institutions: navigate the tension between defending inherited authority and embracing adaptive practices
 *   - Practitioner communities: provide the evidentiary foundation through their experience-tested knowledge and consensus
 *   - External observers: measure whether the endogenous-climb pattern is empirically supported across historical cases
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_commitment_installation_mechanism__endogenous_climb_reading, 0.38).
domain_priors:suppression_score(state_commitment_installation_mechanism__endogenous_climb_reading, 0.22).
domain_priors:theater_ratio(state_commitment_installation_mechanism__endogenous_climb_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_commitment_installation_mechanism__endogenous_climb_reading, rope).
narrative_ontology:human_readable(state_commitment_installation_mechanism__endogenous_climb_reading, "Endogenous Climb: Fringe-to-Apex Legitimacy Pathway").
narrative_ontology:topic_domain(state_commitment_installation_mechanism__endogenous_climb_reading, "historical_sociology/state_formation").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_commitment_installation_mechanism__endogenous_climb_reading, '1ed111cf-fbdb-4923-adec-fa43a398e4e3').
narrative_ontology:cs_kernel_codification('1ed111cf-fbdb-4923-adec-fa43a398e4e3', distributed).
narrative_ontology:cs_authority_grounding('1ed111cf-fbdb-4923-adec-fa43a398e4e3', practice).
narrative_ontology:cs_interpretation_layer_present('1ed111cf-fbdb-4923-adec-fa43a398e4e3').
narrative_ontology:cs_reading_relation('1ed111cf-fbdb-4923-adec-fa43a398e4e3', state_commitment_installation_mechanism__exogenous_imposition_reading, coexists_with).
narrative_ontology:cs_reading_relation('1ed111cf-fbdb-4923-adec-fa43a398e4e3', state_commitment_installation_mechanism__hybrid_cascade_reading, coexists_with).
narrative_ontology:cs_axiom('1ed111cf-fbdb-4923-adec-fa43a398e4e3', foundational, efficacy_driven_institutional_legitimacy).
narrative_ontology:cs_axiom_status(efficacy_driven_institutional_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('1ed111cf-fbdb-4923-adec-fa43a398e4e3', efficacy_driven_institutional_legitimacy, empirically_contingent).
narrative_ontology:cs_axiom('1ed111cf-fbdb-4923-adec-fa43a398e4e3', foundational, fringe_actors_as_innovation_source).
narrative_ontology:cs_axiom_status(fringe_actors_as_innovation_source, holdable).
narrative_ontology:cs_axiom_grounding('1ed111cf-fbdb-4923-adec-fa43a398e4e3', fringe_actors_as_innovation_source, empirically_contingent).
narrative_ontology:cs_reference_frame('1ed111cf-fbdb-4923-adec-fa43a398e4e3', efficacy_driven_legitimacy_adoption).
narrative_ontology:cs_drift_state('1ed111cf-fbdb-4923-adec-fa43a398e4e3', contemporary_institutional_theory_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('1ed111cf-fbdb-4923-adec-fa43a398e4e3', '').
narrative_ontology:cs_kernel_id(state_commitment_installation_mechanism__endogenous_climb_reading, state_commitment_installation_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__endogenous_climb_reading, fringe_institutional_actors).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__endogenous_climb_reading, innovation_advocates).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__endogenous_climb_reading, early_adopters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__endogenous_climb_reading, intermediate_institutions).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__endogenous_climb_reading, practitioner_communities).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__endogenous_climb_reading, apex_gatekeepers).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__endogenous_climb_reading, intermediate_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate at the margins of established institutional structures—universities without state funding, religious orders without political patronage, merchant guilds without aristocratic charter. They develop new practices, doctrines, or organizational forms because the apex institutions do not. Success at the fringe—demonstrable results, follower accumulation, problem-solving superiority—opens pathways to institutional climb. They benefit from a legitimacy mechanism that rewards demonstrated capability over inherited authority.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, fringe_institutional_actors, beneficiary,
    moderate, generational, mobile, regional).

% Individuals and small networks who recognize the superiority of a fringe innovation and work to expand its adoption. They argue from efficacy: this works better, solves the problem faster, produces verifiable outcomes. Their power derives from the strength of the evidence and the growing consensus among practitioners, not from holding formal authority. They are mobile enough to move between institutions if the climb accelerates.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, innovation_advocates, beneficiary,
    moderate, biographical, constrained, regional).

% Apex-adjacent actors who experiment with fringe innovations and publicly endorse them when successful. They hold enough authority to confer legitimacy but enough autonomy to take adoption risks. Their visibility and institutional standing amplify the innovation's signal; their adoption creates a cascade effect where others follow to avoid falling behind.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, early_adopters, beneficiary,
    powerful, biographical, mobile, regional).

% The established authorities (state councils, church hierarchies, guild masters, royal courts) who held monopolies on legitimacy under the prior regime. Endogenous climb delegitimizes their position by proving that legitimacy can be earned through demonstrated superiority, not inherited or granted. They bear the cost of eroding authority, displaced institutions, and the need to adapt or be superseded. Their exit options are trapped: they cannot leave the apex without ceasing to be apex; they must either adapt to the new mechanism or defend against it.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, apex_gatekeepers, payer,
    institutional, generational, trapped, national).

% Regional authorities, merchant organizations, ecclesiastical provinces—neither apex nor fringe. They navigate a tension: if they embrace climbing fringe innovations they weaken their own inherited authority; if they resist they risk being left behind by more adaptive competitors. They pay the cost of institutional instability and role ambiguity but benefit if they position themselves as bridges between fringe and apex.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, intermediate_institutions, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(state_commitment_installation_mechanism__endogenous_climb_reading, intermediate_institutions, beneficiary).

% Networks of practitioners (craftspeople, scholars, administrators, clerics) who do the actual work and observe which methods produce better results. Their consensus about superiority is the evidentiary foundation of the climb. They benefit from a mechanism that makes their experience-tested knowledge visible and rewarded, not suppressed by hierarchy. They can migrate to institutions that adopt superior practices.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, practitioner_communities, beneficiary,
    organized, biographical, mobile, regional).

% Historians, theorists, and analysts who observe state commitment installation mechanisms across time and place. They measure whether legitimacy shifts from inherited to demonstrated superiority, whether adoption curves follow S-curves (slow initial fringe growth, rapid middle climb, stabilization at apex), whether resistance concentrates at apex or is distributed. They assess whether the endogenous-climb reading is empirically supported or one ideological framing among others.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, external_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_commitment_installation_mechanism__endogenous_climb_reading, diffuse).
narrative_ontology:fixing_cost_class(state_commitment_installation_mechanism__endogenous_climb_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of institutional adaptation: in a world where inherited authority is stable but potentially maladaptive, how do superior innovations gain legitimacy and displace inferior practices? The endogenous-climb mechanism routes this through demonstrated superiority at the fringe, growing practitioner consensus, and adoption cascade, rather than through top-down mandate.
% TRANSFER_FUNCTION: Transfers legitimacy authority from apex gatekeepers (who hold it by inheritance, charter, or prior conquest) to fringe actors and early adopters (who earn it through efficacy). What is transferred is not wealth but authority over what counts as a valid commitment, what institutions practitioners trust, and which modes of knowledge count as authoritative.
% ABSENT_VOICES: Practitioners in apex institutions who benefit from the status quo and would argue that inherited legitimacy is more stable, predictable, and socially ordered than the volatility of climbs. Their resistance is structural (built into hierarchy) rather than vocal (they speak from within the gatekeeper position and their voice is often confused with the gatekeeper position itself, not recognized as dissent).
% DISAPPEARANCE_RATIONALE: If the endogenous-climb mechanism vanished—if demonstrated superiority stopped conferring legitimacy and inheritance or mandate became the only path—institutional adaptation would slow or stop. Apex-captured institutions would persist even if inferior; fringe innovations would never reach apex adoption; practitioner consensus would remain invisible. The rate of institutional change would be lower, the lag between available knowledge and institutional practice would be wider, the cost of maintaining maladaptive institutions would be higher.
% FOUNDING_PROBLEM: In early state formation, inherited authority and formal hierarchy were insufficient to legitimize new institutional forms emerging from non-elite sources (merchant networks, monastic innovations, guild practices) without central decree. Yet some innovations (double-entry bookkeeping, collegiate governance, standing armies) did achieve institutional install despite originating outside the apex. How did they cross into legitimacy?
% FOUNDING_PROBLEM_CORROBORATION: Economic historians (Kuran on institutional drift, Greif on reputation mechanisms), organizational sociologists (Stinchcombe on institutional legitimacy), and historical studies of technology adoption (Mokyr on knowledge diffusion, Ferguson on financial innovation) all identify fringe-to-apex climbs as a persistent pattern. The endogenous-climb reading draws corroboration from empirical studies of adoption curves, practitioner testimony in historical records, and documented cases where innovations did succeed without top-down mandate (e.g., double-entry bookkeeping in Italian merchant networks, collegiate governance in Oxbridge, parliamentary procedure in English regional courts). The reading is contested by scholars who emphasize state imposition, elite mediation, or coercive standardization as primary mechanisms.
narrative_ontology:disappearance_verdict(state_commitment_installation_mechanism__endogenous_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_commitment_installation_mechanism__endogenous_climb_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_commitment_installation_mechanism__endogenous_climb_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(state_commitment_installation_mechanism__endogenous_climb_reading, 'none', 1).

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
 *   Extractiveness is moderate (0.38 at interval end, rising from 0.18) because the mechanism imposes a real cost on apex gatekeepers—erosion of authority, displacement pressure, institutional instability. But this is not pure extraction; it is the price of institutional adaptation. The curve shows early rapid growth (t=0 to t=15, rising from 0.18 to 0.35) as the climb accelerates and apex resistance intensifies, then stabilization (t=15 to t=40) as apex institutions either adapt or are fully displaced, reaching equilibrium. Suppression is low and DECLINING (0.35 at t=0, dropping to 0.22 by t=20, then stable) because the mechanism's efficacy depends on LOW coercive overhead—it works through demonstrated superiority, not force. High suppression would indicate top-down mandate (exogenous imposition reading), not endogenous climb. Theater is low (0.15 at interval end) because the mechanism's primary function—routing legitimacy through efficacy—is real and operational, not performative. The time grid is shared across all three metrics, authored at every time point to avoid misalignment. The temporal pattern matches historical adoption curves: slow initial growth (fringe actors accumulating followers), rapid climb phase (apex attention and early-adopter cascade), stabilization (new institutional form becomes normal).
 *
 * PERSPECTIVAL GAP:
 *   The apex-gatekeeper seat and the fringe-beneficiary seat compute dramatically different types from this constraint's structure. From the fringe position, the mechanism is liberating—it offers a pathway to legitimacy that does not require inherited authority or state decree. From the apex position, the same mechanism is destabilizing—it erodes authority and forces adaptation or displacement. The apex seat sees a snare-like extraction of legitimacy; the fringe seat sees a rope-type coordination mechanism that enables institutional learning. The engine computes this divergence from the structural data: power levels (fringe moderate, apex institutional), exit options (fringe mobile, apex trapped), and time horizons (fringe biographical, apex generational). The authored claim (rope) reflects the system-level function—adaptation benefits all through selection for superior institutions; the authored metrics (moderate extractiveness, low suppression, low theater, real resistance) reflect the asymmetric distribution of costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Fringe beneficiaries sit at d~0.2 (net beneficiaries: they collect legitimacy through efficacy, face low suppression, have mobile exit). Apex gatekeepers sit at d~0.8 (net targets: they lose authority, face displacement pressure, are trapped at apex). Intermediate institutions sit near d~0.5 (symmetric: they pay costs of adaptation, benefit from positioning as bridges). The mechanism does not require explicit enforcement (requires_active_enforcement = false) because its power derives from visible efficacy, not coercion. Suppression is low because alternative pathways (inheritance, decree) are theoretically still available but empirically losing appeal. The directionality derivation from beneficiary/victim declarations feeds this: fringe actors are declared beneficiaries; apex gatekeepers are declared payers. The engine produces d values aligned with this structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how do superior innovations gain legitimacy without top-down mandate?) is LIVE: the institutional adaptation challenge persists across all state formations. The disappearance verdict is world_rearranges: if endogenous climb vanished, apex-captured inferior institutions would persist, lagging behind available knowledge. The mismatch test (founding_problem_status=live + disappearance_verdict=world_rearranges) does NOT flag mandatrophy: this is a live coordination mechanism, not a zombie constraint. The constraint's persistence is justified by the ongoing need to route institutional legitimacy through demonstrated superiority. No mandatrophy marker is warranted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    efficacy_vs_narrative_superiority,
    'Is the ''demonstrated superiority'' that drives the climb an objective property of the innovation, or a consensus narrative constructed around it? When practitioners claim an innovation works better, are they measuring against a natural performance standard, or constructing a new standard that privileges the innovation''s characteristics?',
    'Comparative analysis of multiple cases where similar innovations had different adoption curves in different regions. If adoption correlates with objective performance metrics independent of narrative framing, efficacy is real; if adoption correlates primarily with advocate reputation and social network position, superiority is narratively constructed.',
    'If superiority is objective, the endogenous-climb mechanism is a genuine rope-type coordination mechanism that selects for better institutions. If superiority is narrative, the climb is a capture mechanism—fringe actors build a compelling story and apex institutions are persuaded not by efficacy but by narrative authority and network effects. The classification would shift toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficacy_vs_narrative_superiority, empirical, 'Whether ''demonstrated superiority'' is an objective property or a constructed narrative.').

omega_variable(
    suppression_inversion_during_climb,
    'The measurement series show suppression DECLINING as the climb accelerates. Is this accurate, or does apex-gatekeeper suppression increase but operate in subtler forms (regulatory capture, funding control, institutional marginalization) that are harder to observe than early overt resistance?',
    'Detailed historical study of apex-gatekeeper behavior during climbing phases: did they use overt coercion (suppression_requirement rising) or subtle institutional mechanisms (appearing to adapt while controlling innovation channels)? Do practitioner testimonies report increasing pressure or decreasing pressure?',
    'If subtle suppression is actually high while declining-overt-suppression is measured, the constraint is more snare-like (high net suppression masked by theatrical adaptation) than rope-like (genuine low-suppression coordination). The theater_ratio might be undercounted.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_inversion_during_climb, empirical, 'Whether declining-measured-suppression reflects true suppression decline or measurement-artifact of shifting suppression types.').

omega_variable(
    reading_foreclosure_under_competing_axiom,
    'If empirical evidence establishes that innovations spread primarily through apex-institutional mandate with later fringe ratification (supporting the hybrid_cascade_reading), does this foreclose the endogenous_climb_reading''s core axiom (efficacy-driven legitimacy), or merely shows that climb is less frequent than cascade?',
    'Conceptual question: define whether ''foreclosure'' requires showing the axiom never holds (cascade is the ONLY mechanism across all cases) or shows it does not hold in the strongest claimed instances (climb is less primary than cascade). The operator ruling determines the threshold.',
    'If foreclosure requires univocal mechanism (climb never operates), this reading would be structurally refuted by strong cascade evidence. If foreclosure requires only showing relative frequency, cascade dominance would not foreclose climb, only demonstrate it is secondary. This determines whether the three readings remain coexistent or whether one forecloses others.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_under_competing_axiom, conceptual, 'Whether empirical dominance of one mechanism forecloses the axioms of alternative mechanisms or merely shifts their relative weight.').

omega_variable(
    beneficiary_scope_boundary,
    'Who counts as a beneficiary of the endogenous-climb mechanism? Fringe actors clearly gain legitimacy. But do apex institutions also benefit from having a mechanism that forces them to adapt? Do practitioners benefit from working in institutions that must compete on efficacy? Or are apex gatekeepers ONLY payers, never co-beneficiaries?',
    'Analyze the apex-gatekeeper and intermediate-institution positions: do they express (in testimony, behavior, institutional investment) that they benefit from the competitive pressure, or only that they bear costs? Would they choose to preserve the climb mechanism if they could exit?',
    'If apex institutions are co-beneficiaries (pressure to adapt strengthens them long-term), extractiveness decreases because the cost-distribution is more symmetric. If they are only payers (pressure to adapt weakens them; they lose authority without compensating benefit), extractiveness remains at current levels. The measured asymmetry (beneficiary fringe, payer apex) would be validated or challenged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_scope_boundary, empirical, 'Whether the endogenous-climb mechanism distributes costs and benefits symmetrically or asymmetrically across institutional levels.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_commitment_installation_mechanism__endogenous_climb_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(stat_tr_t0, observed).
narrative_ontology:measurement(stat_tr_t5, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement_basis(stat_tr_t5, observed).
narrative_ontology:measurement(stat_tr_t10, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement_basis(stat_tr_t10, observed).
narrative_ontology:measurement(stat_tr_t15, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 15, 0.14).
narrative_ontology:measurement_basis(stat_tr_t15, observed).
narrative_ontology:measurement(stat_tr_t20, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement_basis(stat_tr_t20, observed).
narrative_ontology:measurement(stat_tr_t25, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 25, 0.15).
narrative_ontology:measurement_basis(stat_tr_t25, observed).
narrative_ontology:measurement(stat_tr_t30, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement_basis(stat_tr_t30, observed).
narrative_ontology:measurement(stat_tr_t40, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement_basis(stat_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(stat_be_t0, observed).
narrative_ontology:measurement(stat_be_t5, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 5, 0.22).
narrative_ontology:measurement_basis(stat_be_t5, observed).
narrative_ontology:measurement(stat_be_t10, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement_basis(stat_be_t10, observed).
narrative_ontology:measurement(stat_be_t15, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 15, 0.35).
narrative_ontology:measurement_basis(stat_be_t15, observed).
narrative_ontology:measurement(stat_be_t20, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 20, 0.37).
narrative_ontology:measurement_basis(stat_be_t20, observed).
narrative_ontology:measurement(stat_be_t25, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 25, 0.38).
narrative_ontology:measurement_basis(stat_be_t25, observed).
narrative_ontology:measurement(stat_be_t30, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement_basis(stat_be_t30, observed).
narrative_ontology:measurement(stat_be_t40, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement_basis(stat_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(stat_su_t0, observed).
narrative_ontology:measurement(stat_su_t5, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 5, 0.3).
narrative_ontology:measurement_basis(stat_su_t5, observed).
narrative_ontology:measurement(stat_su_t10, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 10, 0.26).
narrative_ontology:measurement_basis(stat_su_t10, observed).
narrative_ontology:measurement(stat_su_t15, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 15, 0.24).
narrative_ontology:measurement_basis(stat_su_t15, observed).
narrative_ontology:measurement(stat_su_t20, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 20, 0.22).
narrative_ontology:measurement_basis(stat_su_t20, observed).
narrative_ontology:measurement(stat_su_t25, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 25, 0.22).
narrative_ontology:measurement_basis(stat_su_t25, observed).
narrative_ontology:measurement(stat_su_t30, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 30, 0.22).
narrative_ontology:measurement_basis(stat_su_t30, observed).
narrative_ontology:measurement(stat_su_t40, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 40, 0.22).
narrative_ontology:measurement_basis(stat_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_commitment_installation_mechanism__endogenous_climb_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(state_commitment_installation_mechanism__endogenous_climb_reading, 0.12).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__endogenous_climb_reading, state_commitment_installation_mechanism__exogenous_imposition_reading).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__endogenous_climb_reading, state_commitment_installation_mechanism__hybrid_cascade_reading).

% DUAL FORMULATION NOTE:
% The endogenous-climb reading is part of a three-constraint family decomposing the kernel 'state-commitment installation mechanism.' The kernel is contested: different parties and scholars emphasize different primary pathways by which new institutional commitments gain legitimacy and apex install. This reading (endogenous climb) emphasizes bottom-up demonstration and adoption cascade; the exogenous_imposition_reading emphasizes top-down mandate; the hybrid_cascade_reading emphasizes apex installation with fringe validation. The three readings are not different measurements of one constraint—they instantiate genuinely different structures (different beneficiaries, different suppression patterns, different time-horizon emphasis). Each story carries its own ε invariant to its own structural data. Siblings are linked in network.affects_constraints to enable contamination and coupling analysis across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
