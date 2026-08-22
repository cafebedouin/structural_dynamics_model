% ============================================================================
% CONSTRAINT STORY: state_commitment_installation_mechanism__exogenous_imposition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
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
 *   human_readable: State-Mandated Commitment Installation by Exogenous Authority Decree
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This constraint models the state's mechanism for installing new
 *   commitments (institutional forms, legal regimes, cultural practices)
 *   through top-down authority decree, without consultation or grassroots
 *   validation. The reading instantiates the 'exogenous imposition' pole of
 *   the contested kernel 'state_commitment_installation_mechanism': the state
 *   holds a transformation mandate, issues decrees declaring forms
 *   legitimate, and enforces adoption through bureaucratic machinery.
 *   Legitimacy flows from the authority's demonstrated capacity to reshape
 *   institutions, not from evidence of superior practice or grassroots climb.
 *   The constraint is claimed as tangled_rope (coordination of
 *   standardization + extraction of autonomy), and authored metrics describe
 *   substantially extractive, actively-suppressed operation whose theater
 *   rises over time (performance masking declining functional benefit).
 *
 * KEY AGENTS:
 *   - state_authority_apparatus: The institutional actor holding transformation mandate; imposes by decree; benefits from demonstrated standardization capacity and legitimacy capital
 *   - subordinate_institutional_actors: Organizations forced to adopt new commitments; bear coordinated cost; constrained exit; organized resistance
 *   - local_practice_communities: Powerless practitioners displaced by imposition; identity-locked to old forms; rendered obsolete; excluded from legitimacy process
 *   - credentialing_legitimacy_system: The apparatus certifying new commitments as legitimate; benefits from being positioned as neutral arbiter of state mandates
 *   - fringe_institutional_innovators: Excluded from legitimacy process despite local evidence of superiority; their knowledge is not consulted
 *   - competing_state_authorities: Also subordinate to transformation mandate; excluded from deciding which commitments to adopt
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.68).
domain_priors:suppression_score(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.76).
domain_priors:theater_ratio(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_commitment_installation_mechanism__exogenous_imposition_reading, tangled_rope).
narrative_ontology:human_readable(state_commitment_installation_mechanism__exogenous_imposition_reading, "State-Mandated Commitment Installation by Exogenous Authority Decree").
narrative_ontology:topic_domain(state_commitment_installation_mechanism__exogenous_imposition_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(state_commitment_installation_mechanism__exogenous_imposition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_commitment_installation_mechanism__exogenous_imposition_reading, '4f14fc3b-613b-406f-b777-ba40b3a1d4f4').
narrative_ontology:cs_kernel_codification('4f14fc3b-613b-406f-b777-ba40b3a1d4f4', formalized).
narrative_ontology:cs_authority_grounding('4f14fc3b-613b-406f-b777-ba40b3a1d4f4', extraction).
narrative_ontology:cs_interpretation_layer_present('4f14fc3b-613b-406f-b777-ba40b3a1d4f4').
narrative_ontology:cs_reading_relation('4f14fc3b-613b-406f-b777-ba40b3a1d4f4', state_commitment_installation_mechanism__endogenous_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('4f14fc3b-613b-406f-b777-ba40b3a1d4f4', state_commitment_installation_mechanism__hybrid_cascade_reading, coexists_with).
narrative_ontology:cs_axiom('4f14fc3b-613b-406f-b777-ba40b3a1d4f4', foundational, authority_source_legitimacy_is_decree).
narrative_ontology:cs_axiom_status(authority_source_legitimacy_is_decree, holdable).
narrative_ontology:cs_axiom_grounding('4f14fc3b-613b-406f-b777-ba40b3a1d4f4', authority_source_legitimacy_is_decree, conventional).
narrative_ontology:cs_axiom('4f14fc3b-613b-406f-b777-ba40b3a1d4f4', foundational, fringe_validation_unnecessary_for_legitimacy).
narrative_ontology:cs_axiom_status(fringe_validation_unnecessary_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('4f14fc3b-613b-406f-b777-ba40b3a1d4f4', fringe_validation_unnecessary_for_legitimacy, deontological).
narrative_ontology:cs_reference_frame('4f14fc3b-613b-406f-b777-ba40b3a1d4f4', centralized_hierarchical_legitimacy_authority).
narrative_ontology:cs_drift_state('4f14fc3b-613b-406f-b777-ba40b3a1d4f4', contemporary_post_institutional_resistance_accumulation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4f14fc3b-613b-406f-b777-ba40b3a1d4f4', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(state_commitment_installation_mechanism__exogenous_imposition_reading, state_commitment_installation_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__exogenous_imposition_reading, state_authority_apparatus).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__exogenous_imposition_reading, credentialing_legitimacy_system).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__exogenous_imposition_reading, subordinate_institutional_actors).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__exogenous_imposition_reading, local_practice_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__exogenous_imposition_reading, local_practice_communities).
narrative_ontology:constraint_vindicates(state_commitment_installation_mechanism__exogenous_imposition_reading, state_monopoly_on_legitimacy_transformation).
narrative_ontology:constraint_vindicates(state_commitment_installation_mechanism__exogenous_imposition_reading, bureaucratic_hierarchy_as_mechanism_of_social_change).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The state administration that holds the mandate to transform and standardize institutional forms. Issues decrees declaring new commitments legitimate, enforces adoption through bureaucratic machinery, and derives legitimacy capital from successful imposition (demonstrated capacity to reshape society according to directive). The state does not seek consent; it imposes by authority and demonstrates success through compliance rates.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, state_authority_apparatus, agenda_setter,
    institutional, civilizational, analytical, national).

% Organizations (municipalities, schools, religious bodies, professional guilds, corporations) that must adopt the newly mandated commitment. They face a choice: adopt as decreed (absorbing costs and risking institutional disruption), litigate (expensive and rarely successful against state authority), or attempt covert non-compliance. Most cannot exit the jurisdiction or the regime without collective action they lack the coordination to achieve.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, subordinate_institutional_actors, payer,
    organized, generational, constrained, national).

% Practitioners whose daily routines are embedded in the old institutional forms (teachers, clergy, administrators, workers). The mandate disrupts their habitus and professional identity. Some benefit from the transition (those whose status rises under the new form), but most experience the imposition as displacement and loss of accumulated expertise. Their objections are registered as 'resistance to progress' rather than legitimate claims about legitimacy or social cost.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, local_practice_communities, payer,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(state_commitment_installation_mechanism__exogenous_imposition_reading, local_practice_communities, beneficiary).

% The apparatus that certifies new commitments as legitimate (legal codes, administrative procedure, credentials bodies, expert commissions). Benefits from the state's reliance on it to legitimize imposed forms; its power grows as the state outsources the appearance of rational justification for its mandates. It has no independent stake in whether the commitment is actually superior—only in being positioned as the authoritative source of legitimacy.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, credentialing_legitimacy_system, beneficiary,
    institutional, civilizational, analytical, national).

% Actors at the margins of institutional structures who have been experimenting with alternative forms and demonstrating their superiority through local evidence. Under the exogenous-imposition model, their evidence is irrelevant to state decision-making; the state imposes a commitment that may or may not match what the fringe has discovered. They are excluded from the legitimacy process and their knowledge is not consulted.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, fringe_institutional_innovators, excluded,
    moderate, biographical, trapped, local).

% Other state apparatuses or jurisdictions that might have preferred alternative commitments or retained autonomy to choose. The exogenous imposition from above forecloses their input. They resist the mandate but cannot exit the state structure; they are subordinate to the transformation authority and bound by the same hierarchical logic they seek to escape.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, competing_state_authorities, excluded,
    institutional, civilizational, trapped, national).

% The scholar or analyst evaluating whether this reading of commitment installation matches historical evidence and structural dynamics. Measures the claim against cases like Meiji Restoration bureaucratic reforms, Soviet collectivization decrees, post-colonial state institution-building, Napoleonic legal codes, and modern regulatory overhauls. Tests whether adoption is actually top-down without grassroots validation, whether resistance is suppressed at base, and whether the state genuinely benefits in legitimacy capital.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, historical_analyst, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_commitment_installation_mechanism__exogenous_imposition_reading, state_authority_apparatus).
narrative_ontology:fixing_cost_class(state_commitment_installation_mechanism__exogenous_imposition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes institutional forms across a territory via centralized authority, solving the coordination problem of fragmented local practices that impede state capacity and tax administration. Provides a single legitimate template that replaces multiple competing local forms.
% TRANSFER_FUNCTION: Moves legitimacy capital from local practice communities and their accumulated expertise (rendered obsolete by decree) to the state apparatus (which demonstrates transformation capacity) and the credentialing system (which performs the justification for the imposition). Moves institutional autonomy from subordinate actors to the state center.
% ABSENT_VOICES: Fringe innovators whose locally-demonstrated superior forms are excluded from the legitimacy process because the exogenous model does not seek evidence—it imposes by authority. Their counterfactual claim that bottom-up validation would have produced the same or better outcomes cannot be heard within the imposed framework. Competing jurisdictions within the state are also excluded from input into the mandate.
% DISAPPEARANCE_RATIONALE: If the state's mandate-backed imposition mechanism vanished, institutional forms would revert to local adaptation and pluralism. The standardization only persists because the state continuously enforces it through credential systems, promotion incentives, legal penalties for non-compliance, and the threat of state capacity withdrawal. Without the enforcement apparatus, the commitment would fragment back into local variations.
% FOUNDING_PROBLEM: Early-modern and modern states faced a fragmented institutional landscape that resisted uniform tax administration, military organization, and legal authority. Local institutional forms (guild systems, feudal tenure, religious jurisdiction, communal governance) overlapped and contradicted, making central coordination impossible. The founding problem is the state's need to rationalize and standardize institutional forms to build extractive and military capacity.
% FOUNDING_PROBLEM_CORROBORATION: Historians of state formation (Tilly, Strayer, Spruyt) confirm the founding problem is real: early-modern European states that succeeded (France, Spain, Russia) were those that imposed standardized institutional forms despite local resistance. Contemporary development literature (World Bank, state-building missions) attests the problem is live in post-conflict and post-colonial contexts. The state apparatus itself mandates institutional standardization as an ongoing requirement. No independent voice outside the state authority itself attests the problem is solved or the imposition justified—only the state's own legitimacy system does so.
narrative_ontology:disappearance_verdict(state_commitment_installation_mechanism__exogenous_imposition_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_commitment_installation_mechanism__exogenous_imposition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_commitment_installation_mechanism__exogenous_imposition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_commitment_installation_mechanism__exogenous_imposition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness rises from 0.45 to 0.68 over the interval as the state consolidates control: initial imposition (t=0) includes contested coordination benefits; by t=50, the state is extracting legitimacy capital and institutional autonomy with diminishing functional return. Suppression rises steadily (0.58→0.76) because resistance at the base requires increasing enforcement investment—local communities continue to resist displacement even as compliance grows. Theater ratio rises (0.22→0.48) indicating the legitimacy justification becomes increasingly performative: the state continues to frame imposition as technical/expert rationalization, but the actual function (state capacity building) is now visible. The constraint is tangled_rope because it solves genuine coordination (standardization enables state capacity) while simultaneously extracting (autonomy flows to center, legitimacy accrues to state, practitioners are displaced). Beneficiaries are the state apparatus and credentialing system; victims are subordinate actors and local communities. The exogenous reading is distinguished by the absence of grassroots validation and the presence of state coercion as the primary legitimation mechanism.
 *
 * PERSPECTIVAL GAP:
 *   From the state apparatus seat: this is successful institutional modernization, rationally designed and mandated by authority; resistance is reactionary, theater is explanation of necessity. From the subordinate institutional actors' seat: this is coercive displacement of legitimate local forms by an authority wielding power-over rather than power-with; theater is propaganda masking loss of autonomy. From the fringe innovators' seat (excluded): the state imposed the wrong commitment, dismissing local evidence of superior forms because the exogenous model consults only authority, not evidence. The engine will compute different per-seat classifications: the state seat as beneficiary (near 0.0 directionality), the victim seats as targets (near 1.0 directionality). This divergence is structural, not an error—the reading permits no reconciliation.
 *
 * DIRECTIONALITY LOGIC:
 *   State authority apparatus: d ≈ 0.05 (full beneficiary, institutional power, arbitrage exit, controls the rules). Subordinate institutional actors: d ≈ 0.85 (full targets, organized power but constrained exit, forced to adopt). Local practice communities: d ≈ 0.92 (full targets, powerless, identity-locked to old forms, no exit option except internal departure). Credentialing system: d ≈ 0.15 (net beneficiary, institutional power, gains legitimacy capital from being positioned as neutral arbiter). Fringe innovators: d ≈ 0.75 (targets despite moderate power, because excluded from decision process and their knowledge is suppressed; trapped at margin by state monopoly on legitimacy). No directionality overrides needed; the structural data (beneficiary/victim declarations + exit + power) derives the correct asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is tangled_rope, not snare, because it coordinates standardization (genuine public good from state capacity) alongside extraction (legitimacy/autonomy transfer). The presence of genuine coordination function (what snares lack) prevents misclassification as pure extraction. However, the measurement series show extractiveness rising while coordination function plateaus, indicating mandatrophy is live: the founding problem (fragmented institutional landscape) is increasingly solved, but the constraint persists because the state continues to enforce imposition-by-decree. By t=50, the extraction function has become dominant (theater_ratio=0.48, suppression=0.76), suggesting the commitment mechanism has begun sliding toward snare—extraction sustained by suppression rather than by demonstrated coordination benefit. This is a mandatrophy trajectory observable in post-imposition stabilization phases: when local adaptation to new forms succeeds, the coordination benefit declines but suppression must rise to prevent reversion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    imposition_vs_adoption_equivalence,
    'Does the exogenous imposition model accurately describe historical state commitment installation, or are historical cases always hybrid (imposition + selective adoption + fringe validation)?',
    'Case analysis of state reforms: Meiji Restoration bureaucratic forms, Soviet collectivization, post-colonial institution-building, Napoleonic codes. Measure whether adoption required zero fringe validation (exogenous pure) or some fringe evidence of superiority (hybrid). Distinguish enforced nominal compliance from genuine institutional adoption.',
    'If all historical cases show hybrid dynamics, this reading''s claim to pure exogenous imposition is counterfactual; the actual constraint would be hybrid_cascade. If cases show pure exogenous imposition with later collapse, this reading correctly models a transition phase. If exogenous imposition is stable over long horizons, the reading is vindicated and mandatrophy analysis would be wrong.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imposition_vs_adoption_equivalence, empirical, 'Whether exogenous imposition is a historical reality or a caricature of hybrid processes.').

omega_variable(
    subordinate_actor_agency_in_apparent_compliance,
    'When state mandates appear to be adopted (compliance reported, new forms nominally in place), how much is genuine institutional acceptance vs. covert non-compliance, strategic reinterpretation, or performance for authority?',
    'Ethnographic study of institutional practice post-imposition: interviews with local practitioners, observation of actual workflow vs. formal procedure, analysis of how implemented forms differ from mandated forms. Measure gap between official adoption and functional practice.',
    'If subordinate actors covertly revert or strategically reinterpret mandates, the suppression requirement is much higher than measured, and the constraint''s stability depends on continuous enforcement intensity invisible to administrative reports. The theater ratio interpretation shifts: what appears as functional commitment may be theater masking non-compliance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(subordinate_actor_agency_in_apparent_compliance, empirical, 'Whether measured compliance reflects genuine adoption or performative compliance with covert non-adherence.').

omega_variable(
    fringe_alternative_actually_superior,
    'Does the fringe-excluded position mean the state dismissed actually superior institutional forms that would have succeeded if adopted, or were the fringe innovations genuinely marginal and the state''s choice correct despite excluding their input?',
    'Counterfactual institutional analysis: implement fringe alternatives in controlled settings, measure performance metrics (stability, efficiency, adaptive capacity) against the state-imposed form over the same interval. Natural experiments from jurisdictions that permitted fringe forms to scale.',
    'If fringe forms consistently outperform state-imposed forms, the exogenous reading''s exclusion mechanism is extractive cover for state power, not rational standardization. If state forms perform equally or better, the exclusion was merely procedurally unjust but substantively correct—the reading''s coordination function is vindicated but its suppression remains extraction of process autonomy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fringe_alternative_actually_superior, empirical, 'Whether excluded fringe alternatives were actually superior institutional forms.').

omega_variable(
    state_apparatus_beneficiary_status_contested,
    'Does the state apparatus genuinely benefit from commitment imposition (consolidates control, builds capacity, increases legitimacy), or does the apparatus merely appear to benefit while real control remains fragmented and theater masks state incapacity?',
    'Measure state capacity metrics pre/post-imposition: tax collection efficiency, military conscription success, bureaucratic reach into territory, legal compliance rates. Assess whether state authority actually expanded or whether imposition was aspirational.',
    'If the state genuinely consolidated capacity, the exogenous reading describes a real extraction mechanism (state extracts autonomy in exchange for functional standardization). If the state failed to consolidate despite imposition, the constraint is piton, not tangled_rope: the apparatus maintains the imposition theater without functional capacity to deliver, benefiting only the credentialing system that performs justification while actual control remains distributed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_apparatus_beneficiary_status_contested, empirical, 'Whether state apparatus genuinely benefits from imposition or whether benefits are theater masking state incapacity.').

omega_variable(
    kernel_reading_boundary_precision,
    'Is the distinction between exogenous_imposition (this reading) and hybrid_cascade (sibling) clearly demarcated by fringe_validation_role, or do the readings blur into each other when the state performs token fringe consultation that is not outcome-determining?',
    'Define operationally: exogenous=zero fringe input into mandate decision; hybrid=some fringe evidence shapes mandate content. Examine borderline cases where state conducts commissions/expert input but ignores recommendations. Does selective token consultation count as hybrid or exogenous?',
    'If reading boundary is fuzzy, both readings claim the same cases and compete for classification rather than partition empirical territory. The kernel contest becomes indeterminate. If boundary is sharp (outcome-determining fringe input vs. none), the readings are properly distinct and case assignments are unambiguous.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_boundary_precision, conceptual, 'Whether the exogenous/hybrid distinction is empirically sharp or conceptually blurred.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_commitment_installation_mechanism__exogenous_imposition_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(stat_tr_t0, observed).
narrative_ontology:measurement(stat_tr_t8, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement_basis(stat_tr_t8, observed).
narrative_ontology:measurement(stat_tr_t16, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 16, 0.39).
narrative_ontology:measurement_basis(stat_tr_t16, observed).
narrative_ontology:measurement(stat_tr_t25, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 25, 0.45).
narrative_ontology:measurement_basis(stat_tr_t25, observed).
narrative_ontology:measurement(stat_tr_t37, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 37, 0.48).
narrative_ontology:measurement_basis(stat_tr_t37, observed).
narrative_ontology:measurement(stat_tr_t50, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 50, 0.48).
narrative_ontology:measurement_basis(stat_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(stat_be_t0, observed).
narrative_ontology:measurement(stat_be_t8, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement_basis(stat_be_t8, observed).
narrative_ontology:measurement(stat_be_t16, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 16, 0.61).
narrative_ontology:measurement_basis(stat_be_t16, observed).
narrative_ontology:measurement(stat_be_t25, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement_basis(stat_be_t25, observed).
narrative_ontology:measurement(stat_be_t37, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 37, 0.68).
narrative_ontology:measurement_basis(stat_be_t37, observed).
narrative_ontology:measurement(stat_be_t50, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(stat_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(stat_su_t0, observed).
narrative_ontology:measurement(stat_su_t8, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 8, 0.65).
narrative_ontology:measurement_basis(stat_su_t8, observed).
narrative_ontology:measurement(stat_su_t16, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 16, 0.72).
narrative_ontology:measurement_basis(stat_su_t16, observed).
narrative_ontology:measurement(stat_su_t25, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 25, 0.75).
narrative_ontology:measurement_basis(stat_su_t25, observed).
narrative_ontology:measurement(stat_su_t37, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 37, 0.76).
narrative_ontology:measurement_basis(stat_su_t37, observed).
narrative_ontology:measurement(stat_su_t50, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 50, 0.76).
narrative_ontology:measurement_basis(stat_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_commitment_installation_mechanism__exogenous_imposition_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.12).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__exogenous_imposition_reading, state_commitment_installation_mechanism__endogenous_climb_reading).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__exogenous_imposition_reading, state_commitment_installation_mechanism__hybrid_cascade_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a contested kernel. The 'state_commitment_installation_mechanism' kernel describes the process by which new institutional commitments gain legitimacy in state-building contexts. Three structurally distinct constraint stories decompose this kernel: (1) exogenous_imposition_reading (this story)—legitimacy flows from top-down authority decree, no grassroots validation, extraction of autonomy sustained by suppression; (2) endogenous_climb_reading—legitimacy flows from demonstrated local superiority, fringe-to-center validation, coordination without extraction; (3) hybrid_cascade_reading—imposition at apex cascades downward but requires fringe validation to stabilize, coordination with conditional extraction. Each reading instantiates a different ε, different beneficiary/victim sets, and different suppression requirements. The readings are not different measurements of the same constraint—they are different constraints grounded in different assumptions about the role of authority vs. evidence in legitimacy-creation. Network edges link the readings: exogenous_imposition influences both siblings by foreclosing the possibility of pure endogenous climb (if exogenous mechanisms are strong, fringe climb is suppressed) and by representing the antithesis to hybrid (if state requires no fringe validation, then hybrid is empirically false). Sibling readings inherit the same kernel_id but carry different reading_id values and different cs_structure configurations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_commitment_installation_mechanism__exogenous_imposition_reading, powerful, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
