% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__popular_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__popular_sovereignty_reading, []).

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
 *   constraint_id: secession_legitimacy_boundary__popular_sovereignty_reading
 *   human_readable: Popular Sovereignty Reading of Provincial Secession Legitimacy
 *   domain: political/federalism/resource_politics
 *
 * SUMMARY:
 *   This story instantiates the popular sovereignty reading of the secession
 *   legitimacy boundary kernel: the claim that a democratic majority within
 *   provincial boundaries holds ultimate sovereignty and that a referendum
 *   result is self-legitimating, requiring no further constitutional, treaty,
 *   or grievance-threshold validation. Under this reading, federal
 *   constitutional authority is subordinate to the counted majority, and
 *   claims of unfair extraction by the federation are valid whenever the
 *   provincial majority perceives them as such — perception substitutes for
 *   adjudication. This is generated as a single, ε-invariant constraint: it
 *   does not describe or average over the sibling readings
 *   (constitutional_impossibility_reading, grievance_threshold_reading,
 *   treaty_primacy_reading), which are separate constraint stories with their
 *   own ε values and structures, linked via network.affects_constraints.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__popular_sovereignty_reading, 0.61).
domain_priors:suppression_score(secession_legitimacy_boundary__popular_sovereignty_reading, 0.55).
domain_priors:theater_ratio(secession_legitimacy_boundary__popular_sovereignty_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__popular_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(secession_legitimacy_boundary__popular_sovereignty_reading, "Popular Sovereignty Reading of Provincial Secession Legitimacy").
narrative_ontology:topic_domain(secession_legitimacy_boundary__popular_sovereignty_reading, "political/federalism/resource_politics").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__popular_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__popular_sovereignty_reading, '80ec0874-cc78-46cf-96d0-3ba07acae941').
narrative_ontology:cs_kernel_codification('80ec0874-cc78-46cf-96d0-3ba07acae941', distributed).
narrative_ontology:cs_authority_grounding('80ec0874-cc78-46cf-96d0-3ba07acae941', distributed).
narrative_ontology:cs_reading_relation('80ec0874-cc78-46cf-96d0-3ba07acae941', secession_legitimacy_boundary__constitutional_impossibility_reading, forecloses).
narrative_ontology:cs_reading_relation('80ec0874-cc78-46cf-96d0-3ba07acae941', secession_legitimacy_boundary__grievance_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('80ec0874-cc78-46cf-96d0-3ba07acae941', secession_legitimacy_boundary__treaty_primacy_reading, influences).
narrative_ontology:cs_axiom('80ec0874-cc78-46cf-96d0-3ba07acae941', foundational, majority_referendum_self_legitimating).
narrative_ontology:cs_axiom_status(majority_referendum_self_legitimating, holdable).
narrative_ontology:cs_axiom_grounding('80ec0874-cc78-46cf-96d0-3ba07acae941', majority_referendum_self_legitimating, conventional).
narrative_ontology:cs_axiom('80ec0874-cc78-46cf-96d0-3ba07acae941', secondary, provincial_boundary_is_sovereign_unit).
narrative_ontology:cs_axiom_status(provincial_boundary_is_sovereign_unit, holdable).
narrative_ontology:cs_axiom_grounding('80ec0874-cc78-46cf-96d0-3ba07acae941', provincial_boundary_is_sovereign_unit, conventional).
narrative_ontology:cs_reference_frame('80ec0874-cc78-46cf-96d0-3ba07acae941', westphalian_popular_sovereignty_norm).
narrative_ontology:cs_drift_state('80ec0874-cc78-46cf-96d0-3ba07acae941', post_quebec_scotland_catalonia_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('80ec0874-cc78-46cf-96d0-3ba07acae941', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_majority_bloc).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__popular_sovereignty_reading, secessionist_provincial_government).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_minority_residents).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, federal_taxpayers_outside_province).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, indigenous_nations_within_province).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Votes as a bloc in the referendum and treats the resulting majority as sufficient in itself to authorize withdrawal from the federation and unilateral claim over provincial resources and institutions. Frames federal counterclaims as illegitimate interference with self-determination. Bears none of the transition costs disproportionately — those fall on minorities and non-resident interests.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_majority_bloc, beneficiary,
    organized, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_majority_bloc, agenda_setter).

% Administers the referendum process, sets the threshold question and timing, and moves to claim exclusive control of provincial territory, resource revenue, and institutions the moment a majority is recorded. Converts a bare vote-count into an assertion of full sovereign authority, foreclosing negotiation with actors who were not asked.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, secessionist_provincial_government, agenda_setter,
    institutional, generational, arbitrage, regional).

% Voted against secession or would have, but are bound by the majority result inside the same boundary lines. Citizenship, currency, pensions, and legal status become contested overnight; exit means abandoning home, property, and community ties on a timeline they did not choose.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_minority_residents, payer,
    moderate, biographical, constrained, regional).

% Have subsidized shared federal infrastructure, transfers, and debt now unilaterally repudiated or renegotiated on terms set entirely by the seceding province. Had no vote in the referendum but absorb the fiscal and constitutional shock of its outcome.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, federal_taxpayers_outside_province, payer,
    powerless, biographical, trapped, national).

% Hold pre-existing treaty relationships with the federal Crown that the referendum's boundary lines and majority-rule logic simply do not address. The popular sovereignty reading treats provincial territory as a settled unit available for majority disposal, overriding nations who were never incorporated into that majority in the first place.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, indigenous_nations_within_province, payer,
    powerless, civilizational, trapped, regional).

% Under this reading, federal constitutional authority is treated as subordinate to the referendum result and has no independent veto — its objections are recast as illegitimate obstruction of self-determination rather than as a constitutionally cognizable interest.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, federal_government, excluded,
    institutional, generational, constrained, national).

% Assess whether the referendum meets thresholds of free expression, fair process, and clear majority under international self-determination norms, and whether recognition of the new state should follow — without power to compel any party inside the boundary.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, international_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(secession_legitimacy_boundary__popular_sovereignty_reading, secessionist_provincial_government).
narrative_ontology:fixing_cost_class(secession_legitimacy_boundary__popular_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clean, legible decision rule for resolving a genuine collective-action problem — settling contested political authority — by referring it to a single countable event (the referendum) rather than protracted, indefinite negotiation.
% TRANSFER_FUNCTION: Moves sovereign authority, resource revenue, fiscal obligations, and institutional control from the federal government and non-consenting residents to the secessionist provincial government and its majority bloc, on the strength of a bare majority vote within boundaries the majority itself does not have to justify.
% ABSENT_VOICES: Indigenous nations whose treaty relationships predate the provincial boundary were not separately consulted; provincial minority residents who voted no are bound regardless; federal taxpayers outside the province had no vote at all. All three are structurally outside the 'majority' the reading treats as self-legitimating.
% DISAPPEARANCE_RATIONALE: The secessionist government and majority bloc would say the world rearranges catastrophically — their sovereignty claim evaporates. Federal authorities and treaty nations would say the underlying constitutional and treaty order was never actually displaced by a vote count, so nothing that was ever legitimately settled changes; only an assertion collapses. The parties dispute which world we are actually in.
% FOUNDING_PROBLEM: A durable, deeply felt disagreement between a provincial population and the federal center over cultural, fiscal, or resource control that ordinary federal politics repeatedly fails to resolve, creating pressure for a decisive mechanism to settle the question.
% FOUNDING_PROBLEM_CORROBORATION: The secessionist government and majority bloc attest the grievance is live and unresolved by federal politics. Federal officials and constitutional scholars outside the province attest that federal mechanisms (transfers, devolution, amendment processes) remain available and untried or under-tried, and that the referendum substitutes a majority head-count for the harder constitutional work required to actually resolve the underlying dispute. Indigenous nations attest that their own founding problem — treaty relationship with the Crown — is not the same problem at all and is not touched by settling it.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__popular_sovereignty_reading, contested).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__popular_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__popular_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(secession_legitimacy_boundary__popular_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__popular_sovereignty_reading, 0.61, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__popular_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(secession_legitimacy_boundary__popular_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(secession_legitimacy_boundary__popular_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction rises over the interval (0.38 to 0.61) as the referendum process moves from a diffuse political grievance toward a concrete assertion of exclusive control over territory, revenue, and institutions — the transfer becomes more consequential and more unilateral as the process matures. Suppression rises in step (0.30 to 0.55) as the secessionist government must increasingly treat federal objections, minority dissent, and treaty claims as noise to be overridden rather than positions to be negotiated, since the reading's own logic gives it no vocabulary for accommodating them. Theater ratio stays comparatively low and rises only modestly (0.15 to 0.28): the referendum mechanism does real political work (it genuinely aggregates preferences within the counted population) even though its self-legitimating claim overreaches what that aggregation can support.
 *
 * PERSPECTIVAL GAP:
 *   From the majority bloc's seat, the referendum result IS the whole of legitimacy — asking for more validation is itself the illegitimate move. From the minority, non-resident, and treaty-nation seats, the same event is an assertion of authority over people who were never asked, dressed in procedural legitimacy borrowed from an election they either lost or were never entered into. The engine computes these as structurally different positions from the declared power/exit/scope data; this story does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   The provincial majority bloc and the secessionist government sit at the beneficiary end: they set the terms of the vote, control its administration, and inherit sovereign authority, resource control, and institutional power from a favorable count. Provincial minority residents, federal taxpayers outside the province, and indigenous nations within it sit at the target end: each bears real costs (displaced citizenship status, unilaterally reallocated fiscal obligations, boundary claims that ignore treaty relationships) without having consented through the specific majority the reading treats as sufficient. The federal government is structurally excluded from a veto role under this reading despite bearing major downstream costs, which is why it is marked excluded rather than payer — its objection is treated as illegitimate rather than as a competing claim requiring resolution.
 *
 * MANDATROPHY ANALYSIS:
 *   The coordination function this reading solves is real: federations do sometimes reach genuine impasses that ordinary politics cannot resolve, and a clear decision procedure has value. Classifying this as tangled_rope rather than snare preserves that the referendum is not pure theater — it does aggregate a real majority preference within its counted population. But the reading's self-legitimating move (treating the count alone as sufficient to override treaty holders, non-resident taxpayers, and internal minorities without their separate consent) is the extraction layer riding on top of the coordination function, and it requires active enforcement (border/institutional claims, revenue seizure, legal assertion) to hold against those who never consented to the counted boundary in the first place.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    referendum_threshold_sufficiency,
    'Does a bare majority (50%+1) within an administratively drawn provincial boundary constitute sufficient democratic warrant for unilateral secession, or does legitimate self-determination require a supermajority, sustained preference over time, or external validation?',
    'Comparative analysis of historical secession referenda and their subsequent stability/legitimacy outcomes; international law scholarship on self-determination thresholds.',
    'If a bare majority is insufficient, the popular sovereignty reading''s self-legitimating claim collapses into the grievance_threshold_reading or requires supplementation by negotiated process, sharply reducing the extractiveness this reading can claim as procedurally clean.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(referendum_threshold_sufficiency, conceptual, 'Whether the referendum''s numerical threshold alone can bear the legitimacy weight this reading places on it.').

omega_variable(
    boundary_naturalness_vs_construction,
    'Is the provincial boundary itself a natural unit of political self-determination, or a historically constructed administrative line that this reading treats as naturally sovereign in order to make the majority within it dispositive?',
    'Historical review of how the provincial boundary was drawn (colonial administration, treaty negotiation, federal statute) and whether affected populations (especially indigenous nations) had any voice in its formation.',
    'If the boundary is a constructed artifact rather than a natural political unit, the reading''s core move — treating the majority within that boundary as sovereign — inherits the arbitrariness of the boundary itself, weakening its self-legitimating claim substantially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_naturalness_vs_construction, conceptual, 'Whether the provincial boundary the majority is counted within is itself a legitimate unit of sovereignty.').

omega_variable(
    committer_structure_reading_location,
    'This constraint is one reading (popular_sovereignty_reading) of the secession_legitimacy_boundary kernel. Where exactly does the disagreement with treaty_primacy_reading and grievance_threshold_reading live structurally?',
    'The disagreement is not about facts on the ground but about which prior claim (majority count, treaty consent, injustice threshold, constitutional text) has lexical priority when they conflict. No empirical measurement resolves this; it is a foundational ordering choice each reading makes differently.',
    'If future constitutional or international practice settles a lexical ordering (e.g., treaty consent as an absolute precondition regardless of majority size), the popular_sovereignty_reading''s claim to self-legitimation becomes structurally subordinate rather than supreme, changing its classification from tangled_rope toward snare with respect to treaty nations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_reading_location, conceptual, 'Locates the kernel disagreement as an ordering-priority dispute among the four readings, not a factual dispute.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__popular_sovereignty_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t0, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(sece_tr_t4, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement(sece_tr_t8, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 8, 0.21).
narrative_ontology:measurement(sece_tr_t12, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 12, 0.23).
narrative_ontology:measurement(sece_tr_t16, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 16, 0.25).
narrative_ontology:measurement(sece_tr_t20, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(sece_tr_t24, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(sece_be_t0, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(sece_be_t4, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 4, 0.44).
narrative_ontology:measurement(sece_be_t8, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(sece_be_t12, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(sece_be_t16, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(sece_be_t20, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(sece_be_t24, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 24, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t0, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(sece_su_t4, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 4, 0.36).
narrative_ontology:measurement(sece_su_t8, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(sece_su_t12, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 12, 0.47).
narrative_ontology:measurement(sece_su_t16, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 16, 0.5).
narrative_ontology:measurement(sece_su_t20, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 20, 0.53).
narrative_ontology:measurement(sece_su_t24, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 24, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__popular_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(secession_legitimacy_boundary__popular_sovereignty_reading, 0.1).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary__constitutional_impossibility_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary__grievance_threshold_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary__treaty_primacy_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling constraints decomposing the natural-language concept 'secession legitimacy' per the ε-invariance principle: each reading (popular_sovereignty, constitutional_impossibility, grievance_threshold, treaty_primacy) answers a structurally distinct question about what makes secession legitimate, and each carries its own ε, beneficiary/victim structure, and classification. They are linked here rather than merged because measuring 'secession legitimacy' by the referendum-count observable yields a very different extraction profile than measuring it by the treaty-consent or constitutional-text observable — exactly the signal that indicates decomposition rather than a single parameterized story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
