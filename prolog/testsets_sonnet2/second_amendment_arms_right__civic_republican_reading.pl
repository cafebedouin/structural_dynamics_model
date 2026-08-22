% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__civic_republican_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_arms_right__civic_republican_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: second_amendment_arms_right__civic_republican_reading
 *   human_readable: Second Amendment Arms Right — Civic Republican (Militia-Citizenship) Reading
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the civic-republican reading of the Second
 *   Amendment kernel: the right to keep and bear arms is protected not as a
 *   free-standing individual liberty nor as a mere incident of state militia
 *   authority, but as an inseparable pairing of right and duty grounded in
 *   the citizen's role in collective self-governance and defense against
 *   tyranny. This reading treats civic participation (readiness, training,
 *   community defense embeddedness) as the interpretive center, which gives
 *   regulatory authority more room to condition or structure the right around
 *   participatory norms than a pure individual-liberty reading would allow,
 *   while giving individual claimants more protection than a pure
 *   collective/state-authority reading would allow when their claim is
 *   civic-participation-adjacent. The rising extraction and suppression
 *   trajectories reflect a reading whose doctrinal machinery (defining who
 *   counts as sufficiently 'civic,' administering training/qualification
 *   regimes) has grown more elaborate and more actively enforced over the
 *   interval as courts and legislatures work out its implications.
 *
 * KEY AGENTS:
 *   - civic_militia_eligible_citizens: dual beneficiary and payer — hold the strengthened right but also bear the implicit civic duty
 *   - citizens_excluded_from_militia_eligibility: bear the cost of a framework historically and functionally conditioned on eligibility they lack or lacked
 *   - unorganized_arms_owners_outside_civic_frame: weaker claim than under individual-right reading
 *   - state_and_federal_legislatures: administer the civic-participation-linked regulatory regime
 *   - courts_applying_second_amendment_doctrine: adjudicate the civic-core/individual-liberty boundary
 *   - historically_disarmed_populations: excluded from the founding-era concept this reading anchors to
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__civic_republican_reading, 0.38).
domain_priors:suppression_score(second_amendment_arms_right__civic_republican_reading, 0.42).
domain_priors:theater_ratio(second_amendment_arms_right__civic_republican_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__civic_republican_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_arms_right__civic_republican_reading, "Second Amendment Arms Right — Civic Republican (Militia-Citizenship) Reading").
narrative_ontology:topic_domain(second_amendment_arms_right__civic_republican_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(second_amendment_arms_right__civic_republican_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__civic_republican_reading, '4dcfd640-96d4-4c00-8280-887dd9fb51af').
narrative_ontology:cs_kernel_codification('4dcfd640-96d4-4c00-8280-887dd9fb51af', fixed_text).
narrative_ontology:cs_authority_grounding('4dcfd640-96d4-4c00-8280-887dd9fb51af', lineage).
narrative_ontology:cs_interpretation_layer_present('4dcfd640-96d4-4c00-8280-887dd9fb51af').
narrative_ontology:cs_reading_relation('4dcfd640-96d4-4c00-8280-887dd9fb51af', second_amendment_arms_right__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('4dcfd640-96d4-4c00-8280-887dd9fb51af', second_amendment_arms_right__collective_right_reading, influences).
narrative_ontology:cs_axiom('4dcfd640-96d4-4c00-8280-887dd9fb51af', foundational, arms_right_conditioned_on_civic_participation_ideal).
narrative_ontology:cs_axiom_status(arms_right_conditioned_on_civic_participation_ideal, holdable).
narrative_ontology:cs_axiom_grounding('4dcfd640-96d4-4c00-8280-887dd9fb51af', arms_right_conditioned_on_civic_participation_ideal, conventional).
narrative_ontology:cs_axiom('4dcfd640-96d4-4c00-8280-887dd9fb51af', foundational, citizen_soldier_capacity_as_check_on_tyranny).
narrative_ontology:cs_axiom_status(citizen_soldier_capacity_as_check_on_tyranny, holdable).
narrative_ontology:cs_axiom_grounding('4dcfd640-96d4-4c00-8280-887dd9fb51af', citizen_soldier_capacity_as_check_on_tyranny, empirically_contingent).
narrative_ontology:cs_reference_frame('4dcfd640-96d4-4c00-8280-887dd9fb51af', founding_era_civic_militia_synthesis).
narrative_ontology:cs_drift_state('4dcfd640-96d4-4c00-8280-887dd9fb51af', post_heller_doctrinal_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4dcfd640-96d4-4c00-8280-887dd9fb51af', '').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__civic_republican_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__civic_republican_reading, civic_militia_eligible_citizens).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__civic_republican_reading, republican_self_governance_project).
narrative_ontology:constraint_victim(second_amendment_arms_right__civic_republican_reading, citizens_excluded_from_militia_eligibility).
narrative_ontology:constraint_victim(second_amendment_arms_right__civic_republican_reading, unorganized_arms_owners_outside_civic_frame).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(second_amendment_arms_right__civic_republican_reading, civic_militia_eligible_citizens).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__civic_republican_reading, civic_virtue_precondition_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__civic_republican_reading, armed_citizenry_as_check_on_tyranny).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adult citizens who meet the civic-republican reading's implicit eligibility profile (able-bodied, willing to train, embedded in a local community defense tradition) hold both the right to keep arms and an implicit civic duty to maintain proficiency and readiness. They benefit from constitutional protection of their arms but are also expected to bear training, qualification, and periodic muster-like obligations that a purely individualist reading would not impose. Exit from the duty component is not really available if they wish to retain the strongest form of protection this reading offers.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, civic_militia_eligible_citizens, beneficiary,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_arms_right__civic_republican_reading, civic_militia_eligible_citizens, payer).

% Historically and in some contemporary applications, groups deemed outside the 'able-bodied citizen-soldier' frame (due to disability, age, historically due to race or sex) find their arms rights treated as weaker or contingent because they fall outside the civic-participation ideal the reading centers. They bear the cost of a framework that ties gun rights to a civic-virtue prerequisite they cannot or were not permitted to satisfy.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, citizens_excluded_from_militia_eligibility, payer,
    powerless, biographical, trapped, national).

% Individuals who own arms for self-defense or recreation without any connection to militia service, training, or civic defense ideology find their claim to protection weaker under this reading than under the pure individual-right reading, since the civic-republican frame conditions strong protection on participation in (or eligibility for) the militia-citizenship role. Their exit is constrained because courts applying this reading may treat their claims as less central to the amendment's purpose.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, unorganized_arms_owners_outside_civic_frame, payer,
    moderate, biographical, constrained, national).

% Legislatures administer training, registration, and qualification regimes that this reading treats as legitimate expressions of the civic-participation norm rather than infringements on an individual liberty. They set the terms of what counts as adequate civic engagement with arms, and can expand or narrow the regulatory footprint depending on how they read the militia-citizenship link, giving them substantial interpretive latitude.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, state_and_federal_legislatures, agenda_setter,
    institutional, generational, arbitrage, national).

% Judges deciding Second Amendment cases under this reading must adjudicate whether a given regulation burdens the civic-republican core (training, militia-adjacent activity, community defense participation) or merely burdens individual gun ownership disconnected from that core. This gives courts discretion to uphold regulations an individual-rights court would strike down, and to strike down militia-participation burdens an individual-rights court might uphold for different reasons.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, courts_applying_second_amendment_doctrine, agenda_setter,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_arms_right__civic_republican_reading, courts_applying_second_amendment_doctrine, observer).

% The abstract project of maintaining a citizenry capable of collective self-defense and resistance to tyranny is advanced whenever arms ownership is tied to civic participation rather than treated as purely personal or purely governmental. It is not an actor and collects no direct rents, but the reading's legitimacy depends on this project remaining a plausible, live justification.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, republican_self_governance_project, beneficiary,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(second_amendment_arms_right__civic_republican_reading, republican_self_governance_project).

% Groups historically barred from militia service (enslaved people, free Black citizens under antebellum militia acts, women until the 20th century) are not present in the founding-era conception this reading looks back to, and are not centrally consulted in how the civic-participation ideal is retrospectively applied today. Their absence from the founding militia concept is a structural silence in the reading's own genealogy.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, historically_disarmed_populations, excluded,
    powerless, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_arms_right__civic_republican_reading, diffuse).
narrative_ontology:fixing_cost_class(second_amendment_arms_right__civic_republican_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ties the individual possession of arms to a civic obligation of readiness for collective self-defense, coordinating a decentralized citizenry into a latent capacity for republican self-governance and resistance to concentrated federal power, without requiring a standing government-controlled military monopoly.
% TRANSFER_FUNCTION: Moves interpretive legitimacy and regulatory latitude toward arrangements that link gun ownership to training, community defense participation, or militia-adjacent civic activity, and away from claims grounded purely in individual autonomy or purely in state militia control; correspondingly shifts constitutional protection away from arms owners whose relationship to guns has no civic-participation dimension.
% ABSENT_VOICES: Populations historically excluded from militia eligibility (enslaved and formerly enslaved people, women, people with disabilities) had no voice in shaping the founding-era militia concept this reading anchors itself to, and their descendants' claims to full protection are treated as derivative rather than foundational under this frame.
% DISAPPEARANCE_RATIONALE: If the civic-republican reading vanished as a live doctrinal option, courts would default entirely to either the individual-right or collective-right poles; regulatory schemes premised on training/qualification-as-civic-duty would lose their strongest justification and would need to be recast as either individual-safety regulation or pure state-militia administration. Proponents say the republican self-governance tradition would lose its clearest constitutional anchor; critics say the doctrinal work this reading does is already performed adequately by the individual-right reading's own 'reasonable regulation' exceptions, so little would actually change in case outcomes.
% FOUNDING_PROBLEM: At the founding, standing armies were feared as instruments of tyranny, and the constitutional solution was to preserve a citizenry capable of arming itself for collective defense and resistance, tying the right to bear arms to the civic duty of militia service rather than treating either as free-standing.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the founding era and some originalist scholars outside the gun-rights advocacy community (citing militia acts, contemporaneous debates over standing armies) attest the civic-participation linkage was genuinely central to the framers' concern. Critics outside both advocacy camps note the practical function of citizen militias was effectively superseded by professional military and National Guard structures by the early 20th century, making the doctrine's contemporary invocation more interpretive-legitimation than functional necessity — a status the reading's own proponents dispute.
narrative_ontology:disappearance_verdict(second_amendment_arms_right__civic_republican_reading, contested).
narrative_ontology:founding_problem_status(second_amendment_arms_right__civic_republican_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__civic_republican_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_arms_right__civic_republican_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_arms_right__civic_republican_reading, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__civic_republican_reading_tests).
:- end_tests(second_amendment_arms_right__civic_republican_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.38 at interval end) is moderate — the civic-republican reading imposes real costs (training/qualification burdens, weaker protection for non-civic-adjacent ownership) but is not primarily extractive in the way a pure rent-seeking arrangement would be; its coordination function (sustaining a citizenry capable of collective self-governance) is genuine. Suppression (0.42) reflects the active doctrinal and regulatory work required to maintain the civic-core/periphery distinction — courts must continually police which claims count as sufficiently civic. Theater ratio (0.28) is present but moderate: some training/qualification regimes function more as symbolic civic performance than as functionally necessary readiness, and this share has grown as actual militia function has become vestigial relative to professional military and law enforcement structures. All three metrics share one time grid across the interval.
 *
 * DIRECTIONALITY LOGIC:
 *   Civic-militia-eligible citizens sit near symmetric-to-beneficiary: they receive strengthened constitutional protection precisely because they satisfy the civic-participation criterion, but they also carry its duties, so their directionality is not purely low-d. Citizens excluded from militia eligibility and unorganized arms owners outside the civic frame are pushed toward the target end because the reading's core logic structurally discounts claims that lack the civic-participation dimension — this is not incidental but definitional to how this reading is more restrictive than the individual-right reading for that population. Legislatures and courts sit at the agenda-setting pole with wide interpretive latitude, which is the source of the moderate suppression score: the boundary they administer is not self-executing.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — fear of standing armies and the need for a citizenry capable of self-defense — is substantially resolved by modern professional military and National Guard institutions, making the tangled_rope classification (rather than pure rope) appropriate: the coordination function persists doctrinally, but its function has been partially supplanted by other institutions, while the reading's costs (weaker protection for non-civic-adjacent owners, ongoing eligibility-boundary litigation) persist and even intensify. Classifying this as tangled_rope rather than snare avoids treating the genuine historical coordination logic as pure pretext, while classifying it as tangled_rope rather than rope or mountain acknowledges the real, asymmetric costs the civic-participation boundary imposes on excluded groups.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    civic_republican_reading_identity,
    'Is the civic-republican reading a historically accurate recovery of the founding-era militia concept, or a modern interpretive construction that retrofits founding-era language to license a middle-ground regulatory posture unavailable to either the individual-right or collective-right poles?',
    'Historical analysis of founding-era militia acts, contemporaneous debates on standing armies, and ratification-era commentary, cross-checked against the doctrinal timeline of when the civic-republican reading was first articulated as a distinct judicial or scholarly position.',
    'If the reading is a genuine historical recovery, its coordination function (sustaining civic-defense capacity) is more substantively grounded and the tangled_rope classification''s coordination half is stronger. If it is a modern retrofit, the reading functions more as an interpretive tool for justifying a particular regulatory posture, shifting the balance toward extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civic_republican_reading_identity, conceptual, 'Historical authenticity vs. modern interpretive construction of the civic-republican reading.').

omega_variable(
    sibling_reading_boundary_location,
    'Where exactly does the civic-republican reading''s boundary with the individual-right reading sit — is civic participation a genuine gating condition on protection, or merely one relevant factor among several that courts applying an individual-right framework would also consider?',
    'Comparative doctrinal analysis of how courts applying each reading would decide the same set of hypothetical regulations (e.g., mandatory training requirements, restrictions on ownership by those with no militia-adjacent activity) — divergent outcomes would locate the boundary precisely.',
    'A sharp boundary supports treating this as a genuinely distinct constraint with its own ε (as authored); if the boundary collapses under scrutiny into a soft-factor version of the individual-right reading, the two constraints may converge in practice even though ε is authored separately here per the ε-invariance principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_boundary_location, conceptual, 'Precision of the doctrinal boundary between civic-republican and individual-right readings.').

omega_variable(
    founding_militia_exclusion_ambiguity,
    'Does the civic-republican reading''s reliance on the founding-era militia concept structurally inherit that concept''s exclusions (of enslaved people, women, and others barred from militia service), or can the reading be extended today without carrying forward that exclusionary genealogy?',
    'Doctrinal tracing of whether contemporary applications of the civic-republican reading explicitly disclaim the founding-era eligibility criteria or implicitly reproduce them through facially neutral ''civic participation'' proxies (e.g., criminal history, mental health adjudications) that correlate with historically excluded status.',
    'If the exclusionary genealogy is inherited, the victim group (citizens_excluded_from_militia_eligibility) is larger and more structurally embedded than a surface reading suggests, pushing the classification further from rope and more firmly into tangled_rope or even snare territory for that subgroup.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_militia_exclusion_ambiguity, empirical, 'Whether founding-era militia exclusions persist through facially neutral eligibility proxies today.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__civic_republican_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_arms_right__civic_republican_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(seco_tr_t8, second_amendment_arms_right__civic_republican_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(seco_tr_t16, second_amendment_arms_right__civic_republican_reading, theater_ratio, 16, 0.21).
narrative_ontology:measurement(seco_tr_t24, second_amendment_arms_right__civic_republican_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement(seco_tr_t32, second_amendment_arms_right__civic_republican_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement(seco_tr_t40, second_amendment_arms_right__civic_republican_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(seco_be_t8, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 8, 0.27).
narrative_ontology:measurement(seco_be_t16, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 16, 0.31).
narrative_ontology:measurement(seco_be_t24, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 24, 0.34).
narrative_ontology:measurement(seco_be_t32, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 32, 0.36).
narrative_ontology:measurement(seco_be_t40, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 40, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(seco_su_t8, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 8, 0.33).
narrative_ontology:measurement(seco_su_t16, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 16, 0.36).
narrative_ontology:measurement(seco_su_t24, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 24, 0.39).
narrative_ontology:measurement(seco_su_t32, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 32, 0.41).
narrative_ontology:measurement(seco_su_t40, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 40, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__civic_republican_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(second_amendment_arms_right__civic_republican_reading, 0.1).
narrative_ontology:affects_constraint(second_amendment_arms_right__civic_republican_reading, second_amendment_arms_right__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__civic_republican_reading, second_amendment_arms_right__collective_right_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the single natural-language label 'the Second Amendment right.' Each reading (individual_right_reading, collective_right_reading, civic_republican_reading) is authored as its own constraint with its own ε, beneficiary/victim structure, and claimed type, per the ε-invariance principle — they are not the same constraint viewed from different angles but three structurally distinct claims sharing a contested kernel. The civic-republican reading sits doctrinally between its two siblings: it shares the individual-right reading's grant of protection to persons rather than only to state militias, but shares the collective-right reading's willingness to condition or structure that protection around a civic/institutional function rather than treating it as unconditioned personal liberty.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
