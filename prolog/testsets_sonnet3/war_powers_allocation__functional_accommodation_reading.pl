% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__functional_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_powers_allocation__functional_accommodation_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: war_powers_allocation__functional_accommodation_reading
 *   human_readable: Functional Accommodation Reading of War Powers Allocation
 *   domain: constitutional_law/separation_of_powers
 *
 * SUMMARY:
 *   This story authors the functional accommodation reading of the war powers
 *   kernel: the claim that the correct allocation of authority between
 *   Congress and the President depends on operational context — imminent
 *   threats permit unilateral executive action, prolonged campaigns require
 *   congressional authorization. This is presented by its proponents as a
 *   pragmatic middle path avoiding both legislative paralysis in emergencies
 *   and unchecked executive war-making in extended conflicts. Structurally,
 *   however, the reading's core mechanism — a contextual, non-bright-line
 *   trigger — is also what allows the executive to control the classification
 *   decision that determines which regime applies, and the ambiguity zone
 *   this creates has widened over the measured interval as deployments
 *   increasingly claim 'imminent threat' status for durations that resemble
 *   prolonged campaigns.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__functional_accommodation_reading, 0.58).
domain_priors:suppression_score(war_powers_allocation__functional_accommodation_reading, 0.62).
domain_priors:theater_ratio(war_powers_allocation__functional_accommodation_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__functional_accommodation_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__functional_accommodation_reading, "Functional Accommodation Reading of War Powers Allocation").
narrative_ontology:topic_domain(war_powers_allocation__functional_accommodation_reading, "constitutional_law/separation_of_powers").

domain_priors:requires_active_enforcement(war_powers_allocation__functional_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__functional_accommodation_reading, '799929e0-8ad8-4f7f-b3c3-77558d720f83').
narrative_ontology:cs_kernel_codification('799929e0-8ad8-4f7f-b3c3-77558d720f83', distributed).
narrative_ontology:cs_authority_grounding('799929e0-8ad8-4f7f-b3c3-77558d720f83', distributed).
narrative_ontology:cs_reading_relation('799929e0-8ad8-4f7f-b3c3-77558d720f83', war_powers_allocation__congressional_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('799929e0-8ad8-4f7f-b3c3-77558d720f83', war_powers_allocation__inherent_executive_reading, coexists_with).
narrative_ontology:cs_axiom('799929e0-8ad8-4f7f-b3c3-77558d720f83', foundational, authority_allocation_tracks_operational_context).
narrative_ontology:cs_axiom_status(authority_allocation_tracks_operational_context, holdable).
narrative_ontology:cs_axiom_grounding('799929e0-8ad8-4f7f-b3c3-77558d720f83', authority_allocation_tracks_operational_context, instrumental).
narrative_ontology:cs_axiom('799929e0-8ad8-4f7f-b3c3-77558d720f83', secondary, categorical_bright_line_rules_are_impractical_for_force_allocation).
narrative_ontology:cs_axiom_status(categorical_bright_line_rules_are_impractical_for_force_allocation, holdable).
narrative_ontology:cs_axiom_grounding('799929e0-8ad8-4f7f-b3c3-77558d720f83', categorical_bright_line_rules_are_impractical_for_force_allocation, empirically_contingent).
narrative_ontology:cs_reference_frame('799929e0-8ad8-4f7f-b3c3-77558d720f83', war_powers_resolution_functional_compromise).
narrative_ontology:cs_drift_state('799929e0-8ad8-4f7f-b3c3-77558d720f83', post_9_11_extended_deployments_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('799929e0-8ad8-4f7f-b3c3-77558d720f83', '').
narrative_ontology:cs_kernel_id(war_powers_allocation__functional_accommodation_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__functional_accommodation_reading, executive_branch).
narrative_ontology:constraint_beneficiary(war_powers_allocation__functional_accommodation_reading, national_security_apparatus).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, congressional_war_powers_committees).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, affected_civilian_populations_in_theater).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, congress_war_powers_committees).
narrative_ontology:constraint_vindicates(war_powers_allocation__functional_accommodation_reading, functional_flexibility_doctrine).
narrative_ontology:constraint_vindicates(war_powers_allocation__functional_accommodation_reading, operational_context_test).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Determines in the first instance whether a given deployment counts as an 'imminent threat' response or a 'prolonged campaign,' and acts unilaterally under the former characterization while resisting congressional demands for authorization. Controls the classified intelligence and operational tempo that make the characterization difficult for outsiders to contest in real time. Benefits from the ambiguity because the burden of forcing reclassification falls on Congress, not on the executive.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, executive_branch, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(war_powers_allocation__functional_accommodation_reading, executive_branch, beneficiary).

% Holds the constitutional authorization power but must first establish that a deployment has crossed from 'imminent threat' into 'prolonged campaign' before its authorization requirement even attaches. Lacks contemporaneous access to the operational facts the executive uses to justify unilateral characterization, and by the time consensus forms that authorization was required, the campaign is often already underway and politically difficult to defund.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, congress_war_powers_committees, payer,
    organized, biographical, constrained, national).

% Operates within the classification the executive assigns, planning and executing deployments with a degree of continuity that the categorical ambiguity protects from interruption by legislative debate. Institutional planning horizons are set assuming the accommodation reading will hold, which itself reinforces the reading's persistence.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, national_security_apparatus, beneficiary,
    institutional, generational, arbitrage, global).

% Bear the direct consequences of military action authorized (or not) under whichever characterization prevails, with no voice in the domestic constitutional dispute over which branch's authority applies. The duration and intensity of the conflict they live through is partly a function of how long the imminent-threat characterization can be sustained before authorization debates force a change.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, affected_civilian_populations_in_theater, payer,
    powerless, immediate, trapped, regional).

% Are the institution best positioned to adjudicate the imminent-threat/prolonged-campaign boundary but routinely decline to do so under political question doctrine, leaving the line to be drawn by the very branches contesting it. Their absence from the adjudication is what allows the ambiguity zone to persist unresolved.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, federal_courts, excluded,
    institutional, generational, analytical, national).

% Study the pattern of executive characterization and congressional acquiescence across historical deployments, documenting how often the 'imminent threat' label is invoked and for how long it is sustained before authorization is sought or the deployment simply concludes without ever triggering the categorical requirement.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, constitutional_law_scholars, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a workable operating rule for the genuine problem that some military threats require response faster than the deliberative authorization process can move, while other engagements are extended enough that legislative buy-in is both feasible and constitutionally appropriate. Without SOME contextual rule, either every response would be paralyzed pending authorization or every use of force would bypass Congress entirely.
% TRANSFER_FUNCTION: Moves the practical initiative over the decision to use force from Congress to the executive in the ambiguous middle range between clear emergency and clear prolonged campaign, and moves the political and human costs of that initiative onto the populations in theater and onto Congress's institutional standing.
% ABSENT_VOICES: Federal courts, which decline to adjudicate the boundary under political question doctrine, and the civilian populations subject to the resulting military action, who have no standing or voice in the domestic separation-of-powers dispute despite bearing its most direct consequences.
% DISAPPEARANCE_RATIONALE: If the functional accommodation reading disappeared, the executive would lose its primary claimed basis for indefinite unilateral characterization; Congress would need to establish a bright-line temporal or scope trigger (as the congressional_primacy_reading urges) or the executive would need to claim inherent authority without a contextual limiting principle (as the inherent_executive_reading urges). Whether this counts as 'world rearranges' or 'contested' depends on which sibling reading fills the vacuum — the accommodation reading's function is precisely to avoid forcing that choice, so its removal forces the underlying kernel dispute into the open.
% FOUNDING_PROBLEM: The constitutional text (Congress declares war; the President is commander-in-chief) does not specify how authority is allocated across the wide range of military actions between full-scale declared war and clearly defensive emergency response, and post-WWII conflicts made a categorical either/or rule impractical.
% FOUNDING_PROBLEM_CORROBORATION: Executive branch legal counsel (OLC opinions across administrations of both parties) attest the functional distinction is a live, necessary operating principle. Congressional Research Service reports and war powers scholars outside the executive attest the 'imminent threat' characterization has been stretched to cover deployments of years-long duration, suggesting the functional test has become a vehicle for avoiding rather than honoring the authorization requirement it purports to preserve.
narrative_ontology:disappearance_verdict(war_powers_allocation__functional_accommodation_reading, contested).
narrative_ontology:founding_problem_status(war_powers_allocation__functional_accommodation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__functional_accommodation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(war_powers_allocation__functional_accommodation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(war_powers_allocation__functional_accommodation_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_powers_allocation__functional_accommodation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_powers_allocation__functional_accommodation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_powers_allocation__functional_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) and suppression (0.62) are moderate-to-substantial rather than extreme because the reading genuinely does solve a real coordination problem (rapid response capability) and is not pure executive aggrandizement — the coordination function is real. But the metrics rise across the interval because the operative distinction (imminent vs. prolonged) has no enforceable threshold, and the party controlling the initial characterization (the executive) has structural incentive and capacity to sustain the more permissive label. Theater ratio (0.48) reflects that a substantial share of the 'context assessment' apparatus (briefings, notifications, consultations) functions as a compliance performance around a decision already made rather than as a genuine joint determination.
 *
 * DIRECTIONALITY LOGIC:
 *   The executive branch and national security apparatus sit near the beneficiary end: they control the classification call, bear low switching costs from characterizing an action either way, and their institutional planning benefits from sustained ambiguity. Congress and civilian populations in theater sit near the target end: Congress bears the political and institutional cost of contesting a characterization after operational momentum has built, and civilians bear the direct human costs of however long the 'imminent threat' framing is sustained, with zero voice in the domestic dispute.
 *
 * MANDATROPHY ANALYSIS:
 *   The functional accommodation reading resists a pure mandatrophy diagnosis because its founding problem — the impracticality of a rigid declared-war/no-force binary — remains genuinely live; a categorical rule really would either paralyze emergency response or eliminate meaningful congressional check. What has drifted is not the founding problem's disappearance but the erosion of any operative boundary within the accommodation, which is why the reading classifies as tangled_rope rather than either rope (if the boundary held) or snare (if no coordination function existed at all).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    characterization_authority_locus,
    'Who has the legitimate authority to determine, in a contested case, whether a given deployment is an ''imminent threat'' response or a ''prolonged campaign'' — and does the absence of a neutral adjudicator make the functional accommodation reading structurally indistinguishable from unchecked executive discretion?',
    'A pattern of federal court willingness to adjudicate war powers boundary disputes on the merits (rather than dismissing under political question doctrine) would establish a neutral characterization authority; continued near-universal dismissal supports the reading that characterization authority defaults entirely to the executive.',
    'If characterization authority is effectively unchecked executive discretion, the functional accommodation reading collapses toward the inherent_executive_reading in practice even while formally endorsing congressional authorization for prolonged campaigns — the ε would rise and the classification would drift toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(characterization_authority_locus, conceptual, 'Whether the accommodation reading has an actual arbiter or defaults to executive self-certification.').

omega_variable(
    committer_structure_sibling_delta,
    'This story is one reading (functional_accommodation_reading) of the war_powers_allocation kernel, alongside congressional_primacy_reading and inherent_executive_reading. Where precisely does the structural disagreement between the three readings live?',
    'Compare the beneficiary/victim structure and ε across all three sibling stories: congressional_primacy_reading would author near-zero executive discretion and correspondingly lower ε for the executive-beneficiary channel (since the coordination function shifts entirely to Congress); inherent_executive_reading would author near-zero congressional authorization requirement and treat the resulting concentration of power as either a rope (executive efficiency) or a mountain (inherent constitutional fact) rather than a tangled_rope.',
    'The disagreement is located specifically in WHERE the authorization trigger sits: congressional_primacy places it at any force beyond immediate defense; inherent_executive places it nowhere (no trigger); functional_accommodation places it at an operationally-defined but practically-unenforced boundary. This story''s tangled_rope classification is a direct consequence of authoring a real-but-unenforced trigger — a bright-line trigger would push toward rope or snare depending on enforcement, and no trigger at all would push toward the inherent_executive_reading''s own classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_sibling_delta, conceptual, 'Locating the structural disagreement among the three kernel readings.').

omega_variable(
    natural_vs_constructed_ambiguity,
    'Is the imminent-threat/prolonged-campaign distinction a genuine, discoverable feature of military operations (some threats really are time-sensitive in a way that maps cleanly onto a category), or is it a constructed ambiguity that persists because it is useful to the executive branch regardless of its descriptive accuracy?',
    'Examine whether military planners and intelligence analysts internally use a similar temporal/scope distinction for operational (not legal) purposes independent of the constitutional dispute; convergence would support a natural-category reading, divergence would support constructed ambiguity.',
    'If the distinction tracks a genuine operational reality, the tangled_rope classification''s coordination component is more robust; if it is a legal construct with no independent operational referent, the reading is closer to a cover story for extraction and the classification would trend toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_vs_constructed_ambiguity, empirical, 'Whether the operative distinction has independent operational grounding or is legally constructed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__functional_accommodation_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t0, war_powers_allocation__functional_accommodation_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(war__tr_t8, war_powers_allocation__functional_accommodation_reading, theater_ratio, 8, 0.34).
narrative_ontology:measurement(war__tr_t16, war_powers_allocation__functional_accommodation_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement(war__tr_t24, war_powers_allocation__functional_accommodation_reading, theater_ratio, 24, 0.42).
narrative_ontology:measurement(war__tr_t32, war_powers_allocation__functional_accommodation_reading, theater_ratio, 32, 0.45).
narrative_ontology:measurement(war__tr_t40, war_powers_allocation__functional_accommodation_reading, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(war__be_t0, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(war__be_t8, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(war__be_t16, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 16, 0.49).
narrative_ontology:measurement(war__be_t24, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 24, 0.53).
narrative_ontology:measurement(war__be_t32, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(war__be_t40, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t0, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(war__su_t8, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 8, 0.53).
narrative_ontology:measurement(war__su_t16, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 16, 0.56).
narrative_ontology:measurement(war__su_t24, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 24, 0.58).
narrative_ontology:measurement(war__su_t32, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 32, 0.6).
narrative_ontology:measurement(war__su_t40, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_powers_allocation__functional_accommodation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(war_powers_allocation__functional_accommodation_reading, congressional_primacy_reading).
narrative_ontology:affects_constraint(war_powers_allocation__functional_accommodation_reading, inherent_executive_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the war_powers_allocation kernel. congressional_primacy_reading authors a bright-line authorization requirement with correspondingly different ε and beneficiary/victim structure (Congress as agenda_setter rather than payer). inherent_executive_reading authors near-total executive discretion with a different coordination story (efficiency/responsiveness) and likely a different claimed_type. All three share the same underlying constitutional text and historical deployment record but diverge on where authority is located, producing three structurally distinct constraints rather than three measurements of one constraint — consistent with the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
