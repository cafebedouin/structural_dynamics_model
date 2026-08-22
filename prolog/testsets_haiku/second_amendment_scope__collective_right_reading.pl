% ============================================================================
% CONSTRAINT STORY: second_amendment_scope__collective_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_scope__collective_right_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: second_amendment_scope__collective_right_reading
 *   human_readable: Second Amendment Collective Right Reading — State Militia Authority
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   Under the collective-right reading, the Second Amendment is understood as
 *   a constitutional guarantee to STATES and ORGANIZED MILITIAS of the
 *   authority to maintain armed forces, not as a guarantee to individual
 *   citizens of ownership rights independent of militia service. The
 *   prefatory clause — 'A well regulated Militia, being necessary to the
 *   security of a free State' — controls the operative clause, delimiting its
 *   scope to militia-related bearing of arms. This reading vindicates state
 *   regulatory authority: a state can condition firearm ownership on militia
 *   participation, prohibit ownership outside that context, and structure
 *   militia enrollment as it sees fit. The reading asserts that the
 *   Constitution simply does not protect an individual's right to own
 *   firearms for self-defense or any other non-militia purpose. This story
 *   generates ONE constraint — the collective reading's institutionalization
 *   of militia-authority beneficiaries — and does not describe or adjudicate
 *   the competing readings (individual_right_reading, civic_right_reading),
 *   which are separate constraint stories linked via the network.
 *
 * KEY AGENTS:
 *   - State governments: primary institutional beneficiary; hold constitutional authority to define, arm, and control militia structure
 *   - Organized militia systems (National Guard, state guard units): institutional beneficiary; embody the right as defined by this reading
 *   - Individual gun owners: excluded from the beneficiary coalition; their ownership rights derive from state permission, not constitutional protection
 *   - Federal government: observer and partial check; federal authority to regulate interstate commerce is separate from this constraint
 *   - Judiciary: agenda-setter administering the constraint's interpretation; constrained by textual primacy of prefatory clause
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__collective_right_reading, 0.28).
domain_priors:suppression_score(second_amendment_scope__collective_right_reading, 0.15).
domain_priors:theater_ratio(second_amendment_scope__collective_right_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, accessibility_collapse, 0.41).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__collective_right_reading, mountain).
narrative_ontology:human_readable(second_amendment_scope__collective_right_reading, "Second Amendment Collective Right Reading — State Militia Authority").
narrative_ontology:topic_domain(second_amendment_scope__collective_right_reading, "constitutional_law/political_theory").

domain_priors:emerges_naturally(second_amendment_scope__collective_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__collective_right_reading, '60bea69c-9f78-4f6d-9814-f4cac3c23f45').
narrative_ontology:cs_kernel_codification('60bea69c-9f78-4f6d-9814-f4cac3c23f45', fixed_text).
narrative_ontology:cs_authority_grounding('60bea69c-9f78-4f6d-9814-f4cac3c23f45', lineage).
narrative_ontology:cs_interpretation_layer_present('60bea69c-9f78-4f6d-9814-f4cac3c23f45').
narrative_ontology:cs_reading_relation('60bea69c-9f78-4f6d-9814-f4cac3c23f45', second_amendment_scope__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('60bea69c-9f78-4f6d-9814-f4cac3c23f45', second_amendment_scope__civic_right_reading, influences).
narrative_ontology:cs_axiom('60bea69c-9f78-4f6d-9814-f4cac3c23f45', foundational, prefatory_clause_controls_operative_scope).
narrative_ontology:cs_axiom_status(prefatory_clause_controls_operative_scope, holdable).
narrative_ontology:cs_axiom_grounding('60bea69c-9f78-4f6d-9814-f4cac3c23f45', prefatory_clause_controls_operative_scope, empirically_contingent).
narrative_ontology:cs_axiom('60bea69c-9f78-4f6d-9814-f4cac3c23f45', foundational, state_militia_authority_is_primary_right_holder).
narrative_ontology:cs_axiom_status(state_militia_authority_is_primary_right_holder, holdable).
narrative_ontology:cs_axiom_grounding('60bea69c-9f78-4f6d-9814-f4cac3c23f45', state_militia_authority_is_primary_right_holder, deontological).
narrative_ontology:cs_reference_frame('60bea69c-9f78-4f6d-9814-f4cac3c23f45', federalism_with_militia_authority).
narrative_ontology:cs_drift_state('60bea69c-9f78-4f6d-9814-f4cac3c23f45', contemporary_post_heller_jurisprudence, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('60bea69c-9f78-4f6d-9814-f4cac3c23f45', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(second_amendment_scope__collective_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__collective_right_reading, state_governments).
narrative_ontology:constraint_beneficiary(second_amendment_scope__collective_right_reading, organized_militia_systems).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under this reading, state governments retain constitutional authority to regulate firearms as necessary to maintain organized militia capacity. They are the primary holders of the right; they design militia structure and training, and control the context in which citizens bear arms in service of the state militia mission. They collect the regulatory authority and benefit from the constraint's insulation of that authority from individual claims.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, state_governments, beneficiary,
    institutional, generational, analytical, national).

% National Guard units and state-organized militia structures are the institutional embodiment of the right as this reading defines it. They benefit from constitutional protection against federal usurpation of militia authority. The constraint establishes their structure as the lawful locus of the Second Amendment right.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, organized_militia_systems, beneficiary,
    institutional, generational, analytical, national).

% This reading explicitly excludes individual ownership from Second Amendment protection. Individual gun owners are not present at the constitutional table in this framework; any rights they hold derive from state regulatory permission, not constitutional status. They would object that the amendment protects their ownership and self-defense capacity; they are excluded from the beneficiary coalition by the reading's textual and structural interpretation.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, individual_gun_owners, excluded,
    moderate, biographical, constrained, national).

% The federal government is positioned as a check against state militia authority under the collective reading, though federal power to regulate interstate commerce and commerce in firearms is not directly at issue here. Federal institutions and courts interpret the amendment; this reading constrains what they can read into it.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, federal_government, observer,
    institutional, generational, analytical, national).

% Appellate courts and the Supreme Court adjudicate what the amendment means. Under this reading, the judiciary's agenda-setting power is constrained by the prefatory clause and textual primacy: the amendment's language about militia service controls interpretation, limiting what can be read into or around it. Courts administer this constraint through interpretation.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, judiciary, agenda_setter,
    institutional, generational, analytical, national).

% The scholarly tradition reading the amendment's prefatory clause as controlling its operative clause vindicates this constraint's interpretation. This is not an actor but a doctrine-set.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, constitutional_scholars__collective_school, observer,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(second_amendment_scope__collective_right_reading, constitutional_scholars__collective_school).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_scope__collective_right_reading, state_governments).
narrative_ontology:fixing_cost_class(second_amendment_scope__collective_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates defensive militia authority between federal and state governments, and specifies that citizen participation in arms-bearing occurs within organized state militia structures authorized by legislatures, not as a standalone individual right.
% TRANSFER_FUNCTION: Transfers from individual citizens any claim to unilateral, unorganized self-defense firearms ownership under the Constitution, vesting instead the decision-making authority over militia composition and arms access in state governments.
% ABSENT_VOICES: Individual gun owners, self-defense advocates, and private gun manufacturers are excluded from the constitutional coalition this reading recognizes. They would testify that the amendment protects personal ownership and self-defense; they are structurally outside the beneficiary set.
% DISAPPEARANCE_RATIONALE: If this reading vanished and were replaced by one protecting individual ownership, regulatory landscape would shift: states could no longer condition gun ownership on militia service, private ownership could not be restricted to militia participants, and federal and state gun regulations would face heightened constitutional scrutiny. The constitutional authority structure itself would reorganize.
% FOUNDING_PROBLEM: To prevent federal disarmament of state militias and to secure state authority to maintain citizen-based armed forces as a check against federal tyranny.
% FOUNDING_PROBLEM_CORROBORATION: Historians and original-intent scholars aligned with the collective reading attest the founding problem: the framers feared federal monopoly on arms and sought to protect state militia capacity. However, competing scholarship from individual-right advocates and some constitutional historians dispute this reading of the founding intent and argue the militia language was not intended to limit the scope of the right. The founding problem's status is live in historical scholarship but deeply contested between reading traditions.
narrative_ontology:disappearance_verdict(second_amendment_scope__collective_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_scope__collective_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__collective_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_scope__collective_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_scope__collective_right_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_scope__collective_right_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, ExtMetricName, E),
    domain_priors:suppression_score(second_amendment_scope__collective_right_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(second_amendment_scope__collective_right_reading),
    narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(second_amendment_scope__collective_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured as LOW (0.28 at interval end) because this reading, if institutionalized, would NARROW the scope of the Second Amendment right: individuals lose a constitutional claim, and the constraint's primary function is to protect state militia authority, not to extract rents from individual citizens. The beneficiaries (states, militias) are the institutional holders of the right. Suppression is LOW (0.15) because this reading does not require active coercion to suppress competing claims — it settles the matter textually: the amendment simply does not protect individual ownership. The amendment's language itself is the suppressive mechanism. Theater ratio is LOW-TO-MODERATE (0.22) because the constraint's operation is not primarily performative; it articulates a doctrinal boundary that states can operationalize through legislation and regulation. The measurement series shows mild drift upward in extractiveness and theater ratio over the interval: as the reading's institutional adoption spreads and states begin to regulate on its basis, the perceived cost (extractiveness) to individual gun owners may rise, and the constraint's legitimation narrative may require more theatrical maintenance (speeches about federalism, state sovereignty, constitutional textualism) even as the core function remains stable. Suppression remains stable because the core suppressive mechanism — the textual reading itself — does not change in strength.
 *
 * PERSPECTIVAL GAP:
 *   The perspective from the state government seat and the perspective from the individual gun owner seat should compute as radically divergent types. From the state seat, this is a MOUNTAIN: it is presented as emerging from the text itself, unchangeable, a brute fact of constitutional language. From the individual gun owner seat, this is a SNARE or TANGLED_ROPE: it extracts the constitutional status of ownership, is defended by institutional power, and suppresses the owner's self-defense claim. The engine computes these divergences from the structural data: the beneficiary/victim declarations, exit options, and power atoms. The authored low extractiveness reflects the state-beneficiary reading; individual gun owners excluded from that reading will experience the constraint as more extractive from their seat. The engine's per-seat classification captures this split.
 *
 * DIRECTIONALITY LOGIC:
 *   State governments are structural beneficiaries (d near 0.0): the reading elevates them to the constitutional right-holders, gives them regulatory authority, and costs them nothing to administer. Organized militias are beneficiaries (d near 0.0): they are constitutionally protected and can be structured by state legislatures without individual constitutional objection. Individual gun owners are neither beneficiaries nor formally victims in the base_properties (they appear as excluded stakeholders instead) because their exclusion IS the reading's point — the reading simply does not grant them a constitutional status. However, if individual owners were to contest this and claim the individual-right reading, they would occupy a victim position in the collective reading's frame: they bear the cost of narrowed constitutional scope. The judiciary as agenda-setter sits near symmetric (d around 0.5): courts must interpret the amendment, but this reading constrains their interpretive freedom by textual direction. No stakeholder is highly trapped; states can operate militia systems, individuals have constrained exit (they cannot claim a constitutional right under this reading, but they can seek statutory protections), judges can revisit interpretation in future cases.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate of the Second Amendment (to protect armed strength against federal tyranny) is LIVE under this reading: states maintain militias, and the reading insulates militia authority from individual constitutional claims. The constraint's function — allocating militia authority to the state — persists as long as the reading holds in courts and academic authority. No divergence between founding problem (state militia authority) and current operation (state regulation of militia-service firearms) has emerged; the founding problem is not obsolete. However, the reading faces substantial resistance (0.72 at interval end) because competing readings dispute both the textual interpretation and the structural allocation it implies. The mandatrophy resolution here is NOT that the constraint has become zombie; rather, the reading itself is contested, and the contest is the source of resistance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prefatory_vs_operative_primacy,
    'Does the prefatory clause of the Second Amendment (''A well regulated Militia, being necessary to the security of a free State'') limit the scope of the operative clause (''the right of the people to keep and bear Arms''), or is it merely explanatory context?',
    'Originalist textual analysis and historical evidence about 18th-century grammar and legal drafting convention. If contemporary grammar practice shows that prefatory clauses commonly narrow operative scope, this reading gains textual support; if they are typically merely explanatory, the individual-right reading gains support.',
    'If the prefatory clause is limiting, the collective reading is strongly supported and ε remains low (the constraint is a natural reading of the text). If it is merely explanatory, the collective reading becomes a forced or strained reading, and other readings rise in plausibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prefatory_vs_operative_primacy, empirical, 'Grammatical and historical interpretation of prefatory-clause function in constitutional text').

omega_variable(
    founding_intent_militia_scope,
    'Did the framers intend the Second Amendment to protect individual self-defense ownership, or only militia-organized bearing of arms?',
    'Historical scholarship examining founding-era documents, state constitutions, militia laws, and judicial commentary contemporaneous with the 1791 ratification. Primary-source evidence from framers'' writings, ratification debates, and contemporaneous legal practice.',
    'If founding intent clearly protected individual ownership, the collective reading becomes a historically indefensible minority position (though still a live legal theory). If founding intent clearly protected only militia authority, the collective reading gains historical grounding. Most likely: historical sources are ambiguous or support both readings under different interpretations (the reading ambiguity itself is the omega).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_intent_militia_scope, empirical, 'Historical evidence about founding intent for the scope of the right').

omega_variable(
    judicial_path_dependence,
    'Does the institutional entrenchment of the individual-right reading in Heller and McDonald constitute a de facto overriding of the collective reading, or can the collective reading remain live as a dissenting doctrinal position?',
    'Future Supreme Court composition and jurisprudential shifts. A reversal of Heller would formally restore the collective reading to live status; continuing Heller precedent keeps it as a minority position. Alternatively, the precedent could be narrowed (recognizing individual rights but upholding stronger state regulation), which would create conceptual space for a hybrid reading.',
    'If the collective reading is treated as foreclosed by higher-court precedent, its institutional legitimacy drops and it functions as a defeated doctrinal position rather than a live option. If reversals or narrowing occur, its legitimacy and institutional adoption could rise. The reading''s practical authority depends partly on whether it is treated as merely dissenting or genuinely foreclosed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_path_dependence, empirical, 'Institutional and jurisprudential path-dependence of the collective reading''s authority').

omega_variable(
    committer_frame_ambiguity,
    'Is this constraint being authored as a reading the author endorses (a normative position), or as a reading that is institutionally live and structurally coherent regardless of endorsement?',
    'The authoring frame takes the reading as an institutionally live position held by real actors (states claiming regulation authority, scholars defending the reading, dissenting judges). The constraint is NOT authored as if this reading is true; it is authored as what the constraint is IF this reading is adopted. The engine distinguishes readings by structural data, not by truth-value or endorsement.',
    'If the reading''s plausibility or truth-value is conflated with its structural coherence and institutional instantiation, the constraint''s metrics and beneficiary/victim declarations become distorted. Authoring it cleanly (as a coherent position regardless of endorsement) preserves the ε-invariance principle: each reading gets its own ε and its own structural analysis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_ambiguity, conceptual, 'Committer-frame disambiguation: reading as live position versus reading as endorsed truth').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__collective_right_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_scope__collective_right_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(seco_tr_t8, second_amendment_scope__collective_right_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement(seco_tr_t16, second_amendment_scope__collective_right_reading, theater_ratio, 16, 0.18).
narrative_ontology:measurement(seco_tr_t24, second_amendment_scope__collective_right_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement(seco_tr_t32, second_amendment_scope__collective_right_reading, theater_ratio, 32, 0.22).
narrative_ontology:measurement(seco_tr_t40, second_amendment_scope__collective_right_reading, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_scope__collective_right_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(seco_be_t8, second_amendment_scope__collective_right_reading, base_extractiveness, 8, 0.22).
narrative_ontology:measurement(seco_be_t16, second_amendment_scope__collective_right_reading, base_extractiveness, 16, 0.25).
narrative_ontology:measurement(seco_be_t24, second_amendment_scope__collective_right_reading, base_extractiveness, 24, 0.27).
narrative_ontology:measurement(seco_be_t32, second_amendment_scope__collective_right_reading, base_extractiveness, 32, 0.28).
narrative_ontology:measurement(seco_be_t40, second_amendment_scope__collective_right_reading, base_extractiveness, 40, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_scope__collective_right_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(seco_su_t8, second_amendment_scope__collective_right_reading, suppression_requirement, 8, 0.11).
narrative_ontology:measurement(seco_su_t16, second_amendment_scope__collective_right_reading, suppression_requirement, 16, 0.13).
narrative_ontology:measurement(seco_su_t24, second_amendment_scope__collective_right_reading, suppression_requirement, 24, 0.15).
narrative_ontology:measurement(seco_su_t32, second_amendment_scope__collective_right_reading, suppression_requirement, 32, 0.15).
narrative_ontology:measurement(seco_su_t40, second_amendment_scope__collective_right_reading, suppression_requirement, 40, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_scope__collective_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_scope__collective_right_reading, 0.12).
narrative_ontology:affects_constraint(second_amendment_scope__collective_right_reading, second_amendment_scope__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__collective_right_reading, second_amendment_scope__civic_right_reading).

% DUAL FORMULATION NOTE:
% The three readings (collective, individual, civic) of the second_amendment_scope kernel are structurally distinct constraints with different beneficiaries, ε values, and institutional adoption patterns. The collective reading constrains the operative clause to militia-related bearing and vests primary authority in states; the individual reading uncouples ownership from militia service; the civic reading conditions individual rights on civic militia participation. Each reading is compiled as a separate constraint story with its own classification. Edges in the network encode influence: the collective reading constrains what the individual reading can claim (if collective is adopted, individual ownership has no constitutional protection); the civic reading attempts to bridge the two by conditioning individual ownership on militia service (influences both). All three are linked to track the kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
