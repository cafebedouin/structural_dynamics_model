% ============================================================================
% CONSTRAINT STORY: montevideo_statehood_criteria__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_montevideo_statehood_criteria__hybrid_reading, []).

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
 *   constraint_id: montevideo_statehood_criteria__hybrid_reading
 *   human_readable: Hybrid Statehood Doctrine (Objective Criteria + Normative Legitimacy)
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   The Montevideo Convention (1933) defines statehood by four objective
 *   criteria: permanent population, defined territory, government, and
 *   capacity to enter relations with other states. This story instantiates
 *   the hybrid reading of that kernel: the view that objective criteria are
 *   necessary but not sufficient, and that normative legitimacy — democratic
 *   governance, human rights compliance, non-aggression in the entity's
 *   formation — is also required before recognition, membership, and treaty
 *   capacity follow. This reading is distinct from the declaratory reading
 *   (objective criteria alone are constitutive of statehood as legal fact)
 *   and the constitutive reading (recognition by the existing community of
 *   states is what creates statehood, criteria or no criteria). The hybrid
 *   reading shares surface vocabulary with both — it uses the same four
 *   Montevideo criteria as the declaratory reading, and it depends on
 *   collective recognition practice like the constitutive reading — but it is
 *   a structurally distinct claim with its own extraction profile: it adds a
 *   governance-and-rights filter that the other two readings do not carry,
 *   and that filter is what moves non-liberal secessionists and
 *   non-democratic de facto states into the victim set that neither sibling
 *   reading generates. ε here is authored for the hybrid arrangement as
 *   actually practiced (selective application of the normative overlay by
 *   powers with intervention capacity), not for an idealized version where
 *   the overlay is applied even-handedly.
 *
 * KEY AGENTS:
 *   - established_liberal_democracies: agenda_setter/beneficiary (institutional/arbitrage) — administer and apply the normative overlay selectively
 *   - human_rights_advocacy_networks: beneficiary (organized/mobile) — gain doctrinal leverage without bearing selective-application costs
 *   - intervention_capable_powers: beneficiary (institutional/arbitrage) — gain legal cover for regime change and intervention
 *   - non_liberal_secessionist_movements: payer (powerless/trapped) — meet objective criteria but denied recognition on governance grounds
 *   - post_colonial_states_with_contested_governance: payer (moderate/constrained) — face ongoing legitimacy review layered onto settled sovereignty
 *   - de_facto_states_lacking_democratic_form: payer (powerless/trapped) — permanently locked out regardless of factual statehood
 *   - international_law_scholars: observer (analytical/analytical) — study whether the doctrine is genuine evolution or rationalized selectivity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__hybrid_reading, 0.68).
domain_priors:suppression_score(montevideo_statehood_criteria__hybrid_reading, 0.61).
domain_priors:theater_ratio(montevideo_statehood_criteria__hybrid_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(montevideo_statehood_criteria__hybrid_reading, "Hybrid Statehood Doctrine (Objective Criteria + Normative Legitimacy)").
narrative_ontology:topic_domain(montevideo_statehood_criteria__hybrid_reading, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(montevideo_statehood_criteria__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__hybrid_reading, 'b15d4b61-d2aa-49da-a020-0b40ac166620').
narrative_ontology:cs_kernel_codification('b15d4b61-d2aa-49da-a020-0b40ac166620', distributed).
narrative_ontology:cs_authority_grounding('b15d4b61-d2aa-49da-a020-0b40ac166620', distributed).
narrative_ontology:cs_reading_relation('b15d4b61-d2aa-49da-a020-0b40ac166620', montevideo_statehood_criteria__declaratory_reading, influences).
narrative_ontology:cs_reading_relation('b15d4b61-d2aa-49da-a020-0b40ac166620', montevideo_statehood_criteria__constitutive_reading, coexists_with).
narrative_ontology:cs_axiom('b15d4b61-d2aa-49da-a020-0b40ac166620', foundational, legitimacy_requires_governance_form_not_merely_effective_control).
narrative_ontology:cs_axiom_status(legitimacy_requires_governance_form_not_merely_effective_control, holdable).
narrative_ontology:cs_axiom_grounding('b15d4b61-d2aa-49da-a020-0b40ac166620', legitimacy_requires_governance_form_not_merely_effective_control, deontological).
narrative_ontology:cs_axiom('b15d4b61-d2aa-49da-a020-0b40ac166620', secondary, objective_criteria_necessary_but_not_sufficient_for_statehood).
narrative_ontology:cs_axiom_status(objective_criteria_necessary_but_not_sufficient_for_statehood, holdable).
narrative_ontology:cs_axiom_grounding('b15d4b61-d2aa-49da-a020-0b40ac166620', objective_criteria_necessary_but_not_sufficient_for_statehood, conventional).
narrative_ontology:cs_reference_frame('b15d4b61-d2aa-49da-a020-0b40ac166620', post_cold_war_liberal_internationalist_consensus).
narrative_ontology:cs_drift_state('b15d4b61-d2aa-49da-a020-0b40ac166620', contemporary_multipolar_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('b15d4b61-d2aa-49da-a020-0b40ac166620', '').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__hybrid_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__hybrid_reading, established_liberal_democracies).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__hybrid_reading, human_rights_advocacy_networks).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__hybrid_reading, intervention_capable_powers).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, non_liberal_secessionist_movements).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, post_colonial_states_with_contested_governance).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, de_facto_states_lacking_democratic_form).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__hybrid_reading, liberal_peace_theory).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__hybrid_reading, responsibility_to_protect_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sit on the UN Security Council and dominant regional bodies; they author and apply the normative overlay, deciding case by case whether a claimant entity's internal governance qualifies it for recognition. They face no comparable scrutiny of their own statehood and can invoke the doctrine selectively — recognizing secessions that produce friendly, democratic-leaning states while withholding recognition from those that do not.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, established_liberal_democracies, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(montevideo_statehood_criteria__hybrid_reading, established_liberal_democracies, beneficiary).

% Gain a legal vocabulary that ties recognition and legitimacy directly to human rights and democratic performance, which strengthens their leverage in advocacy campaigns; they do not bear the costs when the doctrine is applied selectively or weaponized against entities they did not target.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, human_rights_advocacy_networks, beneficiary,
    organized, generational, mobile, global).

% Gain legal cover for humanitarian intervention and regime-change operations by framing a target government's rights record as disqualifying it from full sovereign legitimacy, or by framing a favored secessionist group as more legitimate than the parent state. They control the military and diplomatic capacity to act on the doctrine; entities without such capacity cannot invoke it symmetrically.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, intervention_capable_powers, beneficiary,
    institutional, generational, arbitrage, global).

% Control territory, population, and an effective government satisfying the Montevideo objective criteria, but are denied recognition because their internal governance is not democratic or does not visibly commit to a liberal rights framework. They have no forum to contest the normative add-on and no path to statehood except adopting institutional forms chosen by external recognizers, which may be alien to their own political tradition or simply infeasible amid active conflict.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, non_liberal_secessionist_movements, payer,
    powerless, biographical, trapped, regional).

% Already recognized states whose continued legitimacy is now subject to ongoing normative review — coups, election irregularities, or human rights crises can trigger suspension from regional bodies or non-recognition of governments, layering a legitimacy test onto sovereignty that was previously settled by the objective criteria alone. They must continually perform democratic and rights compliance to retain full standing.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, post_colonial_states_with_contested_governance, payer,
    moderate, generational, constrained, national).

% Function as states in practice — currency, courts, borders, security forces — but their governance model (monarchic, one-party, clan-based, theocratic) forecloses recognition under the hybrid standard regardless of how completely they satisfy the objective criteria. They remain locked out of treaty capacity, international financial institutions, and diplomatic standing indefinitely.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, de_facto_states_lacking_democratic_form, payer,
    powerless, biographical, trapped, regional).

% Study and debate whether the hybrid standard is a genuine evolution of customary international law or a doctrinal rationalization for selective recognition practice by powerful states; their scholarship shapes how the doctrine is cited in future disputes without controlling its application.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, international_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(montevideo_statehood_criteria__hybrid_reading, diffuse).
narrative_ontology:fixing_cost_class(montevideo_statehood_criteria__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the international system with a shared standard for deciding contested claims to statehood in a way that also screens out entities whose internal governance threatens regional stability, protects populations from abusive rule, or destabilizes the liberal international order — coordinating recognition decisions around more than territorial control alone.
% TRANSFER_FUNCTION: Moves recognition, treaty capacity, UN membership eligibility, and access to international financial and legal institutions away from entities that meet the objective Montevideo criteria but fail the normative overlay, and toward entities whose governance form and geopolitical alignment satisfy the powers administering the standard.
% ABSENT_VOICES: Non-liberal secessionist movements and unrecognized de facto states have no seat in the bodies that apply the hybrid standard to their own cases; they can lobby individual states but cannot compel a hearing before the community whose collective judgment determines their status. Their objection — that they satisfy the traditional objective criteria and are being held to a standard invented after the fact — is rarely addressed directly in recognition debates.
% DISAPPEARANCE_RATIONALE: If the normative overlay disappeared and pure declaratory criteria governed, several currently unrecognized de facto states and non-liberal secessionist entities would have strong claims to recognition overnight, existing intervention and non-recognition policies premised on governance-based legitimacy would lose their legal cover, and recognition practice would revert to a narrower, more mechanical test — a substantial rearrangement of which entities can access international personhood.
% FOUNDING_PROBLEM: Pure objective-criteria statehood (declaratory doctrine) allowed entities formed through ethnic cleansing, coups, or naked aggression to claim full sovereign legitimacy merely by holding territory and exercising effective control, with no doctrinal tool to withhold recognition from illegitimate or abusive regimes.
% FOUNDING_PROBLEM_CORROBORATION: Human rights organizations and liberal-democratic foreign ministries attest the founding problem remains live, citing cases of ethnic-cleansing-consolidated territorial control. Non-liberal secessionist movements, several post-colonial governments, and a substantial minority of international law scholars attest the doctrine's actual operation has drifted from remedying illegitimate conquest toward a general license for powerful states to withhold or extend recognition based on alignment and governance preference, unmoored from the founding problem — this dissenting reading comes from outside the doctrine's principal beneficiaries.
narrative_ontology:disappearance_verdict(montevideo_statehood_criteria__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(montevideo_statehood_criteria__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(montevideo_statehood_criteria__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(montevideo_statehood_criteria__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(montevideo_statehood_criteria__hybrid_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(montevideo_statehood_criteria__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(montevideo_statehood_criteria__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(montevideo_statehood_criteria__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.68 and rising because the normative overlay is not applied symmetrically: powerful recognizing states are never subjected to the same governance test they impose on claimant entities, and the criterion is invoked more often against weak claimants whose recognition would be geopolitically inconvenient than withheld from strong claimants whose recognition is desired despite governance defects. Suppression (0.61) reflects that unrecognized entities have essentially no institutional forum to contest the standard's application to their case. Theater ratio (0.42) is substantial: much invocation of 'democratic legitimacy' and 'human rights compliance' in recognition debates functions as post-hoc justification for decisions already made on strategic grounds, though the underlying coordination function (screening out illegitimately-formed regimes) is not wholly fictional. accessibility_collapse is moderate (0.5) — the objective-criteria path remains visible and is sometimes still sufficient on its own in weak-observer cases, but for contested cases the normative overlay effectively closes the declaratory path.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (established democracies), the hybrid standard reads as principled progress — sovereignty should not shield atrocity or naked conquest. From the payer seats (non-liberal secessionists, non-democratic de facto states), the identical structure reads as a moving target: they satisfy the historical test for statehood and are told that satisfying it is no longer enough, with the additional bar defined and applied by the very powers whose recognition they are seeking. The engine computing different seat types from this same structural data is the point — it is not an error to be reconciled.
 *
 * DIRECTIONALITY LOGIC:
 *   Established liberal democracies and intervention-capable powers sit at the beneficiary end: they administer the overlay, invoke it selectively, and are never subject to symmetric scrutiny of their own governance as a condition of continued statehood. Human rights networks benefit from the doctrinal leverage without bearing the selective-application costs. Non-liberal secessionists and non-democratic de facto states sit at the target end — trapped, powerless, with no forum — because the overlay is precisely the mechanism denying them the recognition the objective criteria alone would grant. Post-colonial states with contested governance sit in an intermediate position: already recognized, but subject to a live and recurring legitimacy test that the declaratory reading would not impose on them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — that pure territorial control should not automatically confer sovereign legitimacy on conquest or ethnic-cleansing-consolidated regimes — remains genuinely live in some cases, which is why this is authored as tangled_rope rather than snare: there is a real coordination function (denying legitimacy to force-consolidated illegitimate rule) riding alongside the extraction (using the same overlay to deny recognition to inconvenient but non-abusive claimants, and to exempt powerful recognizers from symmetric review). Collapsing this into pure snare would erase the doctrine's genuine remedial cases; collapsing it into pure rope would erase the asymmetric application documented in the corroboration field. The mismatch between founding_problem_status=contested and disappearance_verdict=world_rearranges is exactly the signal the mandatrophy detection surface is built to catch: the doctrine still does real work in some cases while having drifted into a general discretionary tool in others.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hybrid_reading_symmetric_application_ambiguity,
    'Is the normative overlay applied as a genuinely universal legitimacy test, or selectively invoked only against claimants whose recognition is geopolitically inconvenient to the powers administering it?',
    'Comparative case analysis: code all contested recognition decisions 1990-2025 by (a) claimant''s objective-criteria satisfaction, (b) claimant''s democratic/rights profile, (c) recognizing powers'' own governance profile, (d) geopolitical alignment with recognizing powers. A consistent effect of alignment independent of governance profile would indicate selective application.',
    'If application is symmetric and governance-driven, this reading functions closer to genuine coordination (rope-leaning tangled rope); if application tracks alignment rather than governance, the normative overlay is closer to pure discretionary cover for extraction (snare-leaning).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_reading_symmetric_application_ambiguity, empirical, 'Whether the hybrid standard''s normative filter is applied symmetrically or selectively by recognizing powers.').

omega_variable(
    kernel_reading_location_of_disagreement,
    'Where exactly do the three readings of the Montevideo kernel disagree — on what counts as satisfying statehood (the criteria question) or on who has authority to certify satisfaction (the authority question)?',
    'Textual and jurisprudential analysis distinguishing cases where declaratory and hybrid readings would reach the same recognition outcome from cases where they diverge, and cross-referencing with constitutive-reading outcomes in the same cases.',
    'If the readings converge in most real cases and diverge only in a small set of contested ones, the practical stakes of the kernel dispute are narrower than the doctrinal debate suggests; if divergence is pervasive, the choice of reading is doing most of the outcome-determining work.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location_of_disagreement, conceptual, 'Locating the structural element the three sibling readings actually disagree on.').

omega_variable(
    governance_criteria_naturalness_ambiguity,
    'Is the requirement of democratic/rights-respecting governance a genuine feature of evolved customary international law (an emergent legal fact), or a doctrine constructed and sustained by the states best positioned to define and enforce ''legitimate governance'' in their own image?',
    'Genealogical tracing of state practice and opinio juris invoking governance-based non-recognition prior to and following the end of the Cold War; assess whether the pattern reflects converging customary practice among a broad range of states or promotion primarily by a specific bloc.',
    'If genuinely customary and broadly convergent, the hybrid standard has stronger claim to being an emergent, less discretionary legal norm; if promoted primarily by one bloc of powerful states, the doctrine''s normative content is closer to constructed leverage than discovered law.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(governance_criteria_naturalness_ambiguity, conceptual, 'Whether the normative overlay is emergent customary law or a constructed instrument of powerful-state discretion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__hybrid_reading, 1990, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mont_tr_t1990, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 1990, 0.25).
narrative_ontology:measurement(mont_tr_t1997, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 1997, 0.3).
narrative_ontology:measurement(mont_tr_t2004, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 2004, 0.34).
narrative_ontology:measurement(mont_tr_t2011, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 2011, 0.37).
narrative_ontology:measurement(mont_tr_t2018, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 2018, 0.4).
narrative_ontology:measurement(mont_tr_t2025, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(mont_be_t1990, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 1990, 0.42).
narrative_ontology:measurement(mont_be_t1997, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 1997, 0.48).
narrative_ontology:measurement(mont_be_t2004, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 2004, 0.55).
narrative_ontology:measurement(mont_be_t2011, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 2011, 0.61).
narrative_ontology:measurement(mont_be_t2018, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 2018, 0.65).
narrative_ontology:measurement(mont_be_t2025, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(mont_su_t1990, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 1990, 0.4).
narrative_ontology:measurement(mont_su_t1997, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 1997, 0.47).
narrative_ontology:measurement(mont_su_t2004, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 2004, 0.51).
narrative_ontology:measurement(mont_su_t2011, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 2011, 0.56).
narrative_ontology:measurement(mont_su_t2018, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 2018, 0.59).
narrative_ontology:measurement(mont_su_t2025, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 2025, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(montevideo_statehood_criteria__hybrid_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(montevideo_statehood_criteria__hybrid_reading, 0.12).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, montevideo_statehood_criteria__declaratory_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, montevideo_statehood_criteria__constitutive_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the montevideo_statehood_criteria kernel. declaratory_reading treats the four objective Montevideo criteria as dispositive of statehood as legal fact, with negligible normative filtering — its ε and victim set differ substantially from this story's, since it generates no victim class among non-liberal claimants. constitutive_reading treats collective recognition by the existing state community as the operative mechanism regardless of objective criteria, generating a different victim set (any unrecognized entity, criteria satisfied or not) and a different extraction profile centered on the recognizing community's discretion rather than a governance filter specifically. This hybrid_reading sits structurally between them: it uses declaratory's objective criteria as a floor and adds a normative overlay that functions similarly to constitutive's recognition discretion but is justified by appeal to governance and rights rather than bare political will. All three are ε-invariant standalone constraints; none averages over or references the others' metrics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
