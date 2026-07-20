% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__collective_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [INACTIVE]
% ============================================================================

:- module(constraint_second_amendment_arms_right__collective_right_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: second_amendment_arms_right__collective_right_reading
 *   human_readable: Second Amendment Collective Right Reading (State Militia Authority)
 *   domain: constitutional_law/political_philosophy/legal_interpretation
 *
 * SUMMARY:
 *   This constraint instantiates the collective_right_reading of the
 *   second_amendment_arms_right kernel. It holds that the Second Amendment
 *   protects only state militia authority and does not extend to individual
 *   firearm ownership outside organized militia service. This reading
 *   dominated federal jurisprudence from United States v. Miller (1939) until
 *   District of Columbia v. Heller (2008), during which state governments
 *   exercised plenary regulatory authority over private arms. The constraint
 *   is authored as a tangled rope: it coordinates genuine federalism values
 *   (state military autonomy) while asymmetrically extracting constitutional
 *   protection from individual owners. The claim/metric independence is
 *   maintainedâthe claimed type is tangled_rope while metrics reflect
 *   low-to-moderate extraction, consistent with the structural delta that
 *   this reading generates low Îµ on prohibition measures because its primary
 *   operation is regulatory empowerment rather than material rent.
 *
 * KEY AGENTS:
 *   - Federal judiciary (agenda_setter/institutional): administers the doctrinal framework through constitutional interpretation.
 *   - State governments (beneficiary/institutional): gain plenary regulatory authority over private arms outside militia context.
 *   - Organized militia entities (beneficiary/institutional): receive constitutional shelter for state military forces.
 *   - Individual firearm owners (payer/organized): bear the cost of lost constitutional protection for private possession.
 *   - Gun rights advocates (excluded/organized): advance competing readings but are ruled legally irrelevant.
 *   - Legal historians collective (observer/analytical): provide historical corroboration from outside the beneficiary set.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__collective_right_reading, 0.32).
domain_priors:suppression_score(second_amendment_arms_right__collective_right_reading, 0.58).
domain_priors:theater_ratio(second_amendment_arms_right__collective_right_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__collective_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_arms_right__collective_right_reading, "Second Amendment Collective Right Reading (State Militia Authority)").
narrative_ontology:topic_domain(second_amendment_arms_right__collective_right_reading, "constitutional_law/political_philosophy/legal_interpretation").

domain_priors:requires_active_enforcement(second_amendment_arms_right__collective_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__collective_right_reading, '6f34ebc4-e52b-4653-b436-75808429ba80').
narrative_ontology:cs_kernel_codification('6f34ebc4-e52b-4653-b436-75808429ba80', fixed_text).
narrative_ontology:cs_authority_grounding('6f34ebc4-e52b-4653-b436-75808429ba80', lineage).
narrative_ontology:cs_interpretation_layer_present('6f34ebc4-e52b-4653-b436-75808429ba80').
narrative_ontology:cs_reading_relation('6f34ebc4-e52b-4653-b436-75808429ba80', second_amendment_arms_right__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('6f34ebc4-e52b-4653-b436-75808429ba80', second_amendment_arms_right__civic_republican_reading, forecloses).
narrative_ontology:cs_axiom('6f34ebc4-e52b-4653-b436-75808429ba80', foundational, state_militia_as_exclusive_purpose).
narrative_ontology:cs_axiom_status(state_militia_as_exclusive_purpose, holdable).
narrative_ontology:cs_axiom_grounding('6f34ebc4-e52b-4653-b436-75808429ba80', state_militia_as_exclusive_purpose, empirically_contingent).
narrative_ontology:cs_axiom('6f34ebc4-e52b-4653-b436-75808429ba80', foundational, plenary_state_power_over_civilian_arms).
narrative_ontology:cs_axiom_status(plenary_state_power_over_civilian_arms, holdable).
narrative_ontology:cs_axiom_grounding('6f34ebc4-e52b-4653-b436-75808429ba80', plenary_state_power_over_civilian_arms, conventional).
narrative_ontology:cs_reference_frame('6f34ebc4-e52b-4653-b436-75808429ba80', state_militia_supremacy_framework).
narrative_ontology:cs_drift_state('6f34ebc4-e52b-4653-b436-75808429ba80', modern_era_pre_heller, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('6f34ebc4-e52b-4653-b436-75808429ba80', '').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__collective_right_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, state_governments).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, organized_militia_entities).
narrative_ontology:constraint_victim(second_amendment_arms_right__collective_right_reading, individual_firearm_owners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Second Amendment through the lens of United States v. Miller and subsequent precedent, holding that the right extends only to arms possession in connection with militia service. Maintains this doctrine through appellate review, rejecting individual-rights claims as constitutionally baseless outside organized state military service.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Hold plenary authority to regulate or prohibit individual firearm possession outside militia context, free from federal constitutional constraint. Exercise this authority differentiallyâsome states enact strict prohibitions, others permissive regimesâbut all operate within the constitutional shelter that the federal right does not reach private arms.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, state_governments, beneficiary,
    institutional, generational, mobile, national).

% State National Guard units and state defense forces operate as the constitutionally protected militia entities. Their organizational integrity and state command structure are shielded from federal interference by the amendment's doctrinal focus.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, organized_militia_entities, beneficiary,
    institutional, generational, constrained, national).

% Lack federal constitutional protection for private arms possession outside militia service. Subject to state plenary regulation including prohibition. In restrictive states, must disarm, relocate, or accept legal vulnerability; their individual-rights claims are foreclosed in federal court.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, individual_firearm_owners, payer,
    organized, biographical, constrained, national).

% Advance individual-rights and civic-republican interpretations of the Second Amendment but are structurally excluded from prevailing constitutional doctrine. Their arguments are ruled legally irrelevant in federal adjudication under the collective-right framework.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, gun_rights_advocates, excluded,
    organized, biographical, constrained, national).

% Provide historical evidence and textual analysis supporting the militia-focused reading. Their scholarship corroborates the state's authority narrative while remaining analytically distinct from the state beneficiaries.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, legal_historians_collective, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves state military autonomy against federal encroachment; enables states to maintain organized militia forces as counterweights to federal standing army power; clarifies the federalism boundary in military affairs by assigning constitutional protection to state-organized military capacity rather than private arms.
% TRANSFER_FUNCTION: Transfers constitutional protection from individual private arms possession to state governments and their organized militia entities; moves regulatory authority over non-militia arms from federal constitutional constraint to state plenary discretion, permitting prohibition at the state level.
% ABSENT_VOICES: Individual firearm owners and gun rights advocates are structurally excluded; their individual-rights claims are treated as constitutionally cognitively irrelevant within this doctrinal framework. Civic-republican scholars who see armed citizenship as neither purely individual nor state-centered are also marginalized.
% DISAPPEARANCE_RATIONALE: If the collective-right doctrine vanished overnight, federal courts would cease rejecting individual-rights claims, states would lose their constitutional shelter for prohibitory regimes, and the entire landscape of firearms regulation would shift as individual constitutional immunity became cognizable against state and federal action.
% FOUNDING_PROBLEM: How to prevent federal disarmament of state militias and preserve state military capacity as a check on federal standing army power in the early Republic.
% FOUNDING_PROBLEM_CORROBORATION: Independent military historians attest that the state militia as a counterweight to federal power became operationally obsolete with the National Guard Act and modern warfare. Individual-rights legal scholars outside the state-beneficiary camp corroborate that the founding problem is superseded, while collective-rights historians attest to its original relevance. No non-historian, non-legal neutral party provides independent corroboration of the problem's current liveness.
narrative_ontology:disappearance_verdict(second_amendment_arms_right__collective_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_arms_right__collective_right_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__collective_right_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_arms_right__collective_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_arms_right__collective_right_reading, 0.32, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__collective_right_reading_tests).
:- end_tests(second_amendment_arms_right__collective_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.32 (low-moderate) because the reading's primary operation is the transfer of regulatory authority to states rather than direct material extraction from individuals; it enables state prohibition but does not itself extract a resource. Suppression is 0.58 because individual-rights alternatives are judicially foreclosedâthe constraint persists only so long as courts actively reject individual-rights claims. Theater_ratio rises from 0.20 to 0.65 over the interval because originalist historical claims performatively maintain a doctrine whose empirical fit to modern military and political conditions decayed substantially. Accessibility_collapse is 0.55: alternatives (individual rights as constitutional baseline) are collapsed in federal courts but remain live in popular political culture and state politics. Resistance is 0.62 because gun owners, advocacy organizations, and eventually competing judicial coalitions mounted sustained resistance.
 *
 * PERSPECTIVAL GAP:
 *   State governments experience this constraint as a necessary federalism protectionâa rope that preserves their regulatory sovereignty. Individual firearm owners experience the identical doctrinal structure as a constitutional snare that removes their shield against state prohibition. The federal judiciary experiences it as an interpretive duty tied to historical lineage. These divergences are structurally determined by beneficiary/victim position and exit options, not by subjective disagreement.
 *
 * DIRECTIONALITY LOGIC:
 *   State governments and organized militia entities are structural beneficiaries (low d): the constraint subsidizes their authority and organizational integrity. Individual firearm owners are structural targets (high d): the constraint extracts constitutional protection from them, leaving them exposed to state plenary power. The federal judiciary sits near symmetric or slightly subsidized (moderate d): it administers the constraint without being its primary beneficiary or victim, though its institutional authority is reinforced by maintaining doctrinal coherence.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problemâpreserving state militia capacity against federal standing army dominanceâbecame operationally obsolete with the National Guard integration, selective service system, and modern military transformation. The doctrine persisted for decades past this obsolescence, maintained by judicial inertia and state regulatory interest rather than by live coordination need. This is classic mandatrophy: the arrangement outlived its function. The eventual resolution came not through internal sunset but through external rejection in Heller (2008), which reclassified the constitutional baseline. The T17 theater accumulation and founding-problem-status mismatch (dead problem + world_rearranges disappearance) correctly flags this as a resolved mandatrophy case.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_meaning_determinacy,
    'Does the historical record of the Second Amendment''s ratification debates and early practice determinately support state-militia exclusivity, or does it underdetermine among the three sibling readings?',
    'Comprehensive archival synthesis by independent historical linguists and historians of the Founding era, disaggregating militia-service references from private-possession references.',
    'If the record underdetermines, the collective right reading rests on judicial construction rather than recovered historical meaning, raising its Îµ and shifting its classification toward extraction-heavy types; if determinate, the reading''s coordination function is historically grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_meaning_determinacy, empirical, 'Whether historical evidence uniquely supports the collective reading.').

omega_variable(
    militia_functional_obsolescence,
    'Has the transformation of warfare and the National Guard system rendered the state militia function that the amendment was built to protect operationally obsolete?',
    'Military operational analysis comparing state militia capacity in the 18th century to National Guard integration and modern force structure.',
    'If obsolete, the coordination function is hollow and the constraint persists as extraction (state regulatory power) without genuine coordination, supporting mandatrophy or piton classification; if still live, the coordination function remains valid.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(militia_functional_obsolescence, empirical, 'Whether the founding militia function still exists.').

omega_variable(
    collective_civic_republican_foreclosure,
    'Does the collective right reading genuinely foreclose the civic republican reading, or can both be held as distinct analytical planes (state authority vs. civic virtue)?',
    'Jurisprudential analysis of whether a single constitutional framework can protect state militia authority while also treating armed citizenship as a republican good without constitutional stature.',
    'If they can coexist, the reading_relations should shift from forecloses to coexists_with or influences, altering the kernel''s contamination topology; if foreclosure is real, the kernel is a zero-sum interpretive field.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_civic_republican_foreclosure, conceptual, 'Whether collective and civic republican readings are mutually exclusive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__collective_right_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sa_collective_tr_t0, second_amendment_arms_right__collective_right_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(sa_collective_tr_t14, second_amendment_arms_right__collective_right_reading, theater_ratio, 14, 0.25).
narrative_ontology:measurement(sa_collective_tr_t28, second_amendment_arms_right__collective_right_reading, theater_ratio, 28, 0.3).
narrative_ontology:measurement(sa_collective_tr_t42, second_amendment_arms_right__collective_right_reading, theater_ratio, 42, 0.4).
narrative_ontology:measurement(sa_collective_tr_t56, second_amendment_arms_right__collective_right_reading, theater_ratio, 56, 0.5).
narrative_ontology:measurement(sa_collective_tr_t70, second_amendment_arms_right__collective_right_reading, theater_ratio, 70, 0.65).

% Extraction over time
narrative_ontology:measurement(sa_collective_be_t0, second_amendment_arms_right__collective_right_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(sa_collective_be_t14, second_amendment_arms_right__collective_right_reading, base_extractiveness, 14, 0.32).
narrative_ontology:measurement(sa_collective_be_t28, second_amendment_arms_right__collective_right_reading, base_extractiveness, 28, 0.35).
narrative_ontology:measurement(sa_collective_be_t42, second_amendment_arms_right__collective_right_reading, base_extractiveness, 42, 0.38).
narrative_ontology:measurement(sa_collective_be_t56, second_amendment_arms_right__collective_right_reading, base_extractiveness, 56, 0.35).
narrative_ontology:measurement(sa_collective_be_t70, second_amendment_arms_right__collective_right_reading, base_extractiveness, 70, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(sa_collective_su_t0, second_amendment_arms_right__collective_right_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(sa_collective_su_t14, second_amendment_arms_right__collective_right_reading, suppression_requirement, 14, 0.45).
narrative_ontology:measurement(sa_collective_su_t28, second_amendment_arms_right__collective_right_reading, suppression_requirement, 28, 0.5).
narrative_ontology:measurement(sa_collective_su_t42, second_amendment_arms_right__collective_right_reading, suppression_requirement, 42, 0.6).
narrative_ontology:measurement(sa_collective_su_t56, second_amendment_arms_right__collective_right_reading, suppression_requirement, 56, 0.55).
narrative_ontology:measurement(sa_collective_su_t70, second_amendment_arms_right__collective_right_reading, suppression_requirement, 70, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__collective_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_arms_right__collective_right_reading, individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__collective_right_reading, civic_republican_reading).

% DUAL FORMULATION NOTE:
% The second_amendment_arms_right kernel decomposes into three structurally distinct readings: collective_right_reading (state militia authority), individual_right_reading (individual liberty), and civic_republican_reading (armed citizenship). Each reading carries a distinct Îµ, beneficiary/victim structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
