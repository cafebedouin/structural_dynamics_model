% ============================================================================
% CONSTRAINT STORY: second_amendment_scope__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_scope__individual_right_reading, []).

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
 *   constraint_id: second_amendment_scope__individual_right_reading
 *   human_readable: Second Amendment Individual Right Reading (Unconnected to Militia Service)
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   This constraint instantiates the individual_right_reading of the
 *   contested Second Amendment kernel: the claim that the Amendment protects
 *   an individual right to possess and carry firearms unconnected to militia
 *   service. The reading was canonized in District of Columbia v. Heller
 *   (2008) and expanded in New York State Rifle & Pistol Association v. Bruen
 *   (2022). It constrains state and local regulatory authority while
 *   empowering individual gun owners and the firearms industry. The
 *   classification as tangled_rope reflects that the constraint coordinates a
 *   genuine liberty interest (individual self-defense) while asymmetrically
 *   extracting regulatory capacity from democratic institutions and imposing
 *   diffuse public safety costs on concentrated communities.
 *
 * KEY AGENTS:
 *   - individual_persons: Broad beneficiary class (organized/biographical) â holds the constitutional right
 *   - firearms_industry: Concentrated beneficiary (powerful/generational) â profits from protected market
 *   - state_legislatures: Primary payer (institutional/generational) â loses regulatory capacity
 *   - gun_violence_affected_communities: Secondary payer (powerless/biographical) â bears localized violence costs
 *   - supreme_court: Agenda setter (institutional/civilizational) â administers interpretive boundaries
 *   - gun_control_advocacy_groups: Excluded voice (organized/generational) â structurally foreclosed from policy wins
 *   - constitutional_scholars: Analytical observer (analytical/civilizational) â provides interpretive analysis without direct stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__individual_right_reading, 0.74).
domain_priors:suppression_score(second_amendment_scope__individual_right_reading, 0.82).
domain_priors:theater_ratio(second_amendment_scope__individual_right_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_scope__individual_right_reading, "Second Amendment Individual Right Reading (Unconnected to Militia Service)").
narrative_ontology:topic_domain(second_amendment_scope__individual_right_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(second_amendment_scope__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__individual_right_reading, '11c8023c-516c-425e-b83e-5f2a0138ee0d').
narrative_ontology:cs_kernel_codification('11c8023c-516c-425e-b83e-5f2a0138ee0d', fixed_text).
narrative_ontology:cs_authority_grounding('11c8023c-516c-425e-b83e-5f2a0138ee0d', lineage).
narrative_ontology:cs_interpretation_layer_present('11c8023c-516c-425e-b83e-5f2a0138ee0d').
narrative_ontology:cs_reading_relation('11c8023c-516c-425e-b83e-5f2a0138ee0d', second_amendment_scope__collective_right_reading, forecloses).
narrative_ontology:cs_reading_relation('11c8023c-516c-425e-b83e-5f2a0138ee0d', second_amendment_scope__civic_right_reading, forecloses).
narrative_ontology:cs_axiom('11c8023c-516c-425e-b83e-5f2a0138ee0d', foundational, unconnected_individual_right_to_arms).
narrative_ontology:cs_axiom_status(unconnected_individual_right_to_arms, holdable).
narrative_ontology:cs_axiom_grounding('11c8023c-516c-425e-b83e-5f2a0138ee0d', unconnected_individual_right_to_arms, deontological).
narrative_ontology:cs_axiom('11c8023c-516c-425e-b83e-5f2a0138ee0d', foundational, prefatory_clause_non_limiting).
narrative_ontology:cs_axiom_status(prefatory_clause_non_limiting, holdable).
narrative_ontology:cs_axiom_grounding('11c8023c-516c-425e-b83e-5f2a0138ee0d', prefatory_clause_non_limiting, conventional).
narrative_ontology:cs_reference_frame('11c8023c-516c-425e-b83e-5f2a0138ee0d', original_public_meaning_1791).
narrative_ontology:cs_drift_state('11c8023c-516c-425e-b83e-5f2a0138ee0d', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('11c8023c-516c-425e-b83e-5f2a0138ee0d', '').
narrative_ontology:cs_kernel_id(second_amendment_scope__individual_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, individual_persons).
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, firearms_industry).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, state_legislatures).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, gun_violence_affected_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold a constitutionally protected right to possess and carry firearms for self-defense, independent of militia service. The right limits what state and federal legislatures can prohibit, though its boundaries are set by ongoing litigation. Exit from this constitutional framework is difficult because it is embedded in the federal structure.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, individual_persons, beneficiary,
    organized, biographical, constrained, national).

% Benefits from constitutional protection of its core product category, which limits regulatory bans and sustains a national market. Invests in litigation and lobbying to maintain the individual right reading and expand its scope.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, firearms_industry, beneficiary,
    powerful, generational, arbitrage, national).

% Possess traditional police powers over public health and safety but are constitutionally barred from prohibiting handgun possession in the home and from imposing certain licensing regimes. Their regulatory space is actively narrowed by federal judicial review.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, state_legislatures, payer,
    institutional, generational, constrained, national).

% Bear disproportionate costs of firearm violence in jurisdictions where state and local regulatory tools have been judicially invalidated. Unable to exit the constitutional framework that limits local gun regulation or the neighborhoods where violence concentrates.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, gun_violence_affected_communities, payer,
    powerless, biographical, constrained, local).

% Sets the interpretive boundaries of the Second Amendment through constitutional adjudication. Determines which regulations survive and which are struck down. Can revise the reading but faces institutional pressure for consistency and legitimacy.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, supreme_court, agenda_setter,
    institutional, civilizational, analytical, national).

% Advance regulatory frameworks and collective-right interpretations that are foreclosed by the individual right reading. Their policy preferences are structurally excluded from legislative enactment by constitutional judicial review.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, gun_control_advocacy_groups, excluded,
    organized, generational, constrained, national).

% Provide historical, textual, and normative analysis of the amendment's meaning. Observe the doctrinal evolution without directly benefiting from or paying the constraint's costs.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves individual capacity for armed self-defense against criminal threats and distributes the means of resistance away from a state monopoly on force.
% TRANSFER_FUNCTION: Transfers authority to regulate private firearm possession from state and local legislatures to individual possessors and to federal courts for boundary enforcement.
% ABSENT_VOICES: Collective-right interpreters who view the amendment as militia-only; public health officials who would treat firearms as subject to standard consumer-product regulation; international human rights bodies favoring strict state gun control.
% DISAPPEARANCE_RATIONALE: If the individual right reading disappeared overnight, state and local governments would regain broad authority to prohibit handgun possession and impose discretionary licensing; the firearms industry would face contracting markets; and federal courts would lose a major vehicle for reviewing democratic gun policy.
% FOUNDING_PROBLEM: Founding-era fear of standing armies and disarmed populaces; ensuring the people retained arms for militia service and tyranny resistance.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by civic-republican historians and Anti-Federalist writings from outside the contemporary gun-owner beneficiary set; contested by historians who argue the individual-right framing is a twentieth-century ideological construction.
narrative_ontology:disappearance_verdict(second_amendment_scope__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_scope__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__individual_right_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_scope__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_scope__individual_right_reading, 0.74, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_scope__individual_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_scope__individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_scope__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is mod-high (0.74) because the constraint removes broad policy tools from state governments and imposes uncompensated safety externalities on specific communities. Suppression is higher (0.82) because the reading's persistence requires active judicial nullification of democratically enacted laws; alternatives are structurally excluded by constitutional review. Theater is moderate (0.40) and rising: the historical analysis in Heller and Bruen performs methodological rigor while functionally serving to enforce a policy boundary. Accessibility collapse (0.65) captures that while the collective-right alternative persists in academic discourse, it has largely collapsed as a live legal argument in federal courts. Resistance (0.55) reflects ongoing legislative circumvention, lower-court friction, and political contestation.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seat (individual gun owner or firearms industry), the constraint is a shield against state overreach â a genuine liberty that coordinates self-protection. From the payer seat (state legislator or violence-affected community), it is an externally imposed limit on democratic problem-solving that extracts regulatory capacity and imposes uncompensated safety costs. The Court seat experiences the constraint as a power-conferring interpretive tool; the excluded advocacy seat experiences it as a wall against policy change. The engine computes this divergence from the structural asymmetry in cost-bearing and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual persons and the firearms industry are declared beneficiaries (low d, near the subsidy end). State legislatures and affected communities are declared victims/payers (high d, near the full-target end). The Court is not declared in either base property array; its directionality will derive from its institutional position as agenda setter with analytical exit. The extraction is amplified for the powerless, local-scope communities and damped for the powerful, arbitrage-capable industry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â fear of disarmed populaces and standing armies â is contested as to whether it maps onto modern conditions. The constraint persists and has expanded (Bruen strengthening the scrutiny regime) beyond any plausible militia function. Classification as tangled_rope prevents mislabeling it as pure coordination (rope) by acknowledging the asymmetric extraction from state capacity and affected communities, while also preventing mislabeling it as pure extraction (snare) by preserving the genuine liberty-coordination function for individual self-defense.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_location,
    'This reading instantiates the individual_right_reading of kernel second_amendment_scope. A sibling collective_right_reading would remove individual persons from the beneficiary set and eliminate judicial enforcement against state legislatures; a civic_right_reading would condition the right on militia participation. Where is the disagreement structurally located: in the semantic relationship between the amendment''s prefatory and operative clauses, or in the normative priority of individual versus collective security?',
    'Historical-linguistic analysis of 18th-century usage of ''bear arms'' and ''well-regulated militia''; cross-referencing with state ratifying convention records.',
    'If the prefatory clause is grammatically non-limiting, the individual right reading holds and state regulatory capacity remains constrained; if limiting, the collective or civic reading prevails and regulatory space expands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Location of structural disagreement between kernel readings').

omega_variable(
    extraction_bearing_group,
    'Is the extraction from this constraint borne by state governments as lost regulatory capacity, or by urban communities as elevated violence exposure?',
    'Empirical comparison of gun violence rates and regulatory nullification effects across jurisdictions with different pre-Heller regulatory regimes.',
    'If borne by communities, the victim set is concentrated and the constraint leans snare-like; if borne by governments as legitimate constitutional limitation, the extraction is diffuse institutional friction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_bearing_group, empirical, 'Whether extraction falls on communities or state capacity').

omega_variable(
    enforcement_judicial_vs_textual,
    'Does this constraint require active judicial enforcement because the text compels it, or because the reading itself is contested and would collapse without ongoing interpretive maintenance?',
    'Track lower-court compliance rates and legislative bypass attempts; observe whether the constraint persists absent Supreme Court review.',
    'If enforcement demand is high because the reading is textually unstable, theater_ratio understates the performative component; if textually compelled, the constraint approaches mountain-like persistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_judicial_vs_textual, conceptual, 'Whether enforcement reflects textual stability or interpretive theater').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__individual_right_reading, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_scope__individual_right_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(seco_tr_t4, second_amendment_scope__individual_right_reading, theater_ratio, 4, 0.28).
narrative_ontology:measurement(seco_tr_t8, second_amendment_scope__individual_right_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(seco_tr_t12, second_amendment_scope__individual_right_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(seco_tr_t16, second_amendment_scope__individual_right_reading, theater_ratio, 16, 0.4).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_scope__individual_right_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(seco_be_t4, second_amendment_scope__individual_right_reading, base_extractiveness, 4, 0.62).
narrative_ontology:measurement(seco_be_t8, second_amendment_scope__individual_right_reading, base_extractiveness, 8, 0.66).
narrative_ontology:measurement(seco_be_t12, second_amendment_scope__individual_right_reading, base_extractiveness, 12, 0.7).
narrative_ontology:measurement(seco_be_t16, second_amendment_scope__individual_right_reading, base_extractiveness, 16, 0.74).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_scope__individual_right_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(seco_su_t4, second_amendment_scope__individual_right_reading, suppression_requirement, 4, 0.62).
narrative_ontology:measurement(seco_su_t8, second_amendment_scope__individual_right_reading, suppression_requirement, 8, 0.68).
narrative_ontology:measurement(seco_su_t12, second_amendment_scope__individual_right_reading, suppression_requirement, 12, 0.75).
narrative_ontology:measurement(seco_su_t16, second_amendment_scope__individual_right_reading, suppression_requirement, 16, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(second_amendment_scope__individual_right_reading, second_amendment_scope__collective_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__individual_right_reading, second_amendment_scope__civic_right_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the second_amendment_scope kernel. The natural-language label 'Second Amendment' conflates three structurally distinct claims: individual right unconnected to militia service (this story, high extraction from regulatory capacity), collective right of state militia authority (separate constraint, different beneficiary/victim structure), and civic right conditioned on militia participation (separate constraint). Each has a distinct epsilon and stakeholder geometry.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
