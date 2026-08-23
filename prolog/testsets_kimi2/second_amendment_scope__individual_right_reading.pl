% ============================================================================
% CONSTRAINT STORY: second_amendment_scope__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: second_amendment_scope__individual_right_reading
 *   human_readable: Second Amendment Individual Right Reading (Unconnected to Militia Service)
 *   domain: constitutional_law
 *
 * SUMMARY:
 *   This constraint story instantiates the individual_right_reading of the
 *   second_amendment_scope kernel: the Second Amendment protects an
 *   individual right to keep and bear arms unconnected to militia service. It
 *   is one of three structurally distinct readings of the same constitutional
 *   text; the other two (collective_right_reading and civic_right_reading)
 *   are sibling constraints in the same family. Under this reading, the
 *   beneficiary set is universal (all individuals), state regulatory
 *   authority is heavily constrained by judicial review, and the constraint
 *   exhibits high base extractiveness due to broad coverage and strict
 *   scrutiny of gun regulation. The claim is tangled_rope because a genuine
 *   coordination function (rights-protection against government overreach)
 *   operates alongside asymmetric extraction (public safety costs borne by
 *   trapped communities, regulatory capacity confiscated from legislatures).
 *
 * KEY AGENTS:
 *   - individual_right_bearers: Universal beneficiary class (moderate/mobile) â gains constitutional immunity from prohibitory regulation.
 *   - state_legislatures: Primary institutional payer (institutional/constrained) â loses traditional police power over firearms.
 *   - gun_violence_affected_communities: Secondary popular payer (powerless/trapped) â bears diffuse safety costs without exit.
 *   - federal_judiciary: Agenda setter (institutional/analytical) â interprets and enforces the constraint.
 *   - firearms_industry: Concentrated beneficiary (powerful/arbitrage) â captures expanded market from regulatory floor.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__individual_right_reading, 0.78).
domain_priors:suppression_score(second_amendment_scope__individual_right_reading, 0.85).
domain_priors:theater_ratio(second_amendment_scope__individual_right_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_scope__individual_right_reading, "Second Amendment Individual Right Reading (Unconnected to Militia Service)").
narrative_ontology:topic_domain(second_amendment_scope__individual_right_reading, "constitutional_law").

domain_priors:requires_active_enforcement(second_amendment_scope__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__individual_right_reading, '0fa62be9-e394-46e4-a7dd-6449893de685').
narrative_ontology:cs_kernel_codification('0fa62be9-e394-46e4-a7dd-6449893de685', fixed_text).
narrative_ontology:cs_authority_grounding('0fa62be9-e394-46e4-a7dd-6449893de685', lineage).
narrative_ontology:cs_interpretation_layer_present('0fa62be9-e394-46e4-a7dd-6449893de685').
narrative_ontology:cs_reading_relation('0fa62be9-e394-46e4-a7dd-6449893de685', second_amendment_scope__collective_right_reading, forecloses).
narrative_ontology:cs_reading_relation('0fa62be9-e394-46e4-a7dd-6449893de685', second_amendment_scope__civic_right_reading, forecloses).
narrative_ontology:cs_axiom('0fa62be9-e394-46e4-a7dd-6449893de685', foundational, right_bears_individual_scope_unconnected_to_militia_service).
narrative_ontology:cs_axiom_status(right_bears_individual_scope_unconnected_to_militia_service, holdable).
narrative_ontology:cs_axiom_grounding('0fa62be9-e394-46e4-a7dd-6449893de685', right_bears_individual_scope_unconnected_to_militia_service, conventional).
narrative_ontology:cs_reference_frame('0fa62be9-e394-46e4-a7dd-6449893de685', original_public_meaning_framework).
narrative_ontology:cs_drift_state('0fa62be9-e394-46e4-a7dd-6449893de685', contemporary_post_heller, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0fa62be9-e394-46e4-a7dd-6449893de685', '').
narrative_ontology:cs_kernel_id(second_amendment_scope__individual_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, individual_right_bearers).
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, firearms_industry).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, state_legislatures).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, gun_violence_affected_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% All individuals within the jurisdiction who may possess and carry firearms under the constitutional umbrella; they gain a judicially enforceable immunity from certain legislative restrictions and may move between regulatory environments to optimize exercise of the right.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, individual_right_bearers, beneficiary,
    moderate, generational, mobile, national).

% Manufacturers, distributors, and retailers who benefit from expanded legal markets and reduced regulatory variance; they can relocate operations to favorable jurisdictions but depend on the constitutional floor preventing prohibitory regimes.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, firearms_industry, beneficiary,
    powerful, biographical, arbitrage, national).

% State and local legislative bodies whose traditional police powers over public safety are preempted by constitutional litigation; they must draft regulations within judicially narrowed corridors and bear the cost of defending struck-down laws.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, state_legislatures, payer,
    institutional, biographical, constrained, national).

% Urban neighborhoods and populations disproportionately exposed to firearm homicide and injury; they cannot exit the jurisdiction or the constitutional framework and bear the diffuse safety costs of broad ownership rights without commensurate regulatory offset.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, gun_violence_affected_communities, payer,
    powerless, immediate, trapped, local).

% Federal courts that elaborate the scope of the right, select historical analogues for regulatory validation, and actively enforce the constraint by striking down legislation; their interpretive methodology determines where the extraction-coordination boundary falls.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

narrative_ontology:fixing_cost_class(second_amendment_scope__individual_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Protects individual possession and use of firearms from government prohibition or confiscation, establishing a judicially enforceable limit on state power and providing a decentralized check on government overreach.
% TRANSFER_FUNCTION: Transfers regulatory authority away from state and local legislatures toward individual firearm owners, while transferring risk and violence exposure toward communities unable to opt out of firearm-saturated environments.
% ABSENT_VOICES: Public health researchers, comparative constitutional scholars, and gun violence survivors are structurally excluded from originalist interpretive frameworks that privilege founding-era text and history over contemporary empirical outcomes; their exclusion is built into the litigation architecture that elaborates the right.
% DISAPPEARANCE_RATIONALE: If the individual right reading disappeared overnight, legislative majorities in high-regulation jurisdictions would immediately enact and enforce magazine limits, assault weapon bans, and discretionary permitting regimes previously struck down; the firearms market would contract and the judicial enforcement architecture would collapse.
% FOUNDING_PROBLEM: Founding generation concern that standing armies and strong central governments could tyrannize the populace; armed citizenry conceived as check on state power and guarantor of other liberties.
% FOUNDING_PROBLEM_CORROBORATION: Originalist historians and some constitutional scholars attest the founding problem as live. Public health researchers, comparative constitutional scholars, and criminologists attest the problem is dead or transformed; the mismatch between 18th-century anti-tyranny rationale and 21st-century gun violence patterns is corroborated by empirical security studies and international comparative law from outside the beneficiary set.
narrative_ontology:disappearance_verdict(second_amendment_scope__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_scope__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__individual_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_scope__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_scope__individual_right_reading, 0.78, 'kimi-k2.6', 'none', direct).

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
 *   Base extractiveness (0.78) is high because the reading constrains a vast domain of state regulation and imposes strict historical-analogy review (Bruen), effectively extracting legislative capacity from states and safety from exposed communities. Suppression (0.85) is higher than extractiveness because the constraint's persistence depends on active judicial enforcement striking down alternatives, not on voluntary compliance. Theater ratio (0.32) is moderate: originalist methodology provides real doctrinal structure, but performative appeals to founding-era history sometimes substitute for empirical accountability. Accessibility collapse (0.75) is substantial but not total because some regulatory alternatives (permitting, red-flag laws) remain contested rather than fully foreclosed. Resistance (0.70) reflects sustained legislative, academic, and grassroots opposition.
 *
 * PERSPECTIVAL GAP:
 *   The individual_right_bearers seat perceives the constraint as protective coordination (a rope), while the state_legislatures and gun_violence_affected_communities seats perceive it as active extraction (a snare). The engine computes this divergence from the same structural data; the authored tangled_rope claim reflects that both perceptions are structurally rooted rather than merely perspectival.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/payer boundary maps cleanly to the structural positions: individual_right_bearers and firearms_industry sit at low directionality (subsidized by the constraint), while state_legislatures and gun_violence_affected_communities sit at high directionality (targets of extraction). The federal_judiciary is agenda_setter rather than beneficiary; its directionality is analytically mediated. The engine will compute high effective extraction for the payer seats due to their institutional or identity-locked exit options and large spatial scope, and low or negative extraction for the beneficiary seats.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the R5 genealogy fields, this constraint could be misclassified as a rope (if only the rights-protective story is heard) or a snare (if only the public-safety cost is heard). The founding_problem_status=contested flag prevents both errors: it records that the anti-tyranny rationale is disputed, which keeps the coordination function from being taken for granted while also acknowledging that the function is not merely cover.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'This constraint is the individual_right_reading of kernel second_amendment_scope. How would the beneficiary-victim structure change if the collective_right_reading or civic_right_reading were substituted?',
    'Comparative analysis of the three compiled constraint stories measuring beneficiary cardinality, victim cardinality, and directionality variance across the kernel family.',
    'Would reveal whether the kernel is a genuine ambiguity or a site of structurally incompatible commitments.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Structural position of this reading within the contested kernel.').

omega_variable(
    founding_problem_empirical_salience,
    'Is the founding-era problem of standing-army tyranny still live enough to justify the constraint''s 21st-century extraction profile?',
    'Cross-national comparative constitutional analysis and empirical security-studies assessment of whether armed populaces correlate with liberal democratic stability or with state fragility.',
    'If the founding problem is dead, the constraint''s coordination function is mandatrophic and the residual extraction is unmoored; if live, the extraction is the price of the coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_empirical_salience, empirical, 'Whether the constraint''s founding rationale remains empirically active.').

omega_variable(
    regulatory_extraction_measurement,
    'Does the constraint extract primarily from state regulatory capacity, from public safety, or from both in equal measure?',
    'Jurisdictional panel study comparing regulatory output in high-restriction states before and after Heller and Bruen, cross-referenced with public health mortality data.',
    'Would clarify whether the victim seat is best modeled as institutional (state_legislatures) or popular (gun_violence_affected_communities), refining directionality derivation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_extraction_measurement, empirical, 'Which payer seat bears the primary extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__individual_right_reading, 0, 233).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sa_individual_tr_t0, second_amendment_scope__individual_right_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sa_individual_tr_t50, second_amendment_scope__individual_right_reading, theater_ratio, 50, 0.12).
narrative_ontology:measurement(sa_individual_tr_t100, second_amendment_scope__individual_right_reading, theater_ratio, 100, 0.15).
narrative_ontology:measurement(sa_individual_tr_t150, second_amendment_scope__individual_right_reading, theater_ratio, 150, 0.2).
narrative_ontology:measurement(sa_individual_tr_t200, second_amendment_scope__individual_right_reading, theater_ratio, 200, 0.35).
narrative_ontology:measurement(sa_individual_tr_t217, second_amendment_scope__individual_right_reading, theater_ratio, 217, 0.3).
narrative_ontology:measurement(sa_individual_tr_t233, second_amendment_scope__individual_right_reading, theater_ratio, 233, 0.32).

% Extraction over time
narrative_ontology:measurement(sa_individual_be_t0, second_amendment_scope__individual_right_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(sa_individual_be_t50, second_amendment_scope__individual_right_reading, base_extractiveness, 50, 0.18).
narrative_ontology:measurement(sa_individual_be_t100, second_amendment_scope__individual_right_reading, base_extractiveness, 100, 0.22).
narrative_ontology:measurement(sa_individual_be_t150, second_amendment_scope__individual_right_reading, base_extractiveness, 150, 0.25).
narrative_ontology:measurement(sa_individual_be_t200, second_amendment_scope__individual_right_reading, base_extractiveness, 200, 0.45).
narrative_ontology:measurement(sa_individual_be_t217, second_amendment_scope__individual_right_reading, base_extractiveness, 217, 0.7).
narrative_ontology:measurement(sa_individual_be_t233, second_amendment_scope__individual_right_reading, base_extractiveness, 233, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(sa_individual_su_t0, second_amendment_scope__individual_right_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(sa_individual_su_t50, second_amendment_scope__individual_right_reading, suppression_requirement, 50, 0.15).
narrative_ontology:measurement(sa_individual_su_t100, second_amendment_scope__individual_right_reading, suppression_requirement, 100, 0.2).
narrative_ontology:measurement(sa_individual_su_t150, second_amendment_scope__individual_right_reading, suppression_requirement, 150, 0.35).
narrative_ontology:measurement(sa_individual_su_t200, second_amendment_scope__individual_right_reading, suppression_requirement, 200, 0.6).
narrative_ontology:measurement(sa_individual_su_t217, second_amendment_scope__individual_right_reading, suppression_requirement, 217, 0.8).
narrative_ontology:measurement(sa_individual_su_t233, second_amendment_scope__individual_right_reading, suppression_requirement, 233, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(second_amendment_scope__individual_right_reading, second_amendment_scope__collective_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__individual_right_reading, second_amendment_scope__civic_right_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'Second Amendment' conflates three structurally distinct constraints. This story isolates the individual-right-unconnected-to-militia reading; its siblings isolate the collective-right and civic-right readings. All three share the same fixed textual kernel but assign the right to different beneficiaries (individuals universally, state militias, or militia-conditioned individuals) and produce different extraction profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
