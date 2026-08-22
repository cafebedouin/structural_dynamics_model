% ============================================================================
% CONSTRAINT STORY: commerce_clause_scope__narrow_originalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_scope__narrow_originalist, []).

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
 *   constraint_id: commerce_clause_scope__narrow_originalist
 *   human_readable: Narrow Originalist Commerce Clause: Interstate Trade Facilitation
 *   domain: constitutional/federalism
 *
 * SUMMARY:
 *   The narrow originalist reading of the Commerce Clause interprets
 *   'commerce among the several states' as trade crossing state lines and
 *   'regulate' as removing barriers and establishing uniformity for that
 *   cross-border trade. Under this reading, federal regulatory power is
 *   limited to preventing state discrimination or burdensome regulation of
 *   interstate commerce and ensuring uniform rules for genuinely interstate
 *   transactions. Federal statutes that regulate intrastate economic
 *   activity, or that use commerce power to enforce non-commercial goals
 *   (environmental protection, labor rights, civil rights) in purely local
 *   contexts, are beyond the enumerated power. This reading constrains
 *   federal authority and preserves state sovereignty over intrastate
 *   affairs. The constraint is enforced by judicial invalidation of federal
 *   statutes courts determine fall outside the narrow scope.
 *
 * KEY AGENTS:
 *   - State governments: primary beneficiaries, retain regulatory autonomy over intrastate economic activity
 *   - Federal government / Supreme Court: agenda-setter, enforces the boundary through constitutional interpretation
 *   - Interstate commerce operators: benefit from uniform federal rules for cross-border trade; also benefit from state regulatory variation on intrastate components
 *   - Marginalized groups in recalcitrant states: primary victims, lose federal civil rights and labor enforcement reach
 *   - Congress: observer with constrained options; must re-frame statutes to survive constitutional scrutiny
 *   - National regulatory uniformity (non-agent): doctrine/goal that is the structural victim of the constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_scope__narrow_originalist, 0.28).
domain_priors:suppression_score(commerce_clause_scope__narrow_originalist, 0.15).
domain_priors:theater_ratio(commerce_clause_scope__narrow_originalist, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, extractiveness, 0.28).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__narrow_originalist, rope).
narrative_ontology:human_readable(commerce_clause_scope__narrow_originalist, "Narrow Originalist Commerce Clause: Interstate Trade Facilitation").
narrative_ontology:topic_domain(commerce_clause_scope__narrow_originalist, "constitutional/federalism").

domain_priors:requires_active_enforcement(commerce_clause_scope__narrow_originalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__narrow_originalist, 'd95abe1e-063d-451d-9a5b-c8b5ca973f93').
narrative_ontology:cs_kernel_codification('d95abe1e-063d-451d-9a5b-c8b5ca973f93', fixed_text).
narrative_ontology:cs_authority_grounding('d95abe1e-063d-451d-9a5b-c8b5ca973f93', lineage).
narrative_ontology:cs_interpretation_layer_present('d95abe1e-063d-451d-9a5b-c8b5ca973f93').
narrative_ontology:cs_reading_relation('d95abe1e-063d-451d-9a5b-c8b5ca973f93', commerce_clause_scope__broad_effects_test, forecloses).
narrative_ontology:cs_reading_relation('d95abe1e-063d-451d-9a5b-c8b5ca973f93', commerce_clause_scope__intermediate_channels, forecloses).
narrative_ontology:cs_axiom('d95abe1e-063d-451d-9a5b-c8b5ca973f93', foundational, commerce_means_trade_crossing_lines).
narrative_ontology:cs_axiom_status(commerce_means_trade_crossing_lines, holdable).
narrative_ontology:cs_axiom_grounding('d95abe1e-063d-451d-9a5b-c8b5ca973f93', commerce_means_trade_crossing_lines, empirically_contingent).
narrative_ontology:cs_axiom('d95abe1e-063d-451d-9a5b-c8b5ca973f93', foundational, regulate_means_remove_barriers_not_restrict).
narrative_ontology:cs_axiom_status(regulate_means_remove_barriers_not_restrict, holdable).
narrative_ontology:cs_axiom_grounding('d95abe1e-063d-451d-9a5b-c8b5ca973f93', regulate_means_remove_barriers_not_restrict, deontological).
narrative_ontology:cs_axiom('d95abe1e-063d-451d-9a5b-c8b5ca973f93', secondary, enumerated_powers_strictly_construed).
narrative_ontology:cs_axiom_status(enumerated_powers_strictly_construed, holdable).
narrative_ontology:cs_axiom_grounding('d95abe1e-063d-451d-9a5b-c8b5ca973f93', enumerated_powers_strictly_construed, deontological).
narrative_ontology:cs_reference_frame('d95abe1e-063d-451d-9a5b-c8b5ca973f93', interstate_trade_barrier_prevention).
narrative_ontology:cs_drift_state('d95abe1e-063d-451d-9a5b-c8b5ca973f93', contemporary_environmental_civil_rights_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d95abe1e-063d-451d-9a5b-c8b5ca973f93', '').
narrative_ontology:cs_kernel_id(commerce_clause_scope__narrow_originalist, commerce_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, state_governments).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, local_businesses).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, decentralized_experimentation_advocates).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, national_regulatory_uniformity).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, federal_civil_rights_enforcement).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, marginalized_groups_in_recalcitrant_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, interstate_commerce_operators).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, interstate_commerce_operators).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, congress).
narrative_ontology:constraint_vindicates(commerce_clause_scope__narrow_originalist, federalism_structural_limit).
narrative_ontology:constraint_vindicates(commerce_clause_scope__narrow_originalist, enumerated_powers_doctrine).
narrative_ontology:constraint_vindicates(commerce_clause_scope__narrow_originalist, original_public_meaning_textualism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain broad autonomy to regulate economic activity that occurs wholly within state borders, including labor conditions, environmental practices, consumer protection, and health standards. Under this reading, states can experiment with divergent regulatory regimes on non-interstate matters without federal preemption. They collect regulatory authority and preserve political flexibility.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, state_governments, beneficiary,
    institutional, generational, analytical, national).

% Face only uniform federal rules for genuinely interstate transactions; intrastate operations remain subject to state and local regulation alone. Small and regional firms benefit from lower compliance burden if federal rules do not reach their markets. They avoid the cost of conforming to multiple federal regulatory regimes.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, local_businesses, beneficiary,
    moderate, biographical, constrained, regional).

% Enforces the boundary between interstate and intrastate commerce by striking down federal statutes that regulate activity the reading categorizes as purely local (environmental rules applied to intrastate pollution, labor law applied to intrastate employment, civil rights law applied to local accommodations). The constraint operates through constitutional interpretation; the federal judiciary is the enforcement machinery.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, federal_government, agenda_setter,
    institutional, generational, analytical, national).

% A doctrine/policy goal that cannot be achieved if the reading holds. Uniform national labor standards, environmental protection, and consumer safety across all states cannot be enforced via the Commerce Clause if the clause is confined to facilitating interstate trade. This is not an agent; it appears here as the structural victim of the constraint's operation.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, national_regulatory_uniformity, payer,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(commerce_clause_scope__narrow_originalist, national_regulatory_uniformity).

% The enforcement apparatus for national civil rights standards becomes constitutionally unreliable under this reading. Public accommodations law, voting rights enforcement, and anti-discrimination statutes lose their Commerce Clause foundation to the extent they regulate activity the reading classes as intrastate. This is a structural victim rather than an actor.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, federal_civil_rights_enforcement, payer,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(commerce_clause_scope__narrow_originalist, federal_civil_rights_enforcement).

% Bear the costs of state-level discrimination and protective-labor denial if the federal civil rights and labor enforcement apparatus loses Commerce Clause constitutional footing. Their exit options are severely constrained by identity and geography; leaving the state is not a practical alternative, and within-state political remedies are structurally blocked if the recalcitrant state majority rejects federal norms.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, marginalized_groups_in_recalcitrant_states, payer,
    powerless, biographical, identity_locked, regional).

% Benefit from uniform federal rules for genuinely interstate transactions (shipping, telecommunications, financial services crossing state lines), which reduces compliance cost. They also benefit from state-level regulatory variation on intrastate components, which allows them to minimize total cost by locating activities in favorable regulatory jurisdictions. They bear indirect costs if state fragmentation creates arbitrary barriers to interstate commerce (which the reading's enforcement function is designed to prevent).
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, interstate_commerce_operators, beneficiary,
    powerful, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_scope__narrow_originalist, interstate_commerce_operators, payer).

% Support the reading as a principled protection for regulatory federalism and interstate policy competition (the 'laboratories of democracy' framing). They benefit ideologically and institutionally from legal frameworks that preserve state autonomy. They articulate the constraint as coordinating beneficial competition in governance rather than as extracting from federal enforcement capacity.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, decentralized_experimentation_advocates, beneficiary,
    organized, biographical, mobile, national).

% Administers the reading through constitutional interpretation doctrine. The Court draws the line between interstate commerce (regulable under the narrow reading) and intrastate economic activity (beyond federal reach). The constraint persists because the Court authoritatively declares what the Commerce Clause means; the interpretation is the constraint's enforcement machinery.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, supreme_court, agenda_setter,
    institutional, generational, analytical, national).

% Occupies an ambiguous position: it legislates within the constraint's boundaries, but the constraint is enforced by courts striking down legislation Congress passes. Congress observes the constraint; the Court administers it. Congress bears the cost of legislative creativity needed to re-frame statutes to stay within the narrow Commerce Clause (e.g., attaching a jurisdictional element to civil rights law to connect it to interstate commerce channels).
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, congress, observer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_scope__narrow_originalist, congress, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_scope__narrow_originalist, state_governments).
narrative_ontology:fixing_cost_class(commerce_clause_scope__narrow_originalist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a uniform legal framework for transactions that cross state borders, removing state-imposed barriers to interstate trade and ensuring predictable rules for commerce-crossing-lines. Solves the coordination problem of states imposing discriminatory or burdensome regulations on goods, services, and persons moving across state boundaries.
% TRANSFER_FUNCTION: Transfers regulatory authority from federal government to state governments for all economic activity occurring wholly within state borders, including labor, environment, consumer protection, civil rights, and health. The constraint moves autonomy from the federal center to the states and removes federal enforcement power over intrastate arrangements.
% ABSENT_VOICES: Federal administrative agencies (EPA, OSHA, EEOC, etc.) that would argue for expansive federal reach to achieve national uniformity on environmental, labor, and civil rights standards are absent from the framing courts use when applying the narrow reading. National advocacy organizations representing marginalized groups and uniform-standard constituencies lack formal standing in the constitutional adjudication that enforces this constraint.
% DISAPPEARANCE_RATIONALE: If this constraint—understood as a judicial interpretation limiting Commerce Clause reach—were overturned and replaced with a broad-effects reading, federal regulatory authority would expand dramatically: labor standards, environmental rules, and civil rights protections would be enforceable nationwide via the Commerce Clause rather than confined to their narrower constitutional foundations. States would lose regulatory autonomy over intrastate activity; the federal system would shift toward centralization. Congress's legislative options would expand; state governments' regulatory space would contract.
% FOUNDING_PROBLEM: The Articles of Confederation failed because states imposed tariffs and protectionist barriers on each other's goods, fragmenting the national market and weakening commercial incentives to unite. The Commerce Clause was drafted to remove state-imposed barriers to interstate trade and ensure that commerce crossing state lines operates under uniform federal rules, not state-by-state fragmentation.
% FOUNDING_PROBLEM_CORROBORATION: Historians and originalist scholars (Randy Barnett, Keith Whittington) attest the founding problem was interstate trade barriers and state economic warfare. Broad-effects scholars and civil rights advocates attest the founding problem is solved but the constraint persists as a barrier to federal enforcement of national standards. Economic historians confirm the tariff war problem was real; they also note that intrastate labor and environmental externalities were not contemplated at the founding and represent new problems the founders did not address. No external, disinterested source fully corroborates one reading over the other; the disagreement is constitutive of the kernel contest.
narrative_ontology:disappearance_verdict(commerce_clause_scope__narrow_originalist, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_scope__narrow_originalist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_scope__narrow_originalist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(commerce_clause_scope__narrow_originalist, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_scope__narrow_originalist, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_scope__narrow_originalist_tests).
:- end_tests(commerce_clause_scope__narrow_originalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.28) because the constraint operates by limiting federal power rather than by coercively transferring resources from one party to another in the ordinary sense. The beneficiaries (state governments, local businesses) gain autonomy and regulatory flexibility; the victims (national uniformity, federal civil rights enforcement, marginalized groups) lose enforcement reach and coordination power. Suppression is very low (0.15) because the constraint is not enforced through coercive suppression of alternatives—it is enforced through judicial interpretation of constitutional text. Theater ratio is moderate (0.22) and rising slightly over the interval: the constraint's enforcement requires repeated judicial doctrinal articulation and re-affirmation; as congressional pressure builds for federal environmental and civil rights enforcement, the Court must increasingly perform doctrinal work (via jurisdictional elements, aggregation limitations) to maintain the constraint's boundaries. The measurement series track a slow increase in both extractiveness and theater over the 100-year interval, reflecting gradual accumulation of unmet regulatory demands (environmental externalities, labor exploitation, civil rights denial) that the narrow reading prevents federal government from addressing, and the Court's increasing doctrinal elaboration to defend the boundary.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between institutional seats operating at different levels of federalism: state governments and the federal government perceive the constraint fundamentally differently. From the state seat, federalism is a structural protection and coordination benefit. From the federal seat, it is a barrier to uniform protection. Neither perception is false; they reflect structural positions in a federal system. The Court, as the agenda-setter, enforces one reading; but the enforceability of judicial interpretation depends on political support from state governments and public acceptance. If federal enforcement pressure from Congress and civil rights advocates reaches critical mass, the constraint's theater ratio would spike (doctrinal labor increases) and the constraint might tip toward mandatrophy (the founding problem—state trade barriers—is solved; the constraint persists for reasons other than solving the founding problem).
 *
 * DIRECTIONALITY LOGIC:
 *   State governments occupy the beneficiary end of the directionality spectrum (d near 0.0) because the constraint allocates regulatory authority to them and removes federal preemption. Federal civil rights enforcement and national uniformity occupy the victim end (d near 1.0) because the constraint removes their reach. Marginalized groups in recalcitrant states occupy the strongest victim position (d = 1.0): they are identity_locked (cannot exit the state as a practical matter), face direct harm from state discrimination without federal remedy, and have no political power within the state to change the situation. Interstate commerce operators sit in a dual position: they benefit from uniform federal rules for the transactions they engage in (low d on the interstate component) but also benefit from state regulatory variation on intrastate components (d near 0.5 on the full system). The directionality derivation should reflect this structure: beneficiaries get lower effective extraction, victims get higher.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—state-imposed tariffs and trade barriers fragmenting the national market—was substantially solved by the late 19th century. Modern state taxation and regulation is, for the most part, facially neutral and non-discriminatory on interstate commerce. Yet the narrow originalist constraint persists and is applied to strike down federal statutes regulating intrastate activity (environmental regulation, labor law, civil rights enforcement) that have nothing to do with preventing state trade discrimination. This is mandatrophy: the constraint's original function has atrophied. The narrow originalist reading justifies continued enforcement by appeal to text and original meaning (the formalist defense), not by appeal to the founding coordination problem. The rising theater ratio reflects this: the Court must increasingly deploy doctrinal techniques (jurisdictional elements, aggregation limits, instrumentalities tests) to maintain the boundary, adding performative work to the enforcement of a function that no longer solves the founding problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_public_meaning_determination,
    'What was the original public meaning of ''commerce'' and ''regulate'' in 1787–1789? Was it limited to trade crossing state lines, or did it include intrastate economic activity with interstate effects?',
    'Historical linguistic analysis, founding-era dictionaries, Framers'' correspondence, ratification debates—but also recognition that ''original public meaning'' is itself contested (Framers'' intent vs. ratifiers'' understanding vs. ordinary English usage). No single archive resolves the question definitively; historians and originalists genuinely disagree.',
    'If the narrow reading is what the text originally meant, the constraint is a defense of fidelity to the Constitution. If the text''s original meaning was broader (including intrastate activity with interstate effects), the narrow reading is a modern doctrinal restriction unsupported by the original meaning it claims to serve, and the constraint should be reclassified as extractive (a judicial power grab defending state autonomy against the Constitution''s actual grant).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(original_public_meaning_determination, empirical, 'Original meaning of ''commerce'' and ''regulate'' in 1787').

omega_variable(
    federalism_coordination_vs_extraction,
    'Is the preservation of state regulatory autonomy a genuine coordination benefit (solving the problem of federal overreach and regulatory homogenization) or a constraint that extracts from national uniformity and federal civil rights enforcement capacity to benefit state governments?',
    'No empirical resolution: this is a preference question about federalism values. Some polities value experimentation and diversity; others value uniformity and national standards. The disagreement is structural, not empirical.',
    'If federalism is a coordination benefit, the constraint is a rope and the victims are accepting costs for genuine gains. If federalism is extraction from national capacity, the constraint should be classified as snare and the beneficiaries are structural victims (they benefit from the constraint, but the constraint persists because federal enforcement is blocked, not because states chose the arrangement). This is a conceptual question about what federalism IS.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federalism_coordination_vs_extraction, preference, 'Whether federalism is coordination or extraction of federal enforcement capacity').

omega_variable(
    state_recalcitrance_feedback,
    'When a state uses its regulatory autonomy to deny civil rights or labor protections to marginalized groups within its borders, and the narrow Commerce Clause interpretation prevents federal remedy, what is the structural relationship between the constraint and the harm? Does the constraint cause the harm, permit it, or neither?',
    'Causal analysis: trace the harm (e.g., wage discrimination, racial exclusion) to its immediate institutional causes (state law, state enforcement) and then to the constraint (which removes federal override). The causal chain is real, but its force is contestable: one seat argues the constraint is necessary to preserve federalism; another argues the constraint actively protects discrimination by blocking federal remedy.',
    'If the constraint actively protects discrimination (by design or foreseeable consequence), the classification should emphasize suppression and extraction from the marginalized group. If the constraint merely fails to prevent discrimination (by leaving it to state discretion), the classification emphasizes under-enforcement rather than active harm. The measurement of suppression (currently low at 0.15) might be understated if the constraint''s function is understood as actively protecting state-level suppression from federal override.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_recalcitrance_feedback, conceptual, 'Whether the constraint actively protects state discrimination or merely declines to override it').

omega_variable(
    one_reading_of_contested_kernel,
    'This constraint is one reading of the commerce_clause_scope kernel. Are the three readings (narrow_originalist, broad_effects_test, intermediate_channels) truly coexisting in contemporary constitutional practice, or is one reading in the process of foreclosing the others?',
    'Monitoring the Supreme Court''s actual decisions, scholarly consensus, and lower-court application over the next 10–20 years. If one reading produces consistent majorities and the others fade from judicial practice, one reading has foreclosed the alternatives. If all three remain live in different decisions and doctrinal contexts, they genuinely coexist.',
    'If this narrow reading is foreclosing the broad_effects reading (as has occurred episodically in recent decades), the constraint''s classification as rope becomes harder to sustain—it is more like a doctrinal victory in an ongoing contest, less like a principled coordination device. If all three remain genuinely live (competing judges, competing schools of thought), the coexistence claim holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(one_reading_of_contested_kernel, empirical, 'Whether the three commerce readings genuinely coexist or one is foreclosing the others').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_scope__narrow_originalist, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, commerce_clause_scope__narrow_originalist, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(comm_tr_t0, observed).
narrative_ontology:measurement(comm_tr_t16, commerce_clause_scope__narrow_originalist, theater_ratio, 16, 0.11).
narrative_ontology:measurement_basis(comm_tr_t16, observed).
narrative_ontology:measurement(comm_tr_t33, commerce_clause_scope__narrow_originalist, theater_ratio, 33, 0.15).
narrative_ontology:measurement_basis(comm_tr_t33, observed).
narrative_ontology:measurement(comm_tr_t50, commerce_clause_scope__narrow_originalist, theater_ratio, 50, 0.18).
narrative_ontology:measurement_basis(comm_tr_t50, observed).
narrative_ontology:measurement(comm_tr_t67, commerce_clause_scope__narrow_originalist, theater_ratio, 67, 0.2).
narrative_ontology:measurement_basis(comm_tr_t67, observed).
narrative_ontology:measurement(comm_tr_t100, commerce_clause_scope__narrow_originalist, theater_ratio, 100, 0.22).
narrative_ontology:measurement_basis(comm_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, commerce_clause_scope__narrow_originalist, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(comm_be_t0, observed).
narrative_ontology:measurement(comm_be_t16, commerce_clause_scope__narrow_originalist, base_extractiveness, 16, 0.18).
narrative_ontology:measurement_basis(comm_be_t16, observed).
narrative_ontology:measurement(comm_be_t33, commerce_clause_scope__narrow_originalist, base_extractiveness, 33, 0.22).
narrative_ontology:measurement_basis(comm_be_t33, observed).
narrative_ontology:measurement(comm_be_t50, commerce_clause_scope__narrow_originalist, base_extractiveness, 50, 0.26).
narrative_ontology:measurement_basis(comm_be_t50, observed).
narrative_ontology:measurement(comm_be_t67, commerce_clause_scope__narrow_originalist, base_extractiveness, 67, 0.27).
narrative_ontology:measurement_basis(comm_be_t67, observed).
narrative_ontology:measurement(comm_be_t100, commerce_clause_scope__narrow_originalist, base_extractiveness, 100, 0.28).
narrative_ontology:measurement_basis(comm_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, commerce_clause_scope__narrow_originalist, suppression_requirement, 0, 0.1).
narrative_ontology:measurement_basis(comm_su_t0, observed).
narrative_ontology:measurement(comm_su_t16, commerce_clause_scope__narrow_originalist, suppression_requirement, 16, 0.11).
narrative_ontology:measurement_basis(comm_su_t16, observed).
narrative_ontology:measurement(comm_su_t33, commerce_clause_scope__narrow_originalist, suppression_requirement, 33, 0.13).
narrative_ontology:measurement_basis(comm_su_t33, observed).
narrative_ontology:measurement(comm_su_t50, commerce_clause_scope__narrow_originalist, suppression_requirement, 50, 0.14).
narrative_ontology:measurement_basis(comm_su_t50, observed).
narrative_ontology:measurement(comm_su_t67, commerce_clause_scope__narrow_originalist, suppression_requirement, 67, 0.15).
narrative_ontology:measurement_basis(comm_su_t67, observed).
narrative_ontology:measurement(comm_su_t100, commerce_clause_scope__narrow_originalist, suppression_requirement, 100, 0.15).
narrative_ontology:measurement_basis(comm_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_scope__narrow_originalist, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(commerce_clause_scope__narrow_originalist, 0.08).
narrative_ontology:affects_constraint(commerce_clause_scope__narrow_originalist, commerce_clause_scope__broad_effects_test).
narrative_ontology:affects_constraint(commerce_clause_scope__narrow_originalist, commerce_clause_scope__intermediate_channels).
narrative_ontology:affects_constraint(commerce_clause_scope__narrow_originalist, federal_environmental_authority).
narrative_ontology:affects_constraint(commerce_clause_scope__narrow_originalist, federal_labor_law_authority).
narrative_ontology:affects_constraint(commerce_clause_scope__narrow_originalist, federal_civil_rights_enforcement).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the commerce_clause_scope kernel (narrow_originalist). The kernel is contested across three live readings. Each reading produces a distinct constraint story with its own ε, beneficiary/victim structure, and classification. The three stories are linked as constraint family members via network.affects_constraints. See commentary.kernel_context for the reading relations and foundational axioms that distinguish this reading from its siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(commerce_clause_scope__narrow_originalist, powerless, 1.0).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
