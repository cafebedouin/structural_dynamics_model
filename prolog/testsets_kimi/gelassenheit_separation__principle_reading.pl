% ============================================================================
% CONSTRAINT STORY: gelassenheit_separation__principle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gelassenheit_separation__principle_reading, []).

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
 *   constraint_id: gelassenheit_separation__principle_reading
 *   human_readable: Gelassenheit Principle Reading: Structural Entanglement Separation
 *   domain: religious/technology_governance/commitment_systems
 *
 * SUMMARY:
 *   The Amish and Mennonite Ordnung includes a principle of separation from
 *   worldly systems (Gelassenheit). This constraint instantiates the
 *   principle_reading of the contested gelassenheit_separation kernel:
 *   technology is evaluated based on whether it creates structural
 *   entanglement with worldly institutions, not merely surface resemblance to
 *   English society. Off-grid solar and pneumatic tools are permitted because
 *   they function in isolation; internet and commercial insurance are
 *   forbidden because they structurally tie individuals to external systems
 *   regardless of apparent isolation. This is one reading of a three-way
 *   contested kernel.
 *
 * KEY AGENTS:
 *   - ordnung_leadership (institutional/regional): Agenda-setter â interprets structural entanglement and enforces the principle through church discipline.
 *   - plain_community (organized/regional): Beneficiary â identity-fused members who benefit from preserved communal boundaries and mutual aid.
 *   - modernizing_members (moderate/regional): Payer/victim â members who bear the cost of denied internet and insurance despite offering functionally isolated implementations.
 *   - external_service_providers (institutional/global): Excluded â structurally barred from serving the community by the entanglement principle.
 *   - religious_studies_scholars (analytical/global): Observer â documents the competition between readings and their empirical effects.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gelassenheit_separation__principle_reading, 0.48).
domain_priors:suppression_score(gelassenheit_separation__principle_reading, 0.55).
domain_priors:theater_ratio(gelassenheit_separation__principle_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gelassenheit_separation__principle_reading, tangled_rope).
narrative_ontology:human_readable(gelassenheit_separation__principle_reading, "Gelassenheit Principle Reading: Structural Entanglement Separation").
narrative_ontology:topic_domain(gelassenheit_separation__principle_reading, "religious/technology_governance/commitment_systems").

domain_priors:requires_active_enforcement(gelassenheit_separation__principle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gelassenheit_separation__principle_reading, '0c4d712a-0a47-4388-8d6e-f86efda0eae6').
narrative_ontology:cs_kernel_codification('0c4d712a-0a47-4388-8d6e-f86efda0eae6', formalized).
narrative_ontology:cs_authority_grounding('0c4d712a-0a47-4388-8d6e-f86efda0eae6', lineage).
narrative_ontology:cs_interpretation_layer_present('0c4d712a-0a47-4388-8d6e-f86efda0eae6').
narrative_ontology:cs_reading_relation('0c4d712a-0a47-4388-8d6e-f86efda0eae6', gelassenheit_separation__artifact_reading, coexists_with).
narrative_ontology:cs_reading_relation('0c4d712a-0a47-4388-8d6e-f86efda0eae6', gelassenheit_separation__consequence_reading, coexists_with).
narrative_ontology:cs_axiom('0c4d712a-0a47-4388-8d6e-f86efda0eae6', foundational, structural_entanglement_over_appearance).
narrative_ontology:cs_axiom_status(structural_entanglement_over_appearance, holdable).
narrative_ontology:cs_axiom_grounding('0c4d712a-0a47-4388-8d6e-f86efda0eae6', structural_entanglement_over_appearance, theological).
narrative_ontology:cs_reference_frame('0c4d712a-0a47-4388-8d6e-f86efda0eae6', yielded_separation_from_worldly_systems).
narrative_ontology:cs_drift_state('0c4d712a-0a47-4388-8d6e-f86efda0eae6', digital_infrastructure_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0c4d712a-0a47-4388-8d6e-f86efda0eae6', '').
narrative_ontology:cs_kernel_id(gelassenheit_separation__principle_reading, gelassenheit_separation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gelassenheit_separation__principle_reading, plain_community).
narrative_ontology:constraint_beneficiary(gelassenheit_separation__principle_reading, ordnung_leadership).
narrative_ontology:constraint_victim(gelassenheit_separation__principle_reading, modernizing_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Ordnung to distinguish structural entanglement from permissible functional isolation. Decides that solar and pneumatic tools are acceptable off-grid but internet and insurance are forbidden regardless of isolation. Enforces through church discipline, ministerial counsel, and the threat of shunning. Authority depends on maintaining theological distinctiveness and the community's separation from worldly systems.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, ordnung_leadership, agenda_setter,
    institutional, generational, constrained, regional).

% Benefits from preserved communal boundaries, mutual aid obligations, and protection from cultural assimilation. Members understand the principle as protecting Gelassenheit and community autonomy. Their identity is fused with the ordnung; leaving would mean loss of family, language, and spiritual meaning.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, plain_community, beneficiary,
    organized, generational, identity_locked, regional).

% Amish and Mennonite members who seek internet access or commercial insurance for business or family security, even proposing functionally isolated implementations. They are denied because the principle forbids structural entanglement regardless of isolation. They bear costs of limited connectivity, financial risk exposure, and restricted information access. Exit would mean shunning and identity dissolution.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, modernizing_members, payer,
    moderate, biographical, identity_locked, regional).

% Insurance companies, internet service providers, and government program administrators are structurally excluded from serving this population. The community's principled rejection of entanglement bars them from a market segment, regardless of what isolated or off-grid products they could offer.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, external_service_providers, excluded,
    institutional, biographical, constrained, global).

% Academic observers who document the theological and sociological distinctions between artifact-based, consequence-based, and principle-based readings of Gelassenheit separation. They trace how different districts adopt different criteria and how the principle reading functions as a commitment-system adaptation.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, religious_studies_scholars, observer,
    analytical, generational, analytical, global).

narrative_ontology:fixing_cost_class(gelassenheit_separation__principle_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves communal boundaries and spiritual autonomy by distinguishing technologies that create structural dependency on worldly institutions from those that can operate in functional isolation, solving the coordination problem of uniform community standards across districts.
% TRANSFER_FUNCTION: Denies individual members access to internet connectivity and commercial insurance regardless of functional isolation, while permitting off-grid pneumatic and solar tools; transfers risk-bearing and information-gathering back to communal mutual aid and face-to-face networks.
% ABSENT_VOICES: Young adults during rumspringa who would choose functional isolation over total prohibition; commercial providers who would offer isolated infrastructure; and theological conservatives who favor artifact-based evaluation over structural reasoning.
% DISAPPEARANCE_RATIONALE: If the principle reading vanished overnight, the community's technology policy would reorganize around artifact-based or consequence-based readings, altering which technologies are permissible, how businesses operate, and how risk is managedâshifting the community's relationship to modern infrastructure.
% FOUNDING_PROBLEM: How to maintain Gelassenheit (yieldedness) and separation from worldly systems in the face of technologies that appear neutral or isolated but may create hidden structural dependencies.
% FOUNDING_PROBLEM_CORROBORATION: Anabaptist historians and ethnographers outside the benefiting parties attest to the historical separation imperative; the specific structural-entanglement interpretation is contested by theological conservatives favoring artifact-based readings and by scholars documenting consequence-based evaluations in other districts.
narrative_ontology:disappearance_verdict(gelassenheit_separation__principle_reading, world_rearranges).
narrative_ontology:founding_problem_status(gelassenheit_separation__principle_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gelassenheit_separation__principle_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gelassenheit_separation__principle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gelassenheit_separation__principle_reading, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gelassenheit_separation__principle_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gelassenheit_separation__principle_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gelassenheit_separation__principle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48) because the principle genuinely permits some technologies and serves a real coordination function, but it forcibly denies access to internet and insurance even when functionally isolated. Suppression (0.55) reflects church discipline, shunning, and social enforcement rather than state coercion. Theater ratio (0.30) is moderate-low because the structural-entanglement reasoning is substantive theology, though some boundary maintenance may be performative. Accessibility collapse (0.60) captures how alternatives become socially inaccessible through identity fusion; resistance (0.45) reflects ongoing tension, rumspringa departures, and district-level disputes.
 *
 * PERSPECTIVAL GAP:
 *   The ordnung_leadership and plain_community experience this constraint as identity-preserving boundary maintenance with low effective extraction. Modernizing_members experience it as denied access to essential modern infrastructure despite offering functionally isolated implementations, yielding high effective extraction. The engine computes this divergence from beneficiary/victim declarations and the identity_locked exit options of both community and modernizing members.
 *
 * DIRECTIONALITY LOGIC:
 *   plain_community and ordnung_leadership are declared beneficiaries with constrained or identity_locked exit, placing them near the beneficiary end (low d). modernizing_members are declared victims with identity_locked exit, placing them near the full-target end (high d). external_service_providers are structurally excluded. The directionality derivation correctly maps the asymmetric extraction: the same constraint subsidizes communal identity while extracting from members who need modern protective infrastructure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâmaintaining spiritual separation from worldly systemsâis still live, so this is not piton-stage mandatrophy. However, the specific principle reading competes with artifact and consequence readings, suggesting the coordination mechanism is under adaptive pressure rather than obsolescence. The classification as tangled_rope prevents misreading the genuine coordination function as pure extraction, while acknowledging the asymmetric cost borne by modernizing_members.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    principle_reading_kernel_position,
    'Is the principle reading (structural entanglement) the authentic theological core of Gelassenheit separation, or a modern rationalization evolved to manage technological pressure?',
    'Historical-theological analysis of Ordnung evolution across districts, comparing adoption timelines of principle-based vs. artifact-based rulings against technological availability curves.',
    'If evolved under pressure, the principle reading is a scaffold-like adaptation managing contingent technological change; if authentically foundational, it is a stable commitment-system reading with deeper lineage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(principle_reading_kernel_position, conceptual, 'Whether the principle reading represents authentic tradition or adaptive rationalization.').

omega_variable(
    functional_isolation_boundary,
    'Can functional isolation from worldly systems be maintained in practice, or does the use of any modern technology inevitably create hidden structural dependencies (supply chains, maintenance networks, legal frameworks)?',
    'Ethnographic tracing of supply chains and support networks for off-grid solar and pneumatic systems in Amish communities, documenting hidden dependencies on worldly manufacturing, financing, and expertise.',
    'If hidden dependencies exist, the principle reading''s extraction is higher than claimed because permitted technologies still entangle; if genuine isolation is possible, the coordination function is stronger and the victim profile narrower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_isolation_boundary, empirical, 'Whether permitted technologies actually achieve functional isolation.').

omega_variable(
    enforcement_mechanism_ambiguity,
    'Is the constraint''s persistence driven by external church discipline (shunning, ministerial rulings) or by internalized identity fusion that makes members self-enforce?',
    'Comparative analysis of defection rates and member reasoning in communities with stronger vs. weaker formal discipline, including post-exit suppression trajectories.',
    'If primarily internalized, effective suppression is higher than structural measures suggest and the constraint travels with members after physical exit; if primarily external, the constraint is more vulnerable to leadership change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gelassenheit_separation__principle_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gelassenheit_principle_tr_t0, gelassenheit_separation__principle_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(gelassenheit_principle_tr_t10, gelassenheit_separation__principle_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(gelassenheit_principle_tr_t20, gelassenheit_separation__principle_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(gelassenheit_principle_tr_t30, gelassenheit_separation__principle_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(gelassenheit_principle_tr_t40, gelassenheit_separation__principle_reading, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(gelassenheit_principle_be_t0, gelassenheit_separation__principle_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(gelassenheit_principle_be_t10, gelassenheit_separation__principle_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(gelassenheit_principle_be_t20, gelassenheit_separation__principle_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(gelassenheit_principle_be_t30, gelassenheit_separation__principle_reading, base_extractiveness, 30, 0.45).
narrative_ontology:measurement(gelassenheit_principle_be_t40, gelassenheit_separation__principle_reading, base_extractiveness, 40, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(gelassenheit_principle_su_t0, gelassenheit_separation__principle_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(gelassenheit_principle_su_t10, gelassenheit_separation__principle_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(gelassenheit_principle_su_t20, gelassenheit_separation__principle_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(gelassenheit_principle_su_t30, gelassenheit_separation__principle_reading, suppression_requirement, 30, 0.54).
narrative_ontology:measurement(gelassenheit_principle_su_t40, gelassenheit_separation__principle_reading, suppression_requirement, 40, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gelassenheit_separation__principle_reading, identity_coordination).
narrative_ontology:affects_constraint(gelassenheit_separation__principle_reading, gelassenheit_separation__artifact_reading).
narrative_ontology:affects_constraint(gelassenheit_separation__principle_reading, gelassenheit_separation__consequence_reading).

% DUAL FORMULATION NOTE:
% The gelassenheit_separation kernel decomposes into three structurally distinct readings (artifact, consequence, principle) with different epsilon profiles, beneficiary structures, and victim sets. Each reading is a separate constraint linked by family membership.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
