% ============================================================================
% CONSTRAINT STORY: refugee_convention_text__expansive_humanitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-21
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_refugee_convention_text__expansive_humanitarian_reading, []).

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
 *   constraint_id: refugee_convention_text__expansive_humanitarian_reading
 *   human_readable: Expansive Humanitarian Reading of the Refugee Convention
 *   domain: international_law/migration_governance/human_rights
 *
 * SUMMARY:
 *   The 1951 Refugee Convention and 1967 Protocol, read through an expansive
 *   humanitarian lens, constitute an unbendable international legal mandate
 *   requiring broad protection for asylum seekers. This reading interprets
 *   'well-founded fear' to encompass generalized violence and non-state
 *   persecution, and 'particular social group' to include gender, LGBTQ+
 *   status, and clan-based persecution. It treats interdiction and offshore
 *   processing as refoulement violations and demands substantive assessment
 *   of all claims. States experience this as a significant constraint on
 *   sovereign border control, while asylum seekers benefit from expanded
 *   recognition categories. The constraint is actively contested by
 *   restrictionist states and maintained through international legal
 *   monitoring, NGO litigation, and UNHCR interpretive guidance.
 *
 * KEY AGENTS:
 *   - Asylum seekers: Primary beneficiary (powerless/trapped) â receive expanded protection and non-refoulement guarantees
 *   - Sovereign states: Primary payer (institutional/constrained) â bear sovereignty extraction, fiscal costs, and procedural obligations
 *   - UNHCR: Agenda setter (institutional/constrained) â administers the regime and pushes expansive interpretation
 *   - International courts: Agenda setter (institutional/analytical) â adjudicate broad interpretations but lack direct enforcement
 *   - Restrictionist advocates: Excluded voice (organized/constrained) â oppose expansive reading domestically but are marginalized in humanitarian discourse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__expansive_humanitarian_reading, 0.76).
domain_priors:suppression_score(refugee_convention_text__expansive_humanitarian_reading, 0.68).
domain_priors:theater_ratio(refugee_convention_text__expansive_humanitarian_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, extractiveness, 0.76).
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__expansive_humanitarian_reading, tangled_rope).
narrative_ontology:human_readable(refugee_convention_text__expansive_humanitarian_reading, "Expansive Humanitarian Reading of the Refugee Convention").
narrative_ontology:topic_domain(refugee_convention_text__expansive_humanitarian_reading, "international_law/migration_governance/human_rights").

domain_priors:requires_active_enforcement(refugee_convention_text__expansive_humanitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__expansive_humanitarian_reading, '4bcccb8d-bf75-4ca3-8281-7663ba4bc6c6').
narrative_ontology:cs_kernel_codification('4bcccb8d-bf75-4ca3-8281-7663ba4bc6c6', fixed_text).
narrative_ontology:cs_authority_grounding('4bcccb8d-bf75-4ca3-8281-7663ba4bc6c6', lineage).
narrative_ontology:cs_interpretation_layer_present('4bcccb8d-bf75-4ca3-8281-7663ba4bc6c6').
narrative_ontology:cs_reading_relation('4bcccb8d-bf75-4ca3-8281-7663ba4bc6c6', refugee_convention_text__restrictive_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('4bcccb8d-bf75-4ca3-8281-7663ba4bc6c6', refugee_convention_text__procedural_integrity_reading, coexists_with).
narrative_ontology:cs_axiom('4bcccb8d-bf75-4ca3-8281-7663ba4bc6c6', foundational, convention_as_unbendable_humanitarian_mandate).
narrative_ontology:cs_axiom_status(convention_as_unbendable_humanitarian_mandate, holdable).
narrative_ontology:cs_axiom_grounding('4bcccb8d-bf75-4ca3-8281-7663ba4bc6c6', convention_as_unbendable_humanitarian_mandate, deontological).
narrative_ontology:cs_axiom('4bcccb8d-bf75-4ca3-8281-7663ba4bc6c6', foundational, particular_social_group_expansive_interpretation).
narrative_ontology:cs_axiom_status(particular_social_group_expansive_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('4bcccb8d-bf75-4ca3-8281-7663ba4bc6c6', particular_social_group_expansive_interpretation, conventional).
narrative_ontology:cs_reference_frame('4bcccb8d-bf75-4ca3-8281-7663ba4bc6c6', universal_humanitarian_protection_mandate).
narrative_ontology:cs_drift_state('4bcccb8d-bf75-4ca3-8281-7663ba4bc6c6', contemporary_restrictionist_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('4bcccb8d-bf75-4ca3-8281-7663ba4bc6c6', '').
narrative_ontology:cs_kernel_id(refugee_convention_text__expansive_humanitarian_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, asylum_seekers).
narrative_ontology:constraint_victim(refugee_convention_text__expansive_humanitarian_reading, sovereign_states).
narrative_ontology:constraint_vindicates(refugee_convention_text__expansive_humanitarian_reading, non_refoulement_peremptory_status).
narrative_ontology:constraint_vindicates(refugee_convention_text__expansive_humanitarian_reading, gender_based_persecution_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Flee persecution and seek recognition under the Convention. Under this reading, they benefit from broad inclusion of gender, sexuality, clan affiliation, and non-state violence as grounds for protection. Their exit options are severely limited by closed borders, lack of legal status in transit countries, and the impossibility of returning to countries of persecution.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, asylum_seekers, beneficiary,
    powerless, biographical, trapped, global).

% Bear the legal obligation to admit, process, and not return applicants. The expansive reading constrains their border control, externalization policies, offshore processing, and discretion to define persecution narrowly. They face international litigation, UNHCR monitoring, and diplomatic pressure if they resist the broad mandate.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, sovereign_states, payer,
    institutional, generational, constrained, national).

% Administers the international protection regime, issues authoritative guidance interpreting the Convention expansively, and monitors state compliance. Constrained by its treaty mandate and dependence on donor state funding, it functions as the primary interpreter pushing for broad protection categories and against interdiction.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, unhcr, agenda_setter,
    institutional, generational, constrained, global).

% Render binding or advisory opinions on state obligations under refugee and human rights law. They expand the reading of well-founded fear and particular social group through case law, but lack direct enforcement mechanisms to compel recalcitrant states to comply with expansive obligations.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, international_courts, agenda_setter,
    institutional, generational, analytical, global).

% Advocate for narrow interpretation, offshore processing, reduced humanitarian intake, and broad state discretion. They are excluded from the legitimating discourse of the expansive humanitarian framework and treated as sovereignty concerns subordinate to protection mandates, yet exercise significant domestic political influence.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, restrictionist_advocates, excluded,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates international burden-sharing and protection standards for people fleeing persecution, establishing a common legal framework so that protection does not depend entirely on the generosity or discretion of individual states.
% TRANSFER_FUNCTION: Moves obligation, fiscal cost, and sovereignty from states to the international protection system, converting state resources into legal recognition and non-return guarantees for asylum seekers.
% ABSENT_VOICES: Restrictionist political movements and frontline host populations bear significant practical burdens but are largely excluded from the expansive reading's legitimating discourse; their objections to burden distribution are treated as sovereignty concerns subordinate to humanitarian mandate.
% DISAPPEARANCE_RATIONALE: If the expansive Convention reading disappeared overnight, states would abandon broad non-refoulement obligations, offshore processing and interdiction would proliferate without legal check, gender- and sexuality-based persecution would lose recognized status, and the global protection architecture would collapse into ad hoc state discretion.
% FOUNDING_PROBLEM: Post-WWII displacement crisis and the failure of interwar arrangements, where millions fled persecution but lacked guaranteed international legal status or protection from forcible return to danger.
% FOUNDING_PROBLEM_CORROBORATION: UNHCR and independent human rights organizations attest to ongoing persecution and displacement from Syria, Afghanistan, Venezuela, and LGBTQ+ communities globally. Restrictionist states contest the scale and character of the problem, arguing economic migration dominates. Independent academic and journalistic documentation from outside the benefiting parties corroborates ongoing persecution meeting the expansive criteria.
narrative_ontology:disappearance_verdict(refugee_convention_text__expansive_humanitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(refugee_convention_text__expansive_humanitarian_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(refugee_convention_text__expansive_humanitarian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(refugee_convention_text__expansive_humanitarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(refugee_convention_text__expansive_humanitarian_reading, 0.76, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(refugee_convention_text__expansive_humanitarian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(refugee_convention_text__expansive_humanitarian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(refugee_convention_text__expansive_humanitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.76) because the expansive reading imposes broad non-refoulement, inclusive persecution categories, and substantive assessment duties that significantly constrain state sovereignty and fiscal autonomy. Suppression is substantial (0.68) because the constraint's persistence depends on actively closing off alternatives such as pushbacks, offshore processing, and narrow definitions. Theater ratio rises to 0.40 as states develop increasingly performative compliance mechanisms that mimic process while subverting protection substance. The measurement series run on one shared time grid so every metric is authored at every examined time point, showing a steady accumulation of extraction and enforcement requirement as state resistance hardens.
 *
 * PERSPECTIVAL GAP:
 *   The asylum seeker seat experiences the constraint as vital protection and legal recognition; the sovereign state seat experiences it as an imposed erosion of border sovereignty and fiscal burden. The agenda-setter seats (UNHCR, courts) experience it as a mandate to enforce against non-compliant states. The engine computes these divergences from the same structural facts â the claim and metrics are independently authored and not reconciled to each other.
 *
 * DIRECTIONALITY LOGIC:
 *   Asylum seekers are structural beneficiaries (low d, subsidized by the constraint's recognition function). Sovereign states are structural payers (high d, extraction of sovereignty and resources). UNHCR and international courts sit near symmetric: their institutional purpose is constituted by the constraint, but they do not personally collect its transfers and are mandate-bound rather than mobile.
 *
 * MANDATROPHY ANALYSIS:
 *   The expansive reading prevents mandatrophy by continuously updating interpretation to address emerging persecution types (gender-based claims, non-state violence, LGBTQ+ status), keeping the founding problem live and the coordination function current. However, rising state resistance risks converting the constraint toward theater if enforcement capacity cannot match aspirational breadth.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'This constraint is the expansive_humanitarian_reading of the refugee_convention_text kernel; what would structurally change if the restrictive_sovereignty_reading were adopted instead?',
    'Comparative jurisdictional analysis measuring recognition rates, excluded category definitions, and interdiction legality across states adopting restrictive versus expansive interpretive frameworks.',
    'Adopting the restrictive reading would shrink the beneficiary set to state-persecuted individuals with immutable characteristics, recast interdiction as permissible sovereign prerogative, and substantially reduce effective extraction from states.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Structural delta between expansive and restrictive readings of the same kernel').

omega_variable(
    enforcement_sovereignty_gap,
    'Does the expansive reading''s extractiveness exceed the international community''s enforcement capacity, rendering parts of the constraint theatrical rather than operational?',
    'Empirical measurement of compliance rates with non-refoulement and procedural standards versus documented pushback and externalization practices across major destination states.',
    'If enforcement capacity is systematically exceeded, the constraint''s effective type would drift toward piton in practice despite its tangled rope legal structure; if capacity holds, the expansive reading remains substantively extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_sovereignty_gap, empirical, 'Whether international enforcement matches the mandate''s aspirational breadth').

omega_variable(
    non_state_actor_expansion,
    'Is non-state persecution coverage an authentic textual interpretation of the Convention or a constructive expansion beyond the original kernel''s scope?',
    'Historical treaty negotiation records combined with subsequent state practice and international judicial interpretation tracing the emergence of non-state actor persecution doctrine.',
    'If authentically textual, the expansive reading is lineage-faithful interpretation; if constructive expansion, it represents drift that the authority structure has absorbed without formal codification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_state_actor_expansion, conceptual, 'Textual authenticity of non-state persecution coverage').

omega_variable(
    particular_social_group_boundary,
    'Does the inclusion of gender and LGBTQ+ identity in ''particular social group'' rest on immutable characteristics logic, self-identification logic, or social perception logic, and does the choice affect the constraint''s stability?',
    'Comparative analysis of domestic and international jurisprudence on PSG definition to identify which grounding logic predominates and where it produces fracture.',
    'Immutable-characteristics grounding aligns with restrictive reading premises and may narrow over time; self-identification or social-perception grounding aligns with expansive humanitarian logic but faces greater state resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(particular_social_group_boundary, conceptual, 'Grounding logic for gender and LGBTQ+ inclusion in PSG').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__expansive_humanitarian_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refugee_exp_humanitarian_tr_t0, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(refugee_exp_humanitarian_tr_t10, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(refugee_exp_humanitarian_tr_t20, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(refugee_exp_humanitarian_tr_t30, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(refugee_exp_humanitarian_tr_t40, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement(refugee_exp_humanitarian_tr_t50, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 50, 0.3).
narrative_ontology:measurement(refugee_exp_humanitarian_tr_t60, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 60, 0.35).
narrative_ontology:measurement(refugee_exp_humanitarian_tr_t70, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 70, 0.4).

% Extraction over time
narrative_ontology:measurement(refugee_exp_humanitarian_be_t0, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(refugee_exp_humanitarian_be_t10, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 10, 0.44).
narrative_ontology:measurement(refugee_exp_humanitarian_be_t20, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(refugee_exp_humanitarian_be_t30, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 30, 0.56).
narrative_ontology:measurement(refugee_exp_humanitarian_be_t40, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement(refugee_exp_humanitarian_be_t50, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement(refugee_exp_humanitarian_be_t60, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 60, 0.72).
narrative_ontology:measurement(refugee_exp_humanitarian_be_t70, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 70, 0.76).

% Suppression requirement over time
narrative_ontology:measurement(refugee_exp_humanitarian_su_t0, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(refugee_exp_humanitarian_su_t10, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 10, 0.35).
narrative_ontology:measurement(refugee_exp_humanitarian_su_t20, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(refugee_exp_humanitarian_su_t30, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement(refugee_exp_humanitarian_su_t40, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement(refugee_exp_humanitarian_su_t50, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 50, 0.65).
narrative_ontology:measurement(refugee_exp_humanitarian_su_t60, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 60, 0.72).
narrative_ontology:measurement(refugee_exp_humanitarian_su_t70, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 70, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(refugee_convention_text__expansive_humanitarian_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
