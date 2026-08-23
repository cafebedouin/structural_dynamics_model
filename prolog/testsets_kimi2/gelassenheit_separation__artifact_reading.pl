% ============================================================================
% CONSTRAINT STORY: gelassenheit_separation__artifact_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gelassenheit_separation__artifact_reading, []).

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
 *   constraint_id: gelassenheit_separation__artifact_reading
 *   human_readable: Gelassenheit Separation: Artifact Reading (Visible Distinction Regardless of Function)
 *   domain: religious/technology_governance/commitment_systems
 *
 * SUMMARY:
 *   In certain Plain Anabaptist communities, the Gelassenheit commitment to
 *   separation from 'the world' is interpreted through an artifact-reading
 *   lens: technology is forbidden if its physical appearance resembles
 *   English or worldly artifacts, regardless of whether it is functionally
 *   isolated or off-grid. Solar panels, synthetic fabrics, and certain
 *   finishes are banned because they look modern, even when they would not
 *   connect the community to external systems. This constraint story treats
 *   the artifact reading as a distinct commitment-system constraint with high
 *   extraction and high suppression, grounded in a theological kernel but
 *   enforced through communal practice and ministerial authority. It is
 *   claimed as tangled_rope: it coordinates genuine identity-boundary
 *   maintenance while asymmetrically extracting material costs from
 *   households that would benefit from functionally harmless technology.
 *
 * KEY AGENTS:
 *   - ordnung_ministry (agenda_setter / institutional / constrained): Interprets and enforces the Gelassenheit rule. Bears the political cost of rule maintenance but derives authority from successful boundary policing.
 *   - traditionalist_majority (beneficiary / organized / identity_locked): Values visible distinction and benefits from the clear boundary that validates their identity and life choices.
 *   - pragmatic_households (payer / moderate / constrained): Bear direct material costs of banned technology (off-grid solar, modern fabrics) with no functional offset. Limited exit due to land and kinship.
 *   - youth_and_innovators (payer / powerless / identity_locked): Young members blocked from viable non-farm paths and comfortable standards. Highly identity-locked due to upbringing.
 *   - english_tech_vendors (excluded / moderate / trapped): Barred from supplying technology to the community; their exclusion is constitutive of the boundary.
 *   - outside_scholars (observer / analytical / analytical): Provide external analytical perspective on the tension between symbolic boundary and functional need.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gelassenheit_separation__artifact_reading, 0.82).
domain_priors:suppression_score(gelassenheit_separation__artifact_reading, 0.88).
domain_priors:theater_ratio(gelassenheit_separation__artifact_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gelassenheit_separation__artifact_reading, tangled_rope).
narrative_ontology:human_readable(gelassenheit_separation__artifact_reading, "Gelassenheit Separation: Artifact Reading (Visible Distinction Regardless of Function)").
narrative_ontology:topic_domain(gelassenheit_separation__artifact_reading, "religious/technology_governance/commitment_systems").

domain_priors:requires_active_enforcement(gelassenheit_separation__artifact_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gelassenheit_separation__artifact_reading, '4abf159e-a339-4893-a010-faed27cefb58').
narrative_ontology:cs_kernel_codification('4abf159e-a339-4893-a010-faed27cefb58', distributed).
narrative_ontology:cs_authority_grounding('4abf159e-a339-4893-a010-faed27cefb58', lineage).
narrative_ontology:cs_interpretation_layer_present('4abf159e-a339-4893-a010-faed27cefb58').
narrative_ontology:cs_reading_relation('4abf159e-a339-4893-a010-faed27cefb58', gelassenheit_separation__principle_reading, coexists_with).
narrative_ontology:cs_reading_relation('4abf159e-a339-4893-a010-faed27cefb58', gelassenheit_separation__consequence_reading, coexists_with).
narrative_ontology:cs_axiom('4abf159e-a339-4893-a010-faed27cefb58', foundational, visible_resemblance_trumps_function).
narrative_ontology:cs_axiom_status(visible_resemblance_trumps_function, holdable).
narrative_ontology:cs_axiom_grounding('4abf159e-a339-4893-a010-faed27cefb58', visible_resemblance_trumps_function, theological).
narrative_ontology:cs_axiom('4abf159e-a339-4893-a010-faed27cefb58', secondary, functional_isolation_is_no_exemption).
narrative_ontology:cs_axiom_status(functional_isolation_is_no_exemption, holdable).
narrative_ontology:cs_axiom_grounding('4abf159e-a339-4893-a010-faed27cefb58', functional_isolation_is_no_exemption, theological).
narrative_ontology:cs_reference_frame('4abf159e-a339-4893-a010-faed27cefb58', visible_distinction_orthodoxy).
narrative_ontology:cs_drift_state('4abf159e-a339-4893-a010-faed27cefb58', contemporary_tech_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4abf159e-a339-4893-a010-faed27cefb58', '').
narrative_ontology:cs_kernel_id(gelassenheit_separation__artifact_reading, gelassenheit_separation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gelassenheit_separation__artifact_reading, traditionalist_majority).
narrative_ontology:constraint_victim(gelassenheit_separation__artifact_reading, pragmatic_households).
narrative_ontology:constraint_victim(gelassenheit_separation__artifact_reading, youth_and_innovators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces the Gelassenheit separation rule through the Ordnung. Determines whether a given technology resembles worldly artifacts regardless of its functional isolation. Derives authority and legitimacy from maintaining visible community boundaries. Can alter the rule but risks schism or loss of standing if they relax the standard.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, ordnung_ministry, agenda_setter,
    institutional, generational, constrained, regional).

% Members whose social standing, identity, and family networks depend on the visible distinction from English society being maintained. They benefit from the clear boundary because it validates their life choices and preserves the community's special status. Exit would mean losing kinship, salvation framework, and identity.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, traditionalist_majority, beneficiary,
    organized, generational, identity_locked, regional).

% Member households that bear practical costs of the rule: denied off-grid solar electricity, restricted to less effective fabrics and tools. They comply to remain in good standing but suffer material disadvantage with no functional offset. Their exit options are limited by land ownership patterns and family ties.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, pragmatic_households, payer,
    moderate, biographical, constrained, local).

% Young members and those inclined toward technological adaptation. They experience the rule as blocking viable non-farm economic paths and comfortable living standards. Highly identity-locked because they were raised inside the community with limited outside contacts, capital, or English language fluency.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, youth_and_innovators, payer,
    powerless, biographical, identity_locked, local).

% Merchants and technicians who would supply solar panels, modern fabrics, and functional tools to Plain households if permitted. Structurally barred from the market by the Ordnung's prohibition on possession, not merely use. Their exclusion is constitutive of the boundary.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, english_tech_vendors, excluded,
    moderate, immediate, trapped, regional).

% Academic observers of Anabaptist and Plain communities who document technology rules. They analyze the tension between functional need and symbolic boundary maintenance. They have no stake in the outcome but provide the external analytical seat.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, outside_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gelassenheit_separation__artifact_reading, ordnung_ministry).
narrative_ontology:fixing_cost_class(gelassenheit_separation__artifact_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a visible boundary between the Plain community and mainstream English society, preventing cultural assimilation by ensuring all material culture signals non-membership and religious distinctiveness.
% TRANSFER_FUNCTION: Transfers compliance labor and material disadvantage (denied functional technology, higher labor inputs) from member householdsâespecially pragmatic households and youthâto the maintenance of communal boundary purity, while consolidating interpretive authority in the ordnung ministry.
% ABSENT_VOICES: Technologists and engineers who could design culturally distinctive but functionally equivalent tools; member households with medical or off-grid energy needs that functional technology would address; youth who have experienced English society and see the arbitrariness of resemblance-based rules; sibling congregations that evaluate technology by function or consequence rather than appearance.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight, the visible distinction that marks the community as separate would erode; material practices would shift toward English patterns as pragmatic households adopted solar panels and modern fabrics; the ministry's authority over technology adjudication would collapse; and the community would face accelerated assimilation pressure.
% FOUNDING_PROBLEM: The historical threat of cultural assimilation into mainstream English society, which eroded religious minority communities through gradual adoption of material culture, dress, and technology, dissolving the visible witness of nonconformity.
% FOUNDING_PROBLEM_CORROBORATION: Historical Anabaptist scholarship documents assimilation pressure in the 19th and early 20th centuries. However, independent sociologists of religion note that many communities now face economic and demographic pressures that the artifact reading exacerbates rather than solves, and no outside corroboration confirms that forbidding off-grid solar panels prevents assimilation in the contemporary context.
narrative_ontology:disappearance_verdict(gelassenheit_separation__artifact_reading, world_rearranges).
narrative_ontology:founding_problem_status(gelassenheit_separation__artifact_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gelassenheit_separation__artifact_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gelassenheit_separation__artifact_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gelassenheit_separation__artifact_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gelassenheit_separation__artifact_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gelassenheit_separation__artifact_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gelassenheit_separation__artifact_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82) is high because the rule forbids functionally beneficial items purely on aesthetic/theological grounds, creating pure cost with no functional offset for payer households. Suppression (0.88) is maximal because persistence depends on active ministerial enforcement, church discipline, and the threat of shunning; without this, members would adopt beneficial banned technology. Theater_ratio (0.65) reflects that the rule prioritizes visible performance of separation over actual structural isolation â a solar panel ban off-grid is symbolic theater. Accessibility_collapse (0.78) is high because identity-locked and kinship-bound members experience exit as near-total life disruption. Resistance (0.45) captures ongoing quiet non-compliance, youth departure, and internal debate, though open resistance is suppressed. Measurements share one time grid and show extraction, theater, and enforcement all rising as external technology advances.
 *
 * PERSPECTIVAL GAP:
 *   From the ministry seat, the constraint is faithful stewardship of a sacred boundary preventing assimilation. From the pragmatic household seat, it is arbitrary deprivation with no compensatory benefit. The engine computes this divergence from structural asymmetry in exit options and the beneficiary/victim declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   The ordnung_ministry sits near the agenda-setter end: they do not personally bear technology costs, and their authority is reinforced by boundary policing. The traditionalist_majority sits as beneficiary: their identity investment is subsidized by the constraint. Pragmatic_households and youth_and_innovators are structural targets: they bear extraction directly, with limited exit. English_tech_vendors are excluded entirely, their directionality derived from structural lockout rather than direct extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying as tangled_rope prevents mislabeling the constraint as pure extraction (snare) or pure coordination (rope). The genuine coordination function â preventing assimilation and maintaining a centuries-old religious identity â is real, but the artifact reading layers onto it an extractive asymmetry: the cost of boundary maintenance is borne disproportionately by households with practical needs, while the benefit of clear identity markers flows to the traditionalist majority and ministry authority. If the founding problem (assimilation) were solvable by functional isolation rather than visible resemblance, the artifact reading would lose its coordination justification and reveal itself as pure performance. The temporal measurements show extraction and theater rising over time as external technology advances, suggesting the coordination function is being progressively overtaken by enforcement overhead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    artifact_theological_cover,
    'Is the artifact reading grounded in a genuine theological requirement of visible nonconformity, or is it a conventional boundary marker that could be reinterpreted without loss of doctrinal core?',
    'Comparative theological analysis across Plain congregations: if congregations holding the principle reading (functional isolation) maintain the same doctrinal commitment without the artifact ban, the visible-resemblance rule is conventional rather than theological.',
    'If conventional, the constraint''s extraction is not offset by a deontological coordination function and computes closer to snare; if theological, the high extraction is partially offset by a legitimacy claim that members genuinely hold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(artifact_theological_cover, conceptual, 'Whether the artifact reading is theological or conventional in origin').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the suppression that maintains this constraint primarily structural (threat of shunning, expulsion, loss of livelihood) or internalized (members believe they deserve the deprivation and have fused their identity with the community)?',
    'Post-exit trajectory study: if suppression of technology adoption persists after a member has physically left the community, the suppression is partially internalized; if adoption resumes immediately upon exit, it was structural.',
    'If internalized, effective suppression exceeds the structural measure because members carry the constraint with them after exit, raising accessibility_collapse and extractiveness for the identity-locked seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized suppression mechanism').

omega_variable(
    enforcement_tech_race,
    'Does the enforcement of visible distinction require ever-increasing suppression as external technology becomes more ubiquitous and affordable?',
    'Time-series correlation between external technology diffusion indices and internal enforcement incidents (confessions, shunnings, ministerial interventions) within artifact-reading communities.',
    'If enforcement must ratchet upward to maintain the same visible gap, the coordination function is being overtaken by enforcement overhead, indicating a drift toward snare or piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_tech_race, empirical, 'Whether suppression intensifies with external technological advance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gelassenheit_separation__artifact_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gela_tr_t0, gelassenheit_separation__artifact_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(gela_tr_t8, gelassenheit_separation__artifact_reading, theater_ratio, 8, 0.45).
narrative_ontology:measurement(gela_tr_t16, gelassenheit_separation__artifact_reading, theater_ratio, 16, 0.5).
narrative_ontology:measurement(gela_tr_t24, gelassenheit_separation__artifact_reading, theater_ratio, 24, 0.56).
narrative_ontology:measurement(gela_tr_t32, gelassenheit_separation__artifact_reading, theater_ratio, 32, 0.61).
narrative_ontology:measurement(gela_tr_t40, gelassenheit_separation__artifact_reading, theater_ratio, 40, 0.65).

% Extraction over time
narrative_ontology:measurement(gela_be_t0, gelassenheit_separation__artifact_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(gela_be_t8, gelassenheit_separation__artifact_reading, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(gela_be_t16, gelassenheit_separation__artifact_reading, base_extractiveness, 16, 0.66).
narrative_ontology:measurement(gela_be_t24, gelassenheit_separation__artifact_reading, base_extractiveness, 24, 0.72).
narrative_ontology:measurement(gela_be_t32, gelassenheit_separation__artifact_reading, base_extractiveness, 32, 0.78).
narrative_ontology:measurement(gela_be_t40, gelassenheit_separation__artifact_reading, base_extractiveness, 40, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(gela_su_t0, gelassenheit_separation__artifact_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(gela_su_t8, gelassenheit_separation__artifact_reading, suppression_requirement, 8, 0.68).
narrative_ontology:measurement(gela_su_t16, gelassenheit_separation__artifact_reading, suppression_requirement, 16, 0.74).
narrative_ontology:measurement(gela_su_t24, gelassenheit_separation__artifact_reading, suppression_requirement, 24, 0.8).
narrative_ontology:measurement(gela_su_t32, gelassenheit_separation__artifact_reading, suppression_requirement, 32, 0.85).
narrative_ontology:measurement(gela_su_t40, gelassenheit_separation__artifact_reading, suppression_requirement, 40, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gelassenheit_separation__artifact_reading, identity_coordination).
narrative_ontology:affects_constraint(gelassenheit_separation__artifact_reading, principle_reading).
narrative_ontology:affects_constraint(gelassenheit_separation__artifact_reading, consequence_reading).

% DUAL FORMULATION NOTE:
% The gelassenheit_separation kernel decomposes into three structurally distinct readings (artifact, principle, consequence) because the natural-language concept of 'separation' conflates incompatible decision procedures: visible resemblance, functional isolation, and consequential evaluation. Each reading has a distinct epsilon, stakeholder structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
