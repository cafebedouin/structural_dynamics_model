% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__treaty_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__treaty_primacy_reading, []).

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
 *   constraint_id: secession_legitimacy_boundary__treaty_primacy_reading
 *   human_readable: Indigenous Treaty Primacy in Secession Legitimacy
 *   domain: political_economy/federalism/resource_politics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'treaty primacy' reading of the
 *   secession legitimacy boundary kernel. It asserts that Indigenous treaty
 *   rights are paramount, predating and superseding both federal and
 *   provincial authority, and therefore, no secession is legitimate without
 *   the explicit consent of Indigenous treaty holders. This reading places
 *   Indigenous nations as key actors whose consent is a prerequisite for any
 *   territorial redefinition, fundamentally altering the traditional
 *   federal-provincial dynamic of secession debates.
 *
 * KEY AGENTS:
 *   - indigenous_treaty_holders: Primary beneficiary (institutional/identity_locked) — their consent is required
 *   - provincial_separatist_movements: Primary target (organized/constrained) — must seek Indigenous consent
 *   - federal_government: Secondary target (institutional/constrained) — constrained in secession negotiations
 *   - provincial_governments: Secondary target (institutional/constrained) — challenged on land/resource claims
 *   - international_legal_observers: Analytical observer (analytical/analytical) — assesses legitimacy against international norms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__treaty_primacy_reading, 0.65).
domain_priors:suppression_score(secession_legitimacy_boundary__treaty_primacy_reading, 0.7).
domain_priors:theater_ratio(secession_legitimacy_boundary__treaty_primacy_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__treaty_primacy_reading, tangled_rope).
narrative_ontology:human_readable(secession_legitimacy_boundary__treaty_primacy_reading, "Indigenous Treaty Primacy in Secession Legitimacy").
narrative_ontology:topic_domain(secession_legitimacy_boundary__treaty_primacy_reading, "political_economy/federalism/resource_politics").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__treaty_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__treaty_primacy_reading, '16fdd28d-36f7-4062-8ecf-fd9fbedce6eb').
narrative_ontology:cs_kernel_codification('16fdd28d-36f7-4062-8ecf-fd9fbedce6eb', formalized).
narrative_ontology:cs_authority_grounding('16fdd28d-36f7-4062-8ecf-fd9fbedce6eb', lineage).
narrative_ontology:cs_interpretation_layer_present('16fdd28d-36f7-4062-8ecf-fd9fbedce6eb').
narrative_ontology:cs_reading_relation('16fdd28d-36f7-4062-8ecf-fd9fbedce6eb', secession_legitimacy_boundary__constitutional_impossibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('16fdd28d-36f7-4062-8ecf-fd9fbedce6eb', secession_legitimacy_boundary__popular_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('16fdd28d-36f7-4062-8ecf-fd9fbedce6eb', secession_legitimacy_boundary__grievance_threshold_reading, influences).
narrative_ontology:cs_axiom('16fdd28d-36f7-4062-8ecf-fd9fbedce6eb', foundational, indigenous_sovereignty_predates_state).
narrative_ontology:cs_axiom_status(indigenous_sovereignty_predates_state, holdable).
narrative_ontology:cs_axiom_grounding('16fdd28d-36f7-4062-8ecf-fd9fbedce6eb', indigenous_sovereignty_predates_state, deontological).
narrative_ontology:cs_axiom('16fdd28d-36f7-4062-8ecf-fd9fbedce6eb', foundational, treaties_are_sacred_agreements).
narrative_ontology:cs_axiom_status(treaties_are_sacred_agreements, holdable).
narrative_ontology:cs_axiom_grounding('16fdd28d-36f7-4062-8ecf-fd9fbedce6eb', treaties_are_sacred_agreements, deontological).
narrative_ontology:cs_reference_frame('16fdd28d-36f7-4062-8ecf-fd9fbedce6eb', pre_colonial_indigenous_sovereignty).
narrative_ontology:cs_drift_state('16fdd28d-36f7-4062-8ecf-fd9fbedce6eb', contemporary_post_colonial_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('16fdd28d-36f7-4062-8ecf-fd9fbedce6eb', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__treaty_primacy_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__treaty_primacy_reading, indigenous_treaty_holders).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__treaty_primacy_reading, provincial_separatist_movements).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__treaty_primacy_reading, federal_government).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__treaty_primacy_reading, provincial_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their rights and sovereignty are affirmed as foundational, predating colonial structures. They benefit from the recognition that their consent is required for any territorial alteration, including secession. Their identity is deeply tied to their ancestral lands and treaty relationships, making 'exit' from these relationships unthinkable.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, indigenous_treaty_holders, beneficiary,
    institutional, generational, identity_locked, regional).

% Bear the cost of needing Indigenous consent for their secession claims to be legitimate. This reading directly challenges their popular sovereignty arguments and requires them to engage in complex, potentially unfavorable, negotiations with Indigenous nations.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, provincial_separatist_movements, payer,
    organized, biographical, constrained, national).

% Is constrained by this reading, as it cannot unilaterally negotiate secession with a province without also upholding its fiduciary and treaty obligations to Indigenous peoples. This complicates federal responses to separatist movements and limits its flexibility.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, federal_government, payer,
    institutional, generational, constrained, national).

% Are directly challenged by this reading, as it asserts a higher authority over land and resource claims than their own provincial jurisdiction. They must contend with Indigenous rights in all land-use and governance decisions, especially those related to secession.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, provincial_governments, payer,
    institutional, generational, constrained, regional).

% Analyze the legal and ethical implications of secession claims in light of Indigenous rights and international law. Their assessments can influence global opinion and diplomatic pressure on states involved in such disputes.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, international_legal_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate the recognition of pre-existing Indigenous sovereignty and rights within the framework of modern state formation and potential dissolution, ensuring that Indigenous peoples are not marginalized or dispossessed by internal state reconfigurations.
% TRANSFER_FUNCTION: Transfers ultimate authority over land and territorial integrity from federal/provincial claims to Indigenous treaty holders, requiring their consent for significant changes. It also transfers political leverage and recognition to Indigenous nations.
% ABSENT_VOICES: Historical colonial authorities and legal doctrines that denied Indigenous sovereignty are absent from contemporary legitimate discourse, though their legacy continues to shape institutional structures. Their 'voice' would assert terra nullius or parliamentary supremacy without regard for treaty obligations.
% DISAPPEARANCE_RATIONALE: If this reading vanished, provincial separatist movements would gain significant leverage, potentially leading to unilateral declarations of independence that disregard Indigenous rights. Indigenous nations would lose a critical legal and political tool for protecting their lands and self-determination, leading to widespread conflict and dispossession. The entire federal structure would be destabilized.
% FOUNDING_PROBLEM: The historical and ongoing dispossession of Indigenous peoples through colonial expansion and the assertion of state sovereignty over their traditional territories, often in violation of treaties or without consent.
% FOUNDING_PROBLEM_CORROBORATION: Indigenous legal scholars, international human rights organizations, and UN declarations (e.g., UNDRIP) consistently corroborate that the problem of colonial dispossession and the need for Indigenous self-determination remain live and urgent. This corroboration comes from sources outside the direct beneficiaries of the reading.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__treaty_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__treaty_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__treaty_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(secession_legitimacy_boundary__treaty_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__treaty_primacy_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__treaty_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(secession_legitimacy_boundary__treaty_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(secession_legitimacy_boundary__treaty_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high from the perspective of provincial separatist movements and federal/provincial governments, as it imposes a significant hurdle (Indigenous consent) on their claims to territorial sovereignty. Suppression (0.70) is also high, as this reading actively suppresses unilateral secessionist claims that disregard Indigenous rights, requiring active legal and political enforcement to uphold treaty obligations. The rising trend in both metrics reflects the increasing legal and political recognition of Indigenous rights over time, making it harder for state actors to ignore them. The claimed type is Tangled Rope because it genuinely coordinates the recognition of Indigenous sovereignty (beneficiary) while simultaneously extracting concessions and imposing constraints on federal and provincial actors (victims) through active enforcement of treaty law.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Indigenous treaty holders, this constraint is a vital Rope, a mechanism for upholding justice and self-determination. From the perspective of provincial separatist movements, it is a Snare, an illegitimate barrier to their self-determination claims. The engine's classification will reflect this divergence based on the declared roles and structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous treaty holders are beneficiaries (d near 0.0) as the constraint affirms their pre-existing rights and grants them significant leverage. Provincial separatist movements and federal/provincial governments are targets (d near 1.0) as the constraint imposes substantial limitations and costs on their actions. International legal observers are analytical (d near 0.5). The 'identity_locked' exit option for Indigenous treaty holders reflects their deep, non-negotiable connection to their lands and treaties, which cannot be 'exited' in the same way a political movement might be dissolved.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents the mislabeling of Indigenous rights as mere 'grievances' that can be dismissed. By framing treaty primacy as a Tangled Rope, it acknowledges both the genuine coordination function (upholding Indigenous sovereignty) and the asymmetric extraction (from state actors who must now seek consent). It ensures that the constraint's persistence is tied to the ongoing need to rectify historical injustices and uphold pre-existing rights, rather than merely institutional inertia. The 'live' status of the founding problem further confirms that the mandate has not atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_indigenous_consent,
    'What is the precise scope and mechanism of ''Indigenous consent'' required for secession? Does it imply a veto, a consultation process, or a negotiation leading to an agreement?',
    'Legal precedent from future court cases, specific legislative frameworks, or negotiated agreements between Indigenous nations and state actors defining the process.',
    'If consent implies a veto, the constraint''s suppression on separatist movements is higher. If it''s a consultation, the suppression is lower, but the process is still binding. This impacts the effective extractiveness on provincial and federal actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_indigenous_consent, conceptual, 'Ambiguity in the operationalization of Indigenous consent.').

omega_variable(
    treaty_vs_aboriginal_rights_distinction,
    'How does the ''primacy'' of treaty rights interact with unceded Aboriginal title and rights, which may not be formally ''treatied'' but are equally foundational?',
    'Further legal clarification from higher courts or comprehensive land claims agreements that reconcile treaty and aboriginal rights in the context of state reconfigurations.',
    'If unceded Aboriginal title is given equal primacy, the scope of Indigenous consent expands beyond formal treaty areas, increasing the constraint''s impact on all state actors. If only formal treaties are recognized, the constraint''s scope is narrower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_vs_aboriginal_rights_distinction, conceptual, 'Distinction between treaty rights and broader aboriginal rights in secession context.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers, political pressure) or internalized (state actors accepting the moral force of Indigenous claims)?',
    'Post-exit suppression trajectory: if state actors continue to acknowledge Indigenous consent requirements even after legal challenges fail, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — state actors carry the suppression with them after legal defeats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for state actors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__treaty_primacy_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(sece_be_t1970, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 1970, 0.4).
narrative_ontology:measurement(sece_be_t1985, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 1985, 0.5).
narrative_ontology:measurement(sece_be_t2000, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(sece_be_t2010, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement(sece_be_t2024, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t1970, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 1970, 0.5).
narrative_ontology:measurement(sece_su_t1985, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 1985, 0.58).
narrative_ontology:measurement(sece_su_t2000, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement(sece_su_t2010, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement(sece_su_t2024, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__treaty_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, constitutional_impossibility_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, popular_sovereignty_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, grievance_threshold_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the 'secession_legitimacy_boundary' kernel. It asserts the primacy of Indigenous treaty rights, influencing and coexisting with other readings that emphasize constitutional text, popular sovereignty, or grievance thresholds.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
