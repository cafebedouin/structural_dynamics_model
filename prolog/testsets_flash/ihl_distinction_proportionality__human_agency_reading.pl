% ============================================================================
% CONSTRAINT STORY: ihl_distinction_proportionality__human_agency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ihl_distinction_proportionality__human_agency_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ihl_distinction_proportionality__human_agency_reading
 *   human_readable: IHL Human Agency Requirement for Lethal Force
 *   domain: international_humanitarian_law/military_ethics/technology_governance
 *
 * SUMMARY:
 *   This constraint represents the 'human agency' reading of International
 *   Humanitarian Law's (IHL) distinction and proportionality obligations,
 *   asserting that irreducible human moral judgment is required for lethal
 *   force application. It posits that the Martens Clause principles of
 *   humanity prohibit delegating life/death decisions to machines. This
 *   reading renders fully autonomous lethal weapons systems (LAWS) unlawful
 *   unless a human makes the final targeting decision, thereby suppressing
 *   the development and deployment of such systems. It is a contested
 *   interpretation within the broader IHL framework.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__human_agency_reading, 0.7).
domain_priors:suppression_score(ihl_distinction_proportionality__human_agency_reading, 0.6).
domain_priors:theater_ratio(ihl_distinction_proportionality__human_agency_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__human_agency_reading, tangled_rope).
narrative_ontology:human_readable(ihl_distinction_proportionality__human_agency_reading, "IHL Human Agency Requirement for Lethal Force").
narrative_ontology:topic_domain(ihl_distinction_proportionality__human_agency_reading, "international_humanitarian_law/military_ethics/technology_governance").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__human_agency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__human_agency_reading, 'a6fb1434-2847-492e-91f9-d7555e8bdbc6').
narrative_ontology:cs_kernel_codification('a6fb1434-2847-492e-91f9-d7555e8bdbc6', formalized).
narrative_ontology:cs_authority_grounding('a6fb1434-2847-492e-91f9-d7555e8bdbc6', lineage).
narrative_ontology:cs_interpretation_layer_present('a6fb1434-2847-492e-91f9-d7555e8bdbc6').
narrative_ontology:cs_reading_relation('a6fb1434-2847-492e-91f9-d7555e8bdbc6', ihl_distinction_proportionality__categorical_prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('a6fb1434-2847-492e-91f9-d7555e8bdbc6', ihl_distinction_proportionality__outcomes_based_reading, forecloses).
narrative_ontology:cs_axiom('a6fb1434-2847-492e-91f9-d7555e8bdbc6', foundational, human_moral_judgment_is_irreducible).
narrative_ontology:cs_axiom_status(human_moral_judgment_is_irreducible, holdable).
narrative_ontology:cs_axiom_grounding('a6fb1434-2847-492e-91f9-d7555e8bdbc6', human_moral_judgment_is_irreducible, deontological).
narrative_ontology:cs_axiom('a6fb1434-2847-492e-91f9-d7555e8bdbc6', foundational, delegation_of_lethal_force_to_machines_violates_martens_clause).
narrative_ontology:cs_axiom_status(delegation_of_lethal_force_to_machines_violates_martens_clause, holdable).
narrative_ontology:cs_axiom_grounding('a6fb1434-2847-492e-91f9-d7555e8bdbc6', delegation_of_lethal_force_to_machines_violates_martens_clause, deontological).
narrative_ontology:cs_reference_frame('a6fb1434-2847-492e-91f9-d7555e8bdbc6', human_centric_ihl_interpretation).
narrative_ontology:cs_drift_state('a6fb1434-2847-492e-91f9-d7555e8bdbc6', contemporary_ai_advances, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('a6fb1434-2847-492e-91f9-d7555e8bdbc6', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__human_agency_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, ihl_interpretive_authorities).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, human_rights_advocates).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, military_operational_efficiency).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, autonomous_weapons_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, military_commanders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organizations like the ICRC that interpret and promote IHL, emphasizing the centrality of human judgment in lethal force decisions. They benefit from this reading by maintaining their interpretive authority and the moral framework of IHL.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, ihl_interpretive_authorities, agenda_setter,
    institutional, generational, analytical, global).

% Bear the operational costs of requiring human-in-the-loop decisions, potentially sacrificing speed, scale, and precision offered by fully autonomous systems. They must ensure compliance while seeking to maximize military effectiveness.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, military_commanders, payer,
    powerful, biographical, constrained, global).

% Face significant restrictions on the design and deployment of their systems, requiring human oversight even for highly capable AI. This limits market potential and R&D directions, imposing costs on their innovation and business models.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, autonomous_weapons_developers, payer,
    organized, biographical, constrained, global).

% Benefit from this reading as it upholds human dignity and accountability in warfare, aligning with their broader advocacy for ethical technology use. They actively promote and defend this interpretation.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, human_rights_advocates, beneficiary,
    organized, generational, mobile, global).

% States that prioritize military advantage through full autonomy are effectively excluded from legitimizing their systems under this reading, facing international pressure and potential legal challenges if they deploy such weapons.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, states_developing_laws, excluded,
    institutional, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the interpretation of IHL to ensure that the principles of distinction and proportionality are applied with human moral judgment, preventing the dehumanization of warfare and maintaining accountability.
% TRANSFER_FUNCTION: Transfers the burden of moral judgment and accountability for lethal force decisions to human operators, preventing its delegation to machines. It transfers potential military efficiency gains (speed, scale) back to human oversight costs.
% ABSENT_VOICES: States and military strategists who advocate for fully autonomous weapons systems based on perceived operational advantages (e.g., faster reaction times, reduced human risk) are effectively sidelined in this interpretive framework. They would argue for an outcomes-based approach to IHL compliance.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, the development and deployment of fully autonomous lethal weapons systems would accelerate significantly. The legal and ethical landscape of warfare would shift, potentially leading to a 'race to autonomy' and a redefinition of accountability in armed conflict.
% FOUNDING_PROBLEM: The problem of ensuring that the principles of distinction and proportionality, central to IHL, are applied with the necessary moral and contextual judgment, especially in the face of emerging military technologies that could automate lethal decision-making.
% FOUNDING_PROBLEM_CORROBORATION: IHL interpretive authorities and human rights advocates consistently attest that the problem of maintaining human moral judgment in lethal force application is live and increasingly urgent due to advancements in AI and autonomous weapons. This is corroborated by ongoing international debates and expert reports from independent bodies.
narrative_ontology:disappearance_verdict(ihl_distinction_proportionality__human_agency_reading, world_rearranges).
narrative_ontology:founding_problem_status(ihl_distinction_proportionality__human_agency_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__human_agency_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ihl_distinction_proportionality__human_agency_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ihl_distinction_proportionality__human_agency_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ihl_distinction_proportionality__human_agency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ihl_distinction_proportionality__human_agency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates the interpretation of IHL principles (beneficiaries: IHL interpretive authorities, human rights advocates) while simultaneously extracting from military operational efficiency and autonomous weapons developers (victims). The extractiveness (0.7) is high due to the significant limitations placed on military technology and strategy. Suppression (0.6) is also substantial, as this reading actively seeks to prevent the adoption of fully autonomous systems through legal and ethical pressure. Active enforcement is required to maintain this interpretation against competing views and technological advancements.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of IHL interpretive authorities, this constraint is a necessary safeguard for human dignity and accountability in warfare, a 'Rope' ensuring ethical conduct. From the perspective of military commanders and autonomous weapons developers, it is an 'Snare' that imposes significant operational and developmental costs, hindering technological progress and military effectiveness. The engine's computation will reflect this divergence based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   IHL interpretive authorities and human rights advocates are beneficiaries (d near 0.0) as this reading reinforces their mandate and values. Military commanders and autonomous weapons developers are targets (d near 1.0) as they bear the costs of restricted autonomy. States developing LAWS are excluded, facing structural barriers to legitimizing their systems under this framework.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not experiencing mandatrophy; its mandate is actively contested and evolving in response to technological advancements. The 'live' status of the founding problem, coupled with the 'world_rearranges' disappearance verdict, indicates that the constraint's function is still highly relevant and impactful, even if its interpretation is disputed. The classification as Tangled Rope reflects this ongoing tension between coordination and extraction, preventing mislabeling it as a Piton (atrophied) or a pure Snare (no coordination function).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    human_judgment_definition,
    'What constitutes ''irreducible human moral judgment'' in the context of lethal force application, and how can it be verified in practice?',
    'Development of clear, internationally agreed-upon operational definitions and verification protocols for human control over LAWS, potentially through expert consensus or state practice.',
    'A precise definition would clarify the boundary between lawful and unlawful autonomous systems, reducing ambiguity for developers and militaries. An overly broad or vague definition could lead to arbitrary enforcement or ''human-on-the-loop'' theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(human_judgment_definition, conceptual, 'Ambiguity in defining the core concept of human moral judgment.').

omega_variable(
    martens_clause_scope,
    'Does the Martens Clause categorically prohibit delegation of life/death decisions to machines, or does it allow for technological advancements that uphold humanity principles through other means?',
    'Further legal scholarship, state practice, and international judicial interpretation clarifying the scope and application of the Martens Clause in the context of emerging technologies.',
    'A categorical interpretation strengthens this reading, while a more flexible interpretation could open pathways for outcomes-based approaches to LAWS.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(martens_clause_scope, conceptual, 'Scope of Martens Clause prohibition on machine-decided killing.').

omega_variable(
    ihl_interpretation_contest,
    'Is this reading of IHL''s distinction and proportionality obligations the dominant or most defensible interpretation, or is it one among several equally valid readings?',
    'Analysis of state practice, opinio juris, and the positions of leading international legal bodies and academic consensus over time.',
    'If this reading gains wider acceptance, its suppressive and extractive force increases. If other readings (e.g., outcomes-based) gain dominance, this constraint''s influence diminishes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ihl_interpretation_contest, empirical, 'Contestation over the authoritative interpretation of IHL for LAWS.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__human_agency_reading, 1977, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(ihl__be_t1977, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 1977, 0.4).
narrative_ontology:measurement(ihl__be_t1990, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 1990, 0.5).
narrative_ontology:measurement(ihl__be_t2000, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(ihl__be_t2010, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(ihl__be_t2024, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 2024, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(ihl__su_t1977, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 1977, 0.3).
narrative_ontology:measurement(ihl__su_t1990, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 1990, 0.4).
narrative_ontology:measurement(ihl__su_t2000, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 2000, 0.48).
narrative_ontology:measurement(ihl__su_t2010, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 2010, 0.55).
narrative_ontology:measurement(ihl__su_t2024, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_distinction_proportionality__human_agency_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__human_agency_reading, ihl_distinction_proportionality__categorical_prohibition_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__human_agency_reading, ihl_distinction_proportionality__outcomes_based_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__human_agency_reading, laws_development_and_deployment).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'ihl_distinction_proportionality' kernel. It emphasizes human agency, contrasting with outcomes-based and categorical prohibition readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
