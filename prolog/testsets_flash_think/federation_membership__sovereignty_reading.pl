% ============================================================================
% CONSTRAINT STORY: federation_membership__sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership__sovereignty_reading, []).

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
 *   constraint_id: federation_membership__sovereignty_reading
 *   human_readable: Federation Membership (Sovereignty Reading)
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint represents the 'sovereignty reading' of federation
 *   membership, where national authority retains primary legitimacy over
 *   borders and migration policy, and free movement is treated as a
 *   negotiable policy rather than an inherent right. It is one reading of the
 *   'federation_membership' kernel, contrasting with an 'integration_reading'
 *   that emphasizes supranational authority and constitutional rights. The
 *   constraint functions as a Tangled Rope, coordinating national interests
 *   while extracting from mobile citizens and businesses through enforced
 *   mobility restrictions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership__sovereignty_reading, 0.78).
domain_priors:suppression_score(federation_membership__sovereignty_reading, 0.72).
domain_priors:theater_ratio(federation_membership__sovereignty_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership__sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership__sovereignty_reading, "Federation Membership (Sovereignty Reading)").
narrative_ontology:topic_domain(federation_membership__sovereignty_reading, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership__sovereignty_reading, 'b69619db-5fbb-462b-a4ed-319e41357311').
narrative_ontology:cs_kernel_codification('b69619db-5fbb-462b-a4ed-319e41357311', formalized).
narrative_ontology:cs_authority_grounding('b69619db-5fbb-462b-a4ed-319e41357311', lineage).
narrative_ontology:cs_interpretation_layer_present('b69619db-5fbb-462b-a4ed-319e41357311').
narrative_ontology:cs_reading_relation('b69619db-5fbb-462b-a4ed-319e41357311', federation_membership__integration_reading, coexists_with).
narrative_ontology:cs_axiom('b69619db-5fbb-462b-a4ed-319e41357311', foundational, national_sovereignty_primacy).
narrative_ontology:cs_axiom_status(national_sovereignty_primacy, holdable).
narrative_ontology:cs_axiom_grounding('b69619db-5fbb-462b-a4ed-319e41357311', national_sovereignty_primacy, deontological).
narrative_ontology:cs_axiom('b69619db-5fbb-462b-a4ed-319e41357311', foundational, free_movement_as_negotiable_policy).
narrative_ontology:cs_axiom_status(free_movement_as_negotiable_policy, holdable).
narrative_ontology:cs_axiom_grounding('b69619db-5fbb-462b-a4ed-319e41357311', free_movement_as_negotiable_policy, conventional).
narrative_ontology:cs_reference_frame('b69619db-5fbb-462b-a4ed-319e41357311', nation_state_primacy_framework).
narrative_ontology:cs_drift_state('b69619db-5fbb-462b-a4ed-319e41357311', contemporary_political_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b69619db-5fbb-462b-a4ed-319e41357311', '').
narrative_ontology:cs_kernel_id(federation_membership__sovereignty_reading, federation_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership__sovereignty_reading, national_governments).
narrative_ontology:constraint_beneficiary(federation_membership__sovereignty_reading, local_labor_markets).
narrative_ontology:constraint_beneficiary(federation_membership__sovereignty_reading, existing_citizens).
narrative_ontology:constraint_victim(federation_membership__sovereignty_reading, mobile_citizens).
narrative_ontology:constraint_victim(federation_membership__sovereignty_reading, migrants).
narrative_ontology:constraint_victim(federation_membership__sovereignty_reading, federation_businesses_seeking_labor).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain primary authority over borders, migration policy, and national laws within the federation framework. They benefit from the ability to control national demographics and labor markets, and from the legitimacy derived from upholding national sovereignty.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, national_governments, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Benefit from the ability of national governments to regulate the inflow of labor, potentially reducing competition for existing workers and allowing for more controlled wage dynamics. They are subject to national policy decisions.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, local_labor_markets, beneficiary,
    organized, biographical, constrained, local).

% Benefit from the perceived stability and cultural cohesion that national border control and migration policies aim to provide. They may also experience costs through reduced economic dynamism or labor shortages in specific sectors.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, existing_citizens, beneficiary,
    moderate, biographical, mobile, national).

% Face restrictions on their freedom of movement within the federation, subject to national policies, visa requirements, and border controls. They bear the costs of administrative hurdles, delays, and potential denial of entry or residence.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, mobile_citizens, payer,
    powerless, immediate, constrained, global).

% Are subject to strict national immigration laws and border enforcement, often with limited legal pathways for entry or residence. They bear the highest costs of exclusion and lack of access to federation benefits.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, migrants, payer,
    powerless, immediate, trapped, global).

% Face challenges in recruiting labor across national borders within the federation due to restrictive national policies. They bear the costs of labor shortages, increased recruitment expenses, and reduced competitiveness.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, federation_businesses_seeking_labor, payer,
    powerful, biographical, constrained, national).

% Their claims to legitimate authority over free movement and border policy are downplayed or rejected by this reading. They are structurally excluded from setting or enforcing these policies in a way that would supersede national control.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, supranational_institutions, excluded,
    institutional, generational, analytical, global).

% Advocate for greater supranational authority and unrestricted free movement within the federation. Their arguments are marginalized in policy debates dominated by the sovereignty reading, and their proposals face significant political resistance.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, pro_integration_advocates, excluded,
    organized, biographical, mobile, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the interests of national governments within a federal structure, allowing member states to cooperate on shared goals while retaining sovereign control over their borders and internal migration policies.
% TRANSFER_FUNCTION: Transfers control over border and migration policy from a potential supranational authority to national governments. It extracts costs from mobile citizens and businesses seeking labor by restricting movement and access.
% ABSENT_VOICES: Supranational institutions and pro-integration advocates, who would argue for free movement as a fundamental right and for greater shared sovereignty, are structurally excluded from the policy-making process under this reading.
% DISAPPEARANCE_RATIONALE: If this reading of federation membership vanished, national governments would lose a key framework for managing their borders and internal affairs in coordination with other states. The legal and political landscape for migration and citizenship would be fundamentally renegotiated, leading to significant disruption and uncertainty for all parties.
% FOUNDING_PROBLEM: To establish a framework for inter-state cooperation that respects and preserves the sovereignty of member nations, particularly regarding sensitive issues like national borders, citizenship, and internal security, while preventing uncontrolled migration flows.
% FOUNDING_PROBLEM_CORROBORATION: National political leaders, constitutional scholars focused on state rights, and public opinion in many member states consistently corroborate the ongoing relevance of national sovereignty and border control as a live problem within the federation. This is often articulated in national legislative debates and electoral campaigns.
narrative_ontology:disappearance_verdict(federation_membership__sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership__sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership__sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(federation_membership__sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership__sovereignty_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership__sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership__sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership__sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) due to the significant costs imposed on mobile citizens and migrants through restricted movement and access. Suppression is also high (0.72) as national border controls and policy enforcement are actively maintained to uphold this reading. The theater ratio is low (0.15) because the functions of border control and national policy enforcement are genuinely active and not merely performative. Accessibility collapse is moderate (0.60) as alternatives for movement exist but are heavily constrained by national policies. Resistance is moderate (0.55) from groups advocating for greater integration and free movement.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of national governments and existing citizens, this constraint is a legitimate mechanism for preserving national interests and stability. However, from the perspective of mobile citizens and migrants, it operates as a highly extractive and suppressive barrier to their freedom and opportunities. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   National governments, local labor markets, and existing citizens are beneficiaries (low directionality) as they gain control, stability, or reduced competition. Mobile citizens, migrants, and federation businesses seeking labor are targets (high directionality) as they bear the costs of restricted movement and labor access. Supranational institutions and pro-integration advocates are excluded, as their preferred framing is actively suppressed by this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    federation_nature_ambiguity,
    'Is the federation fundamentally a conditional treaty between sovereign states, or an irreversible process of integration towards a supranational entity?',
    'Analysis of constitutional amendments, judicial rulings by supranational courts, and shifts in member states'' treaty ratification processes over time.',
    'If resolved as irreversible integration, the ''sovereignty_reading'' would be reclassified as a Snare or Piton, as its claims of national control would be revealed as cover for an already superseded reality. If resolved as a conditional treaty, this reading''s claims would be reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(federation_nature_ambiguity, conceptual, 'Ambiguity regarding the fundamental nature of the federation''s legal and political structure.').

omega_variable(
    free_movement_status_ambiguity,
    'Is free movement within the federation a negotiable policy subject to national discretion, or a constitutional right derived from supranational law?',
    'Judicial review by the highest courts of the federation and its member states, particularly in cases challenging national migration policies.',
    'If free movement is affirmed as a constitutional right, the extraction from mobile citizens under the ''sovereignty_reading'' would be reclassified as illegitimate and the constraint would shift towards a Snare. If affirmed as negotiable, the current classification would be reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(free_movement_status_ambiguity, conceptual, 'Ambiguity regarding the legal status of free movement within the federal structure.').

omega_variable(
    legitimacy_of_border_control_grounding,
    'Is national border control an inherent right of sovereign states, or a delegated power within a larger federal framework?',
    'Comparative constitutional analysis across member states and the federation''s founding documents, alongside historical legal interpretations.',
    'If border control is found to be a delegated power, the ''sovereignty_reading''s claim to inherent national legitimacy would be undermined, potentially shifting the constraint towards a Snare by revealing its enforcement as exceeding its legitimate scope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_border_control_grounding, conceptual, 'Ambiguity regarding the ultimate source of legitimacy for national border control within the federation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership__sovereignty_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership__sovereignty_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(fede_tr_t5, federation_membership__sovereignty_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(fede_tr_t10, federation_membership__sovereignty_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(fede_tr_t15, federation_membership__sovereignty_reading, theater_ratio, 15, 0.15).
narrative_ontology:measurement(fede_tr_t20, federation_membership__sovereignty_reading, theater_ratio, 20, 0.15).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership__sovereignty_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(fede_be_t5, federation_membership__sovereignty_reading, base_extractiveness, 5, 0.7).
narrative_ontology:measurement(fede_be_t10, federation_membership__sovereignty_reading, base_extractiveness, 10, 0.74).
narrative_ontology:measurement(fede_be_t15, federation_membership__sovereignty_reading, base_extractiveness, 15, 0.76).
narrative_ontology:measurement(fede_be_t20, federation_membership__sovereignty_reading, base_extractiveness, 20, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership__sovereignty_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(fede_su_t5, federation_membership__sovereignty_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(fede_su_t10, federation_membership__sovereignty_reading, suppression_requirement, 10, 0.69).
narrative_ontology:measurement(fede_su_t15, federation_membership__sovereignty_reading, suppression_requirement, 15, 0.71).
narrative_ontology:measurement(fede_su_t20, federation_membership__sovereignty_reading, suppression_requirement, 20, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership__sovereignty_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
