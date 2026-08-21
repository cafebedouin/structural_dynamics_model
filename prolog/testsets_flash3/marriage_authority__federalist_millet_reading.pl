% ============================================================================
% CONSTRAINT STORY: marriage_authority__federalist_millet_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__federalist_millet_reading, []).

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
 *   constraint_id: marriage_authority__federalist_millet_reading
 *   human_readable: Federalist-Millet Reading of Fragmented Marriage Authority
 *   domain: legal/political/constitutional
 *
 * SUMMARY:
 *   This constraint describes the 'federalist-millet' reading of marriage
 *   authority, where legal pluralism in family law is deliberately maintained
 *   as a consociational mechanism to prevent majoritarian domination and
 *   ensure political stability in diverse societies. It views the
 *   fragmentation of authority not as a defect, but as a feature that
 *   protects minority rights and manages inter-communal relations through
 *   elite bargaining. This reading emphasizes the political function of legal
 *   pluralism over its communal or individual rights implications.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__federalist_millet_reading, 0.15).
domain_priors:suppression_score(marriage_authority__federalist_millet_reading, 0.2).
domain_priors:theater_ratio(marriage_authority__federalist_millet_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__federalist_millet_reading, rope).
narrative_ontology:human_readable(marriage_authority__federalist_millet_reading, "Federalist-Millet Reading of Fragmented Marriage Authority").
narrative_ontology:topic_domain(marriage_authority__federalist_millet_reading, "legal/political/constitutional").

domain_priors:requires_active_enforcement(marriage_authority__federalist_millet_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__federalist_millet_reading, 'ae0e7a71-0d03-456d-a69b-b8bc994b21a0').
narrative_ontology:cs_kernel_codification('ae0e7a71-0d03-456d-a69b-b8bc994b21a0', formalized).
narrative_ontology:cs_authority_grounding('ae0e7a71-0d03-456d-a69b-b8bc994b21a0', lineage).
narrative_ontology:cs_interpretation_layer_present('ae0e7a71-0d03-456d-a69b-b8bc994b21a0').
narrative_ontology:cs_reading_relation('ae0e7a71-0d03-456d-a69b-b8bc994b21a0', marriage_authority__communal_autonomy_reading, coexists_with).
narrative_ontology:cs_reading_relation('ae0e7a71-0d03-456d-a69b-b8bc994b21a0', marriage_authority__secularist_reading, coexists_with).
narrative_ontology:cs_reading_relation('ae0e7a71-0d03-456d-a69b-b8bc994b21a0', marriage_authority__gender_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('ae0e7a71-0d03-456d-a69b-b8bc994b21a0', marriage_authority__judicial_harmonization_reading, coexists_with).
narrative_ontology:cs_axiom('ae0e7a71-0d03-456d-a69b-b8bc994b21a0', foundational, fragmented_authority_prevents_tyranny).
narrative_ontology:cs_axiom_status(fragmented_authority_prevents_tyranny, holdable).
narrative_ontology:cs_axiom_grounding('ae0e7a71-0d03-456d-a69b-b8bc994b21a0', fragmented_authority_prevents_tyranny, instrumental).
narrative_ontology:cs_axiom('ae0e7a71-0d03-456d-a69b-b8bc994b21a0', foundational, elite_bargain_secures_minority_rights).
narrative_ontology:cs_axiom_status(elite_bargain_secures_minority_rights, holdable).
narrative_ontology:cs_axiom_grounding('ae0e7a71-0d03-456d-a69b-b8bc994b21a0', elite_bargain_secures_minority_rights, conventional).
narrative_ontology:cs_reference_frame('ae0e7a71-0d03-456d-a69b-b8bc994b21a0', consociational_pluralism_framework).
narrative_ontology:cs_drift_state('ae0e7a71-0d03-456d-a69b-b8bc994b21a0', contemporary_global_rights_discourse, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('ae0e7a71-0d03-456d-a69b-b8bc994b21a0', '').
narrative_ontology:cs_kernel_id(marriage_authority__federalist_millet_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__federalist_millet_reading, minority_religious_communities).
narrative_ontology:constraint_beneficiary(marriage_authority__federalist_millet_reading, political_elites).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_authority__federalist_millet_reading, majoritarian_political_parties).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the ability to govern internal family matters according to their own traditions, protecting their cultural and religious identity from majoritarian imposition. Their exit options are limited by national borders and the difficulty of maintaining cultural cohesion without legal recognition.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, minority_religious_communities, beneficiary,
    organized, generational, constrained, national).

% Maintain stability by accommodating diverse groups through legal pluralism, preventing majoritarian tyranny and managing inter-communal tensions. They navigate complex coalition-building and elite bargaining to sustain the fragmented system, avoiding politically costly uniform legislation.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, political_elites, agenda_setter,
    institutional, biographical, mobile, national).

% Bear the cost of legislative paralysis on family law, unable to impose a uniform civil code that might align with their ideological preferences or simplify the legal landscape. Their power is diffused by the consociational arrangement.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, majoritarian_political_parties, payer,
    institutional, biographical, constrained, national).

% Are excluded from direct legislative reform of personal laws due to the fragmented authority structure. They advocate for a uniform civil code or judicial intervention to ensure gender equality across all communities, often facing resistance from religious authorities and political elites.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, gender_equality_advocates, excluded,
    moderate, generational, constrained, national).

% Interprets the constitutional framework that permits legal pluralism, often tasked with balancing community autonomy against fundamental rights. Their role is to adjudicate disputes within the existing fragmented system, not to dismantle it.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, judiciary, observer,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the peaceful coexistence of diverse religious and cultural communities within a single state by granting them autonomy over personal law, thereby preventing majoritarian imposition and reducing inter-communal conflict.
% TRANSFER_FUNCTION: Transfers legislative authority over personal law from a centralized state legislature to recognized community-specific legal systems, in exchange for political stability and minority group loyalty.
% ABSENT_VOICES: Secularist reformers and gender equality advocates are largely absent from the elite bargains that sustain legal pluralism; they would argue for a uniform civil code and universal rights, but their proposals are sidelined by the consociational framework.
% DISAPPEARANCE_RATIONALE: If this fragmented authority vanished overnight, it would likely lead to immediate legislative attempts to impose a uniform civil code, triggering widespread resistance and potential civil unrest from minority communities who would perceive it as an attack on their identity and autonomy. The political landscape would be fundamentally reshaped.
% FOUNDING_PROBLEM: The problem of governing a deeply diverse society with multiple religious and cultural groups, each with distinct personal law traditions, without resorting to majoritarian oppression or civil conflict.
% FOUNDING_PROBLEM_CORROBORATION: Minority community leaders and political scientists specializing in consociationalism corroborate that the problem of managing diversity and preventing majoritarian domination remains live. While secularist critics argue the problem is manufactured, the historical record of inter-communal conflict in the absence of such arrangements provides strong external corroboration.
narrative_ontology:disappearance_verdict(marriage_authority__federalist_millet_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__federalist_millet_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__federalist_millet_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(marriage_authority__federalist_millet_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__federalist_millet_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__federalist_millet_reading_tests).
:- end_tests(marriage_authority__federalist_millet_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Rope because its primary function is genuine coordination (managing diversity, preventing tyranny) with relatively low extraction. Extraction (0.15) is present in the form of legislative paralysis and the costs borne by those seeking uniform laws, but it is not the primary driver. Suppression (0.2) is low, reflecting the consensual nature of consociational arrangements among elites, though it does suppress alternative legal frameworks. Theater ratio is low (0.05) as the system genuinely performs its function of maintaining stability.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of minority communities and political elites, this arrangement is a functional Rope that ensures their interests and stability. From the perspective of majoritarian parties or gender equality advocates, it might appear as a Tangled Rope or even a Snare, due to the legislative costs or the perceived suppression of universal rights. The engine's classification will reflect the structural position of each seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Minority religious communities are clear beneficiaries, gaining autonomy over personal law. Political elites also benefit by maintaining stability and their own positions through the consociational bargain. Majoritarian political parties bear costs through legislative gridlock. Gender equality advocates and secularists are excluded, as their goals are often at odds with the preservation of fragmented authority.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consociational_stability_vs_rights_erosion,
    'Does the consociational stability achieved by fragmented marriage authority come at the cost of eroding individual (especially gender) rights within minority communities?',
    'Empirical studies comparing individual rights outcomes in consociational systems with fragmented personal law versus systems with uniform civil codes, controlling for other factors.',
    'If rights erosion is significant, the effective extraction from individuals within minority communities is higher than currently measured, potentially reclassifying the constraint as a Tangled Rope from their seat. If not, the Rope classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consociational_stability_vs_rights_erosion, empirical, 'Trade-off between political stability and individual rights within legal pluralism.').

omega_variable(
    elite_bargain_vs_genuine_autonomy,
    'Is the fragmented authority a genuine expression of community autonomy, or primarily an elite bargain among political factions to maintain power?',
    'Analysis of decision-making processes within minority communities regarding personal law: do community members genuinely consent, or are decisions dictated by religious/political leaders aligned with the state''s consociational framework?',
    'If primarily an elite bargain, the ''beneficiary'' status of minority communities is partially theatrical, and the ''political_elites'' seat captures more of the effective benefit, shifting the constraint towards a Tangled Rope or Snare from the perspective of ordinary community members.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_bargain_vs_genuine_autonomy, conceptual, 'Distinguishing genuine community autonomy from elite-driven consociationalism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__federalist_millet_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority__federalist_millet_reading, theater_ratio, 0, 0.03).
narrative_ontology:measurement(marr_tr_t10, marriage_authority__federalist_millet_reading, theater_ratio, 10, 0.04).
narrative_ontology:measurement(marr_tr_t20, marriage_authority__federalist_millet_reading, theater_ratio, 20, 0.04).
narrative_ontology:measurement(marr_tr_t30, marriage_authority__federalist_millet_reading, theater_ratio, 30, 0.05).
narrative_ontology:measurement(marr_tr_t40, marriage_authority__federalist_millet_reading, theater_ratio, 40, 0.05).
narrative_ontology:measurement(marr_tr_t50, marriage_authority__federalist_millet_reading, theater_ratio, 50, 0.05).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority__federalist_millet_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(marr_be_t10, marriage_authority__federalist_millet_reading, base_extractiveness, 10, 0.12).
narrative_ontology:measurement(marr_be_t20, marriage_authority__federalist_millet_reading, base_extractiveness, 20, 0.13).
narrative_ontology:measurement(marr_be_t30, marriage_authority__federalist_millet_reading, base_extractiveness, 30, 0.14).
narrative_ontology:measurement(marr_be_t40, marriage_authority__federalist_millet_reading, base_extractiveness, 40, 0.15).
narrative_ontology:measurement(marr_be_t50, marriage_authority__federalist_millet_reading, base_extractiveness, 50, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority__federalist_millet_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(marr_su_t10, marriage_authority__federalist_millet_reading, suppression_requirement, 10, 0.17).
narrative_ontology:measurement(marr_su_t20, marriage_authority__federalist_millet_reading, suppression_requirement, 20, 0.18).
narrative_ontology:measurement(marr_su_t30, marriage_authority__federalist_millet_reading, suppression_requirement, 30, 0.19).
narrative_ontology:measurement(marr_su_t40, marriage_authority__federalist_millet_reading, suppression_requirement, 40, 0.2).
narrative_ontology:measurement(marr_su_t50, marriage_authority__federalist_millet_reading, suppression_requirement, 50, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__federalist_millet_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__secularist_reading).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of several readings of the 'marriage_authority' kernel, focusing on legal pluralism as a consociational anti-tyranny mechanism. It is linked to other readings that emphasize communal autonomy, secularism, gender rights, and judicial harmonization, as they all address different facets of the same underlying kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
