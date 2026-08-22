% ============================================================================
% CONSTRAINT STORY: federation_membership_kernel__welfare_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_kernel__welfare_coordination_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: federation_membership_kernel__welfare_coordination_reading
 *   human_readable: EU Welfare Coordination Reading of Free Movement Kernel
 *   domain: political_economy/federalism/migration
 *
 * SUMMARY:
 *   This constraint instantiates the welfare_coordination_reading of the
 *   federation_membership_kernel, which treats EU free movement of workers as
 *   operating through intergovernmental coordination of national welfare
 *   systems rather than supranational harmonization. The EU enforces
 *   anti-social-dumping rules and social security coordination regulations
 *   while preserving member state autonomy over welfare design. Sibling
 *   readings are the integration_reading (expansive supranational labor
 *   mobility as citizenship right) and the member_sovereignty_reading
 *   (national boundedness of free movement by welfare capacity). This reading
 *   is distinguished by its dual commitment: genuine coordination function
 *   (preserving diverse welfare states) and asymmetric extraction (posted
 *   workers, native workers, and sending states bear mobility costs).
 *
 * KEY AGENTS:
 *   - eu_commission: Primary agenda-setter (institutional/arbitrage) â enforces coordination and anti-social-dumping
 *   - member_state_governments: Primary coordination beneficiaries (institutional/constrained) â retain welfare design autonomy
 *   - posting_employers: Secondary beneficiary (powerful/mobile) â captures cost arbitrage from posting
 *   - posted_workers: Primary target (powerless/trapped) â bears wage undercutting and exclusion from host social protection
 *   - native_workers_receiving_states: Secondary target (moderate/constrained) â faces labor market competition from undercut posted labor
 *   - sending_state_governments: Tertiary target (institutional/constrained) â loses fiscal base without compensation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__welfare_coordination_reading, 0.62).
domain_priors:suppression_score(federation_membership_kernel__welfare_coordination_reading, 0.55).
domain_priors:theater_ratio(federation_membership_kernel__welfare_coordination_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__welfare_coordination_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__welfare_coordination_reading, "EU Welfare Coordination Reading of Free Movement Kernel").
narrative_ontology:topic_domain(federation_membership_kernel__welfare_coordination_reading, "political_economy/federalism/migration").

domain_priors:requires_active_enforcement(federation_membership_kernel__welfare_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__welfare_coordination_reading, 'fa265e05-6e9b-4622-98d9-1c7d3a3077e1').
narrative_ontology:cs_kernel_codification('fa265e05-6e9b-4622-98d9-1c7d3a3077e1', formalized).
narrative_ontology:cs_authority_grounding('fa265e05-6e9b-4622-98d9-1c7d3a3077e1', lineage).
narrative_ontology:cs_interpretation_layer_present('fa265e05-6e9b-4622-98d9-1c7d3a3077e1').
narrative_ontology:cs_reading_relation('fa265e05-6e9b-4622-98d9-1c7d3a3077e1', federation_membership_kernel__integration_reading, influences).
narrative_ontology:cs_reading_relation('fa265e05-6e9b-4622-98d9-1c7d3a3077e1', federation_membership_kernel__member_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('fa265e05-6e9b-4622-98d9-1c7d3a3077e1', foundational, coordination_precedes_harmonization).
narrative_ontology:cs_axiom_status(coordination_precedes_harmonization, holdable).
narrative_ontology:cs_axiom_grounding('fa265e05-6e9b-4622-98d9-1c7d3a3077e1', coordination_precedes_harmonization, conventional).
narrative_ontology:cs_axiom('fa265e05-6e9b-4622-98d9-1c7d3a3077e1', foundational, social_dumping_prohibition_mandate).
narrative_ontology:cs_axiom_status(social_dumping_prohibition_mandate, holdable).
narrative_ontology:cs_axiom_grounding('fa265e05-6e9b-4622-98d9-1c7d3a3077e1', social_dumping_prohibition_mandate, instrumental).
narrative_ontology:cs_reference_frame('fa265e05-6e9b-4622-98d9-1c7d3a3077e1', coordinated_national_welfare_autonomy).
narrative_ontology:cs_drift_state('fa265e05-6e9b-4622-98d9-1c7d3a3077e1', post_enlargement_mobility_regime, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fa265e05-6e9b-4622-98d9-1c7d3a3077e1', '').
narrative_ontology:cs_kernel_id(federation_membership_kernel__welfare_coordination_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, member_state_governments).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, posting_employers).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, posted_workers).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, native_workers_receiving_states).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, sending_state_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enforces the coordination framework through anti-social-dumping directives, posted workers regulations, and infringement proceedings against member states that restrict cross-border service provision. Proposes legislation to rebalance mobility and social rights while defending the primacy of coordination over harmonization.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, eu_commission, agenda_setter,
    institutional, generational, arbitrage, continental).

% Retain formal autonomy over welfare system design, contribution rates, and benefit levels within the EU coordination architecture. Receive the structural benefit of non-harmonization while accepting EU oversight of cross-border situations. Constrained by Treaty obligations and ECJ jurisprudence from closing borders unilaterally.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, member_state_governments, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_kernel__welfare_coordination_reading, member_state_governments, agenda_setter).

% Deploy workers across borders under posting arrangements, avoiding host-state social contributions and undercutting local collective agreements. Capture the margin between regulated domestic labor costs and the reduced posted-worker cost base.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, posting_employers, beneficiary,
    powerful, biographical, mobile, continental).

% Work temporarily in host states under home-country terms, often at sub-local wages and excluded from host social insurance during posting periods. Dependent on the posting employer for contract continuity and residence status; limited access to host collective representation.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, posted_workers, payer,
    powerless, immediate, trapped, national).

% Face wage and job competition from posted workers who are exempt from host social levies and collective agreement coverage. Their unions' capacity to extend standards to posted workers is constrained by EU anti-social-dumping jurisprudence that treats some protective rules as obstacles to free movement.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, native_workers_receiving_states, payer,
    moderate, biographical, constrained, national).

% Lose working-age population and associated tax and social contribution bases to emigration without receiving compensatory fiscal transfers from receiving states. Remittances partially offset losses but do not fund public infrastructure or pension systems at scale.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, sending_state_governments, payer,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_kernel__welfare_coordination_reading, posting_employers).
narrative_ontology:fixing_cost_class(federation_membership_kernel__welfare_coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables cross-border labor mobility without requiring full supranational harmonization of welfare systems by coordinating social security aggregation, preserving diverse national architectures, and policing the boundary between legitimate mobility and social dumping.
% TRANSFER_FUNCTION: Moves labor cost advantages from host-state regulatory standards to posting employers; transfers mobility adjustment costs to posted workers (reduced coverage), native workers (competition), and sending states (fiscal drain); transfers regulatory autonomy to member state governments.
% ABSENT_VOICES: Supranational harmonization advocates who would prefer unified European welfare standards are marginalized in Council negotiations; posted workers themselves are underrepresented in host-state social dialogue; receiving state trade unions lack standing to bind posted employers to local collective agreements.
% DISAPPEARANCE_RATIONALE: If the welfare coordination framework vanished overnight, member states would face immediate welfare arbitrage pressures; posting employers would lose their cost advantage; labor markets would fragment along national benefit boundaries unless rapid harmonization or renationalization filled the gap.
% FOUNDING_PROBLEM: How to establish free movement of workers across diverse national welfare states without triggering a regulatory race to the bottom or dissolving national social solidarity.
% FOUNDING_PROBLEM_CORROBORATION: European Commission historical memoranda and Council regulations attest to the coordination founding intent. Critical political economists, sending-state ministries, and European Parliament petitions committees attest the arrangement has mutated toward cost-competition extraction; independent labor-law scholarship documents the shift from protective coordination to regulatory arbitrage.
narrative_ontology:disappearance_verdict(federation_membership_kernel__welfare_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_kernel__welfare_coordination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_kernel__welfare_coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership_kernel__welfare_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_kernel__welfare_coordination_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_kernel__welfare_coordination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_kernel__welfare_coordination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_kernel__welfare_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is substantial because the coordination framework permits systematic cost-competition posting: posted workers are exempt from host social levies and often from collective agreements, creating a structural wage-cost gap that employers capture. Suppression (0.55) is moderate-to-high because the constraint's persistence requires active EU enforcement against receiving state protective rules (Laval/Viking/RÃ¼ffert jurisprudence) and suppression of alternative harmonization proposals. Theater_ratio (0.40) reflects that anti-social-dumping rules and the 2014 Enforcement Directive are partly performative responses to political crises, masking continued extraction. Accessibility_collapse (0.45) captures that while supranational harmonization is technically imaginable, it is politically blocked by member state unanimity requirements. Resistance (0.50) reflects sustained trade union, sending-state, and some receiving-state governmental opposition.
 *
 * PERSPECTIVAL GAP:
 *   From the Commission and member state seats, the arrangement reads as necessary coordination preserving plural welfare regimes. From posted worker, native worker, and sending state seats, the same legal architecture reads as an extraction mechanism that socializes mobility costs onto immobile or weakly positioned actors while capital captures the arbitrage. The engine computes this divergence from the structural declarations; the authored claim (tangled_rope) encodes that both readings are structurally rooted.
 *
 * DIRECTIONALITY LOGIC:
 *   Member state governments and posting_employers sit near the beneficiary end: member states gain regulatory autonomy, posting_employers gain cost advantages. Posted_workers sit near the full-target end (powerless, trapped, paying through wage and coverage gaps). Native_workers_receiving_states are high-target (constrained exit from labor market competition). Sending_state_governments are moderate-target (institutional but constrained by free movement obligations). The EU Commission sits near symmetric to low-beneficiary: it gains institutional purpose from coordinating but does not directly collect extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is not a rope because asymmetric extraction is substantial and structurally necessary to the arrangement (posting relies on cost gaps). It is not a snare because the coordination function is genuine: without some mechanism to reconcile mobility with diverse welfare states, either free movement or welfare diversity would collapse. It is not a mountain because the arrangement is institutionally constructed and actively enforced. It is not a scaffold because it lacks a credible sunset clause â the coordination function is presented as permanent. It is not a piton because beneficiaries (member states, posting employers) continue to capture real value from the arrangement rather than maintaining it through mere inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    anti_social_dumping_efficacy,
    'Do EU anti-social-dumping rules effectively protect workers and receiving state labor markets, or do they create a veneer of protection while permitting substantial cost-competition posting?',
    'Comparative enforcement data on posting violations, wage-gap studies between posted and local workers in identical occupations, and post-2014 Enforcement Directive impact assessments.',
    'If the rules are largely performative, the constraint''s theater_ratio understates its extraction and its effective coordination function is weaker than claimed; if effective, the extraction is moderated by genuine protection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anti_social_dumping_efficacy, empirical, 'Whether anti-social-dumping enforcement is functional or theatrical').

omega_variable(
    member_state_autonomy_genuine,
    'Is member state welfare design autonomy genuinely preserved under this coordination regime, or is it progressively hollowed out by ECJ free movement jurisprudence and market pressure toward regulatory convergence?',
    'Longitudinal analysis of member state welfare differentiation indices correlated with ECJ case volume and posting flow intensity.',
    'If autonomy is illusory, the coordination function collapses toward extraction; if genuine, the beneficiary structure is validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(member_state_autonomy_genuine, empirical, 'Whether welfare autonomy is substantive or symbolic').

omega_variable(
    kernel_reading_boundary_stability,
    'Does the welfare_coordination_reading remain a stable, distinct framing of the free movement kernel, or does it collapse toward either the integration_reading (supranational expansion) or the member_sovereignty_reading (national closure) under jurisprudential or political pressure?',
    'Discourse analysis of Commission, Council, and ECJ texts to track whether coordination language is converging toward harmonization or renationalization.',
    'If unstable, this constraint''s classification may need to be merged with a sibling; if stable, the three-reading decomposition of the kernel is validated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary_stability, conceptual, 'Stability of the welfare coordination reading as distinct from its siblings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__welfare_coordination_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(welfare_coordination_tr_t0, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(welfare_coordination_tr_t10, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(welfare_coordination_tr_t20, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(welfare_coordination_tr_t30, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(welfare_coordination_tr_t40, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(welfare_coordination_tr_t50, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(welfare_coordination_be_t0, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(welfare_coordination_be_t10, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(welfare_coordination_be_t20, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(welfare_coordination_be_t30, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(welfare_coordination_be_t40, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 40, 0.6).
narrative_ontology:measurement(welfare_coordination_be_t50, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 50, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(welfare_coordination_su_t0, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(welfare_coordination_su_t10, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 10, 0.3).
narrative_ontology:measurement(welfare_coordination_su_t20, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(welfare_coordination_su_t30, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(welfare_coordination_su_t40, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement(welfare_coordination_su_t50, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 50, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(federation_membership_kernel__welfare_coordination_reading, integration_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__welfare_coordination_reading, member_sovereignty_reading).

% DUAL FORMULATION NOTE:
% The natural-language concept 'EU free movement of workers' decomposes into at least three structurally distinct constraints (kernel readings): integration_reading (supranational expansion), member_sovereignty_reading (national boundedness), and this welfare_coordination_reading (intergovernmental coordination with anti-social-dumping). Each reading carries a distinct epsilon, beneficiary/victim topology, and stakeholder configuration. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
