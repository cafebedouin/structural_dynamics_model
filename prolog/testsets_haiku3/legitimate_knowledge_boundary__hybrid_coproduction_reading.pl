% ============================================================================
% CONSTRAINT STORY: legitimate_knowledge_boundary__hybrid_coproduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_knowledge_boundary__hybrid_coproduction_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: legitimate_knowledge_boundary__hybrid_coproduction_reading
 *   human_readable: Hybrid Co-production Epistemic Authority
 *   domain: epistemology/science_and_technology_studies
 *
 * SUMMARY:
 *   This constraint instantiates a hybrid co-production reading of the
 *   legitimate-knowledge-boundary kernel. The claim: legitimate knowledge
 *   requires BOTH methodological rigor (validated through peer review,
 *   experimental design, statistical testing) AND experiential validity
 *   (grounded in lived experience, community knowledge, situated
 *   understanding). Neither alone is sufficient; knowledge is legitimate only
 *   when both are integrated through co-production processes involving both
 *   methodological experts and experiential knowledge holders in joint
 *   inquiry. This reading requires dual validation infrastructure, formal
 *   integration procedures, and active enforcement of both standards — making
 *   it a tangled rope: genuine coordination problem solved (how do you
 *   validate knowledge that emerges from both systematic inquiry and lived
 *   experience?), but also asymmetric extraction (the institutional benefits
 *   accrue to research infrastructure operators and credentialed researchers;
 *   experiential knowledge holders bear the cost of participation and
 *   boundary negotiation). The measurement series show rising extraction and
 *   theater ratio over time, suggesting increasing performative maintenance
 *   of the co-production frame as actual power asymmetries ossify.
 *
 * KEY AGENTS:
 *   - co_production_infrastructure_operators (institutional/powerful): set research agendas, design validation procedures, control publication, define what counts as 'rigorous' and 'valid'
 *   - methodologically_trained_researchers (institutional/powerful): implement validation procedures, possess credentialed authority, profit from publication in peer-review venues
 *   - experiential_knowledge_holders (powerless-to-moderate/trapped): bring lived experience, must learn research languages and institutional procedures to participate, bear unpaid labor costs of co-production
 *   - under_credentialed_practitioners (moderate/constrained): possess relevant knowledge but lack formal credentials, face barriers to legitimacy without institutional sponsorship
 *   - disciplinary gatekeepers (institutional/analytical): adjudicate what counts as methodologically rigorous, control peer-review venues and funding
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.68).
domain_priors:suppression_score(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.72).
domain_priors:theater_ratio(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_knowledge_boundary__hybrid_coproduction_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_knowledge_boundary__hybrid_coproduction_reading, "Hybrid Co-production Epistemic Authority").
narrative_ontology:topic_domain(legitimate_knowledge_boundary__hybrid_coproduction_reading, "epistemology/science_and_technology_studies").

domain_priors:requires_active_enforcement(legitimate_knowledge_boundary__hybrid_coproduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_knowledge_boundary__hybrid_coproduction_reading, '9f9f122a-26c1-4271-9062-1c6ee3e7b04b').
narrative_ontology:cs_kernel_codification('9f9f122a-26c1-4271-9062-1c6ee3e7b04b', distributed).
narrative_ontology:cs_authority_grounding('9f9f122a-26c1-4271-9062-1c6ee3e7b04b', extraction).
narrative_ontology:cs_interpretation_layer_present('9f9f122a-26c1-4271-9062-1c6ee3e7b04b').
narrative_ontology:cs_reading_relation('9f9f122a-26c1-4271-9062-1c6ee3e7b04b', legitimate_knowledge_boundary__credentialed_expertise_reading, coexists_with).
narrative_ontology:cs_reading_relation('9f9f122a-26c1-4271-9062-1c6ee3e7b04b', legitimate_knowledge_boundary__experiential_pluralism_reading, coexists_with).
narrative_ontology:cs_axiom('9f9f122a-26c1-4271-9062-1c6ee3e7b04b', foundational, dual_validation_requirement).
narrative_ontology:cs_axiom_status(dual_validation_requirement, holdable).
narrative_ontology:cs_axiom_grounding('9f9f122a-26c1-4271-9062-1c6ee3e7b04b', dual_validation_requirement, deontological).
narrative_ontology:cs_axiom('9f9f122a-26c1-4271-9062-1c6ee3e7b04b', secondary, co_production_power_sharing).
narrative_ontology:cs_axiom_status(co_production_power_sharing, holdable).
narrative_ontology:cs_axiom_grounding('9f9f122a-26c1-4271-9062-1c6ee3e7b04b', co_production_power_sharing, deontological).
narrative_ontology:cs_reference_frame('9f9f122a-26c1-4271-9062-1c6ee3e7b04b', integrated_epistemic_pluralism).
narrative_ontology:cs_drift_state('9f9f122a-26c1-4271-9062-1c6ee3e7b04b', contemporary_higher_education_and_research, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9f9f122a-26c1-4271-9062-1c6ee3e7b04b', '').
narrative_ontology:cs_kernel_id(legitimate_knowledge_boundary__hybrid_coproduction_reading, legitimate_knowledge_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, co_production_infrastructure_operators).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, methodologically_trained_researchers).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, experiential_knowledge_holders).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, under_credentialed_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, methodologically_trained_researchers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Universities, research institutes, and funding bodies that set up co-production programs, design the procedures that define what counts as 'rigorous' and 'valid', control publication channels, and determine which communities are invited to participate. They frame co-production as decolonization and democratization while retaining control over methodology definition, validation procedures, and dissemination. They benefit institutionally through increased legitimacy, access to research sites and data, and partnerships with communities that enhance funding competitiveness.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, co_production_infrastructure_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% PhD-credentialed researchers, statisticians, and methodologists who implement and adjudicate the rigor standards. They benefit from co-production framing (it legitimizes their work, expands their research questions, provides access to sites they couldn't reach alone) while retaining gatekeeping power. They pay a moderate cost in time spent on translation, community engagement, and negotiating between standards, but control the final definition of 'rigorous' methodology.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, methodologically_trained_researchers, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__hybrid_coproduction_reading, methodologically_trained_researchers, payer).

% Community members, patients, workers, Indigenous peoples, and others whose lived experience is the object of study and whose knowledge is claimed to be validated through co-production. They participate unpaid or underpaid, learn institutional research languages, submit their knowledge to validation procedures they did not design, and often see their insights published under researcher names. Their identity is fused with the knowledge domain (a person living with chronic illness cannot separate their experiential knowledge from who they are), making exit costly. They experience the constraint as mandatory for gaining institutional legitimacy for their knowledge.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, experiential_knowledge_holders, payer,
    powerless, biographical, identity_locked, local).

% Practitioners with deep experiential expertise but no formal academic credentials (community health workers, traditional healers, craft experts, experienced practitioners). They hold knowledge the co-production framework claims to value, but must either gain credentials (costly, time-consuming) or participate through institutional gatekeepers who mediate their knowledge. Their exit options are constrained: they can operate outside institutions (losing legitimacy and access to resources) or participate within structures designed by credentialed experts.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, under_credentialed_practitioners, payer,
    moderate, biographical, constrained, regional).

% People whose experience is relevant but who are not formally invited into co-production processes: the most marginalized, those without institutional connections, those whose knowledge contradicts institutional assumptions. The co-production frame does not eliminate their exclusion; it makes exclusion selective, determining which voices get incorporated and which remain outside.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, excluded_non_credentialed_voices, excluded,
    powerless, immediate, trapped, local).

% Journal editors, funding agencies, academic departments, and professional societies that adjudicate what counts as legitimate knowledge. They set the peer-review standards, evaluate whether co-production counts as 'real research', determine which methodologies are recognized, and decide whether experiential validity is genuinely integrated or merely performative. They have power to enforce standards but are increasingly pressured to include community voices.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, disciplinary_gatekeepers, agenda_setter,
    institutional, generational, analytical, global).

% Scholars critically examining the legitimacy-knowledge boundary from perspectives of colonialism, epistemic injustice, and power analysis. They observe and analyze the co-production constraint, asking whether it genuinely decolonizes knowledge or re-colonizes it by requiring communities to speak in institutional languages. They take testimony from other seats and provide critical analysis that often contradicts the beneficiary parties' narratives.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, decolonial_scholars, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_knowledge_boundary__hybrid_coproduction_reading, co_production_infrastructure_operators).
narrative_ontology:fixing_cost_class(legitimate_knowledge_boundary__hybrid_coproduction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of how to validate knowledge that emerges from both systematic methodological inquiry and lived experience. Provides procedures for integrating standards that historically excluded each other: methodological rigor and community knowledge validation. Creates institutional space for knowledge forms that neither pure expertise nor pure pluralism can adjudicate alone.
% TRANSFER_FUNCTION: Transfers legitimacy, institutional resources, publication opportunities, and research authority from credentialed researchers and institutions to experiential knowledge holders — in theory. In practice, transfers experiential knowledge and community sites/data to researchers while conferring partial legitimacy on community knowledge only when validated through institutional procedures. Moves unpaid labor from communities into research infrastructure.
% ABSENT_VOICES: Those excluded from co-production processes: the most marginalized, those without institutional connections, those whose knowledge fundamentally contradicts the institution's assumptions. Also absent: voices from future generations who will bear the cost of knowledge legitimized through a procedure that requires institutional participation. The co-production frame selects which voices are incorporated and which remain excluded.
% DISAPPEARANCE_RATIONALE: If the co-production requirement disappeared, institutional epistemology would shift: either back toward credentialed-expertise dominance (pure methodological rigor, communities excluded) or toward experiential pluralism (diverse validation standards, less institutional gatekeeping). Research agendas would change — questions generated in community partnership would lose funding and institutional support. Communities would lose the institutional validation channel and would need to develop parallel legitimacy structures. The knowledge landscape would reorganize around whichever reading of the legitimate-knowledge-boundary kernel became institutionally dominant.
% FOUNDING_PROBLEM: How can research honor both systematic methodological rigor and the validity of knowledge that emerges from lived experience and community practice? How can knowledge forms historically excluded from institutional legitimacy be validated without requiring them to conform entirely to methodological standards developed without their input? How can decolonial scholarship genuinely center community knowledge without reproducing extractive research relationships?
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by decolonial scholars, community-based researchers, and STS scholars who document the epistemic injustice of excluding experiential knowledge from institutional legitimacy. It is also contested: credentialed-expertise-reading defenders argue the founding problem is a category error (experiential knowledge is observation, not rigorous inquiry) and that co-production compromises standards; experiential-pluralism defenders argue the founding problem assumes methodological rigor is necessary at all. The evidence is mixed: some co-production programs report genuine integration and enhanced research quality; others document appropriation and extraction despite co-production framing. No corroborating evidence exists from parties outside the co-production infrastructure itself that the founding problem is being solved rather than theatrically staged.
narrative_ontology:disappearance_verdict(legitimate_knowledge_boundary__hybrid_coproduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_knowledge_boundary__hybrid_coproduction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_knowledge_boundary__hybrid_coproduction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimate_knowledge_boundary__hybrid_coproduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_knowledge_boundary__hybrid_coproduction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_knowledge_boundary__hybrid_coproduction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_knowledge_boundary__hybrid_coproduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 final) because the arrangement requires experiential knowledge holders to participate in institutional validation procedures designed by methodological experts — the cost of legitimacy is borne by the less-powered seat. Theater is moderate (0.41) because a real coordination function exists (how do you honor both standards?), but the growth of theater over time (from 0.22 to 0.41) indicates increasing performative emphasis on 'co-production' language while actual power distribution remains asymmetric. Suppression is high (0.72) because the constraint requires both standards to be enforced — experiential knowledge is suppressed when it contradicts methodological rigor, and methodological rigor is suppressed when it dismisses community knowledge as 'anecdotal'; the suppression is mutual but not symmetrical (the institutional apparatus has more power to enforce methodological standards than communities have to enforce experiential validity). Accessibility collapse is moderate (0.58) because alternatives (pure experiential pluralism, pure credentialed expertise) remain theoretically articulated and have institutional defenders; participants can exit toward either pole, though institutional incentive structures make co-production attractive.
 *
 * PERSPECTIVAL GAP:
 *   The infrastructure-operator seat and the experiential-knowledge-holder seat compute dramatically differently. From the operator's position the arrangement is genuine solution to a real problem: how to honor both rigor and lived experience, create space for marginalized knowledge, decolonize research. From the experiential-knowledge holder's position the same structure operates as co-option: you must learn our methods, accept our validation procedures, participate in our grant cycles, publish in our venues, or your knowledge remains illegitimate. The engine computes this from the structural data — victims list, trapped/constrained exit, institutional power asymmetry — and surfaces the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Infrastructure operators and credentialed researchers benefit structurally: they gain legitimacy for their work by partnering with communities (institutional prestige, access to research sites, narrative of decolonization), they control the procedures that define what 'co-production' means, they own the publication channels. Exit options are high (arbitrage: they can shift to pure methodological rigor if co-production becomes burdensome). Experiential knowledge holders are the targets: they must participate to gain legitimacy, they bear unpaid labor, they must learn institutional languages, their contributions are often appropriated and republished under researcher names. Exit is trapped or identity-locked (the knowledge is tied to who they are; leaving means abandoning the frame that legitimizes their expertise). The measured directionality favors the operators substantially.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how to validate knowledge from both systematic inquiry and lived experience — is live and contested. Some argue it is solved by the co-production procedures (mandatrophy_status=live); others argue the procedures have become theatrical maintenance of a hierarchy that retains methodological supremacy (mandatrophy_status=dead). The constraint meets the tangled-rope gate: genuine coordination (both standards matter, integration is non-trivial) AND asymmetric extraction (one seat benefits structurally, the other bears cost). The theater-ratio growth suggests the mandatrophy risk: as institutional adoption of 'co-production' language increases without corresponding shifts in power and resource distribution, the constraint risks drifting toward piton (mostly performative, actual function atrophied, but maintained because no party is motivated to fix it).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    co_production_power_asymmetry,
    'Does the co-production requirement genuinely equalize epistemic authority between methodological experts and experiential knowledge holders, or does it embed methodological validation as the final gate, leaving experiential knowledge structurally subordinate?',
    'Ethnographic study of actual co-production process control: who sets research questions, who designs methodology, whose findings override the other''s when results conflict, who publishes first and in what venues. Track decision authority, not nominal inclusion.',
    'If experiential holders control research design and methodology equally, extraction is minimal and the constraint is genuine rope. If methodological experts retain veto power over questions and interpretation, the constraint embeds methodological supremacy with experiential cover — reclassification to snare with theatrical co-production staging.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(co_production_power_asymmetry, empirical, 'Whether co-production distributes epistemic authority or concentrates it under methodological control.').

omega_variable(
    reading_boundary_kernel_contest,
    'Is this reading instantiating a structurally coherent third position (genuine integration of standards), or is it a unstable middle ground that collapses toward credentialed_expertise_reading when validation conflicts arise?',
    'Historical analysis of hybrid programs that face cases where methodological rigor and experiential validity contradict: do institutions develop principled integration frameworks, or do they resolve conflicts by deferring to methodological standards as the tie-breaker?',
    'Stable integration frameworks (principled, documented conflict-resolution procedures that do not always favor methodology) support the co-production reading as genuinely coherent. Consistent pattern of deferring to methodology under conflict would indicate the reading is a transitional staging toward credentialed_expertise dominance — a bridge rather than a stable equilibrium.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_kernel_contest, conceptual, 'Whether hybrid co-production is a stable epistemic commitment or an unstable intermediate state collapsing toward expertise dominance.').

omega_variable(
    infrastructure_cost_distribution,
    'Who bears the cost of building and maintaining the co-production infrastructure (research ethics review, participatory design, iterative validation, translation of findings across disciplines and communities)?',
    'Budget analysis of actual co-production programs: institutional investment, researcher time allocation, community capacity-building cost, revenue-generating activities that subsidize the program.',
    'If infrastructure costs are borne primarily by the institution and researchers, the program may be sustainable but dependent on external funding — extraction may be masked by institutional subsidy. If costs are passed to experiential knowledge holders (unpaid participation, opportunity cost of involvement), the extraction increases substantially and the co-production frame becomes a mechanism for appropriating volunteer labor.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(infrastructure_cost_distribution, empirical, 'Whether co-production infrastructure cost is borne by the institution or shifted to community knowledge holders.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(legi_tr_t5, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(legi_tr_t10, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 10, 0.33).
narrative_ontology:measurement(legi_tr_t15, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement(legi_tr_t20, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(legi_tr_t25, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 25, 0.41).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(legi_be_t5, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(legi_be_t10, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(legi_be_t15, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(legi_be_t20, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(legi_be_t25, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(legi_su_t5, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(legi_su_t10, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 10, 0.67).
narrative_ontology:measurement(legi_su_t15, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(legi_su_t20, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(legi_su_t25, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_knowledge_boundary__hybrid_coproduction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.18).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__hybrid_coproduction_reading, legitimate_knowledge_boundary__credentialed_expertise_reading).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__hybrid_coproduction_reading, legitimate_knowledge_boundary__experiential_pluralism_reading).

% DUAL FORMULATION NOTE:
% This is one of three readings of the legitimate-knowledge-boundary kernel. All three stories are linked via network.affects_constraints. The three readings have different epsilon values (different referents: what is being assessed as extractive varies by reading) and different victim structures. ε is reading-indexed. The kernel is the unresolved question; the readings are the three live answers in contemporary STS and epistemology discourse. The corpus should include all three stories to track how a single conceptual domain instantiates multiple structurally distinct constraints depending on which reading one adopts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
