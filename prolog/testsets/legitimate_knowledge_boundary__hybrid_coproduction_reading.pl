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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: legitimate_knowledge_boundary__hybrid_coproduction_reading
 *   human_readable: Hybrid Co-Production Knowledge Legitimation
 *   domain: epistemology/science_and_technology_studies
 *
 * SUMMARY:
 *   The legitimate_knowledge_boundary kernel represents a contested claim
 *   about what makes knowledge valid and who gets to decide. This story
 *   instantiates the hybrid_coproduction_reading: the claim that legitimate
 *   knowledge requires both methodological rigor AND experiential validity,
 *   integrated through formalized co-production processes where credentialed
 *   researchers and community knowledge practitioners jointly design
 *   research, validate findings, and determine what counts as knowledge. This
 *   reading distinguishes itself from pure credentialism (methodology alone)
 *   and from experiential pluralism (lived experience as sufficient). It
 *   operates as a tangled rope: it coordinates genuinely excluded knowledge
 *   into institutional legitimacy processes, but requires active enforcement
 *   to maintain dual validation standards, and distributes extraction
 *   asymmetrically — benefiting community knowledge practitioners through
 *   institutional recognition while extracting their unpaid labor and
 *   imposing translation demands.
 *
 * KEY AGENTS:
 *   - Disciplinary gatekeepers (institutional agenda-setter): historically monopolized epistemic legitimacy via peer review; now required to share adjudication with community partners and fund co-production infrastructure
 *   - Community knowledge practitioners (organized beneficiary/payer): gain epistemic legitimacy and research access but must participate in lengthy co-production processes, often unpaid, and learn credentialing language not native to their practice
 *   - Marginalized experiential communities (powerless beneficiary/payer, identity-locked): direct experience holders (patients, residents, marginalized groups) whose knowledge is elevated but extraction burden is highest — they must translate lived knowledge into research-legible forms without institutional power
 *   - Applied research institutions (institutional beneficiary): gain legitimacy and funding access from co-production requirement but carry infrastructure costs
 *   - Co-production administrators (institutional agenda-setter + beneficiary): operationalize the mandate and gain authority, but mediate incommensurable validity standards and often reproduce gatekeeping under a new name
 *   - Positivist methodology defenders (powerful payer): methodological authority is retained but supplemented; disciplinary power is constrained though not eliminated
 *   - Excluded pure disciplinarians (powerful, excluded): institutional researchers who view co-production as corrupting rigor are structurally excluded from legitimacy under this reading's framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.42).
domain_priors:suppression_score(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.38).
domain_priors:theater_ratio(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_knowledge_boundary__hybrid_coproduction_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_knowledge_boundary__hybrid_coproduction_reading, "Hybrid Co-Production Knowledge Legitimation").
narrative_ontology:topic_domain(legitimate_knowledge_boundary__hybrid_coproduction_reading, "epistemology/science_and_technology_studies").

domain_priors:requires_active_enforcement(legitimate_knowledge_boundary__hybrid_coproduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_knowledge_boundary__hybrid_coproduction_reading, '3bcfd109-0365-4fb5-ac3a-b5ead2589875').
narrative_ontology:cs_kernel_codification('3bcfd109-0365-4fb5-ac3a-b5ead2589875', distributed).
narrative_ontology:cs_authority_grounding('3bcfd109-0365-4fb5-ac3a-b5ead2589875', distributed).
narrative_ontology:cs_reading_relation('3bcfd109-0365-4fb5-ac3a-b5ead2589875', legitimate_knowledge_boundary__credentialed_expertise_reading, influences).
narrative_ontology:cs_reading_relation('3bcfd109-0365-4fb5-ac3a-b5ead2589875', legitimate_knowledge_boundary__experiential_pluralism_reading, influences).
narrative_ontology:cs_axiom('3bcfd109-0365-4fb5-ac3a-b5ead2589875', foundational, both_methods_and_experience_required_for_legitimacy).
narrative_ontology:cs_axiom_status(both_methods_and_experience_required_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('3bcfd109-0365-4fb5-ac3a-b5ead2589875', both_methods_and_experience_required_for_legitimacy, deontological).
narrative_ontology:cs_axiom('3bcfd109-0365-4fb5-ac3a-b5ead2589875', foundational, gatekeeping_power_must_be_shared_in_knowledge_adjudication).
narrative_ontology:cs_axiom_status(gatekeeping_power_must_be_shared_in_knowledge_adjudication, holdable).
narrative_ontology:cs_axiom_grounding('3bcfd109-0365-4fb5-ac3a-b5ead2589875', gatekeeping_power_must_be_shared_in_knowledge_adjudication, conventional).
narrative_ontology:cs_reference_frame('3bcfd109-0365-4fb5-ac3a-b5ead2589875', excluded_knowledge_integration_framework).
narrative_ontology:cs_drift_state('3bcfd109-0365-4fb5-ac3a-b5ead2589875', contemporary_institutional_adoption, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3bcfd109-0365-4fb5-ac3a-b5ead2589875', '').
narrative_ontology:cs_kernel_id(legitimate_knowledge_boundary__hybrid_coproduction_reading, legitimate_knowledge_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, community_knowledge_practitioners).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, marginalized_experiential_communities).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, applied_research_institutions).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, disciplinary_gatekeepers).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, academic_credentialing_bodies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, co_production_administrators).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, community_knowledge_practitioners).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, marginalized_experiential_communities).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, positivist_methodology_defenders).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__hybrid_coproduction_reading, epistemic_justice_doctrine).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__hybrid_coproduction_reading, standpoint_epistemology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Academic peer-review bodies, journal editors, and credentialed disciplinary authorities that have historically adjudicated legitimate knowledge claims. Under the hybrid co-production reading, they must now integrate experiential validation into their review and publication criteria, share adjudicatory power with community representatives, and fund co-production infrastructure. This resets their monopoly on epistemic legitimacy while expanding what they must credential and defend.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, disciplinary_gatekeepers, agenda_setter,
    institutional, generational, constrained, global).

% Indigenous knowledge keepers, community health workers, participatory action researchers, and experiential experts who generate knowledge through lived practice. Under this reading, their knowledge is elevated from anecdotal to legitimate, but only when integrated with methodological validation. They must learn credentialing language, participate in lengthy co-production processes, and often work without compensation in knowledge generation processes designed by others.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, community_knowledge_practitioners, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__hybrid_coproduction_reading, community_knowledge_practitioners, payer).

% Populations experiencing a phenomenon directly — patients in health systems, residents in polluted neighborhoods, people navigating discriminatory institutions — whose experiential knowledge is epistemically valuable but has been systematically excluded. Co-production offers recognition and a voice in research design, but requires them to translate lived knowledge into research-legible forms, negotiate with credentialed researchers, and navigate institutional processes they do not control.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, marginalized_experiential_communities, beneficiary,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__hybrid_coproduction_reading, marginalized_experiential_communities, payer).

% Universities, NGOs, and research centers adopting co-production methodologies gain legitimacy, funding, and access to situated knowledge resources. They carry infrastructure costs and must develop new evaluation criteria, but their research output is presented as more rigorous and socially grounded. Some experience genuine benefit; others perform co-production theatrically while preserving disciplinary gatekeeping.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, applied_research_institutions, beneficiary,
    institutional, generational, mobile, national).

% Researchers and philosophers of science who argue that methodological rigor (randomized controls, statistical significance, quantifiable reproducibility) is the only legitimate epistemic standard. Under co-production reading, their methodological authority is retained but supplemented; they must accommodate incommensurable validity standards and may see their grant portfolios shift toward co-production requirements.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, positivist_methodology_defenders, payer,
    powerful, biographical, mobile, global).

% Program officers, research administrators, and policy brokers who operationalize co-production mandates: design partnership agreements, manage community-researcher relationships, adjudicate disputes over knowledge ownership and validity. They gain authority but carry coordination burden and often mediate incommensurable claims about what counts as valid knowledge.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, co_production_administrators, agenda_setter,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__hybrid_coproduction_reading, co_production_administrators, beneficiary).

% Researchers and institutions that view co-production as corrupting disciplinary purity and rigor. They argue the constraint introduces subjective, non-reproducible elements into knowledge production. Their position is structurally excluded from the hybrid reading's legitimacy framework — co-production is presented as enhancing rather than compromising rigor.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, excluded_pure_disciplinarians, excluded,
    powerful, biographical, mobile, global).

% Science studies scholars, epistemology analysts, and policy research communities documenting how the co-production reading operates: whether it genuinely integrates power or reproduces gatekeeping dynamics within a new form; whether it produces legitimacy gain for marginalized knowledge or instrumental co-optation.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, analytical_observer, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_knowledge_boundary__hybrid_coproduction_reading, disciplinary_gatekeepers).
narrative_ontology:fixing_cost_class(legitimate_knowledge_boundary__hybrid_coproduction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the epistemic legitimacy crisis for knowledge generated outside credentialing institutions by creating a formal mechanism for integration: both methodological rigor AND experiential validity are required, and neither is sufficient alone. Coordinates disciplinary gatekeepers and community knowledge practitioners into a shared framework for knowledge adjudication.
% TRANSFER_FUNCTION: Moves epistemic authority and research resources from credentialed disciplinary authorities toward co-production infrastructure and community-embedded knowledge practitioners. Also moves labor burden (unpaid or underpaid participation in knowledge generation) from community practitioners to credentialed researchers and funders.
% ABSENT_VOICES: Pure disciplinarians (those who see co-production as corrupting rigor) are structurally excluded from legitimacy within this reading's framework. Knowledge systems that resist translation into methodological or experiential forms (e.g., tacit craft knowledge, spiritual/cosmological systems that reject the research framing) are marginalized. Communities that view co-production as colonial appropriation of knowledge rather than genuine partnership are not positioned to adjudicate the reading itself.
% DISAPPEARANCE_RATIONALE: If co-production requirements vanished, disciplinary gatekeeping would consolidate further, community knowledge would revert to anecdotal status, and research institutions would no longer be required to fund participatory infrastructure. Knowledge institutions would reorganize around credentialing authority alone; experiential knowledge would be decoupled from institutional legitimacy.
% FOUNDING_PROBLEM: Historical exclusion of non-credentialed knowledge from epistemic legitimacy despite demonstrated validity and social relevance: indigenous environmental knowledge dismissed as unscientific, patient experience excluded from health research, community knowledge of discrimination treated as anecdotal, frontline worker expertise treated as non-expert.
% FOUNDING_PROBLEM_CORROBORATION: Science and technology studies scholarship (Jasanoff, Wynne, Turnbull) documents this exclusion extensively. Community-based participatory research evaluations and indigenous research sovereignty movements attest the founding problem remains active. Disciplinary gatekeepers acknowledge it through funding requirements for community engagement, though they often dispute the co-production reading's solution and frame engagement as consultation rather than partnership.
narrative_ontology:disappearance_verdict(legitimate_knowledge_boundary__hybrid_coproduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_knowledge_boundary__hybrid_coproduction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_knowledge_boundary__hybrid_coproduction_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(legitimate_knowledge_boundary__hybrid_coproduction_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_knowledge_boundary__hybrid_coproduction_reading_tests).
:- end_tests(legitimate_knowledge_boundary__hybrid_coproduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) because the constraint coordinates genuine knowledge sharing but requires asymmetric labor from experientially-grounded communities. The translation burden (converting lived knowledge into methodological forms) and uncompensated participation time constitute extraction, but the epistemic inclusion gain partially offsets it — not all payers are net losers. Suppression is moderate (0.38) because enforcement is needed to maintain dual-validation requirements against both pure credentialists who resist experiential validation and communities that view co-production as a colonizing research framing. Theater is low-moderate (0.28) and rising: genuine co-production work happens early (t=0–10), but as the constraint institutionalizes, theater increases (t=10–25) — performance includes 'community engagement' without genuine power-sharing, administrators cite co-production in grant proposals without restructuring adjudicatory power, and the infrastructure cost is presented as commitment when it might be gatekeeping multiplication. Accessibility collapse is moderate (0.65): alternatives exist (pure credentialism, reject institutional legitimacy entirely, create parallel knowledge systems) but are constrained by funding, career incentives, and the rising institutional expectation of co-production. Resistance is substantial (0.58): positivist researchers resist the constraint; some communities resist co-production as research colonialism; pure disciplinarians maintain that only methods are rigorous. The measurement series on a shared grid show extractiveness rising and stabilizing (initial coordination work transitioning to infrastructure maintenance), theater rising as institutional routinization kicks in, and suppression requirement remaining stable (enforcement burden consistent across the interval). The claim/metric independence is preserved: this is CLAIMED as tangled rope (genuine coordination plus extraction) while the metrics describe substantially extractive operation — the divergence between claim and metrics is exactly what the engine measures to detect whether the integration is real or performative.
 *
 * PERSPECTIVAL GAP:
 *   The disciplinary gatekeeper and co-production administrator seats should compute as beneficiaries with moderate directionality (they retain and extend authority, managing knowledge adjudication). Community practitioners should compute as targets with high directionality (they contribute unpaid labor, face translation demands, and are dependent on institutional processes they do not control). Marginalized experiential communities have the highest directionality (identity-locked, powerless, carry the heaviest translation and justification burden). The engine computes seat-specific types from this divergent structural situation: from the gatekeeper seat this may appear as rope (coordination gain, modest cost-sharing); from the community practitioner seat it appears as tangled rope (coordination access, substantial extraction); from the marginalized community seat it may compute as snare (inclusion framed as partnership, extraction of labor and knowledge framing sovereignty). The perspectival divergence is not an error — it is the core phenomenon: the same constraint operates differently depending on where you sit in the knowledge hierarchy.
 *
 * DIRECTIONALITY LOGIC:
 *   Disciplinary gatekeepers derive d ≈ 0.25–0.35 (powerful institutional position, beneficiary role, mobile exit): they face costs (infrastructure investment, review time, methodological boundary expansion) but retain control and gain legitimacy from co-production requirement. Community knowledge practitioners derive d ≈ 0.65–0.75 (organized but constrained, payer + beneficiary hybrid, constrained exit): they gain epistemic recognition and research access (beneficiary) but contribute unpaid or underpaid labor and must learn institutional language (payer). Marginalized experiential communities derive d ≈ 0.8–0.9 (powerless, identity-locked): they are the knowledge subjects, contribute essential labor, carry the identity-translation burden, and have the least capacity to exit if the process becomes extractive. No directionality overrides are needed; the structural derivation captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids misclassification as pure coordination or pure extraction because it clearly establishes both: the coordination function (integrating excluded knowledge into institutional legitimacy) and the asymmetric extraction (unpaid community labor, translation demands, institutional power asymmetry in adjudication). Declaring it tangled rope is accurate to the intentional design: the constraint is supposed to integrate, and it does integrate, but the integration carries substantial labor costs on those being integrated. The risk is in the institution performing co-production theatrically while preserving the old gatekeeping: in that case, the constraint becomes a snare (community participation theater without power-sharing, extraction without genuine coordination). The measurement series captures this risk: theater_ratio rising t=15–25 suggests increasing performative co-production. The omega on integration_or_subordination directly addresses whether the constraint is what it claims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    integration_or_subordination,
    'Does the co-production constraint genuinely integrate methodological and experiential validation as equals, or does it subordinate experiential knowledge to methodological requirements (making experiential knowledge ''readable'' only when translated into methodological forms)?',
    'Comparative analysis of knowledge claims recognized under co-production: if experiential claims succeed without methodological translation, integration is genuine; if all legitimated claims require methodological restatement, subordination is evident.',
    'If integration is genuine, the constraint reduces extraction and operates as tangled rope with genuine coordination. If subordination is real, the constraint is a snare disguised as coordination — community labor translating knowledge into methodological forms with no reciprocal translation from methodologists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(integration_or_subordination, empirical, 'Whether co-production requires asymmetric translation of knowledge forms.').

omega_variable(
    gatekeeping_dissolution_or_multiplication,
    'Does co-production dissolve the gatekeeping function of credentialed disciplinary authority, or create a second gatekeeper role (co-production administrator) that multiplies gatekeeping without removing the original?',
    'Institutional analysis of power dynamics in co-production partnerships: if decision-making authority shifts to partnerships, original gatekeeping diminishes; if administrators mediate all partnerships and disciplinary authority remains intact, gatekeeping multiplies.',
    'If dissolved, extraction decreases substantially. If multiplied, the constraint becomes a more complex tangled rope or snare — two legitimacy gates instead of one, with community practitioners passing through both.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeping_dissolution_or_multiplication, empirical, 'Whether co-production eliminates or reproduces epistemic gatekeeping.').

omega_variable(
    identity_lock_durability,
    'For marginalized experiential communities (identity_locked exit), does participation in co-production processes increase or decrease capacity to exit if the process becomes extractive?',
    'Longitudinal tracking of community practitioner career trajectories: if co-production participation increases funding access and institutional mobility, identity-lock weakens; if it increases dependence on research institutions, identity-lock strengthens.',
    'If identity-lock strengthens, suppression increases and the constraint becomes more extractive. The constraint''s sustainability depends on managing this dynamic — too much dependence triggers exit (resistance grows); too much exit opportunity makes the constraint unnecessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_durability, empirical, 'Whether co-production strengthens or weakens identity-lock for epistemically marginalized communities.').

omega_variable(
    committer_axiomatic_compatibility,
    'Can the co-production reading and the credentialed_expertise_reading coexist within a single legitimate framework, or does the co-production reading''s axiom (both methods and experience are required) logically foreclose the expertise reading''s axiom (methods are sufficient)?',
    'Philosophical analysis of the readings'' core premises: if expertise reading claims sufficiency (''methods are sufficient for legitimacy'') and co-production claims necessity of both (''methods alone are insufficient''), the readings logically contradict within a single framework. If expertise reading claims only that methods are valid (not the only path), the readings coexist.',
    'If logically foreclosing, the relation is ''forecloses'' and the reading structure is binary (co-production and experiential_pluralism are compatible siblings; expertise reading is foreclosed by either). If compatible, relation is ''influences'' (co-production constrains expertise reading but does not eliminate it) and all three readings can coexist as live positions held by different parties.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_axiomatic_compatibility, conceptual, 'Logical compatibility of the three kernel readings'' foundational axioms.').

omega_variable(
    infrastructure_investment_asymmetry,
    'Does co-production require greater institutional infrastructure investment from disciplinary gatekeepers (partnership offices, administrator roles, new review criteria) than from community practitioners (whose time and knowledge are often contributed unpaid)?',
    'Audit of co-production program budgets: track institutional investment (staff, facilities, administrative overhead) versus community practitioner compensation. If gatekeepers invest in infrastructure while practitioners invest unpaid labor, asymmetry is confirmed.',
    'Confirmed asymmetry indicates the constraint extracts community labor while claiming integration. This would strengthen the case for reclassifying from tangled_rope to snare if asymmetry is severe.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(infrastructure_investment_asymmetry, empirical, 'Whether co-production investment burden falls asymmetrically on community practitioners.').

omega_variable(
    sibling_reading_empirical_dominance,
    'What empirical conditions would demonstrate that one of the sibling readings (credentialed_expertise_reading or experiential_pluralism_reading) more accurately describes how knowledge legitimacy actually operates in contemporary institutions?',
    'Large-scale institutional analysis of acceptance rates for knowledge claims under each reading: if credentialed methods alone are sufficient for legitimacy in most contexts (contra co-production mandate), expertise reading is empirically superior; if experience-only claims achieve legitimacy without methods, pluralism reading is superior.',
    'Empirical dominance of a sibling reading would indicate the co-production reading is a contested policy proposal rather than a description of actual epistemic practice. This reclassifies the constraint as a scaffold (transitional policy) rather than a tangled rope (structural integration).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_empirical_dominance, empirical, 'Which kernel reading currently dominates in institutional practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(legi_tr_t0, observed).
narrative_ontology:measurement(legi_tr_t5, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 5, 0.16).
narrative_ontology:measurement_basis(legi_tr_t5, observed).
narrative_ontology:measurement(legi_tr_t10, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement_basis(legi_tr_t10, observed).
narrative_ontology:measurement(legi_tr_t15, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 15, 0.27).
narrative_ontology:measurement_basis(legi_tr_t15, observed).
narrative_ontology:measurement(legi_tr_t20, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement_basis(legi_tr_t20, observed).
narrative_ontology:measurement(legi_tr_t25, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement_basis(legi_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(legi_be_t0, observed).
narrative_ontology:measurement(legi_be_t5, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 5, 0.35).
narrative_ontology:measurement_basis(legi_be_t5, observed).
narrative_ontology:measurement(legi_be_t10, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement_basis(legi_be_t10, observed).
narrative_ontology:measurement(legi_be_t15, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 15, 0.42).
narrative_ontology:measurement_basis(legi_be_t15, observed).
narrative_ontology:measurement(legi_be_t20, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement_basis(legi_be_t20, observed).
narrative_ontology:measurement(legi_be_t25, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 25, 0.42).
narrative_ontology:measurement_basis(legi_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement_basis(legi_su_t0, observed).
narrative_ontology:measurement(legi_su_t5, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 5, 0.35).
narrative_ontology:measurement_basis(legi_su_t5, observed).
narrative_ontology:measurement(legi_su_t10, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 10, 0.37).
narrative_ontology:measurement_basis(legi_su_t10, observed).
narrative_ontology:measurement(legi_su_t15, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 15, 0.38).
narrative_ontology:measurement_basis(legi_su_t15, observed).
narrative_ontology:measurement(legi_su_t20, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement_basis(legi_su_t20, observed).
narrative_ontology:measurement(legi_su_t25, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 25, 0.38).
narrative_ontology:measurement_basis(legi_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_knowledge_boundary__hybrid_coproduction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.12).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__hybrid_coproduction_reading, legitimate_knowledge_boundary__credentialed_expertise_reading).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__hybrid_coproduction_reading, legitimate_knowledge_boundary__experiential_pluralism_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of a three-reading kernel family: legitimate_knowledge_boundary. Each reading represents a distinct answer to the question 'What makes knowledge legitimate and who decides?'. The hybrid_coproduction_reading claims integration of methodological and experiential validation through partnership. This reading affects and is affected by the credentialed_expertise_reading (which co-production constrains by requiring experiential validation) and the experiential_pluralism_reading (which co-production partially displaces by imposing methodological requirements). The three constraints share the same kernel but have distinct ε values, beneficiary/victim structures, and claimed types. Analysis must preserve the distinction: these are not the same constraint viewed from different angles, but three different constraints arising from a contested interpretive commitment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
