% ============================================================================
% CONSTRAINT STORY: ai_alignment_commitment__ethics_justice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_commitment__ethics_justice_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: ai_alignment_commitment__ethics_justice_reading
 *   human_readable: AI Alignment as Bias and Harm Prevention (Ethics/Justice Reading)
 *   domain: AI Governance / Technology Ethics / Risk Assessment
 *
 * SUMMARY:
 *   This constraint story captures the ethics/justice reading of the AI
 *   alignment kernel: alignment is defined as preventing the reproduction of
 *   social bias and present-day harm in AI systems. This reading prioritizes
 *   demonstrated current harms to marginalized populations and treats
 *   long-term safety research as a competing claim that extracts attention
 *   and resources from urgent justice work. The constraint operates through
 *   regulatory mandates, audit requirements, and funding criteria that
 *   enforce bias mitigation, while the safety research community experiences
 *   reduced support and institutional recognition. The kernel is contested:
 *   sibling readings frame alignment as catastrophic risk prevention
 *   (safety_control) or as a simultaneous dual mandate (integrated). This
 *   reading asserts a primacy of current harm prevention.
 *
 * KEY AGENTS:
 *   - marginalized_communities: Primary beneficiary (organized/constrained) — experiences reduced bias and displacement when constraint is enforced
 *   - current_harm_victims: Primary beneficiary (organized/constrained) — direct recipients of harm prevention
 *   - long_term_safety_researchers: Primary victim (moderate/constrained) — loses funding, talent, and institutional legitimacy to ethics/justice priorities
 *   - ai_safety_field: Victim (organized/constrained) — the research program as a whole is marginalized
 *   - ai_ethics_regulators: Agenda setter (institutional/generational) — sets and enforces bias mitigation standards
 *   - tech_companies_implementing_ethics: Agenda setter (powerful/constrained) — operationalizes constraint via compliance teams and product reviews
 *   - ai_policy_scholars: Observer (analytical/analytical) — analyzes the structural dynamics
 *   - long_term_safety_advocates: Excluded (moderate/trapped) — would contest the priority ordering but are structurally excluded from ethics/justice governance forums
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_commitment__ethics_justice_reading, 0.65).
domain_priors:suppression_score(ai_alignment_commitment__ethics_justice_reading, 0.6).
domain_priors:theater_ratio(ai_alignment_commitment__ethics_justice_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_commitment__ethics_justice_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_commitment__ethics_justice_reading, "AI Alignment as Bias and Harm Prevention (Ethics/Justice Reading)").
narrative_ontology:topic_domain(ai_alignment_commitment__ethics_justice_reading, "AI Governance / Technology Ethics / Risk Assessment").

domain_priors:requires_active_enforcement(ai_alignment_commitment__ethics_justice_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_commitment__ethics_justice_reading, '99c1080a-6b5f-43d7-a8ed-14af9060a77f').
narrative_ontology:cs_kernel_codification('99c1080a-6b5f-43d7-a8ed-14af9060a77f', formalized).
narrative_ontology:cs_authority_grounding('99c1080a-6b5f-43d7-a8ed-14af9060a77f', expertise).
narrative_ontology:cs_interpretation_layer_present('99c1080a-6b5f-43d7-a8ed-14af9060a77f').
narrative_ontology:cs_reading_relation('99c1080a-6b5f-43d7-a8ed-14af9060a77f', ai_alignment_commitment__safety_control_reading, influences).
narrative_ontology:cs_reading_relation('99c1080a-6b5f-43d7-a8ed-14af9060a77f', ai_alignment_commitment__integrated_reading, coexists_with).
narrative_ontology:cs_axiom('99c1080a-6b5f-43d7-a8ed-14af9060a77f', foundational, current_harm_prevention_primacy).
narrative_ontology:cs_axiom_status(current_harm_prevention_primacy, holdable).
narrative_ontology:cs_axiom_grounding('99c1080a-6b5f-43d7-a8ed-14af9060a77f', current_harm_prevention_primacy, deontological).
narrative_ontology:cs_axiom('99c1080a-6b5f-43d7-a8ed-14af9060a77f', secondary, long_term_safety_research_secondary).
narrative_ontology:cs_axiom_status(long_term_safety_research_secondary, holdable).
narrative_ontology:cs_axiom_grounding('99c1080a-6b5f-43d7-a8ed-14af9060a77f', long_term_safety_research_secondary, empirically_contingent).
narrative_ontology:cs_reference_frame('99c1080a-6b5f-43d7-a8ed-14af9060a77f', ethical_obligation_to_marginalized).
narrative_ontology:cs_drift_state('99c1080a-6b5f-43d7-a8ed-14af9060a77f', contemporary_ai_ethics_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('99c1080a-6b5f-43d7-a8ed-14af9060a77f', '').
narrative_ontology:cs_kernel_id(ai_alignment_commitment__ethics_justice_reading, ai_alignment_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__ethics_justice_reading, marginalized_communities).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__ethics_justice_reading, current_harm_victims).
narrative_ontology:constraint_victim(ai_alignment_commitment__ethics_justice_reading, long_term_safety_researchers).
narrative_ontology:constraint_victim(ai_alignment_commitment__ethics_justice_reading, ai_safety_field).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__ethics_justice_reading, tech_companies_implementing_ethics).
narrative_ontology:constraint_vindicates(ai_alignment_commitment__ethics_justice_reading, justice_requires_addressing_current_harm).
narrative_ontology:constraint_vindicates(ai_alignment_commitment__ethics_justice_reading, bias_reproduction_is_primary_alignment_failure).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities historically subjected to algorithmic bias (racial, gender, disability, etc.) who experience reduced harm when bias mitigation is enforced. Their exit from the constraint is constrained — they cannot opt out of AI systems deployed in hiring, lending, policing, healthcare. They gain protection but do not control the enforcement machinery.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, marginalized_communities, beneficiary,
    organized, generational, constrained, global).

% Individuals and groups currently experiencing displacement, discrimination, or harm from deployed AI systems (e.g., gig workers, loan applicants, patients). They benefit directly from harm prevention mandates. Exit is constrained — they are subject to AI decisions regardless of consent.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, current_harm_victims, beneficiary,
    organized, biographical, constrained, global).

% Researchers working on existential risk, alignment theory, and scalable oversight. They face reduced funding (government and philanthropic grants redirected to bias mitigation), hiring freezes, and publication pressure to frame work in ethics/justice terms. Exit is constrained — shifting fields costs years of specialized training; leaving AI entirely abandons the problem they consider most critical.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, long_term_safety_researchers, payer,
    moderate, generational, constrained, global).

% The institutional ecosystem of AI safety research (labs, conferences, funding programs). It loses legitimacy and resources as policy attention shifts to near-term harms. The field cannot easily exit because its mission (preventing catastrophe) is defined against the very framing that marginalizes it.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, ai_safety_field, payer,
    organized, generational, constrained, global).

% Government agencies and international bodies setting AI ethics standards (e.g., EU AI Act, US executive orders). They gain authority and enforcement capacity from the constraint. Exit is arbitrage-grade — they can pivot to other regulatory domains if AI governance loses salience.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, ai_ethics_regulators, agenda_setter,
    institutional, generational, arbitrage, national).

% Large AI developers who build compliance teams, bias audit pipelines, and responsible AI frameworks. They gain regulatory goodwill and market trust (beneficiary) but also set the practical interpretation of the constraint (agenda setter). Exit is constrained — they must comply to operate in major markets.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, tech_companies_implementing_ethics, agenda_setter,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_commitment__ethics_justice_reading, tech_companies_implementing_ethics, beneficiary).

% Academic and think-tank researchers studying AI governance. They analyze the constraint's effects without directly bearing its costs or collecting its benefits. Their exit is analytical — they can shift research focus freely.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, ai_policy_scholars, observer,
    analytical, generational, analytical, global).

% Researchers and advocates who argue that catastrophic risk deserves priority. They are structurally excluded from ethics/justice governance forums (funding panels, standard-setting bodies) where the constraint's priorities are set. Their exit is trapped — they cannot influence the constraint that marginalizes their work, and leaving the field abandons their mission.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, long_term_safety_advocates, excluded,
    moderate, generational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_commitment__ethics_justice_reading, ai_ethics_regulators).
narrative_ontology:fixing_cost_class(ai_alignment_commitment__ethics_justice_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents AI systems from reproducing and amplifying existing social biases and inflicting present-day harms on marginalized populations by mandating bias audits, fairness metrics, and harm mitigation in deployment.
% TRANSFER_FUNCTION: Moves research funding, policy attention, talent, and institutional legitimacy from long-term AI safety research to near-term bias/harm mitigation programs. The transfer is enforced through grant criteria, regulatory mandates, and publication norms.
% ABSENT_VOICES: Long-term safety advocates and researchers who would contest the priority ordering are excluded from ethics/justice governance forums. Also absent: future generations who would bear catastrophic risk if safety research is underfunded — they have no voice in current allocation.
% DISAPPEARANCE_RATIONALE: If the ethics/justice constraint vanished overnight, bias mitigation mandates would lapse, funding would flow back to safety research, and AI deployment would proceed with less near-term fairness oversight but more long-term safety investment. The world would rearrange along the justice/safety axis.
% FOUNDING_PROBLEM: Early AI deployments reproduced and scaled historical biases (hiring, lending, policing, healthcare), harming marginalized communities. The alignment commitment was reoriented to address these demonstrated harms as the primary meaning of 'alignment'.
% FOUNDING_PROBLEM_CORROBORATION: Civil rights organizations, affected community groups, and independent algorithmic auditing bodies (outside the benefiting regulatory and industry actors) attest that bias and harm remain pervasive and worsening. The benefiting parties (ethics regulators, tech compliance teams) also attest the problem is live, but their corroboration is self-interested.
narrative_ontology:disappearance_verdict(ai_alignment_commitment__ethics_justice_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_commitment__ethics_justice_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_commitment__ethics_justice_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_alignment_commitment__ethics_justice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_commitment__ethics_justice_reading, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_commitment__ethics_justice_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_commitment__ethics_justice_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_alignment_commitment__ethics_justice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) reflects the diversion of research funding, talent, and policy attention from long-term safety to near-term bias mitigation. Suppression (0.6) captures the active enforcement via funding mandates, publication norms, and regulatory requirements that marginalize safety research. Theater ratio (0.3) indicates some performative compliance (bias audits that don't change outcomes) but substantial functional activity. Accessibility collapse (0.5) shows alternatives (e.g., integrated approaches) are partially suppressed but not eliminated. Resistance (0.55) comes from safety researchers and some industry actors who argue for balanced investment. The claimed type tangled_rope fits: genuine coordination function (preventing bias/harm) coexists with asymmetric extraction (safety research pays).
 *
 * PERSPECTIVAL GAP:
 *   From the ethics/justice seat, the constraint is a necessary correction to historical neglect of current harms — a rope-like coordination. From the safety research seat, the same structure is a snare that starves existential risk work. The engine's per-seat classification will reveal this divergence. The integrated reading attempts to bridge the gap but does not eliminate the resource competition.
 *
 * DIRECTIONALITY LOGIC:
 *   Marginalized communities and current harm victims are beneficiaries (d near 0.0): the constraint subsidizes their protection. Long-term safety researchers and the AI safety field are targets (d near 1.0): they bear the extraction. AI ethics regulators and tech companies are agenda setters with low d (beneficiary side) because they gain legitimacy and control. Long-term safety advocates are excluded with high d (trapped exit): they cannot exit the constraint's effects but have no voice in its design. The engine will compute per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing AI from reproducing social bias) remains live and worsening as AI deployment expands. However, the constraint shows mandatrophy signs: the enforcement machinery (audits, compliance teams) has grown beyond the minimal coordination needed, and the extraction from safety research has increased without proportional harm reduction. The theater ratio rise suggests performative maintenance. The constraint is not yet a piton because the coordination function is still actively defended, but the drift is toward tangled_rope with increasing extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_priority,
    'Is the prioritization of current bias/harm prevention over long-term safety a genuine moral necessity or a constructed framing that benefits near-term ethics institutions?',
    'Cross-disciplinary meta-analysis of harm prevalence vs. existential risk probability; tracking of funding flows between ethics and safety research programs.',
    'If constructed, the constraint is a tangled_rope with extractive diversion; if moral necessity, the extraction from safety research is a justified trade-off.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_priority, conceptual, 'Whether the ethics/justice reading''s priority ordering reflects an irreducible moral fact or a contingent institutional framing.').

omega_variable(
    suppression_mechanism_in_ai_ethics_enforcement,
    'Is the suppression of long-term safety research structural (funding mandates, publication norms) or internalized (researchers self-censor to align with dominant ethics narrative)?',
    'Survey of AI safety researchers on perceived pressure; analysis of grant review criteria and conference acceptance rates over time.',
    'If internalized, effective suppression is higher than structural measures suggest; the constraint carries its enforcement within the research community.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_in_ai_ethics_enforcement, empirical, 'Structural vs. internalized suppression mechanism in the ethics/justice reading''s marginalization of safety research.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_commitment__ethics_justice_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ai_a_tr_t4, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement(ai_a_tr_t8, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(ai_a_tr_t12, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 12, 0.26).
narrative_ontology:measurement(ai_a_tr_t16, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 16, 0.28).
narrative_ontology:measurement(ai_a_tr_t20, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 20, 0.3).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(ai_a_be_t4, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(ai_a_be_t8, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(ai_a_be_t12, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 12, 0.6).
narrative_ontology:measurement(ai_a_be_t16, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 16, 0.63).
narrative_ontology:measurement(ai_a_be_t20, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 20, 0.65).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(ai_alignment_commitment__ethics_justice_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_commitment__ethics_justice_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(ai_alignment_commitment__ethics_justice_reading, 0.08).
narrative_ontology:affects_constraint(ai_alignment_commitment__ethics_justice_reading, ai_alignment_commitment__safety_control_reading).
narrative_ontology:affects_constraint(ai_alignment_commitment__ethics_justice_reading, ai_alignment_commitment__integrated_reading).

% DUAL FORMULATION NOTE:
% Kernel decomposition per ε-invariance: ethics_justice_reading (ε=0.65, tangled_rope) vs safety_control_reading (ε≈0.3, rope?) vs integrated_reading (ε≈0.5, tangled_rope?). The ethics_justice reading extracts from safety research; the safety_control reading extracts from ethics work; the integrated reading attempts coordination but may inherit extraction from both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_alignment_commitment__ethics_justice_reading, moderate, 0.85).
constraint_indexing:directionality_override(ai_alignment_commitment__ethics_justice_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
