% ============================================================================
% CONSTRAINT STORY: ai_risk_prioritization__near_term_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_prioritization__near_term_harms_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ai_risk_prioritization__near_term_harms_reading
 *   human_readable: Near-Term AI Harm Prioritization Frame
 *   domain: technology_governance/ai_safety/risk_assessment
 *
 * SUMMARY:
 *   This constraint story instantiates the 'near-term harms' reading of the
 *   contested kernel 'AI risk prioritization.' The reading asserts that AI
 *   risk is primarily constituted by measurable, deployed-system harms —
 *   algorithmic discrimination in lending/hiring/policing, worker
 *   displacement and surveillance via algorithmic management, biometric mass
 *   surveillance — and that justice interventions (bias audits, worker
 *   protections, surveillance regulation) are the paramount response. The
 *   kernel is contested: the sibling 'existential_risk_reading' frames AI
 *   risk as primarily extinction-level threat from misaligned AGI, demanding
 *   alignment research as the paramount intervention. This story authors ONLY
 *   the near_term_harms_reading as a clean ε-invariant constraint. The
 *   committer structure (kernel_id, reading_id, sibling relations) is routed
 *   to omega variables and cs_structure per Rules 2 and 4.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_prioritization__near_term_harms_reading, 0.25).
domain_priors:suppression_score(ai_risk_prioritization__near_term_harms_reading, 0.42).
domain_priors:theater_ratio(ai_risk_prioritization__near_term_harms_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_prioritization__near_term_harms_reading, rope).
narrative_ontology:human_readable(ai_risk_prioritization__near_term_harms_reading, "Near-Term AI Harm Prioritization Frame").
narrative_ontology:topic_domain(ai_risk_prioritization__near_term_harms_reading, "technology_governance/ai_safety/risk_assessment").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_prioritization__near_term_harms_reading, '2e3a6c55-84e3-4bc3-afd2-6494b2338e6c').
narrative_ontology:cs_kernel_codification('2e3a6c55-84e3-4bc3-afd2-6494b2338e6c', distributed).
narrative_ontology:cs_authority_grounding('2e3a6c55-84e3-4bc3-afd2-6494b2338e6c', distributed).
narrative_ontology:cs_reading_relation('2e3a6c55-84e3-4bc3-afd2-6494b2338e6c', ai_risk_prioritization__existential_risk_reading, influences).
narrative_ontology:cs_axiom('2e3a6c55-84e3-4bc3-afd2-6494b2338e6c', foundational, present_harm_epistemic_primacy).
narrative_ontology:cs_axiom_status(present_harm_epistemic_primacy, holdable).
narrative_ontology:cs_axiom_grounding('2e3a6c55-84e3-4bc3-afd2-6494b2338e6c', present_harm_epistemic_primacy, empirically_contingent).
narrative_ontology:cs_axiom('2e3a6c55-84e3-4bc3-afd2-6494b2338e6c', foundational, justice_intervention_paramountcy).
narrative_ontology:cs_axiom_status(justice_intervention_paramountcy, holdable).
narrative_ontology:cs_axiom_grounding('2e3a6c55-84e3-4bc3-afd2-6494b2338e6c', justice_intervention_paramountcy, deontological).
narrative_ontology:cs_reference_frame('2e3a6c55-84e3-4bc3-afd2-6494b2338e6c', emergent_ai_governance_field_2016).
narrative_ontology:cs_drift_state('2e3a6c55-84e3-4bc3-afd2-6494b2338e6c', post_llm_deployment_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2e3a6c55-84e3-4bc3-afd2-6494b2338e6c', '').
narrative_ontology:cs_kernel_id(ai_risk_prioritization__near_term_harms_reading, ai_risk_prioritization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, marginalized_communities).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, fairness_accountability_researchers).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, labor_rights_advocates).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, digital_rights_organizations).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, x_risk_research_funding).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, longtermist_institutional_attention).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, low_wage_workers).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, industry_ai_labs).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, low_wage_workers).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, surveilled_populations).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, x_risk_researchers).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, longtermist_institutions).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, industry_ai_labs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Experience algorithmic discrimination in hiring, lending, policing, and healthcare today. Their lived reality is the evidence base for this reading. They have no structural exit from the systems that classify, score, and gatekeep them — exit would mean opting out of employment, credit, public services, or freedom of movement.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, marginalized_communities, beneficiary,
    powerless, biographical, trapped, global).

% Bear displacement and surveillance harms from algorithmic management, gig platforms, and workplace AI. They pay through lost autonomy, wage theft via algorithmic opacity, and biometric monitoring. They benefit from this reading's push for worker protections and transparency mandates, but their exit options are constrained by economic necessity — they cannot refuse algorithmic work arrangements without losing livelihood.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, low_wage_workers, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_prioritization__near_term_harms_reading, low_wage_workers, beneficiary).

% Subject to facial recognition, predictive policing, social scoring, and border AI systems. They bear the false-positive costs, chilling effects, and democratic erosion. Exit is structurally blocked — these systems are embedded in state infrastructure, public space, and migration pathways. This reading centers their experience as the primary harm evidence.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, surveilled_populations, payer,
    powerless, biographical, trapped, regional).

% Build the technical and policy infrastructure for bias audits, impact assessments, and regulatory compliance. They gain funding, institutional recognition, and field-defining authority from this reading's dominance. Their exit options are mobile — they can move between academia, industry labs, civil society, and regulatory bodies. They are not trapped by the frame; they help construct it.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, fairness_accountability_researchers, beneficiary,
    organized, biographical, mobile, global).

% Leverage this reading to win algorithmic transparency laws, worker data rights, and collective bargaining over AI deployment. They gain policy traction and membership relevance. Exit is mobile — they operate in established labor law frameworks and can pivot strategies. They benefit from the reading's resource allocation toward worker protections.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, labor_rights_advocates, beneficiary,
    organized, biographical, mobile, national).

% Campaign against biometric surveillance, predictive policing, and automated decision-making in public services. They gain litigation opportunities, donor visibility, and legislative windows from this reading's framing. Mobile exit — they operate across jurisdictions and issue areas. Not trapped by the frame.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, digital_rights_organizations, beneficiary,
    organized, biographical, mobile, global).

% See funding and talent diverted from alignment research to near-term auditing. They argue the reading suppresses consideration of catastrophic tail risks that require long-horizon investment. Their exit is constrained — the field's epistemology and funding structures are shaped by the dominant risk frame. They cannot easily rebuild a displaced research agenda.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, x_risk_researchers, payer,
    moderate, generational, constrained, global).

% Philanthropic and research organizations (e.g., Open Philanthropy, Future of Life Institute) that prioritize existential risk. They lose narrative control over the 'AI safety' brand and see policy windows captured by near-term frames. Their exit is arbitrage-grade — they command independent capital, global networks, and multi-decade horizons. They are not trapped; they contest the frame from a position of structural power.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, longtermist_institutions, payer,
    institutional, civilizational, arbitrage, global).

% Write and enforce AI acts, executive orders, and sectoral rules. They must allocate finite enforcement capacity. This reading gives them a clear, measurable, politically legible mandate: audit deployed systems, ban harmful uses, protect rights now. They gain bureaucratic legitimacy and congressional mandate. Their analytical exit means they can evaluate frames without being captured by any single one — but their institutional role forces a choice of priority.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, ai_policy_regulators, agenda_setter,
    institutional, generational, analytical, regional).

% Bear compliance costs for bias audits, transparency reporting, and deployment restrictions. They also benefit: near-term regulation is legible, finite, and creates moats against smaller competitors. They can afford compliance; startups cannot. Their arbitrage exit means they can shape, lobby, relocate, or acquire their way through regulatory regimes. They are not trapped by this reading — they instrumentalize it.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, industry_ai_labs, payer,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_prioritization__near_term_harms_reading, industry_ai_labs, beneficiary).

% Produces the evidence base for this reading: audit methodologies, disparity measurements, harm taxonomies. They observe the frame's construction and contest its boundaries from within. Analytical exit — they evaluate the reading's epistemic adequacy, not just its political utility. Their situation is to maintain methodological rigor while the policy world instrumentalizes their output.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, academic_ai_ethics_field, observer,
    organized, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates attention, funding, and regulatory capacity toward measurable, documentable harms from deployed AI systems — discrimination, displacement, surveillance — creating a shared evidentiary and policy framework for justice interventions.
% TRANSFER_FUNCTION: Moves research funding, policy attention, regulatory enforcement capacity, and narrative authority from long-horizon existential risk research toward near-term algorithmic justice work: bias audits, worker data rights, surveillance bans, impact assessments. From x-risk researchers and longtermist institutions to fairness/accountability researchers, labor advocates, digital rights orgs, and affected communities.
% ABSENT_VOICES: Future generations who would bear existential catastrophe costs — they are structurally excluded from present deliberation. Also excluded: populations in Global South regions where both near-term deployment harms AND long-term capability externalities concentrate, but who lack representation in either research community. The x-risk reading's 'future stakeholders' are a rhetorical construct, not a seated voice.
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight, regulatory agendas would shift toward capability control and alignment metrics; funding would flow to interpretability and scalable oversight rather than bias audits and worker protections; affected communities would lose their primary policy lever. The material arrangements of AI governance would reorganize around a different harm ontology.
% FOUNDING_PROBLEM: AI systems deployed at scale since ~2016 were producing documented, measurable harms — racial bias in criminal risk scores, gender bias in hiring algorithms, worker exploitation via algorithmic management, mass biometric surveillance — while the dominant 'AI safety' discourse focused on speculative future AGI scenarios. This reading was built to force the field to account for harms happening now, to people who cannot wait.
% FOUNDING_PROBLEM_CORROBORATION: Documented by independent investigative journalism (ProPublica's COMPAS exposé, 2016), academic audit literature (Buolamwini & Gebru 2018, Raji et al. 2020), labor organizing (Alphabet Workers Union, App Workers Alliance), and regulatory findings (EU AI Act high-risk categories, FTC algorithmic disgorgement cases). Corroboration comes from outside the benefiting research community — from courts, journalists, workers, and regulators who had no stake in the 'AI ethics' field's formation.
narrative_ontology:disappearance_verdict(ai_risk_prioritization__near_term_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_prioritization__near_term_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_prioritization__near_term_harms_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(ai_risk_prioritization__near_term_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_prioritization__near_term_harms_reading, 0.25, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_prioritization__near_term_harms_reading_tests).
:- end_tests(ai_risk_prioritization__near_term_harms_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.25) because this reading's operation primarily coordinates resources toward documented harms rather than extracting from a trapped population. The 'victims' declared (x_risk_research_funding, longtermist_institutional_attention) experience resource diversion, not material extraction — they remain powerful actors with arbitrage exit. Suppression (0.42) reflects the reading's rhetorical marginalization of x-risk concerns as 'speculative distraction,' which shapes funding and hiring but does not physically prevent x-risk work. Theater ratio (0.18) captures the growing performative compliance by industry labs (ethics boards, published principles) that do not change deployment practices. Accessibility collapse (0.35) is moderate — alternatives (x-risk framing) remain institutionally viable and well-funded. Resistance (0.58) is significant: the reading faces active contestation from longtermist institutions, industry labs, and parts of the AI research community.
 *
 * PERSPECTIVAL GAP:
 *   From the marginalized community seat, this reading is a rope — genuine coordination toward justice. From the x-risk researcher seat, it functions as a snare — their research agenda is suppressed by a frame that declares their core concern 'speculative.' From the industry lab seat, it is a tangled rope — real compliance burden (coordination) that also creates competitive moats (extraction). The engine computes this divergence; the claim 'rope' reflects the authoring seat's assessment of the constraint's dominant structural character.
 *
 * DIRECTIONALITY LOGIC:
 *   Marginalized communities, low-wage workers, and surveilled populations are structural payers (bear harms) and beneficiaries (gain protections) — their exit is trapped or constrained, so directionality is high target. Fairness researchers, labor advocates, and digital rights orgs are beneficiaries with mobile exit — directionality near zero. X-risk researchers and longtermist institutions are declared victims of resource diversion but hold moderate-to-institutional power with constrained-to-arbitrage exit — their effective extraction is dampened by exit quality. Industry labs are payers (compliance costs) and beneficiaries (moat creation) with arbitrage exit — directionality near symmetric. Regulators are agenda_setters with analytical exit. The engine computes per-seat χ from these structural declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (documented near-term harms ignored by x-risk discourse) remains live — harms have intensified with LLM deployment. No mandatrophy: the reading's function has not atrophied. However, the reading's own success creates a new risk: as near-term regulation matures (EU AI Act, US executive orders), the coordination function may be captured by compliance theater, and the reading could drift toward piton if the justice interventions become performative checkboxes. The theater_ratio trajectory (0.05→0.18) warrants monitoring.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the ai_risk_prioritization kernel a single commitment with multiple readings, or are these two distinct constraint stories artificially linked by a shared label?',
    'Trace whether the two readings share a stabilized kernel (fixed text, formal rule, practice-based norm) that both sides accept as authoritative ground for adjudication. If no shared kernel exists — if the ''contest'' is merely two advocacy frames using the same words — then the kernel_id is a category error and each reading should stand as an independent constraint without cs_structure.',
    'If no shared kernel exists, cs_structure fields (reading_relations, axioms, reference_frame, drift_state) are inapplicable and should be removed. The two stories would be unrelated constraints in the same domain, linked only by network.affects_constraints if they compete for the same resources.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the declared kernel has ontological standing or is a linguistic artifact.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.42) structural (funding structures, hiring norms, conference gatekeeping) or internalized (x-risk researchers self-censor, adopt near-term vocabulary to remain fundable)?',
    'Survey x-risk researchers on perceived speech constraints; analyze funding acknowledgment patterns in papers; track hiring committee deliberations (where accessible). If suppression persists after structural barriers are removed (e.g., dedicated x-risk funding streams exist), reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them. This would increase effective extraction for the x-risk researcher seat and could shift their computed type toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for x-risk researchers.').

omega_variable(
    extraction_referent_stability,
    'Does this reading''s ε (0.25) refer to the standing arrangement of AI governance prioritization, or has it been influenced by the reading''s own endorsed alternative (a justice-centered governance regime)?',
    'Per OQ-258 ruling: ε''s referent for a kernel-reading story is the standing arrangement under contest — the current AI governance field as the near-term reading sees it — assessed by the reading''s own lights. Verify the authored ε describes the extraction inherent in the current prioritization frame, not the extraction of the reading''s proposed reforms.',
    'If ε was inadvertently measured against the reading''s endorsed alternative (where near-term justice interventions are centered and extraction would be near-zero), the value is invalid and must be re-authored against the actual standing arrangement where both readings contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_referent_stability, conceptual, 'ε-invariance compliance for kernel-reading stories: referent discipline.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_prioritization__near_term_harms_reading, 2016, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t2016, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 2016, 0.05).
narrative_ontology:measurement(ai_r_tr_t2018, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 2018, 0.08).
narrative_ontology:measurement(ai_r_tr_t2020, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 2020, 0.12).
narrative_ontology:measurement(ai_r_tr_t2022, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 2022, 0.15).
narrative_ontology:measurement(ai_r_tr_t2024, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 2024, 0.17).
narrative_ontology:measurement(ai_r_tr_t2025, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 2025, 0.18).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t2016, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 2016, 0.12).
narrative_ontology:measurement(ai_r_be_t2018, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 2018, 0.18).
narrative_ontology:measurement(ai_r_be_t2020, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 2020, 0.22).
narrative_ontology:measurement(ai_r_be_t2022, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 2022, 0.24).
narrative_ontology:measurement(ai_r_be_t2024, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 2024, 0.25).
narrative_ontology:measurement(ai_r_be_t2025, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 2025, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t2016, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 2016, 0.25).
narrative_ontology:measurement(ai_r_su_t2018, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 2018, 0.32).
narrative_ontology:measurement(ai_r_su_t2020, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 2020, 0.38).
narrative_ontology:measurement(ai_r_su_t2022, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 2022, 0.4).
narrative_ontology:measurement(ai_r_su_t2024, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 2024, 0.41).
narrative_ontology:measurement(ai_r_su_t2025, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 2025, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_prioritization__near_term_harms_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(ai_risk_prioritization__near_term_harms_reading, 0.08).
narrative_ontology:affects_constraint(ai_risk_prioritization__near_term_harms_reading, ai_risk_prioritization__existential_risk_reading).

% DUAL FORMULATION NOTE:
% The ai_risk_prioritization kernel decomposes into two constraint stories with divergent ε and victim structures: near_term_harms_reading (ε=0.25, victims=present marginalized populations, claimed rope) and existential_risk_reading (expected ε=0.15, victims=future generations, claimed mountain/tangled_rope). They compete for the same research funding, policy windows, and 'AI safety' brand authority. The near-term reading's dominance in regulatory venues (EU AI Act, US EO) structurally pressures the existential reading's resource base — this is an 'influences' relation from near-term to existential.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_risk_prioritization__near_term_harms_reading, institutional, 0.15).
constraint_indexing:directionality_override(ai_risk_prioritization__near_term_harms_reading, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
