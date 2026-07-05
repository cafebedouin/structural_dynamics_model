% ============================================================================
% CONSTRAINT STORY: ai_human_relationship__instrumental_subsidiarity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_human_relationship__instrumental_subsidiarity, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: ai_human_relationship__instrumental_subsidiarity
 *   human_readable: AI as Morally Neutral Tool Governed by Law: Instrumental-Subsidiarity Reading
 *   domain: technology_ethics/political_theology/regulatory_governance
 *
 * SUMMARY:
 *   This story instantiates one reading within the contested 'AI as neutral
 *   tool' kernel: the instrumental-subsidiarity reading, which holds that AI
 *   technology is morally neutral in itself, that moral responsibility
 *   attaches to specific use-cases rather than to the technology or its
 *   designers, and that subsidiarity functions as a procedural principle for
 *   allocating regulatory authority to the appropriate legal layer (local,
 *   national, supranational). This reading coordinates real governance work —
 *   it lets jurisdictions with deep disagreement about technology's proper
 *   ends act on shared procedural ground (disclosure, audit trails, appeal
 *   mechanisms). But the coordination rides alongside an asymmetric
 *   extraction: the procedural frame concentrates benefit on deploying firms
 *   and the compliance-consulting apparatus that services them, while
 *   individualizing harm-discovery costs onto algorithmically managed workers
 *   and diffusing accountability away from deployment-pattern critique. The
 *   rising theater_ratio reflects a documented drift: as the technology
 *   matures, an increasing share of 'governance' activity becomes
 *   disclosure-and-audit theater that certifies process without reaching the
 *   substantive question of whether a deployment serves human ends.
 *
 * KEY AGENTS:
 *   - ai_deploying_firms: primary beneficiary (institutional/arbitrage) — benefits from responsibility being located downstream in use rather than upstream in design
 *   - regulatory_professional_class: co-beneficiary and agenda-setter (institutional/constrained) — gains a durable procedural mandate
 *   - compliance_consultancies: beneficiary (organized/mobile) — monetizes the procedural apparatus itself
 *   - algorithmically_managed_workers: primary target (powerless/trapped) — bears individualized harm-discovery costs
 *   - unrepresented_affected_communities: excluded voice (powerless/trapped) — cumulative harm the frame cannot register
 *   - catholic_social_teaching_tradition: analytical observer (analytical/analytical) — sees the tradition's substantive subsidiarity being thinned into a procedural allocation device
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_human_relationship__instrumental_subsidiarity, 0.42).
domain_priors:suppression_score(ai_human_relationship__instrumental_subsidiarity, 0.28).
domain_priors:theater_ratio(ai_human_relationship__instrumental_subsidiarity, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, extractiveness, 0.42).
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_human_relationship__instrumental_subsidiarity, tangled_rope).
narrative_ontology:human_readable(ai_human_relationship__instrumental_subsidiarity, "AI as Morally Neutral Tool Governed by Law: Instrumental-Subsidiarity Reading").
narrative_ontology:topic_domain(ai_human_relationship__instrumental_subsidiarity, "technology_ethics/political_theology/regulatory_governance").

domain_priors:requires_active_enforcement(ai_human_relationship__instrumental_subsidiarity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_human_relationship__instrumental_subsidiarity, 'f55dbb29-87cf-4332-bac4-d5891f5033fc').
narrative_ontology:cs_kernel_codification('f55dbb29-87cf-4332-bac4-d5891f5033fc', distributed).
narrative_ontology:cs_authority_grounding('f55dbb29-87cf-4332-bac4-d5891f5033fc', distributed).
narrative_ontology:cs_reading_relation('f55dbb29-87cf-4332-bac4-d5891f5033fc', ai_human_relationship__technocratic_optimization, influences).
narrative_ontology:cs_reading_relation('f55dbb29-87cf-4332-bac4-d5891f5033fc', ai_human_relationship__incarnational_humanism, coexists_with).
narrative_ontology:cs_axiom('f55dbb29-87cf-4332-bac4-d5891f5033fc', foundational, technology_morally_neutral_in_itself).
narrative_ontology:cs_axiom_status(technology_morally_neutral_in_itself, holdable).
narrative_ontology:cs_axiom_grounding('f55dbb29-87cf-4332-bac4-d5891f5033fc', technology_morally_neutral_in_itself, conventional).
narrative_ontology:cs_axiom('f55dbb29-87cf-4332-bac4-d5891f5033fc', foundational, subsidiarity_as_jurisdictional_allocation_device).
narrative_ontology:cs_axiom_status(subsidiarity_as_jurisdictional_allocation_device, holdable).
narrative_ontology:cs_axiom_grounding('f55dbb29-87cf-4332-bac4-d5891f5033fc', subsidiarity_as_jurisdictional_allocation_device, conventional).
narrative_ontology:cs_reference_frame('f55dbb29-87cf-4332-bac4-d5891f5033fc', procedural_regulatory_neutrality).
narrative_ontology:cs_drift_state('f55dbb29-87cf-4332-bac4-d5891f5033fc', post_generative_ai_deployment_wave, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f55dbb29-87cf-4332-bac4-d5891f5033fc', '').
narrative_ontology:cs_kernel_id(ai_human_relationship__instrumental_subsidiarity, ai_human_relationship).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, ai_deploying_firms).
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, regulatory_professional_class).
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, compliance_consultancies).
narrative_ontology:constraint_victim(ai_human_relationship__instrumental_subsidiarity, algorithmically_managed_workers).
narrative_ontology:constraint_victim(ai_human_relationship__instrumental_subsidiarity, unrepresented_affected_communities).
narrative_ontology:constraint_vindicates(ai_human_relationship__instrumental_subsidiarity, technological_neutrality_thesis).
narrative_ontology:constraint_vindicates(ai_human_relationship__instrumental_subsidiarity, subsidiarity_as_procedural_safeguard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Deploy AI systems and shape the regulatory conversation by supplying the technical expertise regulators depend on. Under the neutrality frame, their moral exposure is limited to compliance with whatever use-case rules are written; the underlying design choices and business model are treated as ethically inert. They benefit from a framework that locates responsibility downstream in 'use' rather than upstream in design, training-data selection, and deployment incentive structure.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, ai_deploying_firms, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_human_relationship__instrumental_subsidiarity, ai_deploying_firms, agenda_setter).

% Legislators, agency staff, and standards bodies draft transparency and audit requirements. The instrumental-subsidiarity frame gives them a durable mandate: as long as tools are neutral, governance work is procedural and can always be made more elaborate (more disclosure forms, more audit committees) without ever reaching a verdict on whether a given deployment should exist at all.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, regulatory_professional_class, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(ai_human_relationship__instrumental_subsidiarity, regulatory_professional_class, agenda_setter).

% Sell audit, documentation, and 'ethical AI' certification services built entirely on the premise that risk is manageable through paperwork and process design. Their business model requires the neutrality-plus-regulation frame to remain dominant; a substantive ends-based critique (dignity, common good) would not generate the same volume of billable procedural work.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, compliance_consultancies, beneficiary,
    organized, biographical, mobile, continental).

% Are scheduled, scored, and disciplined by AI systems whose harms manifest as individual 'use-case' disputes rather than as evidence against the system's design. Under the procedural-subsidiarity frame, their recourse is an appeals process or a transparency disclosure, not a challenge to whether the deployment itself violates their standing as ends rather than optimization inputs. They bear the accumulated cost of a governance model that treats each harm as a compliance gap rather than a structural pattern.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, algorithmically_managed_workers, payer,
    powerless, immediate, trapped, national).

% Communities affected by AI-driven allocation decisions (credit, housing, policing, benefits) are not parties to the standards-setting process; their objections would be to aggregate and cumulative harm, which the neutral-tool frame is structurally unequipped to register since it evaluates instances, not patterns.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, unrepresented_affected_communities, excluded,
    powerless, generational, trapped, regional).

% Holds subsidiarity as a substantive principle ordered toward the flourishing of persons and intermediate communities, not merely a procedural allocation-of-authority device. From this seat, the instrumental-subsidiarity reading is visible as a partial and potentially reductive appropriation of the tradition's vocabulary — using 'subsidiarity' to mean 'the right regulatory layer' while dropping the tradition's insistence that technology be evaluated against integral human development.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, catholic_social_teaching_tradition, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a workable, legally tractable framework for governing a fast-moving technology across many jurisdictions and use-cases without requiring prior theological or philosophical consensus on the nature of AI or personhood — regulators, firms, and courts can act on shared procedural ground (disclosure, audit, appeal) even when they disagree about deeper ends.
% TRANSFER_FUNCTION: Moves the burden of proof and the cost of harm-discovery from designers and deployers of AI systems onto the individuals affected by specific deployments, who must identify, document, and contest particular use-case failures one at a time; moves compliance revenue and regulatory legitimacy toward firms and consultancies who can supply the procedural apparatus the frame demands.
% ABSENT_VOICES: Communities experiencing cumulative or structural harm from AI deployment patterns (rather than isolated incidents) are not represented in standards bodies; the incarnational-humanist critique that dignity is being subordinated to instrumental use has organized adherents but limited access to the technical standards-setting rooms where the neutrality frame is operationalized.
% DISAPPEARANCE_RATIONALE: Firms and regulators would argue the world rearranges badly without procedural governance (a vacuum inviting either unchecked deployment or blunt prohibition); affected workers and communities would argue the underlying extraction pattern continues regardless of which governance vocabulary is used, since the harm is located in deployment incentives the neutrality frame does not reach — hence contested rather than a clean verdict.
% FOUNDING_PROBLEM: Rapid AI deployment across consequential domains (lending, hiring, policing, healthcare) outpaced existing legal categories, creating urgent demand for a governance vocabulary that could be adopted quickly across jurisdictions with differing philosophical and religious commitments about technology and personhood.
% FOUNDING_PROBLEM_CORROBORATION: Regulatory agencies and industry standards bodies attest the procedural framework remains necessary and functioning as designed. Independent labor advocates, algorithmic-accountability researchers, and the incarnational-humanist strand within Catholic social teaching itself attest that the founding problem has shifted: the acute governance vacuum is substantially addressed, but the frame now persists chiefly because it lets deploying firms and compliance intermediaries avoid a substantive reckoning with whether specific deployments serve or subordinate human ends — this latter attestation comes from outside the beneficiary set.
narrative_ontology:disappearance_verdict(ai_human_relationship__instrumental_subsidiarity, contested).
narrative_ontology:founding_problem_status(ai_human_relationship__instrumental_subsidiarity, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_human_relationship__instrumental_subsidiarity, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_human_relationship__instrumental_subsidiarity, 'none', 1).
narrative_ontology:epsilon_provenance(ai_human_relationship__instrumental_subsidiarity, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_human_relationship__instrumental_subsidiarity_tests).
:- end_tests(ai_human_relationship__instrumental_subsidiarity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.42) and theater ratio (0.48, rising toward the frame's midlife) reflect a governance structure whose coordination function is genuine but increasingly outpaced by its extractive drift: procedural compliance work grows faster than substantive harm reduction. Suppression is comparatively low (0.28) because exit from the frame is not physically or legally blocked — firms, workers, and communities can and do contest specific applications — but accessibility_collapse (0.35) and resistance (0.40) register that once the neutrality-plus-procedure vocabulary becomes the dominant regulatory language, alternative framings (ends-based, dignity-centered) struggle to gain standing in the rooms where standards are actually written.
 *
 * PERSPECTIVAL GAP:
 *   From the regulatory-professional and firm seats, this is a functioning Rope: a hard problem (governing fast-moving, jurisdictionally dispersed technology) solved through shared procedural commitments that let disagreeing parties act together. From the worker and excluded-community seats, the same structure computes as Tangled Rope at best: real coordination exists, but it rides on an asymmetric transfer where harm-discovery costs and structural critique are pushed outward while procedural legitimacy and revenue concentrate upward. The engine's per-seat computation is expected to diverge along exactly this line; the divergence is not an error in the story.
 *
 * DIRECTIONALITY LOGIC:
 *   Deploying firms and the regulatory-compliance apparatus sit near the beneficiary end: they set the terms of the procedural frame and capture its legitimacy and revenue. Algorithmically managed workers sit near the full-target end: trapped exit options, immediate time horizon, and the individualized burden of proving specific use-case harm. Unrepresented communities are structurally excluded rather than merely disadvantaged — their harm is aggregate and pattern-level, which the instance-by-instance procedural frame is not built to detect, let alone remedy.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — an acute governance vacuum for fast-deploying AI — was real and largely addressed by the initial wave of transparency and audit requirements. What has NOT resolved is the substantive question the incarnational-humanist reading presses: whether specific deployments serve integral human development or merely optimize a metric at human cost. The instrumental-subsidiarity frame's procedural apparatus keeps expanding (rising theater_ratio) even as the founding vacuum closes, which is the signature this framework is built to catch: a mandate persisting past its live function, sustained now chiefly by the interests of the professional and consulting class it created.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    neutrality_thesis_contestability,
    'Is the claim that AI technology is morally neutral in itself (with responsibility located only in use-cases) a defensible philosophical position, or does it function as a structural device that shields upstream design and deployment-incentive choices from ethical scrutiny?',
    'Comparative case analysis of deployments where identical technical architecture produces harm across a class of use-cases (suggesting the harm is designed-in) versus deployments where harm is genuinely idiosyncratic to a single use-case (supporting the neutrality thesis in that instance).',
    'If harm is substantially attributable to design and deployment-incentive patterns rather than isolated misuse, the neutrality premise this reading rests on weakens, and the constraint''s coordination function increasingly looks like cover for extraction rather than a genuine philosophical position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neutrality_thesis_contestability, conceptual, 'Whether technological neutrality is a sound premise or a structural shield.').

omega_variable(
    subsidiarity_procedural_vs_substantive,
    'Does this reading''s use of ''subsidiarity'' as a procedural allocation-of-authority device preserve the substantive content the principle carries in the Catholic social teaching tradition it invokes, or has the term been thinned to mean something the tradition would not recognize as sufficient?',
    'Textual and magisterial comparison between this reading''s operative use of subsidiarity and its treatment in the tradition''s foundational texts (e.g., Quadragesimo Anno, Centesimus Annus, Fratelli Tutti), focused on whether subsidiarity is ever invoked independent of its ordering toward the common good and person.',
    'If the tradition''s subsidiarity is inseparable from its substantive end-orientation, this reading''s procedural-only appropriation is a partial and potentially distorting use of the vocabulary — strengthening the case that the reading functions as legitimating cover rather than authentic doctrinal application.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsidiarity_procedural_vs_substantive, conceptual, 'Whether procedural subsidiarity is faithful to or a thinning of the tradition''s substantive principle.').

omega_variable(
    aggregate_harm_detectability,
    'Can a use-case-by-use-case procedural governance frame ever detect and remedy cumulative or pattern-level harm to a class of people, or is that structurally outside what such a frame can register?',
    'Empirical audit of whether any use-case-level compliance regime has, in practice, led to withdrawal or structural redesign of a deployment pattern (as opposed to individual remediation), across a sample of jurisdictions with mature AI governance frameworks.',
    'If aggregate harm is never remedied through the procedural frame, this constrains the frame''s coordination claim significantly and supports classifying the persistent gap as a design feature rather than an implementation shortfall.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(aggregate_harm_detectability, empirical, 'Structural capacity of use-case governance to address cumulative harm.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_human_relationship__instrumental_subsidiarity, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_h_tr_t0, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ai_h_tr_t4, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 4, 0.26).
narrative_ontology:measurement(ai_h_tr_t8, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 8, 0.32).
narrative_ontology:measurement(ai_h_tr_t12, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 12, 0.37).
narrative_ontology:measurement(ai_h_tr_t16, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 16, 0.41).
narrative_ontology:measurement(ai_h_tr_t20, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 20, 0.45).
narrative_ontology:measurement(ai_h_tr_t24, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 24, 0.48).

% Extraction over time
narrative_ontology:measurement(ai_h_be_t0, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(ai_h_be_t4, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 4, 0.27).
narrative_ontology:measurement(ai_h_be_t8, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 8, 0.31).
narrative_ontology:measurement(ai_h_be_t12, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 12, 0.35).
narrative_ontology:measurement(ai_h_be_t16, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 16, 0.38).
narrative_ontology:measurement(ai_h_be_t20, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(ai_h_be_t24, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 24, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(ai_h_su_t0, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(ai_h_su_t4, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 4, 0.2).
narrative_ontology:measurement(ai_h_su_t8, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 8, 0.22).
narrative_ontology:measurement(ai_h_su_t12, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 12, 0.24).
narrative_ontology:measurement(ai_h_su_t16, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 16, 0.26).
narrative_ontology:measurement(ai_h_su_t20, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 20, 0.27).
narrative_ontology:measurement(ai_h_su_t24, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 24, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_human_relationship__instrumental_subsidiarity, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_human_relationship__instrumental_subsidiarity, 0.1).
narrative_ontology:affects_constraint(ai_human_relationship__instrumental_subsidiarity, ai_human_relationship__technocratic_optimization).
narrative_ontology:affects_constraint(ai_human_relationship__instrumental_subsidiarity, ai_human_relationship__incarnational_humanism).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the ai_human_relationship kernel. instrumental_subsidiarity (this file) treats AI as morally neutral with subsidiarity as procedural safeguard; technocratic_optimization drops the governance constraint and treats efficiency maximization as the operative end; incarnational_humanism rejects the neutrality premise and requires evaluation against integral human development and the common good. All three share the same underlying technology and regulatory environment but instantiate structurally distinct constraints with different beneficiary/victim structures and different epsilon profiles — they are not the same constraint viewed from different angles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
