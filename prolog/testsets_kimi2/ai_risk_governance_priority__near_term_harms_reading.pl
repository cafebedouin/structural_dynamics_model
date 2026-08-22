% ============================================================================
% CONSTRAINT STORY: ai_risk_governance_priority__near_term_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_governance_priority__near_term_harms_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ai_risk_governance_priority__near_term_harms_reading
 *   human_readable: AI Risk Governance Priority: Near-Term Harms Reading
 *   domain: AI Governance / Technology Ethics / Risk Assessment
 *
 * SUMMARY:
 *   This constraint story instantiates the near_term_harms_reading of the
 *   contested kernel ai_risk_governance_priority. The constraint is the
 *   institutionalized governance arrangement that prioritizes mitigating
 *   demonstrated present harmsâalgorithmic bias, misinformation, labor
 *   displacement, and surveillanceâaffecting marginalized populations. From
 *   this reading, the coordination function is genuine but captured: fairness
 *   audits and regulatory frameworks do direct resources toward harm
 *   mitigation, yet technology companies benefit asymmetrically by
 *   substituting manageable compliance for structural accountability. The
 *   beneficiary/victim structure follows the source material:
 *   technology_companies capture the governance process while
 *   global_south_populations, marginalized_groups, and displaced_workers bear
 *   the ongoing costs of extraction disguised as mitigation. The reading is
 *   structurally distinct from its sibling existential_risk_reading (which
 *   would reverse the beneficiary structure) and bridge_reading (which would
 *   deny the separability of present and future harms). Per the Îµ-invariance
 *   principle, this constraint has one stable Îµ assessed by its own lights:
 *   the standing arrangement is the present-harm governance priority itself,
 *   not the abolitionist or x-risk alternatives this reading rejects.
 *
 * KEY AGENTS:
 *   - technology_companies: Primary beneficiary (institutional/arbitrage) â captures governance through managed compliance and regulatory capture
 *   - global_south_populations: Primary target (powerless/trapped) â bears algorithmic extraction and surveillance with minimal recourse or voice
 *   - marginalized_groups: Primary target (powerless/identity_locked) â subject to discriminatory systems that governance audits but does not dismantle
 *   - displaced_workers: Target (powerless/constrained) â faces automation-driven displacement that governance acknowledges but does not prevent
 *   - regulatory_institutions: Agenda setter (institutional/constrained) â administers audits and frameworks with industry-dependent expertise
 *   - existential_risk_advocates: Excluded voice (organized/constrained) â marginalized in priority-setting despite advocating for long-term risk prevention
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__near_term_harms_reading, 0.78).
domain_priors:suppression_score(ai_risk_governance_priority__near_term_harms_reading, 0.65).
domain_priors:theater_ratio(ai_risk_governance_priority__near_term_harms_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__near_term_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_governance_priority__near_term_harms_reading, "AI Risk Governance Priority: Near-Term Harms Reading").
narrative_ontology:topic_domain(ai_risk_governance_priority__near_term_harms_reading, "AI Governance / Technology Ethics / Risk Assessment").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__near_term_harms_reading, 'a8e594b2-432e-4114-97b4-8bc1dee0350f').
narrative_ontology:cs_kernel_codification('a8e594b2-432e-4114-97b4-8bc1dee0350f', distributed).
narrative_ontology:cs_authority_grounding('a8e594b2-432e-4114-97b4-8bc1dee0350f', distributed).
narrative_ontology:cs_reading_relation('a8e594b2-432e-4114-97b4-8bc1dee0350f', ai_risk_governance_priority__existential_risk_reading, influences).
narrative_ontology:cs_reading_relation('a8e594b2-432e-4114-97b4-8bc1dee0350f', ai_risk_governance_priority__bridge_reading, coexists_with).
narrative_ontology:cs_axiom('a8e594b2-432e-4114-97b4-8bc1dee0350f', foundational, demonstrated_harms_take_precedence).
narrative_ontology:cs_axiom_status(demonstrated_harms_take_precedence, holdable).
narrative_ontology:cs_axiom_grounding('a8e594b2-432e-4114-97b4-8bc1dee0350f', demonstrated_harms_take_precedence, deontological).
narrative_ontology:cs_axiom('a8e594b2-432e-4114-97b4-8bc1dee0350f', foundational, marginalized_voices_center).
narrative_ontology:cs_axiom_status(marginalized_voices_center, holdable).
narrative_ontology:cs_axiom_grounding('a8e594b2-432e-4114-97b4-8bc1dee0350f', marginalized_voices_center, deontological).
narrative_ontology:cs_reference_frame('a8e594b2-432e-4114-97b4-8bc1dee0350f', present_harm_mitigation_focus).
narrative_ontology:cs_drift_state('a8e594b2-432e-4114-97b4-8bc1dee0350f', contemporary_ai_governance, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a8e594b2-432e-4114-97b4-8bc1dee0350f', '').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__near_term_harms_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__near_term_harms_reading, technology_companies).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, global_south_populations).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, marginalized_groups).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, displaced_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Shape AI governance agendas through lobbying, standards-setting participation, and funding of fairness audit infrastructure. They benefit from near-term harms framing because it produces manageable compliance obligations, allows continued deployment of profitable systems, and diverts regulatory attention from structural redistribution or existential risk constraints. They can exit to jurisdictions with weaker regulation or rebrand compliance as corporate social responsibility.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, technology_companies, beneficiary,
    institutional, generational, arbitrage, global).

% Experience algorithmic harm, surveillance, and labor market disruption from AI systems designed primarily in the Global North. Governance frameworks nominally address their concerns but operate without meaningful participation from affected communities. They have limited ability to exit platform ecosystems or data extraction relationships due to economic dependence and infrastructure monopoly.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, global_south_populations, payer,
    powerless, biographical, trapped, global).

% Subject to discriminatory algorithmic decision-making in hiring, lending, policing, and content moderation. The governance prioritization produces fairness audits and bias metrics that document harm without altering the power to deploy the systems. Their identities and survival needs are locked into platform ecosystems they cannot abandon without social and economic exclusion.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, marginalized_groups, payer,
    powerless, biographical, identity_locked, national).

% Face job displacement and wage suppression from automated systems. Governance acknowledges labor impacts but channels responses into retraining programs and future-of-work discourse rather than restricting corporate automation decisions. They have constrained exit options due to skill specificity and regional labor market limitations.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, displaced_workers, payer,
    powerless, biographical, constrained, national).

% Administer fairness audits, review algorithmic impact assessments, and develop regulatory frameworks for current AI systems. Their staffing and expertise depend significantly on industry secondments and funding. They enforce the near-term harms prioritization through reporting requirements and standards adoption.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, regulatory_institutions, agenda_setter,
    institutional, generational, constrained, global).

% Argue that superintelligence poses existential threats requiring governance priority. They are structurally marginalized in policy processes that have embraced near-term harms framing, receiving fewer funding and policy access opportunities. They would reorder governance priorities toward long-term risk prevention if included.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, existential_risk_advocates, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_governance_priority__near_term_harms_reading, technology_companies).
narrative_ontology:fixing_cost_class(ai_risk_governance_priority__near_term_harms_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates governance attention, regulatory resources, and research funding toward demonstrable present harms of AI deployment including algorithmic bias, misinformation propagation, labor displacement, and surveillance infrastructure.
% TRANSFER_FUNCTION: Moves institutional legitimacy, regulatory capacity, and public attention from speculative long-term risks and from community-led abolitionist approaches toward corporate-managed audit frameworks and bias mitigation protocols.
% ABSENT_VOICES: Existential risk researchers who believe superintelligence threatens human survival; community abolitionists who reject AI deployment entirely rather than seeking harm mitigation within existing power structures; Global South civil society organizations excluded from standards-setting bodies and funding allocation decisions.
% DISAPPEARANCE_RATIONALE: If the near-term harms prioritization vanished, governance resources would shift toward existential risk prevention or toward laissez-faire industry self-regulation. The fairness audit industry, bias mitigation regulatory apparatus, and associated research funding would lose their mandate, and affected communities would face altered patterns of corporate extraction.
% FOUNDING_PROBLEM: AI deployment in the 2010s produced measurable discrimination, labor disruption, and informational harm without accountability mechanisms, creating pressure for governance to address concrete harms to real populations.
% FOUNDING_PROBLEM_CORROBORATION: Affected communities and critical AI scholars attest the founding problem is live and deepening. Technology companies and mainstream governance institutions attest it is being managed through existing audit and reporting frameworks. Independent civil society audits from outside the corporate-beneficiary set corroborate that present harms persist and are structurally incentivized despite mitigation efforts.
narrative_ontology:disappearance_verdict(ai_risk_governance_priority__near_term_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_governance_priority__near_term_harms_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__near_term_harms_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_risk_governance_priority__near_term_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_governance_priority__near_term_harms_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_governance_priority__near_term_harms_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_governance_priority__near_term_harms_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_risk_governance_priority__near_term_harms_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the constraint allows continued deployment and extraction from marginalized populations while substituting audit theater for structural change. Suppression is substantial (0.65) because alternativesâexistential risk governance, abolitionist approaches, and community-led accountabilityâare structurally marginalized in funding and policy access. Theater ratio is moderate (0.45): fairness audits and bias metrics are real activities with some genuine harm reduction, but an increasing share functions as corporate ethics-washing. Accessibility collapse (0.60) reflects that while alternatives exist discursively, they lack institutional traction. Resistance (0.50) captures pushback from x-risk advocates and some community organizers who reject co-optation. Temporal measurements show extraction intensifying as the audit industry matures and corporate capture hardens.
 *
 * PERSPECTIVAL GAP:
 *   The technology_companies seat experiences the constraint as a manageable cost of doing business that legitimates continued deployment; the global_south_populations and marginalized_groups seats experience it as continued extraction with a governance veneer. The regulatory_institutions seat experiences it as a legitimate public mission with captured implementation. The engine computes this divergence from structural dataâpower, exit options, and role declarationsânot from authored type claims.
 *
 * DIRECTIONALITY LOGIC:
 *   technology_companies is declared as beneficiary with arbitrage-grade exit options, producing a strongly beneficiary-weighted directionality. global_south_populations, marginalized_groups, and displaced_workers are declared as victims with trapped or constrained exit, producing strongly target-weighted directionality. regulatory_institutions sits near the middle as agenda_setter with constrained exit. existential_risk_advocates are excluded with constrained exit, producing moderate target directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâunchecked AI deployment causing concrete harmsâremains live in the testimony of affected communities but is contested by technology companies who claim audit frameworks have addressed it. The constraint shows signs of mandatrophy if the audit apparatus persists while the underlying extraction deepens. The temporal theater_ratio trajectory (0.20 to 0.45) suggests growing performative maintenance relative to genuine harm reduction, which would support a future piton classification if the coordination function atrophies further. However, the current measurements retain enough genuine coordination that tangled_rope remains the structurally accurate claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ethics_washing_depth,
    'Does the governance prioritization of near-term harms produce substantive redistribution of power and resources to marginalized populations, or primarily generate performative compliance artifacts that benefit technology companies?',
    'Comparative longitudinal study of governance outcomes in jurisdictions with strong versus weak near-term harm regulatory frameworks, measuring material outcomes for affected communities versus audit expenditures.',
    'If primarily performative, the constraint''s extractiveness is higher than its coordination score suggests, pushing classification toward snare. If substantive, the coordination function is genuine and tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ethics_washing_depth, empirical, 'Whether near-term harm governance produces real protection or corporate ethics-washing').

omega_variable(
    suppression_of_alternatives,
    'Is the marginalization of existential risk and abolitionist framings in governance due to structural resource competition, or to epistemic capture by technology companies?',
    'Network analysis of funding flows between technology companies, governance institutions, and research organizations; discourse analysis of regulatory agendas.',
    'If epistemic capture, suppression is higher and the constraint functions more as snare. If resource competition, the dynamics are closer to normal institutional politics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_alternatives, empirical, 'Whether suppression of alternative frames is structural or captured').

omega_variable(
    kernel_reading_scope,
    'This constraint is one reading of the contested kernel ai_risk_governance_priority. A sibling existential_risk_reading would reverse the beneficiary/victim structure, treating x-risk prevention as the coordination function and near-term governance as a distraction. A bridge_reading would deny the separability of the two priorities.',
    'Evaluation requires treating each reading as a separate constraint with its own Îµ and stakeholder structure, linked via network.affects_constraints.',
    'The classification of this constraint is valid only for the near_term_harms_reading; other readings instantiate different constraints with different structural properties.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_scope, conceptual, 'Committer frame uncertainty for kernel reading decomposition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__near_term_harms_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_near_term_tr_t0, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ai_near_term_tr_t4, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 4, 0.25).
narrative_ontology:measurement(ai_near_term_tr_t8, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(ai_near_term_tr_t12, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(ai_near_term_tr_t16, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 16, 0.4).
narrative_ontology:measurement(ai_near_term_tr_t20, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 20, 0.45).

% Extraction over time
narrative_ontology:measurement(ai_near_term_be_t0, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(ai_near_term_be_t4, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 4, 0.55).
narrative_ontology:measurement(ai_near_term_be_t8, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 8, 0.62).
narrative_ontology:measurement(ai_near_term_be_t12, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 12, 0.68).
narrative_ontology:measurement(ai_near_term_be_t16, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 16, 0.73).
narrative_ontology:measurement(ai_near_term_be_t20, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 20, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(ai_near_term_su_t0, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(ai_near_term_su_t4, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 4, 0.42).
narrative_ontology:measurement(ai_near_term_su_t8, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 8, 0.5).
narrative_ontology:measurement(ai_near_term_su_t12, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 12, 0.57).
narrative_ontology:measurement(ai_near_term_su_t16, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 16, 0.62).
narrative_ontology:measurement(ai_near_term_su_t20, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__near_term_harms_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_risk_governance_priority__near_term_harms_reading, ai_risk_governance_priority__existential_risk_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__near_term_harms_reading, ai_risk_governance_priority__bridge_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel ai_risk_governance_priority. The near_term_harms_reading and its siblings (existential_risk_reading, bridge_reading) instantiate structurally distinct constraints from the same governance priority kernel. Each has different beneficiary/victim structures, Îµ values, and classifications. The kernel-level disagreement is about what AI risk governance should prioritize, but each reading emits a different constraint with different structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
