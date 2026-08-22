% ============================================================================
% CONSTRAINT STORY: liability_attribution__developer_liability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_liability_attribution__developer_liability, []).

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
 *   constraint_id: liability_attribution__developer_liability
 *   human_readable: Developer Primary Liability Reading
 *   domain: legal/regulatory/technology
 *
 * SUMMARY:
 *   This constraint is the developer_liability reading of the
 *   liability_attribution kernel. It treats developers as the primary bearers
 *   of legal liability for harms arising from technology they create, even
 *   when deployers control the context of use. Under this reading, developers
 *   enter the victim set under regulatory and tort pressure, deployers become
 *   beneficiaries of externalized risk, and opacity is framed as a
 *   developer-side burden to manage or disclose. Sibling readings include
 *   deployer_liability and shared_liability.
 *
 * KEY AGENTS:
 *   - technology_developers: Primary target (moderate power, constrained exit) â bears extraction via liability assignment
 *   - deployer_entities: Primary beneficiary (powerful, mobile exit) â captures risk externalization
 *   - regulatory_bodies: Agenda setter (institutional, analytical exit) â administers the liability framework
 *   - open_source_developers: Excluded voice (powerless, trapped) â swept into liability without representation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liability_attribution__developer_liability, 0.68).
domain_priors:suppression_score(liability_attribution__developer_liability, 0.62).
domain_priors:theater_ratio(liability_attribution__developer_liability, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liability_attribution__developer_liability, extractiveness, 0.68).
narrative_ontology:constraint_metric(liability_attribution__developer_liability, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(liability_attribution__developer_liability, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(liability_attribution__developer_liability, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(liability_attribution__developer_liability, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_attribution__developer_liability, tangled_rope).
narrative_ontology:human_readable(liability_attribution__developer_liability, "Developer Primary Liability Reading").
narrative_ontology:topic_domain(liability_attribution__developer_liability, "legal/regulatory/technology").

domain_priors:requires_active_enforcement(liability_attribution__developer_liability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_attribution__developer_liability, '4576d2f7-d677-43a3-ae4b-91526cbf9a73').
narrative_ontology:cs_kernel_codification('4576d2f7-d677-43a3-ae4b-91526cbf9a73', formalized).
narrative_ontology:cs_authority_grounding('4576d2f7-d677-43a3-ae4b-91526cbf9a73', lineage).
narrative_ontology:cs_interpretation_layer_present('4576d2f7-d677-43a3-ae4b-91526cbf9a73').
narrative_ontology:cs_reading_relation('4576d2f7-d677-43a3-ae4b-91526cbf9a73', liability_attribution__deployer_liability, coexists_with).
narrative_ontology:cs_reading_relation('4576d2f7-d677-43a3-ae4b-91526cbf9a73', liability_attribution__shared_liability, coexists_with).
narrative_ontology:cs_axiom('4576d2f7-d677-43a3-ae4b-91526cbf9a73', foundational, creator_bears_primary_risk).
narrative_ontology:cs_axiom_status(creator_bears_primary_risk, holdable).
narrative_ontology:cs_axiom_grounding('4576d2f7-d677-43a3-ae4b-91526cbf9a73', creator_bears_primary_risk, instrumental).
narrative_ontology:cs_axiom('4576d2f7-d677-43a3-ae4b-91526cbf9a73', foundational, opacity_is_upstream_controllable).
narrative_ontology:cs_axiom_status(opacity_is_upstream_controllable, holdable).
narrative_ontology:cs_axiom_grounding('4576d2f7-d677-43a3-ae4b-91526cbf9a73', opacity_is_upstream_controllable, empirically_contingent).
narrative_ontology:cs_reference_frame('4576d2f7-d677-43a3-ae4b-91526cbf9a73', developer_as_risk_source).
narrative_ontology:cs_drift_state('4576d2f7-d677-43a3-ae4b-91526cbf9a73', post_generative_ai_regulatory_wave, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4576d2f7-d677-43a3-ae4b-91526cbf9a73', '').
narrative_ontology:cs_kernel_id(liability_attribution__developer_liability, liability_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, deployer_entities).
narrative_ontology:constraint_victim(liability_attribution__developer_liability, technology_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Create underlying AI models, software libraries, or general-purpose tools that may be deployed by others. Under this liability reading, they face primary tort and regulatory liability for harms arising from downstream deployment, regardless of whether they control the deployment context. They must carry insurance, invest in preemptive safety auditing, or absorb damages. Exit is constrained because liability attaches to the act of creation itself, which cannot be easily relocated or contractually waived against public policy.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, technology_developers, payer,
    moderate, biographical, constrained, national).

% Deploy technology developed by others into specific contexts and user-facing applications. Under developer-primary liability, they externalize liability risk to the creators of the underlying capability, even though they control the deployment environment, user interaction, and fine-tuning. They benefit from lower insurance costs and reduced direct exposure to tort claims while retaining deployment discretion.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, deployer_entities, beneficiary,
    powerful, biographical, mobile, national).

% Draft and enforce liability frameworks for technology governance. They assign primary liability to developers on the theory that upstream control over model architecture and training data is the most efficient locus for safety investment. They maintain the rule through enforcement actions, standards-setting, and statutory updates.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, regulatory_bodies, agenda_setter,
    institutional, generational, analytical, national).

% Individual or small-team creators of open-source tools who lack legal resources to defend against liability claims and are structurally absent from regulatory drafting processes, yet their creations are swept into developer-liability regimes.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, open_source_developers, excluded,
    powerless, biographical, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Assigns responsibility for technology-related harm to the party that created the underlying capability, theoretically incentivizing safer design and clarifying who must answer for defects.
% TRANSFER_FUNCTION: Moves liability risk and compliance cost from deployers (who control deployment context) to developers (who created the capability), allowing deployers to externalize downside risk.
% ABSENT_VOICES: Open-source developers and individual toolmakers are structurally excluded from regulatory drafting; deployers are present but argue for alternative allocations; end-users harmed by deployments are heard through tort plaintiffs but do not shape the upstream liability assignment.
% DISAPPEARANCE_RATIONALE: If developer-primary liability vanished overnight, deployers would absorb liability risk, contractual terms between developers and deployers would reprice dramatically, insurance markets would shift coverage from development-phase to deployment-phase, and the incentive structure for building general-purpose tools versus application-specific systems would change.
% FOUNDING_PROBLEM: Uncertainty about who compensates victims and pays for remediation when complex technological systems cause harm, and the need to incentivize upstream safety investment.
% FOUNDING_PROBLEM_CORROBORATION: Tort law scholars and regulatory impact assessments outside the deployer community attest that the harm-compensation problem is real, but dispute whether assigning primary liability to developers optimally solves it; open-source advocacy groups and some technology developers argue the problem is misattributed and that deployer control makes them the more efficient liability bearer.
narrative_ontology:disappearance_verdict(liability_attribution__developer_liability, world_rearranges).
narrative_ontology:founding_problem_status(liability_attribution__developer_liability, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(liability_attribution__developer_liability, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(liability_attribution__developer_liability, 'none', 1).
narrative_ontology:epsilon_provenance(liability_attribution__developer_liability, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(liability_attribution__developer_liability_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(liability_attribution__developer_liability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(liability_attribution__developer_liability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is substantial (0.68 at interval end) because developers assume liability costs that may exceed their control over downstream use. Suppression is moderately high (0.62) because alternative arrangements such as contractual waivers or full deployer indemnity are often void as against public policy or displaced by statute. Theater ratio is moderate (0.30): some safety investment is genuine, but a portion of developer compliance activity is performative documentation designed to satisfy liability rules rather than reduce real risk. Resistance (0.55) reflects ongoing industry and scholarly pushback against developer-primary framing.
 *
 * PERSPECTIVAL GAP:
 *   The developer seat experiences the constraint as extractive because liability attaches at the moment of creation and is hard to contract away. The deployer seat experiences it as beneficial risk coordination because deployment-side exposure is minimized. The regulatory seat experiences it as genuine coordination toward safer technology. The engine computes these divergences from structural data rather than authored claims.
 *
 * DIRECTIONALITY LOGIC:
 *   Technology developers are declared victims (role: payer) with constrained exit options, placing their directionality near the full-target end; effective extraction is amplified for them. Deployer entities are declared beneficiaries with mobile exit, placing their directionality near the beneficiary end; effective extraction is damped or inverted into subsidy. Regulatory bodies occupy an analytical seat with limited direct extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â compensating victims of technological harm and incentivizing safety â remains live. However, the specific developer-primary reading risks mandatrophy if empirical evidence shows that deployer control over context, fine-tuning, and user interaction is the dominant causal factor in realized harms. Should that evidence solidify while the developer-primary rule persists, the constraint would drift toward pure extraction or piton status.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_sibling_delta,
    'How would the classification change if the kernel were read under the deployer_liability or shared_liability framing instead of developer_liability?',
    'Compare the beneficiary/victim sets and directionality vectors across the three sibling readings; the deployer_liability reading would invert the developer/deployer positions, while shared_liability would flatten extraction across the value chain.',
    'If the kernel is better captured by a sibling reading, this constraint''s epsilon and claimed_type are misattributed to a reading-specific artifact rather than a stable structural feature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_sibling_delta, conceptual, 'Structural sensitivity of the constraint to kernel reading choice').

omega_variable(
    opacity_burden_naturality,
    'Is the developer''s burden to manage or disclose opacity a natural feature of creating complex systems, or a constructed liability expansion that benefits deployers?',
    'Empirical comparison of liability costs between transparent and opaque systems, and analysis of whether deployer control over training data and deployment context actually creates the opacity.',
    'If opacity is deployment-created, the developer-liability reading extracts from developers for a condition they do not control.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opacity_burden_naturality, empirical, 'Whether opacity burden is naturally developer-owned or constructed').

omega_variable(
    deployer_beneficiary_ambiguity,
    'Does the developer-liability reading genuinely benefit deployers structurally, or does it merely shift visibility while deployers pay via higher vendor prices and indemnification demands?',
    'Examine contractual terms and insurance pricing between developers and deployers to see whether liability costs are passed through.',
    'If deployers pay indirectly, the effective extraction is less asymmetric than the victim/beneficiary structure suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deployer_beneficiary_ambiguity, empirical, 'Whether deployer benefit from externalized risk is net or pass-through').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_attribution__developer_liability, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liab_tr_t0, liability_attribution__developer_liability, theater_ratio, 0, 0.15).
narrative_ontology:measurement(liab_tr_t4, liability_attribution__developer_liability, theater_ratio, 4, 0.18).
narrative_ontology:measurement(liab_tr_t8, liability_attribution__developer_liability, theater_ratio, 8, 0.21).
narrative_ontology:measurement(liab_tr_t12, liability_attribution__developer_liability, theater_ratio, 12, 0.24).
narrative_ontology:measurement(liab_tr_t16, liability_attribution__developer_liability, theater_ratio, 16, 0.27).
narrative_ontology:measurement(liab_tr_t20, liability_attribution__developer_liability, theater_ratio, 20, 0.3).

% Extraction over time
narrative_ontology:measurement(liab_be_t0, liability_attribution__developer_liability, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(liab_be_t4, liability_attribution__developer_liability, base_extractiveness, 4, 0.5).
narrative_ontology:measurement(liab_be_t8, liability_attribution__developer_liability, base_extractiveness, 8, 0.56).
narrative_ontology:measurement(liab_be_t12, liability_attribution__developer_liability, base_extractiveness, 12, 0.62).
narrative_ontology:measurement(liab_be_t16, liability_attribution__developer_liability, base_extractiveness, 16, 0.66).
narrative_ontology:measurement(liab_be_t20, liability_attribution__developer_liability, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(liab_su_t0, liability_attribution__developer_liability, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(liab_su_t4, liability_attribution__developer_liability, suppression_requirement, 4, 0.5).
narrative_ontology:measurement(liab_su_t8, liability_attribution__developer_liability, suppression_requirement, 8, 0.54).
narrative_ontology:measurement(liab_su_t12, liability_attribution__developer_liability, suppression_requirement, 12, 0.58).
narrative_ontology:measurement(liab_su_t16, liability_attribution__developer_liability, suppression_requirement, 16, 0.6).
narrative_ontology:measurement(liab_su_t20, liability_attribution__developer_liability, suppression_requirement, 20, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
