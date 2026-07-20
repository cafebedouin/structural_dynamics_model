% ============================================================================
% CONSTRAINT STORY: derivative_work_statutory_boundary__coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_derivative_work_statutory_boundary__coordination_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: derivative_work_statutory_boundary__coordination_reading
 *   human_readable: Derivative Work Boundary â Coordination Reading (Transformative Use Safe Harbor)
 *   domain: intellectual_property/technology_governance
 *
 * SUMMARY:
 *   This constraint instantiates the coordination reading of the
 *   derivative-work statutory boundary kernel. It holds that only fixed
 *   recastings substantially incorporating original expression qualify as
 *   derivative works, while transformative and intermediate uses remain
 *   non-infringing. This reading functions as a low-extraction coordination
 *   mechanism: it removes ex-ante licensing friction for follow-on creators,
 *   remix artists, and generative-technology developers, treating the
 *   statutory boundary as a scaffold for cumulative innovation rather than an
 *   enclosure device. The kernel is contested: the enclosure reading treats
 *   any use as infringing, and the hybrid carveout reading would bifurcate
 *   permission by commerciality. This story isolates the coordination reading
 *   as a structurally distinct constraint with its own Îµ.
 *
 * KEY AGENTS:
 *   - transformative_creators: Primary beneficiary (moderate/mobile) â shielded from infringement for remix and appropriation art
 *   - generative_technology_sector: Primary beneficiary (powerful/mobile) â trains models and builds tools without individual licensing
 *   - copyright_holders: Structural payer (powerful/constrained) â absorb reduced licensing leverage as the price of a functional creative ecosystem
 *   - federal_judiciary: Agenda-setter (institutional/analytical) â interprets the statutory boundary through case law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__coordination_reading, 0.2).
domain_priors:suppression_score(derivative_work_statutory_boundary__coordination_reading, 0.18).
domain_priors:theater_ratio(derivative_work_statutory_boundary__coordination_reading, 0.14).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, theater_ratio, 0.14).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__coordination_reading, rope).
narrative_ontology:human_readable(derivative_work_statutory_boundary__coordination_reading, "Derivative Work Boundary â Coordination Reading (Transformative Use Safe Harbor)").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__coordination_reading, "intellectual_property/technology_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__coordination_reading, '90eccb24-0c59-4b73-ab2a-4039184eb297').
narrative_ontology:cs_kernel_codification('90eccb24-0c59-4b73-ab2a-4039184eb297', fixed_text).
narrative_ontology:cs_authority_grounding('90eccb24-0c59-4b73-ab2a-4039184eb297', lineage).
narrative_ontology:cs_interpretation_layer_present('90eccb24-0c59-4b73-ab2a-4039184eb297').
narrative_ontology:cs_reading_relation('90eccb24-0c59-4b73-ab2a-4039184eb297', derivative_work_statutory_boundary__enclosure_reading, forecloses).
narrative_ontology:cs_reading_relation('90eccb24-0c59-4b73-ab2a-4039184eb297', derivative_work_statutory_boundary__hybrid_carveout_reading, coexists_with).
narrative_ontology:cs_axiom('90eccb24-0c59-4b73-ab2a-4039184eb297', foundational, transformative_use_non_infringing).
narrative_ontology:cs_axiom_status(transformative_use_non_infringing, holdable).
narrative_ontology:cs_axiom_grounding('90eccb24-0c59-4b73-ab2a-4039184eb297', transformative_use_non_infringing, conventional).
narrative_ontology:cs_axiom('90eccb24-0c59-4b73-ab2a-4039184eb297', foundational, substantial_incorporation_threshold).
narrative_ontology:cs_axiom_status(substantial_incorporation_threshold, holdable).
narrative_ontology:cs_axiom_grounding('90eccb24-0c59-4b73-ab2a-4039184eb297', substantial_incorporation_threshold, conventional).
narrative_ontology:cs_reference_frame('90eccb24-0c59-4b73-ab2a-4039184eb297', follow_on_creativity_regime).
narrative_ontology:cs_drift_state('90eccb24-0c59-4b73-ab2a-4039184eb297', generative_ai_litigation_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('90eccb24-0c59-4b73-ab2a-4039184eb297', '').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__coordination_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, transformative_creators).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, generative_technology_sector).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__coordination_reading, copyright_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Create remixes, appropriation art, and follow-on works that build on existing copyrighted expression. Under this reading, their transformative uses are shielded from derivative-work infringement claims without ex-ante licensing, allowing them to publish and distribute without negotiating with upstream rights-holders.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, transformative_creators, beneficiary,
    moderate, biographical, mobile, national).

% Develop and train machine-learning models and generative tools that ingest copyrighted works as intermediate inputs. Rely on the statutory boundary that intermediate copying and transformative outputs do not trigger the derivative-work right, avoiding billions of individual licensing transactions.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, generative_technology_sector, beneficiary,
    powerful, generational, mobile, global).

% Hold exclusive rights in original expressive works. Under this reading, they forfeit licensing leverage over transformative and intermediate uses; their exit is constrained because they cannot unilaterally opt out of the statutory safe harbor for follow-on uses that do not substantially incorporate the original's expression.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, copyright_holders, payer,
    powerful, biographical, constrained, global).

% Interpret the statutory derivative-work definition through case law, drawing the boundary between infringing recasting and permissible transformation. They administer the safe harbor by adjudicating fair-use and derivative-work claims, but do not directly collect or pay the constraint's costs.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(derivative_work_statutory_boundary__coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(derivative_work_statutory_boundary__coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the transaction-cost barrier to cumulative creativity and technological innovation by providing a legal safe harbor: creators and systems can build on existing expressive works without individualized licensing negotiations, as long as the use is transformative or intermediate rather than a fixed recasting.
% TRANSFER_FUNCTION: Moves the freedom to transform and intermediate-use copyrighted expression from rights-holders to subsequent creators and technology developers, preserving only the monopoly over verbatim reproduction and substantially similar recasting of the original fixed expression.
% ABSENT_VOICES: Strict enclosure advocatesârights-holder lobbies and some originalist copyright scholarsâargue that any use of expression should require authorization; their position is heard in dissenting litigation briefs and legislative testimony but is not reflected in the prevailing judicial doctrine.
% DISAPPEARANCE_RATIONALE: If the permissive boundary vanished overnight, every generative training run, remix, and transformative adaptation would require bilateral licensing with upstream rights-holders; the generative-technology sector and remix ecosystems would stall under holdout and transaction costs, while courts would be submerged in infringement claims over intermediate copying.
% FOUNDING_PROBLEM: The copyright system risked over-enclosing culture: if every use of existing expression required a license, cumulative creativity, technological innovation, and follow-on authorship would be paralyzed by transaction costs, strategic holdout, and the impossibility of tracing all upstream rights.
% FOUNDING_PROBLEM_CORROBORATION: Technology economists, innovation scholars, and the software and AI research communities attest that permissive boundaries are structurally necessary for cumulative and generative innovation; their analysis is independent of the copyright industries and supports the coordination reading.
narrative_ontology:disappearance_verdict(derivative_work_statutory_boundary__coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(derivative_work_statutory_boundary__coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(derivative_work_statutory_boundary__coordination_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(derivative_work_statutory_boundary__coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(derivative_work_statutory_boundary__coordination_reading, 0.2, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(derivative_work_statutory_boundary__coordination_reading_tests).
:- end_tests(derivative_work_statutory_boundary__coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.20 at interval end) because the constraint deliberately leaves value on the table for rights-holders by exempting transformative and intermediate uses from the derivative-work right. Suppression is low (0.18) because the constraint increases the option set for follow-on users rather than coercively suppressing alternatives. Theater ratio is low (0.14) because judicial administration of the boundary is functional precedent-setting, not performative compliance. Accessibility collapse is moderate-low (0.25): understanding the constraint opens creative and technological alternatives rather than closing them, though legal uncertainty around new technologies keeps some alternatives latent. Resistance is low (0.20) because the prevailing doctrinal consensus supports the coordination function, even as rights-holder lobbying generates friction. Measurements share a single time grid to prevent misalignment artifacts.
 *
 * PERSPECTIVAL GAP:
 *   The generative-technology sector and transformative creators experience the constraint as a genuine subsidy enabling their activity (low d, low Ï). Copyright holders experience it as a forced subsidy to downstream innovationâtheir licensing monopoly is deliberately truncated (higher d, higher Ï). The federal judiciary sits at an analytical distance, adjudicating the boundary without bearing its direct costs or gains. The engine will compute divergent seat types: beneficiaries near rope, payers potentially near tangled rope if extraction were higher, but the base Îµ is low enough that even the payer seat computes as rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (transformative_creators, generative_technology_sector) receive structural subsidy through the safe harbor; their directionality is near the beneficiary end. Copyright holders bear the cost of truncated exclusivity; their directionality trends toward the target end, though because the constraint is a rope with low Îµ, their effective extraction remains modest. The federal judiciary is analytical with no direct flow.
 *
 * MANDATROPHY ANALYSIS:
 *   The low theater ratio and stable low extractiveness prevent mislabeling this as a piton. The absence of a sunset clause prevents scaffold classification, and the absence of active enforcement or victimization prevents snare or tangled rope. Mandatrophy risk would arise if the coordination reading were abandoned in practice while the statutory text remained unchanged, but the measurements show the doctrine remains functional and resistance is minimal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Which reading of the derivative-work kernelâcoordination, enclosure, or hybrid carveoutâcorrectly describes the structural operation of copyright''s adaptation right?',
    'Comparative jurisdictional analysis of innovation outcomes, licensing-market structure, and litigation rates across regimes adopting enclosure versus coordination readings.',
    'If the enclosure reading is structurally correct, the authored low Îµ is mismeasured and effective extraction is far higher than recorded; if the coordination reading is correct, the permissive boundary is a genuine rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Structural ambiguity between coordination and enclosure readings of the same statutory kernel.').

omega_variable(
    ml_training_boundary,
    'Does large-scale machine learning training constitute a transformative or intermediate use within the safe harbor, or does it represent a novel exploitative use that overwhelms the coordination function?',
    'Judicial determinations in pending and future litigation, supplemented by empirical study of market substitution between generative outputs and source works.',
    'If ML training is excluded from the safe harbor, base_extractiveness rises sharply as the technology sector must license inputs; if included, the rope holds and generative innovation remains coordinated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ml_training_boundary, empirical, 'Whether machine learning training falls inside or outside the coordination reading''s boundary.').

omega_variable(
    commercial_noncommercial_fracture,
    'Does the coordination reading''s uniform permissive boundary hold across commercial and non-commercial uses, or does commercial exploitation of generative outputs fracture the rope into a hybrid carveout?',
    'Court rulings distinguishing commercial generative-application outputs from non-commercial research uses, and legislative proposals to introduce commercial carveouts.',
    'If commercial uses are carved out, the constraint bifurcates into a scaffold or tangled rope; if the boundary holds uniformly, it remains a rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commercial_noncommercial_fracture, conceptual, 'Whether commercial exploitation pressures split the coordination reading into a hybrid regime.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__coordination_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(derivative_coordination_tr_t0, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(derivative_coordination_tr_t8, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 8, 0.06).
narrative_ontology:measurement(derivative_coordination_tr_t16, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 16, 0.08).
narrative_ontology:measurement(derivative_coordination_tr_t24, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 24, 0.1).
narrative_ontology:measurement(derivative_coordination_tr_t32, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 32, 0.12).
narrative_ontology:measurement(derivative_coordination_tr_t40, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 40, 0.14).

% Extraction over time
narrative_ontology:measurement(derivative_coordination_be_t0, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(derivative_coordination_be_t8, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 8, 0.13).
narrative_ontology:measurement(derivative_coordination_be_t16, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 16, 0.14).
narrative_ontology:measurement(derivative_coordination_be_t24, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 24, 0.16).
narrative_ontology:measurement(derivative_coordination_be_t32, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 32, 0.18).
narrative_ontology:measurement(derivative_coordination_be_t40, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 40, 0.2).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(derivative_work_statutory_boundary__coordination_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_statutory_boundary__coordination_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, derivative_work_statutory_boundary__enclosure_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, derivative_work_statutory_boundary__hybrid_carveout_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the derivative_work_statutory_boundary kernel. It is structurally distinct from its siblings because its Îµ is low and its beneficiary structure is broad and coordination-oriented, whereas the enclosure reading has high Îµ and concentrated vindication for rights-holders, and the hybrid carveout reading has medium Îµ with bifurcated commercial/non-commercial treatment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
