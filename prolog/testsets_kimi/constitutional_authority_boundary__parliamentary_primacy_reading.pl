% ============================================================================
% CONSTRAINT STORY: constitutional_authority_boundary__parliamentary_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_authority_boundary__parliamentary_primacy_reading, []).

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
 *   constraint_id: constitutional_authority_boundary__parliamentary_primacy_reading
 *   human_readable: Parliamentary Primacy Reading of Constitutional Authority
 *   domain: constitutional law / political philosophy / institutional design
 *
 * SUMMARY:
 *   This constraint instantiates the parliamentary primacy reading of the
 *   constitutional authority boundary kernel. Under this reading, the
 *   constitutional textâwhere one existsâis formally and normatively
 *   subordinate to the will of the elected legislature. The legislature
 *   retains final authority to define constitutional meaning through ordinary
 *   or entrenched legislation, and the judiciary is confined to an advisory
 *   or easily-overridden role. The coordination function is the resolution of
 *   constitutional ambiguity by assigning final interpretive authority to a
 *   democratically accountable body. The asymmetric extraction is the
 *   constraint placed on judicial review and the consequent exposure of
 *   individual rights to legislative majorities. The constraint is actively
 *   enforced through constitutional convention, legislative control over
 *   court structure and jurisdiction, and professional legal socialization.
 *
 * KEY AGENTS:
 *   - elected_legislature (agenda_setter/beneficiary): institutional power, generational horizon, sets constitutional meaning and benefits from unconstrained democratic authority
 *   - judiciary (payer): institutional power, biographical horizon, constrained to advisory or easily-overridden review, bears loss of final interpretive authority
 *   - individual_rights_claimants (payer): powerless, immediate horizon, lack strong judicial protection against legislative majorities, bear exposure to democratic excess
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_authority_boundary__parliamentary_primacy_reading, 0.2).
domain_priors:suppression_score(constitutional_authority_boundary__parliamentary_primacy_reading, 0.35).
domain_priors:theater_ratio(constitutional_authority_boundary__parliamentary_primacy_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_authority_boundary__parliamentary_primacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_authority_boundary__parliamentary_primacy_reading, "Parliamentary Primacy Reading of Constitutional Authority").
narrative_ontology:topic_domain(constitutional_authority_boundary__parliamentary_primacy_reading, "constitutional law / political philosophy / institutional design").

domain_priors:requires_active_enforcement(constitutional_authority_boundary__parliamentary_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_authority_boundary__parliamentary_primacy_reading, '1b3b19b8-d58d-4de0-8958-b498cf931c15').
narrative_ontology:cs_kernel_codification('1b3b19b8-d58d-4de0-8958-b498cf931c15', formalized).
narrative_ontology:cs_authority_grounding('1b3b19b8-d58d-4de0-8958-b498cf931c15', lineage).
narrative_ontology:cs_interpretation_layer_present('1b3b19b8-d58d-4de0-8958-b498cf931c15').
narrative_ontology:cs_reading_relation('1b3b19b8-d58d-4de0-8958-b498cf931c15', constitutional_authority_boundary__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('1b3b19b8-d58d-4de0-8958-b498cf931c15', constitutional_authority_boundary__coordinate_construction_reading, forecloses).
narrative_ontology:cs_axiom('1b3b19b8-d58d-4de0-8958-b498cf931c15', foundational, parliamentary_finality_of_constitutional_meaning).
narrative_ontology:cs_axiom_status(parliamentary_finality_of_constitutional_meaning, holdable).
narrative_ontology:cs_axiom_grounding('1b3b19b8-d58d-4de0-8958-b498cf931c15', parliamentary_finality_of_constitutional_meaning, conventional).
narrative_ontology:cs_axiom('1b3b19b8-d58d-4de0-8958-b498cf931c15', foundational, electoral_accountability_trumps_judicial_expertise).
narrative_ontology:cs_axiom_status(electoral_accountability_trumps_judicial_expertise, holdable).
narrative_ontology:cs_axiom_grounding('1b3b19b8-d58d-4de0-8958-b498cf931c15', electoral_accountability_trumps_judicial_expertise, deontological).
narrative_ontology:cs_reference_frame('1b3b19b8-d58d-4de0-8958-b498cf931c15', parliamentary_sovereignty_framework).
narrative_ontology:cs_drift_state('1b3b19b8-d58d-4de0-8958-b498cf931c15', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1b3b19b8-d58d-4de0-8958-b498cf931c15', '').
narrative_ontology:cs_kernel_id(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_authority_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__parliamentary_primacy_reading, elected_legislature).
narrative_ontology:constraint_victim(constitutional_authority_boundary__parliamentary_primacy_reading, judiciary).
narrative_ontology:constraint_victim(constitutional_authority_boundary__parliamentary_primacy_reading, individual_rights_claimants).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__parliamentary_primacy_reading, parliamentary_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__parliamentary_primacy_reading, electoral_accountability_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retains final authority to define constitutional meaning through ordinary or entrenched legislation. Benefits from unconstrained democratic legitimacy and the absence of judicial veto over its enactments. Exit would require self-binding through constitutional entrenchment, which is politically and institutionally costly.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, elected_legislature, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__parliamentary_primacy_reading, elected_legislature, beneficiary).

% Exercises constitutional review only in an advisory or easily-overridden capacity. Final interpretive authority is reserved to the legislature. Judicial decisions on constitutional questions can be reversed by ordinary legislation. Exit from this subordination is constrained by professional role, appointment structure, and constitutional convention.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, judiciary, payer,
    institutional, biographical, constrained, national).

% Seek protection for individual and minority rights against legislative majorities. Under this framework, their strongest institutional shieldâbinding judicial reviewâis unavailable. They must rely on political processes rather than legal recourse, with limited ability to exit the jurisdiction or the democratic system.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, individual_rights_claimants, payer,
    powerless, immediate, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_authority_boundary__parliamentary_primacy_reading, elected_legislature).
narrative_ontology:fixing_cost_class(constitutional_authority_boundary__parliamentary_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves constitutional ambiguity and inter-branch conflict by concentrating final interpretive authority in the elected legislature, providing a single accountable arbiter for constitutional meaning.
% TRANSFER_FUNCTION: Transfers final constitutional interpretive authority from courts and diffuse legal sources to the elected legislature; moves the power to override legislative acts on constitutional grounds from judiciary to parliamentary majority.
% ABSENT_VOICES: Advocates of strong-form judicial review, entrenched constitutional limits, and supranational human rights adjudication are structurally marginalized; their arguments are heard as advisory dissent but carry no binding force against legislative will.
% DISAPPEARANCE_RATIONALE: If parliamentary sovereignty as final constitutional authority vanished, courts would immediately assert independent review power, legislative acts would face judicial scrutiny for constitutional compliance, and the architecture of government would shift toward coordinate or judicial supremacy models; the distribution of power among branches would fundamentally reorganize.
% FOUNDING_PROBLEM: The problem of constitutional ambiguity and the risk of unelected judges overriding democratic decisions; the need for a clear, accountable final arbiter that preserves democratic legitimacy and prevents inter-branch deadlock.
% FOUNDING_PROBLEM_CORROBORATION: Political constitutionalists such as Griffith and proponents of the Diceyan tradition attest the problem from within legal theory, though they are ideologically aligned with the reading. International human rights institutions and legal liberals contest the framing, attesting that the problem of majority tyranny remains unsolved and that the arrangement persists because it benefits legislative majorities rather than because it uniquely solves constitutional ambiguity.
narrative_ontology:disappearance_verdict(constitutional_authority_boundary__parliamentary_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_authority_boundary__parliamentary_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_authority_boundary__parliamentary_primacy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_authority_boundary__parliamentary_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_authority_boundary__parliamentary_primacy_reading, 0.2, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_authority_boundary__parliamentary_primacy_reading_tests).
:- end_tests(constitutional_authority_boundary__parliamentary_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.20) because the constraint does not abolish the judiciary or deny all rights protection; it merely subordinates them to legislative finality. Suppression is moderate (0.35) because strong-form judicial review is structurally suppressed as an alternative, yet courts continue to operate and occasionally resist. Theater ratio is low (0.15) because parliamentary sovereignty is largely a sincere and functional constitutional principle rather than performative maintenance. Accessibility collapse is moderate (0.40): alternatives like judicial supremacy are institutionally closed in this framework but remain conceptually available in comparative discourse. Resistance is moderate (0.45) because legal scholars and rights advocates continuously contest the reading, though courts within the tradition typically acquiesce.
 *
 * PERSPECTIVAL GAP:
 *   From the legislative seat, the constraint appears as necessary democratic coordinationâwithout it, unelected judges would frustrate the people's will. From the judicial seat, the same structure reads as institutional subordination that extracts interpretive independence and converts courts into advisory bodies. From the rights-claimant seat, the constraint appears as exposure to majority tyranny with limited recourse. The engine computes these divergent classifications from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The elected legislature is the structural beneficiary (d near 0.0): it receives interpretive authority and the power to define constitutional limits without external veto. The judiciary is a structural target (d near 1.0): it loses the power of final constitutional review and must defer to parliamentary enactments. Individual rights claimants are also targets (d near 1.0) because the constraint removes their strongest institutional shield against legislative majorities. The directionality derives from beneficiary/victim declarations combined with constrained exit options for the target seats.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint prevents mandatrophy mislabeling because it carries a live coordination functionâresolving constitutional ambiguity through a clear, accountable arbiterâwhile also documenting asymmetric extraction. A pure snare reading would ignore the genuine coordination benefit of democratic finality; a pure rope reading would ignore the costs to judicial independence and minority protection. The Tangled Rope classification captures both. If the coordination function atrophied and the legislature maintained the arrangement purely to avoid judicial scrutiny of politically convenient legislation, the constraint would drift toward Snare; the theater_ratio and base_extractiveness measurements would signal this.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Does the constitutional text logically require parliamentary primacy, or is this one of several coherent readings of an underdetermined kernel?',
    'Comparative constitutional analysis and textual archaeology of the specific constitutional instrument to determine whether the text fixes a single authority structure or permits multiple stable readings.',
    'If the text underdetermines the reading, the constraint is a conventional coordination arrangement rather than a necessary legal implication; this lowers the epistemic status from mountain-like necessity to constructed institution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether parliamentary primacy is textually determined or one reading among many.').

omega_variable(
    judicial_subordination_mechanism,
    'Is judicial deference to parliamentary sovereignty enforced by institutional sanctions or by internalized professional legal culture?',
    'Historical analysis of judicial appointments, parliamentary responses to adverse decisions, and judicial rhetoric in jurisdictions operating under parliamentary sovereignty.',
    'If internalized, effective suppression exceeds structural measures because judges carry the constraint with them; if purely structural, the constraint weakens when enforcement signals fade.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_subordination_mechanism, empirical, 'Structural versus internalized suppression of judicial independence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_authority_boundary__parliamentary_primacy_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cons_tr_t12, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 12, 0.13).
narrative_ontology:measurement(cons_tr_t25, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 25, 0.18).
narrative_ontology:measurement(cons_tr_t38, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 38, 0.22).
narrative_ontology:measurement(cons_tr_t50, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 50, 0.25).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(cons_be_t12, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 12, 0.17).
narrative_ontology:measurement(cons_be_t25, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 25, 0.2).
narrative_ontology:measurement(cons_be_t38, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 38, 0.22).
narrative_ontology:measurement(cons_be_t50, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 50, 0.2).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(constitutional_authority_boundary__parliamentary_primacy_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_authority_boundary__parliamentary_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_authority_boundary__parliamentary_primacy_reading, judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_authority_boundary__parliamentary_primacy_reading, coordinate_construction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the constitutional_authority_boundary constraint family. It decomposes the contested kernel into a structurally precise claim with its own Îµ, stakeholders, and classification. The Îµ-invariance principle requires separate stories for each reading because the observable (which branch holds final authority) changes the beneficiary/victim structure and the classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
