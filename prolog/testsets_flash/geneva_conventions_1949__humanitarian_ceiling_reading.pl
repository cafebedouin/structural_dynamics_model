% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__humanitarian_ceiling_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_1949__humanitarian_ceiling_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: geneva_conventions_1949__humanitarian_ceiling_reading
 *   human_readable: Geneva Conventions (1949): Humanitarian Ceiling Reading
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint represents the 'humanitarian ceiling' reading of the 1949
 *   Geneva Conventions, which posits that the conventions establish absolute,
 *   non-reciprocal minimum standards for the conduct of armed conflict. These
 *   standards constrain state violence regardless of adversary compliance or
 *   perceived security threats, prioritizing the protection of civilians and
 *   those hors de combat. This reading places an asymmetric burden on state
 *   militaries, suppressing security rationales that would otherwise justify
 *   degrading protections. The claimed type is 'tangled_rope' because it
 *   genuinely coordinates a global standard for humane warfare
 *   (beneficiaries) but does so by extracting significant operational
 *   flexibility and security advantages from state actors (victims),
 *   requiring active enforcement to hold.
 *
 * KEY AGENTS:
 *   - civilian_populations: Primary beneficiary (powerless/trapped) — receive protection.
 *   - detained_combatants: Primary beneficiary (powerless/trapped) — receive humane treatment.
 *   - medical_personnel: Beneficiary (moderate/constrained) — protected in their mission.
 *   - state_militaries: Primary payer (institutional/constrained) — bear asymmetric burden.
 *   - security_rationales: Conceptual payer (institutional/identity_locked) — suppressed in favor of humanitarianism.
 *   - international_courts: Agenda setter (institutional/analytical) — enforce and interpret the conventions.
 *   - human_rights_advocates: Observer (organized/mobile) — monitor and lobby for adherence.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__humanitarian_ceiling_reading, 0.65).
domain_priors:suppression_score(geneva_conventions_1949__humanitarian_ceiling_reading, 0.78).
domain_priors:theater_ratio(geneva_conventions_1949__humanitarian_ceiling_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__humanitarian_ceiling_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_1949__humanitarian_ceiling_reading, "Geneva Conventions (1949): Humanitarian Ceiling Reading").
narrative_ontology:topic_domain(geneva_conventions_1949__humanitarian_ceiling_reading, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(geneva_conventions_1949__humanitarian_ceiling_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__humanitarian_ceiling_reading, 'f1daaba2-8ac0-44bd-a9d7-6bd5e6d7277e').
narrative_ontology:cs_kernel_codification('f1daaba2-8ac0-44bd-a9d7-6bd5e6d7277e', fixed_text).
narrative_ontology:cs_authority_grounding('f1daaba2-8ac0-44bd-a9d7-6bd5e6d7277e', lineage).
narrative_ontology:cs_interpretation_layer_present('f1daaba2-8ac0-44bd-a9d7-6bd5e6d7277e').
narrative_ontology:cs_reading_relation('f1daaba2-8ac0-44bd-a9d7-6bd5e6d7277e', geneva_conventions_1949__conditional_reciprocity_reading, coexists_with).
narrative_ontology:cs_reading_relation('f1daaba2-8ac0-44bd-a9d7-6bd5e6d7277e', geneva_conventions_1949__security_maximization_reading, coexists_with).
narrative_ontology:cs_axiom('f1daaba2-8ac0-44bd-a9d7-6bd5e6d7277e', foundational, humanitarian_imperative_absolute).
narrative_ontology:cs_axiom_status(humanitarian_imperative_absolute, holdable).
narrative_ontology:cs_axiom_grounding('f1daaba2-8ac0-44bd-a9d7-6bd5e6d7277e', humanitarian_imperative_absolute, deontological).
narrative_ontology:cs_axiom('f1daaba2-8ac0-44bd-a9d7-6bd5e6d7277e', foundational, non_reciprocity_principle).
narrative_ontology:cs_axiom_status(non_reciprocity_principle, holdable).
narrative_ontology:cs_axiom_grounding('f1daaba2-8ac0-44bd-a9d7-6bd5e6d7277e', non_reciprocity_principle, conventional).
narrative_ontology:cs_reference_frame('f1daaba2-8ac0-44bd-a9d7-6bd5e6d7277e', universal_humanitarian_law).
narrative_ontology:cs_drift_state('f1daaba2-8ac0-44bd-a9d7-6bd5e6d7277e', contemporary_asymmetric_conflict_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f1daaba2-8ac0-44bd-a9d7-6bd5e6d7277e', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__humanitarian_ceiling_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, civilian_populations).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, detained_combatants).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, medical_personnel).
narrative_ontology:constraint_victim(geneva_conventions_1949__humanitarian_ceiling_reading, state_militaries).
narrative_ontology:constraint_victim(geneva_conventions_1949__humanitarian_ceiling_reading, security_rationales).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive protection from direct targeting, indiscriminate attacks, and disproportionate harm. Their safety is prioritized above military advantage, even when caught in conflict zones. They have no direct means to enforce the conventions.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, civilian_populations, beneficiary,
    powerless, immediate, trapped, local).

% Are entitled to humane treatment, due process, and protection from torture or degrading treatment, regardless of their combatant status. Their protections are absolute, not contingent on reciprocity.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, detained_combatants, beneficiary,
    powerless, immediate, trapped, local).

% Are protected from attack and granted immunity to provide care to all wounded, without distinction. Their neutrality and mission are sacrosanct, even when operating in enemy territory.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, medical_personnel, beneficiary,
    moderate, biographical, constrained, local).

% Bear the asymmetric burden of adhering to absolute humanitarian standards, even when facing adversaries who do not. This constrains their operational flexibility, intelligence gathering, and targeting decisions, potentially increasing their own casualties or prolonging conflict. They must actively enforce these rules within their ranks.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, state_militaries, payer,
    institutional, biographical, constrained, global).

% The imperative for state security and military advantage is structurally subordinated to humanitarian principles. This means foregoing certain tactics or intelligence gains that would violate the conventions, even if they might enhance security. This is a conceptual 'payer' as a set of justifications that are suppressed.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, security_rationales, payer,
    institutional, generational, identity_locked, universal).
narrative_ontology:stakeholder_non_agent(geneva_conventions_1949__humanitarian_ceiling_reading, security_rationales).

% Interpret and enforce the conventions, holding states and individuals accountable for violations. They actively promote the humanitarian ceiling reading through jurisprudence and prosecution, acting as a primary institutional force for its persistence.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, international_courts, agenda_setter,
    institutional, generational, analytical, global).

% Monitor compliance, document violations, and lobby for stricter adherence to the humanitarian ceiling. They provide critical external pressure and analysis, shaping public discourse and influencing international legal bodies.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, human_rights_advocates, observer,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal, non-reciprocal framework for the conduct of armed conflict, ensuring a baseline of humanity and protection for non-combatants and those hors de combat, preventing a race to the bottom in wartime brutality.
% TRANSFER_FUNCTION: Transfers operational flexibility and potential security advantages from state militaries to civilian populations and detained combatants, in exchange for a universal standard of humane conduct in war.
% ABSENT_VOICES: Military strategists and political leaders prioritizing 'total victory' or 'national survival' above all else would argue for greater flexibility in applying humanitarian law, especially against non-state actors. Their voices are often marginalized in international legal discourse but remain powerful in national policy debates.
% DISAPPEARANCE_RATIONALE: If this reading of the Geneva Conventions vanished, the conduct of armed conflict would rapidly degrade. State militaries would face fewer constraints, leading to increased civilian casualties, widespread abuse of prisoners, and a general erosion of any shared understanding of 'humane' warfare. The international legal and moral landscape would be fundamentally reshaped.
% FOUNDING_PROBLEM: The horrors of World War II, particularly the widespread targeting of civilians and mistreatment of prisoners, demonstrated the catastrophic consequences of a lack of universal, non-reciprocal humanitarian standards in armed conflict.
% FOUNDING_PROBLEM_CORROBORATION: International humanitarian organizations, UN bodies, and numerous academic studies consistently attest to the ongoing relevance of the founding problem, citing contemporary conflicts where the absence or violation of these standards leads to immense suffering. This corroboration comes from outside the direct beneficiaries (civilian populations) and payers (state militaries) of the conventions.
narrative_ontology:disappearance_verdict(geneva_conventions_1949__humanitarian_ceiling_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_1949__humanitarian_ceiling_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__humanitarian_ceiling_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(geneva_conventions_1949__humanitarian_ceiling_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_1949__humanitarian_ceiling_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_1949__humanitarian_ceiling_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_1949__humanitarian_ceiling_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_1949__humanitarian_ceiling_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because state militaries must forgo significant tactical advantages and bear increased risks to adhere to these absolute standards. Suppression is also high (0.78) as this reading actively suppresses security-maximization rationales and requires continuous enforcement against state interests that would prefer more flexibility. Theater ratio is moderate (0.40) because while there is genuine humanitarian work, there's also a performative aspect where states publicly commit to the ceiling while privately seeking loopholes or exceptions. The temporal measurements show a gradual increase in both extractiveness and suppression, reflecting the growing tension between humanitarian ideals and the realities of asymmetric warfare, where states feel increasingly constrained.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of civilian populations and detained combatants, this reading is a vital 'rope' providing essential protection. From the perspective of state militaries, it operates as a 'snare' or 'tangled rope,' imposing significant costs and constraints on their operations without guaranteed reciprocity. International courts and human rights advocates view it as a 'rope' or 'scaffold' for building a more humane international order. The engine's per-seat classification will reflect these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Civilian populations, detained combatants, and medical personnel are clear beneficiaries (low d) as the conventions directly protect them. State militaries and 'security rationales' are targets/payers (high d) because they bear the costs of compliance and have their operational freedom curtailed. International courts are agenda setters (d near symmetric) as they enforce the rules but also derive legitimacy from upholding them. Human rights advocates are observers (d near symmetric) as they promote the constraint but are not directly subject to its operational costs in the same way as militaries.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the constraint as a pure 'snare' by acknowledging its genuine coordination function in establishing universal humanitarian standards. However, it also prevents mislabeling it as a pure 'rope' by recognizing the substantial, asymmetric extraction from state militaries and the active suppression of security-first rationales. The rising theater ratio suggests a risk of future mandatrophy if the gap between declared adherence and actual practice widens, turning genuine coordination into mere performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reciprocity_vs_absolutism,
    'To what extent is the ''humanitarian ceiling'' reading genuinely non-reciprocal in practice, versus being subtly influenced by expectations of reciprocity from state actors?',
    'Empirical study of state military doctrine and operational decisions in conflicts where adversaries demonstrably violate humanitarian law: does state behavior degrade in response, or maintain the ceiling?',
    'If reciprocity is a strong implicit factor, the ''humanitarian ceiling'' is less of an absolute constraint and more of a conditional one, shifting its classification closer to a ''conditional reciprocity'' reading, potentially reducing its effective suppression and increasing its perceived extractiveness for state militaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_vs_absolutism, empirical, 'Ambiguity of non-reciprocal application in practice.').

omega_variable(
    security_rationale_suppression_efficacy,
    'How effectively does this reading suppress security-maximization rationales in actual state decision-making, particularly in asymmetric conflicts?',
    'Analysis of classified military legal advice, internal policy debates, and post-conflict reviews: do security rationales consistently yield to humanitarian concerns, or are they merely reframed to appear compliant?',
    'If security rationales are merely reframed rather than suppressed, the effective suppression of the constraint is lower than measured, and its theater ratio is higher, indicating a greater degree of performative compliance rather than genuine adherence. This would push the classification closer to a ''piton'' or a more extractive ''tangled rope''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_rationale_suppression_efficacy, empirical, 'Efficacy of suppressing security rationales.').

omega_variable(
    reading_framing_underdetermination,
    'Is the ''humanitarian ceiling'' reading the most defensible framing of the Geneva Conventions, or does the ''conditional reciprocity'' or ''security maximization'' reading offer an equally coherent, albeit different, interpretation?',
    'Conceptual analysis of the conventions'' text, drafting history, and subsequent state practice, alongside philosophical arguments for the nature of international law. This is a debate over the core interpretive methodology.',
    'If an alternative reading is found to be equally coherent, the classification of the Geneva Conventions as a whole becomes more contested, and the ''humanitarian ceiling'' reading''s claim to universal applicability is weakened. This would highlight the deep conceptual divisions within international humanitarian law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_framing_underdetermination, conceptual, 'Alternative coherent readings of the Geneva Conventions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__humanitarian_ceiling_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1949, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 1949, 0.1).
narrative_ontology:measurement(gene_tr_t1965, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 1965, 0.15).
narrative_ontology:measurement(gene_tr_t1980, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(gene_tr_t1995, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 1995, 0.3).
narrative_ontology:measurement(gene_tr_t2010, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 2010, 0.35).
narrative_ontology:measurement(gene_tr_t2024, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(gene_be_t1949, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 1949, 0.5).
narrative_ontology:measurement(gene_be_t1965, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 1965, 0.55).
narrative_ontology:measurement(gene_be_t1980, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 1980, 0.6).
narrative_ontology:measurement(gene_be_t1995, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 1995, 0.63).
narrative_ontology:measurement(gene_be_t2010, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 2010, 0.64).
narrative_ontology:measurement(gene_be_t2024, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1949, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 1949, 0.6).
narrative_ontology:measurement(gene_su_t1965, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 1965, 0.65).
narrative_ontology:measurement(gene_su_t1980, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 1980, 0.7).
narrative_ontology:measurement(gene_su_t1995, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 1995, 0.75).
narrative_ontology:measurement(gene_su_t2010, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 2010, 0.77).
narrative_ontology:measurement(gene_su_t2024, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__humanitarian_ceiling_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(geneva_conventions_1949__humanitarian_ceiling_reading, 0.1).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 1949 Geneva Conventions kernel, each representing a distinct structural claim about the conventions' operation and impact.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
