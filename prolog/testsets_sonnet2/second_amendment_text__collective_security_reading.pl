% ============================================================================
% CONSTRAINT STORY: second_amendment_text__collective_security_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__collective_security_reading, []).

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
 *   constraint_id: second_amendment_text__collective_security_reading
 *   human_readable: Second Amendment — Militia-Conditioned Collective Security Reading
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This story instantiates the collective-security reading of the Second
 *   Amendment's text: the prefatory militia clause ('A well regulated
 *   Militia, being necessary to the security of a free State') is read as
 *   conditioning the operative clause ('the right of the people to keep and
 *   bear Arms, shall not be infringed'), such that the constitutionally
 *   protected activity is organized, state-supervised civic defense rather
 *   than unorganized individual possession. Under this reading the state's
 *   regulatory apparatus is a structural beneficiary — its licensing and
 *   permitting authority over arms is constitutionally grounded rather than
 *   merely a police-power exercise contestable on textual grounds. Individual
 *   owners outside organized militia service, and civilians relying on
 *   personal arms for self-defense, become a constrained class whose claims
 *   to protection are weaker within this reading's own logic. This is one of
 *   three sibling readings of the same kernel text (second_amendment_text);
 *   the individual_right_reading and originalist_civic_virtue_reading are
 *   separate constraint stories with their own ε, beneficiary structures, and
 *   classifications — not alternative measurements of this one.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__collective_security_reading, 0.42).
domain_priors:suppression_score(second_amendment_text__collective_security_reading, 0.38).
domain_priors:theater_ratio(second_amendment_text__collective_security_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__collective_security_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_text__collective_security_reading, "Second Amendment — Militia-Conditioned Collective Security Reading").
narrative_ontology:topic_domain(second_amendment_text__collective_security_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_text__collective_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__collective_security_reading, 'd3aa6ad8-4547-4cb1-875b-1c83e617f2d7').
narrative_ontology:cs_kernel_codification('d3aa6ad8-4547-4cb1-875b-1c83e617f2d7', fixed_text).
narrative_ontology:cs_authority_grounding('d3aa6ad8-4547-4cb1-875b-1c83e617f2d7', lineage).
narrative_ontology:cs_interpretation_layer_present('d3aa6ad8-4547-4cb1-875b-1c83e617f2d7').
narrative_ontology:cs_reading_relation('d3aa6ad8-4547-4cb1-875b-1c83e617f2d7', second_amendment_text__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('d3aa6ad8-4547-4cb1-875b-1c83e617f2d7', second_amendment_text__originalist_civic_virtue_reading, coexists_with).
narrative_ontology:cs_axiom('d3aa6ad8-4547-4cb1-875b-1c83e617f2d7', foundational, prefatory_clause_is_binding_condition).
narrative_ontology:cs_axiom_status(prefatory_clause_is_binding_condition, holdable).
narrative_ontology:cs_axiom_grounding('d3aa6ad8-4547-4cb1-875b-1c83e617f2d7', prefatory_clause_is_binding_condition, conventional).
narrative_ontology:cs_axiom('d3aa6ad8-4547-4cb1-875b-1c83e617f2d7', foundational, organized_state_supervision_is_constitutive_of_protected_arms_bearing).
narrative_ontology:cs_axiom_status(organized_state_supervision_is_constitutive_of_protected_arms_bearing, holdable).
narrative_ontology:cs_axiom_grounding('d3aa6ad8-4547-4cb1-875b-1c83e617f2d7', organized_state_supervision_is_constitutive_of_protected_arms_bearing, conventional).
narrative_ontology:cs_reference_frame('d3aa6ad8-4547-4cb1-875b-1c83e617f2d7', militia_conditioned_civic_defense_framework).
narrative_ontology:cs_drift_state('d3aa6ad8-4547-4cb1-875b-1c83e617f2d7', post_heller_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('d3aa6ad8-4547-4cb1-875b-1c83e617f2d7', '').
narrative_ontology:cs_kernel_id(second_amendment_text__collective_security_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__collective_security_reading, state_regulatory_apparatus).
narrative_ontology:constraint_beneficiary(second_amendment_text__collective_security_reading, law_enforcement_agencies).
narrative_ontology:constraint_beneficiary(second_amendment_text__collective_security_reading, organized_militia_successor_institutions).
narrative_ontology:constraint_victim(second_amendment_text__collective_security_reading, individual_gun_owners_outside_organized_service).
narrative_ontology:constraint_victim(second_amendment_text__collective_security_reading, self_defense_focused_civilians).
narrative_ontology:constraint_vindicates(second_amendment_text__collective_security_reading, collective_security_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_text__collective_security_reading, state_police_power_over_arms).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers licensing regimes, permit-to-purchase systems, and registration requirements, justified by reading the operative clause as conditioned on the prefatory militia clause. Gains expanded regulatory jurisdiction over who may keep and bear arms and under what organizational auspices, and enforces this through courts and administrative agencies.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, state_regulatory_apparatus, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_text__collective_security_reading, state_regulatory_apparatus, beneficiary).

% Benefit operationally from a legal framework that treats civilian arms-bearing as presumptively regulable rather than presumptively protected, giving them broader discretion in enforcement, seizure, and permit denial decisions.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, law_enforcement_agencies, beneficiary,
    institutional, generational, analytical, national).

% The National Guard and state defense forces are treated as the constitutionally contemplated 'well regulated militia,' giving their organized, state-supervised structure a monopoly claim on the amendment's core protected activity — individual, unorganized possession is read as outside that core.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, organized_militia_successor_institutions, beneficiary,
    organized, generational, constrained, national).

% Are not enrolled in any organized militia body and hold arms for personal reasons. Under this reading, their claim to constitutional protection is weakened or absent, subjecting them to licensing burdens, waiting periods, and possession restrictions they cannot contest on pure textual grounds. Exit means either compliance, relocation to a friendlier jurisdiction, or political mobilization.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, individual_gun_owners_outside_organized_service, payer,
    moderate, biographical, constrained, national).

% Individuals in high-crime or rural areas who rely on personal firearm ownership for immediate self-protection bear the practical cost of this reading most acutely: permit denial or delay directly affects their capacity to defend themselves in situations the militia framework was never designed to address.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, self_defense_focused_civilians, payer,
    powerless, immediate, trapped, local).

% Argue the operative clause is severable from the prefatory militia clause and reject the collective-security frame entirely. Their objection is a matter of public record in litigation and legislative testimony, but within THIS reading's own commitment structure their premise is treated as historically mistaken, not merely disagreed with.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, gun_rights_advocacy_organizations, excluded,
    organized, generational, mobile, national).

% Study founding-era militia statutes, ratification debates, and contemporaneous state constitutions to assess whether the prefatory clause was understood as a limiting condition or an explanatory-but-non-restrictive preamble. Produce competing historical accounts that different readings selectively invoke.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates civic defense capacity through state-supervised, organized bodies rather than an atomized population of independently armed individuals — solving the collective-action problem of ensuring trained, accountable, disciplined defense forces answerable to democratic institutions.
% TRANSFER_FUNCTION: Moves interpretive authority and regulatory discretion over arms-bearing from individual claimants to state institutions; moves the practical burden of proving a constitutionally protected purpose onto individual owners who fall outside organized militia service.
% ABSENT_VOICES: Individual rights advocates and self-defense-focused civilians who read the operative clause as freestanding are not absent from the broader political conversation, but within this reading's own commitment framework their premise — that the prefatory clause is non-restrictive — is treated as a rejected historical claim rather than a live alternative inside the same interpretive system.
% DISAPPEARANCE_RATIONALE: If this reading's dominance in a jurisdiction disappeared overnight, licensing regimes premised on militia-conditioning would lose their constitutional grounding, courts would need to re-derive permit and registration authority from other sources (police power generally, rather than a militia-conditioned right), and the burden of justification would shift from individual owners to the state.
% FOUNDING_PROBLEM: Newly independent states feared both a standing federal army that could be turned against the states and disorganized, untrained armed populations incapable of coordinated defense; the militia clause was meant to guarantee that organized, state-regulated bodies of citizen-soldiers would remain the primary vehicle of collective defense, checking federal military power without relying on unorganized individual armament.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians outside both advocacy camps document that early militia statutes required near-universal enrollment and mandated privately-owned arms, complicating a purely institutional reading; some historians corroborate the collective-security framing as consistent with founding-era distrust of standing armies, while others attest the organized-militia function was substantially superseded by the professionalized National Guard system created in 1903, making the constitutional 'problem' as originally conceived largely resolved outside the reading's own advocates.
narrative_ontology:disappearance_verdict(second_amendment_text__collective_security_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_text__collective_security_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_text__collective_security_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_text__collective_security_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_text__collective_security_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__collective_security_reading_tests).
:- end_tests(second_amendment_text__collective_security_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.42) is moderate: the reading does genuinely solve a coordination problem (organized, accountable, trained defense capacity is a real public good) but it also imposes real costs on individuals who fall outside the coordinated structure, particularly those whose self-defense needs are immediate and personal rather than collective. Suppression (0.38) reflects the active enforcement licensing regimes require but is well below what a pure extraction constraint would show, because organized militia successor institutions and law enforcement genuinely benefit from and rely on the coordinated structure — it is not purely a cover story. Resistance is high (0.72) because this reading is one of the most actively contested constitutional interpretations in American law, defended and attacked in ongoing litigation.
 *
 * DIRECTIONALITY LOGIC:
 *   State regulatory apparatus and organized militia successor institutions sit near the beneficiary end: they gain expanded jurisdiction and constitutional legitimacy for regulation, with analytical/institutional exit options meaning the constraint costs them little. Individual gun owners outside organized service and self-defense-focused civilians sit toward the target end: their constitutional claim is structurally weakened by this reading, and their exit options range from constrained (relocate, comply) to trapped (immediate self-defense need, no time to litigate or relocate). The powerless self-defense-focused civilian seat is the sharpest directional asymmetry in the story.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — distrust of standing armies, need for organized citizen-soldier defense — was substantially addressed by the professionalization of the National Guard in 1903 and the permanent standing military the Constitution's framers feared. Yet this reading's regulatory apparatus persists and has intensified (suppression_requirement rising from 0.10 to 0.38 over the interval) even as the militia function it claims to serve has been institutionally absorbed elsewhere. This is exactly the divergence the R5 corroboration surfaces: is the collective-security reading defending a live founding problem, or using an obsolete militia rationale to sustain expanded regulatory reach? The tangled_rope classification captures both halves honestly — genuine coordination function (organized defense is a real good) plus asymmetric extraction (individual owners bear costs the state apparatus does not).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prefatory_clause_restrictive_function,
    'Does the prefatory militia clause operate as a legally restrictive condition on the operative clause, or as a non-binding explanatory preamble stating one purpose among others?',
    'Comparative analysis of contemporaneous state constitutional provisions using similar prefatory-clause structures, and founding-era legal treatises on how such clauses were construed in statutory and constitutional interpretation.',
    'If restrictive, this reading''s core premise holds and the individual_right_reading is foreclosed within an originalist-textualist framework. If merely explanatory, this reading''s premise collapses and the individual_right_reading becomes the textually dominant one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prefatory_clause_restrictive_function, conceptual, 'Whether the militia clause is a legal condition or a non-binding preamble — the central interpretive fork of the kernel.').

omega_variable(
    militia_institutional_succession,
    'Has the constitutionally contemplated militia function been fully absorbed by the National Guard and professional military institutions, or does an individual/unorganized militia obligation persist alongside them?',
    'Historical and legal analysis of the Militia Act of 1903 (Dick Act) and subsequent statutes defining ''unorganized militia'' status for civilians, cross-referenced against this reading''s claim that organized service is the operative locus of protection.',
    'If the unorganized militia concept retains independent legal force, this reading''s exclusion of individual owners from core protection weakens considerably. If fully absorbed, the reading''s beneficiary structure (state apparatus, organized successor institutions) is more secure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_institutional_succession, empirical, 'Whether an independent unorganized-militia status survives to anchor individual claims within this reading''s own framework.').

omega_variable(
    reading_selection_indeterminacy,
    'Is the collective-security reading the most textually and historically defensible framing among the three sibling readings, or is its selection here itself a contestable interpretive choice not compelled by the text?',
    'None fully resolves this — it is a live jurisprudential dispute reflected in split circuit court reasoning prior to Heller (2008) and continued academic disagreement after.',
    'This omega documents that authoring this reading as a discrete constraint does not assert its correctness; it asserts only that the reading is a coherent, structurally distinct claim worth modeling on its own terms, per the ε-invariance decomposition principle.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_indeterminacy, conceptual, 'Meta-level acknowledgment that reading selection among the three sibling constraints is itself contested.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__collective_security_reading, 1789, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1789, second_amendment_text__collective_security_reading, theater_ratio, 1789, 0.05).
narrative_ontology:measurement(seco_tr_t1900, second_amendment_text__collective_security_reading, theater_ratio, 1900, 0.08).
narrative_ontology:measurement(seco_tr_t1939, second_amendment_text__collective_security_reading, theater_ratio, 1939, 0.12).
narrative_ontology:measurement(seco_tr_t1980, second_amendment_text__collective_security_reading, theater_ratio, 1980, 0.16).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_text__collective_security_reading, theater_ratio, 2008, 0.19).
narrative_ontology:measurement(seco_tr_t2025, second_amendment_text__collective_security_reading, theater_ratio, 2025, 0.22).

% Extraction over time
narrative_ontology:measurement(seco_be_t1789, second_amendment_text__collective_security_reading, base_extractiveness, 1789, 0.15).
narrative_ontology:measurement(seco_be_t1900, second_amendment_text__collective_security_reading, base_extractiveness, 1900, 0.2).
narrative_ontology:measurement(seco_be_t1939, second_amendment_text__collective_security_reading, base_extractiveness, 1939, 0.25).
narrative_ontology:measurement(seco_be_t1980, second_amendment_text__collective_security_reading, base_extractiveness, 1980, 0.32).
narrative_ontology:measurement(seco_be_t2008, second_amendment_text__collective_security_reading, base_extractiveness, 2008, 0.38).
narrative_ontology:measurement(seco_be_t2025, second_amendment_text__collective_security_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1789, second_amendment_text__collective_security_reading, suppression_requirement, 1789, 0.1).
narrative_ontology:measurement(seco_su_t1900, second_amendment_text__collective_security_reading, suppression_requirement, 1900, 0.15).
narrative_ontology:measurement(seco_su_t1939, second_amendment_text__collective_security_reading, suppression_requirement, 1939, 0.2).
narrative_ontology:measurement(seco_su_t1980, second_amendment_text__collective_security_reading, suppression_requirement, 1980, 0.26).
narrative_ontology:measurement(seco_su_t2008, second_amendment_text__collective_security_reading, suppression_requirement, 2008, 0.32).
narrative_ontology:measurement(seco_su_t2025, second_amendment_text__collective_security_reading, suppression_requirement, 2025, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__collective_security_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_text__collective_security_reading, 0.12).
narrative_ontology:affects_constraint(second_amendment_text__collective_security_reading, second_amendment_text__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_text__collective_security_reading, second_amendment_text__originalist_civic_virtue_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the second_amendment_text kernel. All three share the same underlying constitutional text but instantiate structurally distinct constraints with different ε, beneficiary/victim sets, and classifications: collective_security_reading (this story, tangled_rope — state regulatory apparatus benefits, individual owners outside organized service pay), individual_right_reading (expected rope/mountain-leaning — individual self-defense as core protected activity, minimal state-benefit asymmetry), and originalist_civic_virtue_reading (expected scaffold or rope — universal armed citizenry as civic-virtue infrastructure). Per the ε-invariance principle, these are not one constraint measured three ways; they are three constraints sharing a contested kernel, linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
