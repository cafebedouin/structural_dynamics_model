% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__constitutional_impossibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__constitutional_impossibility_reading, []).

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
 *   constraint_id: secession_legitimacy_boundary__constitutional_impossibility_reading
 *   human_readable: Constitutional Impossibility Reading of the Secession Legitimacy Boundary
 *   domain: political/legal/federalism
 *
 * SUMMARY:
 *   This story authors the constitutional-impossibility reading of a single
 *   contested kernel about secession legitimacy: unilateral secession is
 *   legally impossible, and only the amendment formula can alter the
 *   federation's territorial composition. Under this reading's own lights,
 *   there is no victim set — the doctrine does not extract from the
 *   secessionist region, because the reading holds the region never possessed
 *   an autonomous claim to exit outside the amendment channel. The sibling
 *   readings (popular sovereignty, grievance threshold, treaty primacy)
 *   instantiate structurally different constraints with different
 *   beneficiary/victim sets and are NOT part of this story's classification;
 *   they exist as separate files linked through the kernel.
 *
 * KEY AGENTS:
 *   - federal_government: agenda_setter (institutional/analytical) — administers and enforces the amendment-exclusivity rule
 *   - secessionist_provincial_movement: excluded (organized/constrained) — referendum result held legally inert
 *   - constitutional_courts: agenda_setter/beneficiary (institutional/analytical) — final legal arbiter, reinforces own authority
 *   - national_unity_constituencies: beneficiary (organized/mobile) — benefit from continued integration, no direct cost
 *   - other_provinces_and_regions: beneficiary/excluded (organized/constrained) — hold amendment veto, also excluded from unilateral resolution
 *   - comparative_constitutional_scholars: observer (analytical/analytical) — assess doctrine against comparative practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.28).
domain_priors:suppression_score(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.42).
domain_priors:theater_ratio(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__constitutional_impossibility_reading, rope).
narrative_ontology:human_readable(secession_legitimacy_boundary__constitutional_impossibility_reading, "Constitutional Impossibility Reading of the Secession Legitimacy Boundary").
narrative_ontology:topic_domain(secession_legitimacy_boundary__constitutional_impossibility_reading, "political/legal/federalism").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__constitutional_impossibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__constitutional_impossibility_reading, '9432b8fc-b20f-4a02-861b-ea94c276b634').
narrative_ontology:cs_kernel_codification('9432b8fc-b20f-4a02-861b-ea94c276b634', formalized).
narrative_ontology:cs_authority_grounding('9432b8fc-b20f-4a02-861b-ea94c276b634', lineage).
narrative_ontology:cs_interpretation_layer_present('9432b8fc-b20f-4a02-861b-ea94c276b634').
narrative_ontology:cs_reading_relation('9432b8fc-b20f-4a02-861b-ea94c276b634', secession_legitimacy_boundary__popular_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('9432b8fc-b20f-4a02-861b-ea94c276b634', secession_legitimacy_boundary__grievance_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('9432b8fc-b20f-4a02-861b-ea94c276b634', secession_legitimacy_boundary__treaty_primacy_reading, influences).
narrative_ontology:cs_axiom('9432b8fc-b20f-4a02-861b-ea94c276b634', foundational, amendment_procedure_exclusivity).
narrative_ontology:cs_axiom_status(amendment_procedure_exclusivity, holdable).
narrative_ontology:cs_axiom_grounding('9432b8fc-b20f-4a02-861b-ea94c276b634', amendment_procedure_exclusivity, conventional).
narrative_ontology:cs_axiom('9432b8fc-b20f-4a02-861b-ea94c276b634', foundational, unilateral_declaration_legally_void).
narrative_ontology:cs_axiom_status(unilateral_declaration_legally_void, holdable).
narrative_ontology:cs_axiom_grounding('9432b8fc-b20f-4a02-861b-ea94c276b634', unilateral_declaration_legally_void, conventional).
narrative_ontology:cs_reference_frame('9432b8fc-b20f-4a02-861b-ea94c276b634', federal_compact_amendment_exclusivity).
narrative_ontology:cs_drift_state('9432b8fc-b20f-4a02-861b-ea94c276b634', contemporary_referendum_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('9432b8fc-b20f-4a02-861b-ea94c276b634', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__constitutional_impossibility_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__constitutional_impossibility_reading, federal_government).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__constitutional_impossibility_reading, national_unity_constituencies).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__constitutional_impossibility_reading, constitutional_courts).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__constitutional_impossibility_reading, other_provinces_and_regions).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__constitutional_impossibility_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__constitutional_impossibility_reading, amendment_procedure_exclusivity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the constitutional order and, under this reading, holds that no province or region may exit unilaterally: the amendment procedure is the only legitimate channel, and the federal government is a necessary party to any negotiated exit. Litigates and legislates to keep unilateral referenda from having binding legal effect.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, federal_government, agenda_setter,
    institutional, generational, analytical, national).

% Holds or seeks a popular mandate within provincial boundaries to exit the federation. Under this reading, their referendum has no independent legal force; they must persuade the federal government and other provinces to open the amendment process, which gives every other party a veto over their claimed self-determination.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, secessionist_provincial_movement, excluded,
    organized, biographical, constrained, regional).

% Adjudicate the legal boundary itself, generally by ruling that unilateral declarations of independence have no domestic legal effect and that only the amendment formula can alter the federation's territorial composition. Their institutional authority and continued relevance as final arbiters is reinforced by treating the amendment path as the sole legitimate channel.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, constitutional_courts, agenda_setter,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__constitutional_impossibility_reading, constitutional_courts, beneficiary).

% Populations and interest groups outside the secessionist region who benefit from continued federal integration — shared currency, internal market, defense, and fiscal transfers. They face no direct extraction under this reading; the constraint simply preserves the arrangement they already prefer.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, national_unity_constituencies, beneficiary,
    organized, generational, mobile, national).

% Hold an effective veto over any negotiated exit via the amendment formula's ratification thresholds. Benefit from stability of the federal bargain but are also excluded from having their own preferences about a departing province's status decided without their formal consent — a double-edged position the amendment path structures deliberately.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, other_provinces_and_regions, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__constitutional_impossibility_reading, other_provinces_and_regions, excluded).

% Compare this reading's amendment-exclusivity logic against jurisdictions with explicit secession clauses, unilateral-declaration precedents, and international law's self-determination doctrine, to assess whether the impossibility reading is a genuine constitutional feature or a contingent judicial policy choice dressed as necessity.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(secession_legitimacy_boundary__constitutional_impossibility_reading, diffuse).
narrative_ontology:fixing_cost_class(secession_legitimacy_boundary__constitutional_impossibility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, predictable, rule-bound channel for altering the federation's territorial composition, preventing ad hoc unilateral exits that could destabilize currency, defense, debt allocation, and minority protections within a departing region.
% TRANSFER_FUNCTION: Under this reading there is no transfer to identify: the constraint does not move resources from a secessionist region to the federation, because the reading holds the region has no valid claim to autonomous resources in the first place. What is 'moved' is legal recognition — withheld from unilateral declarations, available only through the amendment process.
% ABSENT_VOICES: The secessionist provincial movement's referendum result is treated as politically significant but legally inert; the population that voted is present in politics but structurally absent from the legal channel that would give their vote binding effect. Indigenous treaty holders within the disputed territory are also absent from this reading's account, which speaks only to federal-provincial authority.
% DISAPPEARANCE_RATIONALE: If the constitutional-impossibility doctrine vanished overnight, federal courts and legislatures dispute whether the world rearranges (secessionist movements would immediately treat referenda as self-executing, forcing a constitutional crisis) or stays substantially the same (because international recognition and practical statehood still require negotiated settlement with the rump state regardless of domestic doctrine). The verdict depends on which downstream consequence — domestic legal effect or international recognition — is treated as the operative one.
% FOUNDING_PROBLEM: Federations historically fractured or faced war when territorial exit was treated as a bare political fact rather than a legal process — the doctrine was built to route existential disputes about the state's boundaries through a predictable, amendable procedure rather than through unilateral declaration or force.
% FOUNDING_PROBLEM_CORROBORATION: Federal governments and constitutional courts attest the founding problem — disorderly, potentially violent territorial fragmentation — remains live and the doctrine still serves it. Secessionist movements and a portion of comparative constitutional scholarship attest the doctrine has drifted from conflict-prevention into a structural veto that entrenches the status quo regardless of the underlying grievance; this dissenting attestation comes from outside the federal-government/court beneficiary set.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__constitutional_impossibility_reading, contested).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__constitutional_impossibility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__constitutional_impossibility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(secession_legitimacy_boundary__constitutional_impossibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__constitutional_impossibility_reading_tests).
:- end_tests(secession_legitimacy_boundary__constitutional_impossibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-moderate (0.28 at interval end) and rising only slightly, because from this reading's own premises the doctrine does not transfer resources or autonomy away from anyone who held a valid claim to them — the reading denies the claim exists outside the amendment channel. Suppression is moderate (0.42) because real coercive machinery (judicial injunctions against unilateral declarations, non-recognition by allied states, potential use of federal police power) stands behind the doctrine and is exercised when referenda are attempted. Accessibility collapse is high (0.7): once the doctrine is accepted, the amendment path is presented as the only legally cognizable route, closing off unilateral action as a live legal option even though it remains a live political option. Resistance is moderate (0.55): secessionist movements persistently contest the doctrine's legitimacy rather than accepting it as settled.
 *
 * PERSPECTIVAL GAP:
 *   From the federal government and courts' seats, this is functioning coordination: a rule-bound, predictable channel that prevents disorderly fragmentation. From the secessionist movement's seat — even though this story does not author them as a victim — the same structure would compute, under a different reading (popular_sovereignty_reading), as extraction of self-determination. The engine computes each seat's type from this reading's own structural data; it is not asked to average across readings.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal government, constitutional courts, and national unity constituencies are beneficiaries — the doctrine preserves the territorial and institutional status quo they hold or prefer, and none of them bear a cost under this reading's own accounting. The secessionist provincial movement is not authored as a 'victim' in base_properties because this reading's premise is precisely that no valid extraction occurs — declaring victims here would smuggle in the grievance-threshold or popular-sovereignty reading's premises. Instead the movement is authored as `excluded`: present in the political system, absent from the legal channel that would give its preference binding force. Other provinces sit in a genuinely dual position — beneficiaries of stability, but also excluded from unilaterally resolving a departing region's status without going through the same amendment gate.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing disorderly, potentially violent territorial fragmentation — is authored as contested rather than resolved as dead, because federal actors credibly maintain the problem persists (secession attempts remain live in several federations) even as critics argue the doctrine has calcified into a structural veto insulated from the merits of any particular grievance. Declaring the status contested rather than dead prevents this reading from being mislabeled as pure inertial extraction (piton) when its coordination function — an orderly amendment channel — remains actively invoked and litigated.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_text_vs_judicial_construction,
    'Is amendment-exclusivity actually written into the constitutional text, or is it a judicial construction layered onto textual silence?',
    'Comparative textual analysis: does the constitution explicitly bar unilateral secession, or does the impossibility doctrine derive from judicial interpretation of structural principles (federalism, rule of law) absent explicit text?',
    'If explicitly textual, the doctrine is closer to a genuine constitutional constraint (rope/mountain-adjacent); if constructed from silence, it is more plausibly read as a policy choice that could have gone the other way, strengthening the sibling readings'' claim that this reading is one contestable interpretation among several.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_text_vs_judicial_construction, conceptual, 'Whether amendment-exclusivity is textual or judicially constructed.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly do the four readings of the secession legitimacy boundary diverge — is it a disagreement about legal text, about the ultimate source of sovereignty, about a moral threshold, or about a prior treaty obligation?',
    'This is not resolvable by further data within any single reading; it is the structural map of the kernel contest itself. Documented here for the record: constitutional_impossibility_reading locates sovereignty in the amendment procedure and the federal-provincial compact as a whole; popular_sovereignty_reading locates it in the provincial demos; grievance_threshold_reading locates legitimacy in substantive justice outcomes rather than procedure; treaty_primacy_reading locates it in pre-existing treaty relationships that predate the federation itself.',
    'A sibling reading gaining ascendancy (e.g. through a constitutional court''s shift in doctrine, or an international law development elevating self-determination) would not change this story''s ε — it would mean a different constraint from the same kernel has become the operative one in practice. This story remains a stable description of the constitutional_impossibility_reading regardless.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Structural map of where the four kernel readings diverge; routes committer content out of the classification fields.').

omega_variable(
    no_victim_claim_contestability,
    'Is the absence of an authored victim set in this reading a defensible structural fact, or does it obscure real costs borne by the secessionist population under the guise of denying their claim''s validity?',
    'Compare economic and political outcomes for the secessionist region under continued federation versus modeled outcomes under a negotiated or unilateral exit; if the region demonstrably bears costs (fiscal transfers, resource extraction, political marginalization) that a victim-set omission conceals, that is evidence the ''no valid claim'' premise is doing normative work beyond legal description.',
    'If the region bears identifiable costs independent of the secession question, this reading''s ''no victims'' authoring choice would itself be a contested move rather than a neutral structural fact — though it would still not convert this reading into the grievance_threshold_reading, since that conversion requires adopting a different premise about what legitimates exit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(no_victim_claim_contestability, preference, 'Whether omitting victims here is neutral description or a normatively loaded choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__constitutional_impossibility_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t0, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sece_tr_t8, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement(sece_tr_t16, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 16, 0.14).
narrative_ontology:measurement(sece_tr_t24, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 24, 0.16).
narrative_ontology:measurement(sece_tr_t32, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 32, 0.18).
narrative_ontology:measurement(sece_tr_t40, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 40, 0.2).

% Extraction over time
narrative_ontology:measurement(sece_be_t0, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(sece_be_t8, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 8, 0.2).
narrative_ontology:measurement(sece_be_t16, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 16, 0.22).
narrative_ontology:measurement(sece_be_t24, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 24, 0.25).
narrative_ontology:measurement(sece_be_t32, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 32, 0.27).
narrative_ontology:measurement(sece_be_t40, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 40, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t0, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(sece_su_t8, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 8, 0.33).
narrative_ontology:measurement(sece_su_t16, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 16, 0.36).
narrative_ontology:measurement(sece_su_t24, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 24, 0.39).
narrative_ontology:measurement(sece_su_t32, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 32, 0.41).
narrative_ontology:measurement(sece_su_t40, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 40, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__constitutional_impossibility_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__constitutional_impossibility_reading, popular_sovereignty_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__constitutional_impossibility_reading, grievance_threshold_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__constitutional_impossibility_reading, treaty_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four sibling readings of the secession_legitimacy_boundary kernel. Each reading is authored as an independent constraint story with its own ε, beneficiary/victim structure, and claimed type; they are linked via affects_constraints rather than merged into a single multi-valued constraint, per the ε-invariance principle. constitutional_impossibility_reading authors near-zero extraction and no victims by its own premises; popular_sovereignty_reading and grievance_threshold_reading are expected to author federal enforcement as extractive from the secessionist region's perspective; treaty_primacy_reading is expected to author a distinct victim set (treaty-holding indigenous nations) excluded by all three other readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
