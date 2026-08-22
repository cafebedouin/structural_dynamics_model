% ============================================================================
% CONSTRAINT STORY: eu_council_unanimity__sovereignty_guarantor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_council_unanimity__sovereignty_guarantor_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: eu_council_unanimity__sovereignty_guarantor_reading
 *   human_readable: EU Council Unanimity as Sovereignty Guarantor
 *   domain: institutional_design/international_relations
 *
 * SUMMARY:
 *   This story instantiates the sovereignty_guarantor reading of the EU
 *   Council unanimity kernel: the requirement that every member state consent
 *   to action on treaty change, taxation, foreign policy, and other
 *   sovereignty-implicating matters is read here as the operative mechanism
 *   of sovereign equality, not as an extraction vulnerability. On this
 *   reading, every veto exercise is a legitimate rights-exercise by the state
 *   deploying it, the beneficiary set is coextensive with the full membership
 *   (every state holds the identical guarantee), and the ε is authored
 *   moderate — coordination costs from needing full consent are real and
 *   rising slowly as membership has grown, but no state is depicted as
 *   extracting rents from another through the mechanism. This is one of three
 *   linked readings of the same kernel (eu_council_unanimity); the
 *   diplomatic_capital_reading treats unanimity as a
 *   negotiation-strengthening consensus mechanism, and the veto_trap_reading
 *   treats the identical rule as enabling minoritarian extraction via
 *   credible blocking threats. Each reading is authored as its own constraint
 *   with its own ε and stakeholder structure per the ε-invariance principle;
 *   this file does not adjudicate between them.
 *
 * KEY AGENTS:
 *   - small_member_states: beneficiary, holds veto equal to larger states
 *   - sovereignty_sensitive_states: beneficiary, uses veto to protect domestic constitutional commitments
 *   - founding_treaty_order: non-agent beneficiary, the sovereign-equality settlement itself
 *   - council_presidency_and_secretariat: agenda_setter, administers consensus process
 *   - larger_member_states: payer, bears coordination cost as legitimate price of sovereign equality
 *   - eu_citizens_and_residents: observer, no direct seat at the table
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__sovereignty_guarantor_reading, 0.28).
domain_priors:suppression_score(eu_council_unanimity__sovereignty_guarantor_reading, 0.15).
domain_priors:theater_ratio(eu_council_unanimity__sovereignty_guarantor_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__sovereignty_guarantor_reading, rope).
narrative_ontology:human_readable(eu_council_unanimity__sovereignty_guarantor_reading, "EU Council Unanimity as Sovereignty Guarantor").
narrative_ontology:topic_domain(eu_council_unanimity__sovereignty_guarantor_reading, "institutional_design/international_relations").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__sovereignty_guarantor_reading, '1d37a3e8-8c5e-423a-a830-4b61702f1fed').
narrative_ontology:cs_kernel_codification('1d37a3e8-8c5e-423a-a830-4b61702f1fed', formalized).
narrative_ontology:cs_authority_grounding('1d37a3e8-8c5e-423a-a830-4b61702f1fed', lineage).
narrative_ontology:cs_interpretation_layer_present('1d37a3e8-8c5e-423a-a830-4b61702f1fed').
narrative_ontology:cs_reading_relation('1d37a3e8-8c5e-423a-a830-4b61702f1fed', eu_council_unanimity__veto_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('1d37a3e8-8c5e-423a-a830-4b61702f1fed', eu_council_unanimity__diplomatic_capital_reading, influences).
narrative_ontology:cs_axiom('1d37a3e8-8c5e-423a-a830-4b61702f1fed', foundational, veto_exercise_is_always_legitimate_rights_exercise).
narrative_ontology:cs_axiom_status(veto_exercise_is_always_legitimate_rights_exercise, holdable).
narrative_ontology:cs_axiom_grounding('1d37a3e8-8c5e-423a-a830-4b61702f1fed', veto_exercise_is_always_legitimate_rights_exercise, deontological).
narrative_ontology:cs_axiom('1d37a3e8-8c5e-423a-a830-4b61702f1fed', foundational, beneficiary_set_is_universal_and_symmetric_across_member_states).
narrative_ontology:cs_axiom_status(beneficiary_set_is_universal_and_symmetric_across_member_states, holdable).
narrative_ontology:cs_axiom_grounding('1d37a3e8-8c5e-423a-a830-4b61702f1fed', beneficiary_set_is_universal_and_symmetric_across_member_states, conventional).
narrative_ontology:cs_reference_frame('1d37a3e8-8c5e-423a-a830-4b61702f1fed', treaty_founding_sovereign_equality_settlement).
narrative_ontology:cs_drift_state('1d37a3e8-8c5e-423a-a830-4b61702f1fed', post_enlargement_contemporary, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('1d37a3e8-8c5e-423a-a830-4b61702f1fed', '').
narrative_ontology:cs_kernel_id(eu_council_unanimity__sovereignty_guarantor_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_council_unanimity__sovereignty_guarantor_reading, small_member_states).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__sovereignty_guarantor_reading, sovereignty_sensitive_states).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__sovereignty_guarantor_reading, founding_treaty_order).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(eu_council_unanimity__sovereignty_guarantor_reading, larger_member_states).
narrative_ontology:constraint_vindicates(eu_council_unanimity__sovereignty_guarantor_reading, state_consent_doctrine).
narrative_ontology:constraint_vindicates(eu_council_unanimity__sovereignty_guarantor_reading, sovereign_equality_of_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the same veto as larger states on matters requiring unanimity — foreign policy, taxation, treaty change, enlargement. Without unanimity, population-weighted or qualified-majority rules would let a coalition of larger states impose outcomes on them regardless of their objection. The veto is their only structural guarantee that their consent, not just their voice, is required.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, small_member_states, beneficiary,
    moderate, generational, constrained, continental).

% Use unanimity requirements to protect domestic constitutional commitments (neutrality, tax sovereignty, defense posture) from being overridden by collective decision. They exercise the veto rarely but its availability is what makes deeper integration politically acceptable domestically.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, sovereignty_sensitive_states, beneficiary,
    moderate, generational, constrained, continental).

% The treaty-based structure that constituted the Union as a union of sovereign states rather than a federal state depends on unanimity for its foundational acts (accession, treaty amendment). This is not an actor but the legal-political settlement itself, which the unanimity rule sustains.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, founding_treaty_order, beneficiary,
    institutional, civilizational, analytical, continental).
narrative_ontology:stakeholder_non_agent(eu_council_unanimity__sovereignty_guarantor_reading, founding_treaty_order).

% Schedules and chairs unanimity votes, mediates among member states, and administers the consensus-seeking process. Does not itself hold a veto but structures the negotiation sequence within which vetoes are exercised or withheld.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, council_presidency_and_secretariat, agenda_setter,
    institutional, biographical, analytical, continental).

% Bear the coordination cost of needing every state's consent even when a large majority favors action. From this reading's perspective this cost is the legitimate price of respecting sovereign equality, not extraction — larger states accept slower or blocked action as the tradeoff for a union none of them could dominate unilaterally.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, larger_member_states, payer,
    powerful, biographical, constrained, continental).

% Experience the downstream effects of unanimity-gated action or inaction (e.g., delayed sanctions packages, stalled fiscal measures) but do not hold a seat at the Council table. Their interests are represented only through their national government's veto or consent.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, eu_citizens_and_residents, observer,
    powerless, generational, trapped, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eu_council_unanimity__sovereignty_guarantor_reading, diffuse).
narrative_ontology:fixing_cost_class(eu_council_unanimity__sovereignty_guarantor_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unanimity solves the genuine problem of how sovereign states can bind themselves to collective action on matters touching core sovereignty (foreign policy, treaty change, taxation, defense) without any state being coerced into commitments it did not consent to — it is the mechanism by which sovereign equality is made operative rather than merely declared.
% TRANSFER_FUNCTION: The arrangement does not transfer value between states; it withholds collective action absent consent. What moves is not a resource but a veto right, held equally and identically by every state regardless of size or power, which no majority — however large — can override on unanimity-gated matters.
% ABSENT_VOICES: EU citizens and residents, non-state actors, and supranational institutions (Commission, Parliament) that favor faster or deeper integration are not parties to the consent requirement and cannot compel a reluctant state's assent; from this reading, that exclusion is exactly what sovereignty protection requires — extending the franchise to non-state parties would dissolve the guarantee it exists to provide.
% DISAPPEARANCE_RATIONALE: If unanimity disappeared on sovereignty-implicating matters, smaller and sovereignty-sensitive states would lose the structural guarantee that collective action cannot be imposed on them without their consent; qualified-majority voting would let coalitions of larger or more populous states set foreign policy, taxation, or treaty terms for the whole, fundamentally altering the character of the Union from a voluntary union of sovereign equals toward a majoritarian federation.
% FOUNDING_PROBLEM: The founding problem was how to construct a union among states of vastly different size and power without recreating, at the European level, the majoritarian coercion that sovereign statehood was supposed to protect against — how to get pooled decision-making without any state losing its sovereign veto over matters it deems existential.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional courts in multiple member states (notably rulings addressing the limits of EU competence over national sovereignty) have independently affirmed that unanimity on treaty-level and core-sovereignty matters remains a live constitutional safeguard, not a vestige; this corroboration comes from domestic judicial bodies outside the Council itself and outside any single beneficiary state's own advocacy.
narrative_ontology:disappearance_verdict(eu_council_unanimity__sovereignty_guarantor_reading, world_rearranges).
narrative_ontology:founding_problem_status(eu_council_unanimity__sovereignty_guarantor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eu_council_unanimity__sovereignty_guarantor_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(eu_council_unanimity__sovereignty_guarantor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eu_council_unanimity__sovereignty_guarantor_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_council_unanimity__sovereignty_guarantor_reading_tests).
:- end_tests(eu_council_unanimity__sovereignty_guarantor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.28) and rising only slightly over three decades of enlargement — coordination costs increase as membership grows (more parties whose consent must be secured), but this reading finds no systematic transfer from any state to any other through the mechanism itself. Suppression is low (0.15): no state is coerced to vote a particular way, and the entire point of the mechanism is that it cannot be overridden by pressure from others. Accessibility collapse is low (0.2) and resistance is moderate-low (0.3): states retain real alternatives (opt-outs, enhanced cooperation, treaty renegotiation) and while frustration with slow unanimity-gated processes is real, this reading treats that frustration as the acceptable cost of the guarantee rather than evidence the constraint should be dismantled.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seat (Council presidency), the mechanism is a genuine, if sometimes frustrating, consensus-forcing procedure. From any single member state's seat, whether large or small, the identical structure looks like an entitlement they personally hold — the difference in seat experience is about whose interest the veto happens to serve in a given vote, not about whether the underlying guarantee is legitimate. This reading holds the guarantee is legitimate from every seat; it is the veto_trap_reading, authored separately, that locates a payer/victim asymmetry in the identical structural facts.
 *
 * DIRECTIONALITY LOGIC:
 *   All member states are declared beneficiaries under this reading because the veto is held identically and universally — small states benefit from parity with large ones, and even larger states benefit from the reciprocal guarantee that they too cannot be steamrolled on matters where they hold minority views. Larger states are marked as payer because they most often bear the friction of needing unanimous consent when they favor faster action, but this reading treats that friction as the coordination cost of the arrangement, not as extraction directed at them — there is no victim group in this story.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protecting sovereign equality against majoritarian coercion in a union of unequal-sized states) remains live: enlargement to 27+ members has, if anything, intensified rather than dissolved the original problem, since size disparities among members are now larger, not smaller, than at founding. This reading finds no mandatrophy — the mandate has not outlived its function; it treats the mechanism as ongoing constitutional protection rather than a vestige.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_defense_vs_extraction_boundary,
    'Is every historical instance of veto use in this dataset genuinely defensive (protecting a state''s sovereignty interest) or does some subset constitute credible-blocking-threat extraction of side payments, which would be better captured by the veto_trap_reading?',
    'Case-by-case process tracing of specific unanimity votes: did the vetoing state receive a side payment or concession causally linked to lifting its veto, or did it simply withhold consent without extracting compensation?',
    'If a substantial share of veto exercises are shown to extract side payments, this reading''s claim of a universal, symmetric beneficiary set with no extraction would be undermined for that subset of cases, and those cases would belong structurally to the veto_trap_reading rather than this one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_defense_vs_extraction_boundary, empirical, 'Whether observed veto exercises are uniformly defensive or partly extractive.').

omega_variable(
    natural_vs_constructed_sovereignty_norm,
    'Is sovereign equality among states a pre-political fact that unanimity merely respects, or is it itself a constructed convention that the unanimity rule helps manufacture and sustain?',
    'Historical and comparative analysis of alternative international arrangements (e.g., weighted voting in the IMF, UN Security Council permanent-member veto) that instantiate different sovereignty norms; convergence or divergence across regimes would inform whether sovereign equality is a discovered principle or an institutional artifact.',
    'If sovereign equality is itself constructed rather than natural, the sovereignty_guarantor reading''s normative force weakens somewhat, since the ''protection'' it offers protects a convention rather than a fact — though this would not by itself convert the reading into an extraction story.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_vs_constructed_sovereignty_norm, conceptual, 'Whether the sovereignty norm underlying this reading is natural or constructed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__sovereignty_guarantor_reading, 1993, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_c_tr_t1993, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 1993, 0.1).
narrative_ontology:measurement(eu_c_tr_t2000, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(eu_c_tr_t2007, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 2007, 0.11).
narrative_ontology:measurement(eu_c_tr_t2014, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 2014, 0.11).
narrative_ontology:measurement(eu_c_tr_t2020, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 2020, 0.12).
narrative_ontology:measurement(eu_c_tr_t2024, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 2024, 0.12).

% Extraction over time
narrative_ontology:measurement(eu_c_be_t1993, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 1993, 0.22).
narrative_ontology:measurement(eu_c_be_t2000, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 2000, 0.24).
narrative_ontology:measurement(eu_c_be_t2007, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 2007, 0.25).
narrative_ontology:measurement(eu_c_be_t2014, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 2014, 0.26).
narrative_ontology:measurement(eu_c_be_t2020, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 2020, 0.27).
narrative_ontology:measurement(eu_c_be_t2024, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 2024, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(eu_council_unanimity__sovereignty_guarantor_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__sovereignty_guarantor_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(eu_council_unanimity__sovereignty_guarantor_reading, 0.12).
narrative_ontology:affects_constraint(eu_council_unanimity__sovereignty_guarantor_reading, eu_council_unanimity__veto_trap_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__sovereignty_guarantor_reading, eu_council_unanimity__diplomatic_capital_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the eu_council_unanimity kernel. sovereignty_guarantor_reading (this file) authors moderate ε with a universal, symmetric beneficiary set and no victims. veto_trap_reading authors higher ε with an identified victim set (states extracted from via blocking threats). diplomatic_capital_reading authors ε driven by negotiation/legitimacy dynamics rather than sovereignty protection or extraction. All three share the identical formal Council rule as their subject but diverge on beneficiary structure, victim presence, and the normative status of veto exercise — per the ε-invariance principle, they are authored as separate constraint stories linked here rather than as one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
