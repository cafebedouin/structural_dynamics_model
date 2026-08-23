% ============================================================================
% CONSTRAINT STORY: second_amendment_scope__collective_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_scope__collective_right_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: second_amendment_scope__collective_right_reading
 *   human_readable: Second Amendment Collective Right Reading (State Militia Authority)
 *   domain: constitutional_law/political_theory/rights_jurisprudence
 *
 * SUMMARY:
 *   This constraint story models the collective right reading of the Second
 *   Amendment as an operative constitutional constraint during its period of
 *   controlling precedent (United States v. Miller, 1939 to District of
 *   Columbia v. Heller, 2008). The reading functions as a federalism
 *   provision: it allocates military authority by protecting state militia
 *   forces from federal disarmament. The claim/metric independence is
 *   maintained — proponents of this reading claim it represents the fixed
 *   original meaning (mountain), but the authored metrics describe a
 *   coordination mechanism (rope) with low extraction and suppression that
 *   solves a genuine collective-action problem between federal and state
 *   military authority.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__collective_right_reading, 0.15).
domain_priors:suppression_score(second_amendment_scope__collective_right_reading, 0.1).
domain_priors:theater_ratio(second_amendment_scope__collective_right_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__collective_right_reading, rope).
narrative_ontology:human_readable(second_amendment_scope__collective_right_reading, "Second Amendment Collective Right Reading (State Militia Authority)").
narrative_ontology:topic_domain(second_amendment_scope__collective_right_reading, "constitutional_law/political_theory/rights_jurisprudence").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__collective_right_reading, '108ce2fa-d889-42f9-8585-105eaa1d998f').
narrative_ontology:cs_kernel_codification('108ce2fa-d889-42f9-8585-105eaa1d998f', fixed_text).
narrative_ontology:cs_authority_grounding('108ce2fa-d889-42f9-8585-105eaa1d998f', lineage).
narrative_ontology:cs_interpretation_layer_present('108ce2fa-d889-42f9-8585-105eaa1d998f').
narrative_ontology:cs_reading_relation('108ce2fa-d889-42f9-8585-105eaa1d998f', second_amendment_scope__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('108ce2fa-d889-42f9-8585-105eaa1d998f', second_amendment_scope__civic_right_reading, coexists_with).
narrative_ontology:cs_axiom('108ce2fa-d889-42f9-8585-105eaa1d998f', foundational, second_amendment_protects_state_militia_authority_only).
narrative_ontology:cs_axiom_status(second_amendment_protects_state_militia_authority_only, holdable).
narrative_ontology:cs_axiom_grounding('108ce2fa-d889-42f9-8585-105eaa1d998f', second_amendment_protects_state_militia_authority_only, conventional).
narrative_ontology:cs_axiom('108ce2fa-d889-42f9-8585-105eaa1d998f', foundational, individual_firearms_ownership_not_constitutionally_protected).
narrative_ontology:cs_axiom_status(individual_firearms_ownership_not_constitutionally_protected, holdable).
narrative_ontology:cs_axiom_grounding('108ce2fa-d889-42f9-8585-105eaa1d998f', individual_firearms_ownership_not_constitutionally_protected, conventional).
narrative_ontology:cs_reference_frame('108ce2fa-d889-42f9-8585-105eaa1d998f', founding_era_militia_compromise).
narrative_ontology:cs_drift_state('108ce2fa-d889-42f9-8585-105eaa1d998f', post_heller_era, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('108ce2fa-d889-42f9-8585-105eaa1d998f', '').
narrative_ontology:cs_kernel_id(second_amendment_scope__collective_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__collective_right_reading, state_governments).
narrative_ontology:constraint_beneficiary(second_amendment_scope__collective_right_reading, state_militias).
narrative_ontology:constraint_beneficiary(second_amendment_scope__collective_right_reading, national_guard).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(second_amendment_scope__collective_right_reading, federal_government).
narrative_ontology:constraint_vindicates(second_amendment_scope__collective_right_reading, federalism_militia_protection_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_scope__collective_right_reading, anti_federalist_ratification_bargain).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain authority over organized militia forces (National Guard) protected from federal disarmament. The reading secures their constitutional claim to arm and maintain effective state military forces. Exit means accepting federal plenary control over state military capacity.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, state_governments, agenda_setter,
    institutional, generational, constrained, national).

% Direct institutional beneficiaries of the constitutional guarantee that the federal government cannot disarm the organized militia. Their operational readiness depends on this protected status. No meaningful exit — they are the instrument of state military authority.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, state_militias, beneficiary,
    organized, generational, constrained, national).

% The modern organized militia; receives federal funding and equipment but retains state chain of command for domestic missions. The collective right reading protects their state-controlled status against full federalization. Dual role: benefits from the protection and administers the force it protects.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, national_guard, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_scope__collective_right_reading, national_guard, agenda_setter).

% Constrained by this reading from disarming or neutralizing state militia forces. Pays the cost of limited federal authority over state military capacity. Exit is mobile — can pursue policy goals through other constitutional powers (Commerce Clause, Necessary and Proper) but cannot override this specific allocation.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, federal_government, payer,
    institutional, generational, mobile, national).

% Hold no constitutional firearms right under this reading; their access to firearms is entirely subject to federal, state, and local regulation. Structurally excluded from the constraint's protection — the reading's premise forecloses their claim. Exit is trapped: no constitutional avenue, only legislative grace.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, individual_citizens, excluded,
    powerless, biographical, trapped, national).

% Organized political movement that would object to this reading's exclusion of individual rights. Not in the constitutional conversation under this reading's framework — their arguments are treated as policy preferences, not constitutional claims. Exit is constrained: must work through legislative politics or seek judicial overturning.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, gun_rights_advocates, excluded,
    organized, biographical, constrained, national).

% Adjudicate the constraint's scope. Under this reading, they enforce the federal-state militia allocation and reject individual-right claims. Analytical seat: they interpret but do not collect or pay.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, federal_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates military authority between federal and state governments by constitutionally protecting state militia forces from federal disarmament, solving the Anti-Federalist concern that Congress's Article I militia powers would enable destruction of state military autonomy.
% TRANSFER_FUNCTION: Transfers authority over militia armament and maintenance from federal to state control; constrains federal power to organize, arm, and discipline the militia (Art I, Sec 8) by reserving a protected sphere of state autonomy.
% ABSENT_VOICES: Individual citizens seeking constitutional protection for personal firearms ownership, and organized gun rights advocates — structurally excluded by the reading's premise that the Second Amendment protects only collective state authority. Their objections are treated as policy arguments, not constitutional claims.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, the federal government would gain plenary authority to disarm or federalize state militia forces without constitutional limitation. The federal-state military balance established at the Founding would collapse; states would lose their constitutional guarantee of independent military capacity, fundamentally altering the federalism structure the Amendment was designed to protect.
% FOUNDING_PROBLEM: Anti-Federalist fear that the Constitution's grant to Congress of power to 'organize, arm, and discipline' the militia (Article I, Section 8) would allow the federal government to disarm state militias and destroy the states' military autonomy — the very forces that secured independence and guaranteed state sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: The historical record from ratification debates corroborates the militia-protection concern: Anti-Federalist papers, Madison's Federalist No. 46, and the First Congress's militia legislation all reflect the understanding that the Amendment secured state militia authority. However, individual-rights scholars (e.g., Levy, Malcolm, Halbrook) and the Heller majority contest this as the exclusive original meaning, arguing the founding generation also understood an individual right.
narrative_ontology:disappearance_verdict(second_amendment_scope__collective_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_scope__collective_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__collective_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_scope__collective_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_scope__collective_right_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_scope__collective_right_reading_tests).
:- end_tests(second_amendment_scope__collective_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the constraint primarily allocates authority rather than extracting resources — it prevents federal overreach into a reserved state sphere. Suppression is low (0.1) because the constraint does not coerce compliance through force; it operates through judicial review and constitutional structure. Theater ratio is low (0.1) during its dominant period — the doctrine was genuinely applied by courts, not performed. Accessibility collapse is moderate (0.5): within the legal framework, accepting this reading legally forecloses individual-right claims until Heller. Resistance is moderate (0.4): individual-rights advocates consistently challenged the reading through scholarship and litigation.
 *
 * PERSPECTIVAL GAP:
 *   From the state/federal institutional seats, the constraint is experienced as a stable allocation of authority (rope). From the excluded individual-citizen seat, the same constraint operates as a denial of constitutional protection — but because they are excluded rather than extracted from, the engine's per-seat computation will show no effective extraction for them (they are not 'governed' by the constraint as targets). The gap is between institutional coordination and individual constitutional standing.
 *
 * DIRECTIONALITY LOGIC:
 *   State governments and their militias are structural beneficiaries (d near 0.0) — the constraint subsidizes their military autonomy. The federal government is the constrained party (d near 0.7) — it bears the cost of limited authority over state military forces, but as an institutional actor with mobile exit (other constitutional powers), its effective extraction is dampened. Individual citizens are excluded entirely — they have no constitutional claim under this reading, so directionality is not computed for them as constrained parties; they simply fall outside the constraint's protective scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (federal disarmament threat to state militias) is contested as to whether it persists. The National Guard system and dual federal-state control structure arguably solve the original problem, but the constraint persists as a constitutional allocation. The reading does not mislabel coordination as extraction — it genuinely coordinates federal-state military relations. The mandatrophy question is whether the coordination function remains live or has been superseded by modern military federalism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_reading,
    'This constraint is one reading of the second_amendment_scope kernel — specifically the collective_right_reading. What structural elements differ across sibling readings?',
    'Compare the beneficiary/victim sets, claimed_type, and extractiveness values across the three reading stories. The kernel contest is located in the beneficiary structure: collective reading places states/militias as beneficiaries with individuals excluded; individual reading places individuals as beneficiaries; civic reading places civically-engaged individuals as beneficiaries.',
    'If the sibling readings produce divergent classifications from the same kernel text, the kernel itself is the site of structural ambiguity — the Constitution''s fixed text generates multiple ε-invariant constraints depending on which reading is instantiated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_reading, conceptual, 'Commitment-system framing: this story is one reading of a contested kernel; sibling readings are separate constraints.').

omega_variable(
    original_meaning_ambiguity,
    'Does the historical record definitively support the collective right reading as the exclusive original meaning, or is the evidence genuinely ambiguous between collective and individual right understandings?',
    'Systematic historical-linguistic analysis of founding-era usage of ''bear arms,'' ''the people,'' and ''militia'' across legal, military, and civilian contexts; examination of state constitutional analogues and ratification debate records.',
    'If evidence is genuinely ambiguous, the collective reading''s claim to be the fixed original meaning (mountain claim) is undermined — the constraint would be a contested interpretation (rope/tangled_rope) rather than a natural-law-like fixation. If evidence definitively supports collective reading, the individual_right_reading''s ε would reflect extraction from historical truth.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_meaning_ambiguity, empirical, 'Whether historical evidence resolves the kernel contest or leaves it structurally underdetermined.').

omega_variable(
    post_heller_operativity,
    'After Heller (2008), does this reading continue to operate as a constraint in any jurisdiction or legal context, or has it been fully displaced as controlling law?',
    'Survey of post-Heller state court decisions, lower federal court applications, and state constitutional provisions that maintain collective-right frameworks. Track whether any courts continue to apply Miller-style collective right analysis for state-law claims.',
    'If the reading retains operativity in some jurisdictions, the constraint''s interval extends beyond 2008 and its classification may shift toward piton (degraded but persistent) or scaffold (transitional). If fully displaced, the 1939-2008 interval captures its complete lifecycle as binding law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_heller_operativity, empirical, 'Whether the collective right reading persists as an operative constraint after its Supreme Court rejection.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__collective_right_reading, 1939, 2008).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sa_crr_tr_t1939, second_amendment_scope__collective_right_reading, theater_ratio, 1939, 0.08).
narrative_ontology:measurement(sa_crr_tr_t1950, second_amendment_scope__collective_right_reading, theater_ratio, 1950, 0.07).
narrative_ontology:measurement(sa_crr_tr_t1970, second_amendment_scope__collective_right_reading, theater_ratio, 1970, 0.08).
narrative_ontology:measurement(sa_crr_tr_t1990, second_amendment_scope__collective_right_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(sa_crr_tr_t2008, second_amendment_scope__collective_right_reading, theater_ratio, 2008, 0.15).

% Extraction over time
narrative_ontology:measurement(sa_crr_be_t1939, second_amendment_scope__collective_right_reading, base_extractiveness, 1939, 0.12).
narrative_ontology:measurement(sa_crr_be_t1950, second_amendment_scope__collective_right_reading, base_extractiveness, 1950, 0.12).
narrative_ontology:measurement(sa_crr_be_t1970, second_amendment_scope__collective_right_reading, base_extractiveness, 1970, 0.13).
narrative_ontology:measurement(sa_crr_be_t1990, second_amendment_scope__collective_right_reading, base_extractiveness, 1990, 0.14).
narrative_ontology:measurement(sa_crr_be_t2008, second_amendment_scope__collective_right_reading, base_extractiveness, 2008, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(sa_crr_su_t1939, second_amendment_scope__collective_right_reading, suppression_requirement, 1939, 0.08).
narrative_ontology:measurement(sa_crr_su_t1950, second_amendment_scope__collective_right_reading, suppression_requirement, 1950, 0.07).
narrative_ontology:measurement(sa_crr_su_t1970, second_amendment_scope__collective_right_reading, suppression_requirement, 1970, 0.09).
narrative_ontology:measurement(sa_crr_su_t1990, second_amendment_scope__collective_right_reading, suppression_requirement, 1990, 0.12).
narrative_ontology:measurement(sa_crr_su_t2008, second_amendment_scope__collective_right_reading, suppression_requirement, 2008, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_scope__collective_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_scope__collective_right_reading, 0.1).
narrative_ontology:affects_constraint(second_amendment_scope__collective_right_reading, national_guard_federalization_authority).
narrative_ontology:affects_constraint(second_amendment_scope__collective_right_reading, federal_firearms_regulation_scope).
narrative_ontology:affects_constraint(second_amendment_scope__collective_right_reading, state_sovereign_immunity_military).

% DUAL FORMULATION NOTE:
% This story is the collective_right_reading of the second_amendment_scope kernel. It decomposes the natural-language concept 'Second Amendment scope' into three ε-invariant constraints: collective_right_reading (this story, ε≈0.15, rope), individual_right_reading (ε higher, snare/tangled_rope per its proponents' view of regulatory extraction), and civic_right_reading (intermediate ε). The ε values differ because each reading instantiates a different beneficiary/victim structure and regulatory scope. The collective reading's low ε reflects its narrow institutional coordination function; the individual reading's higher ε reflects its broad regulatory displacement effect.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_scope__collective_right_reading, institutional, 0.65).
constraint_indexing:directionality_override(second_amendment_scope__collective_right_reading, organized, 0.15).
constraint_indexing:directionality_override(second_amendment_scope__collective_right_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
