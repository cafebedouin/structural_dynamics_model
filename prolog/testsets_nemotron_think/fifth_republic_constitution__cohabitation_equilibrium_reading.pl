% ============================================================================
% CONSTRAINT STORY: fifth_republic_constitution__cohabitation_equilibrium_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fifth_republic_constitution__cohabitation_equilibrium_reading, []).

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
 *   constraint_id: fifth_republic_constitution__cohabitation_equilibrium_reading
 *   human_readable: Fifth Republic Cohabitation: Negotiated Authority Allocation Between President and Prime Minister
 *   domain: constitutional_law/political_systems/comparative_government
 *
 * SUMMARY:
 *   The Fifth Republic's dual executive creates a structural ambiguity: the
 *   president (directly elected) and prime minister (parliamentarily
 *   responsible) both claim executive authority. During periods of
 *   cohabitation — when president and parliamentary majority oppose each
 *   other — this ambiguity becomes operational. The 'cohabitation equilibrium
 *   reading' holds that the constitution requires negotiated allocation of
 *   policy domains: president retains foreign/defense/European affairs (the
 *   'reserved domain'), prime minister controls
 *   domestic/economic/administrative policy. This reading treats the
 *   arrangement as a genuine coordination mechanism that prevents deadlock,
 *   but acknowledges asymmetric extraction: policy coherence is the victim,
 *   and whichever executive controls the salient policy domain at a given
 *   moment is the beneficiary. Extraction is moderate but unstable — it
 *   spikes during cohabitation (1986-88, 1993-95, 1997-2002) and falls during
 *   aligned periods. The 2000 quinquennat reform (aligning presidential and
 *   legislative terms) reduced cohabitation probability but did not eliminate
 *   the structural tension, as shown by the 2022-2024 period where a relative
 *   majority government creates de facto cohabitation dynamics.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.55).
domain_priors:suppression_score(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.45).
domain_priors:theater_ratio(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__cohabitation_equilibrium_reading, tangled_rope).
narrative_ontology:human_readable(fifth_republic_constitution__cohabitation_equilibrium_reading, "Fifth Republic Cohabitation: Negotiated Authority Allocation Between President and Prime Minister").
narrative_ontology:topic_domain(fifth_republic_constitution__cohabitation_equilibrium_reading, "constitutional_law/political_systems/comparative_government").

domain_priors:requires_active_enforcement(fifth_republic_constitution__cohabitation_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__cohabitation_equilibrium_reading, '3ea9da3f-a8fe-4339-ac7c-378b6463cca9').
narrative_ontology:cs_kernel_codification('3ea9da3f-a8fe-4339-ac7c-378b6463cca9', fixed_text).
narrative_ontology:cs_authority_grounding('3ea9da3f-a8fe-4339-ac7c-378b6463cca9', lineage).
narrative_ontology:cs_interpretation_layer_present('3ea9da3f-a8fe-4339-ac7c-378b6463cca9').
narrative_ontology:cs_reading_relation('3ea9da3f-a8fe-4339-ac7c-378b6463cca9', fifth_republic_constitution__hyper_presidential_reading, coexists_with).
narrative_ontology:cs_reading_relation('3ea9da3f-a8fe-4339-ac7c-378b6463cca9', fifth_republic_constitution__parliamentary_constraint_reading, coexists_with).
narrative_ontology:cs_axiom('3ea9da3f-a8fe-4339-ac7c-378b6463cca9', foundational, dual_executive_negotiated_allocation).
narrative_ontology:cs_axiom_status(dual_executive_negotiated_allocation, holdable).
narrative_ontology:cs_axiom_grounding('3ea9da3f-a8fe-4339-ac7c-378b6463cca9', dual_executive_negotiated_allocation, conventional).
narrative_ontology:cs_axiom('3ea9da3f-a8fe-4339-ac7c-378b6463cca9', secondary, policy_domain_split_foreign_domestic).
narrative_ontology:cs_axiom_status(policy_domain_split_foreign_domestic, holdable).
narrative_ontology:cs_axiom_grounding('3ea9da3f-a8fe-4339-ac7c-378b6463cca9', policy_domain_split_foreign_domestic, conventional).
narrative_ontology:cs_reference_frame('3ea9da3f-a8fe-4339-ac7c-378b6463cca9', gaullist_dual_legitimacy_1958).
narrative_ontology:cs_drift_state('3ea9da3f-a8fe-4339-ac7c-378b6463cca9', post_quinquennat_2002, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3ea9da3f-a8fe-4339-ac7c-378b6463cca9', '').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__cohabitation_equilibrium_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, president_foreign_policy_domain).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, prime_minister_domestic_policy_domain).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, policy_coherence_citizens).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, public_administration).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, president).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, prime_minister).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, parliamentary_majority).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, parliamentary_majority).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, citizens).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__cohabitation_equilibrium_reading, dual_executive_stability_doctrine).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__cohabitation_equilibrium_reading, direct_presidential_legitimacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Directly elected head of state constitutionally entitled to foreign policy, defense, and European affairs. During cohabitation, must negotiate domestic policy with opposing parliamentary majority. Retains dissolution power and constitutional council referral. Exit constrained by fixed term and constitutional role; cannot resign without triggering crisis.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, president, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__cohabitation_equilibrium_reading, president, beneficiary).

% Head of government responsible to parliamentary majority. During cohabitation, controls domestic policy, administration, and legislative agenda. Must maintain Assembly confidence. Exit constrained by parliamentary dependence; can be forced out by censure motion but cannot easily resign without collapsing government.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, prime_minister, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__cohabitation_equilibrium_reading, prime_minister, beneficiary).

% Controls legislative agenda and can censure government. During cohabitation, backs prime minister against president on domestic policy. Gains domestic policy control but loses foreign policy influence. Exit via elections (mobile) but must maintain coalition discipline.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, parliamentary_majority, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__cohabitation_equilibrium_reading, parliamentary_majority, payer).

% Experience policy incoherence when dual executives deadlock or send contradictory signals (e.g., foreign vs domestic economic policy). Bear costs of administrative paralysis. Exit constrained by nationality; emigration is high-cost. Voice only through periodic elections which produce the cohabitation condition.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, citizens, payer,
    moderate, biographical, constrained, national).

% Receives potentially contradictory directives from Élysée (president's staff) and Matignon (prime minister's staff). Must implement both foreign and domestic policy streams. Career civil servants cannot exit; political appointees turn over with each executive. Bears coordination costs of dual command.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, public_administration, payer,
    organized, generational, constrained, national).

% Adjudicates constitutional disputes between president and prime minister (Article 61 referrals, competence conflicts). Provides authoritative interpretation that stabilizes the negotiated allocation. Does not collect rents or bear extraction; its legitimacy depends on perceived neutrality.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, constitutional_council, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Negotiated allocation of executive authority between dual executives to prevent deadlock while maintaining democratic legitimacy of both direct presidential election and parliamentary responsibility
% TRANSFER_FUNCTION: Moves policy initiative and implementation authority between president (foreign policy, defense, European affairs) and prime minister (domestic policy, economy, administration) based on electoral alignment; during cohabitation, the split becomes operational reality rather than theoretical reserve
% ABSENT_VOICES: Citizens who experience policy incoherence but have no direct voice in inter-executive negotiations; junior coalition partners in parliamentary majority excluded from domain allocation decisions; overseas territories whose interests span foreign/domestic split
% DISAPPEARANCE_RATIONALE: Without negotiated allocation, the constitution's ambiguity on executive hierarchy would be resolved by raw power politics — likely producing either hyper-presidentialism (president dominates all policy) or parliamentary supremacy (prime minister becomes sole executive), fundamentally altering the Fifth Republic's institutional logic
% FOUNDING_PROBLEM: Post-1958 need for stable executive authority after Fourth Republic's parliamentary instability (24 governments in 12 years), while preserving presidential legitimacy from direct election (1962 referendum) and parliamentary responsibility
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars outside the executive branch (Dominique Rousseau, Guy Carcassonne, Olivier Duhamel) attest the founding problem was parliamentary instability; political actors in cohabitation periods (Chirac 1986-88, Balladur 1993-95, Jospin 1997-2002) claim the arrangement creates new dysfunction of dual legitimacy; the Constitutional Council's jurisprudence since 1986 corroborates the emergence of cohabitation as unanticipated equilibrium
narrative_ontology:disappearance_verdict(fifth_republic_constitution__cohabitation_equilibrium_reading, world_rearranges).
narrative_ontology:founding_problem_status(fifth_republic_constitution__cohabitation_equilibrium_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__cohabitation_equilibrium_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fifth_republic_constitution__cohabitation_equilibrium_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.55, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fifth_republic_constitution__cohabitation_equilibrium_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fifth_republic_constitution__cohabitation_equilibrium_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fifth_republic_constitution__cohabitation_equilibrium_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55) reflects the real but variable cost of dual command: administrative duplication, policy signaling conflicts, and strategic vetoes. Suppression (0.45) is moderate because the constraint operates through constitutional norms and political convention rather than coercion — but the Constitutional Council's enforcement of competence boundaries creates hard suppression at the margins. Theater ratio (0.32) captures performative conflict (public letters, media posturing) that exceeds functional negotiation. Accessibility collapse (0.52) is partial: alternatives (parliamentary revision, constitutional amendment) exist but require supermajorities that neither executive can muster alone. Resistance (0.48) reflects periodic institutional crises (1986-88 first cohabitation, 1997 dissolution) but no sustained challenge to the dual executive structure itself.
 *
 * PERSPECTIVAL GAP:
 *   From the president's seat (aligned period): the constraint is a Rope — stable coordination enabling strong executive action. From the president's seat (cohabitation): a Tangled Rope — genuine coordination on foreign policy but extraction via blocked domestic initiatives. From the prime minister's seat (cohabitation): a Rope on domestic policy, Snare on foreign policy where president's reserved domain prevents parliamentary accountability. From citizens' seat: consistently extractive (Snare-flavored) — they pay coordination costs without choosing the arrangement. The engine computes this multi-seat divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   President and prime minister are dual agenda-setters with domain-differentiated beneficiary roles: president benefits in foreign/defense (d ~0.2), prime minister in domestic/economic (d ~0.2). During cohabitation, each becomes payer in the other's domain (d ~0.7). Parliamentary majority is beneficiary on domestic policy (supports its PM) but payer on foreign policy (excluded). Citizens and administration are consistent payers (d ~0.6-0.7) bearing coordination costs. Constitutional Council is analytical observer (d ~0.5). The domain-split means directionality flips by policy area — a structural feature the engine computes from the beneficiary/victim declarations per domain.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (parliamentary instability) was solved — the Fifth Republic has had stable governments since 1958. But the solution created a new problem: cohabitation management. The mandate has not atrophied (the dual executive still coordinates), but its function has mutated from 'stability against parliamentary fragmentation' to 'negotiated division of executive labor.' This is not mandatrophy (which requires function loss without replacement) but functional drift — captured by the 'contested' founding_problem_status and the rising extractiveness during cohabitation periods.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is the cohabitation equilibrium a genuine constitutional reading or a post-hoc rationalization of political practice?',
    'Trace the emergence of the ''reserved domain'' doctrine: does it appear in 1958 constitutional debates, Gaullist writings, or only in post-1986 constitutional commentary?',
    'If post-hoc, the coordination function is constructed cover for presidential power retention; if genuine, the domain split is a designed feature of the constitution''s ambiguity',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Whether the cohabitation equilibrium reading reflects original constitutional design or retrospective interpretation').

omega_variable(
    coordination_vs_extraction_boundary,
    'Is the foreign/domestic policy split a genuine coordination solution or a cover for asymmetric extraction?',
    'Compare policy outcomes during cohabitation vs aligned periods: does the domain split produce coherent policy in reserved domains while domestic policy shows extraction signatures (veto proliferation, administrative paralysis)?',
    'If the split produces genuine coordination in both domains, extraction is incidental; if domestic policy shows systematic extraction during cohabitation, the coordination story is partial cover',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, empirical, 'Whether the domain allocation genuinely coordinates or masks extraction').

omega_variable(
    policy_coherence_victim_reality,
    'Is ''policy coherence'' a real victim bearing measurable costs, or a theoretical construct?',
    'Measure administrative implementation gaps, contradictory regulatory signals, and economic outcomes during cohabitation periods vs aligned periods using OECD/INSEE data on policy effectiveness',
    'If measurable, the victim is concrete and extraction is substantiated; if not, the victim claim is rhetorical and the constraint may be closer to Rope',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_coherence_victim_reality, empirical, 'Whether policy coherence loss translates to measurable harm for citizens and administration').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (constitutional text, institutional rules) or internalized (political actors'' self-restraint based on legitimacy beliefs)?',
    'Analyze compliance patterns: do actors respect domain boundaries because of enforceable rules (Constitutional Council decisions) or because violating them would delegitimize their own authority?',
    'If internalized, suppression is higher than structural measure suggests — actors carry the constraint internally; if structural, suppression is bounded by institutional enforcement capacity',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural vs internalized suppression mechanism in dual executive negotiation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__cohabitation_equilibrium_reading, 1986, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fift_tr_t1986, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 1986, 0.25).
narrative_ontology:measurement(fift_tr_t1993, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 1993, 0.28).
narrative_ontology:measurement(fift_tr_t1997, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 1997, 0.35).
narrative_ontology:measurement(fift_tr_t2002, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 2002, 0.3).
narrative_ontology:measurement(fift_tr_t2012, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 2012, 0.22).
narrative_ontology:measurement(fift_tr_t2024, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 2024, 0.32).

% Extraction over time
narrative_ontology:measurement(fift_be_t1986, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 1986, 0.48).
narrative_ontology:measurement(fift_be_t1993, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 1993, 0.52).
narrative_ontology:measurement(fift_be_t1997, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 1997, 0.58).
narrative_ontology:measurement(fift_be_t2002, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 2002, 0.45).
narrative_ontology:measurement(fift_be_t2012, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 2012, 0.38).
narrative_ontology:measurement(fift_be_t2024, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 2024, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(fift_su_t1986, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 1986, 0.4).
narrative_ontology:measurement(fift_su_t1993, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 1993, 0.42).
narrative_ontology:measurement(fift_su_t1997, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 1997, 0.48).
narrative_ontology:measurement(fift_su_t2002, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 2002, 0.38).
narrative_ontology:measurement(fift_su_t2012, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 2012, 0.32).
narrative_ontology:measurement(fift_su_t2024, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 2024, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_republic_constitution__cohabitation_equilibrium_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.12).
narrative_ontology:affects_constraint(fifth_republic_constitution__cohabitation_equilibrium_reading, fifth_republic_constitution__hyper_presidential_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__cohabitation_equilibrium_reading, fifth_republic_constitution__parliamentary_constraint_reading).

% DUAL FORMULATION NOTE:
% This reading decomposes the fifth_republic_constitution kernel alongside hyper_presidential_reading and parliamentary_constraint_reading. The epsilon values differ: this reading (0.55) shows moderate extraction from cohabitation dynamics; hyper_presidential_reading would show higher extraction (president dominates); parliamentary_constraint_reading would show lower extraction (parliament constrains president). They share the constitutional text as kernel but instantiate different constraints with different beneficiary/victim structures and different epsilon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fifth_republic_constitution__cohabitation_equilibrium_reading, institutional, 0.25).
constraint_indexing:directionality_override(fifth_republic_constitution__cohabitation_equilibrium_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
