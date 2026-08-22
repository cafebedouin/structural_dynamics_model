% ============================================================================
% CONSTRAINT STORY: second_amendment_boundary__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_boundary__individual_right_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: second_amendment_boundary__individual_right_reading
 *   human_readable: Second Amendment Individual Right Reading — Heller/Bruen Doctrine
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This story authors the individual-right reading of the Second Amendment's
 *   kernel text: the claim that the operative clause ('the right of the
 *   people to keep and bear Arms, shall not be infringed') establishes a
 *   pre-existing individual right to possess firearms for self-defense, with
 *   the prefatory militia clause serving only as a stated purpose that does
 *   not narrow the operative clause's scope. This reading was substantially
 *   dormant in constitutional doctrine until the late twentieth century,
 *   crystallized in District of Columbia v. Heller (2008), and hardened into
 *   a mandatory historical-analogue methodology in New York State Rifle &
 *   Pistol Association v. Bruen (2022). Under this reading, private
 *   possession enters the core of constitutionally protected activity, state
 *   and local regulation is treated as presumptively suspect unless it finds
 *   a historical analogue, and the firearms commercial market gains
 *   substantial insulation from safety regulation. This is ONE of three
 *   readings of the same kernel text; the militia-conditioned reading (which
 *   treats the prefatory clause as scope-limiting) and the insurrectionist
 *   reading (which treats individual armed capacity as instrumental to
 *   resisting tyranny) are separate constraint stories with their own ε,
 *   victim sets, and classifications — this story does not average across
 *   them or hedge its extraction value to accommodate them.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_boundary__individual_right_reading, 0.62).
domain_priors:suppression_score(second_amendment_boundary__individual_right_reading, 0.58).
domain_priors:theater_ratio(second_amendment_boundary__individual_right_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_boundary__individual_right_reading, "Second Amendment Individual Right Reading — Heller/Bruen Doctrine").
narrative_ontology:topic_domain(second_amendment_boundary__individual_right_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_boundary__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__individual_right_reading, '3ffb3ce4-8af2-45cb-8a67-e5a342eb8dd0').
narrative_ontology:cs_kernel_codification('3ffb3ce4-8af2-45cb-8a67-e5a342eb8dd0', fixed_text).
narrative_ontology:cs_authority_grounding('3ffb3ce4-8af2-45cb-8a67-e5a342eb8dd0', lineage).
narrative_ontology:cs_interpretation_layer_present('3ffb3ce4-8af2-45cb-8a67-e5a342eb8dd0').
narrative_ontology:cs_reading_relation('3ffb3ce4-8af2-45cb-8a67-e5a342eb8dd0', second_amendment_boundary__militia_conditioned_reading, forecloses).
narrative_ontology:cs_reading_relation('3ffb3ce4-8af2-45cb-8a67-e5a342eb8dd0', second_amendment_boundary__insurrectionist_reading, coexists_with).
narrative_ontology:cs_axiom('3ffb3ce4-8af2-45cb-8a67-e5a342eb8dd0', foundational, operative_clause_textual_primacy).
narrative_ontology:cs_axiom_status(operative_clause_textual_primacy, holdable).
narrative_ontology:cs_axiom_grounding('3ffb3ce4-8af2-45cb-8a67-e5a342eb8dd0', operative_clause_textual_primacy, conventional).
narrative_ontology:cs_axiom('3ffb3ce4-8af2-45cb-8a67-e5a342eb8dd0', foundational, self_defense_as_pre_existing_natural_right).
narrative_ontology:cs_axiom_status(self_defense_as_pre_existing_natural_right, holdable).
narrative_ontology:cs_axiom_grounding('3ffb3ce4-8af2-45cb-8a67-e5a342eb8dd0', self_defense_as_pre_existing_natural_right, deontological).
narrative_ontology:cs_axiom('3ffb3ce4-8af2-45cb-8a67-e5a342eb8dd0', secondary, prefatory_clause_non_limiting).
narrative_ontology:cs_axiom_status(prefatory_clause_non_limiting, holdable).
narrative_ontology:cs_axiom_grounding('3ffb3ce4-8af2-45cb-8a67-e5a342eb8dd0', prefatory_clause_non_limiting, conventional).
narrative_ontology:cs_reference_frame('3ffb3ce4-8af2-45cb-8a67-e5a342eb8dd0', originalist_pre_existing_natural_right).
narrative_ontology:cs_drift_state('3ffb3ce4-8af2-45cb-8a67-e5a342eb8dd0', post_bruen_historical_analogue_era, gap(revival_pressure, severe, true)).
narrative_ontology:cs_created_at('3ffb3ce4-8af2-45cb-8a67-e5a342eb8dd0', '').
narrative_ontology:cs_kernel_id(second_amendment_boundary__individual_right_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__individual_right_reading, firearms_manufacturers).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__individual_right_reading, firearms_retailers).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__individual_right_reading, gun_rights_advocacy_organizations).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__individual_right_reading, individual_gun_owners).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, mass_shooting_victims).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, domestic_violence_victims).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, firearm_suicide_completers).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, communities_with_high_gun_violence_exposure).
narrative_ontology:constraint_vindicates(second_amendment_boundary__individual_right_reading, pre_existing_natural_right_to_self_defense).
narrative_ontology:constraint_vindicates(second_amendment_boundary__individual_right_reading, operative_clause_textual_primacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold a constitutionally protected individual right to keep and bear arms for self-defense, independent of militia service. Can acquire, possess, and carry firearms with regulation treated as presumptively suspect. Face few structural barriers to exercising the right in most jurisdictions post-Heller/Bruen.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, individual_gun_owners, beneficiary,
    moderate, biographical, mobile, national).

% Operate in a market substantially shielded from regulation by the constitutional framing, reinforced by statutory liability protections (PLCAA) that ride on the same individual-right logic. Litigate aggressively to strike down state and local restrictions, framing commercial interest as constitutional principle.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, firearms_manufacturers, beneficiary,
    institutional, generational, arbitrage, national).

% Fund litigation strategy, cultivate the doctrinal architecture (text-history-tradition methodology), and select test cases to expand the individual-right reading's scope. Set the interpretive agenda that courts subsequently ratify. Their institutional survival depends on the reading's continued dominance.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, gun_rights_advocacy_organizations, agenda_setter,
    organized, generational, arbitrage, national).

% Bear the lethal consequences of a legal regime that treats most access restrictions as presumptively unconstitutional. Had no voice in the doctrinal construction and no capacity to exit the jurisdictions or circumstances in which the harm occurred.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, mass_shooting_victims, payer,
    powerless, immediate, trapped, local).

% Face elevated lethality risk when abusers retain firearm access; protective-order disarmament provisions have been subject to constitutional challenge under the individual-right framework (e.g., Rahimi litigation). Their safety is contingent on how courts balance the doctrine against narrow exceptions.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, domestic_violence_victims, payer,
    powerless, immediate, trapped, local).

% Access to a highly lethal, low-effort method during acute crisis periods is preserved by a regulatory environment that treats waiting periods, storage mandates, and purchase restrictions as constitutionally suspect burdens. Represent the largest single category of firearm deaths, with no direct voice in doctrinal formation.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, firearm_suicide_completers, payer,
    powerless, immediate, trapped, national).

% Historically exercised broad police-power authority over firearms regulation; that authority is now substantially preempted by federal constitutional doctrine requiring regulations to find analogues in a historical tradition dating to 1791/1868. Their regulatory judgment is displaced by judicial historical analysis.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, state_and_local_legislatures, excluded,
    institutional, biographical, constrained, regional).

% Produce epidemiological evidence on firearm mortality and the efficacy of regulatory interventions, but this evidence is structurally irrelevant to the doctrine's methodology, which asks only whether a historical analogue exists — not whether a regulation reduces harm. Their expertise has no doctrinal purchase.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, public_health_researchers, excluded,
    moderate, generational, constrained, national).

% Adjudicates the boundary of the right using text-history-and-tradition methodology, determining which regulations survive. Holds the actual power to expand or narrow the doctrine's reach case by case, and could in principle adopt a different reading.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_boundary__individual_right_reading, federal_judiciary, observer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_boundary__individual_right_reading, firearms_manufacturers).
narrative_ontology:fixing_cost_class(second_amendment_boundary__individual_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, judicially enforceable rule that private firearm ownership for self-defense is a baseline entitlement not contingent on militia service or legislative grace — coordinating expectations for owners, manufacturers, and lower courts around a single doctrinal test (text, history, and tradition) rather than case-by-case interest balancing.
% TRANSFER_FUNCTION: Moves regulatory authority away from state and local legislatures and toward federal courts applying historical-analogue review; moves risk exposure away from firearms manufacturers and retailers (who gain litigation shields and market protection) and onto populations exposed to firearm violence, who bear the safety cost of a regulatory ceiling.
% ABSENT_VOICES: Mass shooting survivors' families, domestic violence victims, and public health researchers have no direct doctrinal standing — the text-history-tradition methodology does not admit contemporary harm data as relevant to constitutional analysis. State and local legislatures, the traditional locus of police-power regulation, are structurally excluded from setting the substantive standard.
% DISAPPEARANCE_RATIONALE: If the individual-right reading were abandoned and courts reverted to a militia-conditioned or interest-balancing framework, state and local legislatures would regain broad regulatory latitude, firearms manufacturers would lose a primary litigation shield against safety regulation, and gun rights organizations would lose their principal doctrinal lever — the firearms regulatory landscape in the United States would look substantially different within a single legislative cycle.
% FOUNDING_PROBLEM: The doctrine was constructed to resolve genuine ambiguity in a two-clause constitutional text and to establish a stable judicial baseline against what advocates characterized as encroaching and inconsistent state and local gun control that treated firearm ownership as a mere statutory privilege.
% FOUNDING_PROBLEM_CORROBORATION: Gun rights organizations and the doctrine's judicial architects attest the founding problem (protecting a pre-existing natural right from legislative erosion) remains fully live. Public health researchers, dissenting justices (e.g., in Heller and Bruen), and comparative constitutional scholars outside the advocacy network attest that the 'pre-existing right' framing was itself a contested historical reconstruction rather than a rediscovered original meaning, and that the doctrine now functions primarily to insulate a commercial and political interest from empirically grounded regulation.
narrative_ontology:disappearance_verdict(second_amendment_boundary__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_boundary__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_boundary__individual_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_boundary__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_boundary__individual_right_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_boundary__individual_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_boundary__individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_boundary__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.62 by the end of the interval: substantial but not maximal, because the reading genuinely does coordinate a real self-defense interest for a large population of gun owners even as it imposes population-level costs measured in lives. Suppression is measured at 0.58, reflecting the doctrine's active foreclosure of state and local regulatory alternatives via the mandatory historical-analogue test — this is a raw structural property (how completely alternatives are foreclosed), not scaled by scope or power in the authored value; the engine applies its own scaling. Theater ratio is comparatively low (0.28): the doctrine is not mostly performative — it has teeth, striking down actual statutes and shielding actual manufacturers from liability. The temporal grid tracks the doctrine's dormancy (near-zero functional extraction pre-Heller, when courts largely deferred to legislatures under a collective-rights or rational-basis framework) through its crystallization and hardening (Heller 2008 to Bruen 2022 to the present), which is the honest historical trajectory of this specific reading's practical force, not a claim about the text's original meaning.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual gun owners and the firearms industry sit near the beneficiary end: the constraint subsidizes their activity by converting what would otherwise be ordinary regulable commerce and conduct into constitutionally shielded activity, with mobile-to-arbitrage exit options (owners can relocate to favorable jurisdictions; manufacturers can forum-shop litigation). Mass shooting victims, domestic violence victims, and suicide completers sit at the full-target end: they are trapped by circumstance (no advance capacity to exit the risk), bear the downstream cost of the regulatory ceiling, and had no voice in the doctrine's construction. Gun rights advocacy organizations and the federal judiciary are agenda-setters who administer and could in principle revise the doctrine, distinguishing them from the diffuse beneficiary class that merely collects its benefits.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification is deliberate and resists collapsing this reading into either a pure Rope (ignoring the victim class and asymmetric extraction) or a pure Snare (ignoring the genuine coordination value the doctrine provides self-defense-oriented owners and the stability it provides commercial actors). The coordination function is real: a clear, judicially enforceable baseline reduces uncertainty for owners and manufacturers relative to a patchwork of ad hoc interest-balancing tests. The extraction is also real and asymmetric: the costs of a regulatory ceiling fall on populations with no say in the doctrine's construction and no capacity to exit the risk. Treating this as a Mountain (natural, inevitable, no beneficiaries) would be the false-summit failure mode this story is designed to avoid — hence beneficiaries and victims are both explicitly named and enforcement is active (ongoing litigation actively invalidating state and local statutes).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prefatory_clause_scope_function,
    'Does the prefatory militia clause perform any operative legal function, or is it purely explanatory background with zero scope-limiting effect on the operative clause, as this reading holds?',
    'Historical linguistic analysis of eighteenth-century legal drafting conventions for clauses of this grammatical structure; comparative analysis of contemporaneous state constitutional provisions using similar two-clause structures and how courts of the founding era treated them.',
    'If the prefatory clause is found to have historically performed a scope-limiting function, this reading''s core textual premise weakens substantially, strengthening the militia_conditioned_reading and undermining the extension of constitutional protection to purely private, non-militia-related possession.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(prefatory_clause_scope_function, conceptual, 'Whether the prefatory clause has genuine scope-limiting legal effect, contested across readings.').

omega_variable(
    original_public_meaning_vs_constructed_tradition,
    'Is the ''pre-existing individual right'' the doctrine identifies a genuine recovery of 1791 original public meaning, or a late-twentieth-century constructed tradition retrojected onto the historical record by advocacy scholarship preceding Heller?',
    'Independent historiographical review of founding-era militia statutes, gun regulation ordinances, and judicial commentary predating the modern individual-rights advocacy movement (pre-1960s), conducted by historians without litigation-funding relationships to either advocacy side.',
    'If substantially constructed rather than recovered, this reading''s claim to be identifying a ''pre-existing'' right (as opposed to creating a new one via reinterpretation) is significantly weakened, though this would not by itself determine whether the resulting doctrine is normatively defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_public_meaning_vs_constructed_tradition, empirical, 'Whether the doctrine recovers or constructs the individual right it claims to find in 1791.').

omega_variable(
    victim_causal_attribution,
    'What share of firearm-violence mortality is causally attributable specifically to regulations invalidated or chilled by this doctrine, as opposed to regulations that would not have existed regardless of the doctrine?',
    'Natural-experiment studies comparing firearm mortality trends in jurisdictions before and after specific Bruen-driven invalidations of permitting and carry regulations, controlling for other factors.',
    'A strong causal link would sharpen the victim/extraction framing considerably; a weak or attenuated link would suggest some of the authored extractiveness reflects background firearm availability rather than this specific doctrinal reading''s marginal effect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_causal_attribution, empirical, 'Causal attribution of firearm harm specifically to this doctrine''s regulatory foreclosure, versus background availability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__individual_right_reading, 1791, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1791, second_amendment_boundary__individual_right_reading, theater_ratio, 1791, 0.1).
narrative_ontology:measurement_basis(seco_tr_t1791, observed).
narrative_ontology:measurement(seco_tr_t1939, second_amendment_boundary__individual_right_reading, theater_ratio, 1939, 0.15).
narrative_ontology:measurement_basis(seco_tr_t1939, observed).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_boundary__individual_right_reading, theater_ratio, 2008, 0.22).
narrative_ontology:measurement_basis(seco_tr_t2008, observed).
narrative_ontology:measurement(seco_tr_t2016, second_amendment_boundary__individual_right_reading, theater_ratio, 2016, 0.25).
narrative_ontology:measurement_basis(seco_tr_t2016, observed).
narrative_ontology:measurement(seco_tr_t2022, second_amendment_boundary__individual_right_reading, theater_ratio, 2022, 0.27).
narrative_ontology:measurement_basis(seco_tr_t2022, observed).
narrative_ontology:measurement(seco_tr_t2024, second_amendment_boundary__individual_right_reading, theater_ratio, 2024, 0.28).
narrative_ontology:measurement_basis(seco_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(seco_be_t1791, second_amendment_boundary__individual_right_reading, base_extractiveness, 1791, 0.15).
narrative_ontology:measurement_basis(seco_be_t1791, observed).
narrative_ontology:measurement(seco_be_t1939, second_amendment_boundary__individual_right_reading, base_extractiveness, 1939, 0.2).
narrative_ontology:measurement_basis(seco_be_t1939, observed).
narrative_ontology:measurement(seco_be_t2008, second_amendment_boundary__individual_right_reading, base_extractiveness, 2008, 0.45).
narrative_ontology:measurement_basis(seco_be_t2008, observed).
narrative_ontology:measurement(seco_be_t2016, second_amendment_boundary__individual_right_reading, base_extractiveness, 2016, 0.52).
narrative_ontology:measurement_basis(seco_be_t2016, observed).
narrative_ontology:measurement(seco_be_t2022, second_amendment_boundary__individual_right_reading, base_extractiveness, 2022, 0.6).
narrative_ontology:measurement_basis(seco_be_t2022, observed).
narrative_ontology:measurement(seco_be_t2024, second_amendment_boundary__individual_right_reading, base_extractiveness, 2024, 0.62).
narrative_ontology:measurement_basis(seco_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1791, second_amendment_boundary__individual_right_reading, suppression_requirement, 1791, 0.1).
narrative_ontology:measurement_basis(seco_su_t1791, observed).
narrative_ontology:measurement(seco_su_t1939, second_amendment_boundary__individual_right_reading, suppression_requirement, 1939, 0.18).
narrative_ontology:measurement_basis(seco_su_t1939, observed).
narrative_ontology:measurement(seco_su_t2008, second_amendment_boundary__individual_right_reading, suppression_requirement, 2008, 0.4).
narrative_ontology:measurement_basis(seco_su_t2008, observed).
narrative_ontology:measurement(seco_su_t2016, second_amendment_boundary__individual_right_reading, suppression_requirement, 2016, 0.48).
narrative_ontology:measurement_basis(seco_su_t2016, observed).
narrative_ontology:measurement(seco_su_t2022, second_amendment_boundary__individual_right_reading, suppression_requirement, 2022, 0.55).
narrative_ontology:measurement_basis(seco_su_t2022, observed).
narrative_ontology:measurement(seco_su_t2024, second_amendment_boundary__individual_right_reading, suppression_requirement, 2024, 0.58).
narrative_ontology:measurement_basis(seco_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_boundary__individual_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_boundary__individual_right_reading, 0.1).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, second_amendment_boundary__militia_conditioned_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, second_amendment_boundary__insurrectionist_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, firearms_liability_shield_statute).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language label 'the Second Amendment' per the ε-invariance principle: individual_right_reading (this story, ε=0.62, tangled_rope), militia_conditioned_reading (a separate story with a substantially lower ε reflecting broad regulatory latitude and no comparable victim class from unrestricted access), and insurrectionist_reading (a separate story centered on armed-resistance-to-tyranny function with its own distinct beneficiary/victim structure). Each carries its own claimed_type and metrics; none is derived by averaging or hedging across the others. The upstream/downstream relationship here is lateral rather than hierarchical: all three readings compete for judicial and political adoption of the same kernel text, and the ascendance of this reading structurally suppresses the practical operation of its siblings without logically foreclosing either (see cs_structure.reading_relations).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
