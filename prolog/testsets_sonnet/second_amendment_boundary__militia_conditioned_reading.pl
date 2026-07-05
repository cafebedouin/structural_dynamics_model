% ============================================================================
% CONSTRAINT STORY: second_amendment_boundary__militia_conditioned_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_boundary__militia_conditioned_reading, []).

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
 *   constraint_id: second_amendment_boundary__militia_conditioned_reading
 *   human_readable: Militia-Conditioned Reading of the Second Amendment (Prefatory Clause as Scope Limit)
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This story instantiates the militia-conditioned reading of the Second
 *   Amendment's kernel text: the prefatory clause ('A well regulated Militia,
 *   being necessary to the security of a free State') is read as a
 *   scope-defining condition on the operative clause ('the right of the
 *   people to keep and bear Arms, shall not be infringed'), such that the
 *   right attaches to organized collective defense activity rather than to
 *   private possession as such. This reading dominated federal appellate
 *   jurisprudence from United States v. Miller (1939) through the late
 *   twentieth century, treating firearms regulation as presumptively within
 *   ordinary legislative authority subject only to rational-basis-style
 *   review. It was substantially displaced — though not eliminated from
 *   academic and dissenting judicial argument — by District of Columbia v.
 *   Heller (2008), which adopted the individual_right_reading. This story
 *   does not describe that displacement as arbitration of which reading is
 *   correct; it authors the militia-conditioned reading on its own terms, as
 *   one live (if currently subordinated) reading of the kernel, with its own
 *   stable epsilon, beneficiary/victim structure, and classification. The
 *   sibling readings (individual_right_reading, insurrectionist_reading) are
 *   separate constraint stories, not alternative measurements of this one.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_boundary__militia_conditioned_reading, 0.42).
domain_priors:suppression_score(second_amendment_boundary__militia_conditioned_reading, 0.38).
domain_priors:theater_ratio(second_amendment_boundary__militia_conditioned_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__militia_conditioned_reading, rope).
narrative_ontology:human_readable(second_amendment_boundary__militia_conditioned_reading, "Militia-Conditioned Reading of the Second Amendment (Prefatory Clause as Scope Limit)").
narrative_ontology:topic_domain(second_amendment_boundary__militia_conditioned_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_boundary__militia_conditioned_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__militia_conditioned_reading, '2165396f-4787-45b2-8cdf-4f9cef00d88f').
narrative_ontology:cs_kernel_codification('2165396f-4787-45b2-8cdf-4f9cef00d88f', fixed_text).
narrative_ontology:cs_authority_grounding('2165396f-4787-45b2-8cdf-4f9cef00d88f', lineage).
narrative_ontology:cs_interpretation_layer_present('2165396f-4787-45b2-8cdf-4f9cef00d88f').
narrative_ontology:cs_reading_relation('2165396f-4787-45b2-8cdf-4f9cef00d88f', second_amendment_boundary__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('2165396f-4787-45b2-8cdf-4f9cef00d88f', second_amendment_boundary__insurrectionist_reading, forecloses).
narrative_ontology:cs_axiom('2165396f-4787-45b2-8cdf-4f9cef00d88f', foundational, prefatory_clause_is_scope_limiting).
narrative_ontology:cs_axiom_status(prefatory_clause_is_scope_limiting, holdable).
narrative_ontology:cs_axiom_grounding('2165396f-4787-45b2-8cdf-4f9cef00d88f', prefatory_clause_is_scope_limiting, conventional).
narrative_ontology:cs_axiom('2165396f-4787-45b2-8cdf-4f9cef00d88f', foundational, right_attaches_only_to_organized_collective_defense_activity).
narrative_ontology:cs_axiom_status(right_attaches_only_to_organized_collective_defense_activity, holdable).
narrative_ontology:cs_axiom_grounding('2165396f-4787-45b2-8cdf-4f9cef00d88f', right_attaches_only_to_organized_collective_defense_activity, conventional).
narrative_ontology:cs_axiom('2165396f-4787-45b2-8cdf-4f9cef00d88f', secondary, firearms_possession_is_ordinary_regulable_subject_matter).
narrative_ontology:cs_axiom_status(firearms_possession_is_ordinary_regulable_subject_matter, holdable).
narrative_ontology:cs_axiom_grounding('2165396f-4787-45b2-8cdf-4f9cef00d88f', firearms_possession_is_ordinary_regulable_subject_matter, instrumental).
narrative_ontology:cs_reference_frame('2165396f-4787-45b2-8cdf-4f9cef00d88f', collective_defense_militia_polity).
narrative_ontology:cs_drift_state('2165396f-4787-45b2-8cdf-4f9cef00d88f', post_heller_era, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('2165396f-4787-45b2-8cdf-4f9cef00d88f', '').
narrative_ontology:cs_kernel_id(second_amendment_boundary__militia_conditioned_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, state_legislatures).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, municipal_governments).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, gun_violence_prevention_advocates).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, law_enforcement_agencies).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, firearms_collectors_in_restrictive_jurisdictions).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, self_defense_claimants_in_high_regulation_states).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, small_firearms_retailers).
narrative_ontology:constraint_vindicates(second_amendment_boundary__militia_conditioned_reading, collective_rights_theory).
narrative_ontology:constraint_vindicates(second_amendment_boundary__militia_conditioned_reading, civic_republicanism_of_the_founding).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enact firearms regulation (licensing regimes, assault weapon bans, magazine capacity limits, waiting periods) on the premise that the constitutional text does not preempt democratic control over weapons outside an organized militia context. They administer the reading by writing and defending statutes under this scope theory.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, state_legislatures, agenda_setter,
    institutional, generational, analytical, national).

% Rely on the militia-conditioned reading as the doctrinal foundation for legislative campaigns and litigation defense. They benefit from courts treating the prefatory clause as limiting language, which widens the space for regulation they advocate.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, gun_violence_prevention_advocates, beneficiary,
    organized, generational, mobile, national).

% Enforce licensing and possession restrictions premised on the collective-defense reading; benefit from a broader menu of permissible restriction that reduces certain categories of civilian-owned weapons they must otherwise contend with in the field.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, law_enforcement_agencies, beneficiary,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(second_amendment_boundary__militia_conditioned_reading, law_enforcement_agencies, agenda_setter).

% Own or wish to own firearms for reasons unconnected to militia service — collection, sport shooting, heirloom transfer. Under this reading, their possession claims receive no independent constitutional weight and are subject to whatever regulatory scheme the state enacts; their only exit is relocation to a less restrictive jurisdiction or forgoing possession.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, firearms_collectors_in_restrictive_jurisdictions, payer,
    moderate, biographical, constrained, regional).

% Individuals, often in dense urban jurisdictions, who seek to keep a firearm for home or personal defense. Under the militia-conditioned reading, self-defense possession divorced from militia context receives no independent constitutional floor, leaving them dependent entirely on legislative grace and administrative licensing discretion that can be denied or revoked. Relocation is often not a real option given economic constraints.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, self_defense_claimants_in_high_regulation_states, payer,
    powerless, biographical, trapped, local).

% Sell firearms and ammunition in jurisdictions that, under this reading, may impose comprehensive licensing, inventory, and sale restrictions without triggering strict constitutional scrutiny. Their business model is subject to legislative revision at any point, with limited recourse to a countervailing individual-right claim.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, small_firearms_retailers, payer,
    moderate, biographical, constrained, regional).

% Argue that an unorganized citizenry retaining arms IS the modern militia contemplated by the framers, and that this reading's requirement of formal militia affiliation misreads eighteenth-century usage. They are not the interpreting authority under this reading and their historical argument is treated as foreclosed by the doctrinal structure this reading establishes.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, organized_militia_movement_advocates, excluded,
    organized, generational, constrained, national).

% Examine founding-era militia statutes, drafting history, and ratification debates to assess whether the prefatory clause functioned as a scope-limiting condition or a stated-but-non-limiting purpose. Their scholarship is invoked by all three kernel readings selectively.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a doctrinal basis for treating firearms policy as a matter of ordinary democratic regulation rather than a domain walled off by an individual constitutional right — allowing legislatures to coordinate collective safety measures (background checks, weapon-type restrictions, licensing) without each measure facing categorical constitutional veto.
% TRANSFER_FUNCTION: Moves regulatory authority from an individual-rights floor (which would constrain legislatures) to legislative and administrative bodies; correspondingly moves the burden of justifying possession from the state (which would need compelling interest under strict scrutiny) to the individual claimant seeking an exemption or license.
% ABSENT_VOICES: Organized militia-movement advocates and individual-rights originalists are excluded from this reading's interpretive authority — they contest the founding-era meaning of 'militia' and 'the people' but their argument is treated as answered rather than live once this reading is adopted by a court or legislature.
% DISAPPEARANCE_RATIONALE: If this reading were abandoned by the judiciary in favor of the individual-right reading (as substantially occurred in District of Columbia v. Heller, 2008), existing regulatory regimes premised on the collective-defense scope limitation would face strict or intermediate scrutiny challenges, and legislatures would lose the doctrinal basis for a wide category of possession restrictions — this is not hypothetical, it is the actual post-Heller trajectory this reading lost.
% FOUNDING_PROBLEM: Addressing anxiety, at the founding and through the twentieth century, that state governments could be disarmed by a federal standing army, while also needing a textual basis to reconcile the constitutional text with the practical reality that mass civilian firearm ownership divorced from organized militia service was becoming a public safety concern courts and legislatures needed room to regulate.
% FOUNDING_PROBLEM_CORROBORATION: Pre-Heller circuit courts (nearly uniformly) and gun control advocacy organizations attest this reading correctly captures the founding concern with state militias as a check on federal power. Post-Heller, the Supreme Court majority and individual-rights originalist scholars attest the founding problem was preservation of a personal right that pre-existed the Constitution and the prefatory clause was announcing a purpose, not conditioning the right — this corroboration split is exactly the kernel contest; no source entirely outside the interpretive dispute exists because the dispute is itself over how to read the founding record.
narrative_ontology:disappearance_verdict(second_amendment_boundary__militia_conditioned_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_boundary__militia_conditioned_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_boundary__militia_conditioned_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_boundary__militia_conditioned_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_boundary__militia_conditioned_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_boundary__militia_conditioned_reading_tests).
:- end_tests(second_amendment_boundary__militia_conditioned_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects that this reading transfers meaningful control from individual possession claimants to legislatures and administrative licensing bodies — a real cost borne by collectors, self-defense claimants, and retailers in jurisdictions that regulate heavily, but one exercised through ordinary democratic and administrative process rather than raw coercion. Suppression (0.38) is moderate: alternatives (relocation, political mobilization, litigation under competing readings) remain available, distinguishing this from a Snare. The measurement series shows extraction and suppression climbing through the twentieth century as urban gun control regimes matured under this doctrinal umbrella, peaking around 2008 when this reading was the controlling federal doctrine, then receding after Heller displaced it as controlling law (though it persists in dissent and in some state constitutional doctrine, hence values do not fall to zero). Theater ratio is low-to-moderate throughout — the regulatory function is substantially real, not primarily performative, though post-Heller some jurisdictions maintain licensing theater that no longer tracks the doctrine's actual constitutional force.
 *
 * DIRECTIONALITY LOGIC:
 *   State legislatures and gun violence prevention advocates sit near the beneficiary end: the reading directly expands their regulatory latitude and legitimates their policy goals. Firearms collectors and self-defense claimants in restrictive jurisdictions sit near the target end: the reading removes an independent constitutional floor they would otherwise invoke against restriction, and their exit options (relocation, forgoing possession) are costly and often impractical, especially for the powerless self-defense claimant seat, which is coded trapped. Small retailers are moderately targeted with more mobility (relocating a business is costly but possible). Organized militia-movement advocates are excluded rather than targeted or benefited — their competing historical argument is not adjudicated within this reading's own frame, it is simply not the operative premise.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem framing captures the genuine ambiguity: this reading's founding concern (federal power vs. state militia autonomy) has arguably become less central to contemporary firearms debates (which center on individual self-defense and mass-shooting policy) than it was in 1791 or 1939, while the doctrinal apparatus built on this reading (rational-basis review of gun laws, licensing regimes) persists and has in some places been repurposed toward contemporary public-safety goals unrelated to the original militia concern. This is exactly why founding_problem_status is authored as contested rather than dead or live: the reading's advocates would say the underlying value (democratic control of a public-safety-affecting product) transposes cleanly to new problems; critics would say this is bootstrapping a doctrine built for one purpose onto an unrelated contemporary policy goal, which is a live methodological dispute captured by the mismatch consumer (status=contested cross-checked against disappearance_verdict=world_rearranges) rather than resolved here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prefatory_clause_grammatical_function,
    'Does the prefatory clause''s absolute-construction grammar (announcing a reason) function as a legal condition limiting the operative clause''s scope, or as stated purpose that does not narrow an otherwise free-standing right — and is this a question with a determinate historical-linguistic answer or an irreducibly contested interpretive choice?',
    'Corpus linguistic analysis of eighteenth-century legal drafting conventions for absolute constructions in statutory and constitutional prefatory clauses, cross-referenced against contemporaneous state constitutional analogues that used similar phrasing with known scope effects.',
    'If corpus evidence strongly supports scope-limiting function, this reading''s textual claim strengthens relative to individual_right_reading; if evidence is genuinely indeterminate (the current scholarly consensus), the kernel remains contested by construction and no reading can claim decisive textual victory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prefatory_clause_grammatical_function, empirical, 'Whether the prefatory clause''s grammar is legally scope-limiting or merely explanatory — a linguistic-historical question with disputed resolution.').

omega_variable(
    militia_conditioned_vs_individual_right_reading_delta,
    'Given that individual_right_reading is now the controlling federal doctrine post-Heller while this reading was controlling doctrine pre-Heller, is the relevant epsilon comparison synchronic (both readings measured against current enforcement) or diachronic (each reading measured against the period it controlled)?',
    'This story authors epsilon for the period and jurisdictions where this reading is or was operative (pre-Heller federal doctrine, current state-constitutional doctrine in a minority of states, current dissenting judicial argument) — not as a claim about the current controlling national doctrine, which is the separate individual_right_reading story.',
    'Conflating the two would violate epsilon-invariance by measuring one constraint at two different times/enforcement-regimes as if it were a single value; keeping them separate stories with a network link preserves each reading''s own stable epsilon.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(militia_conditioned_vs_individual_right_reading_delta, conceptual, 'Clarifies why this reading is authored with its own historically-bounded epsilon rather than reconciled against the currently-dominant sibling reading.').

omega_variable(
    collective_vs_unorganized_militia_definitional_gap,
    'Does ''well regulated Militia'' in the founding-era sense refer to a formally organized state military body, or to the unorganized body of the armed citizenry as understood in contemporaneous militia statutes (which called up nearly all adult male citizens)?',
    'Comparative analysis of founding-era state militia statutes (which distinguished ''organized'' from ''unorganized'' militia) against the constitutional text''s use of the bare term ''Militia'' without the organizational qualifier used elsewhere in the same era''s legal documents.',
    'If ''Militia'' was understood to include the unorganized citizenry by default, this reading''s requirement of formal militia affiliation as a precondition for the right may rest on a definitional error — this is precisely the ground on which organized_militia_movement_advocates contest this reading''s authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_vs_unorganized_militia_definitional_gap, empirical, 'Whether this reading''s core definitional move (limiting militia to organized bodies) is historically accurate or a later doctrinal narrowing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__militia_conditioned_reading, 1791, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1791, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 1791, 0.1).
narrative_ontology:measurement(seco_tr_t1900, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 1900, 0.12).
narrative_ontology:measurement(seco_tr_t1939, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 1939, 0.15).
narrative_ontology:measurement(seco_tr_t1990, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 2008, 0.3).
narrative_ontology:measurement(seco_tr_t2024, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(seco_be_t1791, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 1791, 0.2).
narrative_ontology:measurement(seco_be_t1900, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 1900, 0.25).
narrative_ontology:measurement(seco_be_t1939, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 1939, 0.3).
narrative_ontology:measurement(seco_be_t1990, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 1990, 0.45).
narrative_ontology:measurement(seco_be_t2008, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 2008, 0.5).
narrative_ontology:measurement(seco_be_t2024, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1791, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 1791, 0.15).
narrative_ontology:measurement(seco_su_t1900, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 1900, 0.2).
narrative_ontology:measurement(seco_su_t1939, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 1939, 0.28).
narrative_ontology:measurement(seco_su_t1990, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 1990, 0.4).
narrative_ontology:measurement(seco_su_t2008, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 2008, 0.55).
narrative_ontology:measurement(seco_su_t2024, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 2024, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(second_amendment_boundary__militia_conditioned_reading, second_amendment_boundary__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__militia_conditioned_reading, second_amendment_boundary__insurrectionist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the second_amendment_boundary kernel, each authored as a separate, epsilon-stable constraint story per the epsilon-invariance decomposition principle. militia_conditioned_reading and individual_right_reading stand in significant historical tension: the former was controlling federal doctrine through Miller (1939) and much of the twentieth century; the latter became controlling in Heller (2008) and displaced it as the operative national doctrine, though militia_conditioned_reading persists in dissent and in some state constitutional frameworks. insurrectionist_reading shares a textual root with individual_right_reading (both treat the right as attaching to individuals) but diverges on the right's normative function (anti-tyranny resistance capacity vs. self-defense/lawful-purpose possession). All three should be read together as an instance of contested-kernel decomposition; none is authored as more 'correct' than the others within this framework — each reflects a different party's actual interpretive commitment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
