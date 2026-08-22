% ============================================================================
% CONSTRAINT STORY: second_amendment_boundary__militia_conditioned_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: second_amendment_boundary__militia_conditioned_reading
 *   human_readable: Militia-Conditioned Reading of the Second Amendment (Collective-Defense Boundary)
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This constraint models the militia-conditioned reading of the Second
 *   Amendment as a controlling constitutional doctrine: the prefatory 'well
 *   regulated Militia' clause is read to define and limit the scope of 'keep
 *   and bear Arms,' such that civilian possession disconnected from organized
 *   militia service receives no special constitutional protection and is
 *   subject to ordinary democratic regulation. This reading held substantial
 *   doctrinal weight through the mid-20th century (United States v. Miller,
 *   1939, and its progeny), reaching a high-water mark of influence as gun
 *   control legislation proliferated (1968 Gun Control Act, 1994 Assault
 *   Weapons Ban), before being sharply curtailed by District of Columbia v.
 *   Heller (2008), which adopted the individual-right reading instead. This
 *   story authors ONLY the militia-conditioned reading as a distinct
 *   constraint with its own epsilon; the individual_right_reading and
 *   insurrectionist_reading are separate constraints in the same kernel
 *   family, not alternative measurements of this one.
 *
 * KEY AGENTS:
 *   - state_and_municipal_regulators: agenda_setter (institutional/analytical) — enacts regulation on the reading's textual authority
 *   - gun_violence_prevention_advocates: beneficiary (organized/mobile) — gains litigation and policy leverage
 *   - rural_self_defense_claimants: payer (powerless/trapped) — personal defense claim denied independent constitutional weight
 *   - firearms_collectors and licensed_gun_dealers: payer (moderate/constrained) — possession and commerce exposed to comprehensive regulation
 *   - federal_judiciary: observer (institutional/analytical) — adjudicates which reading controls
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_boundary__militia_conditioned_reading, 0.42).
domain_priors:suppression_score(second_amendment_boundary__militia_conditioned_reading, 0.38).
domain_priors:theater_ratio(second_amendment_boundary__militia_conditioned_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__militia_conditioned_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_boundary__militia_conditioned_reading, "Militia-Conditioned Reading of the Second Amendment (Collective-Defense Boundary)").
narrative_ontology:topic_domain(second_amendment_boundary__militia_conditioned_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_boundary__militia_conditioned_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__militia_conditioned_reading, '6ee8a99d-53e5-459e-bc4f-6f60d622db97').
narrative_ontology:cs_kernel_codification('6ee8a99d-53e5-459e-bc4f-6f60d622db97', fixed_text).
narrative_ontology:cs_authority_grounding('6ee8a99d-53e5-459e-bc4f-6f60d622db97', lineage).
narrative_ontology:cs_interpretation_layer_present('6ee8a99d-53e5-459e-bc4f-6f60d622db97').
narrative_ontology:cs_reading_relation('6ee8a99d-53e5-459e-bc4f-6f60d622db97', second_amendment_boundary__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('6ee8a99d-53e5-459e-bc4f-6f60d622db97', second_amendment_boundary__insurrectionist_reading, coexists_with).
narrative_ontology:cs_axiom('6ee8a99d-53e5-459e-bc4f-6f60d622db97', foundational, prefatory_clause_limits_operative_scope).
narrative_ontology:cs_axiom_status(prefatory_clause_limits_operative_scope, holdable).
narrative_ontology:cs_axiom_grounding('6ee8a99d-53e5-459e-bc4f-6f60d622db97', prefatory_clause_limits_operative_scope, conventional).
narrative_ontology:cs_axiom('6ee8a99d-53e5-459e-bc4f-6f60d622db97', foundational, arms_right_is_collective_civic_duty_not_individual_entitlement).
narrative_ontology:cs_axiom_status(arms_right_is_collective_civic_duty_not_individual_entitlement, overridden).
narrative_ontology:cs_axiom_grounding('6ee8a99d-53e5-459e-bc4f-6f60d622db97', arms_right_is_collective_civic_duty_not_individual_entitlement, deontological).
narrative_ontology:cs_reference_frame('6ee8a99d-53e5-459e-bc4f-6f60d622db97', founding_era_civic_republican_militia_tradition).
narrative_ontology:cs_drift_state('6ee8a99d-53e5-459e-bc4f-6f60d622db97', post_heller_era, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('6ee8a99d-53e5-459e-bc4f-6f60d622db97', '').
narrative_ontology:cs_kernel_id(second_amendment_boundary__militia_conditioned_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, state_and_municipal_regulators).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, gun_violence_prevention_advocates).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, law_enforcement_agencies).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, urban_communities_facing_gun_violence).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, rural_self_defense_claimants).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, firearms_collectors).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, licensed_gun_dealers).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, gun_owners_in_restrictive_jurisdictions).
narrative_ontology:constraint_vindicates(second_amendment_boundary__militia_conditioned_reading, collective_militia_purpose_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_boundary__militia_conditioned_reading, civic_republican_arms_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts and enforces licensing, registration, storage, and carry restrictions on the premise that the constitutional text does not bar comprehensive regulation of civilian firearms outside an organized militia context. Administers permitting regimes, background-check systems, and assault-weapons or magazine-capacity bans, treating the prefatory clause as a binding scope limitation on the operative clause.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, state_and_municipal_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Litigates and lobbies for regulation on the reading's authority, treating the militia clause as textual leverage to sustain restrictions against individual-right challenges. Benefits directly whenever courts uphold regulation under a means-end scrutiny framework the reading licenses.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, gun_violence_prevention_advocates, beneficiary,
    organized, generational, mobile, national).

% Operates within and enforces the regulatory infrastructure the reading validates — permit checks, red-flag orders, prohibited-persons enforcement. Gains operational tools and legal cover from a reading that treats civilian possession as regulable absent militia service, but bears enforcement burden and community friction.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, law_enforcement_agencies, beneficiary,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(second_amendment_boundary__militia_conditioned_reading, law_enforcement_agencies, agenda_setter).

% Experiences disproportionate firearm mortality and stands to benefit from regulation the reading authorizes (waiting periods, licensing, carry restrictions). Has limited direct voice in the doctrinal contest but is the population most often cited as the reading's real-world stake.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, urban_communities_facing_gun_violence, beneficiary,
    moderate, biographical, constrained, local).

% Lives in jurisdictions with limited police response times and asserts a personal self-defense need distinct from organized militia service. Under this reading, that claim carries no independent constitutional weight — possession for personal defense is subject to the same regulatory calculus as any other civilian activity, and relocation to escape restrictive licensing is often not a realistic option.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, rural_self_defense_claimants, payer,
    powerless, biographical, trapped, regional).

% Owns firearms for historical, sporting, or hobbyist reasons unconnected to militia service. Under the reading, this possession has no privileged constitutional footing and is fully exposed to registration mandates, transfer restrictions, and categorical bans on certain weapon types, with compliance costs and confiscation risk in some jurisdictions.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, firearms_collectors, payer,
    moderate, biographical, constrained, national).

% Operates a lawful business selling firearms to civilians whose possession is not connected to militia service. Faces licensing burdens, inventory restrictions, and demand suppression as jurisdictions regulate more comprehensively on the reading's authority; cannot easily exit the regulatory regime while remaining in the trade.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, licensed_gun_dealers, payer,
    moderate, biographical, constrained, national).

% Owns or wishes to own firearms in states or cities that have adopted comprehensive licensing, storage, or possession restrictions justified by this reading. Compliance costs, denial risk under discretionary permitting, and the practical difficulty of relocating to a more permissive jurisdiction fall directly on this group.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, gun_owners_in_restrictive_jurisdictions, payer,
    powerless, biographical, constrained, regional).

% Argues the operative clause secures a pre-existing individual right independent of militia service and that the prefatory clause states a purpose without narrowing scope. Largely excluded from doctrinal control after the reading's ascendance was reversed in controlling case law, though the scholarship remains active and contests the reading's textual premises.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, originalist_legal_scholars, excluded,
    organized, generational, analytical, national).

% Adjudicates which reading of the prefatory clause controls, weighs historical evidence about militia practice and civilian possession at the founding, and determines the level of scrutiny applied to firearms regulation. Its rulings determine which reading holds doctrinal authority at any given time.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, federal_judiciary, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_boundary__militia_conditioned_reading, diffuse).
narrative_ontology:fixing_cost_class(second_amendment_boundary__militia_conditioned_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a textual basis for treating firearms possession as a democratically regulable activity like any other, allowing legislatures to weigh public safety against an asserted right using ordinary means-end scrutiny rather than a categorical constitutional bar — solving the coordination problem of enabling collective self-governance over a domain with substantial externalities (gun violence, accidents, criminal misuse).
% TRANSFER_FUNCTION: Moves regulatory authority and legal risk from individual possessors to the state: legislatures and regulators gain durable authority to restrict, license, and condition firearm ownership; individual owners whose possession is unconnected to militia service bear compliance costs, denial risk, and reduced autonomy over a previously unregulated domain.
% ABSENT_VOICES: Rural and low-income gun owners with limited access to litigation resources are rarely heard directly in the doctrinal contest, which is fought primarily by well-resourced advocacy organizations and academic scholars on both sides; their lived self-defense concerns are represented, if at all, by proxies whose incentives diverge from theirs.
% DISAPPEARANCE_RATIONALE: If this reading vanished as controlling doctrine (as it substantially has, post-Heller), the immediate legal landscape does not fully rearrange because many state and local regulations survive under alternative rationales (police power, historical tradition tests) — but the doctrinal foundation for the most sweeping regulatory approaches (assault weapons bans defended purely as non-militia-connected activity, comprehensive possession licensing) becomes considerably more vulnerable to individual-right challenge, and future legislative ambition is measurably constrained.
% FOUNDING_PROBLEM: The reading was developed to resolve genuine ambiguity in the constitutional text — why does an amendment securing an individual liberty open with a clause about militias? — and to provide doctrinal grounding for 20th-century firearms regulation (machine gun restrictions, licensing regimes) against constitutional challenge.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the founding era and some originalist scholars outside the gun-control coalition acknowledge that civic-republican and militia-based understandings of the arms right had genuine currency in ratification-era discourse, corroborating that the reading is not purely instrumental invention; however, the controlling Supreme Court majority in District of Columbia v. Heller (2008), drawing on historical analysis from scholars outside the gun-control advocacy coalition, concluded the prefatory clause does not limit the operative clause's individual-right scope, directly disputing the reading's central textual claim from a position with no stake in either advocacy camp.
narrative_ontology:disappearance_verdict(second_amendment_boundary__militia_conditioned_reading, contested).
narrative_ontology:founding_problem_status(second_amendment_boundary__militia_conditioned_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_boundary__militia_conditioned_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored at 0.42, reflecting genuine coordination value (public safety regulation of a domain with real externalities) alongside real costs imposed on a bounded set of possessors whose claims are foreclosed by the reading's scope limitation. Suppression at 0.38 reflects that the reading does not extinguish gun ownership altogether but subjects it to licensing and restriction regimes with real enforcement teeth (denial of permits, confiscation of certain weapon classes) — moderate, not maximal, coercion. Theater ratio is moderate (peaking at 0.35 around 2008 as constitutional litigation intensified) reflecting genuine doctrinal contest rather than pure performance. Resistance is high (0.72) because the reading faces sustained, well-organized opposition from gun-rights advocacy and eventually loses controlling authority in Heller — this is a doctrine that must be actively defended and has in fact been substantially defeated, which is itself diagnostic.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulators, law enforcement, and violence-prevention advocates sit near the beneficiary end: the reading validates authority they already sought to exercise and gives them durable doctrinal cover. Rural self-defense claimants, collectors, and dealers sit near the target end: their possession, previously understood by many as protected, becomes fully exposed to the ordinary democratic process with no special constitutional floor. Urban communities facing gun violence are authored as beneficiaries even though they hold little direct doctrinal power — the reading's stated justification runs through their safety interest, and coordinated regulation is the mechanism by which that benefit is meant to be delivered.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — resolving textual ambiguity about the amendment's scope and providing doctrinal footing for firearms regulation — was live for decades but its authority has been substantially superseded by Heller's individual-right holding. The founding_problem_status is authored as 'contested' rather than 'dead' because state and local regulators continue to invoke militia-conditioned reasoning in briefing and dissenting opinions, and some scholars maintain the historical case remains strong; but the reading no longer controls doctrine, so a story that treated it as simply 'live' would overstate its current authority. This is exactly the kind of status ambiguity R5 is designed to surface rather than adjudicate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prefatory_clause_binding_force,
    'Does the prefatory militia clause function as a legally binding scope limitation on the operative clause, or as a non-limiting statement of purpose — and is this a question with a determinate historical answer or an irreducibly contested interpretive choice?',
    'Historical linguistic analysis of 18th-century constitutional drafting conventions (how prefatory clauses functioned in contemporaneous state constitutions), corpus linguistics on founding-era usage of ''bear arms,'' and analysis of ratification-era debates and militia statutes. Even robust historical evidence may not resolve the interpretive question, since originalist and living-constitutionalist frameworks can weigh the same historical record differently.',
    'If the prefatory clause is determined to be genuinely scope-limiting under original public meaning, this reading''s claim to textual fidelity strengthens considerably. If determined to be non-limiting, the reading''s coordination-function framing depends more heavily on living-constitutionalist or purposivist premises than on originalist textualism, which changes how the reading''s own foundational axiom should be graded.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(prefatory_clause_binding_force, conceptual, 'Whether the prefatory clause is textually binding or merely explanatory — the central interpretive fork of the kernel.').

omega_variable(
    doctrinal_authority_post_heller,
    'Given that Heller (2008) adopted the individual-right reading as controlling federal constitutional doctrine, does the militia-conditioned reading retain any operative legal force, or does it persist only as academic and dissenting-opinion argument with no binding effect?',
    'Tracking citation and reliance on militia-conditioned reasoning in post-Heller state and federal court opinions, and in state constitutional interpretation where state arms-rights provisions differ from the federal text.',
    'If the reading retains no operative force anywhere, this constraint should be understood as describing a formerly controlling but now largely superseded doctrine — closer to a piton than an active tangled_rope. If meaningful reliance persists (state courts, dissents shaping future doctrinal shifts, scholarly influence on a future overruling), the tangled_rope classification as an active, contested arrangement is better supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_authority_post_heller, empirical, 'Whether the reading is a currently operative doctrine or a superseded one whose classification should shift toward inertial/degraded.').

omega_variable(
    regulatory_capture_of_regulators,
    'Do state and municipal regulators authored here as coordination-beneficiaries also function as an interest group that benefits institutionally (budget, headcount, enforcement authority) independent of the public-safety coordination function the reading nominally serves?',
    'Compare regulatory agency budget and authority growth against measured public-safety outcomes attributable to the specific regulations the reading licenses, controlling for other causes of change in firearm mortality.',
    'If regulatory agencies capture institutional benefit disproportionate to public-safety gains, the extraction component of this tangled_rope is larger than the coordination-function framing suggests, and the beneficiary declaration for regulators should be read with an institutional-self-interest override in mind.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_capture_of_regulators, empirical, 'Whether regulatory institutions benefit as self-interested agenda-setters beyond the coordination function they administer.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__militia_conditioned_reading, 1939, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1939, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 1939, 0.15).
narrative_ontology:measurement_basis(seco_tr_t1939, observed).
narrative_ontology:measurement(seco_tr_t1968, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 1968, 0.18).
narrative_ontology:measurement_basis(seco_tr_t1968, observed).
narrative_ontology:measurement(seco_tr_t1994, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 1994, 0.22).
narrative_ontology:measurement_basis(seco_tr_t1994, observed).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 2008, 0.35).
narrative_ontology:measurement_basis(seco_tr_t2008, observed).
narrative_ontology:measurement(seco_tr_t2016, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 2016, 0.3).
narrative_ontology:measurement_basis(seco_tr_t2016, observed).
narrative_ontology:measurement(seco_tr_t2024, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 2024, 0.28).
narrative_ontology:measurement_basis(seco_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(seco_be_t1939, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 1939, 0.22).
narrative_ontology:measurement_basis(seco_be_t1939, observed).
narrative_ontology:measurement(seco_be_t1968, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 1968, 0.3).
narrative_ontology:measurement_basis(seco_be_t1968, observed).
narrative_ontology:measurement(seco_be_t1994, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 1994, 0.4).
narrative_ontology:measurement_basis(seco_be_t1994, observed).
narrative_ontology:measurement(seco_be_t2008, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 2008, 0.5).
narrative_ontology:measurement_basis(seco_be_t2008, observed).
narrative_ontology:measurement(seco_be_t2016, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 2016, 0.46).
narrative_ontology:measurement_basis(seco_be_t2016, observed).
narrative_ontology:measurement(seco_be_t2024, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 2024, 0.42).
narrative_ontology:measurement_basis(seco_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1939, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 1939, 0.2).
narrative_ontology:measurement_basis(seco_su_t1939, observed).
narrative_ontology:measurement(seco_su_t1968, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 1968, 0.28).
narrative_ontology:measurement_basis(seco_su_t1968, observed).
narrative_ontology:measurement(seco_su_t1994, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 1994, 0.35).
narrative_ontology:measurement_basis(seco_su_t1994, observed).
narrative_ontology:measurement(seco_su_t2008, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 2008, 0.45).
narrative_ontology:measurement_basis(seco_su_t2008, observed).
narrative_ontology:measurement(seco_su_t2016, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 2016, 0.4).
narrative_ontology:measurement_basis(seco_su_t2016, observed).
narrative_ontology:measurement(seco_su_t2024, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 2024, 0.38).
narrative_ontology:measurement_basis(seco_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_boundary__militia_conditioned_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_boundary__militia_conditioned_reading, second_amendment_boundary__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__militia_conditioned_reading, second_amendment_boundary__insurrectionist_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'the Second Amendment's scope' per the ε-invariance principle. individual_right_reading treats the operative clause as securing a pre-existing individual right unconstrained by the prefatory clause (low regulatory-authority epsilon, victim set = would-be regulators and public-safety claimants). insurrectionist_reading treats the right as instrumental to armed resistance against tyranny (distinct victim/beneficiary structure again). This story, militia_conditioned_reading, treats the prefatory clause as scope-limiting, authorizing comprehensive regulation, with victims being possessors whose claims fall outside the militia-connection boundary. Each sibling has its own epsilon, its own claimed_type, and its own stakeholder set; they are linked here for contamination-propagation and doctrinal-shift analysis, not averaged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
