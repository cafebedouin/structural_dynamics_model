% ============================================================================
% CONSTRAINT STORY: second_amendment_boundary__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Second Amendment as Individual Pre-Existing Right (Heller/Bruen Reading)
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested Second Amendment
 *   kernel: the individual-right reading, under which the operative clause
 *   ('the right of the people to keep and bear Arms shall not be infringed')
 *   establishes a pre-existing individual right to possess firearms for
 *   self-defense, and the prefatory militia clause ('A well regulated
 *   Militia...') is read as announcing a purpose without narrowing the
 *   operative clause's scope. This reading was substantially adopted in
 *   District of Columbia v. Heller (2008) and extended in New York State
 *   Rifle & Pistol Association v. Bruen (2022), which imposed a
 *   text-history-and-tradition test requiring close historical analogues for
 *   any firearm regulation to survive. Under this reading, state regulation
 *   is presumptively suspect, the firearms market gains constitutional
 *   insulation from ordinary public-safety rulemaking, and the population
 *   that bears the downstream cost is the set of people harmed by firearm
 *   access the now-narrowed regulatory toolkit could otherwise have
 *   mitigated. The sibling readings — militia_conditioned_reading
 *   (regulation-permissive) and insurrectionist_reading
 *   (armed-resistance-instrumental) — are NOT part of this constraint; they
 *   are separate stories with their own ε, beneficiary/victim sets, and
 *   classifications, linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - individual_firearm_owners: primary beneficiary (moderate/mobile) — exercises the protected right
 *   - firearms_manufacturers: primary beneficiary (organized/arbitrage) — market insulated from regulation
 *   - gun_rights_advocacy_organizations: agenda_setter (institutional/arbitrage) — sets litigation and doctrinal strategy
 *   - mass_shooting_victims, domestic_violence_victims, firearm_suicide_decedents, urban_gun_violence_communities: payers (powerless/trapped-constrained) — bear the harm the constrained regulatory toolkit could otherwise reduce
 *   - state_and_local_regulators: payer/excluded (institutional/constrained) — loses policy latitude under historical-analogue review
 *   - federal_appellate_judiciary: observer (institutional/analytical) — applies and elaborates the doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_boundary__individual_right_reading, 0.61).
domain_priors:suppression_score(second_amendment_boundary__individual_right_reading, 0.52).
domain_priors:theater_ratio(second_amendment_boundary__individual_right_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_boundary__individual_right_reading, "Second Amendment as Individual Pre-Existing Right (Heller/Bruen Reading)").
narrative_ontology:topic_domain(second_amendment_boundary__individual_right_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_boundary__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__individual_right_reading, 'b9bbe677-5475-4910-9a01-b166b075933c').
narrative_ontology:cs_kernel_codification('b9bbe677-5475-4910-9a01-b166b075933c', fixed_text).
narrative_ontology:cs_authority_grounding('b9bbe677-5475-4910-9a01-b166b075933c', lineage).
narrative_ontology:cs_interpretation_layer_present('b9bbe677-5475-4910-9a01-b166b075933c').
narrative_ontology:cs_reading_relation('b9bbe677-5475-4910-9a01-b166b075933c', second_amendment_boundary__militia_conditioned_reading, forecloses).
narrative_ontology:cs_reading_relation('b9bbe677-5475-4910-9a01-b166b075933c', second_amendment_boundary__insurrectionist_reading, coexists_with).
narrative_ontology:cs_axiom('b9bbe677-5475-4910-9a01-b166b075933c', foundational, right_predates_and_is_unconditioned_by_prefatory_clause).
narrative_ontology:cs_axiom_status(right_predates_and_is_unconditioned_by_prefatory_clause, holdable).
narrative_ontology:cs_axiom_grounding('b9bbe677-5475-4910-9a01-b166b075933c', right_predates_and_is_unconditioned_by_prefatory_clause, deontological).
narrative_ontology:cs_axiom('b9bbe677-5475-4910-9a01-b166b075933c', secondary, regulation_is_presumptively_suspect_absent_historical_analogue).
narrative_ontology:cs_axiom_status(regulation_is_presumptively_suspect_absent_historical_analogue, holdable).
narrative_ontology:cs_axiom_grounding('b9bbe677-5475-4910-9a01-b166b075933c', regulation_is_presumptively_suspect_absent_historical_analogue, conventional).
narrative_ontology:cs_reference_frame('b9bbe677-5475-4910-9a01-b166b075933c', founding_era_natural_rights_framework).
narrative_ontology:cs_drift_state('b9bbe677-5475-4910-9a01-b166b075933c', post_bruen_contemporary, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('b9bbe677-5475-4910-9a01-b166b075933c', '').
narrative_ontology:cs_kernel_id(second_amendment_boundary__individual_right_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__individual_right_reading, firearms_manufacturers).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__individual_right_reading, firearms_retailers).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__individual_right_reading, gun_rights_advocacy_organizations).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__individual_right_reading, individual_firearm_owners).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, mass_shooting_victims).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, domestic_violence_victims).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, firearm_suicide_decedents).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, urban_gun_violence_communities).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, state_and_local_regulators).
narrative_ontology:constraint_vindicates(second_amendment_boundary__individual_right_reading, natural_rights_predate_constitutional_text).
narrative_ontology:constraint_vindicates(second_amendment_boundary__individual_right_reading, self_defense_is_a_core_liberty_interest).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold a constitutionally protected right to keep and bear arms for self-defense, unconnected to militia service. Can acquire, carry, and use firearms with regulation now facing text-history-and-tradition scrutiny that strikes many restrictions. Exit from this framework is not sought by this group; the reading is their preferred equilibrium.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, individual_firearm_owners, beneficiary,
    moderate, biographical, mobile, national).

% Operate in a market where constitutional protection of the underlying right constrains regulatory reach over sales, design, and liability. Enjoy statutory liability shields (PLCAA) whose political durability is reinforced by the individual-right framing. Can relocate production/distribution across state lines to exploit regulatory variance.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, firearms_manufacturers, beneficiary,
    organized, generational, arbitrage, national).

% Litigate, lobby, and fund the doctrinal architecture (originalist historical argument, test-case selection, amicus strategy) that established and now maintains this reading as controlling law. Set the interpretive agenda through strategic litigation and shape which historical analogues courts credit under text-history-and-tradition analysis.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, gun_rights_advocacy_organizations, agenda_setter,
    institutional, generational, arbitrage, national).

% Bear the lethal cost of firearm access enabled and constitutionally insulated by this reading. Have no ex ante ability to exit the risk environment the reading helps sustain; harm is realized, not anticipatory, and cannot be undone through post hoc regulatory response once this reading constrains what response is permissible.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, mass_shooting_victims, payer,
    powerless, immediate, trapped, national).

% Face elevated lethality risk when abusers retain firearm access; some protective-order disarmament statutes have been narrowed or contested under the individual-right framework's skepticism of categorical restrictions. Exit from the relationship does not remove the risk while access remains constitutionally protected.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, domestic_violence_victims, payer,
    powerless, immediate, trapped, national).

% Firearm access during acute crisis substantially raises suicide completion rates due to lethality and speed; the individual-right framing constrains waiting-period, red-flag, and storage regulation that would otherwise interrupt access during crisis windows. This population cannot be named individually ex ante but is statistically certain to exist under the current access regime.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, firearm_suicide_decedents, payer,
    powerless, immediate, trapped, national).

% Live with elevated firearm-homicide exposure connected to trafficking and diversion from the legal secondary market that constitutional protection of possession makes harder to regulate at the point of sale and resale. Geographic and economic exit from high-violence neighborhoods is limited by cost and social ties.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, urban_gun_violence_communities, payer,
    powerless, biographical, constrained, regional).

% Historically enacted licensing, waiting-period, assault-weapon, and carry regulations calibrated to local violence conditions; now must satisfy a historical-analogue test that discounts contemporary public-safety data and requires close 18th/19th-century analogues, sharply narrowing available policy tools regardless of local democratic preference.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, state_and_local_regulators, payer,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(second_amendment_boundary__individual_right_reading, state_and_local_regulators, excluded).

% Applies the individual-right, text-history-and-tradition framework to adjudicate specific statutes, producing a growing and often inconsistent body of historical-analogue case law that determines which regulations survive.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, federal_appellate_judiciary, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_boundary__individual_right_reading, diffuse).
narrative_ontology:fixing_cost_class(second_amendment_boundary__individual_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, judicially enforceable baseline that individuals may rely on when acquiring and possessing firearms for self-defense, insulating that expectation from shifting legislative majorities.
% TRANSFER_FUNCTION: Moves regulatory authority away from state and local legislatures and toward courts applying historical analogy; moves risk from firearm owners and industry (who retain access and market certainty) onto third parties exposed to firearm violence and self-harm who bear costs the now-constrained regulatory toolkit could otherwise mitigate.
% ABSENT_VOICES: Mass shooting survivors' families, domestic violence victims, and firearm suicide prevention researchers are rarely parties to the litigation that sets the doctrine; the historical record consulted is selected and argued by litigants with a stake in the outcome, not by public health authorities or the populations bearing downstream harm.
% DISAPPEARANCE_RATIONALE: If this reading were overturned in favor of the militia-conditioned reading, state and local legislatures would regain latitude to enact licensing regimes, waiting periods, red-flag laws, and possession restrictions currently invalidated or chilled under text-history-and-tradition review; the firearms market's constitutional insulation from regulation would substantially narrow.
% FOUNDING_PROBLEM: The doctrine was built to resolve genuine ambiguity in the Second Amendment's text — whether the operative 'right of the people to keep and bear Arms' is conditioned by the prefatory militia clause — in favor of a reading protecting individual self-defense against government overreach and rising urban crime concerns in the late 20th century.
% FOUNDING_PROBLEM_CORROBORATION: Originalist legal scholars and gun-rights litigators attest the reading recovers the Framers' actual understanding of a pre-existing natural right. Historians of the militia system, public health researchers studying firearm mortality, and dissenting justices (Stevens, Breyer in Heller; Breyer in Bruen) attest from outside the beneficiary set that the historical record is more contested than the majority opinions present and that the reading's practical effect is measured in preventable deaths, not merely doctrinal fidelity.
narrative_ontology:disappearance_verdict(second_amendment_boundary__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_boundary__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_boundary__individual_right_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_boundary__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_boundary__individual_right_reading, 0.61, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored at 0.61 and rising because the doctrine has hardened since Heller: Bruen's text-history-and-tradition test has invalidated or chilled a widening set of state regulations (licensing regimes, sensitive-place restrictions, some domestic-violence disarmament provisions), transferring an increasing share of regulatory authority to courts applying historically selective analogy while population-level harm indicators (mass shootings, firearm suicide rates) have not declined. Suppression (0.52, structural, unscaled) reflects the doctrine's active foreclosure of legislative alternatives that would otherwise be available under ordinary rational-basis or intermediate scrutiny — this is not merely persuasive precedent but binding constitutional constraint enforced through judicial invalidation of statutes. Theater ratio is comparatively low (0.28) because the coordination function (a stable possession right individuals can rely on) is genuinely operative, not merely performed — this is a tangled_rope, not a piton: real coordination coexists with real extraction. Accessibility collapse (0.42) is moderate rather than high because political and litigation avenues to alter or narrow the doctrine remain open (legislative override is foreclosed by constitutional status, but doctrinal narrowing through future case law or a constitutional amendment remains a live, if difficult, avenue) — this is not a Mountain; it is a constructed doctrinal settlement with identifiable beneficiaries and victims. Resistance is high (0.78) — the doctrine is fiercely contested by state legislatures, public health researchers, gun-violence-prevention organizations, and dissenting jurists.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual firearm owners and the firearms industry sit near the beneficiary end: the right is theirs to exercise, and industry gains market certainty against regulatory constriction — mobile/arbitrage exit options reflect their ability to operate across favorable jurisdictions. Gun-rights advocacy organizations are the agenda-setting seat: institutional power, generational time horizon, arbitrage exit (able to select forums and test cases strategically). The payer seats — mass shooting victims, domestic violence victims, firearm suicide decedents, urban gun violence communities — are powerless and trapped: harm is realized at the moment of the constraint's operation, with no anticipatory exit available, which the directionality derivation should place at or near the full-target end (d approaching 1.0). State and local regulators are institutional in power but constrained in exit — they retain formal authority to legislate but see their statutes struck down under the historical-analogue test, producing a genuine institutional-target relationship rather than a beneficiary one, despite their nominal power level.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope (not snare, not mountain) prevents two mislabeling errors. First, it avoids treating the doctrine as pure extraction: there is a genuine coordination function — a stable, judicially-enforceable expectation interest that individuals can rely on when acquiring firearms for self-defense, which is a real good distinct from any rent extracted around it. Second, it avoids treating the doctrine as natural law (which the individual-right camp's own 'pre-existing right' framing invites, i.e. rights that predate and are merely recognized by the Constitution) — the schema's Mountain gate requires emerges_naturally: true and near-zero suppression/resistance, neither of which honestly describes a doctrine actively defended through fiercely contested 5-4 and 6-3 rulings, extensive resistance from state legislatures and public health authorities, and a rising extraction trajectory. The tangled_rope classification requires both a real beneficiary group (satisfied: firearm owners, industry, advocacy organizations) and a real victim group with active enforcement (satisfied: the harmed populations named above, enforced through judicial invalidation of contrary state statutes) — both are structurally present here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_individual_right,
    'Is the individual-right reading the historically correct interpretation of the Second Amendment''s text, or is it a reading favored because it serves identifiable beneficiaries (firearms industry, gun-rights advocacy organizations) over the militia-conditioned or insurrectionist readings?',
    'Historical linguistic and legislative-record analysis of 18th-century usage of ''keep and bear arms,'' militia statutes, and ratification debates; comparison of how founding-era state constitutions treated analogous rights; assessment of whether Heller''s historical methodology withstands scrutiny from historians outside the litigation record it relied upon.',
    'If the militia-conditioned reading is the better-supported historical account, this constraint would be recharacterized as a constructed doctrinal settlement serving industry and advocacy interests rather than a recovery of original meaning, strengthening the tangled_rope classification and its extraction component. If the individual-right reading is historically well-founded, the coordination function (protecting a genuine pre-existing liberty) would be given more analytical weight relative to the extraction component, though the victim set and enforcement mechanism would remain unchanged.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_individual_right, conceptual, 'Whether the individual-right reading is genuine historical recovery or a beneficiary-serving construction — the central committer-axis question for this kernel.').

omega_variable(
    sibling_reading_structural_delta,
    'What would change structurally if courts adopted the militia-conditioned reading instead of the individual-right reading?',
    'Comparative analysis of pre-Heller Second Amendment jurisprudence (which largely followed a militia-conditioned or collective-rights framework) against post-Heller/Bruen case outcomes, isolating which specific state regulations were invalidated solely due to the reading shift.',
    'Under the militia-conditioned reading, state and local regulators would regain the regulatory latitude currently foreclosed by the historical-analogue test; the beneficiary set (firearms industry, individual owners) would lose constitutional insulation and shift toward ordinary political-process exposure; the victim set here would substantially shrink as regulatory tools for licensing, waiting periods, and red-flag laws became available again without heightened constitutional scrutiny.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, empirical, 'The structural delta this reading produces relative to its sibling readings, documented per Rule 2 rather than embedded in this constraint''s classification.').

omega_variable(
    historical_analogue_test_manipulability,
    'Is the text-history-and-tradition test (Bruen) a neutral historical inquiry or a manipulable standard that systematically favors whichever historical analogues litigants and sympathetic judges select?',
    'Track record analysis of post-Bruen circuit split outcomes: how frequently courts applying the same test to similar statutes reach opposite conclusions, and whether outcome correlates with judicial appointment provenance rather than historical record quality.',
    'If the test is substantially manipulable, the doctrine''s suppression and theater_ratio should be read as understating true discretionary judicial power exercised under a veneer of historical objectivity — this would push the classification further toward extraction and away from principled coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_analogue_test_manipulability, empirical, 'Whether the doctrine''s central adjudicative test is neutral or outcome-driven.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__individual_right_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_boundary__individual_right_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(seco_tr_t0, observed).
narrative_ontology:measurement(seco_tr_t8, second_amendment_boundary__individual_right_reading, theater_ratio, 8, 0.17).
narrative_ontology:measurement_basis(seco_tr_t8, observed).
narrative_ontology:measurement(seco_tr_t16, second_amendment_boundary__individual_right_reading, theater_ratio, 16, 0.19).
narrative_ontology:measurement_basis(seco_tr_t16, observed).
narrative_ontology:measurement(seco_tr_t24, second_amendment_boundary__individual_right_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement_basis(seco_tr_t24, observed).
narrative_ontology:measurement(seco_tr_t32, second_amendment_boundary__individual_right_reading, theater_ratio, 32, 0.25).
narrative_ontology:measurement_basis(seco_tr_t32, observed).
narrative_ontology:measurement(seco_tr_t40, second_amendment_boundary__individual_right_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(seco_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_boundary__individual_right_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(seco_be_t0, observed).
narrative_ontology:measurement(seco_be_t8, second_amendment_boundary__individual_right_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement_basis(seco_be_t8, observed).
narrative_ontology:measurement(seco_be_t16, second_amendment_boundary__individual_right_reading, base_extractiveness, 16, 0.45).
narrative_ontology:measurement_basis(seco_be_t16, observed).
narrative_ontology:measurement(seco_be_t24, second_amendment_boundary__individual_right_reading, base_extractiveness, 24, 0.53).
narrative_ontology:measurement_basis(seco_be_t24, observed).
narrative_ontology:measurement(seco_be_t32, second_amendment_boundary__individual_right_reading, base_extractiveness, 32, 0.58).
narrative_ontology:measurement_basis(seco_be_t32, observed).
narrative_ontology:measurement(seco_be_t40, second_amendment_boundary__individual_right_reading, base_extractiveness, 40, 0.61).
narrative_ontology:measurement_basis(seco_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_boundary__individual_right_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(seco_su_t0, observed).
narrative_ontology:measurement(seco_su_t8, second_amendment_boundary__individual_right_reading, suppression_requirement, 8, 0.35).
narrative_ontology:measurement_basis(seco_su_t8, observed).
narrative_ontology:measurement(seco_su_t16, second_amendment_boundary__individual_right_reading, suppression_requirement, 16, 0.4).
narrative_ontology:measurement_basis(seco_su_t16, observed).
narrative_ontology:measurement(seco_su_t24, second_amendment_boundary__individual_right_reading, suppression_requirement, 24, 0.44).
narrative_ontology:measurement_basis(seco_su_t24, observed).
narrative_ontology:measurement(seco_su_t32, second_amendment_boundary__individual_right_reading, suppression_requirement, 32, 0.48).
narrative_ontology:measurement_basis(seco_su_t32, observed).
narrative_ontology:measurement(seco_su_t40, second_amendment_boundary__individual_right_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement_basis(seco_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_boundary__individual_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, militia_conditioned_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, insurrectionist_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, firearms_industry_liability_shield).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, state_firearm_licensing_regimes).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the second_amendment_boundary kernel: individual_right_reading (this story), militia_conditioned_reading, and insurrectionist_reading. Each reading is authored as a separate, ε-invariant constraint with its own beneficiary/victim structure and classification per the ε-invariance principle — this story does not average over or describe the sibling readings, only links to them structurally. This reading also structurally influences downstream constraints governing firearms industry liability protection and the viability of state licensing regimes, since the historical-analogue test constrains what those downstream constraints can require.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
