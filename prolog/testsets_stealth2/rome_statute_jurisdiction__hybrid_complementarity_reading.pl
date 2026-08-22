% ============================================================================
% CONSTRAINT STORY: rome_statute_jurisdiction__hybrid_complementarity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rome_statute_jurisdiction__hybrid_complementarity_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: rome_statute_jurisdiction__hybrid_complementarity_reading
 *   human_readable: Rome Statute Jurisdiction Under Complementarity Deference (Hybrid Reading)
 *   domain: international_law/treaty_interpretation/institutional_authority
 *
 * SUMMARY:
 *   A permanent international criminal court exercises jurisdiction over
 *   genocide, crimes against humanity, war crimes, and aggression, gated by
 *   complementarity: it acts only where national systems are unwilling or
 *   unable to prosecute genuinely. Authority reaches consenting states
 *   parties, situations referred by the Security Council (binding even
 *   non-party nationals), and states accepting ad hoc jurisdiction, while the
 *   nationals of non-consenting great powers remain formally reachable only
 *   through a Council referral their own veto governs. Enforcement is wholly
 *   cooperative: the Court fields no police, arrest depends on custodial
 *   states, and the budget depends on assessments skewed toward European
 *   donors. The result is a standing accountability forum that delivers real
 *   prosecutions and reparations in weak-state situations while structurally
 *   sparing the powerful, a genuine coordination achievement carrying a
 *   systematic asymmetry inside the same architecture. Epsilon is authored
 *   for this standing arrangement as operated, assessed by this reading's own
 *   lights; the endorsed alternatives of sibling readings are different
 *   constraints in different files.
 *
 * KEY AGENTS:
 *   - icc_assembly_of_states_parties: agenda-setter (institutional/constrained) — administers budget, elections, and amendments; cannot relocate without dissolving the regime
 *   - icc_office_of_the_prosecutor: agenda-setter and receipt seat (institutional/identity_locked) — selects situations and constitutes its own standing through them
 *   - atrocity_victims_and_survivor_communities: primary intended beneficiary (powerless/trapped) — receives forum access where national courts fail
 *   - cooperating_european_donor_states: financier-beneficiary (institutional/mobile) — funds and shields the Court while absorbing its frictions
 *   - transitional_referring_governments: strategic beneficiary (moderate/constrained) — trades sovereignty exposure for rival-delegitimization
 *   - security_council_permanent_members: privileged beneficiary via referral asymmetry (powerful/arbitrage) — direct the Court while preserving veto-governed immunity
 *   - indicted_nationals_of_weak_states: primary bearer of legal exposure (powerless/trapped)
 *   - african_union_targeted_member_states: organized payer bloc (organized/constrained) — coalition resistance failed to reshape the docket
 *   - host_states_bearing_cooperation_costs: territorial payer (moderate/constrained) — host operations, absorb risk, occasionally appear in the dock
 *   - nonparty_resistant_great_powers: excluded lever-pullers (powerful/arbitrage) — sanction, lobby, and immunize from outside the assembly
 *   - international_law_scholars: analytical observers (analytical/analytical) — supply doctrine to every coalition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.65).
domain_priors:suppression_score(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.42).
domain_priors:theater_ratio(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__hybrid_complementarity_reading, tangled_rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__hybrid_complementarity_reading, "Rome Statute Jurisdiction Under Complementarity Deference (Hybrid Reading)").
narrative_ontology:topic_domain(rome_statute_jurisdiction__hybrid_complementarity_reading, "international_law/treaty_interpretation/institutional_authority").

domain_priors:requires_active_enforcement(rome_statute_jurisdiction__hybrid_complementarity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__hybrid_complementarity_reading, '98a855fe-f991-4218-96c5-17c7c60a05c7').
narrative_ontology:cs_kernel_codification('98a855fe-f991-4218-96c5-17c7c60a05c7', fixed_text).
narrative_ontology:cs_authority_grounding('98a855fe-f991-4218-96c5-17c7c60a05c7', lineage).
narrative_ontology:cs_interpretation_layer_present('98a855fe-f991-4218-96c5-17c7c60a05c7').
narrative_ontology:cs_reading_relation('98a855fe-f991-4218-96c5-17c7c60a05c7', rome_statute_jurisdiction__sovereigntist_reading, coexists_with).
narrative_ontology:cs_reading_relation('98a855fe-f991-4218-96c5-17c7c60a05c7', rome_statute_jurisdiction__universalist_reading, influences).
narrative_ontology:cs_axiom('98a855fe-f991-4218-96c5-17c7c60a05c7', foundational, legitimate_jurisdiction_conditioned_on_state_inability_or_unwillingness).
narrative_ontology:cs_axiom_status(legitimate_jurisdiction_conditioned_on_state_inability_or_unwillingness, holdable).
narrative_ontology:cs_axiom_grounding('98a855fe-f991-4218-96c5-17c7c60a05c7', legitimate_jurisdiction_conditioned_on_state_inability_or_unwillingness, conventional).
narrative_ontology:cs_axiom('98a855fe-f991-4218-96c5-17c7c60a05c7', foundational, atrocity_accountability_not_fully_contingent_on_prior_consent).
narrative_ontology:cs_axiom_status(atrocity_accountability_not_fully_contingent_on_prior_consent, holdable).
narrative_ontology:cs_axiom_grounding('98a855fe-f991-4218-96c5-17c7c60a05c7', atrocity_accountability_not_fully_contingent_on_prior_consent, deontological).
narrative_ontology:cs_reference_frame('98a855fe-f991-4218-96c5-17c7c60a05c7', residual_universal_authority_through_consent_gatekeeping).
narrative_ontology:cs_drift_state('98a855fe-f991-4218-96c5-17c7c60a05c7', post_ukraine_warrant_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('98a855fe-f991-4218-96c5-17c7c60a05c7', '').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__hybrid_complementarity_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, atrocity_victims_and_survivor_communities).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, cooperating_european_donor_states).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, international_criminal_justice_professionals).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, transitional_referring_governments).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, security_council_permanent_members).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, indicted_nationals_of_weak_states).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, african_union_targeted_member_states).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, host_states_bearing_cooperation_costs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, icc_office_of_the_prosecutor).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, host_states_bearing_cooperation_costs).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, cooperating_european_donor_states).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, transitional_referring_governments).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__hybrid_complementarity_reading, complementarity_admissibility_doctrine).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__hybrid_complementarity_reading, individual_accountability_over_state_immunity).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__hybrid_complementarity_reading, positive_complementarity_capacity_building).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The treaty body of all states parties. It adopts the budget, elects judges and prosecutors, amends the Statute, and passes cooperation resolutions. Individual members can denounce the treaty and a handful have, but the assembly as a body cannot relocate its functions without dissolving the regime its members built, and its largest funders prefer repair to exit.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, icc_assembly_of_states_parties, agenda_setter,
    institutional, generational, constrained, global).

% Selects which situations to examine, opens investigations, issues arrest requests, and argues admissibility before the chambers. Every new situation enlarges its mandate, staffing, and budget line; its existence and standing are constituted by the Statute's jurisdictional scheme, and its prosecutors serve single nine-year terms inside that scheme.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, icc_office_of_the_prosecutor, agenda_setter,
    institutional, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__hybrid_complementarity_reading, icc_office_of_the_prosecutor, beneficiary).

% Survivors of mass violence in situation countries who receive the possibility of acknowledgment, reparations through the trust fund, and a forum that names perpetrators when national courts will not. Most lack independent means to reach The Hague, depend on intermediaries for participation, and have no alternative permanent forum if this one declines their situation.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, atrocity_victims_and_survivor_communities, beneficiary,
    powerless, biographical, trapped, regional).

% Provide the large majority of assessed contributions, arrest cooperation, and diplomatic shielding for the Court. They gain a foreign-policy instrument that projects accountability norms without deploying force and a way to answer atrocity constituencies at home. They also absorb the budget's growth and the diplomatic friction that warrants against allied governments create.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, cooperating_european_donor_states, beneficiary,
    institutional, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__hybrid_complementarity_reading, cooperating_european_donor_states, payer).

% Post-conflict governments that refer situations on their own territory to delegitimize armed opponents, externalize justice costs, and signal reform credentials to donors. They accept jurisdiction over their own nationals as the price of referral, and some later chafe when the Court's attention turns toward their own forces.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, transitional_referring_governments, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__hybrid_complementarity_reading, transitional_referring_governments, payer).

% Judges, registry staff, investigators, NGO litigators, and academics whose careers, funding lines, and professional networks are built around the Court's docket. New situations generate positions and consultancies; a shrinking docket threatens the ecosystem that services the institution.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, international_criminal_justice_professionals, beneficiary,
    organized, biographical, mobile, global).

% Hold referral power that binds even non-party states' nationals, as exercised for Darfur and Libya, and veto-backed deferral power over any situation. Two of the five are states parties; three are not. Their own forces remain outside the Court's reach unless the Council refers their conduct, an exemption they preserve by veto while directing the Court's attention elsewhere.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, security_council_permanent_members, beneficiary,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__hybrid_complementarity_reading, security_council_permanent_members, agenda_setter).

% Suspects and convicted persons from situation countries, including militia commanders and sitting or former heads of state, who face warrants with no appellate route outside the Court. Those in custody have no exit; those at liberty evade by remaining in friendly territory, as the travel records of indictee heads of state demonstrate.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, indicted_nationals_of_weak_states, payer,
    powerless, biographical, trapped, regional).

% The bloc whose members supplied nearly all early situations. Collectively they pursued a coordinated withdrawal strategy after the Sudan warrant named a sitting head of state, forced deferral debates, and watched members depart or attempt departure and reverse course under domestic court rulings and donor pressure. They remain inside the system their objections failed to reshape, supplying cooperation while arguing the docket reflects power rather than the distribution of atrocity.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, african_union_targeted_member_states, payer,
    organized, generational, constrained, continental).

% States that physically host Court operations, including field offices, witness protection, and detention transfers, absorbing security risks and sovereignty frictions on their territory. Several receive protection and development support in return, and the forces of some have themselves appeared in the dock.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, host_states_bearing_cooperation_costs, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__hybrid_complementarity_reading, host_states_bearing_cooperation_costs, beneficiary).

% Major powers outside the treaty that reject jurisdiction over their nationals, negotiate bilateral immunity agreements with third states, have sanctioned Court personnel when investigations touched their allies, and lobby other governments against cooperation. They engage selectively, supporting some referrals and blocking others, while remaining outside the assembly that sets the rules they are subjecting others to.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, nonparty_resistant_great_powers, excluded,
    powerful, generational, arbitrage, global).

% Analysts who map the Statute's interpretive disputes, publish on admissibility and complementarity jurisprudence, and supply the doctrinal arguments each governmental coalition deploys. They hold no vote in the assembly and bear no warrant.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, international_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rome_statute_jurisdiction__hybrid_complementarity_reading, icc_office_of_the_prosecutor).
narrative_ontology:fixing_cost_class(rome_statute_jurisdiction__hybrid_complementarity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standing, common-definition forum for prosecuting genocide, crimes against humanity, war crimes, and aggression when national systems cannot or will not; the complementarity filter channels pressure toward domestic prosecution, standardizes atrocity law across jurisdictions, and gives victims and civil society a focal point for accountability claims.
% TRANSFER_FUNCTION: Moves legal exposure toward nationals of situation countries, overwhelmingly weaker states; moves money from state-party assessments, disproportionately European, into the Court's budget; moves legitimacy toward referring governments; and places enforcement labor onto cooperating and host states, while leaving the nationals of non-consenting great powers formally reachable only through a Council referral their own veto governs.
% ABSENT_VOICES: Non-party great powers would contest jurisdictional reach but sit outside the assembly; defense communities for indigent suspects are chronically under-resourced relative to the prosecution; victims of conduct inside great-power spheres have no procedural path to place their situation on the docket; and communities swept into the Kenya election-cases objected that the Court criminalized whole electoral blocs, an objection that arrived late and shaped reform only afterward.
% DISAPPEARANCE_RATIONALE: Situation-country accountability strategies, donor human-rights diplomacy, the African Union's judicial posture, thousands of professional careers, and live trial dockets would all require reorganization overnight; ad hoc tribunal proposals and expanded domestic universal-jurisdiction litigation would absorb part of the load at far higher per-case cost, and several sitting indictments would simply lapse.
% FOUNDING_PROBLEM: Impunity for atrocity crimes: after the Nuremberg ad hoc model faded, no standing mechanism existed to hold individuals criminally accountable for genocide, crimes against humanity, and war crimes when national courts were unable or unwilling, a gap made acute by Yugoslavia and Rwanda in the 1990s.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the Security Council, including non-party members, referred Darfur and Libya, attesting live demand for the forum even among states that reject its premise; UN General Assembly resolutions and independent commissions of inquiry repeatedly document continuing atrocity impunity; human-rights organizations outside the Court's funding ecosystem document unprosecuted situations; and the resisting states themselves concede the impunity problem while disputing this remedy. No party with standing claims the founding problem is solved.
narrative_ontology:disappearance_verdict(rome_statute_jurisdiction__hybrid_complementarity_reading, world_rearranges).
narrative_ontology:founding_problem_status(rome_statute_jurisdiction__hybrid_complementarity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rome_statute_jurisdiction__hybrid_complementarity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rome_statute_jurisdiction__hybrid_complementarity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.65, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rome_statute_jurisdiction__hybrid_complementarity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rome_statute_jurisdiction__hybrid_complementarity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rome_statute_jurisdiction__hybrid_complementarity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.65 for the standing arrangement: the Court delivers dozens of convictions, reparations, and standard-setting that no alternative forum provides at comparable scale, while the docket's composition, the P5 immunity structure, and the cooperation burden's placement constitute a systematic asymmetry running through the same institution. Suppression is authored at 0.42 as a raw structural property, unscaled by the engine: coercive apparatus is thin (no police force), compliance is purchased through conditionality and membership benefits, and exit exists and has been used (withdrawals by Burundi and the Philippines, attempted withdrawals reversed in Gambia and South Africa). Theater_ratio at 0.31 reflects anniversary diplomacy, universal-ratification ceremony, and session theatrics against a functioning investigative core, a core the 2025 Manila arrest demonstrated is real. Accessibility_collapse at 0.38: alternatives persist (national prosecution through the complementarity channel, ad hoc and hybrid tribunals, domestic universal-jurisdiction statutes, withdrawal). Resistance at 0.62: sustained organized opposition including the African Union withdrawal strategy, United States sanctions on Court personnel, the bilateral-immunity-agreement campaign, and budget obstruction. The temporal series run on one shared nine-point grid (all three metrics at every point, 2002-2026) so no metric row borrows another's end-state. The suppression_requirement series is authored deliberately: this story specifically tracks enforcement-capacity change, which built through 2011, decayed through the 2014-2017 cooperation collapse, and rebuilt through 2026 as warrant execution resumed; the scalar suppression and the series measure different quantities (structural closure of alternatives versus mobilized enforcement effort) and are not reconciled to each other. The 2020 dip in the extraction series marks the deference episode in which investigation of conduct touching a sanctioning great power was deprioritized under economic pressure; the subsequent recovery tracks the docket's turn toward non-African targets. The oscillation is a backlash-relaxation-rebuild cycle driven by external pressure responses rather than intermittent reinforcement, and the base_properties scalars reflect the interval-end state.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seats compute differently from the same structure. From the indicted-national and African Union seats, the arrangement presents as a forum whose coordination story covers a docket that spares the powerful: the engine should compute a heavily target-side experience there, snare-flavored at the extreme payer seats. From the assembly and donor seats, the same structure presents as an accountability infrastructure they fund, staff, and steer: rope-flavored. The prosecutor's office experiences mandate growth with each new situation. Same-level lateral dynamics matter: African Union member states and European donor states are nominally identical actors, both states parties bound by the same treaty at the same formal level, yet their exits differ sharply, donors are mobile (they can fund alternatives and walk away cheaply) while the African bloc is constrained (dependent on the Court for internal-conflict coverage, exposed to donor pressure on exit, as the Gambian and South African reversals showed). Inter-institutionally, the Security Council holds referral and deferral levers over a court whose rules it largely sits outside: two of five permanent members are parties, three are not, and the veto preserves their nationals' immunity while their referrals bind everyone else's. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations map to low-directionality seats: victims and survivor communities receive the forum's core service; European donors receive a foreign-policy instrument; referring governments receive rival-delegitimization; professionals receive an ecosystem; the permanent members receive the referral asymmetry and veto-governed immunity. The victim declarations map to high-directionality seats: indicted nationals of weak states bear the legal exposure with trapped exit; the African Union bloc bears the asymmetric docket with constrained coalition exit; host states bear territorial cooperation costs. One override is declared: the derivation would place the European donor states near the full-beneficiary pole from their beneficiary listing alone, but they fund the large majority of the budget and supply most arrest cooperation, so their net position is subsidized-but-substantially-contributing; the override to 0.25 registers that contribution while keeping the subsidy sign. Trapped exit pushes the indicted-national seat toward the full-target end; arbitrage exit holds the permanent members and the resistant great powers away from any target-side computation despite their exposure to rhetorical pressure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem, atrocity impunity, remains live and is corroborated from outside the benefiting parties, so no mandatrophy is declared and the status-times-verdict pair (live, world_rearranges) raises no zombie flag. The classification work here prevents mislabeling in both directions. Reading the Court purely as humanitarian coordination would miss the selective-exposure asymmetry, the P5 immunity structure, and the cooperation burdens placed on the weak, and would mislabel a substantially extractive hybrid as pure coordination. Reading it purely as neo-colonial extraction would miss the convictions, reparations, and standard-setting actually delivered where national systems failed, and would mislabel a functioning coordination core as a pure snare. The tangled_rope claim holds both facts in one structure: a genuine coordination function (common definitions, standing forum, positive-complementarity pressure on domestic courts) and an asymmetric extraction pattern (selective enforcement, veto-governed immunity, burdens on the weak), held together by active enforcement in the form of cooperation politics, assembly discipline, and conditionality. Fixing the asymmetry is prohibitive for whoever could fix it: the assembly would need either great-power accession it cannot compel or acceptance of a permanently hobbled enforcement reach, and the gains from fixing diffuse across seats while the costs concentrate on the steering coalition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This file instantiates the hybrid_complementarity_reading of kernel rome_statute_jurisdiction; the disagreement among readings is located in whether residual authority exists absent prior consent, and whether admissibility may legitimately be conditioned on state capacity. What structural differences would adopting a sibling reading produce?',
    'Track Assembly of States Parties amendment outcomes, Security Council referral and deferral practice, and chamber admissibility jurisprudence: sovereigntist consolidation would delete Council-referral reach over non-party nationals and harden immunity-agreement practice; universalist consolidation would extend jurisdiction to non-party nationals without referral.',
    'Under the sovereigntist reading the victim set shrinks to consenting-state nationals and extraction from non-consenting targets falls toward zero; under the universalist reading extraction rises sharply for great-power nationals and suppression climbs because enforcement must be coerced rather than requested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: one reading of the Rome Statute jurisdiction kernel; sibling readings would redraw the victim set and the extraction profile.').

omega_variable(
    situation_selection_bias,
    'Is the concentration of opened situations in weaker states driven by referral economics (self-referrals, Council availability, cooperation willingness) or by selection bias against particular regions and their patrons?',
    'Counterfactual comparison of comparable atrocity situations inside great-power spheres that were never opened against the opened docket, controlling for evidentiary access and territorial control.',
    'If bias-driven, the coordination story covers more extraction than referral economics explains and payer-seat classifications shift toward pure extraction; if economics-driven, the asymmetry is a cost-of-operation artifact rather than a design feature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(situation_selection_bias, empirical, 'Whether the docket''s geographic skew reflects selection bias or referral mechanics.').

omega_variable(
    enforcement_without_state_interest,
    'Can the Court secure arrest of a great-power-aligned indictee absent a domestic political realignment inside the custodial state?',
    'Observe warrant-execution outcomes for sitting and former heads of state across custodial jurisdictions; code whether any execution preceded a custodial-state interest realignment rather than followed one.',
    'If execution never precedes realignment, residual authority over the powerful is nominal and this reading operationally collapses toward strict consent-conditionality despite its doctrinal form.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_without_state_interest, empirical, 'Whether residual universal authority is real or nominal against great-power interests.').

omega_variable(
    complementarity_genuineness,
    'Does complementarity operate as genuine capacity-building that produces domestic prosecutions, or as a deference ritual that launders non-prosecution while preserving cooperation relationships?',
    'Compare domestic prosecution rates in situation countries before and after Court engagement against matched atrocity contexts outside the docket.',
    'If ritual, the coordination half of the arrangement thins and effective extraction rises across payer seats; if genuine, the coordination function is robust and the measured extraction is predominantly the asymmetry component.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(complementarity_genuineness, empirical, 'Whether the complementarity filter builds accountability or launders its absence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__hybrid_complementarity_reading, 2002, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rsj_hybrid_tr_t2002, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2002, 0.15).
narrative_ontology:measurement_basis(rsj_hybrid_tr_t2002, observed).
narrative_ontology:measurement(rsj_hybrid_tr_t2005, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2005, 0.18).
narrative_ontology:measurement_basis(rsj_hybrid_tr_t2005, observed).
narrative_ontology:measurement(rsj_hybrid_tr_t2008, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2008, 0.22).
narrative_ontology:measurement_basis(rsj_hybrid_tr_t2008, observed).
narrative_ontology:measurement(rsj_hybrid_tr_t2011, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2011, 0.26).
narrative_ontology:measurement_basis(rsj_hybrid_tr_t2011, observed).
narrative_ontology:measurement(rsj_hybrid_tr_t2014, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2014, 0.32).
narrative_ontology:measurement_basis(rsj_hybrid_tr_t2014, observed).
narrative_ontology:measurement(rsj_hybrid_tr_t2017, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2017, 0.38).
narrative_ontology:measurement_basis(rsj_hybrid_tr_t2017, observed).
narrative_ontology:measurement(rsj_hybrid_tr_t2020, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2020, 0.36).
narrative_ontology:measurement_basis(rsj_hybrid_tr_t2020, observed).
narrative_ontology:measurement(rsj_hybrid_tr_t2023, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2023, 0.33).
narrative_ontology:measurement_basis(rsj_hybrid_tr_t2023, observed).
narrative_ontology:measurement(rsj_hybrid_tr_t2026, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2026, 0.31).
narrative_ontology:measurement_basis(rsj_hybrid_tr_t2026, projected).

% Extraction over time
narrative_ontology:measurement(rsj_hybrid_be_t2002, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2002, 0.4).
narrative_ontology:measurement_basis(rsj_hybrid_be_t2002, observed).
narrative_ontology:measurement(rsj_hybrid_be_t2005, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2005, 0.47).
narrative_ontology:measurement_basis(rsj_hybrid_be_t2005, observed).
narrative_ontology:measurement(rsj_hybrid_be_t2008, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2008, 0.54).
narrative_ontology:measurement_basis(rsj_hybrid_be_t2008, observed).
narrative_ontology:measurement(rsj_hybrid_be_t2011, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2011, 0.57).
narrative_ontology:measurement_basis(rsj_hybrid_be_t2011, observed).
narrative_ontology:measurement(rsj_hybrid_be_t2014, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2014, 0.6).
narrative_ontology:measurement_basis(rsj_hybrid_be_t2014, observed).
narrative_ontology:measurement(rsj_hybrid_be_t2017, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2017, 0.64).
narrative_ontology:measurement_basis(rsj_hybrid_be_t2017, observed).
narrative_ontology:measurement(rsj_hybrid_be_t2020, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2020, 0.61).
narrative_ontology:measurement_basis(rsj_hybrid_be_t2020, observed).
narrative_ontology:measurement(rsj_hybrid_be_t2023, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2023, 0.63).
narrative_ontology:measurement_basis(rsj_hybrid_be_t2023, observed).
narrative_ontology:measurement(rsj_hybrid_be_t2026, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2026, 0.65).
narrative_ontology:measurement_basis(rsj_hybrid_be_t2026, projected).

% Suppression requirement over time
narrative_ontology:measurement(rsj_hybrid_su_t2002, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2002, 0.3).
narrative_ontology:measurement_basis(rsj_hybrid_su_t2002, observed).
narrative_ontology:measurement(rsj_hybrid_su_t2005, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2005, 0.4).
narrative_ontology:measurement_basis(rsj_hybrid_su_t2005, observed).
narrative_ontology:measurement(rsj_hybrid_su_t2008, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2008, 0.5).
narrative_ontology:measurement_basis(rsj_hybrid_su_t2008, observed).
narrative_ontology:measurement(rsj_hybrid_su_t2011, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2011, 0.52).
narrative_ontology:measurement_basis(rsj_hybrid_su_t2011, observed).
narrative_ontology:measurement(rsj_hybrid_su_t2014, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2014, 0.44).
narrative_ontology:measurement_basis(rsj_hybrid_su_t2014, observed).
narrative_ontology:measurement(rsj_hybrid_su_t2017, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2017, 0.4).
narrative_ontology:measurement_basis(rsj_hybrid_su_t2017, observed).
narrative_ontology:measurement(rsj_hybrid_su_t2020, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2020, 0.46).
narrative_ontology:measurement_basis(rsj_hybrid_su_t2020, observed).
narrative_ontology:measurement(rsj_hybrid_su_t2023, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2023, 0.55).
narrative_ontology:measurement_basis(rsj_hybrid_su_t2023, observed).
narrative_ontology:measurement(rsj_hybrid_su_t2026, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2026, 0.62).
narrative_ontology:measurement_basis(rsj_hybrid_su_t2026, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rome_statute_jurisdiction__hybrid_complementarity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__hybrid_complementarity_reading, universalist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__hybrid_complementarity_reading, sovereigntist_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'ICC jurisdiction' decomposes into three structurally distinct readings of the rome_statute_jurisdiction kernel: universalist_reading (mandate transcends consent), sovereigntist_reading (strict consent conditionality), and this file's hybrid_complementarity_reading (residual universal authority operating through consent gatekeeping). The readings carry different epsilon values, different victim sets, and different failure modes, so each is authored as its own story and cross-linked here. The hybrid reading is the mediating instance: its institutionalized deference structurally conditions what universalist advocates can realize, and its residual-authority premises are precisely what sovereigntists attack.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rome_statute_jurisdiction__hybrid_complementarity_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
