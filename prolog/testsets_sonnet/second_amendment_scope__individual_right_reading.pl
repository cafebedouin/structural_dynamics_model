% ============================================================================
% CONSTRAINT STORY: second_amendment_scope__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_scope__individual_right_reading, []).

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
 *   constraint_id: second_amendment_scope__individual_right_reading
 *   human_readable: Second Amendment as Individual Right Unconnected to Militia Service (Heller/McDonald Reading)
 *   domain: constitutional_law/political_theory/rights_jurisprudence
 *
 * SUMMARY:
 *   This story instantiates the individual-right reading of the Second
 *   Amendment scope kernel: the claim that the amendment protects a personal
 *   right to keep and bear arms unconnected to militia service, as
 *   articulated in District of Columbia v. Heller (2008) and extended in
 *   McDonald v. Chicago (2010) and New York State Rifle & Pistol Association
 *   v. Bruen (2022). This is one of three structurally distinct readings of
 *   the same textual kernel — the collective-right reading (state militia
 *   authority only, no individual claim) and the civic-right reading
 *   (individual right conditioned on militia-type participation) are separate
 *   constraints with their own ε values, beneficiary/victim structures, and
 *   network files. Do not average across readings; each is generated as a
 *   clean, ε-invariant claim per Rule 1. The historical arc modeled here runs
 *   from the pre-Heller era (militia-linked doctrine dominant, lower ε)
 *   through the doctrinal consolidation of Heller/McDonald/Bruen (rising ε as
 *   scrutiny standards harden and municipal regulatory latitude narrows).
 *
 * KEY AGENTS:
 *   - individual_gun_owners: primary declared beneficiary (moderate/mobile) — holds the protected right
 *   - firearms_industry: structural beneficiary (organized/arbitrage) — benefits from narrowed regulatory space
 *   - gun_rights_advocacy_organizations: agenda-setter (organized/arbitrage) — litigates and extends the doctrine
 *   - gun_violence_victims: primary payer (powerless/trapped) — bears costs of constrained regulation
 *   - municipalities_seeking_regulation: institutional payer (institutional/constrained) — loses regulatory latitude
 *   - supreme_court_majority_coalition: agenda-setter (institutional/analytical) — authors and administers the doctrine
 *   - legal_historians_and_originalist_scholars: analytical observer — assesses historical grounding
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__individual_right_reading, 0.58).
domain_priors:suppression_score(second_amendment_scope__individual_right_reading, 0.52).
domain_priors:theater_ratio(second_amendment_scope__individual_right_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_scope__individual_right_reading, "Second Amendment as Individual Right Unconnected to Militia Service (Heller/McDonald Reading)").
narrative_ontology:topic_domain(second_amendment_scope__individual_right_reading, "constitutional_law/political_theory/rights_jurisprudence").

domain_priors:requires_active_enforcement(second_amendment_scope__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__individual_right_reading, 'f5b3a455-01f4-4157-811b-7233b5266e0a').
narrative_ontology:cs_kernel_codification('f5b3a455-01f4-4157-811b-7233b5266e0a', fixed_text).
narrative_ontology:cs_authority_grounding('f5b3a455-01f4-4157-811b-7233b5266e0a', lineage).
narrative_ontology:cs_interpretation_layer_present('f5b3a455-01f4-4157-811b-7233b5266e0a').
narrative_ontology:cs_reading_relation('f5b3a455-01f4-4157-811b-7233b5266e0a', second_amendment_scope__collective_right_reading, forecloses).
narrative_ontology:cs_reading_relation('f5b3a455-01f4-4157-811b-7233b5266e0a', second_amendment_scope__civic_right_reading, coexists_with).
narrative_ontology:cs_axiom('f5b3a455-01f4-4157-811b-7233b5266e0a', foundational, self_defense_is_preexisting_individual_right).
narrative_ontology:cs_axiom_status(self_defense_is_preexisting_individual_right, holdable).
narrative_ontology:cs_axiom_grounding('f5b3a455-01f4-4157-811b-7233b5266e0a', self_defense_is_preexisting_individual_right, deontological).
narrative_ontology:cs_axiom('f5b3a455-01f4-4157-811b-7233b5266e0a', foundational, militia_clause_is_prefatory_not_conditioning).
narrative_ontology:cs_axiom_status(militia_clause_is_prefatory_not_conditioning, holdable).
narrative_ontology:cs_axiom_grounding('f5b3a455-01f4-4157-811b-7233b5266e0a', militia_clause_is_prefatory_not_conditioning, conventional).
narrative_ontology:cs_reference_frame('f5b3a455-01f4-4157-811b-7233b5266e0a', founding_era_natural_rights_self_defense).
narrative_ontology:cs_drift_state('f5b3a455-01f4-4157-811b-7233b5266e0a', post_bruen_contemporary, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('f5b3a455-01f4-4157-811b-7233b5266e0a', '').
narrative_ontology:cs_kernel_id(second_amendment_scope__individual_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, individual_gun_owners).
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, firearms_industry).
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, gun_rights_advocacy_organizations).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, gun_violence_victims).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, municipalities_seeking_regulation).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, communities_with_high_firearm_mortality).
narrative_ontology:constraint_vindicates(second_amendment_scope__individual_right_reading, individual_natural_rights_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_scope__individual_right_reading, self_defense_as_preexisting_right).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold a constitutionally protected right to keep and bear arms for self-defense in the home and, increasingly, in public, without needing to demonstrate militia affiliation or enrollment. This reading removes the need to justify ownership by reference to any collective or civic function; the individual's interest in self-defense is treated as the constitutional core.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, individual_gun_owners, beneficiary,
    moderate, biographical, mobile, national).

% Manufactures and sells firearms into a market whose legal floor is set by this reading: any regulation touching common civilian firearms must survive heightened scrutiny keyed to an individual constitutional right, which narrows the space for restriction and expands the addressable market relative to a militia-conditioned reading. The industry also benefits from statutory liability shields whose political durability depends partly on this constitutional framing.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, firearms_industry, beneficiary,
    organized, generational, arbitrage, national).

% Litigate, lobby, and fund the doctrinal architecture that sustains this reading, including test cases designed to extend it (public carry, weapon types, mental-health adjudications). They administer and defend the interpretive apparatus in courts, legislatures, and public discourse, and their institutional relevance is tied to the reading's continued dominance.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, gun_rights_advocacy_organizations, agenda_setter,
    organized, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_scope__individual_right_reading, gun_rights_advocacy_organizations, beneficiary).

% Bear the direct costs of firearm injury and death in contexts where this reading has constrained the regulatory tools (waiting periods, magazine limits, carry restrictions, red-flag laws) that legislatures might otherwise deploy. They have no litigation standing comparable to organized advocacy groups and cannot exit the jurisdictions where firearm proliferation is highest.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, gun_violence_victims, payer,
    powerless, immediate, trapped, local).

% City and state governments that wish to regulate firearm possession, carry, or transfer to address local violence rates find their statutes struck down or chilled by heightened-scrutiny doctrine flowing from this reading. Their exit is constrained: they can attempt narrower regulations designed to survive the doctrinal test, but cannot exit the constitutional framework itself.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, municipalities_seeking_regulation, payer,
    institutional, generational, constrained, regional).

% Disproportionately Black, Latino, and low-income urban communities experiencing elevated rates of firearm homicide and suicide, whose local governments' regulatory tools are constrained by the same doctrine nationally. Geographic and economic mobility is limited; residents cannot simply relocate away from the resulting risk.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, communities_with_high_firearm_mortality, payer,
    powerless, generational, trapped, local).

% The judicial coalition that authored and extends this reading (Heller, McDonald, Bruen) sets and enforces the doctrinal test that lower courts and legislatures must apply. It administers the interpretive framework and can, in principle, revise or abandon it, but has instead entrenched it through successive rulings raising the scrutiny bar.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, supreme_court_majority_coalition, agenda_setter,
    institutional, civilizational, analytical, national).

% Study the ratification-era historical record, contemporaneous militia statutes, and founding-era firearms regulation to assess whether the individual-right reading reflects original public meaning or a twentieth-century doctrinal innovation. Their scholarship is cited by all sides but does not itself adjudicate the constitutional question.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, legal_historians_and_originalist_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_scope__individual_right_reading, diffuse).
narrative_ontology:fixing_cost_class(second_amendment_scope__individual_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, judicially enforceable rule that individual self-defense with firearms is a preexisting right the state may not extinguish, giving individuals predictable legal footing against total prohibition and giving firearms commerce a durable floor of legal certainty.
% TRANSFER_FUNCTION: Moves regulatory latitude away from municipal and state legislatures (and, derivatively, away from communities bearing firearm violence) toward individual owners and the firearms industry, by raising the constitutional bar any restriction must clear.
% ABSENT_VOICES: Gun violence victims and residents of high-mortality communities are rarely represented as parties in the litigation that defines and extends this reading — the doctrine develops through cases brought by owners and advocacy organizations challenging restrictions, not through cases brought by those harmed by firearm proliferation seeking to defend them.
% DISAPPEARANCE_RATIONALE: If this reading were abandoned overnight in favor of a militia-conditioned or collective reading, the constitutional floor under state and local firearms regulation would drop dramatically: waiting periods, carry restrictions, magazine limits, and licensing regimes currently vulnerable to strict-scrutiny challenge would face a much lower bar, and legislatures would regain latitude currently foreclosed by Heller/Bruen doctrine. Litigation strategy for gun rights organizations, firearms industry legal exposure, and millions of individual owners' legal status would all restructure.
% FOUNDING_PROBLEM: The individual-right reading was advanced to resolve a genuine ambiguity in the Second Amendment's text — the relationship between the militia preamble and the operative right-to-bear-arms clause — and to establish that the amendment protects a personal right of self-defense against both federal and state disarmament, not merely a collective military-organizational entitlement.
% FOUNDING_PROBLEM_CORROBORATION: Gun rights organizations and the Heller/McDonald/Bruen majority attest the individual right was always the amendment's core meaning, temporarily obscured by twentieth-century collective-right doctrine. Independent legal historians outside both advocacy camps (including some originalist scholars who disagree with each other on outcome) attest the historical record is genuinely mixed — founding-era militia statutes, contemporaneous state constitutions, and ratification debates support competing readings — and that the individual-right reading's dominance since 2008 reflects a successful litigation and appointments strategy at least as much as settled historical consensus.
narrative_ontology:disappearance_verdict(second_amendment_scope__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_scope__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__individual_right_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_scope__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_scope__individual_right_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_scope__individual_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_scope__individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_scope__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate-high (0.58 at interval end) because the reading's coverage is broad (all individuals, not a militia-linked subset) and its enforcement mechanism (heightened/strict scrutiny) actively removes regulatory tools from jurisdictions bearing the costs of firearm proliferation — a genuine transfer of decision-making authority away from communities toward owners and industry. Suppression is moderate (0.52): the doctrine does not physically coerce compliance so much as foreclose legislative options through judicial invalidation, a structurally real but less visceral suppression mechanism than direct coercion. Theater ratio is low (0.22): the doctrinal apparatus (originalism, historical-analogue tests) does substantive interpretive work even if contested, rather than functioning as pure performance. Accessibility collapse is moderate-high (0.62): once the doctrine consolidates, alternative regulatory approaches face a genuinely narrowed path, though not fully closed — legislatures can still attempt narrowly tailored measures. Resistance is high (0.7): this reading faces sustained, organized opposition from public health researchers, victim advocacy groups, and a substantial share of state and municipal governments, reflecting genuine contestation rather than settled consensus.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (the Supreme Court coalition and advocacy organizations that built and maintain the doctrine), this reading appears as the correction of a historical judicial error — restoring an individual right always present in the text. From the payer seats (municipalities, high-mortality communities, violence victims), the same structure appears as an enforced transfer of policy authority away from democratically accountable regulators toward a judicially insulated rule that happens to track the preferences of a well-organized beneficiary coalition. The engine computes these divergent seat classifications from the structural power/exit data; the claimed type (tangled_rope) reflects the coordination function (legal certainty for owners and industry) coexisting with real asymmetric extraction (removed regulatory capacity imposed on non-consenting jurisdictions).
 *
 * DIRECTIONALITY LOGIC:
 *   Individual gun owners, the firearms industry, and gun rights advocacy organizations are declared beneficiaries: the reading either directly protects their conduct or expands their market/litigation position, and their exit options (mobile, arbitrage) reflect low structural dependency on any single jurisdiction's regulatory posture. Gun violence victims and residents of high-mortality communities are declared victims with trapped exit options — they cannot relocate away from firearm-related risk, and the doctrine constrains the exact tools (carry limits, licensing, magazine caps) that might reduce that risk in their specific localities. Municipalities are institutional payers with constrained (not trapped) exit — they retain some latitude to craft narrower regulations but cannot exit the constitutional framework itself. The Supreme Court coalition and advocacy organizations share the agenda-setter role because both administer and could, in principle, alter the doctrine, though only the Court can formally do so.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — ambiguity about whether the Second Amendment protects an individual or collective right — was genuinely unresolved by text alone, giving the individual-right reading a live doctrinal function (resolving real interpretive ambiguity) rather than pure invented pretext. But the founding_problem_status is authored as contested rather than dead, because the underlying dispute (how much regulatory latitude the amendment leaves) remains an open, actively litigated question, not a mandate that has quietly outlived its purpose. This prevents mislabeling the reading as either pure coordination (ignoring the real transfer of regulatory power away from non-consenting communities) or pure extraction (ignoring the genuine textual ambiguity the doctrine was built to resolve).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    originalist_historical_record_ambiguity,
    'Does the founding-era historical record (militia statutes, state constitutional analogues, ratification debates) actually support an individual right unconnected to militia service, or does it support a civic/militia-conditioned right that Heller reinterpreted?',
    'Comprehensive historical review by scholars without stake in current gun-policy outcomes, cross-checked against contemporaneous state constitutional provisions and militia statutes from the founding and early republic periods.',
    'If the historical record does not support the unconditioned individual-right reading, this constraint''s claimed_type and its status as the dominant doctrinal reading would rest on a constructed rather than discovered constitutional meaning — strengthening the case that observed ε reflects successful doctrinal construction rather than textual necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalist_historical_record_ambiguity, empirical, 'Whether the individual-right reading is textually/historically grounded or a doctrinal innovation.').

omega_variable(
    kernel_reading_selection_mechanism,
    'Given that the Second Amendment''s text is genuinely open to at least three structurally distinct readings (individual, collective, civic), what explains why the individual-right reading became dominant — genuine interpretive persuasion, or the organizational and litigation capacity of the beneficiary coalition (advocacy organizations, firearms industry) relative to the diffuse and less-organized victim class?',
    'Comparative study of litigation funding, amicus participation, and judicial appointment patterns across the decades preceding and following Heller, weighed against parallel doctrinal shifts unconnected to organized advocacy.',
    'If reading dominance tracks organizational capacity more than interpretive merit, this supports classifying the reading''s persistence as tangled_rope (real coordination function riding on organized capture of the interpretive apparatus) rather than a pure mountain-like discovery of settled constitutional meaning.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_mechanism, conceptual, 'Whether doctrinal dominance among competing kernel readings reflects merit or organized capacity asymmetry.').

omega_variable(
    public_safety_tradeoff_measurement,
    'To what extent does the doctrinal ceiling this reading imposes on firearms regulation cause measurable increases in firearm mortality relative to a counterfactual regime under the civic or collective reading?',
    'Comparative empirical studies across U.S. states and, where available, cross-national comparisons with jurisdictions retaining broader regulatory latitude, controlling for confounds in enforcement and socioeconomic conditions.',
    'Strong causal evidence of increased mortality attributable to doctrinally foreclosed regulations would sharpen the victim classification and could raise the authored extractiveness value in future revisions; weak or null findings would narrow the gap between this reading and its siblings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(public_safety_tradeoff_measurement, empirical, 'Whether the doctrine''s regulatory ceiling has measurable public-safety costs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__individual_right_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_scope__individual_right_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(seco_tr_t4, second_amendment_scope__individual_right_reading, theater_ratio, 4, 0.14).
narrative_ontology:measurement(seco_tr_t8, second_amendment_scope__individual_right_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement(seco_tr_t12, second_amendment_scope__individual_right_reading, theater_ratio, 12, 0.18).
narrative_ontology:measurement(seco_tr_t16, second_amendment_scope__individual_right_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(seco_tr_t20, second_amendment_scope__individual_right_reading, theater_ratio, 20, 0.22).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_scope__individual_right_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(seco_be_t4, second_amendment_scope__individual_right_reading, base_extractiveness, 4, 0.4).
narrative_ontology:measurement(seco_be_t8, second_amendment_scope__individual_right_reading, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(seco_be_t12, second_amendment_scope__individual_right_reading, base_extractiveness, 12, 0.52).
narrative_ontology:measurement(seco_be_t16, second_amendment_scope__individual_right_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(seco_be_t20, second_amendment_scope__individual_right_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_scope__individual_right_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(seco_su_t4, second_amendment_scope__individual_right_reading, suppression_requirement, 4, 0.35).
narrative_ontology:measurement(seco_su_t8, second_amendment_scope__individual_right_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(seco_su_t12, second_amendment_scope__individual_right_reading, suppression_requirement, 12, 0.44).
narrative_ontology:measurement(seco_su_t16, second_amendment_scope__individual_right_reading, suppression_requirement, 16, 0.48).
narrative_ontology:measurement(seco_su_t20, second_amendment_scope__individual_right_reading, suppression_requirement, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_scope__individual_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_scope__individual_right_reading, 0.1).
narrative_ontology:affects_constraint(second_amendment_scope__individual_right_reading, collective_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__individual_right_reading, civic_right_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of a three-member kernel family (second_amendment_scope): individual_right_reading (this file), collective_right_reading, and civic_right_reading. All three readings share the same textual kernel (the Second Amendment's text) but instantiate structurally distinct constraints with different beneficiary/victim sets and different ε values per the ε-invariance principle. individual_right_reading carries the highest ε among the three (broadest coverage, strictest scrutiny, most constrained state authority); collective_right_reading would carry the lowest ε for individual owners (no individual claim recognized) but potentially higher ε from a state-power-concentration angle; civic_right_reading sits between. Each file must be read independently; do not merge or average their classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
