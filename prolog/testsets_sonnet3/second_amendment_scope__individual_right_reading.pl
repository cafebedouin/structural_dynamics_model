% ============================================================================
% CONSTRAINT STORY: second_amendment_scope__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   constraint_id: second_amendment_scope__individual_right_reading
 *   human_readable: Second Amendment as Individual Right Unconnected to Militia Service (Heller/Bruen Reading)
 *   domain: constitutional_law/political_theory/rights_jurisprudence
 *
 * SUMMARY:
 *   This story instantiates the individual-right reading of the Second
 *   Amendment kernel, as crystallized doctrinally in District of Columbia v.
 *   Heller (2008) and extended in New York State Rifle & Pistol Association
 *   v. Bruen (2022). Under this reading, the right to keep and bear arms
 *   belongs to individuals for private purposes (centrally self-defense)
 *   wholly unconnected to service in an organized militia, and state/local
 *   regulation must survive a demanding historical-tradition test. This is a
 *   distinct constraint from the collective_right_reading (which holds the
 *   amendment protects only state authority to organize militias, with no
 *   judicially enforceable individual entitlement) and the
 *   civic_right_reading (which holds an individual right exists but is
 *   conditioned on militia-eligibility or civic-participation framing). The
 *   three readings have different beneficiary/victim structures and different
 *   epsilon values; they are linked here as siblings in the same kernel
 *   rather than folded into one story.
 *
 * KEY AGENTS:
 *   - firearms_owners: beneficiary (organized/mobile) — gains expanded protected entitlement
 *   - gun_rights_advocacy_organizations: agenda_setter/beneficiary (institutional/arbitrage) — funds and directs doctrinal expansion
 *   - state_and_local_governments: payer (institutional/constrained) — loses regulatory latitude
 *   - gun_violence_victims: payer (powerless/trapped) — bears diffuse harm with no doctrinal voice
 *   - constitutional_law_scholars: observer (analytical) — contests the historical record underlying the reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__individual_right_reading, 0.62).
domain_priors:suppression_score(second_amendment_scope__individual_right_reading, 0.58).
domain_priors:theater_ratio(second_amendment_scope__individual_right_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_scope__individual_right_reading, "Second Amendment as Individual Right Unconnected to Militia Service (Heller/Bruen Reading)").
narrative_ontology:topic_domain(second_amendment_scope__individual_right_reading, "constitutional_law/political_theory/rights_jurisprudence").

domain_priors:requires_active_enforcement(second_amendment_scope__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__individual_right_reading, 'b7c88f50-a03a-41f3-9a33-245d16207d69').
narrative_ontology:cs_kernel_codification('b7c88f50-a03a-41f3-9a33-245d16207d69', fixed_text).
narrative_ontology:cs_authority_grounding('b7c88f50-a03a-41f3-9a33-245d16207d69', lineage).
narrative_ontology:cs_interpretation_layer_present('b7c88f50-a03a-41f3-9a33-245d16207d69').
narrative_ontology:cs_reading_relation('b7c88f50-a03a-41f3-9a33-245d16207d69', second_amendment_scope__collective_right_reading, forecloses).
narrative_ontology:cs_reading_relation('b7c88f50-a03a-41f3-9a33-245d16207d69', second_amendment_scope__civic_right_reading, coexists_with).
narrative_ontology:cs_axiom('b7c88f50-a03a-41f3-9a33-245d16207d69', foundational, individual_self_defense_as_core_right).
narrative_ontology:cs_axiom_status(individual_self_defense_as_core_right, holdable).
narrative_ontology:cs_axiom_grounding('b7c88f50-a03a-41f3-9a33-245d16207d69', individual_self_defense_as_core_right, deontological).
narrative_ontology:cs_axiom('b7c88f50-a03a-41f3-9a33-245d16207d69', foundational, prefatory_clause_non_limiting_on_operative_clause).
narrative_ontology:cs_axiom_status(prefatory_clause_non_limiting_on_operative_clause, holdable).
narrative_ontology:cs_axiom_grounding('b7c88f50-a03a-41f3-9a33-245d16207d69', prefatory_clause_non_limiting_on_operative_clause, conventional).
narrative_ontology:cs_reference_frame('b7c88f50-a03a-41f3-9a33-245d16207d69', founding_era_militia_preservation_context).
narrative_ontology:cs_drift_state('b7c88f50-a03a-41f3-9a33-245d16207d69', post_heller_bruen_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('b7c88f50-a03a-41f3-9a33-245d16207d69', '').
narrative_ontology:cs_kernel_id(second_amendment_scope__individual_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, firearms_owners).
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, firearms_manufacturers).
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, firearms_retailers).
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, gun_rights_advocacy_organizations).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, gun_violence_victims).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, urban_gun_regulation_advocates).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, state_and_local_governments).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, domestic_violence_survivors).
narrative_ontology:constraint_vindicates(second_amendment_scope__individual_right_reading, originalist_textual_method).
narrative_ontology:constraint_vindicates(second_amendment_scope__individual_right_reading, individual_natural_right_to_self_defense).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold a constitutionally protected right to keep and carry firearms for self-defense, hunting, and other lawful purposes, independent of any militia affiliation. Gain expanded ability to challenge licensing regimes, carry restrictions, and weapon-type bans under strict/heightened scrutiny standards established by this reading.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, firearms_owners, beneficiary,
    organized, biographical, mobile, national).

% Benefit commercially from an expanded, constitutionally insulated consumer market. Litigate aggressively to strike down state regulations on manufacture, sale, and design, using the individual-right holding as the doctrinal lever; largely immune from liability regimes that would otherwise constrain the industry.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, firearms_manufacturers, beneficiary,
    powerful, generational, arbitrage, national).

% Fund litigation strategy, cultivate originalist scholarship, and select test cases to expand and entrench the individual-right doctrine. Administer the ongoing legal and political apparatus that maintains and extends this reading through courts, legislatures, and public messaging.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, gun_rights_advocacy_organizations, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_scope__individual_right_reading, gun_rights_advocacy_organizations, beneficiary).

% Bear the human cost of expanded firearms availability and constrained regulatory capacity, including mass shootings, homicide, and accidental deaths. Have no direct voice in the doctrinal contest; harms are diffuse, individualized, and occur after the constitutional structure is already fixed.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, gun_violence_victims, payer,
    powerless, immediate, trapped, local).

% Face elevated lethality risk when abusive partners have firearms access; state and federal efforts to restrict firearms possession by domestic abusers must navigate the individual-right framework, and litigation under this reading has been used to challenge such restrictions directly.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, domestic_violence_survivors, payer,
    powerless, immediate, trapped, local).

% Lose substantial regulatory latitude to craft firearms policy responsive to local conditions (urban density, crime patterns, public health data). Must defend any regulation against a demanding historical-analogue test, and adverse rulings preempt or invalidate legislative choices made through ordinary democratic process.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, state_and_local_governments, payer,
    institutional, generational, constrained, regional).

% Argue that concentrated urban gun violence justifies context-sensitive regulation the individual-right doctrine forecloses. Their policy preferences are structurally disfavored by a rights framework indifferent to geographic variation in risk, and they have no forum within the doctrinal contest itself to register that disfavor.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, urban_gun_regulation_advocates, excluded,
    moderate, biographical, constrained, national).

% Analyze the historical, textual, and doctrinal evidence for competing readings; disagree sharply among themselves about whether the individual-right interpretation reflects genuine original public meaning or motivated reasoning layered onto a text whose operative clause foregrounds militia service.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, constitutional_law_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_scope__individual_right_reading, diffuse).
narrative_ontology:fixing_cost_class(second_amendment_scope__individual_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, judicially enforceable baseline entitlement that lets individuals plan around a predictable right to acquire and possess firearms without case-by-case political relitigation in every jurisdiction, and gives firearms markets nationwide regulatory predictability.
% TRANSFER_FUNCTION: Moves regulatory authority from state and local legislatures (accountable to local majorities and responsive to local harm data) to federal courts applying a historical-analogue test, and moves risk from firearms owners/industry (who gain insulation from regulation) to potential victims of firearms violence and to jurisdictions with acute gun-violence problems who cannot tailor responses.
% ABSENT_VOICES: Gun violence victims, their families, and urban communities bearing disproportionate harm have no seat in the doctrinal contest itself — the reading is settled through appellate litigation and originalist historical argument, not through processes where lived harm is directly weighed. Domestic violence survivors' safety interests are represented, if at all, only derivatively through prosecutors and advocacy amici, not as parties.
% DISAPPEARANCE_RATIONALE: If this reading were abandoned overnight in favor of a civic or collective reading, thousands of state and local firearms regulations currently vulnerable to constitutional challenge would become far easier to enact and defend, litigation funded by gun-rights organizations would lose its primary doctrinal lever, and firearms manufacturers would face materially higher regulatory exposure — the entire post-Heller regulatory landscape would reorganize around restored legislative discretion.
% FOUNDING_PROBLEM: The Second Amendment's text was drafted amid concern that a standing federal army could disarm state militias and concentrate coercive power in a distant central government; the amendment aimed to preserve state militia capacity and the concomitant expectation that militia-eligible citizens could keep arms.
% FOUNDING_PROBLEM_CORROBORATION: Historians and originalist scholars outside the gun-rights advocacy apparatus (e.g. legal historians examining founding-era militia statutes and drafting history) are sharply divided: some corroborate that an individual self-defense right existed independent of militia service in founding-era understanding; others attest, using the same founding-era record, that the operative purpose was militia preservation and that the individual-right reading is a late-twentieth-century doctrinal innovation. No consensus corroboration exists from historians who are not themselves parties to the contemporary political dispute.
narrative_ontology:disappearance_verdict(second_amendment_scope__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_scope__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__individual_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_scope__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_scope__individual_right_reading, 0.62, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored mod-high (0.62) reflecting the story's own account: this reading systematically transfers regulatory capacity away from democratically accountable state/local bodies toward a narrow judicially-enforced entitlement, and does so at broad national scope covering all individuals rather than a militia-connected subset — exactly the 'broad coverage plus strict scrutiny' delta specified for this reading. Suppression (0.58) reflects the active judicial enforcement machinery (historical-analogue test, aggressive circuit-splitting litigation) required to keep state regulatory experimentation foreclosed. Theater ratio is comparatively low (0.28) because the doctrinal function — protecting an asserted individual entitlement — is genuinely operative, not merely performative, even though its coordination benefit is contested. Accessibility collapse is moderate (0.5): unlike a mountain, meaningful regulatory alternatives (background checks short of categorical bans, some licensing regimes) persist post-Bruen, they are simply harder to sustain. Resistance is high (0.72): this reading faces sustained, organized opposition from public-health researchers, victim advocacy groups, and a substantial share of state legislatures, unlike a settled natural-law-style consensus.
 *
 * DIRECTIONALITY LOGIC:
 *   Firearms owners, manufacturers, and advocacy organizations sit near the beneficiary end: the reading directly enlarges their protected zone of action and insulates them from regulatory exposure. Gun violence victims, domestic violence survivors, and state/local governments sit near the target end: they bear the transferred cost (foreclosed regulatory options, elevated risk) without commensurate compensating benefit, and their exit options are trapped or constrained respectively — victims cannot opt out of risk exposure, and governments cannot exit the constitutional framework that binds them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (federal army disarming state militias) is largely dead in its literal form — no standing federal army currently threatens to supersede state militias in the 1791 sense, and the National Guard has substantially displaced the militia concept functionally. Yet under this reading the arrangement persists and has expanded rather than atrophied, now serving an individual self-defense/consumer-market function that bears little structural relationship to the founding militia-preservation problem. This mismatch (dead founding problem, expanding rather than receding arrangement) is exactly the signal the R5 corroboration question is designed to surface: the doctrine's own advocates assert continuity with founding purpose while independent historians dispute that continuity, and the disappearance_verdict of world_rearranges alongside a contested founding_problem_status indicates a live capture-pattern candidate for downstream analysis rather than a settled genealogy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    originalist_historical_record_ambiguity,
    'Does the founding-era historical record actually support an individual self-defense right severable from militia service, or does the individual-right reading impose a late-20th-century political framework onto an ambiguous or contrary historical record?',
    'Comprehensive historian consensus (outside litigation-funded scholarship) on founding-era militia statutes, state constitutional analogues, and drafting history of the Second Amendment''s operative and prefatory clauses.',
    'If the record clearly supports the individual-right reading, this constraint''s coordination function is more genuine and less politically constructed; if the record is ambiguous or contrary, the reading looks more like motivated doctrinal construction serving identifiable beneficiaries (firearms industry, advocacy organizations) dressed as originalist textualism — supporting a higher extractiveness assessment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(originalist_historical_record_ambiguity, conceptual, 'Whether founding-era historical evidence genuinely supports the individual-right reading or is contested/constructed.').

omega_variable(
    kernel_reading_selection_mechanism,
    'Why did the individual-right reading, among three live readings of the same constitutional kernel, become the controlling doctrine via Heller/Bruen rather than the civic or collective readings?',
    'Political science and legal history analysis of the decades-long litigation and advocacy strategy (NRA-funded scholarship pipeline, judicial appointment patterns) that preceded Heller, compared against the merits-only account offered by originalist jurisprudence.',
    'If the reading''s ascendance is substantially explained by sustained, well-funded institutional strategy rather than superior historical/textual argument, this supports classifying the reading as tangled_rope (coordination function for firearms owners riding on extraction from disfavored regulatory constituencies) rather than a neutral discovery of pre-existing constitutional meaning.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_mechanism, empirical, 'Whether doctrinal ascendance reflects institutional strategy versus interpretive merit.').

omega_variable(
    strict_scrutiny_coverage_breadth,
    'Is the ''all individuals'' beneficiary scope of this reading itself contestable — does it in practice extend uniformly, or does enforcement in practice differentiate by demographic/geographic factors (e.g., historically disparate enforcement of concealed-carry and licensing law against different populations)?',
    'Empirical study of post-Bruen enforcement patterns and permitting/prosecution data disaggregated by race, geography, and criminal history exemptions (e.g., felon-in-possession carve-outs).',
    'If the formally universal beneficiary class masks differentiated real-world benefit distribution, the true beneficiary set is narrower than ''all individuals,'' which would refine (likely raise) the extractiveness assessment for the excluded sub-populations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strict_scrutiny_coverage_breadth, empirical, 'Whether the formally universal individual-right beneficiary class is uniform in practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__individual_right_reading, 1791, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1791, second_amendment_scope__individual_right_reading, theater_ratio, 1791, 0.1).
narrative_ontology:measurement_basis(seco_tr_t1791, observed).
narrative_ontology:measurement(seco_tr_t1900, second_amendment_scope__individual_right_reading, theater_ratio, 1900, 0.12).
narrative_ontology:measurement_basis(seco_tr_t1900, observed).
narrative_ontology:measurement(seco_tr_t1968, second_amendment_scope__individual_right_reading, theater_ratio, 1968, 0.15).
narrative_ontology:measurement_basis(seco_tr_t1968, observed).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_scope__individual_right_reading, theater_ratio, 2008, 0.22).
narrative_ontology:measurement_basis(seco_tr_t2008, observed).
narrative_ontology:measurement(seco_tr_t2016, second_amendment_scope__individual_right_reading, theater_ratio, 2016, 0.25).
narrative_ontology:measurement_basis(seco_tr_t2016, observed).
narrative_ontology:measurement(seco_tr_t2022, second_amendment_scope__individual_right_reading, theater_ratio, 2022, 0.27).
narrative_ontology:measurement_basis(seco_tr_t2022, observed).
narrative_ontology:measurement(seco_tr_t2024, second_amendment_scope__individual_right_reading, theater_ratio, 2024, 0.28).
narrative_ontology:measurement_basis(seco_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(seco_be_t1791, second_amendment_scope__individual_right_reading, base_extractiveness, 1791, 0.15).
narrative_ontology:measurement_basis(seco_be_t1791, observed).
narrative_ontology:measurement(seco_be_t1900, second_amendment_scope__individual_right_reading, base_extractiveness, 1900, 0.2).
narrative_ontology:measurement_basis(seco_be_t1900, observed).
narrative_ontology:measurement(seco_be_t1968, second_amendment_scope__individual_right_reading, base_extractiveness, 1968, 0.25).
narrative_ontology:measurement_basis(seco_be_t1968, observed).
narrative_ontology:measurement(seco_be_t2008, second_amendment_scope__individual_right_reading, base_extractiveness, 2008, 0.48).
narrative_ontology:measurement_basis(seco_be_t2008, observed).
narrative_ontology:measurement(seco_be_t2016, second_amendment_scope__individual_right_reading, base_extractiveness, 2016, 0.55).
narrative_ontology:measurement_basis(seco_be_t2016, observed).
narrative_ontology:measurement(seco_be_t2022, second_amendment_scope__individual_right_reading, base_extractiveness, 2022, 0.6).
narrative_ontology:measurement_basis(seco_be_t2022, observed).
narrative_ontology:measurement(seco_be_t2024, second_amendment_scope__individual_right_reading, base_extractiveness, 2024, 0.62).
narrative_ontology:measurement_basis(seco_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1791, second_amendment_scope__individual_right_reading, suppression_requirement, 1791, 0.2).
narrative_ontology:measurement_basis(seco_su_t1791, observed).
narrative_ontology:measurement(seco_su_t1900, second_amendment_scope__individual_right_reading, suppression_requirement, 1900, 0.22).
narrative_ontology:measurement_basis(seco_su_t1900, observed).
narrative_ontology:measurement(seco_su_t1968, second_amendment_scope__individual_right_reading, suppression_requirement, 1968, 0.28).
narrative_ontology:measurement_basis(seco_su_t1968, observed).
narrative_ontology:measurement(seco_su_t2008, second_amendment_scope__individual_right_reading, suppression_requirement, 2008, 0.45).
narrative_ontology:measurement_basis(seco_su_t2008, observed).
narrative_ontology:measurement(seco_su_t2016, second_amendment_scope__individual_right_reading, suppression_requirement, 2016, 0.5).
narrative_ontology:measurement_basis(seco_su_t2016, observed).
narrative_ontology:measurement(seco_su_t2022, second_amendment_scope__individual_right_reading, suppression_requirement, 2022, 0.56).
narrative_ontology:measurement_basis(seco_su_t2022, observed).
narrative_ontology:measurement(seco_su_t2024, second_amendment_scope__individual_right_reading, suppression_requirement, 2024, 0.58).
narrative_ontology:measurement_basis(seco_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_scope__individual_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_scope__individual_right_reading, 0.1).
narrative_ontology:affects_constraint(second_amendment_scope__individual_right_reading, collective_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__individual_right_reading, civic_right_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the second_amendment_scope kernel. collective_right_reading holds the amendment protects only state militia-organizing authority (near-mountain epsilon for the underlying text, minimal individual beneficiary set); civic_right_reading occupies a middle position conditioning the individual right on militia-eligibility framing (moderate epsilon, narrower beneficiary set than this reading). This individual_right_reading carries the highest epsilon of the three because it extends the beneficiary class to all individuals nationwide and imposes the most demanding constraint on state regulatory authority (strict/heightened scrutiny via historical-analogue test). Each reading is authored as its own ε-invariant constraint per DP-001; do not average or reconcile epsilon across the three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
