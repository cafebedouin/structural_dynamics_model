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
 *   constraint_id: second_amendment_boundary__individual_right_reading
 *   human_readable: Second Amendment as Individual Right (Heller/Bruen Reading)
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This story authors the individual-right reading of the Second Amendment's
 *   kernel text: the prefatory militia clause is read as announcing a purpose
 *   without limiting the operative clause, which is treated as codifying a
 *   pre-existing individual right to keep and bear arms for self-defense,
 *   unconnected to militia service. This reading was substantially
 *   concretized by District of Columbia v. Heller (2008) and further hardened
 *   by New York State Rifle & Pistol Association v. Bruen (2022), which
 *   imposed a text-history-tradition test requiring regulations to find close
 *   historical analogues. This is ONE of three structurally distinct readings
 *   of the same constitutional kernel; the militia-conditioned reading and
 *   the insurrectionist reading are separate constraint stories with their
 *   own epsilon values, beneficiary/victim sets, and classifications — they
 *   are not alternative measurements of this constraint but different
 *   constraints entirely, linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - individual_gun_owners: Primary beneficiary (moderate/mobile) — holds the protected entitlement
 *   - firearms_manufacturers: Institutional beneficiary (organized/arbitrage) — market insulated from regulatory contraction
 *   - gun_rights_advocacy_organizations: Agenda-setter (organized/arbitrage) — litigates and administers the doctrine's expansion
 *   - mass_shooting_victims, domestic_violence_victims, firearm_suicide_completers: Primary targets (powerless/trapped) — bear harm the doctrine's regulatory ceiling leaves unaddressed
 *   - federal_judiciary: Agenda-setter/observer (institutional/analytical) — adjudicates and concretizes doctrinal scope
 *   - public_health_researchers: Excluded voice (moderate/constrained) — evidence discounted by the historical-analogy methodology
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
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_boundary__individual_right_reading, "Second Amendment as Individual Right (Heller/Bruen Reading)").
narrative_ontology:topic_domain(second_amendment_boundary__individual_right_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_boundary__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__individual_right_reading, '4b6c0f33-664f-4100-bf7d-1e16c59f8638').
narrative_ontology:cs_kernel_codification('4b6c0f33-664f-4100-bf7d-1e16c59f8638', fixed_text).
narrative_ontology:cs_authority_grounding('4b6c0f33-664f-4100-bf7d-1e16c59f8638', lineage).
narrative_ontology:cs_interpretation_layer_present('4b6c0f33-664f-4100-bf7d-1e16c59f8638').
narrative_ontology:cs_reading_relation('4b6c0f33-664f-4100-bf7d-1e16c59f8638', second_amendment_boundary__militia_conditioned_reading, forecloses).
narrative_ontology:cs_reading_relation('4b6c0f33-664f-4100-bf7d-1e16c59f8638', second_amendment_boundary__insurrectionist_reading, coexists_with).
narrative_ontology:cs_axiom('4b6c0f33-664f-4100-bf7d-1e16c59f8638', foundational, operative_clause_self_sufficient).
narrative_ontology:cs_axiom_status(operative_clause_self_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('4b6c0f33-664f-4100-bf7d-1e16c59f8638', operative_clause_self_sufficient, conventional).
narrative_ontology:cs_axiom('4b6c0f33-664f-4100-bf7d-1e16c59f8638', foundational, prefatory_clause_non_limiting).
narrative_ontology:cs_axiom_status(prefatory_clause_non_limiting, holdable).
narrative_ontology:cs_axiom_grounding('4b6c0f33-664f-4100-bf7d-1e16c59f8638', prefatory_clause_non_limiting, conventional).
narrative_ontology:cs_axiom('4b6c0f33-664f-4100-bf7d-1e16c59f8638', secondary, self_defense_as_core_purpose).
narrative_ontology:cs_axiom_status(self_defense_as_core_purpose, holdable).
narrative_ontology:cs_axiom_grounding('4b6c0f33-664f-4100-bf7d-1e16c59f8638', self_defense_as_core_purpose, deontological).
narrative_ontology:cs_reference_frame('4b6c0f33-664f-4100-bf7d-1e16c59f8638', founding_era_individual_possession_right).
narrative_ontology:cs_drift_state('4b6c0f33-664f-4100-bf7d-1e16c59f8638', post_bruen_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4b6c0f33-664f-4100-bf7d-1e16c59f8638', '').
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
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, state_and_local_legislatures).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold a constitutionally protected entitlement to keep and carry firearms for self-defense, unconnected to militia service. Enjoy a strong presumption against most state and local regulation, and can challenge restrictive laws in court under a text-history-tradition standard that favors them.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, individual_gun_owners, beneficiary,
    moderate, biographical, mobile, national).

% Operate in a market where product-liability shields (PLCAA) and the constitutional reading combine to insulate the industry from many regulatory and litigation pressures that would otherwise shrink demand or impose safety-design mandates. Benefit directly from the reading's chilling effect on legislative appetite for restriction.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, firearms_manufacturers, beneficiary,
    organized, generational, arbitrage, national).

% Litigate, lobby, and fund the doctrinal architecture that produced and sustains this reading (case selection, amicus strategy, model legislation). Administer the reading's application through strategic litigation against new regulations and set the interpretive agenda that courts increasingly follow.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, gun_rights_advocacy_organizations, agenda_setter,
    organized, generational, arbitrage, national).

% Bear the direct harm of firearm access enabled by the reading's presumption against regulation. Have no structural mechanism to alter the constitutional baseline; their harm occurs after the interpretive framework has already foreclosed many preventive regulatory options.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, mass_shooting_victims, payer,
    powerless, immediate, trapped, local).

% Face elevated lethality risk when abusers retain firearm access; the reading's presumption against categorical restriction complicates and delays protective-order disarmament regimes in some jurisdictions. Individually cannot alter the doctrinal framework governing their abuser's access.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, domestic_violence_victims, payer,
    powerless, immediate, trapped, local).

% Represent the largest single category of firearm deaths; means-restriction research shows access reduction lowers completion rates, but the reading's framework disfavors the waiting-period and storage mandates that would most directly interrupt impulsive access. Cannot be represented in the interpretive process after the fact.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, firearm_suicide_completers, payer,
    powerless, immediate, trapped, local).

% Attempt to craft firearm regulation responsive to local violence patterns but must design around a text-history-tradition test that presumptively favors the individual-right reading, requiring analogical historical justification for any restriction. Regulatory latitude has narrowed as the doctrine has hardened.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, state_and_local_legislatures, payer,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(second_amendment_boundary__individual_right_reading, state_and_local_legislatures, excluded).

% Disproportionately urban, lower-income communities bear concentrated exposure to gun violence; the reading constrains the regulatory tools (carry restrictions, dealer licensing, high-capacity limits) most locally salient to their exposure, while offering no comparable protective benefit calibrated to their situation.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, communities_with_high_gun_violence_exposure, payer,
    powerless, generational, trapped, regional).

% Adjudicates the scope of the right using the text-history-tradition methodology this reading established; each ruling further concretizes the doctrine's reach and forecloses regulatory approaches lacking sufficiently analogous 18th- and 19th-century precedent.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_boundary__individual_right_reading, federal_judiciary, observer).

% Produce epidemiological evidence on firearm mortality and means-restriction efficacy, but this evidence carries little formal weight under a historical-analogy test that privileges founding-era regulatory tradition over contemporary empirical outcomes. Their findings inform public debate but not the doctrinal test itself.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, public_health_researchers, excluded,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_boundary__individual_right_reading, diffuse).
narrative_ontology:fixing_cost_class(second_amendment_boundary__individual_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, judicially enforceable baseline that lets individuals plan around a predictable entitlement to possess firearms, and gives firearms commerce and self-defense practice a settled legal footing free of case-by-case political renegotiation.
% TRANSFER_FUNCTION: Moves regulatory latitude away from legislatures and toward individual claimants and the firearms industry; moves risk of firearm-enabled harm away from those who would otherwise be restricted by tighter regulation and onto third parties exposed to the resulting access levels.
% ABSENT_VOICES: Mass shooting victims, domestic violence victims, and suicide-risk populations have no seat in the interpretive process that sets the doctrinal baseline; public health researchers' empirical findings on means-restriction are structurally discounted by a methodology privileging historical analogy over contemporary outcome data.
% DISAPPEARANCE_RATIONALE: If this reading were abandoned, state and local legislatures would regain broad latitude to enact licensing, capacity, storage, and carry restrictions without the burden of finding historical analogues; the firearms market's constitutional shield would narrow considerably; litigation strategy built around Heller/Bruen would need to be reconstructed around a different doctrinal test.
% FOUNDING_PROBLEM: Understood by its proponents as correcting a mid-20th-century judicial drift that had treated the Second Amendment as a purely collective, militia-contingent provision, thereby restoring what they view as the amendment's original meaning as an individual pre-existing right.
% FOUNDING_PROBLEM_CORROBORATION: Originalist legal scholars and gun-rights organizations attest the individual-right reading restores original meaning suppressed by mid-century collective-rights jurisprudence. Historians of the founding era and public health researchers outside the gun-rights coalition dispute the historical claim and attest that the reading's practical function is expanding market and possession protections beyond what founding-era regulatory practice supports; this dispute is unresolved and is itself part of the kernel contest.
narrative_ontology:disappearance_verdict(second_amendment_boundary__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_boundary__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_boundary__individual_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.62 by 2024) reflects the accumulating gap between the doctrine's protective scope and the regulatory tools foreclosed to address documented harms; it rose measurably after Bruen's historical-analogy test hardened the presumption against regulation. Suppression (0.58, rising from 0.35) captures the increasing structural difficulty legislatures face justifying any new restriction under the text-history-tradition framework — this is a genuine rise in doctrinal suppressive force, not merely extraction drift, which is why suppression_requirement is tracked separately. Theater ratio stays comparatively low (0.28) because the coordination function (a stable, litigable baseline for possession) is real and functioning, not merely performative — the doctrine does what it claims to do; the extraction is a byproduct of what it forecloses, not of hollow enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the individual gun owner's or manufacturer's seat, this reading is straightforwardly a rope: a stable, judicially guaranteed entitlement solving a genuine problem of political vulnerability to shifting legislative majorities. From the seat of a domestic violence victim or a legislature trying to enact a red-flag or storage law, the same doctrinal structure operates as an actively enforced ceiling on protective policy — the engine's per-seat computation should diverge sharply along these lines, which is the seat-divergence this classification is designed to surface.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual gun owners, manufacturers, and advocacy organizations are declared beneficiaries because the doctrine directly expands their protected domain and insulates their commercial and personal activity from regulatory contraction — low d, benefit-side. Mass shooting victims, domestic violence victims, and suicide completers are declared victims because they bear concentrated, often fatal costs from the access levels the doctrine protects, with no structural voice in the interpretive process and trapped exit (they cannot opt out of exposure to the ambient level of firearm availability their community experiences) — high d, target-side. State/local legislatures sit as constrained payers: institutionally powerful in the abstract but structurally constrained by a doctrinal test that narrows their available tools regardless of local preference.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding-problem status is authored as contested rather than dead or live, because the two sides of the reading dispute even whether the doctrine restored a suppressed original meaning or manufactured a new one. This resists collapsing the story into either 'pure vindicated coordination' (which would ignore the victim set) or 'pure extraction dressed as rights' (which would ignore that the coordination function — a stable possession entitlement — is genuinely operative, not theatrical). The tangled_rope classification requires both a real coordination function (individual possession stability) and asymmetric extraction (harm concentrated on non-consenting third parties) under active enforcement (ongoing litigation and judicial concretization) — all three are present here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    originalist_historical_accuracy,
    'Does the individual-right reading accurately recover founding-era public meaning, or does it selectively emphasize post-ratification commentary and 19th-century state constitutional provisions that diverged from the amendment''s original federal context?',
    'Continued historical scholarship on founding-era militia statutes, personal-arms regulation in the colonies and early republic, and the drafting history of the operative and prefatory clauses; resolution is unlikely to be clean given the adversarial nature of the underlying historiography.',
    'If the individual-right reading''s historical claim is substantially undermined, its legitimacy as ''restoring'' rather than ''constructing'' the right weakens considerably, strengthening the case that the doctrine is an extractive innovation dressed in originalist vocabulary rather than a corrective one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalist_historical_accuracy, empirical, 'Whether the individual-right reading''s historical premise withstands ongoing originalist historiography.').

omega_variable(
    kernel_framing_selection,
    'Is the constitutional text itself the correct unit of analysis, or should the kernel be framed as the interpretive methodology (text-history-tradition) that the individual-right reading has installed as the governing test, since that methodology now does most of the work of foreclosing regulatory alternatives regardless of the underlying text?',
    'Compare classification outcomes under a text-as-kernel framing versus a methodology-as-kernel framing; observe whether the methodology, once entrenched, produces extraction independent of the specific right at issue (as similar historical-analogue reasoning surfaces in other doctrinal areas).',
    'Under the text-framing (adopted here), this constraint is one reading of a contested clause. Under a methodology-framing, the story would center on the text-history-tradition test itself as the extractive mechanism, potentially reclassifying this constraint as downstream of a separate, more general judicial-methodology constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_selection, conceptual, 'Alternative framing: kernel as constitutional text vs. kernel as the interpretive methodology it has generated.').

omega_variable(
    coordination_extraction_severability,
    'Can the individual possession entitlement (the coordination function) be preserved while permitting the categories of regulation the doctrine currently forecloses (the extraction-enabling component), or are they structurally fused under the current test?',
    'Comparative analysis of jurisdictions and historical doctrinal frameworks (e.g., pre-Heller circuit-court frameworks, or post-Bruen legislative responses that survive judicial review) that preserve individual possession while permitting means-restriction, licensing, or storage regulation.',
    'If severable, the extraction is an artifact of how strictly the presumption against regulation is applied, not an inherent feature of individual-right recognition — supporting a narrower, less extractive version of this same reading. If fused under current doctrine, the tangled_rope classification is more firmly warranted as currently applied.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_severability, conceptual, 'Whether the doctrine''s coordination and extraction components can be structurally separated by a less absolutist application of the same reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__individual_right_reading, 2008, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t2008, second_amendment_boundary__individual_right_reading, theater_ratio, 2008, 0.18).
narrative_ontology:measurement(seco_tr_t2011, second_amendment_boundary__individual_right_reading, theater_ratio, 2011, 0.2).
narrative_ontology:measurement(seco_tr_t2014, second_amendment_boundary__individual_right_reading, theater_ratio, 2014, 0.22).
narrative_ontology:measurement(seco_tr_t2017, second_amendment_boundary__individual_right_reading, theater_ratio, 2017, 0.24).
narrative_ontology:measurement(seco_tr_t2020, second_amendment_boundary__individual_right_reading, theater_ratio, 2020, 0.26).
narrative_ontology:measurement(seco_tr_t2022, second_amendment_boundary__individual_right_reading, theater_ratio, 2022, 0.27).
narrative_ontology:measurement(seco_tr_t2024, second_amendment_boundary__individual_right_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(seco_be_t2008, second_amendment_boundary__individual_right_reading, base_extractiveness, 2008, 0.42).
narrative_ontology:measurement(seco_be_t2011, second_amendment_boundary__individual_right_reading, base_extractiveness, 2011, 0.47).
narrative_ontology:measurement(seco_be_t2014, second_amendment_boundary__individual_right_reading, base_extractiveness, 2014, 0.51).
narrative_ontology:measurement(seco_be_t2017, second_amendment_boundary__individual_right_reading, base_extractiveness, 2017, 0.55).
narrative_ontology:measurement(seco_be_t2020, second_amendment_boundary__individual_right_reading, base_extractiveness, 2020, 0.58).
narrative_ontology:measurement(seco_be_t2022, second_amendment_boundary__individual_right_reading, base_extractiveness, 2022, 0.6).
narrative_ontology:measurement(seco_be_t2024, second_amendment_boundary__individual_right_reading, base_extractiveness, 2024, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t2008, second_amendment_boundary__individual_right_reading, suppression_requirement, 2008, 0.35).
narrative_ontology:measurement(seco_su_t2011, second_amendment_boundary__individual_right_reading, suppression_requirement, 2011, 0.4).
narrative_ontology:measurement(seco_su_t2014, second_amendment_boundary__individual_right_reading, suppression_requirement, 2014, 0.44).
narrative_ontology:measurement(seco_su_t2017, second_amendment_boundary__individual_right_reading, suppression_requirement, 2017, 0.48).
narrative_ontology:measurement(seco_su_t2020, second_amendment_boundary__individual_right_reading, suppression_requirement, 2020, 0.52).
narrative_ontology:measurement(seco_su_t2022, second_amendment_boundary__individual_right_reading, suppression_requirement, 2022, 0.56).
narrative_ontology:measurement(seco_su_t2024, second_amendment_boundary__individual_right_reading, suppression_requirement, 2024, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_boundary__individual_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_boundary__individual_right_reading, 0.1).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, militia_conditioned_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, insurrectionist_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, firearms_industry_liability_shield).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, state_concealed_carry_licensing_regimes).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language 'Second Amendment meaning' concept, per the ε-invariance principle. militia_conditioned_reading treats the same text as licensing comprehensive regulation (dramatically lower extraction, near-mountain-adjacent from the regulatory-latitude seat); insurrectionist_reading treats individual possession as instrumental to resistance capacity against tyranny (different beneficiary logic centered on political theory rather than self-defense, and a different victim-weighting). All three readings share the fixed constitutional text as their kernel but instantiate structurally distinct constraints with different epsilon values, different beneficiary/victim sets, and different classifications. This story also links downstream to firearms_industry_liability_shield and state_concealed_carry_licensing_regimes, both of which are structurally influenced by which reading of the kernel currently governs judicial review.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
