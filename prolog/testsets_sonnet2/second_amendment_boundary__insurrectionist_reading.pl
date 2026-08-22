% ============================================================================
% CONSTRAINT STORY: second_amendment_boundary__insurrectionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_boundary__insurrectionist_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: second_amendment_boundary__insurrectionist_reading
 *   human_readable: Second Amendment as Anti-Tyranny Insurrection Guarantee
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This story instantiates the insurrectionist reading of the Second
 *   Amendment kernel: the claim that the right's core purpose is to preserve
 *   a citizenry capable of armed resistance against a tyrannical government,
 *   with individual firearm possession instrumental to that potential
 *   overthrow capacity. This reading gained significant litigation and
 *   advocacy traction from the 1970s onward (Gun Owners of America founding,
 *   Second Amendment Foundation, culminating in academic 'Standard Model'
 *   scholarship of the 1990s and post-Heller elaboration). Under this
 *   reading, the logical endpoint is that military-grade or military-pattern
 *   arms sit at the CORE of protection (a militia deterrent needs weapons
 *   comparable to what it would face), and any state effort to disarm or
 *   register is read as a tyranny precursor rather than ordinary
 *   public-safety regulation. This is a distinct constraint from the
 *   individual_right_reading (which grounds the right in personal
 *   self-defense and does not require military parity as its logical
 *   endpoint) and from the militia_conditioned_reading (which would treat the
 *   same disarmament measures as ordinary permissible regulation). Each
 *   reading has its own epsilon and its own stakeholder set; they are linked,
 *   not merged.
 *
 * KEY AGENTS:
 *   - armed_citizen_militia_movements: primary beneficiary and organizer, claims deterrent legitimacy
 *   - firearms_manufacturers_of_military_pattern_arms: commercial beneficiary, funds advocacy
 *   - insurrectionist_rights_advocacy_groups: agenda-setter, administers the reading's boundaries in courts and legislatures
 *   - state_security_apparatus_personnel: structural target, cast as presumptive tyranny object
 *   - civilians_in_hypothetical_armed_conflict_zones and mass_shooting_casualty_populations: diffuse victims bearing tail-risk and ordinary-risk costs
 *   - communities_experiencing_militia_intimidation: bear localized coercive costs from the reading's legitimation of armed public displays
 *   - constitutional_law_scholars: analytical observers contesting the reading's historical grounding
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_boundary__insurrectionist_reading, 0.58).
domain_priors:suppression_score(second_amendment_boundary__insurrectionist_reading, 0.34).
domain_priors:theater_ratio(second_amendment_boundary__insurrectionist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, suppression_requirement, 0.34).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__insurrectionist_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_boundary__insurrectionist_reading, "Second Amendment as Anti-Tyranny Insurrection Guarantee").
narrative_ontology:topic_domain(second_amendment_boundary__insurrectionist_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_boundary__insurrectionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__insurrectionist_reading, '32a706e3-da88-4eb5-af6b-85cbc11d112b').
narrative_ontology:cs_kernel_codification('32a706e3-da88-4eb5-af6b-85cbc11d112b', fixed_text).
narrative_ontology:cs_authority_grounding('32a706e3-da88-4eb5-af6b-85cbc11d112b', lineage).
narrative_ontology:cs_interpretation_layer_present('32a706e3-da88-4eb5-af6b-85cbc11d112b').
narrative_ontology:cs_reading_relation('32a706e3-da88-4eb5-af6b-85cbc11d112b', second_amendment_boundary__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('32a706e3-da88-4eb5-af6b-85cbc11d112b', second_amendment_boundary__militia_conditioned_reading, forecloses).
narrative_ontology:cs_axiom('32a706e3-da88-4eb5-af6b-85cbc11d112b', foundational, armed_resistance_capacity_is_the_rights_core_purpose).
narrative_ontology:cs_axiom_status(armed_resistance_capacity_is_the_rights_core_purpose, holdable).
narrative_ontology:cs_axiom_grounding('32a706e3-da88-4eb5-af6b-85cbc11d112b', armed_resistance_capacity_is_the_rights_core_purpose, deontological).
narrative_ontology:cs_axiom('32a706e3-da88-4eb5-af6b-85cbc11d112b', secondary, military_parity_arms_fall_within_protected_core).
narrative_ontology:cs_axiom_status(military_parity_arms_fall_within_protected_core, holdable).
narrative_ontology:cs_axiom_grounding('32a706e3-da88-4eb5-af6b-85cbc11d112b', military_parity_arms_fall_within_protected_core, instrumental).
narrative_ontology:cs_reference_frame('32a706e3-da88-4eb5-af6b-85cbc11d112b', founding_era_anti_standing_army_check).
narrative_ontology:cs_drift_state('32a706e3-da88-4eb5-af6b-85cbc11d112b', post_heller_contemporary, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('32a706e3-da88-4eb5-af6b-85cbc11d112b', '').
narrative_ontology:cs_kernel_id(second_amendment_boundary__insurrectionist_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__insurrectionist_reading, armed_citizen_militia_movements).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__insurrectionist_reading, firearms_manufacturers_of_military_pattern_arms).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__insurrectionist_reading, insurrectionist_rights_advocacy_groups).
narrative_ontology:constraint_victim(second_amendment_boundary__insurrectionist_reading, state_security_apparatus_personnel).
narrative_ontology:constraint_victim(second_amendment_boundary__insurrectionist_reading, civilians_in_hypothetical_armed_conflict_zones).
narrative_ontology:constraint_victim(second_amendment_boundary__insurrectionist_reading, mass_shooting_casualty_populations).
narrative_ontology:constraint_victim(second_amendment_boundary__insurrectionist_reading, communities_experiencing_militia_intimidation).
narrative_ontology:constraint_vindicates(second_amendment_boundary__insurrectionist_reading, popular_sovereignty_ultimate_check_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_boundary__insurrectionist_reading, tyranny_precursor_disarmament_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organize around the claim that individual and small-group possession of military-pattern arms is the last-resort check on federal overreach. They lobby against registration and confiscation measures, stockpile weapons framed as deterrent capacity, and treat any restriction as evidence the tyranny scenario is already underway. Their exit from the constraint is easy — they can relocate, arm further, or organize elsewhere — because the constraint as they read it protects rather than binds them.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, armed_citizen_militia_movements, beneficiary,
    organized, civilizational, mobile, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_boundary__insurrectionist_reading, armed_citizen_militia_movements, agenda_setter).

% Sell semi-automatic rifles and accessories marketed explicitly on anti-tyranny and deterrence themes. The insurrectionist reading, if it holds constitutional weight, forecloses an entire category of regulation that would otherwise shrink their market; they fund litigation and advocacy that advance this reading and can relocate production or sales channels to favorable jurisdictions if any single state tightens rules.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, firearms_manufacturers_of_military_pattern_arms, beneficiary,
    powerful, generational, arbitrage, national).

% Litigate and draft model legislation asserting that the right's core purpose is anti-government resistance, pushing courts and legislatures toward treating any effort to restrict military-pattern weapons or high-capacity magazines as a tyranny precursor. They administer the reading's boundaries — which arms count, which restrictions trigger the alarm — and benefit reputationally and financially from the reading's persistence.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, insurrectionist_rights_advocacy_groups, agenda_setter,
    organized, generational, arbitrage, national).

% Police officers, National Guard members, and federal law enforcement operate under a constitutional reading that casts their institutional function as the presumptive tyranny against which the citizenry retains armed deterrent capacity. They face civilian firepower parity or superiority in some jurisdictions and cannot exit the structural position the reading assigns them — their institutional role is fixed by employment, not choice.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, state_security_apparatus_personnel, payer,
    institutional, biographical, trapped, national).

% Bear the tail-risk cost of a constitutional reading whose logical endpoint is a citizenry armed for potential civil conflict — bystanders in any scenario where the deterrent capacity is actually exercised. They have no seat in setting the reading and no exit from the geography where it would be enacted.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, civilians_in_hypothetical_armed_conflict_zones, payer,
    powerless, generational, trapped, national).

% Bear the ordinary-time cost of a legal regime shaped by the insurrectionist reading's resistance to regulating military-pattern weapons, which are disproportionately implicated in mass casualty events. The connection between the constitutional theory and their harm is indirect but structural: the reading forecloses the regulatory tools that would most directly reduce this category of weapon in circulation.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, mass_shooting_casualty_populations, payer,
    powerless, immediate, trapped, national).

% Live near or interact with armed militia groups who cite the insurrectionist reading to justify open-carry shows of force at protests, government buildings, and public meetings. The reading's legitimation of armed deterrence as a constitutional value gives cover to intimidation that would otherwise be read simply as threat display.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, communities_experiencing_militia_intimidation, payer,
    powerless, biographical, constrained, local).

% Debate whether the insurrectionist reading is supported by founding-era text, structure, and history, or whether it is a modern reconstruction serving present-day political and commercial interests. They produce competing originalist and structuralist analyses that feed into litigation but do not themselves hold or exercise the right.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, constitutional_law_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_boundary__insurrectionist_reading, insurrectionist_rights_advocacy_groups).
narrative_ontology:fixing_cost_class(second_amendment_boundary__insurrectionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared constitutional theory under which decentralized armed populations coordinate around a common deterrent posture against potential federal or state overreach, without requiring formal militia organization or government sanction.
% TRANSFER_FUNCTION: Moves regulatory latitude away from legislatures and toward armed citizen and advocacy organizations; moves risk exposure toward state security personnel (who face an armed populace framed as their constitutional check) and toward bystander populations who bear the tail-risk and ordinary-risk costs of a weapons market shaped by anti-regulation constitutional theory.
% ABSENT_VOICES: Civilians who would be caught in any actual armed conflict scenario, and communities on the receiving end of militia intimidation, have no formal role in constitutional interpretation and are not parties to the litigation or legislative fights that entrench this reading.
% DISAPPEARANCE_RATIONALE: If the insurrectionist reading lost its current litigation and advocacy traction, regulation of military-pattern arms and high-capacity magazines would become substantially more feasible, militia groups would lose their strongest constitutional cover for organized armed displays, and manufacturers marketing deterrence-themed weapons would face a materially different legal and reputational landscape.
% FOUNDING_PROBLEM: The reading traces itself to founding-era anxiety about standing armies and centralized power — the belief that a populace capable of armed resistance is the ultimate check against a government that captures or corrupts its own military and police institutions.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the early republic attest that anti-standing-army anxiety was genuinely live in 1791, but many of the same historians and most sitting judiciary outside advocacy circles attest that the specific claim — that individual possession of modern military-pattern semi-automatic weapons is necessary to that check today — is a twentieth/twenty-first century construction not directly traceable to founding-era militia practice, which centered on organized state militias rather than solitary armed resistance.
narrative_ontology:disappearance_verdict(second_amendment_boundary__insurrectionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_boundary__insurrectionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_boundary__insurrectionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_boundary__insurrectionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_boundary__insurrectionist_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_boundary__insurrectionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_boundary__insurrectionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_boundary__insurrectionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.58) because the reading transfers real regulatory latitude and market protection to organized and commercial beneficiaries while imposing diffuse, largely uncompensated risk on bystander populations — but it is not maximal because the coordination function (a genuine anti-tyranny check, historically rooted in real founding-era concerns) is not pure invention. Suppression is comparatively low (0.34): the reading does not itself coerce anyone into arms possession; its coercive force operates mainly by foreclosing regulatory options, not by compelling behavior. Theater ratio rises over the measured interval (0.20 to 0.42) as the reading increasingly serves symbolic/political functions (deterrence rhetoric, litigation posturing) relative to any operative resistance function, since no actual insurrection scenario has materialized to test the capacity it claims to preserve. Accessibility collapse is low (0.30): rival readings (individual-right, militia-conditioned) remain fully live in courts, legislatures, and public discourse — this reading has not foreclosed its siblings, it competes with them. Resistance is high (0.72): scholars, legislators, and law enforcement organizations actively contest this reading's historical grounding and practical consequences.
 *
 * PERSPECTIVAL GAP:
 *   From the armed-citizen and advocacy seats, this reading is a rope: it coordinates decentralized deterrent capacity against a real historical danger, at negligible cost to anyone not planning tyranny. From the state-security and bystander-population seats, the same reading operates as a tangled rope shading toward snare: the coordination story is real in the abstract but the concrete, ongoing costs (casualty populations, intimidated communities) are borne by parties who gain none of the deterrent benefit and cannot exit the arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Armed citizen movements, advocacy groups, and manufacturers sit near the beneficiary end: they collect legitimacy, market protection, and political capital from the reading's persistence, and their exit options (relocate, organize, arbitrage across jurisdictions) are wide. State security personnel sit near the target end: their institutional position is fixed (trapped) and the reading structurally casts their function as the object of legitimate armed deterrence — an unusual and largely externally-imposed directional assignment that could not be derived from ordinary beneficiary/victim declarations alone. Civilian bystander populations (hypothetical conflict zones, mass-shooting casualties, intimidated communities) are structurally trapped victims: they neither hold the right's claimed deterrent capacity nor have exit from the jurisdictions and public spaces where its consequences materialize.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — anxiety about a government capturing its own security apparatus and needing a check — was genuinely live at the founding and arguably remains live in the abstract (state security personnel do periodically become instruments of illegitimate state action in ostensibly protected societies). But the specific INSTRUMENT this reading insists on — broad individual access to military-pattern semi-automatic weapons as the operative deterrent — has never been tested against an actual tyranny scenario in the covered interval, while its ordinary-time costs (mass casualty events, militia intimidation) are continuously realized. This is precisely the mandatrophy pattern: a mandate whose original problem is abstractly plausible but whose specific implementing instrument has drifted into serving different, more concrete beneficiaries (commercial manufacturers, advocacy organizations) while its stated function goes untested. Classifying this as tangled_rope rather than snare preserves the genuine, if abstract and untested, coordination function (anti-tyranny deterrence) alongside the concrete, continuously realized extraction (diffuse civilian risk, commercial capture) — collapsing it to pure snare would erase the founding coordination logic; collapsing it to pure rope would erase the demonstrated victim set.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    insurrectionist_reading_kernel_position,
    'Is the insurrectionist reading (armed resistance as the right''s core purpose, with military-pattern arms as the logical endpoint) the historically correct reading of the Second Amendment kernel, or a twentieth-century reconstruction serving present commercial and political interests?',
    'Comparative historical analysis of founding-era militia statutes, ratification debates, and contemporaneous commentary, weighed against the documented emergence and funding sources of the modern insurrectionist scholarship and advocacy movement from the 1970s forward.',
    'If historically grounded, the reading''s coordination function is more substantial than the metrics here suggest, lowering justified extraction. If a modern reconstruction, the coordination story is closer to pure cover for the extraction already measured, and the classification would drift toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(insurrectionist_reading_kernel_position, conceptual, 'Whether the insurrectionist premise is founding-era doctrine or modern reconstruction.').

omega_variable(
    military_grade_arms_logical_endpoint,
    'Does the insurrectionist reading''s core premise logically require that military-grade or military-pattern arms fall within the protected core, or is that an contestable extension rather than an entailment?',
    'Doctrinal analysis of whether ''preserving resistance capacity'' necessarily implies parity with state military technology, versus a weaker reading requiring only some organized armed capacity short of parity.',
    'If military-grade parity is a strict entailment, the reading''s extraction (blocking regulation of the highest-risk weapon categories) is structurally necessary to the reading and cannot be trimmed without abandoning the reading. If it is a contestable extension, courts could adopt the insurrectionist premise while still permitting regulation of military-pattern arms specifically, substantially lowering measured extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(military_grade_arms_logical_endpoint, conceptual, 'Whether military-pattern arms protection is entailed by or merely associated with the insurrectionist premise.').

omega_variable(
    state_disarmament_as_tyranny_precursor,
    'Under this reading, is ANY state effort to regulate or register firearms properly treated as evidence of incipient tyranny, or only measures that approach effective disarmament?',
    'Track how advocacy and litigation under this reading actually characterize graduated regulatory measures (background checks, waiting periods, registration) versus outright confiscation, across the measured interval.',
    'A reading that treats ALL regulation as a tyranny precursor produces much higher suppression of ordinary public-safety policy than a reading that reserves the tyranny framing for near-total disarmament; this affects how much of the measured extraction is attributable to this specific premise versus adjacent political rhetoric.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_disarmament_as_tyranny_precursor, empirical, 'Scope of what regulatory measures the reading treats as tyranny precursors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__insurrectionist_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1970, second_amendment_boundary__insurrectionist_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement_basis(seco_tr_t1970, observed).
narrative_ontology:measurement(seco_tr_t1980, second_amendment_boundary__insurrectionist_reading, theater_ratio, 1980, 0.24).
narrative_ontology:measurement_basis(seco_tr_t1980, observed).
narrative_ontology:measurement(seco_tr_t1994, second_amendment_boundary__insurrectionist_reading, theater_ratio, 1994, 0.3).
narrative_ontology:measurement_basis(seco_tr_t1994, observed).
narrative_ontology:measurement(seco_tr_t2004, second_amendment_boundary__insurrectionist_reading, theater_ratio, 2004, 0.34).
narrative_ontology:measurement_basis(seco_tr_t2004, observed).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_boundary__insurrectionist_reading, theater_ratio, 2008, 0.36).
narrative_ontology:measurement_basis(seco_tr_t2008, observed).
narrative_ontology:measurement(seco_tr_t2016, second_amendment_boundary__insurrectionist_reading, theater_ratio, 2016, 0.39).
narrative_ontology:measurement_basis(seco_tr_t2016, observed).
narrative_ontology:measurement(seco_tr_t2024, second_amendment_boundary__insurrectionist_reading, theater_ratio, 2024, 0.42).
narrative_ontology:measurement_basis(seco_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(seco_be_t1970, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 1970, 0.28).
narrative_ontology:measurement_basis(seco_be_t1970, observed).
narrative_ontology:measurement(seco_be_t1980, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 1980, 0.34).
narrative_ontology:measurement_basis(seco_be_t1980, observed).
narrative_ontology:measurement(seco_be_t1994, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 1994, 0.4).
narrative_ontology:measurement_basis(seco_be_t1994, observed).
narrative_ontology:measurement(seco_be_t2004, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 2004, 0.46).
narrative_ontology:measurement_basis(seco_be_t2004, observed).
narrative_ontology:measurement(seco_be_t2008, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 2008, 0.5).
narrative_ontology:measurement_basis(seco_be_t2008, observed).
narrative_ontology:measurement(seco_be_t2016, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 2016, 0.54).
narrative_ontology:measurement_basis(seco_be_t2016, observed).
narrative_ontology:measurement(seco_be_t2024, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 2024, 0.58).
narrative_ontology:measurement_basis(seco_be_t2024, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(second_amendment_boundary__insurrectionist_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_boundary__insurrectionist_reading, identity_coordination).
narrative_ontology:affects_constraint(second_amendment_boundary__insurrectionist_reading, individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__insurrectionist_reading, militia_conditioned_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'the Second Amendment right' into structurally distinct constraints per the epsilon-invariance principle. individual_right_reading grounds the right in personal self-defense with a narrower protected core; militia_conditioned_reading bounds the right to organized collective militia service and permits comprehensive individual regulation; insurrectionist_reading (this story) grounds the right in anti-tyranny deterrent capacity with military-pattern arms at its logical endpoint. Each carries its own epsilon, its own beneficiary/victim structure, and its own classification; they are linked via affects_constraints rather than merged into one measurement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
