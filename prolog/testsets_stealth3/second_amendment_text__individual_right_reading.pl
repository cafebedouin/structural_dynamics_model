% ============================================================================
% CONSTRAINT STORY: second_amendment_text__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__individual_right_reading, []).

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
 *   constraint_id: second_amendment_text__individual_right_reading
 *   human_readable: Second Amendment Operative Clause — Individual Right Reading
 *   domain: constitutional law/political theory/firearms policy
 *
 * SUMMARY:
 *   Under this reading, the amendment's operative clause guarantees each
 *   lawful individual a right to keep and bear arms independent of any
 *   militia affiliation, with personal self-defense as the core protected
 *   activity; institutionalized through Heller and extended by Bruen, it now
 *   operates as a standing prohibition on defined classes of firearm
 *   regulation, administered by the federal courts. The ε referent is the
 *   standing arrangement under contest — the constitutionalized
 *   individual-right regime as it actually operates — assessed by this
 *   reading's own lights: the reading endorses the guarantee as a liberty
 *   protection and discounts violence externalities as discrete crimes, so it
 *   authors a comparatively low ε (0.30), driven mainly by the categorical
 *   exclusion of prohibited classes and the authority transferred away from
 *   legislatures. Claim and metrics are independent authored facts: the type
 *   claim (tangled_rope) rests on the structure (real
 *   expectation-coordination plus asymmetric costs plus active judicial
 *   enforcement), while the metrics describe observed operation without being
 *   tuned to any predicted engine output.
 *
 * KEY AGENTS:
 *   - - federal_judiciary: agenda setter (institutional/constrained) — administers the guarantee's boundaries, strikes regulation, owns the historical-analogue method
 *   - - individual_gun_owners: primary beneficiary (organized/identity_locked) — holds the protected activity; exit is identity-fused
 *   - - firearms_industry: concentrated economic beneficiary (powerful/arbitrage) — collects the market-insurance rents
 *   - - gun_rights_advocacy_organizations: beneficiary with agenda-setting reach (organized/identity_locked) — sponsors the enforcement litigation
 *   - - prohibited_persons: primary target (powerless/trapped) — categorically disarmed by the same framework that liberates others
 *   - - communities_exposed_to_firearm_violence: diffuse target (powerless/trapped) — absorbs unpriced externalities, remedies foreclosed
 *   - - state_local_legislatures: institutional target (institutional/constrained) — policy toolkit voided or pre-chilled
 *   - - gun_control_advocacy_organizations: excluded voice (organized/mobile) — participates formally but not in the operative interpretive idiom
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__individual_right_reading, 0.3).
domain_priors:suppression_score(second_amendment_text__individual_right_reading, 0.58).
domain_priors:theater_ratio(second_amendment_text__individual_right_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_text__individual_right_reading, "Second Amendment Operative Clause — Individual Right Reading").
narrative_ontology:topic_domain(second_amendment_text__individual_right_reading, "constitutional law/political theory/firearms policy").

domain_priors:requires_active_enforcement(second_amendment_text__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__individual_right_reading, '89b16f2e-3b0b-4f4b-87b6-6f40873e9d13').
narrative_ontology:cs_kernel_codification('89b16f2e-3b0b-4f4b-87b6-6f40873e9d13', fixed_text).
narrative_ontology:cs_authority_grounding('89b16f2e-3b0b-4f4b-87b6-6f40873e9d13', lineage).
narrative_ontology:cs_interpretation_layer_present('89b16f2e-3b0b-4f4b-87b6-6f40873e9d13').
narrative_ontology:cs_reading_relation('89b16f2e-3b0b-4f4b-87b6-6f40873e9d13', second_amendment_text__collective_security_reading, forecloses).
narrative_ontology:cs_reading_relation('89b16f2e-3b0b-4f4b-87b6-6f40873e9d13', second_amendment_text__originalist_civic_virtue_reading, coexists_with).
narrative_ontology:cs_axiom('89b16f2e-3b0b-4f4b-87b6-6f40873e9d13', foundational, right_independent_of_militia_service).
narrative_ontology:cs_axiom_status(right_independent_of_militia_service, holdable).
narrative_ontology:cs_axiom_grounding('89b16f2e-3b0b-4f4b-87b6-6f40873e9d13', right_independent_of_militia_service, deontological).
narrative_ontology:cs_axiom('89b16f2e-3b0b-4f4b-87b6-6f40873e9d13', foundational, personal_self_defense_core_protected_activity).
narrative_ontology:cs_axiom_status(personal_self_defense_core_protected_activity, holdable).
narrative_ontology:cs_axiom_grounding('89b16f2e-3b0b-4f4b-87b6-6f40873e9d13', personal_self_defense_core_protected_activity, deontological).
narrative_ontology:cs_reference_frame('89b16f2e-3b0b-4f4b-87b6-6f40873e9d13', inherent_individual_armament_liberty).
narrative_ontology:cs_drift_state('89b16f2e-3b0b-4f4b-87b6-6f40873e9d13', post_heller_bruen_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('89b16f2e-3b0b-4f4b-87b6-6f40873e9d13', '').
narrative_ontology:cs_kernel_id(second_amendment_text__individual_right_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__individual_right_reading, individual_gun_owners).
narrative_ontology:constraint_beneficiary(second_amendment_text__individual_right_reading, firearms_industry).
narrative_ontology:constraint_beneficiary(second_amendment_text__individual_right_reading, gun_rights_advocacy_organizations).
narrative_ontology:constraint_victim(second_amendment_text__individual_right_reading, prohibited_persons).
narrative_ontology:constraint_victim(second_amendment_text__individual_right_reading, communities_exposed_to_firearm_violence).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(second_amendment_text__individual_right_reading, state_local_legislatures).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the operative clause and decides what counts as an infringement: which weapons, which carrying arrangements, and which classes of persons fall inside or outside the guarantee. Reviews state and federal legislation against that line and strikes or sustains it. Since 2022 the prevailing method tests proposed regulations against founding- and Reconstruction-era analogues rather than weighing present-day harms. The branch's centrality to American firearms governance depends on the guarantee remaining litigated; stepping back from that role would mean contracting judicial review itself.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Tens of millions of households keep firearms for self-defense, hunting, and sport. The guarantee protects their ability to acquire, keep, and carry against confiscation and discretionary licensing. Many experience lawful armament as central to personal autonomy, family tradition, and political identity, so relinquishing it is not experienced as a realistic option. They bear training, storage, and insurance costs and, where shall-issue permits survive, modest procedural burdens that recent rulings have narrowed.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, individual_gun_owners, beneficiary,
    organized, biographical, identity_locked, national).

% Manufacturers, importers, and dealers sell into a civilian market whose legal durability rests on the guarantee. Constitutional protection shields product lines from prohibition campaigns and stabilizes demand; litigation victories open addressable segments such as everyday carry and modern sporting rifles. The industry funds trade associations and allied advocacy and diversifies into export markets where domestic policy tightens.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, firearms_industry, beneficiary,
    powerful, biographical, arbitrage, global).

% Membership- and donor-funded organizations whose institutional purpose is defending and expanding the guarantee. They sponsor the litigation that drives the enforcement agenda, draft model legislation, score legislators, and mobilize electoral pressure. Organizational identity is fused with the cause; the end of the contest would end their reason to exist. Litigation sponsorship gives them agenda-shaping reach well beyond their formal seat in the process.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, gun_rights_advocacy_organizations, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_text__individual_right_reading, gun_rights_advocacy_organizations, agenda_setter).

% People with felony convictions or qualifying misdemeanor domestic-violence records are categorically barred from possessing firearms under federal and state law. The same rights framework that shields lawful holders cements their exclusion: every expansion of the guarantee for others is argued alongside the necessity of their disarmament, and the governing historical method tends to ratify long-standing exclusions rather than reexamine them. The bar is effectively lifelong; a record cannot be exited, and restoration regimes are rare and narrow.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, prohibited_persons, payer,
    powerless, biographical, trapped, national).

% Neighborhoods — disproportionately urban, poor, and minority — where gunshot injury and homicide are chronic. They absorb medical, policing, mourning, and economic costs of pervasive civilian armament, and their preferred remedies (discretionary permitting, category restrictions, waiting periods) are precisely the instruments the guarantee strikes down or chills. Relocation is rarely feasible; the costs arrive whether or not any given resident ever touches a gun.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, communities_exposed_to_firearm_violence, payer,
    powerless, generational, trapped, regional).

% Statehouses and city councils enact firearm policy responsive to their constituents. The guarantee voids or pre-chills large parts of their preferred toolkit, forcing redrafting around historically analogous measures, expenditure on defenses that fail, and dependence on federal courts for even permitted programs. They cannot opt out of the constitutional layer; their authority over this policy domain is subordinate to it.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, state_local_legislatures, payer,
    institutional, biographical, constrained, national).

% National organizations pursuing restrictive firearm policy through lobbying, ballot initiatives, and litigation. They participate formally — as amici, witnesses, and litigants — but the governing interpretive method admits no present-day empirical or policy input: arguments grounded in casualty data carry no weight in proceedings decided by historical analogy. Their substantive objections circulate everywhere except the room where outcomes are actually determined.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, gun_control_advocacy_organizations, excluded,
    organized, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_text__individual_right_reading, firearms_industry).
narrative_ontology:fixing_cost_class(second_amendment_text__individual_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes a uniform, judicially enforceable floor on personal armament across jurisdictions: citizens gain predictable security against confiscation and patchwork regulation, legislatures gain a known outer boundary for policy, and disputes over who may be armed resolve through a single adjudicative standard instead of fifty divergent regimes.
% TRANSFER_FUNCTION: Moves regulatory authority over firearms from state and local majorities to federal courts and individual holders; shifts primary protection responsibility from public institutions toward private households; converts community safety preferences into constitutionally overridable claims; and delivers durable market certainty to the civilian firearms industry.
% ABSENT_VOICES: Survivors of gunshot violence, the communities that absorb its costs, and public-health researchers would object that the arrangement forecloses their preferred remedies, but they hold no seat: adjudication proceeds by historical analogy, which admits no contemporary testimony, and prohibited persons play no part in defining the terms of their own exclusion. The unanimity of the interpretive conversation partly reflects who was never admitted to it.
% DISAPPEARANCE_RATIONALE: Overnight removal would trigger an immediate regulatory surge in dozens of states, a repricing shock across the civilian firearms market, mass litigation to unwind reliance interests built on Heller and Bruen, and a federalism restructuring of police powers — millions of holders' legal position, an industry's market, and a century of doctrine would all have to reorganize.
% FOUNDING_PROBLEM: Founding-era dread of standing armies and centralized disarmament: the arrangement was built to keep the means of force distributed among the populace, drawing on the 1689 English precedent and the colonial experience of royal disarmament attempts, so that an armed citizenry could constitute the militia and resist governmental usurpation.
% FOUNDING_PROBLEM_CORROBORATION: Professional constitutional historiography — outside any advocacy camp — corroborates the anti-standing-army and militia-genesis account of the founding problem while disputing the modern gloss that individual self-defense was its core; for THIS reading's specific framing (personal self-defense as the central protected activity), corroborating scholarship external to the benefiting parties is thin to absent, and that absence is itself signal.
narrative_ontology:disappearance_verdict(second_amendment_text__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_text__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_text__individual_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_text__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_text__individual_right_reading, 0.3, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__individual_right_reading_tests).
:- end_tests(second_amendment_text__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (ε 0.30, endpoint of the series) is moderate-low BY THIS READING'S LIGHTS over the fixed referent: the reading counts the excluded classes and the transferred regulatory authority as costs but treats violence as discrete wrongdoing rather than systemic extraction — a public-health seat over the same referent would author far higher (see omega epsilon_reading_indexed). Suppression (0.58) is a raw, unscaled structural property: it measures how completely the constraint narrows the legislative alternative space (discretionary permitting struck, category bans chilled, empirical argument inadmissible), not coercive intensity scaled by anything. Theater (0.22 at endpoint) is low because enforcement is substantively consequential; the series is non-monotonic — theater peaked (~0.50) during the Miller-era dormancy when the text was ritually invoked while regulating flowed freely, and fell as adjudication became real, so the peak marks a piton-shaped past phase, not the present. Accessibility_collapse (0.45) is partial: shall-issue permitting, background checks, and historically analogous measures survive, but a wide band of alternatives has collapsed or been pre-chilled. Resistance (0.78) is among the highest of any constitutional constraint: sustained mass mobilization, state-level counter-legislation, and academic opposition meet every expansion. The three tracked metrics share one five-point grid (all authored at every point); the suppression_requirement series is authored deliberately because enforcement-capacity change IS this story's dynamic (dormant machinery → Heller activation → Bruen-wave ratchet), not a static backdrop. gain_flow names firearms_industry: autonomy gains are diffuse across millions of holders, but the monetizable rent stream — market insulation — concentrates and lands there; fixing_cost is prohibitive because remedying or removing the arrangement requires either an Article V amendment or decades-long doctrinal reversal against entrenched identity politics and court-composition dependence.
 *
 * PERSPECTIVAL GAP:
 *   The seats should classify differently and the engine computes that from the structural data. From the individual_gun_owners seat the arrangement is a liberty guarantee protecting a core life activity (low effective extraction, subsidy-flavored). From the prohibited_persons seat the same guarantee is the mechanism that cements categorical, lifelong exclusion — maximal extraction from a trapped, powerless seat. From state_local_legislatures it is a veto imposed on their policy authority; from communities_exposed_to_firearm_violence it is an unfunded externality with the remedy constitutionally foreclosed. The federal_judiciary sits near the beneficiary side without collecting rents: the arrangement subsidizes the branch's centrality. Same-power divergence is visible among the organized advocacy seats: pro-gun organizations (beneficiary-declared, identity-locked) and gun-control organizations (excluded) hold nominally similar resources but opposite structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the three beneficiary seats toward low d (subsidy side); the two victim declarations drive prohibited_persons and communities_exposed_to_firearm_violence toward high d, amplified by trapped exit and powerless power. No directionality_overrides are authored, deliberately: overrides key on power atoms, and this story's opposed seats collide within atoms — 'organized' contains both pro-gun and gun-control advocacy, 'institutional' contains both the enforcing courts and the constrained legislatures — so any per-atom correction would flatten a seat the structural derivation already places correctly. Role-plus-exit data carries the differentiation. Residual uncertainty: the judiciary declares no beneficiary/victim status, so its d rides the canonical institutional fallback rather than structural derivation; its true relationship (centrality subsidized, no rents collected) is mildly beneficiary-side, and the commentary flags that the fallback may under- or over-state this.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — keeping force distributed against standing-army consolidation — was substantially transformed when the militia system was absorbed into a federally controlled National Guard, yet the arrangement persists with a renewed mandate (personal self-defense) grafted on. mandatrophy_resolved is therefore not declared: the mandate is not dead, it was re-founded, which is exactly why this must not be mislabeled. Reading the structure as a snare would erase the genuine expectation-coordination function (a uniform confiscation-proof floor that millions rely on); reading it as a pure rope would erase the asymmetric extraction (lifelong categorical exclusion of prohibited classes; externality costs shifted onto communities whose remedies are foreclosed; market insurance delivered to industry). The tangled_rope claim keeps both halves visible and lets the per-seat computation expose where the arrangement looks like protection and where it looks like imposition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_partition,
    'Which reading of the second_amendment_text kernel governs, and does this story''s classification travel to the sibling readings?',
    'Compile and classify the sibling stories (second_amendment_text__collective_security_reading, second_amendment_text__originalist_civic_virtue_reading) under identical engine settings and compare per-seat outputs.',
    'Classification is reading-indexed: under collective_security_reading the beneficiary set inverts toward state institutions and organized militia, individual holders become the regulated class, and permit regimes this reading resists become the coordination solution. No verdict here transfers across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_partition, conceptual, 'This constraint is one of three readings of one kernel; its type, ε, and victim set are partition-local.').

omega_variable(
    sibling_structural_delta_collective,
    'What exactly changes structurally under the collective_security_reading sibling?',
    'Author the sibling story and diff its beneficiary/victim arrays, enforcement direction, and suppression profile against this file.',
    'Militia-conditionality legitimizes licensing and permitting that this reading strikes down; the excluded-class logic relaxes (regulation serves everyone''s security) while armed individuals inherit the target seat; the enforcement machinery reverses polarity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_structural_delta_collective, conceptual, 'Structural delta expected for the militia-conditioned sibling reading.').

omega_variable(
    epsilon_reading_indexed,
    'How would a public-health/welfarist seat over the SAME referent (the constitutionalized individual-right regime) author ε?',
    'Author a parallel reading-story whose lights count violence externalities, foreclosed remedies, and privatized protection burdens as extraction; compare authored ε values over the identical referent.',
    'ε would rise substantially once externality costs enter the measure; the pair of values demonstrates that ε is a property of the reading, not the topic (OQ-26), and that cross-reading comparisons of the number are invalid by construction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_reading_indexed, conceptual, 'Reading-indexed nature of the authored ε over a fixed referent.').

omega_variable(
    historical_method_inclusion_boundary,
    'Which regulations and which person-classes does the founding-era-analogue method ultimately place inside versus outside the guarantee?',
    'Accumulated circuit decisions plus eventual higher-court treatment of sensitive places, permit regimes, waiting periods, and prohibited-class challenges under the analogue method.',
    'Sets the final size of the victim set and the terminal suppression level: a permissive boundary shrinks prohibited_persons toward the current statutory floor while tightening it elsewhere grows the extracted-from set; the story''s 2026 endpoint values are provisional on this unresolved boundary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(historical_method_inclusion_boundary, conceptual, 'Irreducible uncertainty in the interpretive method that draws the guarantee''s inclusion/exclusion lines.').

omega_variable(
    founding_problem_liveliness,
    'Is the founding problem (preventing government monopoly of force; guarding against standing-army domination) live, or superseded by the National Guard settlement and modern policing?',
    'No in-framework resolution: the question turns on contested political theory about distributed force and insurrectionist legitimacy, not on discoverable data.',
    'If dead, the arrangement persists past its function and the (dead-status x world_rearranges) mismatch flags zombie persistence for investigation; if live, the coordination function remains genuine and the tangled_rope reading strengthens. Parties dispute this along exactly the fault line the sibling readings occupy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_liveliness, preference, 'Contested liveliness of the anti-disarmament founding problem; drives founding_problem_status=contested.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__individual_right_reading, 1791, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(samend_individual_right_tr_t1791, second_amendment_text__individual_right_reading, theater_ratio, 1791, 0.05).
narrative_ontology:measurement(samend_individual_right_tr_t1939, second_amendment_text__individual_right_reading, theater_ratio, 1939, 0.45).
narrative_ontology:measurement(samend_individual_right_tr_t1968, second_amendment_text__individual_right_reading, theater_ratio, 1968, 0.5).
narrative_ontology:measurement(samend_individual_right_tr_t2008, second_amendment_text__individual_right_reading, theater_ratio, 2008, 0.25).
narrative_ontology:measurement(samend_individual_right_tr_t2026, second_amendment_text__individual_right_reading, theater_ratio, 2026, 0.22).

% Extraction over time
narrative_ontology:measurement(samend_individual_right_be_t1791, second_amendment_text__individual_right_reading, base_extractiveness, 1791, 0.15).
narrative_ontology:measurement(samend_individual_right_be_t1939, second_amendment_text__individual_right_reading, base_extractiveness, 1939, 0.2).
narrative_ontology:measurement(samend_individual_right_be_t1968, second_amendment_text__individual_right_reading, base_extractiveness, 1968, 0.18).
narrative_ontology:measurement(samend_individual_right_be_t2008, second_amendment_text__individual_right_reading, base_extractiveness, 2008, 0.24).
narrative_ontology:measurement(samend_individual_right_be_t2026, second_amendment_text__individual_right_reading, base_extractiveness, 2026, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(samend_individual_right_su_t1791, second_amendment_text__individual_right_reading, suppression_requirement, 1791, 0.1).
narrative_ontology:measurement(samend_individual_right_su_t1939, second_amendment_text__individual_right_reading, suppression_requirement, 1939, 0.15).
narrative_ontology:measurement(samend_individual_right_su_t1968, second_amendment_text__individual_right_reading, suppression_requirement, 1968, 0.12).
narrative_ontology:measurement(samend_individual_right_su_t2008, second_amendment_text__individual_right_reading, suppression_requirement, 2008, 0.38).
narrative_ontology:measurement(samend_individual_right_su_t2026, second_amendment_text__individual_right_reading, suppression_requirement, 2026, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__individual_right_reading, identity_coordination).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, second_amendment_text__collective_security_reading).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, second_amendment_text__originalist_civic_virtue_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'the Second Amendment' into three structurally distinct commitments per ε-invariance (DP-001): this file is the individual-right instantiation (ε 0.30, reading-indexed, victim set = prohibited classes plus externality-bearing communities); the collective_security_reading sibling legitimizes regulation and inverts the beneficiary/target topology; the originalist_civic_virtue_reading sibling shares the historical warrant this reading's litigation cites, so it exerts upstream evidentiary influence while remaining a separately classified constraint. Each family member links the others via affects_constraints; no single story averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
