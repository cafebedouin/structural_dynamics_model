% ============================================================================
% CONSTRAINT STORY: second_amendment_boundary__insurrectionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: second_amendment_boundary__insurrectionist_reading
 *   human_readable: Second Amendment — Insurrectionist Reading (Armed Resistance Capacity)
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This story instantiates the insurrectionist reading of the Second
 *   Amendment kernel: the claim that the right to keep and bear arms exists
 *   fundamentally to preserve a citizenry's capacity for armed resistance to
 *   tyrannical government, with individual possession understood as
 *   instrumental to that potential overthrow function. This reading pushes
 *   further than the individual_right_reading (which grounds the right in
 *   personal self-defense and does not require a resistance-to-tyranny
 *   justification) and directly opposes the militia_conditioned_reading
 *   (which would bound the right to organized, regulated collective defense).
 *   The insurrectionist reading's logical endpoint is that military-grade and
 *   military-pattern arms — not merely home-defense weapons — fall within the
 *   protected core, because sporting or self-defense arms would be inadequate
 *   to the deterrence function the reading exists to serve. This is a
 *   distinct constraint from its siblings, not a different observable angle
 *   on the same one: its beneficiary set, victim set, and extraction profile
 *   differ structurally, per the ε-invariance decomposition rule.
 *
 * KEY AGENTS:
 *   - armed_citizen_militias: primary beneficiary (organized/mobile) — claims deterrent legitimacy
 *   - firearms_manufacturers: secondary beneficiary (institutional/arbitrage) — commercial upside from expanded protected category
 *   - insurrectionist_advocacy_organizations: agenda_setter (organized/mobile) — produces legal architecture
 *   - state_security_apparatus_personnel: primary target (institutional/trapped) — designated adversary in the reading's own logic
 *   - civilians_in_hypothetical_conflict_zones: latent victim (powerless/trapped) — bears the worst-case cost the reading's logic requires as a live possibility
 *   - gun_violence_survivors: realized victim (powerless/trapped) — bears concrete, non-hypothetical cost regardless of whether the deterrence scenario ever occurs
 *   - constitutional_scholars: analytical observer — evaluates textual and historical basis independent of advocacy stakes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_boundary__insurrectionist_reading, 0.58).
domain_priors:suppression_score(second_amendment_boundary__insurrectionist_reading, 0.42).
domain_priors:theater_ratio(second_amendment_boundary__insurrectionist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__insurrectionist_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_boundary__insurrectionist_reading, "Second Amendment — Insurrectionist Reading (Armed Resistance Capacity)").
narrative_ontology:topic_domain(second_amendment_boundary__insurrectionist_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_boundary__insurrectionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__insurrectionist_reading, '26bdbe51-efec-4f89-809f-a83ef93c0355').
narrative_ontology:cs_kernel_codification('26bdbe51-efec-4f89-809f-a83ef93c0355', fixed_text).
narrative_ontology:cs_authority_grounding('26bdbe51-efec-4f89-809f-a83ef93c0355', lineage).
narrative_ontology:cs_interpretation_layer_present('26bdbe51-efec-4f89-809f-a83ef93c0355').
narrative_ontology:cs_reading_relation('26bdbe51-efec-4f89-809f-a83ef93c0355', second_amendment_boundary__individual_right_reading, influences).
narrative_ontology:cs_reading_relation('26bdbe51-efec-4f89-809f-a83ef93c0355', second_amendment_boundary__militia_conditioned_reading, forecloses).
narrative_ontology:cs_axiom('26bdbe51-efec-4f89-809f-a83ef93c0355', foundational, armed_citizenry_is_ultimate_check_on_tyranny).
narrative_ontology:cs_axiom_status(armed_citizenry_is_ultimate_check_on_tyranny, holdable).
narrative_ontology:cs_axiom_grounding('26bdbe51-efec-4f89-809f-a83ef93c0355', armed_citizenry_is_ultimate_check_on_tyranny, deontological).
narrative_ontology:cs_axiom('26bdbe51-efec-4f89-809f-a83ef93c0355', foundational, individual_possession_instrumental_to_collective_overthrow_capacity).
narrative_ontology:cs_axiom_status(individual_possession_instrumental_to_collective_overthrow_capacity, holdable).
narrative_ontology:cs_axiom_grounding('26bdbe51-efec-4f89-809f-a83ef93c0355', individual_possession_instrumental_to_collective_overthrow_capacity, instrumental).
narrative_ontology:cs_axiom('26bdbe51-efec-4f89-809f-a83ef93c0355', secondary, military_grade_arms_fall_within_protected_core).
narrative_ontology:cs_axiom_status(military_grade_arms_fall_within_protected_core, holdable).
narrative_ontology:cs_axiom_grounding('26bdbe51-efec-4f89-809f-a83ef93c0355', military_grade_arms_fall_within_protected_core, instrumental).
narrative_ontology:cs_reference_frame('26bdbe51-efec-4f89-809f-a83ef93c0355', founding_era_anti_standing_army_anxiety).
narrative_ontology:cs_drift_state('26bdbe51-efec-4f89-809f-a83ef93c0355', contemporary_post_heller_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('26bdbe51-efec-4f89-809f-a83ef93c0355', '').
narrative_ontology:cs_kernel_id(second_amendment_boundary__insurrectionist_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__insurrectionist_reading, armed_citizen_militias).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__insurrectionist_reading, firearms_manufacturers).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__insurrectionist_reading, insurrectionist_advocacy_organizations).
narrative_ontology:constraint_victim(second_amendment_boundary__insurrectionist_reading, state_security_apparatus_personnel).
narrative_ontology:constraint_victim(second_amendment_boundary__insurrectionist_reading, civilians_in_hypothetical_conflict_zones).
narrative_ontology:constraint_victim(second_amendment_boundary__insurrectionist_reading, gun_violence_survivors).
narrative_ontology:constraint_victim(second_amendment_boundary__insurrectionist_reading, targeted_minority_communities).
narrative_ontology:constraint_vindicates(second_amendment_boundary__insurrectionist_reading, popular_sovereignty_ultimate_check_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_boundary__insurrectionist_reading, distrust_of_standing_government_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organize around the claim that private ownership of military-grade arms is constitutionally protected specifically because it preserves the capacity to resist a future tyrannical government. They lobby against weapons bans, litigate to expand what counts as protected arms, and treat any registration or confiscation effort as evidence the predicted tyranny is arriving. Their legitimacy claim is deterrence: the government restrains itself because the citizenry is armed.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, armed_citizen_militias, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_boundary__insurrectionist_reading, armed_citizen_militias, agenda_setter).

% Sell semi-automatic rifles and related equipment into a market where insurrectionist framing is a marketing and legal asset — the broader the constitutional reading, the larger the addressable market for higher-capacity and military-pattern weapons. They fund advocacy litigation supporting the reading and face no direct cost if the deterrence theory is never tested.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, firearms_manufacturers, beneficiary,
    institutional, generational, arbitrage, national).

% Produce the legal and rhetorical architecture for the reading — model legislation, litigation strategy, public messaging framing gun control as a precursor to tyranny. They set the terms under which the reading is defended in courts and legislatures and benefit organizationally (funding, membership, influence) from the reading's persistence, independent of whether armed resistance ever actually occurs.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, insurrectionist_advocacy_organizations, agenda_setter,
    organized, generational, mobile, national).

% Law enforcement and military personnel who would be the direct targets of any realized armed resistance under this reading's logic. They bear the operational risk of a citizenry legally entitled to military-grade arms explicitly for the purpose of resisting them, and cannot exit the structural position of being the designated adversary in the reading's founding scenario.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, state_security_apparatus_personnel, payer,
    institutional, biographical, trapped, national).

% Ordinary residents who would be caught in any scenario where armed private resistance to government actually materializes — a category the reading requires to exist in principle for its logic to have content, but which bears none of the reading's benefits and all of its worst-case costs. They have no say in whether the deterrence theory is ever tested against them.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, civilians_in_hypothetical_conflict_zones, payer,
    powerless, immediate, trapped, local).

% Bear the routine, non-hypothetical cost of an expanded arms-protection regime — mass casualty events, domestic violence lethality, accidental deaths — that occurs whether or not the insurrectionist scenario ever arrives. The deterrence benefit is speculative and diffuse; their harm is concrete and already realized.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, gun_violence_survivors, payer,
    powerless, biographical, trapped, local).

% Historically, insurrectionist-framed arms rights have been asymmetrically enforced — armed resistance rhetoric and gun ownership by minority groups has drawn far harsher state response than the same conduct by others. They bear a double cost: exposure to armed vigilante action justified by this reading, and unequal access to whatever protective benefit the reading claims to confer.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, targeted_minority_communities, payer,
    powerless, generational, constrained, national).

% Would prefer latitude to regulate military-grade weapons and enforce disarmament measures in genuine emergencies, but under this reading any such effort is pre-framed as a tyranny precursor, making ordinary regulatory action politically and legally fraught. Their voice in defining reasonable regulation is structurally diminished by the reading's own logic, which treats regulatory intent as suspect by default.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, federal_and_state_legislatures, excluded,
    institutional, generational, constrained, national).

% Study the historical, textual, and doctrinal basis for the insurrectionist reading against the militia-conditioned and individual-right readings, without a direct stake in which reading prevails, though their scholarship is frequently cited selectively by advocates on all sides.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_boundary__insurrectionist_reading, armed_citizen_militias).
narrative_ontology:fixing_cost_class(second_amendment_boundary__insurrectionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The reading purports to solve a genuine collective-action problem in political theory: how does a citizenry credibly deter government overreach absent an external enforcer? The claim is that distributed, hard-to-disarm private arms capacity raises the cost of tyranny and thereby coordinates a deterrent that no single citizen could provide alone.
% TRANSFER_FUNCTION: The reading transfers legal and political cover from restrictive gun regulation toward expansive individual arms access, moving risk from armed citizens (who gain expanded rights) to state security personnel, gun violence survivors, and bystanders in any realized conflict (who bear the costs), while moving revenue and legitimacy toward manufacturers and advocacy organizations.
% ABSENT_VOICES: Civilians who would be caught in a hypothetical armed conflict have no seat at the table defining the reading's scope — the scenario the reading requires as its justificatory endpoint is one nobody currently living has been asked to consent to. Gun violence survivors and targeted minority communities are present in public discourse but structurally outvoted by the organized advocacy apparatus that sets litigation and legislative agendas.
% DISAPPEARANCE_RATIONALE: If the insurrectionist reading disappeared overnight (courts and legislatures uniformly adopted the militia-conditioned or narrow individual-right reading instead), armed citizen militias and advocacy organizations would lose their strongest legal argument against weapons bans and disarmament measures, materially reshaping firearms policy litigation; manufacturers would face a narrower protected market for military-pattern weapons. Whether the underlying deterrence function it claims to serve would actually be lost, or was never operative outside rhetoric, is itself the contested question — advocates say the world rearranges catastrophically (tyranny becomes unchecked); critics say nothing changes because the deterrent was never load-bearing.
% FOUNDING_PROBLEM: The reading traces its justification to the founding-era anxiety that a professional standing army under central government control could not be checked by ordinary political processes alone, and that a broadly armed citizenry — organized informally, if necessary, into resistance — was the ultimate backstop against federal tyranny.
% FOUNDING_PROBLEM_CORROBORATION: Insurrectionist advocacy organizations and allied historians attest the problem remains live, citing federal power expansion since the founding era. Constitutional scholars outside the advocacy apparatus are divided: some corroborate the historical anxiety as textually grounded, while others — citing the professionalization of policing and military, the absence of any successful private-arms check on federal action in over two centuries, and the practical unworkability of small-arms resistance against modern state force — argue the founding problem as originally conceived is functionally dead and the reading now serves as post-hoc justification for an unrelated contemporary gun-rights agenda. No corroboration exists from state security apparatus personnel or from communities historically targeted when they attempted to exercise the very resistance right the reading claims to protect.
narrative_ontology:disappearance_verdict(second_amendment_boundary__insurrectionist_reading, contested).
narrative_ontology:founding_problem_status(second_amendment_boundary__insurrectionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_boundary__insurrectionist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored at 0.58 and rising because the reading has drifted from a founding-era theoretical justification toward active legal cover for an expanding category of military-pattern weapons in civilian hands, with real, accumulating costs (mass casualty events, asymmetric vigilante enforcement) that are decoupled from any actual exercise of the deterrence function the reading claims to serve. Suppression is moderate (0.42): the reading does not itself suppress dissenting readings through force, but it does suppress ordinary regulatory action by pre-framing disarmament efforts as tyranny precursors, raising the political cost of any legislature attempting to act on the excluded seat's preferences. Theater ratio (0.40) reflects that a substantial share of the reading's public defense is rhetorical posture about a hypothetical uprising that no living generation has needed to test, rather than functioning deterrent capacity that has ever actually checked state action. Accessibility collapse is authored low-moderate (0.35) because narrower readings remain fully available in courts, scholarship, and legislatures — this reading has not achieved anything like natural-law inevitability; resistance is authored high (0.75) because it is one of the most actively litigated and contested constitutional questions in the country.
 *
 * PERSPECTIVAL GAP:
 *   From the armed-militia and advocacy seats, the reading is a rope: a genuine, hard-won coordination mechanism solving a real collective-action problem (deterring concentrated state power) with the citizenry as net beneficiary. From the state-security-personnel and civilian-bystander seats, the same textual claim computes as extractive and coercive: it licenses an ever-expanding category of lethal capability justified by a scenario in which they are cast as targets, without their consent, in service of a deterrent effect whose actual efficacy has never been empirically tested in two centuries of continuous federal governance. The engine's per-seat computation should recover exactly this asymmetry from the structural declarations above, without the story needing to assert a single verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Armed citizen militias and advocacy organizations sit near the beneficiary end: they collect legitimacy, political capital, and (for manufacturers) revenue from the reading's persistence, and bear none of its worst-case costs personally unless the hypothetical scenario is realized. State security apparatus personnel sit near the full-target end: the reading's own internal logic designates them as the entity to be resisted, and they cannot exit that structural position by mobility or negotiation — it is fixed by the reading's premise, not by their conduct. Civilians in hypothetical conflict zones and gun violence survivors are also target-side but for a different reason: they bear cost not because the reading targets them by design but because the reading's expansive scope generates externalities (mass-casualty risk, conflict exposure) that fall on bystanders regardless of the deterrence theory's validity. Targeted minority communities receive a constrained-exit override consideration: the reading nominally protects their resistance capacity equally, but historical enforcement asymmetry means the protective benefit is unevenly realized while the exposure risk (to both state response and private vigilante violence) is not.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (checking a potentially tyrannical federal government through distributed private arms capacity) is authored as contested rather than resolved either way, precisely because the reading's defenders and critics dispute whether the mechanism ever functioned as claimed or whether it has instead become a standing justification detached from its original condition. Classifying this as tangled_rope rather than snare or mountain preserves that ambiguity: it acknowledges a real, if perhaps never-exercised, coordination function (credible deterrence theory) while also naming the concrete, accumulating extraction (expanded lethal capability, asymmetric enforcement, non-hypothetical casualties) that rides on the same textual claim. Classifying it as a pure mountain would falsely naturalize a contested interpretive choice among three live readings of the same clause; classifying it as a pure snare would deny that the deterrence theory has any genuine, non-cynical constituency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    which_reading_is_textually_correct,
    'Does the Second Amendment''s text and founding-era historical record actually support the insurrectionist reading''s resistance-to-tyranny justification, or is that justification a later ideological gloss added to a narrower original purpose?',
    'Rigorous historical-linguistic analysis of founding-era debates, contemporaneous state constitutions, and ratification-era commentary, cross-checked against how the clause was actually invoked in the first century of use (which was overwhelmingly militia-service context, not individual insurrection).',
    'If the insurrectionist justification is a later ideological construction rather than the founding-era operative understanding, the reading''s coordination claim collapses toward pure cover story, pushing the classification toward snare; if the historical record substantially supports it, the coordination function is more genuinely load-bearing, supporting the tangled_rope reading as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(which_reading_is_textually_correct, empirical, 'Historical accuracy of the insurrectionist justification for the Second Amendment.').

omega_variable(
    deterrence_function_ever_tested,
    'Has the claimed deterrent effect (armed citizenry checks government tyranny) ever actually operated to constrain federal action in American history, or has the deterrence theory been purely rhetorical/untested?',
    'Historical case analysis of instances of expanded federal power (wartime measures, civil rights enforcement, emergency powers) checking whether armed citizen capacity was ever a documented factor in restraining or shaping government decision-making.',
    'If never operative, the coordination function claimed by beneficiaries is speculative rather than demonstrated, weakening the case against pure extraction; if operative, it strengthens the genuine-coordination component of the tangled_rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deterrence_function_ever_tested, empirical, 'Whether the claimed deterrence function has ever been empirically exercised.').

omega_variable(
    kernel_sibling_disagreement_location,
    'The disagreement among the three kernel readings is located specifically at whether the prefatory militia clause is scope-limiting (militia_conditioned) versus merely explanatory (individual_right and insurrectionist) — and then, among the non-limiting readings, whether the purpose served is personal self-defense (individual_right) or resistance capacity against tyranny (insurrectionist). Which of these two loci — clause function, or purpose-if-unlimited — carries more interpretive weight in actual doctrine?',
    'Track how courts actually resolve Second Amendment cases: do they cite militia-clause scope-limiting arguments, or do they adjudicate purpose (self-defense vs. resistance) among readings that already treat the clause as non-limiting?',
    'If courts consistently treat the prefatory clause as non-limiting (as in Heller), the live doctrinal contest is entirely between individual_right_reading and insurrectionist_reading — meaning this story''s sibling that matters most for classification purposes is individual_right_reading, not militia_conditioned_reading, and the extraction profile should be compared against that closer sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_sibling_disagreement_location, conceptual, 'Where the kernel''s live interpretive contest is actually located among the three readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__insurrectionist_reading, 1791, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1791, second_amendment_boundary__insurrectionist_reading, theater_ratio, 1791, 0.1).
narrative_ontology:measurement(seco_tr_t1900, second_amendment_boundary__insurrectionist_reading, theater_ratio, 1900, 0.15).
narrative_ontology:measurement(seco_tr_t1970, second_amendment_boundary__insurrectionist_reading, theater_ratio, 1970, 0.22).
narrative_ontology:measurement(seco_tr_t1994, second_amendment_boundary__insurrectionist_reading, theater_ratio, 1994, 0.28).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_boundary__insurrectionist_reading, theater_ratio, 2008, 0.33).
narrative_ontology:measurement(seco_tr_t2020, second_amendment_boundary__insurrectionist_reading, theater_ratio, 2020, 0.38).
narrative_ontology:measurement(seco_tr_t2026, second_amendment_boundary__insurrectionist_reading, theater_ratio, 2026, 0.4).

% Extraction over time
narrative_ontology:measurement(seco_be_t1791, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 1791, 0.2).
narrative_ontology:measurement(seco_be_t1900, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 1900, 0.25).
narrative_ontology:measurement(seco_be_t1970, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 1970, 0.32).
narrative_ontology:measurement(seco_be_t1994, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 1994, 0.4).
narrative_ontology:measurement(seco_be_t2008, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 2008, 0.48).
narrative_ontology:measurement(seco_be_t2020, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 2020, 0.55).
narrative_ontology:measurement(seco_be_t2026, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 2026, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1791, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 1791, 0.15).
narrative_ontology:measurement(seco_su_t1900, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 1900, 0.18).
narrative_ontology:measurement(seco_su_t1970, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 1970, 0.25).
narrative_ontology:measurement(seco_su_t1994, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 1994, 0.3).
narrative_ontology:measurement(seco_su_t2008, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 2008, 0.35).
narrative_ontology:measurement(seco_su_t2020, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 2020, 0.4).
narrative_ontology:measurement(seco_su_t2026, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 2026, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_boundary__insurrectionist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_boundary__insurrectionist_reading, individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__insurrectionist_reading, militia_conditioned_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language concept 'the Second Amendment' per the ε-invariance principle: individual_right_reading (personal self-defense, no tyranny-resistance requirement), insurrectionist_reading (this story — resistance capacity as the right's deep purpose, extending into military-grade arms), and militia_conditioned_reading (scope bounded to organized collective defense, permitting comprehensive regulation). Each carries a distinct ε, distinct beneficiary/victim sets, and distinct classification; they are linked here rather than merged because measuring 'the Second Amendment' by different observables (personal defense outcomes vs. insurrection-deterrence claims vs. militia-regulation history) yields incompatible ε values — the textbook signal that one label conflates multiple constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_boundary__insurrectionist_reading, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
