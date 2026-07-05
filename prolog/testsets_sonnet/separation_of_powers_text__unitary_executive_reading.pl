% ============================================================================
% CONSTRAINT STORY: separation_of_powers_text__unitary_executive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_separation_of_powers_text__unitary_executive_reading, []).

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
 *   constraint_id: separation_of_powers_text__unitary_executive_reading
 *   human_readable: Unitary Executive Reading of the Vesting Clause (Article II Removal Power)
 *   domain: constitutional_law/political_theory/administrative_law
 *
 * SUMMARY:
 *   This constraint instantiates the unitary executive reading of the
 *   separation-of-powers kernel embedded in Article II's vesting clause: 'The
 *   executive Power shall be vested in a President.' The reading holds that
 *   this clause vests ALL executive power, undivided, in the President
 *   personally, making any statutory insulation of executive officers from
 *   at-will removal (for-cause protections at the FTC, NLRB, and similar
 *   multimember independent commissions) an unconstitutional infringement on
 *   presidential control. Since Humphrey's Executor (1935) permitted
 *   for-cause removal protection for quasi-legislative/quasi-judicial
 *   commissioners, the doctrine has been in retreat through a sequence of
 *   cases (Free Enterprise Fund, Seila Law, Collins v. Yellen) that
 *   progressively narrow the Humphrey's Executor exception. This story
 *   authors that trajectory as accumulating extraction: agencies that were
 *   structurally insulated by design increasingly enter the victim set as the
 *   removal-power doctrine hardens.
 *
 * KEY AGENTS:
 *   - sitting_president: agenda_setter/beneficiary (institutional/arbitrage) — asserts and exercises the removal power claim
 *   - executive_office_of_the_president: beneficiary (institutional/arbitrage) — consolidates cross-agency control
 *   - federal_trade_commission, national_labor_relations_board, federal_reserve_board: payers (institutional/trapped-constrained) — lose designed insulation
 *   - civil_service_career_staff: payer (moderate/constrained) — loses institutional continuity
 *   - congress: excluded (institutional/constrained) — its Article I structural design choices are overridden
 *   - federal_judiciary: observer (institutional/analytical) — adjudicates the doctrine's reach, notably without applying its logic to itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(separation_of_powers_text__unitary_executive_reading, 0.61).
domain_priors:suppression_score(separation_of_powers_text__unitary_executive_reading, 0.52).
domain_priors:theater_ratio(separation_of_powers_text__unitary_executive_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers_text__unitary_executive_reading, tangled_rope).
narrative_ontology:human_readable(separation_of_powers_text__unitary_executive_reading, "Unitary Executive Reading of the Vesting Clause (Article II Removal Power)").
narrative_ontology:topic_domain(separation_of_powers_text__unitary_executive_reading, "constitutional_law/political_theory/administrative_law").

domain_priors:requires_active_enforcement(separation_of_powers_text__unitary_executive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(separation_of_powers_text__unitary_executive_reading, 'ca7aa70d-d729-4765-b45f-ee8232b69369').
narrative_ontology:cs_kernel_codification('ca7aa70d-d729-4765-b45f-ee8232b69369', fixed_text).
narrative_ontology:cs_authority_grounding('ca7aa70d-d729-4765-b45f-ee8232b69369', lineage).
narrative_ontology:cs_interpretation_layer_present('ca7aa70d-d729-4765-b45f-ee8232b69369').
narrative_ontology:cs_reading_relation('ca7aa70d-d729-4765-b45f-ee8232b69369', separation_of_powers_text__formalist_reading, influences).
narrative_ontology:cs_reading_relation('ca7aa70d-d729-4765-b45f-ee8232b69369', separation_of_powers_text__functionalist_reading, forecloses).
narrative_ontology:cs_axiom('ca7aa70d-d729-4765-b45f-ee8232b69369', foundational, executive_power_is_indivisible_and_singular).
narrative_ontology:cs_axiom_status(executive_power_is_indivisible_and_singular, holdable).
narrative_ontology:cs_axiom_grounding('ca7aa70d-d729-4765-b45f-ee8232b69369', executive_power_is_indivisible_and_singular, conventional).
narrative_ontology:cs_axiom('ca7aa70d-d729-4765-b45f-ee8232b69369', foundational, removal_power_is_absolute_incident_of_vesting).
narrative_ontology:cs_axiom_status(removal_power_is_absolute_incident_of_vesting, holdable).
narrative_ontology:cs_axiom_grounding('ca7aa70d-d729-4765-b45f-ee8232b69369', removal_power_is_absolute_incident_of_vesting, conventional).
narrative_ontology:cs_axiom('ca7aa70d-d729-4765-b45f-ee8232b69369', secondary, congressional_insulation_of_officers_is_per_se_invalid).
narrative_ontology:cs_axiom_status(congressional_insulation_of_officers_is_per_se_invalid, holdable).
narrative_ontology:cs_axiom_grounding('ca7aa70d-d729-4765-b45f-ee8232b69369', congressional_insulation_of_officers_is_per_se_invalid, instrumental).
narrative_ontology:cs_reference_frame('ca7aa70d-d729-4765-b45f-ee8232b69369', unitary_executive_original_meaning).
narrative_ontology:cs_drift_state('ca7aa70d-d729-4765-b45f-ee8232b69369', post_seila_law_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('ca7aa70d-d729-4765-b45f-ee8232b69369', '').
narrative_ontology:cs_kernel_id(separation_of_powers_text__unitary_executive_reading, separation_of_powers_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(separation_of_powers_text__unitary_executive_reading, sitting_president).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__unitary_executive_reading, executive_office_of_the_president).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__unitary_executive_reading, presidential_appointees_at_will).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, federal_trade_commission).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, national_labor_relations_board).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, federal_reserve_board).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, civil_service_career_staff).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, regulated_industries_relying_on_agency_independence).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__unitary_executive_reading, regulated_industries_relying_on_agency_independence).
narrative_ontology:constraint_vindicates(separation_of_powers_text__unitary_executive_reading, article_ii_vesting_clause_supremacy).
narrative_ontology:constraint_vindicates(separation_of_powers_text__unitary_executive_reading, presidential_accountability_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts removal power over principal officers of independent agencies as an incident of the Article II vesting clause, litigates to strike for-cause removal protections, and appoints agency heads expecting political alignment. Directly gains centralized control over rulemaking, enforcement priorities, and personnel across the executive branch. Faces essentially no structural check on this claim short of impeachment or judicial reversal.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, sitting_president, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__unitary_executive_reading, sitting_president, beneficiary).

% Coordinates policy across agencies via OMB and OIRA review; the unitary reading eliminates independent agencies as a competing power center, consolidating regulatory and enforcement discretion inside the White House orbit. Institutional continuity across administrations means this seat benefits regardless of which party holds the presidency.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, executive_office_of_the_president, beneficiary,
    institutional, generational, arbitrage, national).

% Structured by Congress with for-cause removal protection for commissioners specifically to insulate antitrust and consumer-protection enforcement from presidential political pressure. Under the unitary reading, commissioners become removable at will, eliminating the insulation Congress designed. The agency cannot exit the constitutional structure it was created inside; its only recourse is litigation or congressional reauthorization, both slow and uncertain.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, federal_trade_commission, payer,
    institutional, generational, trapped, national).

% Adjudicates labor disputes through a quasi-judicial structure meant to be shielded from partisan capture. At-will removal of board members under the unitary reading exposes labor policy adjudication to the same political cycle Congress tried to buffer it from. Board members serve fixed terms with no individual exit from the constitutional exposure.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, national_labor_relations_board, payer,
    institutional, generational, trapped, national).

% Monetary policy independence is widely understood by markets and economists as load-bearing for currency and price stability credibility. The unitary reading's logic, if extended to the Fed, exposes monetary policy to electoral-cycle pressure. The Fed has some informal insulation through market reaction and norm, but no formal constitutional exemption from the doctrine's reach.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, federal_reserve_board, payer,
    institutional, civilizational, constrained, global).

% Career staff within independent agencies rely on the agency's insulated structure to do technical work without being reshuffled or overridden each election cycle. At-will removal of their leadership destabilizes agency continuity and threatens to convert technical staff positions into a shadow patronage layer. Individual staff can leave government but cannot individually alter the doctrine.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, civil_service_career_staff, payer,
    moderate, biographical, constrained, national).

% Some regulated firms benefit from a more responsive, politically pliable regulator under unitary control; others that depend on stable, technocratic, predictable rulemaking (e.g., in monetary policy or utility regulation) are harmed by regulatory whiplash tied to presidential turnover. Larger firms can lobby the White House directly, gaining a new access channel; smaller firms lose the buffered, rule-bound process independent agencies previously offered.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, regulated_industries_relying_on_agency_independence, payer,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__unitary_executive_reading, regulated_industries_relying_on_agency_independence, beneficiary).

% Designed independent agencies with removal protections as an exercise of its own Article I authority to structure the administrative state. The unitary reading treats this design choice as unconstitutional regardless of congressional intent, effectively excluding Congress's structural judgment from the analysis once a court adopts the doctrine. Congress retains the power to legislate but not to insulate what it creates.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, congress, excluded,
    institutional, generational, constrained, national).

% Adjudicates removal-power challenges (Humphrey's Executor, Seila Law, Collins v. Yellen line of cases) and determines how far the unitary reading extends. The judiciary's own institutional independence is arguably the implicit contrast case the unitary reading does not reach — it draws the line for every other branch's insulated actors while its own life tenure remains unquestioned.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, federal_judiciary, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(separation_of_powers_text__unitary_executive_reading, sitting_president).
narrative_ontology:fixing_cost_class(separation_of_powers_text__unitary_executive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, clear line of political accountability: if all executive power runs through one elected official, voters can identify and hold accountable a single person for the conduct of the entire executive branch, rather than diffusing responsibility across insulated agencies no one elected.
% TRANSFER_FUNCTION: Moves personnel control, enforcement discretion, and rulemaking priority-setting from congressionally-designed independent commissions to the President and the executive office, concentrating discretion that Congress had previously distributed to shield technical and adjudicatory functions from electoral-cycle pressure.
% ABSENT_VOICES: Congress, whose Article I structural choices to insulate agencies are treated as constitutionally infirm rather than as a legitimate exercise of its own enumerated powers, has no seat in the doctrinal contest once a court adopts this reading — the doctrine is adjudicated by the judiciary and asserted by the executive, with the legislative branch's original design intent relegated to a losing argument rather than a live voice.
% DISAPPEARANCE_RATIONALE: If the unitary executive reading were abandoned wholesale, for-cause removal protections for FTC, NLRB, and similar agency heads would be restored to full force, agency leadership would regain functional independence from White House political pressure, and decades of agency structuring statutes would return to their originally-designed operation without needing to be rewritten.
% FOUNDING_PROBLEM: The reading was advanced to solve a claimed accountability deficit: agencies exercising significant coercive and regulatory power while insulated from removal by anyone the public elected, creating a 'headless fourth branch' problem.
% FOUNDING_PROBLEM_CORROBORATION: Executive branch officials and unitary-executive legal scholars (many housed in or funded by executive-power-favoring institutions) attest the accountability deficit is real and unaddressed. Independent corroboration is thinner: some constitutional scholars outside the movement (across the political spectrum) argue the accountability problem is overstated because Congress, courts, and elections already constrain agencies, and that the doctrine's growth tracks executive branch litigation strategy more than a settled constitutional consensus — the founding-problem narrative is substantially self-asserted by the reading's principal beneficiaries.
narrative_ontology:disappearance_verdict(separation_of_powers_text__unitary_executive_reading, world_rearranges).
narrative_ontology:founding_problem_status(separation_of_powers_text__unitary_executive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(separation_of_powers_text__unitary_executive_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(separation_of_powers_text__unitary_executive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(separation_of_powers_text__unitary_executive_reading, 0.61, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(separation_of_powers_text__unitary_executive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(separation_of_powers_text__unitary_executive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(separation_of_powers_text__unitary_executive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored as substantial and rising (0.15 to 0.61 across the interval) because the doctrine's practical effect has grown from a narrow carve-out debate in 1935 to an active, litigated mechanism progressively stripping insulation from a widening set of agencies through 2024. Suppression is authored as moderate-to-high (0.52 by the end) and structural: the doctrine requires active judicial enforcement (a sequence of Supreme Court decisions) to displace nearly ninety years of contrary precedent and congressional design, and it forecloses Congress's own structural choices going forward. Theater ratio is kept comparatively low (0.28) because the doctrine's operation is substantive, not performative — actual commissioners are actually removed or exposed to removal, actual agency independence is actually reduced. Accessibility collapse is moderate (0.45): the formalist and functionalist readings remain live alternatives that courts, scholars, and future majorities could adopt instead, so alternatives have not fully collapsed the way a genuine natural-law mountain would collapse them. Resistance is authored high (0.72): the doctrine faces sustained pushback from constitutional scholars, from the excluded institution (Congress) whose statutory designs are invalidated, and from the very independent agencies whose functions it dismantles.
 *
 * PERSPECTIVAL GAP:
 *   From the presidential/executive-office seat, this reading is a rope: it restores accountability that was lost when Congress built headless, unelected fourth-branch power centers, and every citizen benefits from being able to vote out the person now responsible for the whole executive branch. From the independent-agency seat, the same doctrine computes as a tangled rope shading toward snare: the coordination story (accountability) is real but thin, while the extraction (loss of designed insulation, exposure of technical and adjudicatory functions to electoral-cycle political pressure) is substantial and falls specifically on institutions Congress built for the opposite purpose. The engine's per-seat computation should show this divergence directly from the declared power/exit/beneficiary-victim structure, without either seat's framing being privileged in the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   The sitting president and the executive office are the clear structural beneficiaries: they gain concentrated control over personnel and enforcement discretion that Congress had previously distributed elsewhere, and their exit options are best described as arbitrage — the presidency can always assert the doctrine's benefits regardless of which party occupies it, because the doctrine transfers power to the OFFICE, not to any individual officeholder. The named independent agencies (FTC, NLRB, Federal Reserve) are the structural targets: they were affirmatively designed by Congress to be insulated from exactly this kind of removal pressure, and the unitary reading directly reverses that design, so their directionality sits near the full-target end. Civil service staff are moderate targets: not personally removable at will in most cases, but institutionally destabilized when their agency's leadership becomes politically contingent. Regulated industries split — some gain a new lobbying channel to a more responsive executive, others lose the predictable, buffered regulatory environment independent agencies provided, which is why that stakeholder carries both a payer and beneficiary role.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a genuine 'who is accountable for this agency' accountability gap — was real when raised, but its status is contested rather than settled dead or clearly still live: Congress, courts, and periodic elections already provide some accountability channels for agency conduct, and the doctrine's growth over the interval tracks executive-branch litigation strategy at least as much as it tracks a demonstrated failure of those channels. Classifying this as tangled_rope rather than snare or mountain prevents two mislabelings: calling it a snare would ignore the genuine (if contested) accountability coordination function it invokes; calling it a mountain — as unitary-executive advocates sometimes frame the vesting clause's 'plain text' — would launder a contested, actively-litigated doctrinal choice as settled constitutional physics, which the false-summit check exists to catch. The declared beneficiary/victim structure, active judicial enforcement requirement, and rising extraction trajectory jointly support tangled_rope over either pole.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vesting_clause_scope_ambiguity,
    'Does ''The executive Power shall be vested in a President'' textually and originally mean ALL executive power without qualification (the unitary reading), or does it establish only a default/residual allocation compatible with congressionally-created exceptions (the formalist and functionalist readings'' shared premise that some structural flexibility exists)?',
    'Originalist historical evidence about founding-era understanding of removal power (the Decision of 1789 debates), combined with the trajectory of Supreme Court doctrine from Humphrey''s Executor through Seila Law and any future case that either affirms or rejects extending the doctrine to multimember agencies and the Federal Reserve.',
    'If the vesting clause is resolved as textually and originally establishing an unqualified unitary executive, this reading forecloses the formalist and functionalist readings'' shared assumption that Congress may permissibly structure some insulation — the sibling readings would need to be substantially revised or would become minority positions. If resolved the other way, this reading''s foundational axiom is undermined and its extraction is exposed as doctrinal innovation rather than restored original meaning.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(vesting_clause_scope_ambiguity, conceptual, 'Whether the vesting clause textually supports an unqualified or a qualified reading of presidential executive power.').

omega_variable(
    accountability_versus_capture_tradeoff,
    'Does concentrating removal power in the President genuinely increase democratic accountability, or does it primarily increase presidential capture of technical and adjudicatory functions that were insulated precisely because direct political accountability was judged undesirable for those functions (e.g., monetary policy, antitrust adjudication)?',
    'Comparative institutional analysis of agency performance and political-cycle correlation in jurisdictions/periods with and without at-will removal for equivalent functions; economic studies of central bank independence and inflation/interest-rate outcomes under varying degrees of political insulation.',
    'If accountability gains are shown to be real and capture costs modest, this reading''s coordination function is stronger than authored here and the classification should shift toward rope. If capture costs are shown to dominate with accountability gains largely rhetorical, the classification should shift further toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accountability_versus_capture_tradeoff, empirical, 'Whether the doctrine''s accountability rationale outweighs its capture and destabilization costs in practice.').

omega_variable(
    kernel_framing_under_determination,
    'Is the correct unit of analysis ''the vesting clause standing alone'' (which favors the unitary reading''s textualist emphasis) or ''the vesting clause read against the whole structural Constitution including Article I''s necessary-and-proper clause and the historical practice of independent agencies since 1887'' (which favors the functionalist reading)? The choice of interpretive frame substantially predetermines which reading looks textually compelled.',
    'This is a live methodological dispute in constitutional interpretation theory (textualism/originalism vs. structuralism/living constitutionalism) with no external empirical resolution mechanism; it can only be tracked via which framing successive Supreme Court majorities adopt.',
    'Choosing the narrower framing (clause-in-isolation) supports authoring higher confidence in this reading''s textual claim; choosing the broader framing (whole-structure) would support authoring this reading as more contested and doctrinally aggressive than its own rhetoric concedes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_under_determination, conceptual, 'Alternative interpretive framings of the vesting clause yield different assessments of how textually compelled the unitary reading is.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers_text__unitary_executive_reading, 1935, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sepa_tr_t1935, separation_of_powers_text__unitary_executive_reading, theater_ratio, 1935, 0.1).
narrative_ontology:measurement_basis(sepa_tr_t1935, observed).
narrative_ontology:measurement(sepa_tr_t1980, separation_of_powers_text__unitary_executive_reading, theater_ratio, 1980, 0.13).
narrative_ontology:measurement_basis(sepa_tr_t1980, observed).
narrative_ontology:measurement(sepa_tr_t2000, separation_of_powers_text__unitary_executive_reading, theater_ratio, 2000, 0.16).
narrative_ontology:measurement_basis(sepa_tr_t2000, observed).
narrative_ontology:measurement(sepa_tr_t2010, separation_of_powers_text__unitary_executive_reading, theater_ratio, 2010, 0.2).
narrative_ontology:measurement_basis(sepa_tr_t2010, observed).
narrative_ontology:measurement(sepa_tr_t2020, separation_of_powers_text__unitary_executive_reading, theater_ratio, 2020, 0.24).
narrative_ontology:measurement_basis(sepa_tr_t2020, observed).
narrative_ontology:measurement(sepa_tr_t2024, separation_of_powers_text__unitary_executive_reading, theater_ratio, 2024, 0.28).
narrative_ontology:measurement_basis(sepa_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(sepa_be_t1935, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 1935, 0.15).
narrative_ontology:measurement_basis(sepa_be_t1935, observed).
narrative_ontology:measurement(sepa_be_t1980, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 1980, 0.22).
narrative_ontology:measurement_basis(sepa_be_t1980, observed).
narrative_ontology:measurement(sepa_be_t2000, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 2000, 0.3).
narrative_ontology:measurement_basis(sepa_be_t2000, observed).
narrative_ontology:measurement(sepa_be_t2010, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 2010, 0.38).
narrative_ontology:measurement_basis(sepa_be_t2010, observed).
narrative_ontology:measurement(sepa_be_t2020, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 2020, 0.53).
narrative_ontology:measurement_basis(sepa_be_t2020, observed).
narrative_ontology:measurement(sepa_be_t2024, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 2024, 0.61).
narrative_ontology:measurement_basis(sepa_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(sepa_su_t1935, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 1935, 0.2).
narrative_ontology:measurement_basis(sepa_su_t1935, observed).
narrative_ontology:measurement(sepa_su_t1980, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 1980, 0.25).
narrative_ontology:measurement_basis(sepa_su_t1980, observed).
narrative_ontology:measurement(sepa_su_t2000, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 2000, 0.3).
narrative_ontology:measurement_basis(sepa_su_t2000, observed).
narrative_ontology:measurement(sepa_su_t2010, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 2010, 0.36).
narrative_ontology:measurement_basis(sepa_su_t2010, observed).
narrative_ontology:measurement(sepa_su_t2020, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 2020, 0.46).
narrative_ontology:measurement_basis(sepa_su_t2020, observed).
narrative_ontology:measurement(sepa_su_t2024, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 2024, 0.52).
narrative_ontology:measurement_basis(sepa_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(separation_of_powers_text__unitary_executive_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(separation_of_powers_text__unitary_executive_reading, 0.12).
narrative_ontology:affects_constraint(separation_of_powers_text__unitary_executive_reading, formalist_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__unitary_executive_reading, functionalist_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the separation_of_powers_text kernel. formalist_reading emphasizes non-delegation (Congress cannot hand legislative power to agencies); functionalist_reading treats the framework as flexible and delegation-permissive; unitary_executive_reading (this story) uniquely places independent agencies themselves into the victim set via an absolute removal-power claim. The three stories share no single ε — each has its own beneficiary/victim structure and its own extraction trajectory, per the ε-invariance principle. They are linked via network edges rather than merged into one constraint with an observable parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
