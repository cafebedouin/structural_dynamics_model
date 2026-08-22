% ============================================================================
% CONSTRAINT STORY: us_constitution_interpretive__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_interpretive__originalist_reading, []).

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
 *   constraint_id: us_constitution_interpretive__originalist_reading
 *   human_readable: Originalist Reading of Constitutional Interpretation: Meaning Fixed at Ratification
 *   domain: legal/political
 *
 * SUMMARY:
 *   Within the contested kernel of US constitutional interpretation, this
 *   story instantiates the originalist reading: constitutional meaning was
 *   fixed at ratification, and interpretive authority derives from fidelity
 *   to the framers' intent or the original public meaning of the enacted
 *   text. As a standing arrangement, the reading disciplines judicial method
 *   — judges may not update meaning to match contemporary values; change runs
 *   through Article V. The arrangement coordinates (shared discoverable
 *   meaning, constrained discretion, stabilized expectations across
 *   generations) while extracting asymmetrically through the same structure:
 *   claimants whose interests lack founding-era anchors bear the cost of the
 *   freeze, and advocates of federal regulatory capacity lose instruments,
 *   while states, property holders, and traditionally-scoped
 *   religious-liberty claimants collect the protection. Per the
 *   epsilon-invariance principle, the colloquial label 'how to read the
 *   Constitution' decomposes into three structurally distinct constraints —
 *   this file, the living-constitution reading, and the
 *   popular-constitutionalism reading — each with its own epsilon,
 *   beneficiary/victim structure, and classification, linked through
 *   network.affects_constraints. Claimed type and metrics are authored
 *   independently: I claim tangled_rope as structurally true (genuine
 *   coordination function plus identifiable extraction through the same
 *   structure, actively enforced); the metrics describe the arrangement's
 *   actual operation as the record shows it.
 *
 * KEY AGENTS:
 *   - scotus_originalist_majority: agenda-setter (institutional/arbitrage) — polices method, controls the docket, retires precedent built on other methods
 *   - judicial_appointment_pipeline: agenda-setter (institutional/mobile) — presidents, senators, and screening networks that manufacture the enforcing bench
 *   - state_governments: primary beneficiary (institutional/constrained) — receive reserved powers and returned regulatory authority
 *   - federalism_advocates: beneficiary (organized/mobile) — accrue doctrinal and litigation victories for reserved-powers readings
 *   - property_rights_defenders: beneficiary (organized/constrained) — collect takings and major-questions protections
 *   - religious_liberty_claimants_original_scope: beneficiary (moderate/constrained) — win accommodations within founding-era scope
 *   - unenumerated_rights_claimants: primary target (powerless/trapped) — bear the freeze; remedies depend on a method they cannot select
 *   - federal_regulatory_expansion_advocates: target (institutional/constrained) — lose regulatory instruments to nondelegation and major-questions policing
 *   - ratification_excluded_populations: excluded (powerless/trapped) — descendants of those barred from ratification, governed by a text they had no hand in making
 *   - constitutional_theorists: analytical observer — maps the method contest across all seats
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_interpretive__originalist_reading, 0.62).
domain_priors:suppression_score(us_constitution_interpretive__originalist_reading, 0.7).
domain_priors:theater_ratio(us_constitution_interpretive__originalist_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_interpretive__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_interpretive__originalist_reading, "Originalist Reading of Constitutional Interpretation: Meaning Fixed at Ratification").
narrative_ontology:topic_domain(us_constitution_interpretive__originalist_reading, "legal/political").

domain_priors:requires_active_enforcement(us_constitution_interpretive__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_interpretive__originalist_reading, '2e9ffdb4-fe98-4acb-84a2-c51fbc2c4f9b').
narrative_ontology:cs_kernel_codification('2e9ffdb4-fe98-4acb-84a2-c51fbc2c4f9b', fixed_text).
narrative_ontology:cs_authority_grounding('2e9ffdb4-fe98-4acb-84a2-c51fbc2c4f9b', lineage).
narrative_ontology:cs_interpretation_layer_present('2e9ffdb4-fe98-4acb-84a2-c51fbc2c4f9b').
narrative_ontology:cs_reading_relation('2e9ffdb4-fe98-4acb-84a2-c51fbc2c4f9b', us_constitution_interpretive__living_constitution_reading, forecloses).
narrative_ontology:cs_reading_relation('2e9ffdb4-fe98-4acb-84a2-c51fbc2c4f9b', us_constitution_interpretive__popular_constitutionalism_reading, coexists_with).
narrative_ontology:cs_axiom('2e9ffdb4-fe98-4acb-84a2-c51fbc2c4f9b', foundational, constitutional_meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(constitutional_meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('2e9ffdb4-fe98-4acb-84a2-c51fbc2c4f9b', constitutional_meaning_fixed_at_ratification, conventional).
narrative_ontology:cs_axiom('2e9ffdb4-fe98-4acb-84a2-c51fbc2c4f9b', secondary, article_v_exclusive_change_channel).
narrative_ontology:cs_axiom_status(article_v_exclusive_change_channel, holdable).
narrative_ontology:cs_axiom_grounding('2e9ffdb4-fe98-4acb-84a2-c51fbc2c4f9b', article_v_exclusive_change_channel, conventional).
narrative_ontology:cs_reference_frame('2e9ffdb4-fe98-4acb-84a2-c51fbc2c4f9b', ratification_era_public_meaning).
narrative_ontology:cs_drift_state('2e9ffdb4-fe98-4acb-84a2-c51fbc2c4f9b', contemporary_post_bruen_dobbs_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('2e9ffdb4-fe98-4acb-84a2-c51fbc2c4f9b', '').
narrative_ontology:cs_kernel_id(us_constitution_interpretive__originalist_reading, us_constitution_interpretive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, state_governments).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, federalism_advocates).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, property_rights_defenders).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, religious_liberty_claimants_original_scope).
narrative_ontology:constraint_victim(us_constitution_interpretive__originalist_reading, unenumerated_rights_claimants).
narrative_ontology:constraint_victim(us_constitution_interpretive__originalist_reading, federal_regulatory_expansion_advocates).
narrative_ontology:constraint_vindicates(us_constitution_interpretive__originalist_reading, enumerated_powers_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_interpretive__originalist_reading, written_constitution_as_higher_law).
narrative_ontology:constraint_vindicates(us_constitution_interpretive__originalist_reading, article_v_amendment_exclusivity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds a working majority committed to deciding cases by the enacted text's original public meaning. Selects the docket, writes the opinions that retire precedent built on other methods, and polices lower-court adherence. Its members reached the bench through a selection process that screened for this method; switching methods would cost them standing with the coalition that elevated them.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, scotus_originalist_majority, agenda_setter,
    institutional, generational, arbitrage, national).

% Presidents, Senate actors, and allied screening organizations identify, vet, and confirm candidates committed to the founding-era method. Maintains the feeder networks — clerkships, appellate slots, academic credentials — through which compliant candidates advance. Its leverage is upstream: it does not decide cases but determines who does.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, judicial_appointment_pipeline, agenda_setter,
    institutional, generational, mobile, national).

% Receive and defend the authority the arrangement returns to them: criminal justice, family law, election administration, and regulatory space the federal government cannot occupy under the 1787 grant. Litigate to expand reserved-power holdings and against federal mandates. They cannot exit the union; their recourse is litigation and interstate compacts.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, state_governments, beneficiary,
    institutional, generational, constrained, regional).

% State attorneys general, think tanks, and scholars who advance reserved-powers and enumerated-powers readings. Collect doctrinal wins, amicus influence, and funding as the method gains ground. Their alternative — advocating constitutional amendment to relocate power — is available but rarely pursued.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, federalism_advocates, beneficiary,
    organized, biographical, mobile, national).

% Landowners, developers, and industry litigants who invoke founding-era property protections against regulation. Collect compensation requirements, heightened scrutiny of regulatory action, and invalidation of novel regulatory schemes. Exit would mean forgoing development or relocating; they operate inside the system.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, property_rights_defenders, beneficiary,
    organized, biographical, constrained, national).

% Believers and institutions whose free-exercise and non-establishment claims fit the founding-era settlement — exemptions from generally applicable burdens, public funding parity, historical symbols and practices. Win accommodations at growing rates. Claims requiring novel doctrine — minority practices unknown to the founders, new equality frameworks — fare less well under the same method.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, religious_liberty_claimants_original_scope, beneficiary,
    moderate, biographical, constrained, national).

% Persons asserting dignitary, privacy, bodily-autonomy, and equality interests that find no anchor in the 1791 text as understood at ratification. Their claims fail or succeed depending on a method chosen by others; their remedies run through Article V, which requires supermajorities they cannot assemble. Exit means leaving the jurisdiction's legal order entirely.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, unenumerated_rights_claimants, payer,
    powerless, biographical, trapped, national).

% Agencies, congressional drafters, and policy coalitions seeking national solutions to problems the 1787 grant did not name — climate, public health emergencies, platform economy. Nondelegation, major-questions, and enumerated-powers policing narrow their instruments case by case. They retain considerable resources and redesign around the narrowed channels, but each redesign is costlier than the last.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, federal_regulatory_expansion_advocates, payer,
    institutional, generational, constrained, national).

% Descendants of the people barred from the founding franchise — women, enslaved and free Black Americans, Indigenous nations, men without property. They are governed by meanings fixed by a document they had no hand in ratifying, and the interpretive conversation privileges founding-era sources written by those who excluded them. They appear in the process only as litigants, never as constituents of the fixed meaning.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, ratification_excluded_populations, excluded,
    powerless, generational, trapped, national).

% Academics and commentators across all methods who map the contest, produce the histories and linguistic analyses both sides deploy, and train the next bench and bar. Neither collects nor pays directly; their output is the evidentiary substrate every seat argues from.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, constitutional_theorists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_interpretive__originalist_reading, state_governments).
narrative_ontology:fixing_cost_class(us_constitution_interpretive__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of unconstrained judicial discretion over the supreme law: fixes a shared, discoverable meaning so citizens, legislators, and courts can know what the Constitution permits without deferring to judges' moral preferences; stabilizes legal expectations across generations; and routes constitutional change through the amendment process rather than litigation.
% TRANSFER_FUNCTION: Moves interpretive authority from sitting judges and contemporary majorities to the ratifying generation's understanding; moves constitutional protection away from claims lacking founding-era anchors toward claims within original scope; moves governance authority from federal institutions back to the states where the 1787 grant is silent.
% ABSENT_VOICES: Those excluded from the founding franchise — women, enslaved and free Black Americans, Indigenous nations, non-propertied men — never consented to the fixed text, and their descendants enter the process only as litigants; unenumerated-rights claimants whose harms were unacknowledged in 1791 likewise have no seat at the method's table. They are absent from the ratification compact and from the source base (founding-era writings by the enfranchised few) that the method treats as authoritative.
% DISAPPEARANCE_RATIONALE: Interpretive authority would redistribute overnight to sitting judges' reasoned judgment or to mobilized publics; federalism boundaries would loosen as federal institutions reclaim dormant powers; unenumerated-rights claims would gain immediate traction; the appointment wars would lose their defining stakes; and Article V would cease to be the sole live channel of constitutional change.
% FOUNDING_PROBLEM: Built to solve the counter-majoritarian difficulty exposed by mid-twentieth-century judicial expansion: a method that binds judges to enacted law rather than personal values, restores democratic control over constitutional change, and reverses precedents read as judicial overreach — with a deeper genealogy in fidelity to the written Constitution as higher-law constraint on faction.
% FOUNDING_PROBLEM_CORROBORATION: Sources outside the benefiting parties corroborate the historical trigger while disputing its normative weight: political scientists across methodologies document the Warren Court expansion and the ensuing backlash and court-curbing proposals; critical legal scholars and living-constitution proponents attest the grievance was sincerely held even as they deny that unconstrained adaptation is a defect. No source outside the benefiting parties attests that the founding problem is resolved; several attest the problem statement itself was constructed to serve the coalition that proposed the cure.
narrative_ontology:disappearance_verdict(us_constitution_interpretive__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_interpretive__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_interpretive__originalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_interpretive__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_interpretive__originalist_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_interpretive__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_interpretive__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_interpretive__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (epsilon 0.62) reflects a real but bounded transfer: the freeze denies remedies to identifiable claimant classes and strips federal regulatory tools, yet the same structure delivers a functioning coordination good — determinate supreme law — that even affected parties invoke. Suppression (0.70) is the highest metric because persistence depends on actively staffing the bench and policing method: the arrangement survives through appointment machinery and professional gatekeeping, not spontaneous assent; alternatives (the sibling readings) remain legally arguable and are argued daily, which caps suppression below levels seen where exits are physically closed. Theater (0.38) captures the documented growth of law-office history — motivated historicism deployed to reach preferred outcomes — alongside a still-functional core of archival and linguistic analysis. Accessibility collapse (0.40) is low-to-moderate: understanding the originalist rule does not collapse the alternatives, since rival methods remain professionally livable. Resistance (0.65) is high and organized: dissenting opinions, cross-methodological scholarship, court-curbing proposals, and confirmation opposition. All three series run on one shared grid (t=0..50, mapped to 1975-2025); the rising suppression_requirement series models the enforcement ratchet — confirmation hardening and screening-network maturation — rather than extraction drift alone. Receipt concentrates on state_governments: the arrangement's operation returns governance authority to the states (post-Dobbs regulatory return, Bruen aftermath, major-questions spillover), making them the demonstrable net recipient; other beneficiaries accrue shares incidentally. Fixing is prohibitive: the Court could shift method only by repudiating its own recent doctrinal program against an entrenched appointment pipeline, precedent mass, and scholarly apparatus, and Article V repair is unreachable at current consensus thresholds.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seats the arrangement is rule-of-law restoration: judges bound to law rather than values, democracy protected from life-tenured adaptation. From the trapped payer seats the identical structure operates as lockout: their claims fail not on merit but on vintage. Same-level divergence is sharpest inside the judiciary — justices of equal institutional power split by method commitment, and the split behaves like professional identity fusion: careers, coalitions, and reputations are constituted through method allegiance, so exit (method-switching) carries identity cost beyond its doctrinal cost. Inter-institutionally, state governments experience the arrangement as subsidy (returned authority) while federal agencies experience it as enclosure (narrowed instruments), though both sit at institutional power with formally similar exit menus — the difference is position relative to the freeze, not rank.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (states, federalism advocates, property defenders, in-scope religious claimants) derive low d — the arrangement subsidizes them; victims (unenumerated claimants, regulatory-expansion advocates) derive high d — it extracts from them; national spatial scope amplifies effective extraction modestly, since verification of founding meaning is contested at scale. The excluded seat (ratification-excluded populations) sits outside the derivation entirely: they neither collect nor administer, which is precisely the structural complaint — the apparent unanimity of founding-era sources reflects who was allowed in the room. No directionality overrides are authored: the beneficiary/victim declarations plus exit options produce the correct d for every seated agent.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — constraining judicial discretion after mid-century expansion — is contested, not dead: the counter-majoritarian worry remains a live dispute, so no mandatrophy declaration is authored. The classification guards both failure modes: calling the arrangement pure extraction would erase the genuine coordination good (determinate law) that payers themselves invoke; calling it pure coordination would erase the identifiable classes bearing the freeze. Watch item: if the counter-majoritarian concern dissolves — court-curbing succeeds, or the method completes its doctrinal program — the enforcement machinery could outlive its justification; the rising theater_ratio series is the early indicator, and a dead-problem-plus-world-rearranges mismatch would flag it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the us_constitution_interpretive kernel; how would the beneficiary/victim structure and classification change under the sibling readings?',
    'Generate the sibling stories (living_constitution_reading, popular_constitutionalism_reading) and compare computed per-seat classifications across the family; divergence localizes where the readings actually disagree.',
    'Under the living-constitution reading the victim set inverts (incumbents protected by the freeze become targets of adaptation); under popular constitutionalism the agenda-setter seat migrates from courts to mobilized publics. Cross-family comparison is the only route to establishing which reading''s extraction picture governs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame omega: this story is one reading of a contested kernel; sibling readings instantiate different constraints.').

omega_variable(
    intent_vs_public_meaning,
    'Do framers'' subjective intent and original public meaning diverge on outcome-determinative provisions (Ninth Amendment, Establishment Clause, recess appointments), and which variant does enforcing practice actually track?',
    'Founding-era corpora linguistics and archival analysis compared against decided cases; divergence rates between the two variants measured on a common case set.',
    'If the variants diverge widely, the constraint''s epsilon is unstable within the reading itself — enforcement picks winners between rival originalisms, raising theater and extraction measures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intent_vs_public_meaning, empirical, 'Internal ambiguity between subjective-intent and public-meaning originalism.').

omega_variable(
    dead_hand_authority,
    'May the ratifying generation legitimately bind the present, including those its franchise excluded?',
    'Not resolvable by data — a values question about intergenerational obligation and consent, resolvable only by persuasion or constitutional rupture.',
    'If the dead-hand premise loses legitimacy, the constraint''s authority foundation collapses for objectors and its effective extraction rises sharply from their seats; if affirmed, the extraction reads as the price of written-law stability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dead_hand_authority, preference, 'Irreducible normative dispute over intergenerational binding authority.').

omega_variable(
    brown_consistency_question,
    'Does the originalist method genuinely validate the civil-rights canon (Brown v. Board above all) on founding evidence, or does upholding it require the very updating the method forbids?',
    'Historiographic audit of originalist defenses of Brown (Fourteenth Amendment original-scope studies) against mainstream Reconstruction-era historiography.',
    'If the canon requires exception, the constraint''s operation is selectively enforced — a signature of extraction wearing method as cover — raising theater_ratio and supporting downgrade toward pure extraction at objecting seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(brown_consistency_question, empirical, 'Whether landmark equality outcomes are derivable within the reading''s own method.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression primarily structural (appointment gatekeeping, confirmation politics, career incentives) or internalized (professional identity fusion with method coalitions that would persist if gatekeeping weakened)?',
    'Track method compliance among judges and scholars who reached their positions outside the gatekeeping network; if compliance holds where structural pressure is absent, the internalized share is large.',
    'If internalized, suppression persists after institutional change — the arrangement would survive court-curbing or pipeline reform longer than structural measures predict, dating any decay later.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural versus internalized enforcement of interpretive method.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_interpretive__originalist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_interpretive__originalist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(us_c_tr_t0, observed).
narrative_ontology:measurement(us_c_tr_t10, us_constitution_interpretive__originalist_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement_basis(us_c_tr_t10, observed).
narrative_ontology:measurement(us_c_tr_t20, us_constitution_interpretive__originalist_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement_basis(us_c_tr_t20, observed).
narrative_ontology:measurement(us_c_tr_t30, us_constitution_interpretive__originalist_reading, theater_ratio, 30, 0.27).
narrative_ontology:measurement_basis(us_c_tr_t30, observed).
narrative_ontology:measurement(us_c_tr_t40, us_constitution_interpretive__originalist_reading, theater_ratio, 40, 0.32).
narrative_ontology:measurement_basis(us_c_tr_t40, observed).
narrative_ontology:measurement(us_c_tr_t50, us_constitution_interpretive__originalist_reading, theater_ratio, 50, 0.38).
narrative_ontology:measurement_basis(us_c_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_interpretive__originalist_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(us_c_be_t0, observed).
narrative_ontology:measurement(us_c_be_t10, us_constitution_interpretive__originalist_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement_basis(us_c_be_t10, observed).
narrative_ontology:measurement(us_c_be_t20, us_constitution_interpretive__originalist_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement_basis(us_c_be_t20, observed).
narrative_ontology:measurement(us_c_be_t30, us_constitution_interpretive__originalist_reading, base_extractiveness, 30, 0.48).
narrative_ontology:measurement_basis(us_c_be_t30, observed).
narrative_ontology:measurement(us_c_be_t40, us_constitution_interpretive__originalist_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement_basis(us_c_be_t40, observed).
narrative_ontology:measurement(us_c_be_t50, us_constitution_interpretive__originalist_reading, base_extractiveness, 50, 0.62).
narrative_ontology:measurement_basis(us_c_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_interpretive__originalist_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(us_c_su_t0, observed).
narrative_ontology:measurement(us_c_su_t10, us_constitution_interpretive__originalist_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement_basis(us_c_su_t10, observed).
narrative_ontology:measurement(us_c_su_t20, us_constitution_interpretive__originalist_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement_basis(us_c_su_t20, observed).
narrative_ontology:measurement(us_c_su_t30, us_constitution_interpretive__originalist_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement_basis(us_c_su_t30, observed).
narrative_ontology:measurement(us_c_su_t40, us_constitution_interpretive__originalist_reading, suppression_requirement, 40, 0.63).
narrative_ontology:measurement_basis(us_c_su_t40, observed).
narrative_ontology:measurement(us_c_su_t50, us_constitution_interpretive__originalist_reading, suppression_requirement, 50, 0.7).
narrative_ontology:measurement_basis(us_c_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_interpretive__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_interpretive__originalist_reading, us_constitution_interpretive__living_constitution_reading).
narrative_ontology:affects_constraint(us_constitution_interpretive__originalist_reading, us_constitution_interpretive__popular_constitutionalism_reading).

% DUAL FORMULATION NOTE:
% The colloquial concept 'constitutional interpretation' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints — one per reading of the us_constitution_interpretive kernel. Each sibling carries its own epsilon, beneficiary/victim structure, and claimed type: the living-constitution reading inverts this file's victim set (incumbents protected by the freeze become targets of adaptation); the popular-constitutionalism reading relocates the agenda-setter seat from courts to mobilized publics. This file links both siblings; the family is complete only when all three exist. At the constraint level this reading's institutional ascendancy changes the legitimacy conditions and resource availability under which the siblings operate; the sibling files should register corresponding edges back toward this one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
