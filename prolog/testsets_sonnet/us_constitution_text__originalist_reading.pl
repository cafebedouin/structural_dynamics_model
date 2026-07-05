% ============================================================================
% CONSTRAINT STORY: us_constitution_text__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_text__originalist_reading, []).

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
 *   constraint_id: us_constitution_text__originalist_reading
 *   human_readable: Originalist Reading of the U.S. Constitution (Original Public Meaning)
 *   domain: legal/political/interpretive_theory
 *
 * SUMMARY:
 *   This constraint models originalism as the operative interpretive
 *   commitment within a specific, ascendant institutional coalition — not as
 *   an abstract theory but as a methodology now embedded in federal judicial
 *   selection, briefing practice, and precedent. Over the measured interval
 *   (roughly 1980s emergence to present institutional dominance) the
 *   methodology moved from academic minority position to controlling
 *   framework in significant doctrinal areas, tracked here by rising
 *   extractiveness (doctrinal claims outside the historical-tradition frame
 *   increasingly fail) and rising suppression (the methodology increasingly
 *   forecloses alternative interpretive moves rather than merely competing
 *   with them). This is ONE reading of the constitution-as-kernel; the
 *   living-constitutionalist and positivist readings are separate constraint
 *   stories with their own ε values, not alternative measurements of this
 *   one.
 *
 * KEY AGENTS:
 *   - conservative_legal_movement: agenda_setter/beneficiary (institutional/arbitrage) — administers the methodology, collects institutional dominance
 *   - unenumerated_rights_claimants: primary target (powerless/trapped) — bears extraction when no historical analogue exists
 *   - federalist_society_pipeline_judges: beneficiary (institutional/arbitrage) — career and legitimacy benefit from consistent application
 *   - marginalized_groups_absent_from_18th_century_franchise: excluded (powerless/trapped) — structurally cannot generate the evidence the method requires
 *   - constitutional_law_scholars: analytical observer — assesses coherence and outcome correlation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_text__originalist_reading, 0.61).
domain_priors:suppression_score(us_constitution_text__originalist_reading, 0.78).
domain_priors:theater_ratio(us_constitution_text__originalist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_text__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_text__originalist_reading, "Originalist Reading of the U.S. Constitution (Original Public Meaning)").
narrative_ontology:topic_domain(us_constitution_text__originalist_reading, "legal/political/interpretive_theory").

domain_priors:requires_active_enforcement(us_constitution_text__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_text__originalist_reading, '9a9c7360-cb25-4198-b7e9-1c4d06c11e18').
narrative_ontology:cs_kernel_codification('9a9c7360-cb25-4198-b7e9-1c4d06c11e18', fixed_text).
narrative_ontology:cs_authority_grounding('9a9c7360-cb25-4198-b7e9-1c4d06c11e18', lineage).
narrative_ontology:cs_interpretation_layer_present('9a9c7360-cb25-4198-b7e9-1c4d06c11e18').
narrative_ontology:cs_reading_relation('9a9c7360-cb25-4198-b7e9-1c4d06c11e18', us_constitution_text__living_constitutionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('9a9c7360-cb25-4198-b7e9-1c4d06c11e18', us_constitution_text__positivist_reading, influences).
narrative_ontology:cs_axiom('9a9c7360-cb25-4198-b7e9-1c4d06c11e18', foundational, meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('9a9c7360-cb25-4198-b7e9-1c4d06c11e18', meaning_fixed_at_ratification, conventional).
narrative_ontology:cs_axiom('9a9c7360-cb25-4198-b7e9-1c4d06c11e18', secondary, post_ratification_practice_evidentiary_only).
narrative_ontology:cs_axiom_status(post_ratification_practice_evidentiary_only, holdable).
narrative_ontology:cs_axiom_grounding('9a9c7360-cb25-4198-b7e9-1c4d06c11e18', post_ratification_practice_evidentiary_only, conventional).
narrative_ontology:cs_reference_frame('9a9c7360-cb25-4198-b7e9-1c4d06c11e18', ratification_era_public_understanding).
narrative_ontology:cs_drift_state('9a9c7360-cb25-4198-b7e9-1c4d06c11e18', contemporary_judicial_practice, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('9a9c7360-cb25-4198-b7e9-1c4d06c11e18', '').
narrative_ontology:cs_kernel_id(us_constitution_text__originalist_reading, us_constitution_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_text__originalist_reading, conservative_legal_movement).
narrative_ontology:constraint_beneficiary(us_constitution_text__originalist_reading, federalist_society_pipeline_judges).
narrative_ontology:constraint_beneficiary(us_constitution_text__originalist_reading, incumbent_property_and_gun_rights_claimants).
narrative_ontology:constraint_victim(us_constitution_text__originalist_reading, unenumerated_rights_claimants).
narrative_ontology:constraint_victim(us_constitution_text__originalist_reading, reproductive_rights_claimants).
narrative_ontology:constraint_victim(us_constitution_text__originalist_reading, administrative_agencies_relying_on_evolving_doctrine).
narrative_ontology:constraint_victim(us_constitution_text__originalist_reading, marginalized_groups_absent_from_18th_century_franchise).
narrative_ontology:constraint_vindicates(us_constitution_text__originalist_reading, judicial_restraint_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_text__originalist_reading, democratic_legitimacy_through_ratification).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Built the interpretive methodology over five decades through law schools, the Federalist Society pipeline, and coordinated judicial nomination strategy. Administers the reading by training, credentialing, and elevating jurists who apply it; can revise methodological emphasis (which historical sources count, how 'liquidation' works) without abandoning the framework itself. Collects institutional dominance: control over which claims are cognizable in federal courts.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, conservative_legal_movement, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_text__originalist_reading, conservative_legal_movement, beneficiary).

% Sitting judges whose interpretive commitments align with original public meaning; career advancement, professional prestige, and legacy are tied to consistent application of the methodology. Face essentially no personal cost from the constraint's operation and substantial reputational benefit within the movement for rigorous adherence.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, federalist_society_pipeline_judges, beneficiary,
    institutional, civilizational, arbitrage, national).

% Hold rights claims (Second Amendment, takings, contract) with clear grounding in 18th/19th century text and practice. The originalist method validates their claims with high confidence and forecloses balancing tests that might have weighed against them; they can litigate repeatedly with favorable doctrinal wind.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, incumbent_property_and_gun_rights_claimants, beneficiary,
    organized, biographical, mobile, national).

% Assert rights (privacy, bodily autonomy, intimate association) with no clear textual anchor or 18th/19th century historical analogue, often precisely because the relevant social category was excluded from the political community that ratified the text. The method requires them to find a 'deeply rooted' historical tradition that structurally could not have included them; they cannot litigate their way out of a historical record that predates their legal personhood.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, unenumerated_rights_claimants, payer,
    powerless, biographical, trapped, national).

% Lost federal constitutional protection when courts applying the originalist method found no 'history and tradition' support for the prior doctrinal framework. Now dependent on state-by-state legislative and electoral processes with no federal floor; exit means relocation, which is not available to everyone with the same means.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, reproductive_rights_claimants, payer,
    powerless, biographical, trapped, national).

% Built regulatory authority on doctrines (broad Commerce Clause readings, implied delegation, functional separation-of-powers tests) that the originalist method treats as unmoored from original meaning. Face escalating litigation risk and doctrinal rollback; can lobby Congress for explicit statutory authority but cannot simply reinterpret their way to safety.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, administrative_agencies_relying_on_evolving_doctrine, payer,
    institutional, generational, constrained, national).

% Were not parties to the ratifying public whose understanding the method treats as authoritative — enslaved people, women, non-property-holders. Have no seat in the historical record the method privileges; their absence from that record is treated as an evidentiary gap to be filled by amendment, not as evidence the method itself encodes exclusion.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, marginalized_groups_absent_from_18th_century_franchise, excluded,
    powerless, civilizational, trapped, national).

% Judges and scholars who would read constitutional principles as adapting to contemporary circumstances are structurally disfavored in confirmation processes shaped by the movement's institutional dominance; their interpretive framework is treated as illegitimate departure rather than a live methodological alternative within the same profession.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, living_constitutionalist_judges, excluded,
    institutional, generational, constrained, national).

% Study the methodology's internal coherence, its selective use of historical sources, and its correlation with outcomes favorable to the movement that promotes it. Produce competing accounts of whether the method constrains judges or merely launders preferred outcomes through historical citation.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, constitutional_law_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_text__originalist_reading, conservative_legal_movement).
narrative_ontology:fixing_cost_class(us_constitution_text__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a determinate, allegedly non-discretionary decision procedure for constitutional adjudication: judges look to historical evidence of original public meaning rather than exercising open-ended moral or policy judgment, which is presented as constraining judicial power and preserving democratic legitimacy located in the ratifying act.
% TRANSFER_FUNCTION: Moves interpretive authority from contemporary democratic and doctrinal processes to historical record and its curators (originalist scholars, historians selected for testimony, amicus historians), and moves the burden of proof onto claimants whose interests were not represented in the 18th/19th century political community — effectively transferring protection away from groups excluded from that community's franchise toward claims with clean historical analogues.
% ABSENT_VOICES: Groups excluded from the ratifying public (enslaved people, women, non-property-holding men) have no attestable 'original understanding' to be recovered on their behalf; living_constitutionalist_judges and legal realist scholars would object that the method's claimed determinacy is illusory, but their framework is treated as outside the legitimate interpretive mainstream in dominant judicial-selection processes.
% DISAPPEARANCE_RATIONALE: If originalism disappeared as a live interpretive commitment overnight, doctrinal areas built on historical-tradition tests (Second Amendment scope, substantive due process retrenchment, administrative law nondelegation revival) would lose their operative test; litigation strategy across the federal judiciary would reorganize around whichever interpretive method replaced it, and decades of movement-built precedent would face renewed instability.
% FOUNDING_PROBLEM: The perceived problem was judicial activism: unelected judges substituting personal or evolving moral views for the ratified text's fixed meaning, undermining democratic legitimacy and predictability in law.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars and movement institutions attest the problem (judicial policymaking untethered from text) remains live and the method solves it. Independent legal historians and comparative-law scholars outside the movement attest that historical 'original understanding' is frequently indeterminate or contested among historians themselves, and that the method's application correlates strongly with outcomes favorable to the movement's substantive policy commitments — suggesting the founding problem framing is itself partly a retrospective justification for a preferred set of outcomes rather than a neutral decision procedure.
narrative_ontology:disappearance_verdict(us_constitution_text__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_text__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_text__originalist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_text__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_text__originalist_reading, 0.61, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_text__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_text__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_text__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.61) reflects that the methodology systematically disadvantages claims from groups excluded from the ratifying political community — this is not incidental but a structural feature of grounding legitimacy in a historical snapshot that itself encoded exclusion. Suppression (0.78) is high because the constraint's persistence within federal courts increasingly depends on active foreclosure of competing interpretive methods (via judicial selection screening, doctrinal tests like Bruen's history-and-tradition standard) rather than the method simply out-competing alternatives on the merits. Theater ratio is comparatively low (0.28): the historical research performed is often genuine scholarly labor, even where selectively deployed, so this is not primarily a performative constraint — it is a substantively operating one with real distributive consequences.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat, originalism is a constraining discipline on judicial power — a rope binding judges to democratic legitimacy located in ratification. From the payer seats (claimants without historical analogues), the identical structure operates as an enforced exclusion mechanism whose historical-evidence requirement cannot be met by construction, because the excluding community itself is the evidentiary source. The engine's per-seat computation should register this asymmetry directly from the beneficiary/victim and exit-option declarations, independent of either seat's self-characterization.
 *
 * DIRECTIONALITY LOGIC:
 *   The conservative legal movement and its judicial pipeline sit near the full-beneficiary end: they authored the methodology, control its application through selection processes, and collect institutional dominance without bearing its costs. Unenumerated and reproductive rights claimants sit near the full-target end: trapped exit (no alternative forum once federal constitutional protection is foreclosed), no capacity to retroactively generate the historical record the method demands. Incumbent property/gun rights claimants are also beneficiaries but through fortunate historical alignment rather than authorship — their claims happen to have clean 18th/19th century analogues.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unconstrained judicial policymaking) may have been genuinely live in some earlier period, but the founding_problem_status is authored as contested rather than resolved-dead or resolved-live: the movement's own institutions attest continued necessity, while independent legal historians attest the 'original understanding' the method purports to recover is frequently indeterminate, contested among historians, and selectively invoked in ways that track outcome preference more than neutral historical fact. This divergence is exactly the kind of corroboration gap the R5 interview is designed to surface — a founding narrative attested almost entirely by the beneficiaries of the arrangement it justifies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    originalism_as_reading_of_us_constitution_kernel,
    'Is originalism a neutral recovery of a determinate historical fact (original public meaning), or is it one contestable normative reading among several coexisting readings of an irreducibly underdetermined kernel?',
    'This is the committer-frame question itself: it is not resolved empirically within this story but by comparing this story''s ε, beneficiaries, and victims against the living_constitutionalist_reading and positivist_reading sibling stories. Where the three readings diverge sharply in beneficiary structure while sharing the same underlying text, that divergence is evidence the kernel is genuinely contested rather than one reading being simply correct.',
    'If originalism is treated as the uniquely correct recovery of fact, its extraction and suppression appear as necessary costs of fidelity to law. If treated as one reading among coexisting live alternatives, the same extraction and suppression appear as costs of a contested normative choice being enforced as if it were fact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(originalism_as_reading_of_us_constitution_kernel, conceptual, 'Whether originalism recovers fact or enacts one contestable reading among several.').

omega_variable(
    historical_determinacy_of_original_meaning,
    'For any given contested constitutional provision, does the historical record actually yield a determinate original public meaning, or is ''original meaning'' itself constructed through selective historian citation and methodological choices (which sources count, whose understanding counts)?',
    'Comparative analysis of cases where multiple qualified historians reach different conclusions about the same provision''s original meaning using the same primary sources; track rate of scholarly disagreement across contested doctrinal areas.',
    'High historian disagreement would support the reading that originalism''s claimed determinacy is largely rhetorical, and that its actual operation is closer to motivated selection among indeterminate historical materials — raising effective extraction. Low disagreement would support the movement''s self-characterization as a genuinely constraining, non-discretionary method.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_determinacy_of_original_meaning, empirical, 'Whether original public meaning is empirically recoverable or substantially constructed through interpretive selection.').

omega_variable(
    exclusion_of_unrepresented_groups_ambiguity,
    'Is the systematic disadvantage to claims from groups excluded from the 18th/19th century franchise a contingent, fixable feature of current methodological practice, or a structural and irreducible consequence of grounding constitutional legitimacy in a historical snapshot that itself encoded that exclusion?',
    'Track whether methodological refinements (e.g., broader construction of ''the People'' at ratification, incorporation-era evidence for post-Civil-War amendments) meaningfully improve outcomes for previously excluded groups'' claims over time, versus outcomes remaining structurally unchanged regardless of methodological refinement.',
    'If fixable, this constraint''s victim structure could shrink through internal methodological evolution without abandoning originalism. If structural and irreducible, the exclusion is a permanent feature of this reading regardless of refinement, and the tangled_rope classification (genuine coordination function plus asymmetric, structurally locked-in extraction) is the durable correct account rather than a transitional one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusion_of_unrepresented_groups_ambiguity, conceptual, 'Whether exclusionary outcomes are a fixable practice defect or structurally intrinsic to the reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_text__originalist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_text__originalist_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(us_c_tr_t10, us_constitution_text__originalist_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(us_c_tr_t20, us_constitution_text__originalist_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(us_c_tr_t30, us_constitution_text__originalist_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement(us_c_tr_t40, us_constitution_text__originalist_reading, theater_ratio, 40, 0.26).
narrative_ontology:measurement(us_c_tr_t50, us_constitution_text__originalist_reading, theater_ratio, 50, 0.28).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_text__originalist_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(us_c_be_t10, us_constitution_text__originalist_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(us_c_be_t20, us_constitution_text__originalist_reading, base_extractiveness, 20, 0.44).
narrative_ontology:measurement(us_c_be_t30, us_constitution_text__originalist_reading, base_extractiveness, 30, 0.51).
narrative_ontology:measurement(us_c_be_t40, us_constitution_text__originalist_reading, base_extractiveness, 40, 0.57).
narrative_ontology:measurement(us_c_be_t50, us_constitution_text__originalist_reading, base_extractiveness, 50, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_text__originalist_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(us_c_su_t10, us_constitution_text__originalist_reading, suppression_requirement, 10, 0.53).
narrative_ontology:measurement(us_c_su_t20, us_constitution_text__originalist_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(us_c_su_t30, us_constitution_text__originalist_reading, suppression_requirement, 30, 0.67).
narrative_ontology:measurement(us_c_su_t40, us_constitution_text__originalist_reading, suppression_requirement, 40, 0.73).
narrative_ontology:measurement(us_c_su_t50, us_constitution_text__originalist_reading, suppression_requirement, 50, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_text__originalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_text__originalist_reading, 0.12).
narrative_ontology:affects_constraint(us_constitution_text__originalist_reading, living_constitutionalist_reading).
narrative_ontology:affects_constraint(us_constitution_text__originalist_reading, positivist_reading).
narrative_ontology:affects_constraint(us_constitution_text__originalist_reading, second_amendment_history_and_tradition_test).
narrative_ontology:affects_constraint(us_constitution_text__originalist_reading, substantive_due_process_retrenchment).

% DUAL FORMULATION NOTE:
% us_constitution_text__originalist_reading, us_constitution_text__living_constitutionalist_reading, and us_constitution_text__positivist_reading form a kernel-reading family off the shared us_constitution_text kernel. Each reading has a distinct ε, beneficiary/victim structure, and classification: originalist reads as tangled_rope (genuine restraint-on-judicial-discretion coordination function, asymmetric extraction against unrepresented groups, active enforcement via judicial selection); the sibling stories should be evaluated independently rather than as alternative measurements of this same constraint. Downstream doctrinal tests (history-and-tradition standards, substantive due process retrenchment) are structurally dependent on which reading prevails and are listed as affected constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
