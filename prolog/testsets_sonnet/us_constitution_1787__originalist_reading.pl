% ============================================================================
% CONSTRAINT STORY: us_constitution_1787__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_1787__originalist_reading, []).

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
 *   constraint_id: us_constitution_1787__originalist_reading
 *   human_readable: Originalist Reading of the U.S. Constitution (1787 Ratification-Fixed Meaning)
 *   domain: constitutional_law/legal_theory/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the originalist reading of the U.S. Constitution
 *   as a kernel: constitutional meaning is fixed at ratification (1787, with
 *   subsequent amendments fixing meaning at their own ratification dates),
 *   and framers'/ratifiers' original public meaning binds present
 *   adjudication. This is one of three coexisting readings of the same
 *   constitutional text-as-kernel; the living-constitutionalism reading and
 *   the text-plus-amendment positivist reading are separate constraint
 *   stories with their own ε values, beneficiary/victim structures, and
 *   classifications — they are not alternate observables of this constraint
 *   but structurally distinct claims. The originalist reading's structural
 *   signature is a narrow constraint set (modern claims without historical
 *   analogues fall outside protection), legitimation of
 *   pre-1787/pre-amendment practices by default, and high epistemic demands
 *   on historical evidence that in practice concentrate interpretive power in
 *   whichever institutions can produce authoritative-seeming historical
 *   narratives.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_1787__originalist_reading, 0.52).
domain_priors:suppression_score(us_constitution_1787__originalist_reading, 0.58).
domain_priors:theater_ratio(us_constitution_1787__originalist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_1787__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_1787__originalist_reading, "Originalist Reading of the U.S. Constitution (1787 Ratification-Fixed Meaning)").
narrative_ontology:topic_domain(us_constitution_1787__originalist_reading, "constitutional_law/legal_theory/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_1787__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_1787__originalist_reading, '14d7cd16-3347-4840-9d4c-2a691953e442').
narrative_ontology:cs_kernel_codification('14d7cd16-3347-4840-9d4c-2a691953e442', fixed_text).
narrative_ontology:cs_authority_grounding('14d7cd16-3347-4840-9d4c-2a691953e442', lineage).
narrative_ontology:cs_interpretation_layer_present('14d7cd16-3347-4840-9d4c-2a691953e442').
narrative_ontology:cs_reading_relation('14d7cd16-3347-4840-9d4c-2a691953e442', us_constitution_1787__living_reading, forecloses).
narrative_ontology:cs_reading_relation('14d7cd16-3347-4840-9d4c-2a691953e442', us_constitution_1787__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('14d7cd16-3347-4840-9d4c-2a691953e442', foundational, ratification_era_meaning_binds_present_adjudication).
narrative_ontology:cs_axiom_status(ratification_era_meaning_binds_present_adjudication, holdable).
narrative_ontology:cs_axiom_grounding('14d7cd16-3347-4840-9d4c-2a691953e442', ratification_era_meaning_binds_present_adjudication, conventional).
narrative_ontology:cs_axiom('14d7cd16-3347-4840-9d4c-2a691953e442', secondary, judicial_discretion_illegitimate_absent_historical_warrant).
narrative_ontology:cs_axiom_status(judicial_discretion_illegitimate_absent_historical_warrant, holdable).
narrative_ontology:cs_axiom_grounding('14d7cd16-3347-4840-9d4c-2a691953e442', judicial_discretion_illegitimate_absent_historical_warrant, instrumental).
narrative_ontology:cs_reference_frame('14d7cd16-3347-4840-9d4c-2a691953e442', ratification_era_public_meaning).
narrative_ontology:cs_drift_state('14d7cd16-3347-4840-9d4c-2a691953e442', post_1970s_movement_conservatism_revival, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('14d7cd16-3347-4840-9d4c-2a691953e442', '').
narrative_ontology:cs_kernel_id(us_constitution_1787__originalist_reading, us_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, originalist_judiciary).
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, federalist_society_legal_movement).
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, incumbent_property_and_gun_rights_holders).
narrative_ontology:constraint_victim(us_constitution_1787__originalist_reading, groups_excluded_from_1787_political_community).
narrative_ontology:constraint_victim(us_constitution_1787__originalist_reading, claimants_of_unenumerated_modern_rights).
narrative_ontology:constraint_victim(us_constitution_1787__originalist_reading, regulatory_reform_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, historians_and_originalist_evidence_producers).
narrative_ontology:constraint_vindicates(us_constitution_1787__originalist_reading, popular_sovereignty_at_founding).
narrative_ontology:constraint_vindicates(us_constitution_1787__originalist_reading, rule_of_law_against_judicial_discretion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Judges and justices who adjudicate cases by reconstructing 1787-era public meaning or framers' intent. They administer the interpretive method itself, deciding which historical sources count as evidence and how contested history resolves. Their institutional authority and doctrinal legacy are built on the method's claimed determinacy and neutrality; they collect intellectual and career capital from being seen as constrained by history rather than exercising discretion.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, originalist_judiciary, agenda_setter,
    institutional, civilizational, arbitrage, national).

% A coordinated legal and political movement that developed, funded, and staffed the originalist method's rise from academic minority position to controlling judicial doctrine. Benefits from the reading's tendency to narrow the constraint set against economic regulation, administrative agencies, and expansive rights claims. Can shift funding, litigation strategy, and judicial nominations to advance the reading; faces essentially no exit cost since the method's success is the organization's purpose.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, federalist_society_legal_movement, beneficiary,
    organized, generational, arbitrage, national).

% Individuals and entities whose existing property, contract, and firearms interests are protected by a reading that treats 1791-1868 legal categories as the ceiling on permissible regulation. They benefit from the reading's resistance to new regulatory constraint without having to defend the substance of their position on modern policy terms.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, incumbent_property_and_gun_rights_holders, beneficiary,
    powerful, biographical, mobile, national).

% Enslaved people, women, and non-property-holding men were excluded from the political community that ratified the Constitution and shaped its 'original public meaning.' Their descendants and analogous groups today bear a reading that treats a document produced without their participation or consent as the fixed benchmark for legitimate constraint. They cannot exit the jurisdiction the reading governs, and cannot retroactively participate in the ratification whose meaning is now binding on them.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, groups_excluded_from_1787_political_community, payer,
    powerless, civilizational, trapped, national).

% Litigants asserting rights to privacy, bodily autonomy, or novel forms of equal treatment not contemplated by 1787-1868 legal categories. Under this reading their claims fall outside the constraint boundary unless they can show a specific, well-established historical analogue — a high and often impossible epistemic burden. They bear the cost of the reading's narrow scope in the form of foreclosed claims and reversed precedent.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, claimants_of_unenumerated_modern_rights, payer,
    moderate, biographical, constrained, national).

% Legislative and administrative actors seeking to build new regulatory frameworks (environmental, financial, labor) face a reading that treats the framers' 18th-century administrative imagination as a ceiling on delegated power. They can lobby for constitutional amendment (a near-impossible supermajority threshold) or litigate under a method stacked against their theory of governance.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, regulatory_reform_movements, payer,
    organized, generational, constrained, national).

% Legal historians whose scholarship is now load-bearing evidence in constitutional adjudication. Some benefit professionally from originalism's demand for historical expertise; others document how contested, cherry-picked, or indeterminate the historical record actually is, and how liberally 'original public meaning' can be constructed to reach a preferred outcome.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, historians_and_originalist_evidence_producers, observer,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__originalist_reading, historians_and_originalist_evidence_producers, beneficiary).

% Jurists and scholars who hold that constitutional meaning evolves with society, or that it is fixed by text-plus-amendment rather than 1787 intent, are structurally excluded from originalist courts' interpretive method even when they hold judicial seats — their framework is treated as illegitimate discretion rather than a competing reading of the same kernel.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, living_and_positivist_reading_adherents, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_1787__originalist_reading, diffuse).
narrative_ontology:fixing_cost_class(us_constitution_1787__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a determinacy claim that constrains judicial discretion by anchoring meaning to a fixed historical point, offering predictability and a neutral-seeming decision procedure across a diverse, contested polity.
% TRANSFER_FUNCTION: Moves interpretive authority toward those able to produce and control persuasive historical narratives, and moves the burden of constitutional change from judicial adaptation onto the near-unusable Article V amendment process — shifting outcomes toward the status quo ante and away from claimants without an 18th/19th-century analogue.
% ABSENT_VOICES: The enslaved, women, and non-propertied men who had no voice in 1787-1868 ratification are permanently absent from the 'original public meaning' the reading treats as authoritative; modern claimants whose interests were unimaginable to the framers are excluded from the room by construction, not merely by circumstance.
% DISAPPEARANCE_RATIONALE: If originalism disappeared as the controlling interpretive method overnight, decades of doctrine reversing regulatory, reproductive, and voting-rights precedent would lose their principal justification, forcing courts back onto competing frameworks (living constitutionalism, textualism-without-originalism, or positivist text-plus-amendment readings) and reopening settled questions across administrative law, gun regulation, and unenumerated rights.
% FOUNDING_PROBLEM: The reading was built to solve a legitimacy and discretion-constraint problem: as courts exercised increasing interpretive latitude in the mid-20th century, originalism offered a claim that judges were following fixed, discoverable meaning rather than imposing personal values — restoring democratic legitimacy to judicial review by tying it to the ratifying public's consent.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars and the Federalist Society movement attest the discretion-constraint problem remains live and that the method genuinely solves it. Independent legal historians, comparative constitutional scholars, and dissenting jurists attest from outside the movement that the historical record is frequently indeterminate or contested enough that originalist reasoning imports discretion under a determinacy label rather than eliminating it — corroborated by documented divergence among originalist judges reaching opposite results from the same historical record.
narrative_ontology:disappearance_verdict(us_constitution_1787__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_1787__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_1787__originalist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_1787__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_1787__originalist_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_1787__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_1787__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_1787__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored as moderate-to-substantial (0.52 at present) and rising over the interval: the reading was comparatively low-stakes when it was simply 'how ratifying-era judges read the document' (the ordinary interpretive default from 1787-1900s), but its extractive potential rose sharply once it was adopted as a deliberate counter-doctrine (1970s-80s) explicitly organized to reverse mid-20th-century rights expansions — the transfer function became more consequential as the doctrine was weaponized rather than merely descriptive. Suppression is high and rising because the reading requires active judicial enforcement to exclude competing interpretive methods as illegitimate, not merely different. Theater ratio is kept low-moderate (0.22): the historical research the method demands is often genuine scholarly work, not pure performance, though a growing share is result-oriented history production.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (originalist judiciary) this reads as principled constraint-following — judges bound by law rather than exercising will. From the payer seats (groups excluded from 1787's political community, unenumerated-rights claimants) the identical structure reads as a mechanism that launders present-day policy outcomes through a historical record that was never designed to speak to their interests and that they had no part in creating. The engine should compute divergent per-seat types from this same structural data; the divergence is the analytical payload, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   The originalist judiciary and the Federalist Society movement sit near the beneficiary end: they set and administer the method, and its doctrinal success is their institutional currency. Incumbent property/gun-rights holders benefit because the method's default is protective of already-vested 18th/19th-century-recognized interests. Groups excluded from the 1787 ratifying community are the clearest targets: high d, trapped exit, since the 'original public meaning' by construction encodes a political community that excluded them and offers no participatory remedy short of near-impossible constitutional amendment. Unenumerated-rights claimants and regulatory reformers are payers with constrained exit — litigation and lobbying remain available but are structured to fail against the method's default.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (constrain judicial discretion, restore interpretive legitimacy via traceable popular consent) may have been genuinely live circa 1970s given real concerns about judicial policy-making. Whether it remains live or has become a vehicle for the reverse of its stated aim — imposing present-day policy preferences via selective historical narrative construction while claiming neutrality — is exactly the contested genealogy question the founding_problem fields are built to surface. The reading's own proponents and its outside critics give incompatible corroboration, which is itself diagnostic: a genealogy corroborated only from inside the beneficiary set would be a stronger signal of pure legitimation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    originalism_determinacy_vs_discretion_laundering,
    'Does the originalist method actually constrain judicial discretion as claimed, or does the frequent indeterminacy and contestability of the historical record allow judges to reach preferred outcomes while claiming historical compulsion?',
    'Empirical study of originalist judges'' agreement rates when applying the method to the same historical record on contested questions; if originalist reasoning produces outcomes as divergent as non-originalist reasoning on comparably contested questions, the determinacy claim is undermined.',
    'If the method is substantially indeterminate in practice, the reading functions closer to a legitimation device for outcomes reached on other grounds (raising effective extraction and suppression), rather than the genuine discretion-constraint coordination function it claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalism_determinacy_vs_discretion_laundering, empirical, 'Whether originalism genuinely constrains judicial discretion or launders discretion under a determinacy claim.').

omega_variable(
    ratifying_community_legitimacy,
    'Can a constraint whose ''original public meaning'' was fixed by a political community that excluded enslaved people, women, and non-propertied men be treated as democratically legitimate for those excluded groups'' descendants and analogues today?',
    'Political-theory analysis of consent and legitimacy transfer across generations and across categorically excluded groups; comparison to how the reading treats Reconstruction Amendments (which did include broader participation) versus the 1787 text itself.',
    'If ratifying-community exclusion delegitimizes original-public-meaning-as-binding for excluded groups, the reading''s coordination claim (democratic consent transmitted forward) fails for a substantial victim population, strengthening the tangled_rope reading over any rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ratifying_community_legitimacy, conceptual, 'Whether original public meaning fixed by an exclusionary ratifying community can bind excluded groups'' descendants today.').

omega_variable(
    kernel_reading_selection_pressure,
    'Is the choice between originalist, living, and positivist readings of the constitutional kernel itself neutral, or does the selection track which reading currently serves the interests of whichever coalition controls judicial appointments?',
    'Track historical correlation between which reading a judicial coalition champions and whether that reading currently produces outcomes favorable to that coalition''s substantive policy preferences, across multiple issue areas and time periods.',
    'If reading-selection correlates strongly with outcome-preference rather than principled jurisprudential commitment, all three kernel readings (including this one) should be read partly as post-hoc justification structures, raising effective extraction across the whole kernel family.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_pressure, conceptual, 'Whether reading-selection among originalist/living/positivist frameworks tracks substantive outcome preference.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_1787__originalist_reading, 1787, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1787, us_constitution_1787__originalist_reading, theater_ratio, 1787, 0.05).
narrative_ontology:measurement(us_c_tr_t1868, us_constitution_1787__originalist_reading, theater_ratio, 1868, 0.08).
narrative_ontology:measurement(us_c_tr_t1937, us_constitution_1787__originalist_reading, theater_ratio, 1937, 0.1).
narrative_ontology:measurement(us_c_tr_t1980, us_constitution_1787__originalist_reading, theater_ratio, 1980, 0.14).
narrative_ontology:measurement(us_c_tr_t2000, us_constitution_1787__originalist_reading, theater_ratio, 2000, 0.17).
narrative_ontology:measurement(us_c_tr_t2016, us_constitution_1787__originalist_reading, theater_ratio, 2016, 0.2).
narrative_ontology:measurement(us_c_tr_t2025, us_constitution_1787__originalist_reading, theater_ratio, 2025, 0.22).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1787, us_constitution_1787__originalist_reading, base_extractiveness, 1787, 0.28).
narrative_ontology:measurement(us_c_be_t1868, us_constitution_1787__originalist_reading, base_extractiveness, 1868, 0.32).
narrative_ontology:measurement(us_c_be_t1937, us_constitution_1787__originalist_reading, base_extractiveness, 1937, 0.3).
narrative_ontology:measurement(us_c_be_t1980, us_constitution_1787__originalist_reading, base_extractiveness, 1980, 0.38).
narrative_ontology:measurement(us_c_be_t2000, us_constitution_1787__originalist_reading, base_extractiveness, 2000, 0.44).
narrative_ontology:measurement(us_c_be_t2016, us_constitution_1787__originalist_reading, base_extractiveness, 2016, 0.49).
narrative_ontology:measurement(us_c_be_t2025, us_constitution_1787__originalist_reading, base_extractiveness, 2025, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1787, us_constitution_1787__originalist_reading, suppression_requirement, 1787, 0.35).
narrative_ontology:measurement(us_c_su_t1868, us_constitution_1787__originalist_reading, suppression_requirement, 1868, 0.4).
narrative_ontology:measurement(us_c_su_t1937, us_constitution_1787__originalist_reading, suppression_requirement, 1937, 0.38).
narrative_ontology:measurement(us_c_su_t1980, us_constitution_1787__originalist_reading, suppression_requirement, 1980, 0.44).
narrative_ontology:measurement(us_c_su_t2000, us_constitution_1787__originalist_reading, suppression_requirement, 2000, 0.5).
narrative_ontology:measurement(us_c_su_t2016, us_constitution_1787__originalist_reading, suppression_requirement, 2016, 0.55).
narrative_ontology:measurement(us_c_su_t2025, us_constitution_1787__originalist_reading, suppression_requirement, 2025, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_1787__originalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_1787__originalist_reading, 0.12).
narrative_ontology:affects_constraint(us_constitution_1787__originalist_reading, us_constitution_1787__living_reading).
narrative_ontology:affects_constraint(us_constitution_1787__originalist_reading, us_constitution_1787__positivist_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraint stories decomposing the natural-language concept 'constitutional interpretation methodology' per the ε-invariance principle: originalist_reading (this file), living_reading, and positivist_reading. Each reading of the us_constitution_1787 kernel produces a structurally distinct constraint with its own ε, beneficiary/victim set, and classification — they are not the same constraint measured three ways. This file's ε (0.52, rising) reflects the originalist reading's narrow constraint set and high historical-evidence burden; the living reading's ε and beneficiary structure will differ substantially given its broader constraint set and different transfer function (toward claimants of evolving rights rather than away from them). All three should link to each other via affects_constraints since they compete for the same interpretive authority slot in the same judicial system.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
