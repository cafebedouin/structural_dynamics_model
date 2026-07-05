% ============================================================================
% CONSTRAINT STORY: us_constitution_1787__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_1787__positivist_reading, []).

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
 *   constraint_id: us_constitution_1787__positivist_reading
 *   human_readable: Constitutional Positivism: Text-Plus-Amendment as Binding Meaning
 *   domain: legal/political
 *
 * SUMMARY:
 *   This story instantiates the POSITIVIST reading of the contested US
 *   Constitution kernel: constitutional meaning consists of the enacted text
 *   plus whatever has been added through the formal Article V amendment
 *   process, and judicial interpretation is constrained to derive holdings
 *   from that text rather than from framers' intent (the originalist reading)
 *   or from evolving societal values (the living reading). This is a distinct
 *   constraint from its siblings, not a measurement angle on one shared
 *   constraint — its beneficiary set (legislative/amendment-capable
 *   coalitions), its victim set (amendment-incapable minorities,
 *   unenumerated-rights litigants), and its extractiveness profile all differ
 *   structurally from the originalist and living readings. Per the
 *   ε-invariance principle, each reading gets its own file; this file does
 *   not describe or average over the siblings.
 *
 * KEY AGENTS:
 *   - textualist_judges: administer the interpretive method (institutional/constrained)
 *   - legislative_majorities: primary beneficiary of channeling change through amendment (organized/mobile)
 *   - minority_groups_without_amendment_power: bear the cost of textual silence with no amendment leverage (powerless/trapped)
 *   - constitutional_scholars: analytical observers of long-run doctrinal drift
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_1787__positivist_reading, 0.32).
domain_priors:suppression_score(us_constitution_1787__positivist_reading, 0.4).
domain_priors:theater_ratio(us_constitution_1787__positivist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_1787__positivist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_1787__positivist_reading, "Constitutional Positivism: Text-Plus-Amendment as Binding Meaning").
narrative_ontology:topic_domain(us_constitution_1787__positivist_reading, "legal/political").

domain_priors:requires_active_enforcement(us_constitution_1787__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_1787__positivist_reading, 'c3b3f3b0-378f-486a-b5e5-f421f6e4fb60').
narrative_ontology:cs_kernel_codification('c3b3f3b0-378f-486a-b5e5-f421f6e4fb60', fixed_text).
narrative_ontology:cs_authority_grounding('c3b3f3b0-378f-486a-b5e5-f421f6e4fb60', lineage).
narrative_ontology:cs_interpretation_layer_present('c3b3f3b0-378f-486a-b5e5-f421f6e4fb60').
narrative_ontology:cs_reading_relation('c3b3f3b0-378f-486a-b5e5-f421f6e4fb60', us_constitution_1787__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('c3b3f3b0-378f-486a-b5e5-f421f6e4fb60', us_constitution_1787__living_reading, forecloses).
narrative_ontology:cs_axiom('c3b3f3b0-378f-486a-b5e5-f421f6e4fb60', foundational, current_text_plus_amendment_is_sole_authority).
narrative_ontology:cs_axiom_status(current_text_plus_amendment_is_sole_authority, holdable).
narrative_ontology:cs_axiom_grounding('c3b3f3b0-378f-486a-b5e5-f421f6e4fb60', current_text_plus_amendment_is_sole_authority, conventional).
narrative_ontology:cs_axiom('c3b3f3b0-378f-486a-b5e5-f421f6e4fb60', foundational, judicial_role_excludes_extratextual_value_importation).
narrative_ontology:cs_axiom_status(judicial_role_excludes_extratextual_value_importation, holdable).
narrative_ontology:cs_axiom_grounding('c3b3f3b0-378f-486a-b5e5-f421f6e4fb60', judicial_role_excludes_extratextual_value_importation, instrumental).
narrative_ontology:cs_reference_frame('c3b3f3b0-378f-486a-b5e5-f421f6e4fb60', text_plus_amendment_supremacy).
narrative_ontology:cs_drift_state('c3b3f3b0-378f-486a-b5e5-f421f6e4fb60', contemporary_doctrinal_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c3b3f3b0-378f-486a-b5e5-f421f6e4fb60', '').
narrative_ontology:cs_kernel_id(us_constitution_1787__positivist_reading, us_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, legislative_majorities).
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, textualist_judges).
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, amendment_capable_coalitions).
narrative_ontology:constraint_victim(us_constitution_1787__positivist_reading, minority_groups_without_amendment_power).
narrative_ontology:constraint_victim(us_constitution_1787__positivist_reading, litigants_seeking_unenumerated_rights).
narrative_ontology:constraint_victim(us_constitution_1787__positivist_reading, constitutionally_unaddressed_populations).
narrative_ontology:constraint_vindicates(us_constitution_1787__positivist_reading, rule_of_law_predictability_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_1787__positivist_reading, democratic_legitimacy_of_written_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicate disputes by reading the constitutional text and duly ratified amendments as the exclusive source of binding meaning, refusing to import extratextual values, framer psychology, or evolving social consensus. They administer the interpretive method, deciding what counts as text-derivable versus judicial invention, and their rulings are enforced by the federal judiciary's coercive apparatus.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, textualist_judges, agenda_setter,
    institutional, generational, constrained, national).

% Hold the amendment power (with supermajority coalition-building) and thus the only legitimate channel for constitutional change under this reading. They benefit because the positivist frame directs contested moral and political questions back to them rather than to courts, preserving their institutional relevance and insulating statutes from judicial override on non-textual grounds.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, legislative_majorities, beneficiary,
    organized, generational, mobile, national).

% Well-organized political movements with the numbers and geographic distribution to clear Article V's supermajority thresholds. They benefit from a rule that channels all constitutional change through a process they are equipped to win, while movements lacking that reach are foreclosed from judicial workarounds.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, amendment_capable_coalitions, beneficiary,
    powerful, generational, mobile, national).

% Populations too small, too dispersed, or too politically disfavored to ever assemble the supermajorities Article V requires. Under the positivist reading, courts will not read protections for them into ambiguous text absent an amendment, so they bear the cost of textual silence indefinitely — the process most likely to fix their situation is the one structurally hardest for them to use.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, minority_groups_without_amendment_power, payer,
    powerless, biographical, trapped, national).

% Bring claims resting on values (privacy, dignity, evolving equality norms) not textually enumerated. Under strict text-plus-amendment reading, these claims fail regardless of merit unless traceable to specific clause language; their only recourse is the amendment process, which requires resources and coalition strength they typically lack.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, litigants_seeking_unenumerated_rights, payer,
    moderate, biographical, constrained, national).

% Groups whose existence or circumstances (new technologies, novel social arrangements, populations not contemplated by any drafting or amending generation) fall into genuine textual silence. The positivist method offers no interpretive bridge for them beyond legislative or amendment action, leaving them without judicial remedy by design rather than by oversight.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, constitutionally_unaddressed_populations, payer,
    powerless, civilizational, trapped, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__positivist_reading, constitutionally_unaddressed_populations, excluded).

% Argues that text alone, without fixed original public meaning, is manipulable by judges who can read contemporary connotations into old words — they would object that positivism smuggles back judicial discretion under the label of textualism. They participate in the same courts and confirmation battles but are not the authority this constraint's judges answer to; their critique is heard in briefs and dissents, not adjudicated as controlling.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, originalist_legal_movement, excluded,
    organized, generational, mobile, national).

% Study how text-plus-amendment jurisprudence functions across decades, comparing its outcomes to originalist and living-constitution regimes, without a stake in any single ruling's result.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_1787__positivist_reading, amendment_capable_coalitions).
narrative_ontology:fixing_cost_class(us_constitution_1787__positivist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, publicly ascertainable rule for what counts as constitutional law — the enacted text plus whatever has been added through the Article V process — so that judges, legislators, and citizens can predict which claims are legally cognizable without relitigating deep moral or historical questions in every case.
% TRANSFER_FUNCTION: Moves interpretive authority away from courts reading extratextual values (whether framer intent or evolving social consensus) and toward legislative supermajorities capable of formal amendment; correspondingly moves the cost of constitutional silence onto groups unable to marshal amendment-level political power.
% ABSENT_VOICES: Populations too small or dispersed to build Article V supermajorities have no voice in the process this reading treats as the only legitimate mechanism for change; litigants whose claims rest on textually silent values are heard in court but structurally cannot prevail on positivist terms, regardless of the substantive merit of their claim.
% DISAPPEARANCE_RATIONALE: If the positivist convention vanished, courts adjudicating constitutional claims would either revert to searching original public meaning (originalist reading) or reading text against contemporary values (living reading) — either shift would immediately reopen classes of claims currently foreclosed (or open) under text-plus-amendment, changing outcomes in areas like unenumerated rights, administrative deference, and equal protection doctrine.
% FOUNDING_PROBLEM: Post-ratification constitutional practice needed a principle distinguishing legitimate judicial interpretation from judicial lawmaking — courts were accused of inventing rights or narrowing them based on personal or ideological preference rather than any textual warrant, threatening the perceived legitimacy of judicial review itself.
% FOUNDING_PROBLEM_CORROBORATION: Textualist judges and originalist-adjacent legal academics attest the constraining-judicial-discretion problem remains live and cite recent decisions departing from unwritten doctrine as evidence. Critical legal scholars and civil-rights litigation groups, both outside the beneficiary coalition, attest the 'discretion-constraining' framing is itself a vehicle for substantive outcomes favored by the coalition that promotes it, pointing to selective textualism in commerce-clause versus rights-clause cases as evidence the neutrality claim is not fully corroborated by practice.
narrative_ontology:disappearance_verdict(us_constitution_1787__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_1787__positivist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_1787__positivist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_1787__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_1787__positivist_reading, 0.32, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_1787__positivist_reading_tests).
:- end_tests(us_constitution_1787__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.32) and rising slowly: the positivist method is genuinely more constraining on judicial discretion than the living reading, which limits how much it can extract via interpretive drift, but the concentration of legitimate change into the high-threshold Article V process creates a structural asymmetry that compounds over decades as amendment-capable coalitions entrench. Suppression (0.4) reflects the doctrine's active use to foreclose specific classes of claims (unenumerated rights, penumbral protections) rather than raw coercive force. Theater ratio is comparatively low (0.22) because the coordination function — predictable, publicly ascertainable constitutional meaning — is largely real, not performative; the rising trend across the interval tracks selective and inconsistent application (textualism invoked in some doctrinal areas, relaxed in others) that gradually converts some of the method's neutrality claim into cover.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (textualist judges) and the beneficiary seat (amendment-capable coalitions), the constraint reads as principled constraint on judicial power — a rope. From the payer seats (amendment-incapable minorities, unenumerated-rights litigants), the same structure reads as a mechanism that locks in whatever the text happened to say at moments of past political victory, with no path to remedy short of coalition-building most of them cannot achieve. The engine computes these divergent per-seat classifications from the structural power/exit data; this story does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Legislative majorities and amendment-capable coalitions sit near the beneficiary end: the reading directs all legitimate constitutional change through a channel they are structurally equipped to use, and forecloses judicial routes that could bypass their political veto. Minority groups without amendment power and litigants whose claims rest on unenumerated values sit near the target end: trapped or constrained exit, biographical time horizons that cannot wait out generational amendment cycles, and no interpretive recourse when text is silent. Textualist judges are agenda-setters administering the rule rather than direct financial beneficiaries, but they are structurally entrenched by the method's legitimacy claims.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — constraining judicial invention of rights or restrictions untethered to any textual warrant — remains partially live: courts continue to face genuine accusations of results-oriented reasoning. But the mismatch surfaces in application: some doctrinal areas see rigorous text-plus-amendment discipline while others see selective invocation that tracks substantive preference. This divergence is why the story is authored as tangled_rope rather than rope: the coordination function (predictability, constrained judicial discretion) is real, but it operates alongside asymmetric extraction (foreclosure of remedies for amendment-incapable groups) sustained by active enforcement (judicial refusal to recognize non-textual claims).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    positivist_vs_originalist_textual_boundary,
    'When positivist judges read ''the text'' without inquiring into original public meaning, are they applying a genuinely distinct method from originalism, or smuggling contemporary connotations into old words under a textualist label?',
    'Comparative doctrinal analysis: track cases where positivist and originalist methods would predict different outcomes for the same clause, and observe which prediction actual rulings match over a multi-decade sample.',
    'If positivist rulings systematically converge with originalist predictions, the positivist reading may not be structurally distinct enough to warrant separate treatment from originalism; if they diverge in a consistent pattern, positivism is confirmed as its own reading with its own extraction profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(positivist_vs_originalist_textual_boundary, conceptual, 'Whether positivism is structurally distinct from originalism or a rebranded variant.').

omega_variable(
    amendment_threshold_as_extraction_mechanism,
    'Is Article V''s supermajority requirement a neutral democratic safeguard (protecting against transient majority tyranny) or a structural extraction mechanism that entrenches whichever coalition achieved past supermajority status?',
    'Historical base-rate analysis of successful amendments against the demographic and political composition of the coalitions that achieved them, compared to the demographic composition of groups whose claims have failed under positivist doctrine.',
    'If the threshold systematically favors historically dominant coalitions, the positivist reading''s channeling of change through Article V is itself an extraction mechanism, not merely a coordination device; if the threshold is genuinely neutral across eras, the tangled_rope classification weakens toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_threshold_as_extraction_mechanism, empirical, 'Whether the amendment threshold is neutral or systematically entrenching.').

omega_variable(
    selective_textualism_consistency,
    'Is the rising theater_ratio trend evidence of selective application of positivist method (invoked in some doctrinal areas, relaxed in others to reach preferred outcomes), or a genuine artifact of increasingly complex textual questions requiring more elaborate — but still principled — textual analysis?',
    'Systematic coding of positivist-method rulings across doctrinal areas (commerce clause, equal protection, unenumerated rights) for internal consistency in how strictly the text-only rule is applied.',
    'Confirmed selective application would support reclassifying the trend as Goodhart-style metric substitution (textualism as cover story); confirmed consistency would support treating the rising theater_ratio as noise or genuine complexity growth rather than drift toward extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(selective_textualism_consistency, empirical, 'Whether selective invocation of the positivist method is occurring.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_1787__positivist_reading, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1980, us_constitution_1787__positivist_reading, theater_ratio, 1980, 0.12).
narrative_ontology:measurement(us_c_tr_t1989, us_constitution_1787__positivist_reading, theater_ratio, 1989, 0.14).
narrative_ontology:measurement(us_c_tr_t1998, us_constitution_1787__positivist_reading, theater_ratio, 1998, 0.16).
narrative_ontology:measurement(us_c_tr_t2007, us_constitution_1787__positivist_reading, theater_ratio, 2007, 0.18).
narrative_ontology:measurement(us_c_tr_t2016, us_constitution_1787__positivist_reading, theater_ratio, 2016, 0.2).
narrative_ontology:measurement(us_c_tr_t2025, us_constitution_1787__positivist_reading, theater_ratio, 2025, 0.22).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1980, us_constitution_1787__positivist_reading, base_extractiveness, 1980, 0.2).
narrative_ontology:measurement(us_c_be_t1989, us_constitution_1787__positivist_reading, base_extractiveness, 1989, 0.23).
narrative_ontology:measurement(us_c_be_t1998, us_constitution_1787__positivist_reading, base_extractiveness, 1998, 0.25).
narrative_ontology:measurement(us_c_be_t2007, us_constitution_1787__positivist_reading, base_extractiveness, 2007, 0.27).
narrative_ontology:measurement(us_c_be_t2016, us_constitution_1787__positivist_reading, base_extractiveness, 2016, 0.3).
narrative_ontology:measurement(us_c_be_t2025, us_constitution_1787__positivist_reading, base_extractiveness, 2025, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1980, us_constitution_1787__positivist_reading, suppression_requirement, 1980, 0.3).
narrative_ontology:measurement(us_c_su_t1989, us_constitution_1787__positivist_reading, suppression_requirement, 1989, 0.32).
narrative_ontology:measurement(us_c_su_t1998, us_constitution_1787__positivist_reading, suppression_requirement, 1998, 0.34).
narrative_ontology:measurement(us_c_su_t2007, us_constitution_1787__positivist_reading, suppression_requirement, 2007, 0.36).
narrative_ontology:measurement(us_c_su_t2016, us_constitution_1787__positivist_reading, suppression_requirement, 2016, 0.38).
narrative_ontology:measurement(us_c_su_t2025, us_constitution_1787__positivist_reading, suppression_requirement, 2025, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_1787__positivist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_1787__positivist_reading, 0.1).
narrative_ontology:affects_constraint(us_constitution_1787__positivist_reading, originalist_reading).
narrative_ontology:affects_constraint(us_constitution_1787__positivist_reading, living_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language concept 'constitutional interpretive method' under the us_constitution_1787 kernel. originalist_reading fixes meaning at ratification via framer intent; living_reading treats text as an evolving aspirational framework; this positivist_reading treats meaning as text-plus-amendment, judicially constrained to textual derivation without either historical-intent inquiry or evolving-values inquiry. Each has a distinct beneficiary/victim structure and a distinct ε — they are not the same constraint viewed three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
