% ============================================================================
% CONSTRAINT STORY: us_constitution_1787__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   human_readable: Constitutional Positivism: Text-Plus-Amendment Reading of Constitutional Authority
 *   domain: legal/political philosophy
 *
 * SUMMARY:
 *   This story instantiates the positivist reading of the US Constitution as
 *   a contested kernel: constitutional meaning consists of the ratified text
 *   plus whatever has been formally added through Article V amendment, with
 *   judicial interpretation bound to that text and denied license to import
 *   either framers' original intent (the originalist reading) or evolving
 *   societal values (the living reading) as independent sources of meaning.
 *   This reading is genuinely a middle position — it shares originalism's
 *   textual discipline but rejects originalism's historical-intent
 *   excavation, and it shares living constitutionalism's rejection of frozen
 *   1787 meaning but rejects its openness to judicial updating outside the
 *   text. The distinguishing structural claim is that the amendment process
 *   becomes THE primary — indeed only legitimate — democratic mechanism for
 *   constitutional change, which is what produces this reading's
 *   characteristic beneficiary/victim split: coalitions capable of clearing
 *   Article V's supermajority thresholds gain concentrated
 *   constitutional-authorship power, while claimants whose case depends on
 *   textual gaps being filled by either history or evolving values have no
 *   recognized channel at all.
 *
 * KEY AGENTS:
 *   - textualist_judiciary: administers the restraint doctrine and sets its own scope (institutional/analytical)
 *   - legislative_majorities: primary beneficiary of expanded latitude in textually silent domains (organized/mobile)
 *   - amendment_capable_coalitions: concentrated constitutional-authorship power (powerful/mobile)
 *   - unenumerated_rights_claimants and minority_groups_without_amendment_leverage: structurally excluded from the reading's sole recognized change mechanism (powerless/trapped)
 *   - state_legislatures: ratification veto-gate, dual beneficiary/agenda_setter role (institutional/constrained)
 *   - constitutional_law_scholars: analytical observers of the reading's internal coherence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_1787__positivist_reading, 0.38).
domain_priors:suppression_score(us_constitution_1787__positivist_reading, 0.42).
domain_priors:theater_ratio(us_constitution_1787__positivist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_1787__positivist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_1787__positivist_reading, "Constitutional Positivism: Text-Plus-Amendment Reading of Constitutional Authority").
narrative_ontology:topic_domain(us_constitution_1787__positivist_reading, "legal/political philosophy").

domain_priors:requires_active_enforcement(us_constitution_1787__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_1787__positivist_reading, '7f7fb01d-273e-4be7-8458-56b9adda93bb').
narrative_ontology:cs_kernel_codification('7f7fb01d-273e-4be7-8458-56b9adda93bb', fixed_text).
narrative_ontology:cs_authority_grounding('7f7fb01d-273e-4be7-8458-56b9adda93bb', lineage).
narrative_ontology:cs_interpretation_layer_present('7f7fb01d-273e-4be7-8458-56b9adda93bb').
narrative_ontology:cs_reading_relation('7f7fb01d-273e-4be7-8458-56b9adda93bb', us_constitution_1787__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('7f7fb01d-273e-4be7-8458-56b9adda93bb', us_constitution_1787__living_reading, coexists_with).
narrative_ontology:cs_axiom('7f7fb01d-273e-4be7-8458-56b9adda93bb', foundational, text_binding_independent_of_original_intent).
narrative_ontology:cs_axiom_status(text_binding_independent_of_original_intent, holdable).
narrative_ontology:cs_axiom_grounding('7f7fb01d-273e-4be7-8458-56b9adda93bb', text_binding_independent_of_original_intent, conventional).
narrative_ontology:cs_axiom('7f7fb01d-273e-4be7-8458-56b9adda93bb', foundational, amendment_process_sole_legitimate_change_channel).
narrative_ontology:cs_axiom_status(amendment_process_sole_legitimate_change_channel, holdable).
narrative_ontology:cs_axiom_grounding('7f7fb01d-273e-4be7-8458-56b9adda93bb', amendment_process_sole_legitimate_change_channel, conventional).
narrative_ontology:cs_reference_frame('7f7fb01d-273e-4be7-8458-56b9adda93bb', text_plus_amendment_positivism).
narrative_ontology:cs_drift_state('7f7fb01d-273e-4be7-8458-56b9adda93bb', contemporary_textualist_resurgence, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('7f7fb01d-273e-4be7-8458-56b9adda93bb', '').
narrative_ontology:cs_kernel_id(us_constitution_1787__positivist_reading, us_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, legislative_majorities).
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, textualist_judiciary).
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, amendment_capable_coalitions).
narrative_ontology:constraint_victim(us_constitution_1787__positivist_reading, unenumerated_rights_claimants).
narrative_ontology:constraint_victim(us_constitution_1787__positivist_reading, minority_groups_without_amendment_leverage).
narrative_ontology:constraint_victim(us_constitution_1787__positivist_reading, populations_excluded_from_ratification_era_polity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, state_legislatures).
narrative_ontology:constraint_vindicates(us_constitution_1787__positivist_reading, legislative_supremacy_within_text).
narrative_ontology:constraint_vindicates(us_constitution_1787__positivist_reading, judicial_restraint_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_1787__positivist_reading, amendment_process_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Applies the constitutional text as written, refusing to import either framers' subjective intent or evolving social consensus as independent sources of meaning. Administers the reading by deciding cases; can expand or contract its own restraint doctrine, which is the lever that determines how binding this reading actually is in practice.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, textualist_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Gain expanded lawmaking latitude wherever the text is silent or ambiguous, because judicial restraint under this reading declines to fill gaps with either historical intent or contemporary values. Can pursue policy through ordinary legislation without fear of an activist court overriding it on non-textual grounds.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, legislative_majorities, beneficiary,
    organized, biographical, mobile, national).

% Political coalitions large and durable enough to clear Article V's supermajority thresholds can permanently rewrite constitutional meaning. This reading treats the amendment process as the sole legitimate channel for constitutional change, which concentrates constitutional-change power in whoever can assemble that scale of coalition.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, amendment_capable_coalitions, beneficiary,
    powerful, generational, mobile, national).

% Individuals and groups asserting rights or protections not named in the text (or its amendments) find no textual hook for judicial recognition. Under this reading, the courts will not read such claims in from intent or evolving values; the only remedy is the amendment process, which is structurally out of reach for a diffuse or politically weak claimant group.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, unenumerated_rights_claimants, payer,
    powerless, biographical, trapped, national).

% Groups too small or too politically disfavored to ever assemble a two-thirds congressional supermajority and three-fourths of state legislatures are permanently locked out of the sole mechanism this reading recognizes for updating protections. Judicial restraint means courts will not compensate for this structural amendment-access gap.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, minority_groups_without_amendment_leverage, payer,
    powerless, generational, trapped, national).

% Descendants of groups who had no voice in either the original ratification or many subsequent amendment votes inherit a text-plus-amendment structure whose contents were fixed largely without their participation. Their only avenue for change is the same amendment gauntlet that concentrates power in large existing coalitions.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, populations_excluded_from_ratification_era_polity, payer,
    powerless, civilizational, trapped, national).

% Hold ratification power over amendments and thus a veto-and-gate role in the only channel this reading treats as legitimate for constitutional change. Benefit from a reading that routes all constitutional evolution through a process in which they hold structural leverage.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, state_legislatures, beneficiary,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__positivist_reading, state_legislatures, agenda_setter).

% Study and debate whether text-bound-but-not-history-bound interpretation is a coherent middle path between originalism and living constitutionalism, or an unstable compromise that collapses into one or the other under pressure.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, constitutional_law_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_1787__positivist_reading, diffuse).
narrative_ontology:fixing_cost_class(us_constitution_1787__positivist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, publicly ascertainable standard for what the Constitution means: the words on the page plus whatever has been formally added by supermajority process, without importing either contested historical intent or contested contemporary values as a freestanding source of law. This lets courts, legislatures, and citizens converge on a common reference text rather than litigating meta-level interpretive theory in every case.
% TRANSFER_FUNCTION: Moves the power to change constitutional meaning away from judicial discretion (denied to both originalist historical excavation and living-constitutionalist evolution) and concentrates it in whichever political coalitions can meet Article V's supermajority thresholds — shifting effective constitutional authorship from courts and from diffuse social consensus toward organized, resource-rich coalitions capable of sustained multi-state campaigns.
% ABSENT_VOICES: Groups whose claims depend on either subsequently-discovered historical context (favoring originalism) or evolving societal understanding (favoring living constitutionalism) have no seat in this reading's framework — their objection is that a bare text-plus-amendment standard forecloses exactly the interpretive moves that would recognize their claims, and they are structurally unable to marshal Article V supermajorities to be heard through the reading's own preferred channel.
% DISAPPEARANCE_RATIONALE: If judicial commitment to textualist restraint disappeared, courts would resume filling constitutional gaps via either originalist historical inquiry or evolving-values reasoning; legislative majorities would lose their expanded latitude in textually silent areas, and the entire secondary industry of textualist judicial appointments, confirmation battles, and doctrine would lose its organizing premise.
% FOUNDING_PROBLEM: The problem of judicial legitimacy: courts need a principle limiting their own discretion so that constitutional adjudication does not become indistinguishable from unaccountable policymaking by unelected judges, while still permitting some evolution short of full historical fixation.
% FOUNDING_PROBLEM_CORROBORATION: Textualist judges and legal academics who advocate this reading attest the problem of judicial overreach is live and current. Critics outside this camp — including originalist scholars who argue textualism smuggles in judicial discretion under another name, and living-constitutionalist scholars who argue it freezes injustice that only evolving interpretation could remedy — dispute both the diagnosis and the fix; no source entirely outside the reading's own proponents affirms the problem is being solved rather than relabeled.
narrative_ontology:disappearance_verdict(us_constitution_1787__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_1787__positivist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_1787__positivist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_1787__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_1787__positivist_reading, 0.38, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is moderate (0.38) — this reading does coordinate genuine value (predictable, text-anchored adjudication reduces the arbitrariness risk of unconstrained judicial discretion), but it also structurally advantages large, well-resourced coalitions over diffuse or minority claimants by making Article V the sole legitimate channel for change. Suppression (0.42) reflects that the reading actively forecloses two entire categories of legal argument (historical-intent arguments and evolving-values arguments) rather than merely declining to prioritize them — this is an active exclusion, not passive neutrality, which is why it requires ongoing judicial enforcement to maintain against pressure from both originalist and living-constitutionalist advocates. Theater ratio is moderate-low (0.28) and rising slowly, reflecting that 'textualism' as invoked in practice sometimes serves as a rhetorical label for outcomes reached on other grounds — a documented but not yet dominant phenomenon. The suppression spike around 1857 (Dred Scott era) reflects the reading's actual historical deployment to entrench exclusionary textual readings; the decline through the mid-20th century reflects the loosening of purely text-bound doctrine during the Warren-era shift, followed by a renewed rise as textualism regained doctrinal prominence from the 1980s forward.
 *
 * DIRECTIONALITY LOGIC:
 *   Legislative majorities and amendment-capable coalitions sit near the beneficiary end: this reading expands their effective power by denying courts an independent basis to override them absent explicit textual command, and by making them the exclusive engine of constitutional change. Unenumerated rights claimants, amendment-leverage-poor minorities, and populations excluded from the ratification-era and amendment-era polity sit near the target end: the reading's core commitment — no non-textual, non-amendment path to recognition — is precisely what forecloses their claims, and their exit option is trapped because the amendment process this reading treats as adequate is, for them, practically unreachable. State legislatures occupy a genuine dual position: they benefit from ratification-gate leverage while also functioning as agenda-setters who could, collectively, alter the reading's practical bindingness by how they exercise that gate.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — cabining judicial discretion so courts do not become an unaccountable third legislature — remains genuinely live in the sense that the concern about unconstrained judicial power persists across the political spectrum. But the reading's chosen fix (bind judges to text, route all real change through Article V) has itself become a site of contestation: critics on the originalist side argue textualism smuggles back discretion in the selection of which textual meaning counts as 'plain,' while critics on the living-constitutionalist side argue it freezes remediable injustice by design. The classification here (tangled_rope) reflects that both a genuine coordination function (predictability, restraint of raw judicial policymaking) and asymmetric extraction (systematic exclusion of claimants who cannot marshal supermajority coalitions) are simultaneously present and mutually entangled — pure celebration of the coordination story, or pure denunciation as extraction, would each miss half the structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    text_plain_meaning_indeterminacy,
    'Is ''what the text says'' ever genuinely self-executing, or does every non-trivial application require an interpretive supplement that smuggles in either historical or evolving-values reasoning under a textualist label?',
    'Systematic review of textualist judicial opinions to identify cases where the ''plain meaning'' claimed diverges from corpus-linguistic or historical-usage analysis, indicating covert importation of other interpretive modes.',
    'If plain-meaning textualism is rarely self-executing in contested cases, this reading may be less structurally distinct from its siblings than claimed, and its extraction profile would need re-examination as disguised originalism or disguised living-constitutionalism rather than a genuine third position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(text_plain_meaning_indeterminacy, conceptual, 'Whether pure textualism is a coherent, self-executing interpretive method or an unstable compromise.').

omega_variable(
    amendment_process_access_asymmetry,
    'Is the concentration of constitutional-change power in Article V-capable coalitions an inherent, unavoidable feature of any workable amendment mechanism, or a specifically severe asymmetry compared to how other constitutional systems handle textual gaps?',
    'Comparative constitutional analysis of amendment-difficulty across jurisdictions, correlated with measures of minority-rights protection and adaptability to social change.',
    'If the US amendment threshold is an outlier in difficulty relative to comparable democracies, the extraction attributed to this reading is partly an artifact of a specific, contingent design choice (Article V''s supermajority requirements) rather than an inherent feature of the positivist reading itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_process_access_asymmetry, empirical, 'Whether Article V''s difficulty, not textualism per se, drives the exclusion of minority claimants.').

omega_variable(
    committer_frame_reading_distinctness,
    'This story treats positivist, originalist, and living readings as three structurally distinct constraints per the kernel/reading framework. Is the positivist reading genuinely distinct in practice, or does it collapse into originalism when applied to founding-era text and into living constitutionalism when applied to later amendments (whose ''text'' already encodes contested contemporary values at ratification)?',
    'Track a sample of positivist-labeled judicial opinions across constitutional provisions of different amendment-eras to see whether the interpretive method used varies systematically with how recently the provision was ratified.',
    'If the reading systematically collapses into its siblings depending on which text is at issue, the three-way kernel decomposition may need a finer-grained axis (e.g. per-provision reading rather than per-controversy reading) rather than three globally competing readings of the whole document.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_frame_reading_distinctness, conceptual, 'Whether the positivist reading is a stable, distinct reading or an unstable blend of its siblings depending on which provision is being construed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_1787__positivist_reading, 1787, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1787, us_constitution_1787__positivist_reading, theater_ratio, 1787, 0.1).
narrative_ontology:measurement(us_c_tr_t1857, us_constitution_1787__positivist_reading, theater_ratio, 1857, 0.15).
narrative_ontology:measurement(us_c_tr_t1937, us_constitution_1787__positivist_reading, theater_ratio, 1937, 0.2).
narrative_ontology:measurement(us_c_tr_t1980, us_constitution_1787__positivist_reading, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(us_c_tr_t2005, us_constitution_1787__positivist_reading, theater_ratio, 2005, 0.27).
narrative_ontology:measurement(us_c_tr_t2025, us_constitution_1787__positivist_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1787, us_constitution_1787__positivist_reading, base_extractiveness, 1787, 0.55).
narrative_ontology:measurement(us_c_be_t1857, us_constitution_1787__positivist_reading, base_extractiveness, 1857, 0.6).
narrative_ontology:measurement(us_c_be_t1937, us_constitution_1787__positivist_reading, base_extractiveness, 1937, 0.42).
narrative_ontology:measurement(us_c_be_t1980, us_constitution_1787__positivist_reading, base_extractiveness, 1980, 0.35).
narrative_ontology:measurement(us_c_be_t2005, us_constitution_1787__positivist_reading, base_extractiveness, 2005, 0.36).
narrative_ontology:measurement(us_c_be_t2025, us_constitution_1787__positivist_reading, base_extractiveness, 2025, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1787, us_constitution_1787__positivist_reading, suppression_requirement, 1787, 0.3).
narrative_ontology:measurement(us_c_su_t1857, us_constitution_1787__positivist_reading, suppression_requirement, 1857, 0.55).
narrative_ontology:measurement(us_c_su_t1937, us_constitution_1787__positivist_reading, suppression_requirement, 1937, 0.4).
narrative_ontology:measurement(us_c_su_t1980, us_constitution_1787__positivist_reading, suppression_requirement, 1980, 0.35).
narrative_ontology:measurement(us_c_su_t2005, us_constitution_1787__positivist_reading, suppression_requirement, 2005, 0.4).
narrative_ontology:measurement(us_c_su_t2025, us_constitution_1787__positivist_reading, suppression_requirement, 2025, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_1787__positivist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_1787__positivist_reading, 0.12).
narrative_ontology:affects_constraint(us_constitution_1787__positivist_reading, us_constitution_1787__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_1787__positivist_reading, us_constitution_1787__living_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the us_constitution_1787 kernel. The originalist_reading fixes meaning at ratification and treats framers' intent as binding (higher accessibility_collapse, lower amendment-centrality); the living_reading treats the text as an aspirational framework updated through evolving societal understanding (lower suppression of judicial discretion, different beneficiary set favoring contemporary social consensus over Article V coalitions). All three share the same underlying kernel — the ratified constitutional text — but instantiate structurally distinct constraints with different ε profiles, different beneficiaries, and different victim sets, per the ε-invariance decomposition principle. They are linked here rather than merged into one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
