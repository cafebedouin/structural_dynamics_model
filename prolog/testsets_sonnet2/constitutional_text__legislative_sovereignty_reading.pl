% ============================================================================
% CONSTRAINT STORY: constitutional_text__legislative_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text__legislative_sovereignty_reading, []).

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
 *   constraint_id: constitutional_text__legislative_sovereignty_reading
 *   human_readable: Legislative Sovereignty Reading of Constitutional Supremacy (Parliament as Final Constitutional Interpreter)
 *   domain: constitutional_theory/political_philosophy/comparative_law
 *
 * SUMMARY:
 *   This story authors the legislative-sovereignty reading of a contested
 *   constitutional kernel: a constitutional text that establishes parliament
 *   as the ultimate authority on constitutional meaning, with judicial review
 *   functioning as advisory rather than final. Courts may find legislation
 *   inconsistent with constitutional rights, but the legislature may
 *   reinstate the legislation through a notwithstanding clause, simple
 *   override statute, or (in pure Westminster systems) by not entrenching
 *   judicial review at all. This is presented, on its own terms, as the
 *   coordination solution to the problem of unelected judicial policymaking
 *   on contested questions — final constitutional authority tracks electoral
 *   accountability. The reading's own metrics describe a real but partial
 *   coordination function riding alongside a genuine extraction dynamic:
 *   majorities can and repeatedly do use override power against the same
 *   recurring disadvantaged groups. This is a distinct constraint from the
 *   judicial_supremacy_reading and popular_sovereignty_reading siblings
 *   (other files) — per the ε-invariance principle, each reading gets its own
 *   ε, its own beneficiary/victim structure, and its own classification,
 *   linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - governing_party_caucus: agenda_setter/beneficiary (institutional/arbitrage) — invokes override authority, faces no binding external check
 *   - electoral_majorities: beneficiary (organized/mobile) — treated as ultimate source of legitimate constitutional meaning
 *   - constitutional_courts: excluded/observer (institutional/constrained) — reviews but cannot bind outcomes
 *   - discrete_minority_groups: payer (powerless/trapped) — rights findings reversible by override
 *   - unpopular_criminal_defendants: payer (powerless/trapped) — procedural protections vulnerable to panic-driven override
 *   - future_electoral_minorities: payer (powerless/trapped) — inherit weakened rights shield regardless of current governing party
 *   - comparative_constitutional_scholars: observer (analytical/analytical) — cross-jurisdictional evidence on override use patterns
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__legislative_sovereignty_reading, 0.52).
domain_priors:suppression_score(constitutional_text__legislative_sovereignty_reading, 0.47).
domain_priors:theater_ratio(constitutional_text__legislative_sovereignty_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, suppression_requirement, 0.47).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__legislative_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text__legislative_sovereignty_reading, "Legislative Sovereignty Reading of Constitutional Supremacy (Parliament as Final Constitutional Interpreter)").
narrative_ontology:topic_domain(constitutional_text__legislative_sovereignty_reading, "constitutional_theory/political_philosophy/comparative_law").

domain_priors:requires_active_enforcement(constitutional_text__legislative_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__legislative_sovereignty_reading, '8cb41b7f-47b1-4e33-adb3-2d60065d1d93').
narrative_ontology:cs_kernel_codification('8cb41b7f-47b1-4e33-adb3-2d60065d1d93', fixed_text).
narrative_ontology:cs_authority_grounding('8cb41b7f-47b1-4e33-adb3-2d60065d1d93', practice).
narrative_ontology:cs_interpretation_layer_present('8cb41b7f-47b1-4e33-adb3-2d60065d1d93').
narrative_ontology:cs_reading_relation('8cb41b7f-47b1-4e33-adb3-2d60065d1d93', constitutional_text__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('8cb41b7f-47b1-4e33-adb3-2d60065d1d93', constitutional_text__popular_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('8cb41b7f-47b1-4e33-adb3-2d60065d1d93', foundational, electoral_accountability_is_final_legitimacy_test).
narrative_ontology:cs_axiom_status(electoral_accountability_is_final_legitimacy_test, holdable).
narrative_ontology:cs_axiom_grounding('8cb41b7f-47b1-4e33-adb3-2d60065d1d93', electoral_accountability_is_final_legitimacy_test, conventional).
narrative_ontology:cs_axiom('8cb41b7f-47b1-4e33-adb3-2d60065d1d93', foundational, unelected_judicial_finality_is_illegitimate).
narrative_ontology:cs_axiom_status(unelected_judicial_finality_is_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('8cb41b7f-47b1-4e33-adb3-2d60065d1d93', unelected_judicial_finality_is_illegitimate, deontological).
narrative_ontology:cs_reference_frame('8cb41b7f-47b1-4e33-adb3-2d60065d1d93', westminster_parliamentary_supremacy_tradition).
narrative_ontology:cs_drift_state('8cb41b7f-47b1-4e33-adb3-2d60065d1d93', contemporary_rights_litigation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8cb41b7f-47b1-4e33-adb3-2d60065d1d93', '').
narrative_ontology:cs_kernel_id(constitutional_text__legislative_sovereignty_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__legislative_sovereignty_reading, electoral_majorities).
narrative_ontology:constraint_beneficiary(constitutional_text__legislative_sovereignty_reading, governing_party_caucus).
narrative_ontology:constraint_beneficiary(constitutional_text__legislative_sovereignty_reading, cabinet_executive).
narrative_ontology:constraint_victim(constitutional_text__legislative_sovereignty_reading, discrete_minority_groups).
narrative_ontology:constraint_victim(constitutional_text__legislative_sovereignty_reading, unpopular_criminal_defendants).
narrative_ontology:constraint_victim(constitutional_text__legislative_sovereignty_reading, future_electoral_minorities).
narrative_ontology:constraint_vindicates(constitutional_text__legislative_sovereignty_reading, parliamentary_supremacy_doctrine).
narrative_ontology:constraint_vindicates(constitutional_text__legislative_sovereignty_reading, majoritarian_self_governance_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the legislative agenda and can invoke a notwithstanding clause or simple override statute to nullify judicial invalidation of its legislation. Frames this as the constitutionally correct restoration of majoritarian will against unelected judges. Faces no binding external check once it commands a legislative majority.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, governing_party_caucus, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text__legislative_sovereignty_reading, governing_party_caucus, beneficiary).

% Their preferences, expressed through periodic elections, are treated as the ultimate source of legitimate constitutional meaning. When courts strike down popular legislation, the majority's representatives can override the ruling, so the majority's current will is rarely permanently blocked by constitutional interpretation it dislikes.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, electoral_majorities, beneficiary,
    organized, biographical, mobile, national).

% Can review legislation and issue rulings on constitutional compliance, but those rulings are advisory in the sense that the legislature may override them by statute or notwithstanding declaration. The court's institutional voice is present but structurally subordinate to whatever the current legislative majority decides; it has no coercive mechanism to make its reading final.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, constitutional_courts, excluded,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text__legislative_sovereignty_reading, constitutional_courts, observer).

% Rely on constitutional rights protection precisely because they lack the numbers to prevail in ordinary legislative politics. When a court finds legislation infringes their rights, an override can reinstate the infringing law regardless of the finding. They have no electoral leverage sufficient to reverse an entrenched majority's override and no forum above the legislature to appeal to.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, discrete_minority_groups, payer,
    powerless, generational, trapped, national).

% Procedural or substantive rights protections won in court (search and seizure limits, sentencing proportionality, due process) can be legislatively overridden in response to public panic about crime, with the override often popular precisely because this population cannot mobilize sympathetic electoral support.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, unpopular_criminal_defendants, payer,
    powerless, immediate, trapped, national).

% The precedent that legislative majorities may override constitutional rights findings persists as a standing institutional capability. Whichever group is out of power in a future electoral cycle inherits a weaker rights shield than a judicial-supremacy system would offer, regardless of which party currently holds the override power.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, future_electoral_minorities, payer,
    powerless, civilizational, trapped, national).

% Study override use across jurisdictions (Canada's Section 33, UK parliamentary sovereignty, Israel's override mechanisms) to assess whether legislative-sovereignty systems produce systematically different minority-rights outcomes than judicial-supremacy systems, without holding a stake in any single jurisdiction's dispute.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text__legislative_sovereignty_reading, governing_party_caucus).
narrative_ontology:fixing_cost_class(constitutional_text__legislative_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the question of who has final say when courts and elected legislatures disagree about constitutional meaning, by locating ultimate authority in the body most directly and repeatedly accountable to the electorate, avoiding indefinite deadlock or unelected judicial policymaking on contested moral and political questions.
% TRANSFER_FUNCTION: Moves final interpretive authority over contested rights questions from courts (where individuals and minorities can litigate on the merits of a specific claim) to legislative majorities (where outcomes track electoral arithmetic), effectively transferring protective capacity from whoever a court finds has a valid claim to whoever the current governing coalition is willing to protect.
% ABSENT_VOICES: Discrete minorities and future electoral minorities have no seat in the legislative process that produces an override — by definition, an override is invoked precisely when a court has sided with a group the current majority is willing to overrule. Their argument for judicial finality is heard in court but does not bind the outcome.
% DISAPPEARANCE_RATIONALE: If legislative override authority vanished overnight, courts would become the final word on constitutional meaning; legislatures would need to pursue formal constitutional amendment (a much higher threshold) rather than ordinary statute to reverse judicial rulings; rights-based litigation strategy and legislative drafting practice would both change substantially, and several currently-overridden judicial rulings would presumably take effect.
% FOUNDING_PROBLEM: Post-independence and post-colonial legislatures in several Westminster-derived systems sought a way to retain ultimate democratic control over constitutional meaning while still permitting judicial rights review, rejecting both unconstrained legislative supremacy (no rights review at all) and unconstrained judicial supremacy (unelected judges as final arbiters) as illegitimate extremes.
% FOUNDING_PROBLEM_CORROBORATION: Framers and governing-party officials attest the arrangement still solves the live problem of reconciling democratic accountability with rights review. Independent comparative constitutional scholars and international human rights bodies (outside any single jurisdiction's governing coalition) document that override provisions are disproportionately invoked against the same recurring set of disadvantaged groups, corroborating that the framework's protective function has substantially eroded in practice even where its formal justification persists.
narrative_ontology:disappearance_verdict(constitutional_text__legislative_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text__legislative_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__legislative_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_text__legislative_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text__legislative_sovereignty_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text__legislative_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text__legislative_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_text__legislative_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52 at interval end) reflects a real but only moderate transfer: most legislation is never subject to override, and most constitutional disputes are resolved through ordinary judicial-legislative dialogue without invoking final override authority. It is not scored as high as a pure extraction mechanism because the coordination function (resolving legislative-judicial deadlock via democratic accountability) is genuine and frequently exercised without controversy. Suppression (0.47) tracks the standing capability of override, which need not be invoked to suppress rights litigation strategy — the mere availability of override changes what litigants and courts expect to accomplish. Theater ratio (0.28) is moderate-low: override use is a real exercise of legislative power, not primarily performative, though public invocation of override is sometimes timed for electoral signaling value beyond its substantive necessity. Accessibility collapse (0.40) is moderate: minority groups retain the courts as a forum and can pursue political mobilization, amendment campaigns, or international advocacy, so alternatives are not fully foreclosed. Resistance (0.55) is substantial: civil society, opposition parties, and international human rights bodies actively contest override use when it occurs.
 *
 * PERSPECTIVAL GAP:
 *   From the governing_party_caucus seat, override is the correct exercise of a constitutionally granted power restoring majoritarian will. From the discrete_minority_groups seat, the identical structural mechanism experienced as a court victory nullified by ordinary statute is functionally indistinguishable from having no rights protection at all. The engine computes these as different seat-level classifications from the same structural data; the claimed_type (tangled_rope) reflects the analytical judgment that both a genuine coordination function and asymmetric extraction are simultaneously present and require active enforcement (the override statute itself) to persist.
 *
 * DIRECTIONALITY LOGIC:
 *   The governing_party_caucus and electoral_majorities sit near the beneficiary end of directionality: the arrangement subsidizes their capacity to enact and defend preferred policy against judicial rights findings. Discrete minority groups, unpopular criminal defendants, and future electoral minorities sit near the full-target end: they are structurally trapped (no exit from the jurisdiction's constitutional order, no electoral leverage sufficient to prevent override) and bear the extraction directly when override is invoked against a favorable ruling they won in court. Constitutional courts occupy an unusual position — institutionally powerful but structurally excluded from final say on this specific question, which is why they are marked excluded/observer rather than agenda_setter despite their formal review function.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling democratic accountability with rights review, rejecting both unconstrained legislative and unconstrained judicial supremacy — remains genuinely contested rather than resolved or dead. This is not a case of a mandate that has plainly outlived its function: legislative sovereignty proponents can point to ongoing instances where courts have struck down popular, defensible legislation on contestable constitutional grounds, and override or non-entrenchment restores democratic input. The mandatrophy risk is asymmetric and slow-building: each individual override may be a legitimate exercise of the founding design, but the accumulating pattern (per the founding_problem_corroboration) shows override disproportionately falling on the same recurring disadvantaged groups, which is closer to entrenched extraction dressed as recurring democratic correction. The classification as tangled_rope rather than snare reflects that the coordination function is real and independently exercisable without the extractive pattern — a legislature could resolve most legislative-judicial disagreement without disproportionately targeting minorities, but empirically often does not.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    override_as_correction_vs_erosion,
    'Is legislative override, on net, a correction of judicial overreach (restoring legitimate democratic input) or a mechanism of systematic erosion (repeatedly withdrawing rights protection from groups too weak to defend them electorally)?',
    'Longitudinal cross-jurisdictional study of override invocations (Canada Section 33, comparable UK and Commonwealth override mechanisms) coding each instance by which party benefited, which group bore the cost, and whether the underlying judicial finding was later vindicated by international human rights bodies or subsequent domestic consensus.',
    'If override use tracks genuinely contested and reversible policy disagreements evenly across groups, this reading is closer to pure coordination (rope). If override use disproportionately and persistently targets the same disadvantaged groups regardless of which party governs, this reading is closer to tangled_rope shading toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(override_as_correction_vs_erosion, empirical, 'Whether the empirical pattern of override use supports the coordination story or reveals systematic extraction.').

omega_variable(
    kernel_indeterminacy_of_supremacy,
    'Does the founding constitutional text actually settle whether parliament or the courts hold final interpretive authority, or is this reading itself a contestable gloss on genuinely ambiguous or silent constitutional language?',
    'Close textual and historical analysis of the constitution-making record: drafting debates, contemporaneous commentary, and subsequent amendment history, compared against the alternative textual support claimed by the judicial_supremacy_reading and popular_sovereignty_reading siblings.',
    'If the text genuinely and unambiguously establishes legislative sovereignty, this reading''s claim to be the authoritative account of the kernel is strong. If the text is genuinely silent or supports multiple readings, all three sibling readings are better understood as competing constructions layered onto an underdetermined kernel, none uniquely authoritative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_indeterminacy_of_supremacy, conceptual, 'Whether the constitutional text itself determinately supports the legislative-sovereignty reading over its siblings.').

omega_variable(
    override_frequency_vs_availability_extraction,
    'Does the extraction in this reading depend on override actually being invoked, or does the mere standing availability of override authority itself constitute the extraction (by shaping litigation strategy, settlement dynamics, and judicial caution in advance)?',
    'Compare litigation outcomes and judicial reasoning in jurisdictions with override authority against comparable jurisdictions without it, controlling for underlying case mix, to detect anticipatory judicial deference or litigant strategy shifts attributable to override''s mere existence.',
    'If extraction is driven primarily by anticipatory effects rather than actual invocation, the measured extractiveness and suppression should be read as largely independent of override''s observed frequency of use — a low invocation rate would not indicate low effective extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(override_frequency_vs_availability_extraction, empirical, 'Whether override''s extractive effect operates mainly through actual use or through standing availability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__legislative_sovereignty_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text__legislative_sovereignty_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(cons_tr_t8, constitutional_text__legislative_sovereignty_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement(cons_tr_t16, constitutional_text__legislative_sovereignty_reading, theater_ratio, 16, 0.18).
narrative_ontology:measurement(cons_tr_t24, constitutional_text__legislative_sovereignty_reading, theater_ratio, 24, 0.21).
narrative_ontology:measurement(cons_tr_t32, constitutional_text__legislative_sovereignty_reading, theater_ratio, 32, 0.25).
narrative_ontology:measurement(cons_tr_t40, constitutional_text__legislative_sovereignty_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(cons_be_t8, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 8, 0.37).
narrative_ontology:measurement(cons_be_t16, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 16, 0.41).
narrative_ontology:measurement(cons_be_t24, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 24, 0.46).
narrative_ontology:measurement(cons_be_t32, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 32, 0.49).
narrative_ontology:measurement(cons_be_t40, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 40, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(cons_su_t8, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 8, 0.34).
narrative_ontology:measurement(cons_su_t16, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 16, 0.38).
narrative_ontology:measurement(cons_su_t24, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 24, 0.41).
narrative_ontology:measurement(cons_su_t32, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 32, 0.44).
narrative_ontology:measurement(cons_su_t40, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 40, 0.47).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text__legislative_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_text__legislative_sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_text__legislative_sovereignty_reading, constitutional_text__judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_text__legislative_sovereignty_reading, constitutional_text__popular_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the constitutional_text kernel, decomposed per the ε-invariance principle because the natural-language concept 'who has final constitutional authority' conflates structurally distinct claims with different ε values, different beneficiary/victim structures, and different classifications. legislative_sovereignty_reading (this file): parliament final, courts advisory, ε=0.52, tangled_rope. judicial_supremacy_reading (sibling): courts final, ε differs because the extraction/beneficiary structure inverts (minority rights protection becomes the coordination function; majoritarian frustration becomes the cost). popular_sovereignty_reading (sibling): neither institution is supreme, authority rests in constituent power exercised through amendment/convention/revolution, which produces a different and likely lower-frequency, higher-threshold extraction profile. The three readings are linked here via affects_constraints because a jurisdiction's practical operation under one reading exerts downstream pressure on the legitimacy conditions available to the others (e.g., frequent override use under legislative_sovereignty_reading empirically discredits the coordination story that judicial_supremacy_reading opponents would otherwise concede).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
