% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_authority__parliamentary_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_authority__parliamentary_sovereignty_reading, []).

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
 *   constraint_id: basic_law_interpretive_authority__parliamentary_sovereignty_reading
 *   human_readable: Parliamentary Sovereignty Reading of Final Interpretive Authority
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   This story instantiates the parliamentary sovereignty reading of the
 *   contested kernel over final interpretive authority in a constitutional
 *   order: the position that the elected legislature, not the judiciary,
 *   should hold the terminal word on contested constitutional meaning,
 *   because its mandate is renewed by direct election and it bears electoral
 *   accountability that courts do not. The reading is generated as its own
 *   clean, ε-invariant constraint — it does not describe or average over the
 *   judicial-supremacy or popular-constitutionalism readings, which are
 *   separate constraint files linked only by network reference. Under this
 *   reading, the override mechanism functions as coordination (resolving
 *   disagreement between two legitimate interpretive bodies) layered with
 *   asymmetric extraction (rights-vindicated minorities lose the benefit of
 *   favorable rulings when a majority invokes override, and the judiciary
 *   bears reputational and functional costs it cannot decline).
 *
 * KEY AGENTS:
 *   - elected_legislature: primary agenda-setter and beneficiary of override authority (institutional/arbitrage)
 *   - governing_majority_coalition: proximate beneficiary that wields the override in practice (powerful/mobile)
 *   - constitutional_court: institutional payer whose rulings are subordinated (institutional/constrained)
 *   - rights_bearing_minorities: primary victims when override reverses protective rulings (powerless/trapped)
 *   - opposition_parties: excluded from effective veto over override use (organized/constrained)
 *   - constitutional_scholars: analytical observers of comparative override design (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.52).
domain_priors:suppression_score(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.48).
domain_priors:theater_ratio(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_authority__parliamentary_sovereignty_reading, "Parliamentary Sovereignty Reading of Final Interpretive Authority").
narrative_ontology:topic_domain(basic_law_interpretive_authority__parliamentary_sovereignty_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(basic_law_interpretive_authority__parliamentary_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_authority__parliamentary_sovereignty_reading, '6a6f080c-40a3-42ed-9ffd-8c4eea668775').
narrative_ontology:cs_kernel_codification('6a6f080c-40a3-42ed-9ffd-8c4eea668775', formalized).
narrative_ontology:cs_authority_grounding('6a6f080c-40a3-42ed-9ffd-8c4eea668775', lineage).
narrative_ontology:cs_interpretation_layer_present('6a6f080c-40a3-42ed-9ffd-8c4eea668775').
narrative_ontology:cs_reading_relation('6a6f080c-40a3-42ed-9ffd-8c4eea668775', basic_law_interpretive_authority__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('6a6f080c-40a3-42ed-9ffd-8c4eea668775', basic_law_interpretive_authority__popular_constitutionalism_reading, coexists_with).
narrative_ontology:cs_axiom('6a6f080c-40a3-42ed-9ffd-8c4eea668775', foundational, electoral_accountability_confers_terminal_interpretive_legitimacy).
narrative_ontology:cs_axiom_status(electoral_accountability_confers_terminal_interpretive_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('6a6f080c-40a3-42ed-9ffd-8c4eea668775', electoral_accountability_confers_terminal_interpretive_legitimacy, conventional).
narrative_ontology:cs_axiom('6a6f080c-40a3-42ed-9ffd-8c4eea668775', secondary, unelected_judicial_review_is_democratically_illegitimate_when_terminal).
narrative_ontology:cs_axiom_status(unelected_judicial_review_is_democratically_illegitimate_when_terminal, holdable).
narrative_ontology:cs_axiom_grounding('6a6f080c-40a3-42ed-9ffd-8c4eea668775', unelected_judicial_review_is_democratically_illegitimate_when_terminal, instrumental).
narrative_ontology:cs_reference_frame('6a6f080c-40a3-42ed-9ffd-8c4eea668775', electoral_mandate_as_terminal_legitimacy).
narrative_ontology:cs_drift_state('6a6f080c-40a3-42ed-9ffd-8c4eea668775', contemporary_override_normalization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6a6f080c-40a3-42ed-9ffd-8c4eea668775', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_authority__parliamentary_sovereignty_reading, basic_law_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__parliamentary_sovereignty_reading, elected_legislature).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__parliamentary_sovereignty_reading, governing_majority_coalition).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, judicial_independence_function).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, rights_bearing_minorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, constitutional_court).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds and exercises the power to override or legislatively supersede judicial constitutional interpretations, justified by democratic mandate renewed at each election. Sets the terms under which override is invoked (supermajority thresholds, notwithstanding clauses, re-enactment procedures) and administers the mechanism itself. Accrues institutional authority and policy control each time an override succeeds.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, elected_legislature, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__parliamentary_sovereignty_reading, elected_legislature, beneficiary).

% The current parliamentary majority benefits most directly: it can use the override power to pass and shield legislation a court has found constitutionally infirm, without needing to amend the constitution itself. Its exit option is favorable — it can always legislate again, dissolve and re-seek mandate, or simply outlast a court's term.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, governing_majority_coalition, beneficiary,
    powerful, biographical, mobile, national).

% Renders constitutional judgments that can be legislatively overridden, re-enacted around, or nullified by sufficient parliamentary vote. Cannot exit the relationship — its docket and its rulings' finality are structurally subordinate to the legislature's override mechanism. Absorbs the institutional cost of being visibly overruled, which erodes its authority over repeated cycles.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, constitutional_court, payer,
    institutional, generational, constrained, national).

% Groups whose rights claims were vindicated by judicial review (electoral minorities, religious minorities, criminal defendants, protest movements) can have that protection legislatively reversed by a sufficient parliamentary majority invoking the override. They have no equivalent institutional lever to restore the protection short of winning back majority support at the ballot box — a route that is often structurally closed to them precisely because they are minorities.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, rights_bearing_minorities, payer,
    powerless, biographical, trapped, national).

% Object to override use when it entrenches majority policy against judicial check, but their formal power to block an override is limited to whatever supermajority or procedural threshold the legislature itself set. Their objections register in debate and dissent but do not carry a veto unless the threshold specifically requires cross-party consent.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, opposition_parties, excluded,
    organized, biographical, constrained, national).

% Study override mechanisms comparatively across jurisdictions (Canada's notwithstanding clause, UK parliamentary sovereignty, Israeli override debates) and assess whether the mechanism functions as a democratic safety valve against judicial overreach or as an entrenchment tool for transient majorities against minority rights.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_authority__parliamentary_sovereignty_reading, elected_legislature).
narrative_ontology:fixing_cost_class(basic_law_interpretive_authority__parliamentary_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the counter-majoritarian problem: when an unelected judiciary's constitutional reading conflicts with the considered will of an elected legislature, this arrangement gives the body with the more direct and renewable democratic mandate the final word, preventing indefinite judicial veto over legislative policy.
% TRANSFER_FUNCTION: Moves final interpretive authority — and with it, the practical capacity to entrench or reverse rights determinations — from the judiciary to the sitting legislative majority. Where override is exercised, it moves the specific benefit of a favorable judicial ruling away from the litigant class that won it and back to the majority coalition that disagreed with the ruling.
% ABSENT_VOICES: Rights-bearing minorities whose protections were vindicated in court and then legislatively reversed have no seat in the override decision beyond ordinary electoral participation, which is frequently the very avenue foreclosed to them as minorities. Future courts and future minorities affected by precedent-setting overrides are not yet constituted as parties and cannot object at all.
% DISAPPEARANCE_RATIONALE: If legislative override authority vanished overnight, constitutional courts would become the terminal interpreter of contested constitutional questions, judicial rulings would become effectively self-executing against the legislature, and the political incentive structure around court appointments and constitutional litigation would shift dramatically toward juridical strategy over legislative strategy — a substantial rearrangement of where political contestation is fought.
% FOUNDING_PROBLEM: In a constitutional order with judicial review, an unelected judiciary can strike down or narrow legislation passed by a democratically accountable body, raising the question of who should have the last word when reasonable interpreters disagree about constitutional meaning — the override mechanism was built to preserve a democratic check on judicial power.
% FOUNDING_PROBLEM_CORROBORATION: Legislators and parliamentary sovereignty theorists attest the problem is live: they point to instances of judicial rulings striking down popular, arguably constitutional legislation on contestable interpretive grounds. Independent comparative constitutional scholars and civil liberties organizations outside the legislative beneficiary class attest that in practice the mechanism is invoked asymmetrically — disproportionately to reverse rulings protecting minorities and dissenters rather than to correct genuine judicial overreach against majoritarian policy — suggesting the founding justification and the actual pattern of use have diverged.
narrative_ontology:disappearance_verdict(basic_law_interpretive_authority__parliamentary_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_authority__parliamentary_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_authority__parliamentary_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_authority__parliamentary_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_authority__parliamentary_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate-high 0.52 at interval end: the mechanism has a genuine coordination function (resolving disagreement about who has final say) but its actual operation channels a real transfer away from rights-vindicated minorities toward the sitting majority whenever override is invoked, and this pattern has strengthened over the interval as override use has become more normalized in jurisdictions that adopted notwithstanding-style mechanisms. Suppression (0.48) reflects that the mechanism does require active procedural machinery — vote thresholds, re-enactment clauses, sunset-and-renewal requirements — to remain available and credible as a check; it is not self-executing. Theater ratio is comparatively low (0.28) because the override function, when invoked, has real legislative and judicial consequences — it is not merely symbolic — though a rising theater component reflects growing use of the override threat as political signaling without follow-through.
 *
 * DIRECTIONALITY LOGIC:
 *   The elected legislature and the governing majority sit at the beneficiary end: they administer the override and are its most direct beneficiaries in institutional-authority terms. The constitutional court is a structural payer — institutionally powerful in the abstract but constrained here because it cannot decline the subordination the mechanism imposes and cannot exit the relationship. Rights-bearing minorities are the clearest targets: powerless, trapped (their exit option — winning majority support — is often foreclosed by the very fact of being a minority), and bearing the sharpest cost when override reverses a favorable ruling. This is a textbook case where directionality tracks structural position rather than nominal institutional power: the court is 'institutional' power but still a payer, because power atom and directional benefit are distinct axes.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing indefinite judicial veto over legitimate democratic policy) remains genuinely live in some cases — courts do sometimes read contestable meanings into ambiguous text. Classifying this as tangled_rope rather than snare or rope prevents two mislabeling errors: labeling it a pure rope would erase the asymmetric cost borne by rights-minorities when override is used to reverse protective rulings; labeling it a pure snare would erase the genuine, non-trivial coordination function of resolving interpretive disagreement between two legitimately constituted bodies. The tangled_rope classification holds both facts simultaneously and requires the engine to weigh them via the structural data rather than resolving the tension by fiat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    override_frequency_determines_reading_validity,
    'Does the empirical frequency and pattern of override invocation in real jurisdictions (Canada''s notwithstanding clause, UK sovereignty doctrine, Israeli override debates) vindicate the parliamentary sovereignty reading''s founding justification (correcting genuine judicial overreach) or the rival readings'' critique (entrenching majoritarian policy against minority rights)?',
    'Comparative empirical study coding each override invocation by whether the underlying judicial ruling protected a minority/dissenting interest versus corrected genuine judicial overreach into policy territory, across all jurisdictions with codified override mechanisms.',
    'If overrides disproportionately reverse minority-protective rulings, the tangled_rope classification''s extraction component is empirically substantiated and the claimed_type may understate ε; if overrides are evenly distributed or genuinely correct judicial overreach, the coordination function is stronger than authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(override_frequency_determines_reading_validity, empirical, 'Whether empirical override patterns support the coordination or extraction reading of the mechanism.').

omega_variable(
    kernel_framing_committer_structure,
    'This constraint is one of three readings of the basic_law_interpretive_authority kernel. Which reading a jurisdiction''s constitutional text and practice actually instantiate is itself contested — is the disagreement located in the constitutional text''s silence on finality, in the political culture''s tolerance for judicial power, or in the specific override mechanism''s design (threshold height, sunset requirements, re-enactment procedure)?',
    'Comparative constitutional textual analysis identifying whether jurisdictions with explicit override clauses (parliamentary_sovereignty_reading) differ structurally from jurisdictions where judicial supremacy is inferred from silence (judicial_supremacy_reading) or where doctrine leaves the question permanently open to political contestation (popular_constitutionalism_reading).',
    'If the disagreement is located in explicit textual design (override clause present or absent), the three readings are jurisdiction-specific facts rather than genuinely competing interpretive theories of the same text — this would mean the ''kernel'' is better modeled as several kernels. If located in political culture or doctrine, the three readings remain genuinely competing accounts of the same underdetermined kernel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_committer_structure, conceptual, 'Where the reading disagreement is structurally located: text, culture, or mechanism design.').

omega_variable(
    minority_exit_option_endogeneity,
    'Is the ''trapped'' exit option assigned to rights_bearing_minorities an accurate structural fact, or does it partly depend on which specific minority and which specific right is at issue — some minorities retain meaningful electoral leverage (swing constituencies) while others (permanent numerical minorities, non-citizens, incarcerated populations) do not?',
    'Disaggregate the rights_bearing_minorities stakeholder by electoral leverage: swing-relevant minorities versus structurally permanent minorities, and re-assess exit_options per subgroup.',
    'If exit options vary substantially within the victim group, a single aggregated stakeholder entry understates the extraction borne by the most powerless subgroup and overstates it for electorally consequential minorities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_exit_option_endogeneity, empirical, 'Whether the aggregated minority victim group masks internal variation in exit options.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(basi_tr_t8, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement(basi_tr_t16, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 16, 0.19).
narrative_ontology:measurement(basi_tr_t24, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement(basi_tr_t32, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 32, 0.25).
narrative_ontology:measurement(basi_tr_t40, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(basi_be_t8, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 8, 0.39).
narrative_ontology:measurement(basi_be_t16, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 16, 0.44).
narrative_ontology:measurement(basi_be_t24, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 24, 0.47).
narrative_ontology:measurement(basi_be_t32, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 32, 0.5).
narrative_ontology:measurement(basi_be_t40, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 40, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t0, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(basi_su_t8, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 8, 0.35).
narrative_ontology:measurement(basi_su_t16, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 16, 0.4).
narrative_ontology:measurement(basi_su_t24, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 24, 0.43).
narrative_ontology:measurement(basi_su_t32, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 32, 0.46).
narrative_ontology:measurement(basi_su_t40, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 40, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_authority__parliamentary_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__parliamentary_sovereignty_reading, basic_law_interpretive_authority__judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__parliamentary_sovereignty_reading, basic_law_interpretive_authority__popular_constitutionalism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the basic_law_interpretive_authority kernel, decomposed per the ε-invariance principle because the natural-language claim ('who has final interpretive authority') conflates structurally distinct institutional arrangements with different beneficiary/victim sets and different ε. The judicial_supremacy_reading places courts as agenda-setters and (in this reading's own accounting) treats override risk to democratic accountability as its cost; the popular_constitutionalism_reading treats terminal adjudication itself as the extraction mechanism, with ongoing contestation as the coordination good. Each reading is authored as its own file with its own metrics; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
