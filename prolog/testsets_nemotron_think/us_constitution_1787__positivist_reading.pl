% ============================================================================
% CONSTRAINT STORY: us_constitution_1787__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: US Constitution 1787 — Positivist Reading (Text + Democratic Amendments Only)
 *   domain: constitutional_law/legal_theory/political_philosophy
 *
 * SUMMARY:
 *   The positivist reading of the US Constitution holds that constitutional
 *   meaning is exhausted by the semantic content of the ratified text plus
 *   formal amendments adopted through Article V. Judicial interpretation is
 *   constrained to applying that textual meaning — no evolving standards, no
 *   penumbras, no living constitution. This reading presents itself as the
 *   only democratically legitimate approach: if the Constitution is law, it
 *   means what it says until the people change it through the prescribed
 *   supermajority process. The constraint operates on judges (who lose
 *   discretion), minorities (who lose judicial protection for unenumerated
 *   rights), and living constitutionalists (who are excluded from the
 *   interpretive framework). It benefits democratic majorities and
 *   legislatures by reserving constitutional change to them. The claimed type
 *   is 'rope' — a genuine coordination mechanism for democratic legitimacy —
 *   but the metrics reveal moderate extraction from judicial power and
 *   minority protection, with rising suppression as the amendment process
 *   becomes functionally inoperable.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_1787__positivist_reading, 0.38).
domain_priors:suppression_score(us_constitution_1787__positivist_reading, 0.25).
domain_priors:theater_ratio(us_constitution_1787__positivist_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_1787__positivist_reading, rope).
narrative_ontology:human_readable(us_constitution_1787__positivist_reading, "US Constitution 1787 — Positivist Reading (Text + Democratic Amendments Only)").
narrative_ontology:topic_domain(us_constitution_1787__positivist_reading, "constitutional_law/legal_theory/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_1787__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_1787__positivist_reading, '36285e5b-56af-4e5b-87c7-eb73ec72ae5c').
narrative_ontology:cs_kernel_codification('36285e5b-56af-4e5b-87c7-eb73ec72ae5c', formalized).
narrative_ontology:cs_authority_grounding('36285e5b-56af-4e5b-87c7-eb73ec72ae5c', lineage).
narrative_ontology:cs_interpretation_layer_present('36285e5b-56af-4e5b-87c7-eb73ec72ae5c').
narrative_ontology:cs_reading_relation('36285e5b-56af-4e5b-87c7-eb73ec72ae5c', us_constitution_1787__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('36285e5b-56af-4e5b-87c7-eb73ec72ae5c', us_constitution_1787__living_reading, forecloses).
narrative_ontology:cs_axiom('36285e5b-56af-4e5b-87c7-eb73ec72ae5c', foundational, constitutional_meaning_equals_text_plus_amendments).
narrative_ontology:cs_axiom_status(constitutional_meaning_equals_text_plus_amendments, holdable).
narrative_ontology:cs_axiom_grounding('36285e5b-56af-4e5b-87c7-eb73ec72ae5c', constitutional_meaning_equals_text_plus_amendments, conventional).
narrative_ontology:cs_axiom('36285e5b-56af-4e5b-87c7-eb73ec72ae5c', foundational, judicial_interpretation_constrained_to_text).
narrative_ontology:cs_axiom_status(judicial_interpretation_constrained_to_text, holdable).
narrative_ontology:cs_axiom_grounding('36285e5b-56af-4e5b-87c7-eb73ec72ae5c', judicial_interpretation_constrained_to_text, conventional).
narrative_ontology:cs_reference_frame('36285e5b-56af-4e5b-87c7-eb73ec72ae5c', textual_democratic_legitimacy).
narrative_ontology:cs_drift_state('36285e5b-56af-4e5b-87c7-eb73ec72ae5c', contemporary_judicial_review_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('36285e5b-56af-4e5b-87c7-eb73ec72ae5c', '').
narrative_ontology:cs_kernel_id(us_constitution_1787__positivist_reading, us_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, democratic_majorities).
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, legislative_branch).
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, textualist_citizens).
narrative_ontology:constraint_victim(us_constitution_1787__positivist_reading, federal_judges).
narrative_ontology:constraint_victim(us_constitution_1787__positivist_reading, minority_rights_advocates).
narrative_ontology:constraint_victim(us_constitution_1787__positivist_reading, living_constitutionalist_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, originalist_judges).
narrative_ontology:constraint_vindicates(us_constitution_1787__positivist_reading, democratic_legitimacy_requires_textual_constraint).
narrative_ontology:constraint_vindicates(us_constitution_1787__positivist_reading, amendment_process_is_sufficient_for_constitutional_change).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain constitutional stability and democratic control: the Constitution means what its text says plus what supermajorities formally amend. Their policy preferences cannot be overridden by judicial invention, but they must achieve supermajority consensus to change the Constitution. Exit from this constraint means accepting judicial supremacy or extra-constitutional politics.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, democratic_majorities, beneficiary,
    organized, generational, constrained, national).

% Congress proposes amendments and sets statutory policy within the textual envelope. The constraint empowers legislative primacy by denying courts authority to update constitutional meaning. But legislators are also constrained: they cannot evade textual limits by creative interpretation. Their exit is constitutional hardball (court-packing, jurisdiction stripping) which carries high political cost.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, legislative_branch, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__positivist_reading, legislative_branch, agenda_setter).

% Lose interpretive discretion. Under this reading, judges may not import evolving standards, policy preferences, or natural law into constitutional meaning. Their role is exhausted by textual analysis and amendment recognition. Exit means embracing a rival reading (originalism, living constitutionalism) — which many do — but doing so violates the positivist constraint they are sworn to uphold.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, federal_judges, payer,
    powerful, biographical, constrained, national).

% Depend on judicial enforcement of rights not explicitly enumerated or not yet amended in (e.g., privacy, dignity, equality beyond textual floor). The positivist reading denies courts authority to recognize such rights. Their exit is the amendment process — which requires the very majorities whose power the constraint empowers. They are structurally trapped unless supermajorities cooperate.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, minority_rights_advocates, payer,
    moderate, generational, trapped, national).

% Argue that constitutional meaning must evolve with society's values and conditions. They are excluded from the positivist framework because their core premise (evolutionary meaning) contradicts the positivist premise (text + amendments only). They operate in academia, litigation strategy, and public discourse, seeking to shift the interpretive consensus. Their exit is intellectual — they simply reject the constraint's legitimacy.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, living_constitutionalist_scholars, excluded,
    moderate, biographical, mobile, national).

% Share the positivist commitment to textual constraint but disagree on method: they bind meaning to original public meaning at ratification, not current textual semantics. They benefit from the positivist constraint's anti-living-constitutionalism effect but diverge on whether amendments are the only post-ratification change mechanism. They can exit to originalism without leaving the textualist coalition.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, originalist_judges, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__positivist_reading, originalist_judges, observer).

% Study how different constitutional systems handle interpretation, amendment, and judicial review. They see the positivist reading as one stable equilibrium among many (e.g., German Basic Law's eternity clauses, UK's parliamentary sovereignty). They have no stake in the US outcome but provide the external corroboration the six-questions demand.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, comparative_constitutionalists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the democratic legitimacy problem for a written constitution: how can a document bind future generations while remaining democratically legitimate? Answer: by limiting meaning to the enacted text plus formal supermajority amendments, it makes constitutional change a democratic act rather than a judicial one. Coordinates legislative majorities, judicial restraint, and citizen expectation around a single, knowable rule of recognition.
% TRANSFER_FUNCTION: Transfers interpretive authority from courts to the amendment process. Moves the power to update constitutional meaning from judicial discretion (incremental, case-by-case, countermajoritarian) to Article V supermajorities (episodic, deliberate, majoritarian). Judges lose the ability to 'update' the Constitution; democratic majorities gain the exclusive franchise to do so.
% ABSENT_VOICES: Future generations who will live under a Constitution they cannot amend (Article V's supermajority threshold is effectively insurmountable in polarized eras). They would object to being bound by 18th/19th-century textual settlements on issues the framers never contemplated (digital privacy, climate obligation, algorithmic governance). They are absent by definition — not yet born — and the constraint has no mechanism for their representation.
% DISAPPEARANCE_RATIONALE: If the positivist constraint vanished overnight, judicial review would immediately expand to fill the vacuum. Courts would resume recognizing unenumerated rights, updating constitutional standards to contemporary values, and invalidating legislation on evolving-standards grounds. The amendment process would atrophy further. The constitutional order would shift from text-anchored to practice-anchored — a different regime of legitimacy.
% FOUNDING_PROBLEM: How to make a written constitution binding on future officeholders without making it a dead hand of the past? The founding generation needed a constitution that could legitimate a new government, constrain power, and yet remain democratically legitimate across generations. Their solution: a text that means what it says, changeable only by supermajority consent through Article V.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the Federalist Papers (Madison, Hamilton) and the ratification debates — sources outside any modern beneficiary set. Contemporary corroboration: comparative constitutional scholars (e.g., Ackerman, Tushnet, Grimm) confirm the democratic legitimacy problem is real and the text+amendment solution is one historical answer. The status is contested because living constitutionalists argue the founding problem is misconceived (constitutions are frameworks, not codes) and originalists argue the solution failed (Article V is broken, so courts must maintain original meaning).
narrative_ontology:disappearance_verdict(us_constitution_1787__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_1787__positivist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_1787__positivist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_1787__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_1787__positivist_reading, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.38) reflects the transfer of interpretive authority from courts to a supermajority process that rarely functions. Suppression (0.25) is low in absolute terms — no one is jailed for advocating living constitutionalism — but structurally significant: the constraint excludes an entire interpretive tradition from legitimate practice. Theater ratio (0.12) is low because courts genuinely do constrain themselves to text (e.g., Dobbs, Bruen, NFIB) rather than performing textualism while smuggling in policy. Accessibility collapse (0.45) is moderate: the living constitutionalist alternative persists in academia and dissent but cannot capture the institutional center. Resistance (0.55) is substantial: the living constitutionalist coalition remains organized, the Warren/Burger Court precedent library is vast, and the constraint requires continuous judicial discipline to maintain.
 *
 * PERSPECTIVAL GAP:
 *   From the democratic majority seat, this is a rope — pure coordination solving the legitimacy problem. From the federal judge seat, it is a snare — extraction of discretion without compensation. From the minority rights advocate seat, it is a tangled rope — coordination for majorities, extraction from minorities. The engine computes this divergence from the structural data: the same constraint occupies different types in different seats. The authored claim (rope) reflects the majority seat; the metrics capture the cross-seat reality.
 *
 * DIRECTIONALITY LOGIC:
 *   Democratic majorities and legislatures are structural beneficiaries (d ~0.15): they gain the exclusive constitutional change franchise. Federal judges are structural payers (d ~0.85): they lose the interpretive discretion that makes judicial review powerful. Minority rights advocates are trapped payers (d ~0.95): their exit (amendment) is blocked by the very majorities the constraint empowers. Living constitutionalist scholars are excluded (d undefined): they reject the constraint's legitimacy entirely. Originalist judges are mobile beneficiaries (d ~0.20): they share the anti-living-constitutionalism payoff but keep their own interpretive method.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (democratic legitimacy of a binding constitution) remains live — every constitutional system faces it. But the positivist solution (text + Article V) shows mandatrophy symptoms: Article V is functionally paralyzed (no amendment since 1992, none close), so the constraint persists by extracting from judges and minorities without delivering its coordination payoff (democratic constitutional change). The constraint is maintained theatrically (originalist opinions, textualist rhetoric) while its functional justification has atrophied. This is not yet a piton because the coordination function (legitimacy) is still actively invoked and the constraint still shapes judicial behavior — but the drift is toward piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    positivist_reading_of_kernel,
    'Is the positivist reading a distinct constraint from the originalist and living readings, or a variant of one of them?',
    'Compare the beneficiary/victim structures and extraction profiles: if the positivist reading produces a different χ vector across seats than either sibling, it is a distinct constraint per ε-invariance.',
    'If distinct, the kernel us_constitution_1787 decomposes into three constraint stories linked by network.affects_constraints. If not distinct, this story should be merged with the closest sibling.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(positivist_reading_of_kernel, conceptual, 'Commitment-system frame: whether this reading instantiates a separate constraint').

omega_variable(
    article_v_paralysis_extraction,
    'Does Article V''s functional paralysis convert the positivist constraint from rope to tangled_rope or piton?',
    'Track amendment frequency and judicial behavior over time. If extraction from judges/minorities rises while amendment frequency falls to zero, the coordination function is atrophying.',
    'Reclassification from rope to tangled_rope (if coordination persists but extraction rises) or piton (if coordination is theatrical). Triggers T17 mountain_extraction_accumulation if claimed_type remains rope while metrics drift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_v_paralysis_extraction, empirical, 'Mandatrophy dynamics: whether the constraint''s functional justification has decayed').

omega_variable(
    minority_rights_victim_status,
    'Are minority rights advocates genuine victims of the positivist constraint, or do they benefit from the stability it provides?',
    'Counterfactual: in a living constitutionalist regime, would minority rights be more or less secure? Historical comparison: Warren Court (living) vs. Rehnquist/Roberts Courts (positivist/originalist) on minority rights outcomes.',
    'If victims, the constraint is at minimum tangled_rope (coordination + asymmetric extraction). If net beneficiaries, it may be a genuine rope. Affects victim declaration and six-questions absent_voices.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(minority_rights_victim_status, preference, 'Whether the constraint''s extraction from minorities is net harmful or compensated by stability').

omega_variable(
    suppression_mechanism_judicial_discipline,
    'Is the constraint''s suppression of living constitutionalism structural (institutional barriers) or internalized (judges genuinely believe textualism)?',
    'Post-exit trajectory: when judges appointed as textualists drift toward living constitutionalism (e.g., Souter, Stevens, Kennedy), does the constraint''s suppression persist? If yes, internalized; if no, structural.',
    'If internalized, suppression is higher than structural measure suggests — the constraint has colonized the judicial psyche. Affects omega on suppression ambiguity and directional override for federal_judges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_judicial_discipline, empirical, 'Structural vs. internalized suppression in judicial interpretation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_1787__positivist_reading, 0, 237).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_1787__positivist_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(us_c_tr_t0, observed).
narrative_ontology:measurement(us_c_tr_t50, us_constitution_1787__positivist_reading, theater_ratio, 50, 0.06).
narrative_ontology:measurement_basis(us_c_tr_t50, observed).
narrative_ontology:measurement(us_c_tr_t80, us_constitution_1787__positivist_reading, theater_ratio, 80, 0.08).
narrative_ontology:measurement_basis(us_c_tr_t80, observed).
narrative_ontology:measurement(us_c_tr_t100, us_constitution_1787__positivist_reading, theater_ratio, 100, 0.09).
narrative_ontology:measurement_basis(us_c_tr_t100, observed).
narrative_ontology:measurement(us_c_tr_t150, us_constitution_1787__positivist_reading, theater_ratio, 150, 0.1).
narrative_ontology:measurement_basis(us_c_tr_t150, observed).
narrative_ontology:measurement(us_c_tr_t170, us_constitution_1787__positivist_reading, theater_ratio, 170, 0.11).
narrative_ontology:measurement_basis(us_c_tr_t170, observed).
narrative_ontology:measurement(us_c_tr_t200, us_constitution_1787__positivist_reading, theater_ratio, 200, 0.11).
narrative_ontology:measurement_basis(us_c_tr_t200, observed).
narrative_ontology:measurement(us_c_tr_t237, us_constitution_1787__positivist_reading, theater_ratio, 237, 0.12).
narrative_ontology:measurement_basis(us_c_tr_t237, observed).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_1787__positivist_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(us_c_be_t0, observed).
narrative_ontology:measurement(us_c_be_t50, us_constitution_1787__positivist_reading, base_extractiveness, 50, 0.22).
narrative_ontology:measurement_basis(us_c_be_t50, observed).
narrative_ontology:measurement(us_c_be_t80, us_constitution_1787__positivist_reading, base_extractiveness, 80, 0.28).
narrative_ontology:measurement_basis(us_c_be_t80, observed).
narrative_ontology:measurement(us_c_be_t100, us_constitution_1787__positivist_reading, base_extractiveness, 100, 0.31).
narrative_ontology:measurement_basis(us_c_be_t100, observed).
narrative_ontology:measurement(us_c_be_t150, us_constitution_1787__positivist_reading, base_extractiveness, 150, 0.35).
narrative_ontology:measurement_basis(us_c_be_t150, observed).
narrative_ontology:measurement(us_c_be_t170, us_constitution_1787__positivist_reading, base_extractiveness, 170, 0.37).
narrative_ontology:measurement_basis(us_c_be_t170, observed).
narrative_ontology:measurement(us_c_be_t200, us_constitution_1787__positivist_reading, base_extractiveness, 200, 0.38).
narrative_ontology:measurement_basis(us_c_be_t200, observed).
narrative_ontology:measurement(us_c_be_t237, us_constitution_1787__positivist_reading, base_extractiveness, 237, 0.38).
narrative_ontology:measurement_basis(us_c_be_t237, observed).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_1787__positivist_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement_basis(us_c_su_t0, observed).
narrative_ontology:measurement(us_c_su_t50, us_constitution_1787__positivist_reading, suppression_requirement, 50, 0.15).
narrative_ontology:measurement_basis(us_c_su_t50, observed).
narrative_ontology:measurement(us_c_su_t80, us_constitution_1787__positivist_reading, suppression_requirement, 80, 0.2).
narrative_ontology:measurement_basis(us_c_su_t80, observed).
narrative_ontology:measurement(us_c_su_t100, us_constitution_1787__positivist_reading, suppression_requirement, 100, 0.22).
narrative_ontology:measurement_basis(us_c_su_t100, observed).
narrative_ontology:measurement(us_c_su_t150, us_constitution_1787__positivist_reading, suppression_requirement, 150, 0.24).
narrative_ontology:measurement_basis(us_c_su_t150, observed).
narrative_ontology:measurement(us_c_su_t170, us_constitution_1787__positivist_reading, suppression_requirement, 170, 0.25).
narrative_ontology:measurement_basis(us_c_su_t170, observed).
narrative_ontology:measurement(us_c_su_t200, us_constitution_1787__positivist_reading, suppression_requirement, 200, 0.25).
narrative_ontology:measurement_basis(us_c_su_t200, observed).
narrative_ontology:measurement(us_c_su_t237, us_constitution_1787__positivist_reading, suppression_requirement, 237, 0.25).
narrative_ontology:measurement_basis(us_c_su_t237, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_1787__positivist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_1787__positivist_reading, 0.1).
narrative_ontology:affects_constraint(us_constitution_1787__positivist_reading, us_constitution_1787__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_1787__positivist_reading, us_constitution_1787__living_reading).

% DUAL FORMULATION NOTE:
% This constraint family (us_constitution_1787) decomposes the single kernel into three readings with distinct ε values: originalist_reading (low extractiveness, Mountain-like stability), positivist_reading (moderate extractiveness, Rope coordination), living_reading (high extractiveness, Tangled Rope with contested coordination). The positivist reading influences both siblings: it provides the textualist vocabulary originalists use and the democratic legitimacy rhetoric living constitutionalists must answer.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_1787__positivist_reading, powerful, 0.85).
constraint_indexing:directionality_override(us_constitution_1787__positivist_reading, moderate, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
