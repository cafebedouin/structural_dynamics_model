% ============================================================================
% CONSTRAINT STORY: us_constitution_meaning__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_meaning__originalist_reading, []).

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
 *   constraint_id: us_constitution_meaning__originalist_reading
 *   human_readable: Originalist Constitutional Interpretation Constraint
 *   domain: legal/political/philosophical
 *
 * SUMMARY:
 *   This constraint story instantiates the originalist_reading of the
 *   us_constitution_meaning kernel. The originalist reading holds that
 *   constitutional meaning was fixed at the moment of ratification (1788 for
 *   the original Constitution, respective amendment dates for amendments) and
 *   that judges are bound by the historical public meaning of the text.
 *   Contemporary circumstances, evolving social attitudes, and modern moral
 *   judgments are irrelevant to constitutional meaning (though they may be
 *   relevant to application of fixed principles to new facts). The constraint
 *   operates through judicial appointments, law school curricula, Federalist
 *   Society networks, and stare decisis pressure to enforce originalist
 *   methodology. It presents itself as a coordination mechanism — solving the
 *   problem of judicial discretion by binding interpretation to objective
 *   historical evidence — but extracts asymmetrically: it empowers
 *   counter-majoritarian constraint advocates while suppressing rights claims
 *   lacking 18th-century historical pedigree.
 *
 * KEY AGENTS:
 *   - counter_majoritarian_constraint_advocates: Primary beneficiary (institutional/arbitrage) — gain interpretive authority and policy outcomes
 *   - rights_claimants_without_historical_support: Primary victim (powerless/trapped) — bear the cost of foreclosed constitutional arguments
 *   - originalist_judges: Agenda setter (institutional/identity_locked) — administer and enforce the constraint
 *   - living_constitutionalist_judges_scholars: Victim (organized/constrained) — their interpretive approach is structurally suppressed
 *   - analytical_observer: Observer (analytical/analytical) — sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_meaning__originalist_reading, 0.68).
domain_priors:suppression_score(us_constitution_meaning__originalist_reading, 0.79).
domain_priors:theater_ratio(us_constitution_meaning__originalist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_meaning__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_meaning__originalist_reading, "Originalist Constitutional Interpretation Constraint").
narrative_ontology:topic_domain(us_constitution_meaning__originalist_reading, "legal/political/philosophical").

domain_priors:requires_active_enforcement(us_constitution_meaning__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_meaning__originalist_reading, 'f3f93a0c-fd56-4141-aadb-19aaabb557c8').
narrative_ontology:cs_kernel_codification('f3f93a0c-fd56-4141-aadb-19aaabb557c8', fixed_text).
narrative_ontology:cs_authority_grounding('f3f93a0c-fd56-4141-aadb-19aaabb557c8', lineage).
narrative_ontology:cs_interpretation_layer_present('f3f93a0c-fd56-4141-aadb-19aaabb557c8').
narrative_ontology:cs_reading_relation('f3f93a0c-fd56-4141-aadb-19aaabb557c8', us_constitution_meaning__living_constitutionalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('f3f93a0c-fd56-4141-aadb-19aaabb557c8', us_constitution_meaning__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('f3f93a0c-fd56-4141-aadb-19aaabb557c8', foundational, meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('f3f93a0c-fd56-4141-aadb-19aaabb557c8', meaning_fixed_at_ratification, conventional).
narrative_ontology:cs_axiom('f3f93a0c-fd56-4141-aadb-19aaabb557c8', foundational, judges_bound_by_historical_public_meaning).
narrative_ontology:cs_axiom_status(judges_bound_by_historical_public_meaning, holdable).
narrative_ontology:cs_axiom_grounding('f3f93a0c-fd56-4141-aadb-19aaabb557c8', judges_bound_by_historical_public_meaning, conventional).
narrative_ontology:cs_axiom('f3f93a0c-fd56-4141-aadb-19aaabb557c8', secondary, contemporary_circumstances_irrelevant_to_meaning).
narrative_ontology:cs_axiom_status(contemporary_circumstances_irrelevant_to_meaning, holdable).
narrative_ontology:cs_axiom_grounding('f3f93a0c-fd56-4141-aadb-19aaabb557c8', contemporary_circumstances_irrelevant_to_meaning, conventional).
narrative_ontology:cs_axiom('f3f93a0c-fd56-4141-aadb-19aaabb557c8', secondary, article_v_as_exclusive_change_mechanism).
narrative_ontology:cs_axiom_status(article_v_as_exclusive_change_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('f3f93a0c-fd56-4141-aadb-19aaabb557c8', article_v_as_exclusive_change_mechanism, conventional).
narrative_ontology:cs_reference_frame('f3f93a0c-fd56-4141-aadb-19aaabb557c8', founding_era_public_meaning).
narrative_ontology:cs_drift_state('f3f93a0c-fd56-4141-aadb-19aaabb557c8', contemporary_originalist_dominance, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f3f93a0c-fd56-4141-aadb-19aaabb557c8', '').
narrative_ontology:cs_kernel_id(us_constitution_meaning__originalist_reading, us_constitution_meaning).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, counter_majoritarian_constraint_advocates).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, judicial_restraint_proponents).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, state_sovereignty_advocates).
narrative_ontology:constraint_victim(us_constitution_meaning__originalist_reading, rights_claimants_without_historical_support).
narrative_ontology:constraint_victim(us_constitution_meaning__originalist_reading, marginalized_groups_seeking_expanded_protections).
narrative_ontology:constraint_victim(us_constitution_meaning__originalist_reading, living_constitutionalist_judges_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, originalist_judges).
narrative_ontology:constraint_vindicates(us_constitution_meaning__originalist_reading, democratic_legitimacy_requires_fixed_meaning).
narrative_ontology:constraint_vindicates(us_constitution_meaning__originalist_reading, judicial_restraint_prevents_legislating_from_bench).
narrative_ontology:constraint_vindicates(us_constitution_meaning__originalist_reading, article_v_amendment_is_proper_change_mechanism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain interpretive authority and constitutional outcomes aligned with limited government, federalism, and property rights. Control judicial appointment pipelines, fund originalist scholarship, and populate the judiciary. Can move between government, academia, think tanks, and private practice — their professional capital is portable across the originalist ecosystem.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, counter_majoritarian_constraint_advocates, beneficiary,
    institutional, generational, arbitrage, national).

% Bear the cost of foreclosed constitutional arguments: claims for reproductive autonomy, LGBTQ+ rights, positive economic rights, voting rights protections, and criminal procedure protections that lack 1788/1868-era historical support. No exit from the constitutional system; must litigate within originalist framework or abandon claims. Their exclusion is structural — the methodology itself denies their claims' constitutional status.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, rights_claimants_without_historical_support, payer,
    powerless, biographical, trapped, national).

% Organized advocacy groups (civil rights organizations, reproductive rights groups, LGBTQ+ advocacy) that can litigate and mobilize politically but face a constitutional ceiling imposed by originalist methodology. Their exit is constrained: they can seek legislative remedies but constitutional claims are structurally blocked. Some pursue state constitutional litigation as partial exit.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, marginalized_groups_seeking_expanded_protections, payer,
    moderate, generational, constrained, national).

% Administer and enforce the originalist constraint through opinions, hiring, and institutional leadership. Professional identity is fused with originalist methodology — becoming a judge *as an originalist* means the methodology constitutes the judicial self. Exit would mean abandoning professional identity and the institutional position that depends on it. They benefit from the constraint's legitimacy and the professional ecosystem it sustains.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, originalist_judges, agenda_setter,
    institutional, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_meaning__originalist_reading, originalist_judges, beneficiary).

% Their interpretive methodology is structurally suppressed: originalist dominance in appointments, clerkship pipelines, and Supreme Court doctrine marginalizes living constitutionalist approaches. Professional identity is fused with living constitutionalist methodology — exit means abandoning the interpretive self. Some remain in academia or lower courts but with diminished influence on constitutional doctrine.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, living_constitutionalist_judges_scholars, payer,
    organized, biographical, identity_locked, national).

% Gain constitutional constraints on federal power through originalist federalism doctrines (anti-commandeering, sovereign immunity, enumerated powers limits). Their exit is constrained by professional and political commitments to the federalism project, but they have alternative venues (state courts, political branches) if originalism wanes.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, state_sovereignty_advocates, beneficiary,
    organized, generational, constrained, national).

% Observes the full structural relationship: the kernel us_constitution_meaning admits multiple constraint instantiations; the originalist reading extracts from rights claimants while coordinating judicial discretion for originalist advocates; the living constitutionalist and positivist readings would instantiate different extraction/coordination patterns. Sees the seat divergence the engine computes.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of judicial discretion unbounded by democratic text: provides an objective, historically grounded methodology that constrains judges to the meaning the ratifying public understood, preventing judges from imposing their own policy preferences under the guise of interpretation.
% TRANSFER_FUNCTION: Moves constitutional interpretive authority and doctrinal outcomes from rights claimants (whose claims lack 1788/1868-era historical support) and living constitutionalist interpreters to originalist judges, scholars, and the policy coalitions they empower (federalism, property rights, limited government, anti-administrative state). The transfer is effected through judicial appointments, stare decisis, and professional gatekeeping.
% ABSENT_VOICES: Future generations who will live under constitutional doctrine they had no role in ratifying; non-lawyer citizens whose constitutional understandings are mediated by an expert priesthood; international human rights norms that originalism excludes as irrelevant to domestic constitutional meaning. These voices are structurally excluded — the constraint's methodology (historical public meaning at ratification) by design cannot incorporate them.
% DISAPPEARANCE_RATIONALE: If the originalist constraint vanished overnight, constitutional doctrine would shift dramatically: reproductive rights, voting rights, affirmative action, administrative state legitimacy, and federalism doctrine would all be reopened. Rights claimants would advance claims currently foreclosed. Judicial appointments would shift criteria. The Federalist Society ecosystem would lose its central organizing methodology. The constitutional order would rearrange around living constitutionalist or positivist frameworks.
% FOUNDING_PROBLEM: Post-New Deal/Warren Court era: perceived judicial activism where courts imposed policy preferences (incorporation doctrine, substantive due process, expanded criminal procedure rights) without textual or historical warrant, undermining democratic legitimacy and federalism.
% FOUNDING_PROBLEM_CORROBORATION: Originalist founders (Bork, Scalia, Meese) attested the problem was live in the 1970s-80s. Living constitutionalist scholars (Brennan, Dworkin, Tribe) and Warren Court defenders attested the problem was mischaracterized — the Court was enforcing constitutional principles against democratic majorities, not legislating. Modern empirical legal historians (Kramer, Amar, Balkin) attest the historical record is more indeterminate than originalism claims. No consensus outside the beneficiary set.
narrative_ontology:disappearance_verdict(us_constitution_meaning__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_meaning__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_meaning__originalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(us_constitution_meaning__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_meaning__originalist_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_meaning__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_meaning__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_meaning__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is substantial: the constraint transfers interpretive authority and constitutional outcomes from rights claimants and living constitutionalist actors to originalist actors. Suppression (0.79) is high: non-originalist outcomes are suppressed through judicial selection mechanisms, professional gatekeeping, and doctrinal pressure. Theater ratio (0.22) is moderate-low: the historical methodology is genuine (not pure performance) but the constraint's persistence depends on active enforcement (judicial appointments, institutional pressure). Accessibility collapse (0.61) reflects that alternatives are substantially but not completely foreclosed — living constitutionalism remains a live academic and judicial position. Resistance (0.73) is high: the constraint meets active resistance from legal academia, civil rights organizations, and living constitutionalist judges. The claimed type is tangled_rope because the constraint has BOTH a genuine coordination function (constraining judicial discretion via historical evidence) AND asymmetric extraction (benefiting conservative/libertarian policy outcomes while burdening progressive rights claims).
 *
 * PERSPECTIVAL GAP:
 *   From the originalist judge/advocate seat, the constraint is experienced as genuine coordination — a necessary bulwark against judicial legislating. From the rights claimant seat, it is experienced as extraction — their claims are foreclosed not by democratic process but by an interpretive methodology they never consented to. From the living constitutionalist judge seat, it is experienced as suppression — their professional methodology is treated as illegitimate. The engine computes per-seat classifications from the structural data: beneficiaries (counter_majoritarian_constraint_advocates) get low directionality → low effective extraction; victims (rights_claimants_without_historical_support) get high directionality → high effective extraction; agenda setters (originalist_judges) sit near the target end due to identity_locked exit (professional identity fused with methodology).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: counter_majoritarian_constraint_advocates (institutional power, arbitrage exit — they control the judicial appointment pipeline and can move between government/academia/think tanks), judicial_restraint_proponents (organized power, constrained exit — professional reputation tied to methodology), state_sovereignty_advocates (organized power, constrained exit). Victims: rights_claimants_without_historical_support (powerless, trapped — no exit from constitutional system, claims foreclosed by methodology), marginalized_groups_seeking_expanded_protections (moderate power, constrained exit — can litigate but within constrained framework), living_constitutionalist_judges_scholars (organized power, identity_locked exit — professional identity fused with living constitutionalist methodology; leaving the methodology means leaving the professional self). The directionality derivation chain reads beneficiary/victim declarations + exit options: trapped/identity_locked victims → d near 1.0; arbitrage/constrained beneficiaries → d near 0.0-0.3.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (judicial discretion unbounded by text/history) remains contested — originalists attest it is live; living constitutionalists attest it is substantially solved or mischaracterized. The constraint has not resolved its mandatrophy: it continues to expand (rising extractiveness over 55 years) while its coordination justification (binding judges to history) becomes more contested as historical evidence proves indeterminate in key cases. The rising theater ratio (stabilizing at 0.22) suggests the coordination function is not atrophying into pure performance, but the extraction component is growing faster than the coordination component. This is a tangled_rope whose extraction is accumulating — not a piton (function not atrophied) and not a scaffold (no sunset clause, no transitional justification).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_ambiguity,
    'Is the originalist constraint a genuine natural law of legal interpretation, or a constructed constraint that benefits identifiable ideological and institutional actors?',
    'Historical analysis of whether originalist methodology was the dominant or even prevalent interpretive practice at the Founding; institutional analysis of which actors gain interpretive authority and policy outcomes under originalism vs. alternatives.',
    'If constructed, the constraint triggers false_summit_mountain reclassification to tangled_rope (beneficiaries declared). If natural, the high extractiveness/suppression metrics would need re-examination as measurement artifacts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_ambiguity, conceptual, 'Whether originalism is a discovered interpretive truth or an engineered constraint with beneficiaries').

omega_variable(
    historical_evidence_accessibility,
    'How accessible is genuine 1788/amendment-date public meaning to contemporary judges, and does the accessibility gap function as de facto judicial discretion?',
    'Empirical study of originalist opinions: frequency of genuine historical consensus vs. contested evidence; correlation between judges'' priors and ''historical findings''; comparison of originalist methodology across judges of different ideological orientations.',
    'If historical meaning is genuinely inaccessible and judges'' findings track their priors, the constraint''s coordination function (binding judges to history) is cover for extraction (binding judges to conservative outcomes). If accessible, the constraint may be a genuine rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(historical_evidence_accessibility, empirical, 'Whether the historical evidence constraint actually binds or merely channels judicial preferences').

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading of the us_constitution_meaning kernel. How does the originalist reading''s structural relationship to its sibling readings affect its classification?',
    'Compare the originalist reading''s beneficiary/victim structure, extractiveness, and suppression against the living constitutionalist and positivist readings. Determine whether the kernel admits multiple stable constraint instantiations or whether the readings are measuring the same underlying constraint.',
    'If the readings instantiate genuinely different constraints (different ε, different stakeholders), the kernel decomposition is valid. If they are the same constraint measured differently, ε-invariance is violated and the stories must be merged.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Commitment-system framing: this reading instantiates the originalist_reading of kernel us_constitution_meaning; sibling readings are living_constitutionalist_reading and positivist_reading').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the high suppression of non-originalist outcomes structural (formal doctrinal barriers, stare decisis pressure, confirmation politics) or internalized (professional identity fusion with originalist methodology, ideological commitment)?',
    'Track careers of judges/scholars who shift interpretive methodologies; measure professional consequences of methodological deviation; analyze whether suppression persists after formal barriers (e.g., academic freedom) are removed.',
    'If internalized, effective suppression is higher than structural measure suggests — the constraint travels with the agent. If structural, exit options (moving to academia, different jurisdiction) may partially mitigate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for non-originalist interpretive outcomes').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_meaning__originalist_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1970, us_constitution_meaning__originalist_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(us_c_tr_t1980, us_constitution_meaning__originalist_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(us_c_tr_t1986, us_constitution_meaning__originalist_reading, theater_ratio, 1986, 0.18).
narrative_ontology:measurement(us_c_tr_t1995, us_constitution_meaning__originalist_reading, theater_ratio, 1995, 0.2).
narrative_ontology:measurement(us_c_tr_t2005, us_constitution_meaning__originalist_reading, theater_ratio, 2005, 0.21).
narrative_ontology:measurement(us_c_tr_t2015, us_constitution_meaning__originalist_reading, theater_ratio, 2015, 0.22).
narrative_ontology:measurement(us_c_tr_t2025, us_constitution_meaning__originalist_reading, theater_ratio, 2025, 0.22).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1970, us_constitution_meaning__originalist_reading, base_extractiveness, 1970, 0.32).
narrative_ontology:measurement(us_c_be_t1980, us_constitution_meaning__originalist_reading, base_extractiveness, 1980, 0.41).
narrative_ontology:measurement(us_c_be_t1986, us_constitution_meaning__originalist_reading, base_extractiveness, 1986, 0.48).
narrative_ontology:measurement(us_c_be_t1995, us_constitution_meaning__originalist_reading, base_extractiveness, 1995, 0.55).
narrative_ontology:measurement(us_c_be_t2005, us_constitution_meaning__originalist_reading, base_extractiveness, 2005, 0.61).
narrative_ontology:measurement(us_c_be_t2015, us_constitution_meaning__originalist_reading, base_extractiveness, 2015, 0.65).
narrative_ontology:measurement(us_c_be_t2025, us_constitution_meaning__originalist_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1970, us_constitution_meaning__originalist_reading, suppression_requirement, 1970, 0.45).
narrative_ontology:measurement(us_c_su_t1980, us_constitution_meaning__originalist_reading, suppression_requirement, 1980, 0.52).
narrative_ontology:measurement(us_c_su_t1986, us_constitution_meaning__originalist_reading, suppression_requirement, 1986, 0.58).
narrative_ontology:measurement(us_c_su_t1995, us_constitution_meaning__originalist_reading, suppression_requirement, 1995, 0.65).
narrative_ontology:measurement(us_c_su_t2005, us_constitution_meaning__originalist_reading, suppression_requirement, 2005, 0.71).
narrative_ontology:measurement(us_c_su_t2015, us_constitution_meaning__originalist_reading, suppression_requirement, 2015, 0.76).
narrative_ontology:measurement(us_c_su_t2025, us_constitution_meaning__originalist_reading, suppression_requirement, 2025, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_meaning__originalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(us_constitution_meaning__originalist_reading, 0.08).
narrative_ontology:affects_constraint(us_constitution_meaning__originalist_reading, us_constitution_meaning__living_constitutionalist_reading).
narrative_ontology:affects_constraint(us_constitution_meaning__originalist_reading, us_constitution_meaning__positivist_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of us_constitution_meaning kernel. Originalist reading: high extraction/suppression, identity_coordination, beneficiaries = counter-majoritarian advocates, victims = rights claimants without historical support. Living constitutionalist reading (expected): lower extraction, higher coordination, beneficiaries = rights claimants, victims = democratic majorities constrained by judicial review. Positivist reading (expected): lowest extraction (procedural constraint only), enforcement_mechanism coordination, beneficiaries = institutional actors, victims = moral-reading claimants. The three readings are linked by network.affects_constraints forming a kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_meaning__originalist_reading, institutional, 0.15).
constraint_indexing:directionality_override(us_constitution_meaning__originalist_reading, powerless, 0.95).
constraint_indexing:directionality_override(us_constitution_meaning__originalist_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
