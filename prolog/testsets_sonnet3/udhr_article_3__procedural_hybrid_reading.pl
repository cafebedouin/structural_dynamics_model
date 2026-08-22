% ============================================================================
% CONSTRAINT STORY: udhr_article_3__procedural_hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_article_3__procedural_hybrid_reading, []).

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
 *   constraint_id: udhr_article_3__procedural_hybrid_reading
 *   human_readable: UDHR Article 3 — Procedural/Hybrid Due Process Reading
 *   domain: constitutional_law/human_rights/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the procedural/hybrid reading of Article 3 of the
 *   UDHR: the guarantee is understood to secure due process protections —
 *   habeas corpus and an absolute torture prohibition — without adjudicating
 *   the underlying liberty/welfare contest between negative-liberty and
 *   positive-entitlement readings. This is one of three siblings of the same
 *   kernel (udhr_article_3); the other two are separate constraint stories
 *   with different ε and different victim/beneficiary structures, per the
 *   ε-invariance principle. The hybrid reading's structural signature is
 *   moderate extraction: it delivers real judicial leverage where courts
 *   function, but its silence on substantive detention limits is exploited by
 *   states running emergency and immigration detention regimes, producing a
 *   genuine coordination function riding alongside a real extraction channel
 *   — hence tangled_rope rather than rope.
 *
 * KEY AGENTS:
 *   - detained_persons_with_access_to_courts: primary beneficiary where courts function (powerless/constrained)
 *   - detainees_in_non_derogable_emergency_regimes: primary victim of procedural hollowing (powerless/trapped)
 *   - stateless_and_undocumented_detainees: victim of jurisdictional threshold gaps (powerless/trapped)
 *   - domestic_judiciaries: agenda-setter administering the procedural floor (institutional/analytical)
 *   - states_administering_detention: agenda-setter/beneficiary that can claim compliance while narrowing substance (institutional/arbitrage)
 *   - torture_prohibition_treaty_bodies: beneficiary/observer whose jurisdiction depends on the reading's non-derogable core (institutional/analytical)
 *   - welfare_rights_advocates and security_hawks: excluded parties to the sibling disputes this reading brackets
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_article_3__procedural_hybrid_reading, 0.42).
domain_priors:suppression_score(udhr_article_3__procedural_hybrid_reading, 0.48).
domain_priors:theater_ratio(udhr_article_3__procedural_hybrid_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_article_3__procedural_hybrid_reading, tangled_rope).
narrative_ontology:human_readable(udhr_article_3__procedural_hybrid_reading, "UDHR Article 3 — Procedural/Hybrid Due Process Reading").
narrative_ontology:topic_domain(udhr_article_3__procedural_hybrid_reading, "constitutional_law/human_rights/political_philosophy").

domain_priors:requires_active_enforcement(udhr_article_3__procedural_hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_article_3__procedural_hybrid_reading, '7ca46e49-b0d5-46f0-ad4f-fd0a58700513').
narrative_ontology:cs_kernel_codification('7ca46e49-b0d5-46f0-ad4f-fd0a58700513', fixed_text).
narrative_ontology:cs_authority_grounding('7ca46e49-b0d5-46f0-ad4f-fd0a58700513', distributed).
narrative_ontology:cs_reading_relation('7ca46e49-b0d5-46f0-ad4f-fd0a58700513', udhr_article_3__negative_liberty_reading, coexists_with).
narrative_ontology:cs_reading_relation('7ca46e49-b0d5-46f0-ad4f-fd0a58700513', udhr_article_3__positive_entitlement_reading, coexists_with).
narrative_ontology:cs_axiom('7ca46e49-b0d5-46f0-ad4f-fd0a58700513', foundational, procedural_guarantee_independent_of_substantive_theory).
narrative_ontology:cs_axiom_status(procedural_guarantee_independent_of_substantive_theory, holdable).
narrative_ontology:cs_axiom_grounding('7ca46e49-b0d5-46f0-ad4f-fd0a58700513', procedural_guarantee_independent_of_substantive_theory, conventional).
narrative_ontology:cs_axiom('7ca46e49-b0d5-46f0-ad4f-fd0a58700513', foundational, torture_prohibition_absolute_and_non_derogable).
narrative_ontology:cs_axiom_status(torture_prohibition_absolute_and_non_derogable, holdable).
narrative_ontology:cs_axiom_grounding('7ca46e49-b0d5-46f0-ad4f-fd0a58700513', torture_prohibition_absolute_and_non_derogable, deontological).
narrative_ontology:cs_reference_frame('7ca46e49-b0d5-46f0-ad4f-fd0a58700513', id_1948_drafting_compromise_procedural_floor).
narrative_ontology:cs_drift_state('7ca46e49-b0d5-46f0-ad4f-fd0a58700513', post_9_11_security_detention_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7ca46e49-b0d5-46f0-ad4f-fd0a58700513', '').
narrative_ontology:cs_kernel_id(udhr_article_3__procedural_hybrid_reading, udhr_article_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, detained_persons_with_access_to_courts).
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, torture_prohibition_treaty_bodies).
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, domestic_judiciaries).
narrative_ontology:constraint_victim(udhr_article_3__procedural_hybrid_reading, detainees_in_non_derogable_emergency_regimes).
narrative_ontology:constraint_victim(udhr_article_3__procedural_hybrid_reading, stateless_and_undocumented_detainees).
narrative_ontology:constraint_victim(udhr_article_3__procedural_hybrid_reading, torture_victims_in_non_compliant_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, states_administering_detention).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Held by the state but able to petition for habeas corpus review and invoke torture prohibition in a functioning judicial system. The procedural guarantee gives them a lever against arbitrary detention even though it says nothing about the underlying justice of the detention regime.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, detained_persons_with_access_to_courts, beneficiary,
    powerless, immediate, constrained, national).

% Held under states of emergency, indefinite security detention, or extraordinary rendition where habeas corpus is suspended or rendered procedurally hollow (secret courts, classified evidence, indefinite review delay). Article 3's procedural guarantee is formally invoked by the state as legitimating cover while functionally denied in substance.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, detainees_in_non_derogable_emergency_regimes, payer,
    powerless, immediate, trapped, national).

% Immigration detainees, stateless persons, and those outside standard citizenship protections often face weaker or absent habeas access precisely because the procedural reading depends on functioning domestic courts extending jurisdiction to them — a threshold question the hybrid reading does not resolve.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, stateless_and_undocumented_detainees, payer,
    powerless, immediate, trapped, national).

% Subject to torture or ill-treatment in states that have signed onto Article 3's procedural norms but lack enforcement capacity or political will. The prohibition exists on paper; its non-derogability is asserted internationally but enforcement depends entirely on domestic mechanisms the victim cannot compel.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, torture_victims_in_non_compliant_states, payer,
    powerless, immediate, trapped, national).

% Administer habeas review and adjudicate torture-prohibition claims. They interpret how far procedural due process extends without being required by the hybrid reading to rule on whether the detention or its conditions are substantively just — this is the interpretive discretion the hybrid reading grants them.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, domestic_judiciaries, agenda_setter,
    institutional, generational, analytical, national).

% UN Committee Against Torture and analogous bodies gain jurisdiction and institutional standing precisely because Article 3's procedural core (torture absolute prohibition) is treated as settled and non-derogable, which is the strongest and least contested part of the hybrid reading.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, torture_prohibition_treaty_bodies, beneficiary,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(udhr_article_3__procedural_hybrid_reading, torture_prohibition_treaty_bodies, observer).

% Design detention and emergency-powers regimes and can point to formal habeas and torture-prohibition compliance as evidence of legitimacy while narrowing procedural substance during emergencies (secret evidence, indefinite administrative detention, extraterritorial black sites). The hybrid reading's silence on substantive liberty gives states room to satisfy procedure while defeating its purpose.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, states_administering_detention, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(udhr_article_3__procedural_hybrid_reading, states_administering_detention, beneficiary).

% Argue that a purely procedural reading of Article 3 lets states claim compliance while denying the material conditions (food, shelter, healthcare) they say 'security of person' requires. Their positive-entitlement argument is structurally outside the scope this reading adjudicates.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, welfare_rights_advocates, excluded,
    organized, generational, constrained, global).

% Argue any procedural constraint on detention authority is itself a security cost; they would prefer a narrower reading limited to torture prohibition alone, without robust habeas review. Their view is not adjudicated by the hybrid reading, which sits between them and rights-maximalist advocates.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, security_hawks, excluded,
    organized, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(udhr_article_3__procedural_hybrid_reading, diffuse).
narrative_ontology:fixing_cost_class(udhr_article_3__procedural_hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a genuinely cross-ideological floor: whatever states believe about welfare obligations or the proper scope of liberty, they can converge on requiring judicial review of detention and an absolute torture prohibition, letting incompatible normative traditions cooperate on international human rights instruments without resolving their deeper disagreement.
% TRANSFER_FUNCTION: Moves legitimacy and procedural leverage toward detained persons in functioning judicial systems (they gain a court-enforceable claim against arbitrary detention and mistreatment) and moves discretion toward states and courts to define detention's substantive limits — while doing nothing to move material resources or substantive liberty guarantees toward anyone, which is exactly the gap emergency-detention and stateless-detention regimes exploit.
% ABSENT_VOICES: Welfare rights advocates and security hawks are both structurally excluded from what this specific reading adjudicates — the hybrid reading brackets their dispute rather than resolving it, so neither can invoke Article 3's procedural core to settle their substantive claim. Stateless detainees and those under indefinite emergency detention are also functionally absent: they lack the working domestic court access the reading presupposes.
% DISAPPEARANCE_RATIONALE: If the procedural floor vanished, habeas corpus and torture-prohibition claims would lose their treaty anchor; domestic courts adjudicating detention claims under international law would lose a citable source of authority, treaty bodies overseeing torture prohibition would lose jurisdictional grounding, and states currently constrained by even nominal procedural review would face less international pressure to provide it.
% FOUNDING_PROBLEM: Post-1948 drafters needed a formulation of 'security of person' that could secure near-universal ratification across capitalist and socialist blocs with irreconcilable views on the proper relationship between liberty and welfare; deferring the substantive question to procedural guarantees (habeas, torture prohibition) was the compromise that let the Declaration pass without either side conceding its underlying theory.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians of the 1948 drafting process (outside any state party) document the deliberate ambiguity as a diplomatic necessity, not an oversight. Contemporary human rights scholars and UN Special Rapporteurs — bodies independent of the states that benefit from the ambiguity's flexibility — attest that the unresolved liberty/welfare split remains actively exploited by states seeking procedural cover for substantive denial, particularly in emergency and immigration detention contexts.
narrative_ontology:disappearance_verdict(udhr_article_3__procedural_hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_article_3__procedural_hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_article_3__procedural_hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(udhr_article_3__procedural_hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_article_3__procedural_hybrid_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_article_3__procedural_hybrid_reading_tests).
:- end_tests(udhr_article_3__procedural_hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.42) is moderate: real, court-enforceable procedural leverage exists for many detainees, which caps how extractive the arrangement can be relative to a pure snare, but the same procedural formalism is used by administering states as legitimating cover during emergencies and in immigration contexts where habeas is suspended, delayed, or rendered symbolic — producing genuine victims. Suppression (0.48) reflects that alternatives (stronger substantive review, non-derogable habeas even in emergencies) are actively resisted by states rather than simply absent. Theater ratio rose sharply after 2001 (0.28 to 0.42) as post-9/11 security detention regimes proliferated formal review mechanisms (military commissions, administrative review boards) that satisfy the procedural letter while limiting substantive contestability — a clear Goodhart-style metric substitution. Accessibility collapse is moderate-low (0.35) because working domestic courts genuinely remain an alternative path in many jurisdictions; this is not a fully collapsed constraint. Resistance (0.55) is substantial: human rights litigators, treaty bodies, and domestic courts actively contest procedural hollowing, which is itself evidence the coordination function is real and worth fighting to preserve.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of a detainee with genuine court access, the constraint operates close to a rope — real coordination, real leverage, low net cost. From the seat of a detainee under indefinite emergency detention or without stable jurisdictional standing, the identical textual guarantee operates as a snare: procedural language cited by the detaining state as legitimation while providing no functional remedy. The engine should compute these divergent per-seat classifications from the same structural data; the hybrid reading's persistence depends on this divergence remaining under-examined by treating 'due process exists' as satisfied by formal compliance alone.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary status was assigned to detained persons with functioning court access, domestic judiciaries administering genuine review, and treaty bodies whose institutional standing depends on the non-derogable torture prohibition being taken seriously — these seats derive low-to-moderate directionality because the arrangement subsidizes their position (leverage, jurisdiction, legitimacy). Victim status was assigned to detainees under emergency, stateless, or undocumented status because for them the same textual guarantee produces high directionality toward extraction: they bear the cost of a procedural form satisfied without procedural substance. States administering detention hold a dual agenda-setter/beneficiary role: they administer the enforcement machinery and simultaneously benefit from being able to claim compliance while narrowing the practical guarantee — this is exactly the asymmetric-extraction-through-the-same-structure the tangled_rope gate requires.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — securing a workable due-process floor across incompatible 1948-era ideological blocs — remains partly live (states still contest the liberty/welfare question this reading deliberately defers) and partly captured (procedural formalism has become a tool for legitimating substantive denial in emergency and immigration contexts). Classifying this as tangled_rope rather than snare prevents mislabeling: the coordination function is not merely historical residue, it is actively exercised by functioning judiciaries and treaty bodies today. Classifying it as tangled_rope rather than rope prevents whitewashing: the same structure that delivers genuine coordination for some is deployed as extraction cover for others, and the divergence tracks a real, identifiable fault line (court access and jurisdictional standing), not measurement noise.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy,
    'Does Article 3''s text and drafting history support treating the procedural guarantees (habeas, torture prohibition) as a self-sufficient, freestanding reading, or are they inescapably parasitic on one of the two substantive readings (negative liberty or positive entitlement) to give ''security of person'' any content at all?',
    'Comparative analysis of how the negative_liberty_reading and positive_entitlement_reading constraint stories'' ε and victim structures compare to this one; examination of drafting committee records (Cassin, Malik) on whether procedural guarantees were intended as independent or derivative.',
    'If the procedural reading is genuinely freestanding, the tangled_rope classification here stands independently. If it is parasitic on the negative-liberty reading specifically, this story''s coordination function may be better understood as a proper subset of negative_liberty_reading rather than a coequal sibling, which would change the network topology (influences rather than coexists_with).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the procedural/hybrid reading is a genuinely independent kernel reading or derivative of the negative-liberty reading.').

omega_variable(
    procedural_formalism_as_extraction_vector,
    'Is the post-2001 rise in theater_ratio (formal review boards, military commissions, administrative tribunals satisfying procedural letter without substantive contestability) an emergent capture of this specific reading, or was the vulnerability to formalism-as-cover always inherent in a reading that brackets substantive liberty questions?',
    'Trace whether procedural hollowing correlates with specific historical events (post-9/11 security architecture) versus being a constant background rate across the full 1948-2024 interval; compare against jurisdictions with strong constitutional courts versus weak judicial independence.',
    'If historically contingent, the tangled_rope classification may be time-indexed and a purely coordination-function (rope) reading may have been accurate for 1948-2000. If structurally inherent, the reading was always a tangled_rope and the post-2001 data merely reveals what was latent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedural_formalism_as_extraction_vector, empirical, 'Whether procedural formalism as an extraction mechanism is a recent capture or an inherent feature of the hybrid reading.').

omega_variable(
    stakeholder_status_of_states,
    'Should states administering detention regimes be treated as a unified stakeholder, or does the coordination-versus-extraction split actually run along a state-capacity and rule-of-law axis (functioning democracies with independent judiciaries versus authoritarian or crisis-state regimes)?',
    'Decompose the states_administering_detention stakeholder by regime type and judicial independence index; compare per-subgroup ε.',
    'If the split is real, a further decomposition into separate constraint stories per regime-type may be warranted under the ε-invariance principle, rather than treating ''states'' as one stakeholder with dual agenda_setter/beneficiary roles.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(stakeholder_status_of_states, empirical, 'Whether the state stakeholder should be split by regime type for a cleaner ε.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_article_3__procedural_hybrid_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_tr_t1948, udhr_article_3__procedural_hybrid_reading, theater_ratio, 1948, 0.2).
narrative_ontology:measurement(udhr_tr_t1966, udhr_article_3__procedural_hybrid_reading, theater_ratio, 1966, 0.25).
narrative_ontology:measurement(udhr_tr_t1984, udhr_article_3__procedural_hybrid_reading, theater_ratio, 1984, 0.28).
narrative_ontology:measurement(udhr_tr_t2001, udhr_article_3__procedural_hybrid_reading, theater_ratio, 2001, 0.42).
narrative_ontology:measurement(udhr_tr_t2010, udhr_article_3__procedural_hybrid_reading, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(udhr_tr_t2024, udhr_article_3__procedural_hybrid_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(udhr_be_t1948, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 1948, 0.25).
narrative_ontology:measurement(udhr_be_t1966, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 1966, 0.3).
narrative_ontology:measurement(udhr_be_t1984, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 1984, 0.33).
narrative_ontology:measurement(udhr_be_t2001, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 2001, 0.4).
narrative_ontology:measurement(udhr_be_t2010, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 2010, 0.4).
narrative_ontology:measurement(udhr_be_t2024, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(udhr_su_t1948, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 1948, 0.3).
narrative_ontology:measurement(udhr_su_t1966, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 1966, 0.33).
narrative_ontology:measurement(udhr_su_t1984, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 1984, 0.36).
narrative_ontology:measurement(udhr_su_t2001, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 2001, 0.48).
narrative_ontology:measurement(udhr_su_t2010, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 2010, 0.47).
narrative_ontology:measurement(udhr_su_t2024, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 2024, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_article_3__procedural_hybrid_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(udhr_article_3__procedural_hybrid_reading, 0.12).
narrative_ontology:affects_constraint(udhr_article_3__procedural_hybrid_reading, udhr_article_3__negative_liberty_reading).
narrative_ontology:affects_constraint(udhr_article_3__procedural_hybrid_reading, udhr_article_3__positive_entitlement_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'UDHR Article 3' per the ε-invariance principle. negative_liberty_reading treats Article 3 as prohibiting state deprivation of life/liberty absent narrow procedural justice (freedom-from-violence framing); positive_entitlement_reading treats it as obligating material provision (welfare/housing/healthcare framing); this procedural_hybrid_reading treats it as guaranteeing due process machinery (habeas, torture prohibition) while remaining agnostic on the substantive contest between the other two. Each has a distinct ε, distinct beneficiary/victim sets, and is authored as an independent constraint story linked here via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
