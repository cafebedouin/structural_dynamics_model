% ============================================================================
% CONSTRAINT STORY: first_amendment_1951__founders_amending_founders_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_first_amendment_1951__founders_amending_founders_reading, []).

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
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: first_amendment_1951__founders_amending_founders_reading
 *   human_readable: Founders Amending Founders: The 1951 Amendment as Precedent for Textual Reworking
 *   domain: constitutional_law/foundational_authority
 *
 * SUMMARY:
 *   This constraint traces the structural effect of Nehru's government
 *   amending Ambedkar's Constitution within months of its adoption. The
 *   historical fact — that the same political generation that ratified the
 *   text immediately revised it — instantiates a specific reading of the
 *   First Amendment's authority: the reading that the founding was a process,
 *   not a moment, and that the text was designed to be workable and
 *   revisable, not sacred. This reading sits in contest with strong
 *   originalism (which claims the founders' intent is fixed and immutable)
 *   and with constitutional fundamentalism (which claims the foundational
 *   text is an unchanging charter). The amendment itself concerned the
 *   suppression of free speech — Nehru's government added 'public order' and
 *   'friendly relations' to Article 19(2) as grounds for restricting speech,
 *   thereby narrowing the speech protections in Ambedkar's draft. The paradox
 *   is generative: the amendment proves the founding process was open-ended
 *   and revisable; simultaneously, it demonstrates that the first use of that
 *   revisability was to restrict rather than expand rights. The constraint's
 *   extractiveness is moderate because the precedent genuinely enables later
 *   constitutionalism (beneficiary: adaptive amendment authority) while
 *   simultaneously undermining the strong originalist position (victim: the
 *   premise that founders' intent is stable and self-interpreting). The
 *   suppression value (0.42) reflects that the amendment suppresses the
 *   alternative claim — that constitutional founding is closed and the text
 *   is sacred — but does not entirely eliminate it: constitutional
 *   fundamentalism persists as institutional theater despite being
 *   contradicted by historical practice.
 *
 * KEY AGENTS:
 *   - Ambedkar (drafting generation): Drafter of the founding text (institutional/arbitrage) — benefits from the precedent that the text is workable; undermined by the irony that the first amendment narrows rather than extends rights.
 *   - Nehru's government (amending generation): First amendment authority (institutional/arbitrage) — primary beneficiary; demonstrates parliamentary amendment power; sets precedent for treating the text as revisable.
 *   - Strong originalism doctrine: Interpretive methodology claiming founders' intent is fixed (powerful/constrained) — primary victim; the precedent empirically refutes the claim that the founding moment closes the text; originalists must either deny the precedent's force or accept that the founders themselves were not originalists.
 *   - Adaptive constitutionalism movement: Advocates for living constitutionalism and parliamentary amendment (organized/mobile) — primary beneficiary; gains authority and precedent from the founders' own amendments.
 *   - Constitutional fundamentalism narrative: Institutional claim of unchanging foundational text (institutional/arbitrage) — secondary victim; must maintain the fiction of immutability despite the live counterexample.
 *   - Analytical observer: Civilizational perspective on constitutional dynamics (analytical/analytical) — risks naturalizing the contested choice about amendment as an inherent feature of constitutional order.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_1951__founders_amending_founders_reading, 0.38).
domain_priors:suppression_score(first_amendment_1951__founders_amending_founders_reading, 0.42).
domain_priors:theater_ratio(first_amendment_1951__founders_amending_founders_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_1951__founders_amending_founders_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(first_amendment_1951__founders_amending_founders_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(first_amendment_1951__founders_amending_founders_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_1951__founders_amending_founders_reading, tangled_rope).
narrative_ontology:human_readable(first_amendment_1951__founders_amending_founders_reading, "Founders Amending Founders: The 1951 Amendment as Precedent for Textual Reworking").
narrative_ontology:topic_domain(first_amendment_1951__founders_amending_founders_reading, "constitutional_law/foundational_authority").

domain_priors:requires_active_enforcement(first_amendment_1951__founders_amending_founders_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_1951__founders_amending_founders_reading, '5665ffd0-8397-4784-aa4e-6769e57707cf').
narrative_ontology:cs_kernel_codification('5665ffd0-8397-4784-aa4e-6769e57707cf', fixed_text).
narrative_ontology:cs_authority_grounding('5665ffd0-8397-4784-aa4e-6769e57707cf', lineage).
narrative_ontology:cs_interpretation_layer_present('5665ffd0-8397-4784-aa4e-6769e57707cf').
narrative_ontology:cs_reading_relation('5665ffd0-8397-4784-aa4e-6769e57707cf', first_amendment_1951__ninth_schedule_immunity_reading, coexists_with).
narrative_ontology:cs_reading_relation('5665ffd0-8397-4784-aa4e-6769e57707cf', first_amendment_1951__speech_grounds_expansion_reading, coexists_with).
narrative_ontology:cs_axiom('5665ffd0-8397-4784-aa4e-6769e57707cf', foundational, founding_is_process_not_moment).
narrative_ontology:cs_axiom_status(founding_is_process_not_moment, holdable).
narrative_ontology:cs_axiom_grounding('5665ffd0-8397-4784-aa4e-6769e57707cf', founding_is_process_not_moment, empirically_contingent).
narrative_ontology:cs_axiom('5665ffd0-8397-4784-aa4e-6769e57707cf', foundational, constitutional_text_is_workable_instrument).
narrative_ontology:cs_axiom_status(constitutional_text_is_workable_instrument, holdable).
narrative_ontology:cs_axiom_grounding('5665ffd0-8397-4784-aa4e-6769e57707cf', constitutional_text_is_workable_instrument, instrumental).
narrative_ontology:cs_reference_frame('5665ffd0-8397-4784-aa4e-6769e57707cf', contingent_founding_process).
narrative_ontology:cs_drift_state('5665ffd0-8397-4784-aa4e-6769e57707cf', contemporary_constitutional_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5665ffd0-8397-4784-aa4e-6769e57707cf', '').
narrative_ontology:cs_kernel_id(first_amendment_1951__founders_amending_founders_reading, first_amendment_1951).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_1951__founders_amending_founders_reading, adaptive_constitutionalism_doctrine).
narrative_ontology:constraint_beneficiary(first_amendment_1951__founders_amending_founders_reading, later_parliament_amendment_authority).
narrative_ontology:constraint_victim(first_amendment_1951__founders_amending_founders_reading, strong_originalism_premise).
narrative_ontology:constraint_victim(first_amendment_1951__founders_amending_founders_reading, founding_text_sanctity_norm).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ORIGINALIST INTERPRETER (SNARE) — Trapped by the historical fact that the founding generation itself amended the text within months. Cannot credibly argue for original-intent immutability when the original authors revised. Experiences maximum extraction: the precedent undermines the core premise of strong originalism (that the founders' text is self-interpreting and stable). No exit from this epistemic bind.
constraint_indexing:constraint_classification(first_amendment_1951__founders_amending_founders_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: TEXTUALIST JURIST (TANGLED ROPE) — Constrained by the same historical record but retains agency through the distinction between text and interpretation. Can argue that the 1st Amendment's text is stable while acknowledging that application and supplementary rules (suppression of speech, grounds for restriction) evolved. Experiences mixed extraction: the precedent narrows the field of defensible positions but does not eliminate textualist methodology. Coordinates with adaptive doctrines while resisting pure living constitutionalism.
constraint_indexing:constraint_classification(first_amendment_1951__founders_amending_founders_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ADAPTIVE CONSTITUTIONALIST PARLIAMENT (ROPE) — Experiences the constraint as coordination: the founding generation's own amendments prove that the constitution is a workable instrument designed to be revised by later legislatures. Benefits from the precedent — gains authority to amend on the grounds that the founders themselves amended. Sees the constraint as legitimate evolution, not violation. Maximum beneficiary position.
constraint_indexing:constraint_classification(first_amendment_1951__founders_amending_founders_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTITUTIONAL REFORM MOVEMENT (SCAFFOLD) — Organized advocates (social movements, reform lawyers, reform-minded legislators) see the 1951 precedent as temporary validation for broader amendment projects. They experience the constraint as scaffolding: the founders' amendments provide cover for generational reworking of the text to accommodate new social demands (land reform, secularism, social rights). The sunset logic is implicit: once the precedent is absorbed into doctrine, it recedes into background authority. Extractiveness is low because these agents have a clear exit path (absorption into doctrine) and real agency.
constraint_indexing:constraint_classification(first_amendment_1951__founders_amending_founders_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL FUNDAMENTALISM NARRATIVE (PITON) — The institutional claim that the Constitution is an unchangeable foundational charter persists despite the 1951 counterexample. The fundamentalist narrative now carries high theater ratio: it maintains the rhetorical posture of sacred origin while implicitly acknowledging that amendments happen. The constraint here is the performance of immutability despite the lived practice of amendment. Theater ratio is high because the fundamental-charter framing persists as institutional theater even after its grounding premise is contradicted by evidence.
constraint_indexing:constraint_classification(first_amendment_1951__founders_amending_founders_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, some degree of foundational-text revision is inherent to long-lived legal systems: any constitution that persists for more than a generation must accommodate change, and the gap between original text and current application is structurally inevitable. This perspective sees the constraint as a natural law of constitutional dynamics. However, this classification is flagged as a false summit: the 'inevitability' of revision naturalizes what is actually a contested institutional choice about whether and how amendments should occur.
constraint_indexing:constraint_classification(first_amendment_1951__founders_amending_founders_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(first_amendment_1951__founders_amending_founders_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(first_amendment_1951__founders_amending_founders_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(first_amendment_1951__founders_amending_founders_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(first_amendment_1951__founders_amending_founders_reading, TR),
    TR >= 0.70.

:- end_tests(first_amendment_1951__founders_amending_founders_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The amendment establishes that the founders themselves revised the text, which is a genuine structural fact that undermines strong originalism. The extraction is not total (originalism persists in institutional practice) and is not minimal (the precedent genuinely constrains what can be claimed about founding immutability). The measurement trajectory (0.22 → 0.38) reflects that the extractiveness of this reading increased as decades of constitutional practice absorbed the precedent: initially the amendment was a discrete event (low extractiveness); over time it became the standard reference point for all amendment discourse (higher extractiveness). Suppression (0.42): Moderate. The amendment suppresses the strong originalist position by providing empirical refutation, but it does not eliminate that position: originalists can argue (a) that the amendment was an emergency measure not a precedent, (b) that the founders were not acting as constitutional theorists but as pragmatic legislators, or (c) that some degree of original-intent fidelity remains even if the text is revisable. These escape routes mean suppression is moderate, not high. Theater ratio (0.55): Moderate-high. The amendment's presentation involved significant rhetorical work: it was framed as an emergency correction responding to specific doctrinal problems (communal violence, public order), not as a general statement about constitutional revisability. The theater increased over time as the amendment was retrospectively read as a precedent for amendment authority — the initial framing (specific correction) was repurposed as general doctrine (the founders proved revisability). This trajectory (0.30 → 0.55) models how historical facts acquire interpretive force through institutional practice.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full perspectival range because the same historical fact (Nehru's amendment) appears as proof of a workable founding (Adaptive Constitutionalist Parliament: Rope), refutation of fixed intent (Originalist Interpreter: Snare), coordination with constitutional evolution (Textualist Jurist: Tangled Rope), scaffolding for broader reform (Constitutional Reform Movement: Scaffold), performative ritual maintaining immutability narrative (Constitutional Fundamentalism: Piton), and natural law of constitutional dynamics (Analytical Observer: Mountain). The gaps derive from different agents' structural relationships to the claim that the founding is closed vs. open. The originalist interpreter has no exit from the refutation; the adaptive parliament benefits from the precedent; the textualist maintains agency through interpretation; the reform movement gains scaffolding; the fundamentalist narrative persists through theater; the analytical observer risks naturalizing a contingent institutional choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by the agent's structural position relative to the amendment precedent. The originalist interpreter (powerless/trapped) experiences maximum extraction because the precedent refutes the core premise of originalism without providing an alternative methodology — the agent is trapped by the historical fact. The adaptive parliament (institutional/arbitrage) experiences minimal extraction (negative, effectively) because the precedent grants authority and proves the workability of the text. The textualist (moderate/constrained) occupies a middle ground: the amendment constrains originalist positions but does not eliminate textualism as a methodology. The reform movement (organized/constrained) sees the constraint as scaffolding that provides cover for broader amendment projects. The fundamentalist narrative (institutional/arbitrage) experiences low effective extraction because the narrative persists despite being contradicted — the institutional investment in the sacred-text framing is sufficient to maintain it as theater. The analytical observer (analytical/analytical) risks seeing this as a natural law, but the engine's false-summit detector flags this as naturalization of a contingent institutional choice.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does NOT resolve mandatrophy — it exemplifies how mandatrophy operates across a kernel. The classical mandatrophy question is: is this a coordination problem (Rope) or an extraction problem (Snare)? This reading produces six different answers from six legitimate perspectives, none of which is 'the right one.' The originalist sees Snare (trapped by refutation); the adaptive constitutionalist sees Rope (coordinated evolution); the textualist sees Tangled Rope (mixed). The perspectival range is the point. The reading itself does not resolve which view is correct — it establishes that the same historical fact supports incompatible constitutional framings. This is the framework's diagnostic signal: when a single constraint produces the full range of types across perspectives, the presheaf over the observation site — not any single type — is the structure to analyze.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intentionality_vs_precedent,
    'Did Nehru''s government amend the text with conscious intent to establish a precedent for parliamentary amendment, or did the amendments address specific doctrinal problems without meta-commentary on the founding''s revisability?',
    'Examination of parliamentary debates, ministerial statements, and constitutional scholars'' writings from 1950-1952 regarding the intent and scope of the first amendment. Comparison of how the amendment was justified: as emergency correction vs. as precedent for constitutional flexibility.',
    'If intentional precedent-setting: this reading gains doctrinal force — the founders explicitly proved the text was workable. If accidental or problem-specific: the reading relies on inference from action rather than declared principle, weakening its legitimacy claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intentionality_vs_precedent, empirical, 'Whether Nehru''s amendments were intentional precedent-setting or problem-specific corrections').

omega_variable(
    amendment_as_interpretation_vs_amendment_as_replacement,
    'Do constitutional amendments of the type in the 1951 First Amendment count as interpretation of the original founding text, or as replacement/supersession of it?',
    'Conceptual analysis of what constitutes amendment vs interpretation in constitutional law. Empirical comparison: does Indian constitutional practice treat the 1st Amendment as clarifying Ambedkar''s intent, or as new law that supersedes the original? Do later judges cite the original text or the amended version as binding?',
    'If amendment-as-interpretation: the founding maintains unity — the same text, reworked. If amendment-as-replacement: the constraint is more extractive — the original founding is not stable even in its immediate aftermath. Classification could shift toward Snare if amendment logic emphasizes replacement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_as_interpretation_vs_amendment_as_replacement, conceptual, 'Whether amendments constitute interpretation or replacement of the founding text').

omega_variable(
    founding_generation_identity,
    'Who counts as the ''founding generation'' for the purpose of assessing whether amendments violate founding sanctity? Does Nehru''s government belong to the founding generation, or is the founding closed at Ambedkar''s draft?',
    'Historical and institutional analysis: constitutional scholars'' conventions for identifying the founding vs post-founding periods. Does the Constituent Assembly (1949) count as founding; does its immediate successor parliament (1952+) count as founding? Timeline mapping of when the ''founding'' is considered closed.',
    'If Nehru is part of the founding: the amendments are internal to the founding process — the reading holds; the founding was genuinely a process. If Nehru is post-founding: the amendments are post-founding revisions and do not prove the founding process was open-ended — the reading is weakened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_generation_identity, conceptual, 'Temporal and institutional definition of the founding generation').

omega_variable(
    natural_law_vs_constructed_amendment_authority,
    'Is the power to amend the Constitution a natural property of sovereignty (inherent to parliamentary authority), or a constructed property granted by the Constitution itself?',
    'Doctrinal analysis: comparison of constitutional text vs. common law theory of sovereignty. Does the Indian Constitution explicitly grant amendment power, or is amendment authority presupposed? What would happen if the Constitution contained no amendment clause?',
    'If natural/inherent: parliamentary amendment authority needs no special justification — this reading dissolves into background practice. If constructed: the amendment authority itself requires constitutional grounding — which grounds it becomes contestable. Extractiveness could shift upward if amendment authority is seen as constructed and therefore contingent on institutional power structures.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_amendment_authority, conceptual, 'Whether amendment power is natural or constructed authority').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_1951__founders_amending_founders_reading, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fa1951_founders_theater_t0, first_amendment_1951__founders_amending_founders_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(fa1951_founders_theater_t2, first_amendment_1951__founders_amending_founders_reading, theater_ratio, 2, 0.48).
narrative_ontology:measurement(fa1951_founders_theater_t4, first_amendment_1951__founders_amending_founders_reading, theater_ratio, 4, 0.55).

% Extraction over time
narrative_ontology:measurement(fa1951_founders_extract_t0, first_amendment_1951__founders_amending_founders_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(fa1951_founders_extract_t2, first_amendment_1951__founders_amending_founders_reading, base_extractiveness, 2, 0.3).
narrative_ontology:measurement(fa1951_founders_extract_t4, first_amendment_1951__founders_amending_founders_reading, base_extractiveness, 4, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_1951__founders_amending_founders_reading, identity_coordination).
narrative_ontology:affects_constraint(first_amendment_1951__founders_amending_founders_reading, first_amendment_1951__ninth_schedule_immunity_reading).
narrative_ontology:affects_constraint(first_amendment_1951__founders_amending_founders_reading, first_amendment_1951__speech_grounds_expansion_reading).

% DUAL FORMULATION NOTE:
% The first_amendment_1951 kernel decomposes into three readings with distinct structural deltas. The founders_amending_founders_reading (this constraint) addresses the meta-claim that the founding was revisable; the ninth_schedule_immunity_reading addresses the creation of constitutional vault mechanisms; the speech_grounds_expansion_reading addresses the narrowing of speech protections. Each reading extracts a different structural meaning from the same historical amendment. All three are linked because they interpret the same constitutional moment, but each has its own constraint_id, ε value, and perspective set reflecting what that reading makes salient.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(first_amendment_1951__founders_amending_founders_reading, powerful, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
