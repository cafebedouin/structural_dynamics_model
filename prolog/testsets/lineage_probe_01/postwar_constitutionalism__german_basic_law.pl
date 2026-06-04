% ============================================================================
% CONSTRAINT STORY: postwar_constitutionalism__german_basic_law
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_postwar_constitutionalism__german_basic_law, []).

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
 *   constraint_id: postwar_constitutionalism__german_basic_law
 *   human_readable: The German Basic Law: Militant Postwar Constitutionalism
 *   domain: political/legal/constitutional_law
 *
 * SUMMARY:
 *   The German Basic Law (Grundgesetz) of 1949 is the canonical exemplar of
 *   militant postwar constitutionalism: a constitutional order that
 *   entrenches human dignity (Art. 1) beyond even constitutional amendment
 *   (Art. 79(3) eternity clause) and arms the state with explicit authority
 *   to suppress political movements adjudged 'hostile to the free democratic
 *   order' (Art. 21(2), Art. 18). This reading instantiates ONE
 *   interpretation of how defeated societies reconstitute themselves after
 *   catastrophic regime collapse. It contrasts with the Indian Constitution
 *   (1950) — an exhaustive enumeration of social rights and textual
 *   universalism — and the Japanese Constitution (1947) — an imposed victor's
 *   charter that became indigenous through practice. The German reading
 *   emphasizes legal-structural safeguards against regime-type recurrence: if
 *   democracy was destroyed by antidemocratic forces exploiting democratic
 *   freedoms, the response is to deny freedom to those adjudged hostile to
 *   the democratic order itself. This creates a fundamental structural
 *   tension: the constraint simultaneously coordinates (protects individual
 *   rights through immutable human dignity), extracts (suppresses proscribed
 *   political movements), and naturalizes (presents contingent institutional
 *   design as immutable constitutional necessity). The constraint's core
 *   extractiveness is low (0.18) because suppression is explicitly visible
 *   and court-policed rather than hidden behind coordination framing. But
 *   suppression itself is high (0.62) because entire political movements can
 *   be legally dissolved and their members subject to loss of constitutional
 *   freedoms.
 *
 * KEY AGENTS:
 *   - Constitutional Rights Holders (Beneficiaries): individual and collective agents claiming protection under Art. 1 (human dignity) and subsidiary rights; experience the Basic Law as coordination mechanism enabling rights-claiming
 *   - Political Movements Adjudged Hostile to Free Democratic Order (Victims): organizations (NPD, Communist Party, etc.) subject to party bans under Art. 21(2) and loss of constitutional freedoms under Art. 18; trapped within German jurisdiction; experience maximum suppression
 *   - Federal Constitutional Court (Institutional Actor): sole authority determining what constitutes 'hostile to the free democratic order'; benefits from expanded interpretive jurisdiction; court-polices suppression doctrine; experiences the constraint as institutional empowerment coupled with responsibility burden
 *   - Political Opposition Parties (Secondary Actors): operate within lawful bounds but constrained by jeopardy of militant-democracy enforcement; experience tangled coordination and extraction
 *   - Analytical Observer (Civilizational): risks naturalizing the Basic Law's specific historical design (response to Weimar/Nazi experience) as universal constitutional necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(postwar_constitutionalism__german_basic_law, 0.18).
domain_priors:suppression_score(postwar_constitutionalism__german_basic_law, 0.62).
domain_priors:theater_ratio(postwar_constitutionalism__german_basic_law, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(postwar_constitutionalism__german_basic_law, extractiveness, 0.18).
narrative_ontology:constraint_metric(postwar_constitutionalism__german_basic_law, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(postwar_constitutionalism__german_basic_law, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(postwar_constitutionalism__german_basic_law, tangled_rope).
narrative_ontology:human_readable(postwar_constitutionalism__german_basic_law, "The German Basic Law: Militant Postwar Constitutionalism").
narrative_ontology:topic_domain(postwar_constitutionalism__german_basic_law, "political/legal/constitutional_law").

domain_priors:requires_active_enforcement(postwar_constitutionalism__german_basic_law).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(postwar_constitutionalism__german_basic_law, '5c5ee985-8dd2-4a25-854a-5d911eeafe24').
narrative_ontology:cs_kernel_codification('5c5ee985-8dd2-4a25-854a-5d911eeafe24', fixed_text).
narrative_ontology:cs_authority_grounding('5c5ee985-8dd2-4a25-854a-5d911eeafe24', extraction).
narrative_ontology:cs_interpretation_layer_present('5c5ee985-8dd2-4a25-854a-5d911eeafe24').
narrative_ontology:cs_reading_relation('5c5ee985-8dd2-4a25-854a-5d911eeafe24', postwar_constitutionalism__indian_constitution_1950, coexists_with).
narrative_ontology:cs_reading_relation('5c5ee985-8dd2-4a25-854a-5d911eeafe24', postwar_constitutionalism__japanese_constitution_1947, influences).
narrative_ontology:cs_axiom('5c5ee985-8dd2-4a25-854a-5d911eeafe24', foundational, democracy_requires_militant_self_defense).
narrative_ontology:cs_axiom_status(democracy_requires_militant_self_defense, holdable).
narrative_ontology:cs_axiom_grounding('5c5ee985-8dd2-4a25-854a-5d911eeafe24', democracy_requires_militant_self_defense, empirically_contingent).
narrative_ontology:cs_axiom('5c5ee985-8dd2-4a25-854a-5d911eeafe24', foundational, human_dignity_unamendable_by_majority).
narrative_ontology:cs_axiom_status(human_dignity_unamendable_by_majority, holdable).
narrative_ontology:cs_axiom_grounding('5c5ee985-8dd2-4a25-854a-5d911eeafe24', human_dignity_unamendable_by_majority, deontological).
narrative_ontology:cs_reference_frame('5c5ee985-8dd2-4a25-854a-5d911eeafe24', weimar_democratic_failure_and_postwar_reconstruction).
narrative_ontology:cs_drift_state('5c5ee985-8dd2-4a25-854a-5d911eeafe24', contemporary_post_2015, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5c5ee985-8dd2-4a25-854a-5d911eeafe24', '2026-02-26T14:32:15Z').
narrative_ontology:cs_kernel_id(postwar_constitutionalism__german_basic_law, postwar_constitutionalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(postwar_constitutionalism__german_basic_law, constitutional_rights_holders).
narrative_ontology:constraint_beneficiary(postwar_constitutionalism__german_basic_law, liberal_democratic_order).
narrative_ontology:constraint_victim(postwar_constitutionalism__german_basic_law, movements_adjudged_hostile_to_free_democratic_order).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSTITUTIONAL RIGHTS HOLDER (ROPE) — Benefits from entrenched human dignity (Art. 1) beyond amendment reach. Experiences the constraint as pure coordination — the Basic Law enables rights-claiming and judicial vindication. High exit mobility (can emigrate, can participate in lawful speech/association). Classifies as rope: genuine coordination function with low experienced extraction.
constraint_indexing:constraint_classification(postwar_constitutionalism__german_basic_law, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: PROSCRIBED POLITICAL MOVEMENT (SNARE) — Subject to party bans (Art. 21(2)) and loss of constitutional freedoms under the 'militant democracy' doctrine. Trapped within the territorial and legal jurisdiction of the Federal Republic; exit requires emigration or dissolution. Experiences maximum suppression: no lawful avenue for political speech, assembly, or organization if adjudged hostile to the free democratic order. Classifies as snare: high extraction via proscription, minimal coordination benefit, suppression total within legal boundaries.
constraint_indexing:constraint_classification(postwar_constitutionalism__german_basic_law, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: POLITICAL OPPOSITION (TANGLED ROPE) — Operates within lawful democratic bounds but under constant jeopardy of being re-adjudged 'hostile to the free democratic order' if they cross undefined thresholds. Constrained exit: exit to extralegal politics means proscription and legal disablement. Experiences both coordination (democratic participation framework) and extraction (risk of militant-democracy enforcement). Classifies as tangled rope: genuine coordination function (democratic competition) coupled with asymmetric suppression (selective enforcement risk).
constraint_indexing:constraint_classification(postwar_constitutionalism__german_basic_law, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: FEDERAL CONSTITUTIONAL COURT (TANGLED ROPE) — Institutional beneficiary of the Basic Law's structure (expanded constitutional review powers, Art. 93). Court-policed suppression (Art. 21(2) party bans require Bundesverfassungsgericht determination). Constrained exit: cannot refuse the jurisdiction without dissolving the constitutional order it interprets. Experiences coordination (constitutional interpretation) and extraction (institutional power concentration, selective use of 'hostile to free democratic order' doctrine). Classifies as tangled rope: genuine coordinate role in rights protection coupled with significant interpretive discretion enabling selective suppression.
constraint_indexing:constraint_classification(postwar_constitutionalism__german_basic_law, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: THE ETERNITY CLAUSE / UNAMENDABILITY DOCTRINE (PITON) — Declares that Art. 1 (human dignity) and Art. 20 (democratic federalist structure) cannot be amended, even by constitutional amendment process. Functionally degraded: the eternity clause is rarely invoked directly; instead, courts interpret 'basic law' principles to block amendment attempts through judicial review. Theater ratio high: much of the clause's force is performative — it gestures to immutability while actual blocking mechanisms operate through interpretive doctrine. Classification as piton reflects that the institutional claim ('unamendable') persists through inertia and ritual invocation rather than structural necessity.
constraint_indexing:constraint_classification(postwar_constitutionalism__german_basic_law, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / SOCIOLOGICAL NECESSITY (MOUNTAIN) — From a civilizational scope, postwar constitutionalism's militant democracy provisions are analyzed as responses to structural historical necessity: fascism's previous capture of democratic machinery requires institutional safeguards against regime-type recurrence. This perspective risks naturalizing what is actually a contingent political choice. The engine's false summit detector will flag this as a constructed constraint presented as natural law, revealing how 'necessary for survival' covers actual institutional design decisions.
constraint_indexing:constraint_classification(postwar_constitutionalism__german_basic_law, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(postwar_constitutionalism__german_basic_law_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(postwar_constitutionalism__german_basic_law, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(postwar_constitutionalism__german_basic_law, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(postwar_constitutionalism__german_basic_law, TR),
    TR >= 0.70.

:- end_tests(postwar_constitutionalism__german_basic_law_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low but positive. The Basic Law genuinely coordinates constitutional rights protection (Art. 1, fundamental rights enumeration). But it also extracts through suppression: political movements can be proscribed, members lose constitutional freedoms, and the determination of what is 'hostile to the free democratic order' is vested in a single institution (BVerfG). The extractiveness is low because suppression is explicit, court-policed, and theoretically limited to genuine threats to democratic regime. It is not zero because the doctrine scope is ambiguous and application can be selective. Over the 70-year interval, extractiveness has drifted slightly upward (0.12 → 0.18) as BVerfG jurisprudence has expanded the definition of actionable hostility, particularly post-2015 regarding right-wing extremism. Suppression (0.62): Moderate-high. Structures blocking regime-type recurrence are explicit and severe for affected movements (party bans, Art. 18 loss of freedoms), but apply to a narrow actor set (those adjudged hostile). General political opposition faces suppression risk (constrained exit) but not direct legal proscription unless re-categorized. Suppression has remained stable across the interval — militant democracy doctrine is stable in doctrine, though application scope has expanded. Theater (0.35): Moderate-low. The Basic Law's enforcement is relatively functional rather than performative: party bans are rare but executed; Art. 18 speech-freedom loss is rare but real; BVerfG determinations are published and justified. The 35% theater reflects that some institutional messaging (the eternity clause gesture) operates performatively, but most of the constraint's suppressive force is substantive.
 *
 * PERSPECTIVAL GAP:
 *   The German Basic Law reading differs from its siblings in how it handles the fundamental postwar constitutional question: how does a defeated democracy prevent recurrence of antidemocratic regime capture? The German reading answers: entrenched rights beyond amendment, explicit suppression of hostile movements, court-policed limits. The Indian reading answers: exhaustive constitutional enumeration of social transformation goals, textual universalism, delegitimizing hierarchy through constitutional text rather than suppressing antidemocratic movements. The Japanese reading answers: imposed legal structure (renouncing war, sovereignty-theology) that became indigenous through practice and gradual reinterpretation. These are not measurements of one constraint from different angles — they are three genuinely different institutional designs with different beneficiary/victim structures, different suppression mechanisms, and different assumptions about what 'constitution' does in the postwar context.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is derived from beneficiary/victim status plus exit options. Rights-holding beneficiaries with mobile exit experience low d (≈0.15, canonical powerful/mobile) → rope classification. Proscribed movements with trapped exit experience high d (≈0.95, canonical powerless/trapped) → snare classification. Political opposition with constrained exit and mixed beneficiary/victim status experiences moderate d (≈0.55, canonical organized/constrained) → tangled rope. The court as institutional beneficiary with constrained exit (cannot refuse jurisdiction) experiences low-moderate d (≈0.35, canonical institutional/constrained) → tangled rope with institutional benefit. The pervasive structural feature: the agent with concentrated interpretive power (BVerfG) experiences lowest extraction while defining what constitutes extractive suppression for others. This is not a derivation error but a structural reflection of how militant-democracy doctrine operates: the court's power to adjudicate threatens to be extracted (it is burdened with defining limits), but the court simultaneously benefits (its jurisdiction is expanded and validated by the necessity doctrine).
 *
 * MANDATROPHY ANALYSIS:
 *   The German Basic Law reading does NOT resolve mandatrophy in the sense of collapsing to a single type across all perspectives. Instead, it INSTANTIATES mandatrophy: the constraint is legitimately classified as rope (coordination), tangled rope (mixed), snare (suppression), piton (degraded ritual), and mountain (false summit) from different structural positions. The constraint is not incoherent — it is a correct diagnostic exemplar of how indexical classification works across asymmetric power structures. The mandatrophy resolves when we recognize that the German reading's core function (preventing regime-type recurrence through militant-democracy doctrine) is structurally achieved through selective suppression that benefits rights-holding constituencies and institutions adjudicating the threat definition. This is genuinely a tangled rope at the system level: coordination (protecting constitutional democracy) is achieved through extraction (suppressing hostile movements). The extraction is bounded (legally limited, court-policed, applied to defined movement categories) but real and asymmetric. The analytical observer's mountain is false — the constraint is not immutable natural law but a specific institutional design choice responding to German historical experience. The sibling readings (Indian, Japanese) are equally legitimate alternative designs with different mandatrophy structures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    eternity_clause_logical_status,
    'Does the eternity clause (Art. 79(3)) constitute an unamendable constitutional limit or a performative gesture backed by judicial doctrine rather than structural legal force?',
    'Historical counterfactual: would a supermajority amendment of Art. 1 or Art. 20 (assuming political will) be legally enforceable or judicially invalidated? Comparative analysis with other purportedly unamendable constitutions (e.g., whether Turkey''s unamendable articles have been amended de facto).',
    'If unamendable (structural): Basic Law is genuinely mountainous in its dignity core — the constraint is immutable. If performative (judicial doctrine): eternity clause is a piton — courts maintain the appearance of immutability, but political will could override it. This directly affects whether the mountain perspective is false summit or genuine natural law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eternity_clause_logical_status, conceptual, 'Whether Art. 79(3) is structurally unamendable or judicially maintained').

omega_variable(
    militant_democracy_doctrine_scope_ambiguity,
    'What defines ''hostile to the free democratic order'' (Art. 21(2), Art. 18) with sufficient precision to prevent arbitrary application?',
    'Doctrinal analysis: BVerfG case law on party bans (NPD cases 2003, 2017); comparison with disproportionate enforcement against left vs. right movements; audit of BVerfG decisions pre/post-2015 for temporal variance in doctrine application.',
    'If doctrine is precise and consistently applied: suppression represents targeted enforcement against genuine threats (extraction justified by coordination need). If doctrine is vague and applied selectively: suppression is arbitrary political weapon disguised as constitutional necessity (snare mechanism hidden in rope framing). This determines whether the tangled_rope classification reflects genuine hybrid function or whether the constraint should reclassify upward toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(militant_democracy_doctrine_scope_ambiguity, empirical, 'Definitional precision and consistency of ''hostile to free democratic order'' doctrine').

omega_variable(
    postwar_constitutionalism_kernel_specificity,
    'Is the German reading of postwar constitutionalism (militant democracy, eternity clause, court-policed suppression) a necessary response to German-specific historical experience, or a generalizable model of constitutional resilience applicable across democracies?',
    'Comparative constitutional analysis: how do Indian, Japanese, and other postwar democracies address regime recurrence risk without eternity clauses or party ban provisions? Do those democracies exhibit greater democratic fragility, or have they discovered alternative stabilization mechanisms? Analysis of whether the German model is being adopted elsewhere or rejected as contextually specific.',
    'If German-specific: the Basic Law reading is a local solution to a particular historical wound; sister readings (Indian, Japanese) are genuinely alternative approaches with different kernel interpretations. If generalizable: German reading becomes a universal benchmark, and alternative readings are contextual departures from optimal design. This affects whether sibling readings coexist_with or are influenced_by the German reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(postwar_constitutionalism_kernel_specificity, conceptual, 'Whether militant democracy is German-specific or universalizable constitutional principle').

omega_variable(
    suppression_proportionality_empirical_question,
    'Does the empirical distribution of party bans, Art. 18 applications (loss of freedom of speech), and BVerfG ''hostile to free democratic order'' determinations correlate with genuine threat to democratic regime, or with political power asymmetries and elite risk perception?',
    'Statistical analysis of party ban decisions and Art. 18 cases: timing, target political orientation, court composition, electoral context. Comparison with threat severity assessments (violence levels, international funding, paramilitary organization) contemporaneous with bans. Cross-national comparison: do democracies with comparable threat levels use fewer suppressive tools, suggesting German doctrine is over-calibrated?',
    'If correlation is tight: suppression is calibrated enforcement (extractiveness low, justified by coordination need). If correlation is loose or asymmetric: suppression is political extraction disguised as threat-response (extractiveness higher, snare dynamics). This directly affects the base_properties.suppression value and could push the constraint toward higher extractiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_proportionality_empirical_question, empirical, 'Correlation between suppressive measures and measurable democratic threat').

omega_variable(
    reading_contestation_kernel_event,
    'This constraint instantiates ONE reading of the postwar-constitutionalism kernel. The sibling readings (Indian, Japanese) represent different poles of the same contestation: how does a defeated society constitute itself politically in the aftermath of total war or occupation?',
    'The engine''s kernel analysis computes how each reading''s axioms, authority grounding, and reference frames differ and whether they foreclose, coexist_with, or influence each other. No empirical resolution — this is a structural feature of how the contested kernel is instantiated across different historical contexts.',
    'This is a meta-omega: it documents that this constraint is not a standalone structural claim but a reading within a larger committer-axis contest. The German Basic Law reading emphasizes militant democracy, eternity clauses, and court-policed suppression. The Indian Constitution emphasizes exhaustive enumeration of social rights and textual universalism. The Japanese Constitution emphasizes imposed structure that became indigenous through practice. These are THREE DIFFERENT CONSTRAINTS, not three measurements of one constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contestation_kernel_event, conceptual, 'This constraint is one reading of a contested postwar-constitutionalism kernel; sibling readings are separate constraints').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(postwar_constitutionalism__german_basic_law, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(blw_extractiveness_1949, postwar_constitutionalism__german_basic_law, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(blw_extractiveness_1984, postwar_constitutionalism__german_basic_law, base_extractiveness, 35, 0.15).
narrative_ontology:measurement(blw_extractiveness_2019, postwar_constitutionalism__german_basic_law, base_extractiveness, 70, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(blw_suppression_1949, postwar_constitutionalism__german_basic_law, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(blw_suppression_1984, postwar_constitutionalism__german_basic_law, suppression_requirement, 35, 0.62).
narrative_ontology:measurement(blw_suppression_2019, postwar_constitutionalism__german_basic_law, suppression_requirement, 70, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(postwar_constitutionalism__german_basic_law, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(postwar_constitutionalism__german_basic_law, 0.12).
narrative_ontology:affects_constraint(postwar_constitutionalism__german_basic_law, postwar_constitutionalism__indian_constitution_1950).
narrative_ontology:affects_constraint(postwar_constitutionalism__german_basic_law, postwar_constitutionalism__japanese_constitution_1947).

% DUAL FORMULATION NOTE:
% The German Basic Law reading is ONE of three distinct postwar-constitutionalism readings, each with different ε values and suppression mechanisms. The German reading emphasizes militant democracy and court-policed suppression (low ε=0.18 because suppression is explicit); the Indian reading emphasizes exhaustive social rights and textual universalism (different ε, different victim set); the Japanese reading emphasizes imposed-then-indigenous structure (different ε, different authority grounding). These are not observational variants of a single constraint but three separate constraints with different structural properties. They are linked via network.affects_constraints to document that each reading's legitimacy claim in its respective polity is influenced by the existence of alternative postwar constitutional models.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(postwar_constitutionalism__german_basic_law, institutional, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
