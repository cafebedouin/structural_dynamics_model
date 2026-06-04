% ============================================================================
% CONSTRAINT STORY: content_neutrality_doctrine__compelled_speech_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_content_neutrality_compelled_speech, []).

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
 *   constraint_id: content_neutrality_doctrine__compelled_speech_reading
 *   human_readable: Content Neutrality Doctrine: Compelled Speech Reading
 *   domain: constitutional_law/first_amendment
 *
 * SUMMARY:
 *   This constraint instantiates the compelled-speech reading of the
 *   content-neutrality doctrine in First Amendment law. The doctrine holds
 *   that the state cannot require citizens to affirm or recite mandatory
 *   political orthodoxy (flag salutes, patriotic mottos, loyalty oaths) any
 *   more than it can prohibit the expression of disfavored views. The
 *   principle is expressed as a logical mirror: neutrality forbids both
 *   suppression of speech and compulsion of speech. This reading emphasizes
 *   the suppression of mandated orthodoxy as the primary extractive mechanism
 *   and identifies beneficiaries as conscience objectors (those protected
 *   from compulsion) and victims as civic-cohesion programs (those that
 *   relied on mandatory affirmation). The constraint exhibits the full range
 *   of DR types from different perspectives because the doctrine
 *   simultaneously protects individual conscience (snare from the compelled
 *   speaker's view), clarifies constitutional principle (rope from the
 *   doctrine's view), burdens patriotic-unity programs (tangled rope from the
 *   enforcement coalition's view), has become largely theatrical in
 *   enforcement (piton from the civic-ritual perspective), serves as
 *   temporary scaffolding for First Amendment jurisprudence (scaffold from
 *   the remedial perspective), and risks naturalizing a contestable doctrinal
 *   choice as logical necessity (mountain from the analytical perspective).
 *   The temporal arc shows extractiveness declining from 0.78 (early
 *   enforcement era, when compulsion was aggressive and the doctrine was
 *   novel) to 0.58 (contemporary, when compulsion is rarely attempted and the
 *   doctrine is settled). The suppression requirement has similarly declined
 *   as legal norms shifted. Theater has risen, reflecting that the ritual
 *   forms (flag salutes, pledge recitations) persist but enforcement has
 *   atrophied — the mechanism is increasingly performative, not coercive.
 *
 * KEY AGENTS:
 *   - Conscience Objectors: Primary beneficiary (powerless/trapped at constitutional baseline) — protected from legal compulsion to recite patriotic affirmations; typically religious dissenters or political minorities
 *   - Patriotic Orthodoxy Enforcement Coalition: Primary victim (powerful/mobile) — school administrators, civic organizations, political factions that used compulsory expression to enforce national identity and unity; extractiveness is their loss of enforcement tools
 *   - The Compelled Speech Doctrine: Constitutional principle (institutional/arbitrage) — clarifies the First Amendment's protection; benefits from clarity and predictability
 *   - Courts: Institutional beneficiary (institutional/arbitrage) — gain authority over compelled-speech cases and doctrinal development through strict scrutiny application
 *   - Civic Ritual Performance: Institutional practice (institutional/constrained) — flag salutes and pledge recitations persist as theatrical forms despite loss of legal enforcement mechanism
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks treating logical inversion (if no suppression then no compulsion) as immutable law rather than constitutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(content_neutrality_doctrine__compelled_speech_reading, 0.58).
domain_priors:suppression_score(content_neutrality_doctrine__compelled_speech_reading, 0.62).
domain_priors:theater_ratio(content_neutrality_doctrine__compelled_speech_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(content_neutrality_doctrine__compelled_speech_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(content_neutrality_doctrine__compelled_speech_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(content_neutrality_doctrine__compelled_speech_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(content_neutrality_doctrine__compelled_speech_reading, tangled_rope).
narrative_ontology:human_readable(content_neutrality_doctrine__compelled_speech_reading, "Content Neutrality Doctrine: Compelled Speech Reading").
narrative_ontology:topic_domain(content_neutrality_doctrine__compelled_speech_reading, "constitutional_law/first_amendment").

domain_priors:requires_active_enforcement(content_neutrality_doctrine__compelled_speech_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(content_neutrality_doctrine__compelled_speech_reading, '941a7960-0408-440e-9813-8e84a59cfe78').
narrative_ontology:cs_kernel_codification('941a7960-0408-440e-9813-8e84a59cfe78', fixed_text).
narrative_ontology:cs_authority_grounding('941a7960-0408-440e-9813-8e84a59cfe78', lineage).
narrative_ontology:cs_interpretation_layer_present('941a7960-0408-440e-9813-8e84a59cfe78').
narrative_ontology:cs_reading_relation('941a7960-0408-440e-9813-8e84a59cfe78', content_neutrality_doctrine__reed_facial_test_reading, coexists_with).
narrative_ontology:cs_reading_relation('941a7960-0408-440e-9813-8e84a59cfe78', content_neutrality_doctrine__secondary_effects_reading, influences).
narrative_ontology:cs_axiom('941a7960-0408-440e-9813-8e84a59cfe78', foundational, compulsion_is_mirror_of_suppression).
narrative_ontology:cs_axiom_status(compulsion_is_mirror_of_suppression, holdable).
narrative_ontology:cs_axiom_grounding('941a7960-0408-440e-9813-8e84a59cfe78', compulsion_is_mirror_of_suppression, deontological).
narrative_ontology:cs_axiom('941a7960-0408-440e-9813-8e84a59cfe78', foundational, conscience_autonomy_protected).
narrative_ontology:cs_axiom_status(conscience_autonomy_protected, holdable).
narrative_ontology:cs_axiom_grounding('941a7960-0408-440e-9813-8e84a59cfe78', conscience_autonomy_protected, deontological).
narrative_ontology:cs_reference_frame('941a7960-0408-440e-9813-8e84a59cfe78', neutrality_requires_abstention).
narrative_ontology:cs_drift_state('941a7960-0408-440e-9813-8e84a59cfe78', contemporary_pluralist_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('941a7960-0408-440e-9813-8e84a59cfe78', '').
narrative_ontology:cs_kernel_id(content_neutrality_doctrine__compelled_speech_reading, content_neutrality_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(content_neutrality_doctrine__compelled_speech_reading, conscience_objectors).
narrative_ontology:constraint_beneficiary(content_neutrality_doctrine__compelled_speech_reading, religious_dissenters).
narrative_ontology:constraint_beneficiary(content_neutrality_doctrine__compelled_speech_reading, political_minorities).
narrative_ontology:constraint_victim(content_neutrality_doctrine__compelled_speech_reading, civic_cohesion_programs).
narrative_ontology:constraint_victim(content_neutrality_doctrine__compelled_speech_reading, patriotic_orthodoxy_enforcement).
narrative_ontology:constraint_victim(content_neutrality_doctrine__compelled_speech_reading, national_unity_aspirants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE COMPELLED SPEAKER (SNARE) — A citizen required to recite the Pledge, affirm the motto, or perform mandatory patriotic orthodoxy faces maximum extraction: suppress their conscience or suffer legal penalty (school expulsion, loss of citizenship benefits, social stigma). No exit — the obligation is attached to civic participation itself. The state monopolizes the venue (school, naturalization ceremony) and the compulsion mechanism is coercive law, not social pressure. Maximum experienced extraction.
constraint_indexing:constraint_classification(content_neutrality_doctrine__compelled_speech_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE DOCTRINE (ROPE) — From the constitutionalist framework's vantage, compelled speech doctrine is a pure coordination mechanism: it clarifies that the First Amendment protects both the right to speak AND the right not to speak. The doctrine solves a collective action problem (preventing tyranny of the majority through forced orthodoxy) with minimal extractive overhead. The mechanism is transparent: courts apply a formal rule (compelled speech triggers strict scrutiny) without hidden asymmetric benefit. Beneficiary and target align — both citizens and the legal system benefit from predictability.
constraint_indexing:constraint_classification(content_neutrality_doctrine__compelled_speech_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: PATRIOTIC ORTHODOXY ENFORCEMENT COALITION (TANGLED ROPE) — Groups invested in mandatory patriotic expression (school districts, civic organizations, certain political factions) experience the constraint as mixed coordination and extraction. They benefit from using compulsory speech to enforce national unity (coordination function — shared values, civic participation); they also lose the extraction mechanism they relied on (suppression of dissenters, forced affirmation). The constraint does provide a coordination benefit (preventing chaos around flag protocol, prayer, etc.) but at the cost of asymmetric extraction burden (the coalition must abandon enforcement tools). Active enforcement is required to maintain the doctrine's reach — courts must continue overturning compelled speech statutes.
constraint_indexing:constraint_classification(content_neutrality_doctrine__compelled_speech_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: CIVIC RITUAL PERFORMANCE (PITON) — The actual practice of flag salutes, pledge recitations, motto declarations in schools and public settings has become largely theatrical: the state maintains the forms (required classroom recitation, naturalization ceremony scripts) but enforcement has atrophied. After West Virginia v. Board of Education (1943) and subsequent doctrine maturation, compulsion is legally prohibited but the rituals persist. Schools still have flag-raising ceremonies; the pledge is still recited. The theater ratio reflects this: the ritual's binding mechanism has failed (teachers cannot enforce participation), but the performative shell remains. Citizens participate voluntarily or comply with social pressure, not legal coercion. The piton classification captures the attenuation — the mechanism's primary function (enforced orthodoxy) has been disabled by doctrine, but the form persists through institutional inertia.
constraint_indexing:constraint_classification(content_neutrality_doctrine__compelled_speech_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: THE CONSTITUTIONAL REMEDY STRUCTURE (SCAFFOLD) — From the perspective of courts and constitutional doctrine, compelled speech doctrine is a temporary scaffolding mechanism: it provides a bright-line rule (compulsion triggers strict scrutiny) that enables lower courts to handle First Amendment cases without full doctrinal specification. The scaffold has a sunset clause: as social norms shifted and the doctrine matured, the doctrine's protective function was absorbed into broader First Amendment jurisprudence. The mechanism is now largely preventive (statutes are not passed) rather than remedial (cases are not litigated). Low extractiveness at this perspective because the mechanism works — the problem it was designed to solve has been solved, and the doctrine can be sunset as norms solidify.
constraint_indexing:constraint_classification(content_neutrality_doctrine__compelled_speech_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: LOGICAL MIRROR OF NEUTRALITY (MOUNTAIN) — From the perspective of formal logical consistency (civilizational/universal/analytical), the compelled speech doctrine follows from the neutrality principle as an immutable logical law: if neutrality means the state cannot disfavor speech based on content, then by logical inversion, the state also cannot favor speech by forcing its utterance. The two are mirror images under the neutrality framework. This perspective risks naturalizing what is actually a particular doctrinal choice (that neutrality extends to compulsion) as a logical necessity. The engine's false-summit detector will identify this as a contestable reading, not an inevitable law.
constraint_indexing:constraint_classification(content_neutrality_doctrine__compelled_speech_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(content_neutrality_doctrine__compelled_speech_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(content_neutrality_doctrine__compelled_speech_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(content_neutrality_doctrine__compelled_speech_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(content_neutrality_doctrine__compelled_speech_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(content_neutrality_doctrine__compelled_speech_reading, TR),
    TR >= 0.70.

:- end_tests(content_neutrality_doctrine__compelled_speech_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, declining over time. At t=0 (pre-West Virginia v. Board of Education, 1943), compulsion was aggressive — students could be expelled for refusing the flag salute, and the extractiveness was severe (0.78). The doctrine's articulation and maturation reduced the leverage: states can no longer legally compel affirmation. Contemporary extractiveness (0.58) reflects residual extraction through social pressure (peer disapproval, teacher expectation, implicit penalty) rather than legal coercion. The decline trajectory captures doctrinal victories: once the rule is settled, enforcement becomes optional. Suppression (0.62): Moderate-high. The state retains significant capacity to discourage non-participation through institutional channels (classroom norms, civic messaging, naturalization ceremony scripts) even though legal compulsion is prohibited. Conscience objectors face social suppression and implicit pressure. The suppression requirement has declined as norms shifted toward accepting dissent. Theater ratio (0.38): Moderate. Unlike pitons (theater ≥ 0.70), the doctrine still serves real protective function: statutes attempting compulsion are struck down, and the mechanism still prevents significant coercion. The theater component reflects that civic rituals persist in form without legal binding force. The rising theater trajectory shows increasing gap between form (rituals continue) and function (enforcement atrophied).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates radical perspectival divergence from a single structural data set. The compelled speaker experiences snare-level extraction: legal obligation with no exit. The doctrine itself is rope-level coordination: it clarifies constitutional principle and solves collective action problems (preventing tyranny of the majority). The patriotic-orthodoxy coalition experiences tangled rope: they lose extraction leverage but gain clarity and stable rules. Civic rituals present as piton: the form persists, but the enforcing mechanism has failed. Courts see scaffold: the doctrine served as temporary protection while norms shifted; once norms are settled, the strict scrutiny rule can be sunset. The analytical observer sees mountain: logical necessity that neutrality must protect both sides of the speech coin. The gaps are massive because the perspectives occupy genuinely different structural positions relative to the extraction flow. The doctrine's victory (conscience protection) is the coalition's defeat (loss of enforcement); the doctrine's clarity is the compelled speaker's protection; the doctrine's rules are the court's authority.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is derived from structural position. Conscience objectors are victims (high d → high f(d)) with trapped exit options (maximum experienced extraction). The doctrine is beneficiary (low d → low f(d)) with institutional power and arbitrage capacity (can reinterpret, apply selectively). The patriotic coalition is beneficiary under old regime (extraction mechanism relying on compulsion) but victim under doctrine (loss of enforcement tools); their position shifts as the doctrine is applied. The court has institutional power and arbitrage exit (can broaden or narrow the doctrine's scope), making them low-d beneficiaries. The piton perspective shows institutional power but constrained exit (unable to suppress the ritual's attenuation). The scaffold perspective shows organized agents with mobile exit and a sunset mechanism (doctrine can be sunset as norms mature). The mountain perspective risks being a false summit: the analytical observer might treat logical inversion as immutable, but the structural data shows the doctrine is contingent on institutional choices and beneficiary definitions.
 *
 * MANDATROPHY ANALYSIS:
 *   The compelled-speech reading resolves the mandatrophy by showing that the neutrality principle, applied consistently, generates a logical mirror: if the state cannot suppress speech, it also cannot compel speech. The mandatrophy is the apparent contradiction: how can the doctrine be simultaneously rope (coordination mechanism clarifying principle), snare (extracting conscience via compulsion), piton (theatrical ritual), scaffold (temporary doctrinal tool), and mountain (logical necessity)? The resolution is that all six classifications are correct from their respective perspectives. The doctrine coordinates principle for courts (rope), protects conscience from extraction (snare from target view), has become theatrical in enforcement (piton), and serves as temporary scaffolding for First Amendment jurisprudence (scaffold). The mountain classification is the false-summit risk: the doctrine naturalizes a particular reading of neutrality (compulsion matters) rather than deriving inevitably from the principle. The mandatrophy is resolved by accepting perspectival pluralism: the constraint IS all six types, and that plurality is the diagnostically rich feature, not a flaw.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conscience_extractability,
    'Is compelled profession of belief extracting from the agent''s conscience, or merely requiring outward conformity while preserving internal belief?',
    'Theological and phenomenological analysis of conscience: does forcing speech violate conscience only if the speaker''s internal belief is opposite, or does it violate conscience whenever the speaker is not choosing the utterance? If the latter, extractiveness is structural (compulsion itself); if the former, extractiveness depends on belief content (unverifiable).',
    'If conscience is about internal belief only: doctrine''s reach is limited to cases where dissenters can prove contrary belief (high litigation burden). If conscience is about autonomy in utterance: doctrine''s reach is maximal (any compulsion violates conscience). This affects suppression measurement and victim identification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conscience_extractability, conceptual, 'Whether compelled utterance violates conscience independent of internal belief').

omega_variable(
    patriotic_unity_necessity,
    'Is mandatory patriotic expression extracting civic unity, or merely signaling commitment to shared civic symbols?',
    'Empirical: does compulsory flag salute or pledge actually increase patriotic sentiment, civic participation, or national cohesion? Counterfactual comparison with voluntary affirmation systems. Historical correlation analysis across jurisdictions with and without compulsion.',
    'If compulsion achieves genuine unity: the doctrine suppresses a coordination mechanism, and the extraction burden on dissenters is matched by a coordination benefit (false calculation, but believed by enforcement coalition). If compulsion does not achieve unity: the doctrine suppresses mere theater, and the extraction is uncompensated. This shapes whether the constraint is tangled_rope (mixed) or snare (pure extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(patriotic_unity_necessity, empirical, 'Whether mandatory patriotic expression actually increases civic unity').

omega_variable(
    doctrinal_reading_kernel_contest,
    'Is this reading of content neutrality (compulsion is the mirror of suppression) the same constitutional doctrine as the Reed facial-test reading (content-based on its face) and the secondary-effects reading (content-neutral by underlying purpose)?',
    'Doctrinal genealogy: trace which Supreme Court opinions explicitly endorse the ''compulsion is mirror'' framing vs. the facial-test framing vs. the secondary-effects framing. Identify whether courts treat these as one principle with multiple applications or as three distinct principles that sometimes conflict.',
    'If same principle with different applications: all three readings are legitimate perspectives on one constraint (network decomposition is wrong). If three distinct principles: each reading is a separate constraint with different ε values and different structural logic. This affects whether the constraint story should be split into three separate files.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(doctrinal_reading_kernel_contest, conceptual, 'Whether the compelled-speech reading is one doctrine or one reading of contested kernel').

omega_variable(
    authority_grounding_stability,
    'Does the compelled speech doctrine ground its authority in the text of the First Amendment (''Congress shall make no law... abridging freedom of speech''), or in a judicial interpretation that goes beyond the text (adding ''forced utterance'' to ''abridging speech'')?',
    'Originalist vs. living-constitution debate: does the text''s prohibition on ''abridging freedom of speech'' naturally extend to compelled speech, or is that an interpretive move by courts? If the latter, the doctrine''s authority rests on interpretive authority (lineage/expertise) rather than textual authority (fixed text).',
    'If textual: the doctrine is a mountain (immutable principle embedded in constitutional language). If interpretive: the doctrine is a rope or tangled_rope (could be reinterpreted or overruled by interpretive authority shift). This affects whether the mountain classification from the analytical perspective is sustainable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(authority_grounding_stability, conceptual, 'Textual vs. interpretive grounding of compelled speech doctrine').

omega_variable(
    false_summit_natural_law_claim,
    'Is the compelled speech doctrine a genuine natural law of constitutional logic (logically entailed by the neutrality principle), or a constructed doctrine that benefits identifiable actors (conscience objectors, judicial authority) and naturalized as inevitable?',
    'Comparative constitutional analysis: do other legal systems'' neutrality doctrines require the same protection against compelled speech? If yes, natural law candidate. If no, the doctrine is contingent on U.S. constitutional interpretation and beneficiary-structure choices.',
    'If natural law: the mountain perspective is correct, and the false-summit detection is wrong. If constructed: the false-summit detector is right, and the doctrine naturalizes a contestable choice. This determines whether the compelled speech doctrine can be reclassified away from mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_claim, empirical, 'Whether compelled speech protection is natural law or constructed doctrine').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(content_neutrality_doctrine__compelled_speech_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cncs_tr_t0, content_neutrality_doctrine__compelled_speech_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cncs_tr_t15, content_neutrality_doctrine__compelled_speech_reading, theater_ratio, 15, 0.28).
narrative_ontology:measurement(cncs_tr_t30, content_neutrality_doctrine__compelled_speech_reading, theater_ratio, 30, 0.38).

% Extraction over time
narrative_ontology:measurement(cncs_be_t0, content_neutrality_doctrine__compelled_speech_reading, base_extractiveness, 0, 0.78).
narrative_ontology:measurement(cncs_be_t15, content_neutrality_doctrine__compelled_speech_reading, base_extractiveness, 15, 0.68).
narrative_ontology:measurement(cncs_be_t30, content_neutrality_doctrine__compelled_speech_reading, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(cncs_su_t0, content_neutrality_doctrine__compelled_speech_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(cncs_su_t15, content_neutrality_doctrine__compelled_speech_reading, suppression_requirement, 15, 0.72).
narrative_ontology:measurement(cncs_su_t30, content_neutrality_doctrine__compelled_speech_reading, suppression_requirement, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(content_neutrality_doctrine__compelled_speech_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(content_neutrality_doctrine__compelled_speech_reading, content_neutrality_doctrine__reed_facial_test_reading).
narrative_ontology:affects_constraint(content_neutrality_doctrine__compelled_speech_reading, content_neutrality_doctrine__secondary_effects_reading).

% DUAL FORMULATION NOTE:
% The content_neutrality_doctrine kernel has three structurally distinct readings with different ε values and different beneficiary/victim structures. The compelled_speech_reading emphasizes the suppression of mandated orthodoxy (ε ≈ 0.58); the reed_facial_test_reading emphasizes facial categorization (ε ≈ 0.42, lower extraction because it applies formal rule); the secondary_effects_reading permits extraction if justified by non-content purpose (ε ≈ 0.65, higher extraction because the content-defined category is allowed). These are not three measurements of one constraint under different observables — they are three distinct readings of a contested kernel, each with its own structural logic. The network edges link them as siblings in the kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(content_neutrality_doctrine__compelled_speech_reading, powerful, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
