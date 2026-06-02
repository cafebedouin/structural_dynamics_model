% ============================================================================
% CONSTRAINT STORY: salic_prohibition__sovereign_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_salic_prohibition__sovereign_override_reading, []).

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
 *   constraint_id: salic_prohibition__sovereign_override_reading
 *   human_readable: Salic Law as Revocable Positive Law: The Sovereign Override Reading
 *   domain: constitutional_law/dynastic_succession/political_history
 *
 * SUMMARY:
 *   This constraint instantiates the 'sovereign override' reading of Salic
 *   Law as a contested kernel of constitutional legitimacy. Under this
 *   reading, Salic prohibition (exclusion of women from succession) is
 *   positive law enacted by sovereign authority and therefore revocable by
 *   sovereign authority. The Pragmatic Sanction — the sovereign's explicit
 *   authorization of female succession — is the mechanism that demonstrates
 *   sovereignty's power over Salic rules. This reading characterizes
 *   challengers to female succession (after a Pragmatic Sanction) as rebels
 *   against legitimate authority, not defenders of immutable law. The
 *   constraint exhibits Tangled Rope structure: genuine coordination function
 *   (stable succession rules) combined with asymmetric extraction (sovereign
 *   retains unilateral power to alter succession, disenfranchising female
 *   heirs unless sovereign chooses their inclusion). The extractiveness has
 *   risen over the measurement interval (0.38 → 0.58) as the repeated
 *   normalization of Pragmatic Sanction overrides demonstrates the
 *   constraint's contingency on sovereign will rather than natural law. The
 *   theater ratio has risen (0.48 → 0.70) as justifications for Salic
 *   exclusion become increasingly theatrical — the church and legal apparatus
 *   continue to invoke biological/theological grounds for female incapacity
 *   even as they simultaneously crown female sovereigns under override
 *   authority. The suppression requirement rises (0.55 → 0.72) because
 *   maintaining the constraint's legitimacy requires increasing rhetorical
 *   and institutional effort as the empirical falsification accumulates:
 *   female rule proves effective, precedent establishes that female
 *   succession can stabilize dynasties, and the grounds for exclusion erode.
 *
 * KEY AGENTS:
 *   - Reigning Sovereign: Primary beneficiary (institutional/arbitrage) — sole authority to decide whether to invoke Pragmatic Sanction; controls succession outcomes
 *   - Court and Executive Authority: Beneficiary (institutional/arbitrage) — benefits from coordination (stable succession rules) and from sovereign's ability to use override as reward/punishment mechanism
 *   - Disinherited Female Claimants: Primary victim (powerless/trapped) — barred from succession unless sovereign grants exception; cannot appeal to law, only to sovereign mercy
 *   - Cognatic Succession Advocates (Nobility, Legal Scholars): Secondary victims (moderate/constrained) — bear costs of advocacy; risk treason charges; benefit from eventual overrides that validate their position
 *   - Legal and Ecclesiastical Authority: Institutional actors (institutional/constrained) — must justify both Salic prohibition and its overrides; maintain increasingly incoherent naturalizing narratives
 *   - Analytical Observer: Sees through naturalizing moves (analytical/analytical) — recognizes false summit in the 'natural law' framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__sovereign_override_reading, 0.52).
domain_priors:suppression_score(salic_prohibition__sovereign_override_reading, 0.68).
domain_priors:theater_ratio(salic_prohibition__sovereign_override_reading, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__sovereign_override_reading, tangled_rope).
narrative_ontology:human_readable(salic_prohibition__sovereign_override_reading, "Salic Law as Revocable Positive Law: The Sovereign Override Reading").
narrative_ontology:topic_domain(salic_prohibition__sovereign_override_reading, "constitutional_law/dynastic_succession/political_history").

domain_priors:requires_active_enforcement(salic_prohibition__sovereign_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__sovereign_override_reading, '91abc398-4652-4834-b6c2-d1f4ca54a7db').
narrative_ontology:cs_kernel_codification('91abc398-4652-4834-b6c2-d1f4ca54a7db', formalized).
narrative_ontology:cs_authority_grounding('91abc398-4652-4834-b6c2-d1f4ca54a7db', extraction).
narrative_ontology:cs_interpretation_layer_present('91abc398-4652-4834-b6c2-d1f4ca54a7db').
narrative_ontology:cs_reading_relation('91abc398-4652-4834-b6c2-d1f4ca54a7db', salic_prohibition__immutable_mandate_reading, forecloses).
narrative_ontology:cs_reading_relation('91abc398-4652-4834-b6c2-d1f4ca54a7db', salic_prohibition__cognatic_reversion_reading, influences).
narrative_ontology:cs_axiom('91abc398-4652-4834-b6c2-d1f4ca54a7db', foundational, sovereignty_authority_over_succession).
narrative_ontology:cs_axiom_status(sovereignty_authority_over_succession, holdable).
narrative_ontology:cs_axiom_grounding('91abc398-4652-4834-b6c2-d1f4ca54a7db', sovereignty_authority_over_succession, conventional).
narrative_ontology:cs_axiom('91abc398-4652-4834-b6c2-d1f4ca54a7db', foundational, pragmatic_sanction_as_legitimate_authority).
narrative_ontology:cs_axiom_status(pragmatic_sanction_as_legitimate_authority, holdable).
narrative_ontology:cs_axiom_grounding('91abc398-4652-4834-b6c2-d1f4ca54a7db', pragmatic_sanction_as_legitimate_authority, conventional).
narrative_ontology:cs_reference_frame('91abc398-4652-4834-b6c2-d1f4ca54a7db', sovereign_legislative_authority_over_dynastic_rules).
narrative_ontology:cs_drift_state('91abc398-4652-4834-b6c2-d1f4ca54a7db', contemporary_legal_reform_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('91abc398-4652-4834-b6c2-d1f4ca54a7db', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(salic_prohibition__sovereign_override_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__sovereign_override_reading, reigning_sovereign).
narrative_ontology:constraint_beneficiary(salic_prohibition__sovereign_override_reading, executive_authority).
narrative_ontology:constraint_victim(salic_prohibition__sovereign_override_reading, disinherited_female_claimants).
narrative_ontology:constraint_victim(salic_prohibition__sovereign_override_reading, cognatic_succession_advocates).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED FEMALE HEIR (SNARE) — Completely barred from inheritance by Salic Law and trapped by the sovereign's arbitrary refusal to invoke override authority (Pragmatic Sanction). Cannot exit through legal challenge, cannot challenge the king's authority to override without committing treason. Experiences maximum extraction: denied succession rights available to brothers; loses dynasty, lands, political power. No meaningful access to sovereign authority that could grant exception.
constraint_indexing:constraint_classification(salic_prohibition__sovereign_override_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COGNATIC SUCCESSION ADVOCATES (TANGLED ROPE) — Moderate power through alliance networks, legal argumentation, and dynastic pressure. Constrained: advocating for female succession risks charges of sedition and rebellion; defending the Pragmatic Sanction (female override) means accepting the king's unilateral authority to change succession rules. Moderate extraction: bear costs of advocacy and potential disloyalty accusations, but also benefit from dynastic stability mechanisms and negotiated settlement when override is granted. Mixed coordination (stabilizing succession) and extraction (excluding females unless sovereign chooses otherwise).
constraint_indexing:constraint_classification(salic_prohibition__sovereign_override_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REIGNING SOVEREIGN AND COURT (ROPE) — Experiences the constraint as pure coordination: Salic Law provides stable succession rules that reduce civil war risk and dynastic chaos. The override mechanism (Pragmatic Sanction) is a coordination tool, not an extraction mechanism — it allows the sovereign to solve the specific succession problem (no male heir) without dissolving the broader rule structure. Benefits from both the rule and the flexibility to alter it. No meaningful costs to this agent. Arbitrage exit: can ignore Salic Law entirely and declare female succession, or enforce it strictly; no barriers to either choice.
constraint_indexing:constraint_classification(salic_prohibition__sovereign_override_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: NOBILITY AND PROVINCIAL POWERS (TANGLED ROPE) — Constrained by the sovereign's authority to override, but benefit from coordination function. Salic Law provides predictable succession rules (coordination benefit); Pragmatic Sanction override creates uncertainty (extraction cost). Benefits if the resulting sovereign — whether female successor or male heir — stabilizes their region; harmed if the succession produces weak governance or contested legitimacy. Both beneficiary and victim depending on implementation. Cannot exit without challenging sovereign authority, which risks rebellion charges.
constraint_indexing:constraint_classification(salic_prohibition__sovereign_override_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGAL REFORM COALITION / ENLIGHTENMENT JURISTS (SCAFFOLD) — Organized advocates (legal scholars, reform-minded nobles) see Salic Law as a temporary coordination mechanism with an inherent sunset: as succession practices evolve and female sovereigns demonstrate competence (Maria Theresa, Catherine the Great), the justification for female exclusion erodes. The constraint operates within bounds (generational time horizon) and agents see an exit path through accumulated precedent and legal reinterpretation. Sunset mechanism: each Pragmatic Sanction override establishes precedent that delegitimizes the underlying Salic exclusion; after sufficient overrides, the rule becomes unenforceable without explicit repudiation. Theater: the overlay of legal justifications for exclusion (women's alleged unsuitability) is performative and collapses when female rule proves effective.
constraint_indexing:constraint_classification(salic_prohibition__sovereign_override_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ECCLESIASTICAL AUTHORITY / LEGITIMACY APPARATUS (PITON) — The church initially provided religious and legal justification for Salic exclusion (interpretations of Biblical law, canon law arguments about female authority). Over centuries, these justifications become increasingly theatrical — maintained because the institutional memory persists, not because the theological or legal argument convinces contemporaries. The church retains arbitrage exit (can pivot legitimacy claims to alternative grounds) but continues performing the degraded Salic justification out of institutional inertia. Theater ratio high: the legitimacy apparatus invokes Salic reasoning even as it simultaneously crowns and anoints female sovereigns under Pragmatic Sanction authority. The contradiction between what is performed (Salic Law is immutable) and what is enacted (sovereigns override it) is the signature piton dynamic.
constraint_indexing:constraint_classification(salic_prohibition__sovereign_override_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW READING (MOUNTAIN — FALSE SUMMIT CANDIDATE) — From a civilizational/universal perspective, defenders of this reading might claim Salic prohibition is a natural law reflecting immutable biological or psychological facts about female incapacity for rulership. This perspective claims the constraint emerges naturally (ε ≤ 0.25, suppression minimal, accessibility collapse high). However, the structural data contradicts the mountain classification: the constraint has explicit beneficiaries (sovereign, court) whose power to override is itself recognized in the framework; victims can be identified; extraction is measurable and variable. The engine's false summit detector will flag this as naturalization of a contingent institutional arrangement. The 'natural law' reading is a beneficiary frame, not an analytical observation.
constraint_indexing:constraint_classification(salic_prohibition__sovereign_override_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(salic_prohibition__sovereign_override_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(salic_prohibition__sovereign_override_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(salic_prohibition__sovereign_override_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(salic_prohibition__sovereign_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(salic_prohibition__sovereign_override_reading, TR),
    TR >= 0.70.

:- end_tests(salic_prohibition__sovereign_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The sovereign override reading reveals Salic Law as contingent positive law subject to sovereign will rather than immutable natural law. This is extractive because the sovereign unilaterally controls succession — disinheriting females unless sovereign chooses otherwise means females are the structural default excluded group. The moderate level reflects that the extraction is intermittent: when sovereigns invoke Pragmatic Sanction, extraction is temporarily lifted for that individual female heir. Over the 100-year interval, extractiveness rises from 0.38 to 0.58 because the repeated pattern of overrides reveals the constraint's contingency — once Pragmatic Sanction becomes a normalized expectation, the extraction becomes more visible and more resented. Suppression (0.68): High. Maintained through legal apparatus (courts cannot invalidate Salic prohibition), ecclesiastical justification (theological arguments about female incapacity), and coercive readiness (rebellion against a female successor granted by Pragmatic Sanction is treated as treason). The suppression requirement rises from 0.55 to 0.72 over the interval because the empirical falsification of the naturalizing claims requires increasing rhetorical effort to maintain the constraint. Theater ratio (0.64): Moderately high and rising. The church and legal apparatus perform Salic justifications (biological incapacity, theological unfitness) even as they crown and anoint female sovereigns. This contradiction is the signature piton dynamic. The theater rises from 0.48 to 0.70 as the gap widens between what is asserted (females are incapable) and what is enacted (females rule effectively). Claimed type (Tangled Rope): The constraint combines genuine coordination (reducing civil war risk, providing stable succession rules) with asymmetric extraction (sovereign unilateral control, female default exclusion). Requires active enforcement: true — the suppression requires legal, ecclesiastical, and coercive apparatus to maintain.
 *
 * PERSPECTIVAL GAP:
 *   The excluded female heir (Snare perspective) perceives a pure extraction with no coordination benefit — she is barred from power unless sovereign grants exception. The cognatic advocates (Tangled Rope perspective) perceive mixed coordination and extraction — they benefit from succession stability mechanisms but are constrained by the sovereign's unilateral override authority. The sovereign (Rope perspective) perceives pure coordination — Salic Law reduces civil war risk and the override mechanism is a tool for solving specific problems without destroying the broader rule structure. The nobility (Tangled Rope perspective, different from cognatic advocates) perceive coordination benefits from stable succession rules but extraction costs from uncertainty introduced by override mechanism. The legal reformers (Scaffold perspective) perceive a temporary mechanism with an inherent sunset — as female sovereigns prove effective and precedent accumulates, the Salic exclusion becomes unenforceable. The church and legal apparatus (Piton perspective) perceive degraded performance — the naturalizing justifications no longer convince, but the institutional structure persists through inertia. The analytical observer risks the false summit framing (Mountain perspective) of treating Salic prohibition as immutable natural law, which the structural data contradicts: beneficiaries exist, victims are identifiable, extraction is measurable and contingent on sovereign will.
 *
 * DIRECTIONALITY LOGIC:
 *   The sovereign's directionality (d ≈ 0.00) is derived from beneficiary status and arbitrage exit: the sovereign benefits from the constraint's coordination function and can override it unilaterally; no exit costs apply. This produces f(d) ≈ -0.12, negative effective extraction — the constraint subsidizes the sovereign's authority. The female heir's directionality (d ≈ 0.95) is derived from victim status and trapped exit: completely barred from succession unless sovereign grants exception; cannot legally challenge the arrangement; maximum extraction. The cognatic advocates' directionality (d ≈ 0.58) is derived from victim status (they are constrained by the sovereign's authority to override) but organized power (networks, legal argumentation, dynastic pressure); moderate effective extraction. The nobility's directionality (d ≈ 0.52) is derived from mixed beneficiary/victim status and constrained exit: benefit from succession coordination, harmed by succession uncertainty introduced by override mechanism. The legal reformers' directionality (d ≈ 0.45) is derived from victim status (constrained by threat of treason charges) but organized power and emerging exit paths (accumulated precedent eroding Salic justifications). The piton apparatus has arbitrage exit (can pivot legitimacy claims) but continues performing degraded justifications, suggesting d ≈ 0.15. The analytical observer's directionality (d ≈ 0.72) is derived from external analytical position.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolution is routed through the omega variables. The constraint exhibits Tangled Rope structure (genuine coordination + asymmetric extraction) from the primary analytical perspectives, and this classification is stable across the interval. The mandatrophy problem ('is this coordination or extraction?') is resolved by the structural claim that Salic Law is positive law contingent on sovereign will. If Salic prohibition is immutable natural law (immutability reading), then the classification problem dissolves — it is a Mountain, and extraction is illusory (natural laws don't extract, they constrain universally). If female succession is the default under cognatic rules (cognatic reversion reading), then Salic prohibition is itself an extractive overlay, not a coordination mechanism. The sovereign override reading asserts that the constraint is Tangled Rope because it performs both functions: coordination (stable succession rules enacted by sovereign authority) and extraction (the sovereign's unilateral control over succession outcomes). The measurement interval shows how the Tangled Rope classification becomes increasingly unstable as theater ratio rises and extractiveness rises — the coordination function becomes less visible and the extraction becomes more salient. This is the diagnostic signature of Tangled Rope degrading toward Snare: the coordination benefit (stable succession rules) is real, but the extraction benefit (sovereign control) becomes the dominant driver of the constraint's persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pragmatic_sanction_ontology,
    'Is the Pragmatic Sanction an exception to Salic Law (treating Salic prohibition as immutable unless sovereign chooses differently) or a demonstration that Salic Law is merely sovereign positive law with no intrinsic force?',
    'Textual analysis of the Pragmatic Sanction''s language: does it frame the override as a one-time dispensation from an otherwise immutable rule, or as an exercise of sovereign legislative authority that subordinates Salic Law to political will? Comparison across multiple Pragmatic Sanctions (Austria, Spain, France) to identify consistent framing.',
    'If exception: Salic Law retains immutability claim and the override reading''s core premise (sovereignty can revoke positive law) is compatible with the immutability reading. If legislative subordination: Salic Law is revealed as contingent positive law without intrinsic force, foreclosing the immutability reading''s core assertion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pragmatic_sanction_ontology, empirical, 'Whether Pragmatic Sanction frames override as exception or as legislative authority').

omega_variable(
    biological_contingency_of_exclusion,
    'Does the historical record of female sovereigns demonstrate that the alleged biological or psychological grounds for Salic exclusion were empirically false, or were those grounds always post-hoc justifications for political decisions made on other bases?',
    'Historical analysis of female rule outcomes (Maria Theresa, Catherine II, Elizabeth I, Isabella of Castile) comparing actual governance capacity to the theoretical incapacities alleged in Salic justifications. Textual analysis of Salic arguments across time to identify whether justifications shift in response to female rulers'' actual performance.',
    'If empirically false: the naturalizing move (Salic Law as immutable natural law) is exposed as fraudulent, and the constraint is revealed as pure institutional extraction dressed in naturalizing language. If justifications shifted strategically: demonstrates that the naturalization was always performative, and the sovereign override reading is the more accurate structural account.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(biological_contingency_of_exclusion, empirical, 'Whether female ruler performance refutes alleged grounds for Salic exclusion').

omega_variable(
    succession_stability_mechanism,
    'Does the Pragmatic Sanction override mechanism actually reduce civil war risk and succession instability compared to strict Salic enforcement, or does it increase instability by introducing procedural uncertainty about who will inherit?',
    'Comparative historical analysis: succession disputes in regimes that permitted female override (Austria, Hungary) vs strict Salic enforcement (France until 1830s). Measurement of timeline to succession resolution, intensity of dynastic conflict, external intervention rates.',
    'If reduces risk: the coordination function of the sovereign override reading is validated, and Tangled Rope classification is appropriate. If increases uncertainty: the constraint is revealed as primarily extractive (sovereign retains unilateral authority to disinherit heirs at whim), suggesting Snare classification for broader perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(succession_stability_mechanism, empirical, 'Whether override mechanism improves or worsens succession stability').

omega_variable(
    reading_foreclosure_by_practice,
    'Do the repeated, normalized invocations of Pragmatic Sanction override in the 18th–19th centuries constitute sufficient evidence to foreclose the ''immutable mandate'' reading within the same constitutional framework?',
    'Textual and conceptual analysis: if a framework asserts that Salic Law is immutable natural law, but sovereigns successfully override it repeatedly without legal challenge to sovereign authority, at what point does the practice (override as normal) foreclose the premise (immutability as essential)?',
    'If foreclosure is valid: the immutable mandate reading is logically incoherent within the framework that acknowledges Pragmatic Sanction authority, eliminating it as a competing reading and establishing this sovereign override reading as dominant within the constitutional framework. If foreclosure fails: multiple readings coexist, and the contest continues as an interpretation question rather than a logical contradiction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_by_practice, conceptual, 'Whether repeated practice forecloses the immutability reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__sovereign_override_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(salic_override_theater_t0, salic_prohibition__sovereign_override_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(salic_override_theater_t50, salic_prohibition__sovereign_override_reading, theater_ratio, 50, 0.62).
narrative_ontology:measurement(salic_override_theater_t100, salic_prohibition__sovereign_override_reading, theater_ratio, 100, 0.7).

% Extraction over time
narrative_ontology:measurement(salic_override_extract_t0, salic_prohibition__sovereign_override_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(salic_override_extract_t50, salic_prohibition__sovereign_override_reading, base_extractiveness, 50, 0.52).
narrative_ontology:measurement(salic_override_extract_t100, salic_prohibition__sovereign_override_reading, base_extractiveness, 100, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(salic_override_suppress_t0, salic_prohibition__sovereign_override_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(salic_override_suppress_t50, salic_prohibition__sovereign_override_reading, suppression_requirement, 50, 0.68).
narrative_ontology:measurement(salic_override_suppress_t100, salic_prohibition__sovereign_override_reading, suppression_requirement, 100, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(salic_prohibition__sovereign_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(salic_prohibition__sovereign_override_reading, salic_prohibition__immutable_mandate_reading).
narrative_ontology:affects_constraint(salic_prohibition__sovereign_override_reading, salic_prohibition__cognatic_reversion_reading).
narrative_ontology:affects_constraint(salic_prohibition__sovereign_override_reading, pragmatic_sanction_legitimacy).
narrative_ontology:affects_constraint(salic_prohibition__sovereign_override_reading, female_succession_precedent).

% DUAL FORMULATION NOTE:
% The Salic prohibition kernel decomposes into three structurally distinct readings with different ε values, beneficiary/victim structures, and temporal dynamics. The sovereign override reading (this constraint, ε≈0.52) treats the prohibition as positive law contingent on sovereign will. The immutable mandate reading (ε≈0.08-0.15, Mountain candidate) treats Salic prohibition as immutable natural law reflecting biological incapacity. The cognatic reversion reading (ε≈0.35-0.45, Tangled Rope/Scaffold candidate) treats the prohibition as a removable male-preference overlay superimposed on an underlying cognatic default. Each reading has its own perspectives, beneficiary/victim structure, and classification. They are linked through network effects: the repeated invocation of Pragmatic Sanction in the sovereign override reading generates precedent that affects the immutable mandate reading (by demonstrating contingency) and supports the cognatic reversion reading (by showing that female succession can stabilize dynasties). The three readings form a presheaf over the Salic prohibition observation site.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(salic_prohibition__sovereign_override_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
