% ============================================================================
% CONSTRAINT STORY: tenth_amendment__truism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tenth_amendment__truism_reading, []).

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
 *   constraint_id: tenth_amendment__truism_reading
 *   human_readable: Tenth Amendment as Doctrinal Truism (Darby Reading)
 *   domain: constitutional_law/federalism_doctrine
 *
 * SUMMARY:
 *   The Tenth Amendment reads: 'The powers not delegated to the United States
 *   by the Constitution, nor prohibited by it to the States, are reserved to
 *   the States respectively, or to the people.' For the first 150 years, this
 *   was understood as a substantive limit on federal power — states retained
 *   independent authority over matters not enumerated in Article I. But as
 *   federal commerce power expanded, this interpretation became troublesome
 *   for broad federal authority. Darby (1941) offered a doctrinal solution:
 *   the Tenth Amendment is not a limit on federal power; it is merely a
 *   truism, restating what the enumeration structure already implies. The
 *   Amendment adds 'emphasis' but 'nothing else.' This reading suppresses any
 *   independent Tenth Amendment doctrine. If the Amendment is purely
 *   redundant, then litigants cannot invoke it as an independent
 *   constitutional ground. Federal power is limited only by enumeration. The
 *   truism reading converts a potential constitutional barrier into an empty
 *   statement, enabling federal reach to expand without formal constitutional
 *   revision.
 *
 * KEY AGENTS:
 *   - States' Rights Litigants: Primary victims (powerless/trapped) — trapped by the truism's doctrinal move; cannot invoke Tenth Amendment grounds without contradicting Darby
 *   - Broad Federal Power Doctrine: Primary beneficiary (institutional/arbitrage) — benefits from neutralization of Tenth as a limiting text; experiences the truism as pure coordination
 *   - Anticommandeering Doctrine Coalition: Secondary actor (moderate/constrained) — constrained by truism suppression of Tenth's independent force, but benefits from structural clarity that propels development of anticommandeering as alternative federalism doctrine
 *   - Traditional Tenth Amendment Formalism: Institutional actor (institutional/arbitrage) — maintains textual invocation of Tenth as ritual (theater = 0.85) despite doctrinal degradation (piton perspective)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the truism as structural inevitability when it is actually a contingent doctrinal choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenth_amendment__truism_reading, 0.52).
domain_priors:suppression_score(tenth_amendment__truism_reading, 0.68).
domain_priors:theater_ratio(tenth_amendment__truism_reading, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenth_amendment__truism_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(tenth_amendment__truism_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(tenth_amendment__truism_reading, theater_ratio, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenth_amendment__truism_reading, snare).
narrative_ontology:human_readable(tenth_amendment__truism_reading, "Tenth Amendment as Doctrinal Truism (Darby Reading)").
narrative_ontology:topic_domain(tenth_amendment__truism_reading, "constitutional_law/federalism_doctrine").

domain_priors:requires_active_enforcement(tenth_amendment__truism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenth_amendment__truism_reading, 'b7e4175a-ca55-4b29-8399-5d8c5a7a036f').
narrative_ontology:cs_kernel_codification('b7e4175a-ca55-4b29-8399-5d8c5a7a036f', fixed_text).
narrative_ontology:cs_authority_grounding('b7e4175a-ca55-4b29-8399-5d8c5a7a036f', extraction).
narrative_ontology:cs_interpretation_layer_present('b7e4175a-ca55-4b29-8399-5d8c5a7a036f').
narrative_ontology:cs_reading_relation('b7e4175a-ca55-4b29-8399-5d8c5a7a036f', tenth_amendment__anticommandeering_doctrine, influences).
narrative_ontology:cs_reading_relation('b7e4175a-ca55-4b29-8399-5d8c5a7a036f', tenth_amendment__political_safeguards_reading, coexists_with).
narrative_ontology:cs_axiom('b7e4175a-ca55-4b29-8399-5d8c5a7a036f', foundational, tenth_amendment_is_restatement_of_enumeration).
narrative_ontology:cs_axiom_status(tenth_amendment_is_restatement_of_enumeration, holdable).
narrative_ontology:cs_axiom_grounding('b7e4175a-ca55-4b29-8399-5d8c5a7a036f', tenth_amendment_is_restatement_of_enumeration, deontological).
narrative_ontology:cs_axiom('b7e4175a-ca55-4b29-8399-5d8c5a7a036f', foundational, enumeration_exhaustively_specifies_federal_power).
narrative_ontology:cs_axiom_status(enumeration_exhaustively_specifies_federal_power, holdable).
narrative_ontology:cs_axiom_grounding('b7e4175a-ca55-4b29-8399-5d8c5a7a036f', enumeration_exhaustively_specifies_federal_power, deontological).
narrative_ontology:cs_reference_frame('b7e4175a-ca55-4b29-8399-5d8c5a7a036f', tenth_amendment_as_restatement_of_enumeration).
narrative_ontology:cs_drift_state('b7e4175a-ca55-4b29-8399-5d8c5a7a036f', contemporary, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b7e4175a-ca55-4b29-8399-5d8c5a7a036f', '').
narrative_ontology:cs_kernel_id(tenth_amendment__truism_reading, tenth_amendment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenth_amendment__truism_reading, broad_federal_power_readings).
narrative_ontology:constraint_victim(tenth_amendment__truism_reading, states_rights_litigation_strategies).
narrative_ontology:constraint_victim(tenth_amendment__truism_reading, independent_tenth_amendment_doctrine).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STATES' RIGHTS LITIGANTS (SNARE) — Trapped in a doctrinal position where the Tenth Amendment is declared redundant by its reading. Cannot exit the constraint without abandoning constitutional argumentation itself. The truism reading forecloses independent Tenth Amendment claims: if the Amendment merely restates structural implication, litigation based on Tenth Amendment grounds alone has no independent doctrinal force. Maximum experienced extraction — the path to states'-rights victory is suppressed by declaring the only viable constitutional pathway redundant.
constraint_indexing:constraint_classification(tenth_amendment__truism_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: BROAD FEDERAL POWER DOCTRINE (ROPE) — The truism reading is coordinative for federal power expansion. It solves the problem of a constitutional text (the Tenth Amendment) that could obstruct expansive federal reach. The solution: declare the text redundant, convert it from a limit on power to an empty restatement. Federal actors experience this as pure coordination — the amendment's constraint is neutralized without formal override, through doctrinal reframing. Net beneficiary — extraction runs away from this agent, coordination runs toward them.
constraint_indexing:constraint_classification(tenth_amendment__truism_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: ANTICOMMANDEERING DOCTRINE COALITION (TANGLED ROPE) — Constrained by the truism reading's suppression of independent Tenth Amendment grounds but benefits from the structural clarity the truism provides. If the Tenth is merely restatement, then federalism protection must come from structural limits (the anticommandeering doctrine — Congress cannot commandeer state legislatures). This perspective both pays the cost of Tenth Amendment suppression and captures the benefit of a sharper, more defensible federalism doctrine. Moderate experience of extraction, but with compensating coordination function.
constraint_indexing:constraint_classification(tenth_amendment__truism_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: TRADITIONAL TENTH AMENDMENT FORMALISM (PITON) — The textual argument that the Tenth Amendment contains independent substantive limits on federal power persists as institutional inertia despite doctrinal degradation. The Darby truism reading declared this reading inert — a restatement rather than a limit. The formal recitation of the Tenth continues in judicial opinions (theater = 0.85), but its functional force is removed. The traditional formalist perspective sees the constraint as maintenance of a degraded ritual: the text is invoked and quoted, but Darby teaches that it adds nothing of substance.
constraint_indexing:constraint_classification(tenth_amendment__truism_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: STRUCTURAL INEVITABILITY VIEW (MOUNTAIN) — From the analytical observer's civilizational perspective, the truism reading appears as a recognition of structural law: the Tenth Amendment MUST be a restatement of structural implication because the Constitution already defines federal power by enumeration. Any power not enumerated is logically reserved to the states. This perspective sees the truism reading as discovering an immutable feature of written constitutionalism itself — not a doctrinal move, but a mathematical truth about the structure. The engine will compute this as a false summit: what appears as structural inevitability is actually a contingent doctrinal choice to read the Tenth as non-limiting.
constraint_indexing:constraint_classification(tenth_amendment__truism_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tenth_amendment__truism_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(tenth_amendment__truism_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(tenth_amendment__truism_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(tenth_amendment__truism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(tenth_amendment__truism_reading, TR),
    TR >= 0.70.

:- end_tests(tenth_amendment__truism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The truism reading extracts significant doctrinal value for broad federal power by suppressing a genuinely alternative constitutional reading. The extraction is not maximal (snares approach 0.66+) because the Tenth Amendment's textual basis is real — the truism does not require outright falsification, only reinterpretation of what the text means. The truism can be defended as a coherent reading of enumerated powers structure, which limits how much extraction the reading itself admits. Suppression (0.68): High. The truism reading actively suppresses independent Tenth Amendment doctrine by declaring it redundant. States'-rights litigants cannot escape this suppression without abandoning the constitutional framework itself. The suppression operates through doctrinal authority (Darby is binding precedent) rather than through formal ban — the effect is equivalent to barring Tenth Amendment claims. Theater ratio (0.85): Very high. The Tenth Amendment continues to be quoted and invoked in judicial opinions post-Darby, but Darby teaches that such invocations add nothing substantive. The ritual persists (theater rises over time as courts routinely cite the Tenth while acknowledging it is not limiting). The measurements show extractiveness rising and theater rising in tandem from 1787 (low both) through 1927 (moderate both) to 1972 (high both) — as federal commerce power expanded and Darby's logic became settled, the constraint's performance intensified.
 *
 * PERSPECTIVAL GAP:
 *   The truism reading shows maximum perspectival divergence. Federal power doctrine sees coordination (Rope) — the truism solves the problem of a text that could obstruct federal reach. States'-rights litigants see pure extraction (Snare) — their constitutional pathway is declared inert. Anticommandeering scholars see a mixed system (Tangled Rope) — the truism's suppression of Tenth produces structural clarity that enables alternative (anticommandeering) federalism doctrine to develop. Traditional formalism sees a degraded ritual (Piton) — the Tenth Amendment persists in opinions but Darby has stripped its limiting force. The analytical observer risks seeing natural law (Mountain) — written constitutionalism logically implies the Tenth is redundant — but the false summit detector reveals that what looks like structural necessity is actually a contingent doctrinal choice.
 *
 * DIRECTIONALITY LOGIC:
 *   The truism reading's beneficiary (broad federal power doctrine) derives d from institutional power + arbitrage exit: the federal state can always exit the truism reading's constraints by amending the Constitution if needed, but under current law it has no incentive to do so. D ≈ 0.15 (near-full beneficiary), f(d) ≈ -0.01, chi heavily dampened or negative (extraction runs toward the federal doctrine, not from it). States'-rights litigants derive d from powerless + trapped: they cannot exit the constraint without abandoning constitutional adjudication. D ≈ 0.95, f(d) ≈ 1.42, chi is heavily amplified — maximum experienced extractiveness. The anticommandeering coalition (moderate + constrained) derives d ≈ 0.55-0.65, experiencing moderate extraction with coordinating benefit. The analytical observer uses the canonical analytical d ≈ 0.72-0.73, producing chi consistent with the snare classification at the observer's power level.
 *
 * MANDATROPHY ANALYSIS:
 *   The truism reading resolves mandatrophy by showing that calling the Tenth Amendment a 'truism' is itself a interpretive move, not a discovery of how the Constitution 'really' works. The mandatrophy question — is this a coordination mechanism (Rope) or an extraction mechanism (Snare)? — is answered by reference to the agent's position. Federal power doctrine experiences it as pure coordination (solves the problem of a limiting text). States'-rights litigants experience it as pure extraction (suppresses their constitutional path). The resolution is not that one is wrong, but that the classification is perspectival. The truism reading's beneficiaries see their interpretation as neutral description of structure; the victims see it as ideological suppression. The framework's task is to make the structural divergence visible, not to adjudicate which perspective is 'correct' — both are empirically accurate descriptions of the constraint's effect from their respective positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    truism_vs_substantive_limit_boundary,
    'Is the truism reading''s claim that the Tenth merely restates structural implication empirically true, or does it suppress a genuinely substantive limit?',
    'Historical analysis of ratification debates and founding-era usage; comparison of Tenth Amendment invocations pre- and post-Darby; examination of whether federalism jurisprudence changed in ways the truism reading would predict (i.e., if the truism is correct, federal power should be limited ONLY by enumeration; if the Tenth retained independent force, federal power should show additional limits)',
    'If truism is empirically accurate: the Tenth is correctly classified as redundant, and the snare classification reflects institutional extraction only (suppression of a dead doctrine). If truism is false: the snare classification reflects suppression of a genuinely alternative constitutional doctrine, and extractiveness should increase.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(truism_vs_substantive_limit_boundary, empirical, 'Whether truism claim accurately describes Tenth Amendment function').

omega_variable(
    foundational_premise_of_this_reading,
    'Does the truism reading''s core premise — that the Tenth Amendment is purely redundant — logically foreclose the anticommandeering doctrine, or can anticommandeering exist within the truism framework?',
    'Doctrinal analysis: if anticommandeering is a structural limit independent of the Tenth''s text, it can coexist with the truism. If anticommandeering is grounded in or derived from Tenth Amendment protections, the truism forecloses it. Examine New York v. United States and Printz v. United States to determine whether anticommandeering logic requires independent Tenth Amendment substantive content.',
    'If anticommandeering is structurally independent: truism and anticommandeering coexist, both are live options, the reading_relations should be coexists_with. If anticommandeering requires Tenth substantive content: the truism forecloses anticommandeering, reading_relations should be forecloses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foundational_premise_of_this_reading, conceptual, 'Whether truism reading forecloses or coexists with anticommandeering doctrine').

omega_variable(
    political_safeguards_orthogonality,
    'Is the political safeguards reading (federalism protected by Senate and parties) orthogonal to the truism reading, or does accepting the truism entail accepting political safeguards as the primary federalism mechanism?',
    'Logical analysis: if both readings can be true in the same constitutional framework (Tenth is truism AND political safeguards are sufficient), they coexist. If accepting truism implies political safeguards must be primary, the truism influences the political safeguards reading. If accepting truism forecloses the possibility of judicially enforceable federalism limits, the truism forecloses political safeguards'' denial.',
    'If orthogonal: reading_relations = coexists_with. If influences: reading_relations = influences. If forecloses: reading_relations = forecloses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_safeguards_orthogonality, conceptual, 'Structural relationship between truism reading and political safeguards reading').

omega_variable(
    doctrine_vs_doctrinal_performance,
    'What portion of the measured suppression (0.68) reflects genuine doctrinal suppression versus theatrical maintenance of a dead doctrine (misattributed to suppression when it should be theater)?',
    'Case-law analysis: count instances where courts invoke Tenth Amendment for substantive limiting effect post-Darby versus instances where invocation is purely ceremonial. Measure correlation between Tenth Amendment invocation and actual limits imposed on federal power.',
    'If substantive suppression is high (doctrine is actively kept from limiting federal power): snare classification robust, suppression = 0.68 is accurate. If theater is high (doctrine persists as ritual with no effect): theater_ratio should be higher, suppression might be lower, classification might shift toward piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_vs_doctrinal_performance, empirical, 'Proportion of theater vs substantive suppression in measured metrics').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenth_amendment__truism_reading, 0, 185).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenth_truism_theater_1787, tenth_amendment__truism_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(tenth_truism_theater_1927, tenth_amendment__truism_reading, theater_ratio, 140, 0.62).
narrative_ontology:measurement(tenth_truism_theater_1972, tenth_amendment__truism_reading, theater_ratio, 185, 0.85).

% Extraction over time
narrative_ontology:measurement(tenth_truism_extract_1787, tenth_amendment__truism_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(tenth_truism_extract_1927, tenth_amendment__truism_reading, base_extractiveness, 140, 0.38).
narrative_ontology:measurement(tenth_truism_extract_1972, tenth_amendment__truism_reading, base_extractiveness, 185, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenth_amendment__truism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(tenth_amendment__truism_reading, tenth_amendment__anticommandeering_doctrine).
narrative_ontology:affects_constraint(tenth_amendment__truism_reading, tenth_amendment__political_safeguards_reading).
narrative_ontology:affects_constraint(tenth_amendment__truism_reading, dormant_commerce_clause_restriction).

% DUAL FORMULATION NOTE:
% The truism reading is one constraint within the tenth_amendment kernel family. The anticommandeering doctrine and political safeguards reading are structurally distinct constraints (separate story files) representing alternative readings of the same constitutional text. The truism reading suppresses the doctrinal force of the Tenth Amendment itself, creating structural conditions that influence the anticommandeering and political safeguards constraints. Network edges record this influence: the truism reading affects (shapes the environment for) the other two readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
