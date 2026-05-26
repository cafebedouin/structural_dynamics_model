% ============================================================================
% CONSTRAINT STORY: republican_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_republican_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: republican_reading
 *   human_readable: Republican Authority via Delegated Popular Consent (Revocable)
 *   domain: political_philosophy/constitutional_theory
 *
 * SUMMARY:
 *   The republican reading constitutes authority as revocable delegation from
 *   the people. Authority is legitimate because the people retain the power
 *   to remove leaders through elections; leaders are accountable because
 *   their continued tenure depends on periodic renewal of popular consent.
 *   This framework produces a distinctive structural pattern: it coordinates
 *   succession among the included (electorate) while simultaneously requiring
 *   active enforcement to maintain boundaries around who counts as 'the
 *   people.' The constraint exhibits all six classification types depending
 *   on observer position. For participating citizens, it functions as pure
 *   coordination (rope). For permanently excluded populations, it becomes
 *   extraction (snare). For those seeking to expand suffrage, it appears as a
 *   temporary problem with a built-in exit path (scaffold). For the state
 *   apparatus, the revocability mechanism increasingly functions as theater
 *   (piton) as bureaucratic inertia accumulates. The constraint's core
 *   tension: revocability requires enforcement (active suppression of
 *   alternatives), yet the legitimacy claim rests on the fiction that
 *   revocation is freely available. The measurement trajectory shows
 *   extractiveness declining and theater increasing over time, reflecting the
 *   historical expansion of suffrage (reducing extraction on new
 *   constituencies) simultaneous with institutional capture (rising
 *   performativity of elections). The temporal pattern supports the scaffold
 *   perspective — suffrage movements are successfully expanding the
 *   electorate, reducing structural extractiveness — but also diagnoses piton
 *   dynamics as bureaucratic resistance to substantive power transfer
 *   accumulates.
 *
 * KEY AGENTS:
 *   - Participating Electorate: Organized population with voting rights (organized/mobile) — experiences the constraint as coordination; benefits from orderly succession mechanisms and accountability pathways
 *   - Structural Non-Participants: Populations excluded from voting (moderate/constrained) — subject to decisions they cannot influence; experience mixed coordination-extraction as the system builds order among participants while enforcing their subordination
 *   - Permanently Excluded Class: Historically enslaved, caste-subordinated, or stateless populations (powerless/trapped) — no revocability mechanism available; experience constraint as pure extraction legitimized as natural order
 *   - Suffrage Expansion Movements: Organized coalitions seeking electoral inclusion (organized/constrained) — see the republican framework as containing its own logic of inclusion; work to resolve the tension through universal suffrage
 *   - State Bureaucratic Apparatus: Institutional actors managing formal elections and policy implementation (institutional/arbitrage) — maintain the revocability theater while structural incentives preserve policy continuity across electoral transitions
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the republican reading as logical necessity, missing its historical contingency and constructed character
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(republican_reading, 0.42).
domain_priors:suppression_score(republican_reading, 0.35).
domain_priors:theater_ratio(republican_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(republican_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(republican_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(republican_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(republican_reading, tangled_rope).
narrative_ontology:human_readable(republican_reading, "Republican Authority via Delegated Popular Consent (Revocable)").
narrative_ontology:topic_domain(republican_reading, "political_philosophy/constitutional_theory").

domain_priors:requires_active_enforcement(republican_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(republican_reading, formalized).
narrative_ontology:cs_authority_grounding(republican_reading, lineage).
narrative_ontology:cs_interpretation_layer_present(republican_reading).
narrative_ontology:cs_kernel_id(republican_reading, sovereign_legitimacy).
narrative_ontology:cs_reading_relation(republican_reading, monarchical_reading, forecloses).
narrative_ontology:cs_reading_relation(republican_reading, constitutional_hybrid_reading, influences).
narrative_ontology:cs_axiom(republican_reading, foundational, popular_consent_as_legitimacy_source).
narrative_ontology:cs_axiom_status(popular_consent_as_legitimacy_source, holdable).
narrative_ontology:cs_axiom_grounding(republican_reading, popular_consent_as_legitimacy_source, deontological).
narrative_ontology:cs_axiom(republican_reading, foundational, electoral_revocability_constitutes_accountability).
narrative_ontology:cs_axiom_status(electoral_revocability_constitutes_accountability, holdable).
narrative_ontology:cs_axiom_grounding(republican_reading, electoral_revocability_constitutes_accountability, empirically_contingent).
narrative_ontology:cs_reference_frame(republican_reading, popular_sovereign_authority).
narrative_ontology:cs_drift_state(republican_reading, contemporary_democratic_institutions, gap(practice_drift, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(republican_reading, participating_citizens).
narrative_ontology:constraint_beneficiary(republican_reading, elected_representatives).
narrative_ontology:constraint_victim(republican_reading, excluded_non_citizens).
narrative_ontology:constraint_victim(republican_reading, disenfranchised_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PARTICIPATING ELECTORATE (ROPE) — Agents with voting rights experience the constraint as genuine coordination: periodic elections coordinate succession, accountability mechanisms channel dissent, and revocability ensures leader responsiveness. Benefits exceed costs within the framework of participation. No systematic extraction — the constraint solves the collective action problem of orderly power transfer.
constraint_indexing:constraint_classification(republican_reading, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: STRUCTURAL NON-PARTICIPANT (TANGLED ROPE) — Agents excluded from the electorate (historically: enslaved persons, women, propertyless men; contemporaneously: non-citizens, felons, children) experience the constraint as mixed coordination-extraction. The system coordinates succession among participants while simultaneously extracting costs (legal subordination, labor obligation, lack of voice) from non-participants. Active enforcement required to maintain the boundary. Significant perspectival gap: the same revocability mechanism protects participants while binding non-participants to decisions they cannot influence.
constraint_indexing:constraint_classification(republican_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PERMANENTLY EXCLUDED CLASS (SNARE) — When exclusion is structural and treated as permanent (slavery, caste, statelessness), the constraint becomes pure extraction. No revocability mechanism available to the excluded. No coordination function benefits them. Maximum experienced extraction — the electoral mechanism itself legitimizes their subjugation as the 'natural' order of a system designed by and for the included.
constraint_indexing:constraint_classification(republican_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 4: INCUMBENT AUTHORITY STRUCTURE (PITON) — From the perspective of the state apparatus itself, the republican constraint performs its official function (periodic succession, electoral accountability) with declining real force as bureaucratic inertia accumulates. Elections become theaters where substantive policy continues unchanged across electoral cycles (regulatory capture, civil service permanence, structural inequality reproduction). Theater ratio high because the ritual of electoral choice persists while actual power transfer remains limited. The revocability mechanism is maintained through institutional habit rather than active constraint on behavior.
constraint_indexing:constraint_classification(republican_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: SUFFRAGE EXPANSION MOVEMENT (SCAFFOLD) — Organized actors working to expand the electorate (abolitionists, women's suffrage movements, voting rights coalitions) see the republican constraint as a temporary coordination failure with a built-in exit path: the revocability principle logically extends to who has revocation rights. Each successful expansion (1870s, 1920s, 1960s) resolves the tension. Theater declines as the system approaches its own normative endpoint: universal suffrage. Suppression declines as enforcement-dependent exclusions become indefensible within the system's own legitimating commitments.
constraint_indexing:constraint_classification(republican_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a civilizational/universal perspective, the revocability principle could appear as an immutable logical necessity: any legitimate authority structure requires removal mechanisms to prevent tyranny, and delegated popular consent is the only rationally defensible source of legitimacy. The constraint appears ε-minimal because it reflects an irreducible feature of rational political organization. However, this perspective naturalizes what is historically contingent: the republican reading itself emerged in the 17th-18th centuries and competes with alternative legitimacy framings (monarchical divine right, theocratic covenant, customary hereditary succession). The engine's false summit detector will classify this as naturalization.
constraint_indexing:constraint_classification(republican_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(republican_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(republican_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(republican_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(republican_reading, TR),
    TR >= 0.70.

:- end_tests(republican_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42): Moderate, declining over the interval. The constraint extracts from excluded populations (labor obligation, legal subordination, exclusion from benefit streams) but the extraction is resolvable through suffrage expansion. Initial value (0.58) reflects the constraint's origins in 17th-century contexts with severe restrictions on the franchise (property ownership, gender, race); contemporary value (0.42) reflects expanded but still incomplete suffrage. The declining trajectory reflects successful scaffold expansion — each suffrage extension reduces structural extractiveness by expanding the electorate that enjoys coordination benefits. Suppression (0.35): Moderate. Enforcement required to maintain electoral boundaries (voter registration, citizenship requirements, age thresholds) and to suppress alternatives to the republican framework (monarchy, theocracy, autocracy). Suppression is lower than snare-level because most participants accept the legitimacy of electoral mechanisms; enforcement costs are primarily administrative rather than coercive. Theater ratio (0.58): Moderate-high, increasing over interval. Electoral mechanisms increasingly perform legitimacy theater as bureaucratic and regulatory capture insulate policy from electoral signals. Citizens continue voting (ritual persists) but substantive power transfer declines. The theater increase reflects piton dynamics — institutional inertia accumulating as the constraint's original coordination function degrades.
 *
 * PERSPECTIVAL GAP:
 *   The republican reading generates sharp perspectival disagreement across all six classification types. The participating electorate experiences rope — genuine coordination, low extraction, legitimate authority. The excluded experience tangled_rope or snare depending on permanence of exclusion — legitimate authority for some, pure extraction for others. The state apparatus experiences piton — the revocability mechanism persists as theater while actual power is insulated from electoral pressure. The suffrage movements experience scaffold — a temporary tension with a built-in resolution path. The analytical observer risks mountain — seeing the revocability principle as logical necessity — but the structural data reveals false summit: the principle is historically contingent, and the constraint's extractiveness and suppression are measurable human constructions, not laws of nature. The perspectival gap is the diagnostic signal: when the same constraint classifies as rope for beneficiaries and snare for victims, when it appears as mountain to the analyst and piton to the state, the framework reveals that 'republican authority' is not a single constraint but a presheaf of different structures indexed by observational position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies sharply across perspectives. Participating citizens (beneficiaries, mobile exit) experience low d (≈0.30) — the constraint benefits them; effective extraction χ is low or negative. Excluded non-citizens (victims, constrained exit) experience high d (≈0.75) — the constraint extracts from them; effective extraction χ is substantial. The permanently excluded (victims, trapped exit) experience maximum d (≈0.95) — they are the extraction target; maximum experienced extraction. The state bureaucracy (beneficiary, arbitrage) experiences negative d (≈0.10) — extraction flows toward them through the power asymmetries that elections nominally constrain but functionally preserve. The suffrage expansion movement (organized, constrained, both beneficiary and victim) experiences moderate d (≈0.50) — asymmetric in its current state but positioned at the equilibrium point between inclusion and exclusion. The analytical observer (analytical context, d≈0.70) sees the full structure: the constraint is framed as natural law but contains measurable extractive asymmetries that would change if the frame shifted.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that all six types are legitimate perspectival readings of the same structural pattern. The republican reading is NOT univocally a rope, snare, tangled_rope, scaffold, piton, or mountain. It IS all of them simultaneously, differentiated by observer position, power level, temporal horizon, and exit options. The question 'which type is correct?' dissolves when the framework is applied properly — the presheaf over the index domain (P, T, E, S) IS the answer. The false summit (analytical mountain perspective) is diagnostic: the analyst who treats republican authority as a logical necessity of rational governance is naturalizing a historical artifact. The snare (permanently excluded) is equally diagnostic: those locked out of the framework experience pure extraction. The rope (participating citizens) is the framework's success case. The piton (state apparatus) reveals institutional degradation. The scaffold (suffrage movements) shows the immanent logic of the reading itself — revocability entails eventual universal suffrage. The mandatrophy is resolved not by choosing one type but by mapping the structure across all perspectives and recognizing that the constraint's legitimacy claim rests on the asymmetry between rope (beneficiaries) and snare (excluded) — the system is legitimate for the included, extractive for the excluded, and maintaining this boundary requires continuous suppression that the revocability fiction conceals.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revocability_mechanism_effectiveness,
    'Does formal revocability (electoral removal of leaders) constitute genuine popular sovereignty, or is it a theater obscuring structural continuity of power concentration?',
    'Comparative historical analysis: do electoral turnovers produce substantive policy changes, or do bureaucratic inertia and regulatory capture preserve continuity across electoral cycles? Measure policy drift at transitions vs within-cycle drift.',
    'If effective: republican reading is genuine coordination mechanism (rope from more perspectives). If theater: more perspectives shift toward piton classification, and extractiveness should be revised upward to reflect false choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revocability_mechanism_effectiveness, empirical, 'Whether electoral revocability produces substantive power transfer').

omega_variable(
    consent_boundary_contestation,
    'Does the republican reading''s commitment to popular consent logically entail universal suffrage, or can the ''people'' legitimately be a defined subset?',
    'Internal coherence analysis within republican political theory; historical trajectory of suffrage expansion; identification of principled vs arbitrary boundaries (property ownership, literacy, citizenship status, age).',
    'If universal suffrage is logically entailed: scaffold perspective is correct and exclusions are unstable within the system''s own commitments; current extractiveness reflects temporary phase. If subset is defensible: exclusions can persist indefinitely; snare classification applies to permanently excluded populations; extractiveness is stable, not declining.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consent_boundary_contestation, conceptual, 'Logical scope of ''the people'' in republican consent framework').

omega_variable(
    institutional_capture_reversibility,
    'When regulatory and bureaucratic capture prevent electoral signals from translating into policy change (piton perspective), is the mechanism restorable through internal reform, or does the reading require external reconstitution?',
    'Case study analysis of reform movements (anti-corruption, transparency, campaign finance) that explicitly invoke republican principles; measure success rates in restoring electoral responsiveness; identify whether successful reforms operated within or outside the republican framework.',
    'If internal restoration possible: piton is degraded temporary state (close to scaffold). If external reconstitution required: the piton is stable and extractiveness should remain high — the republican reading naturalizes institutional capture through the fiction of revocability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_reversibility, empirical, 'Whether institutional capture within republican systems is reversible').

omega_variable(
    sibling_reading_committer_structure,
    'This constraint is ONE reading of the contested kernel ''sovereign_legitimacy'' — are the monarchical and constitutional_hybrid readings genuinely alternative frameworks, or are they epistemic cousins of this republican reading that share core commitments?',
    'Comparative axiom analysis across the three readings: identify foundational claims that each reading would need to abandon to adopt another''s premises. Map the logical structure of forecloses vs coexists_with vs influences relationships.',
    'If readings genuinely foreclose: the kernel is a deep philosophical schism with no unified framework. If coexist: republican reading competes but does not eliminate alternatives; different contexts/parties maintain simultaneous commitment to different readings. If influences: there is an upstream-downstream causal structure where one reading''s success conditions are presuppositions of another''s.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_committer_structure, conceptual, 'Logical structure of relationships between republican, monarchical, and constitutional hybrid readings').

omega_variable(
    natural_law_reading_contingency,
    'Is the mountain perspective''s characterization of republican authority as a logical necessity (immutable principle of rational governance) defensible, or is it a false summit that naturalizes a historically contingent 17th-18th century European philosophical innovation?',
    'Genealogical analysis: when and why did the republican reading emerge; what alternative legitimacy framings existed before and after; are there contemporary non-republican legitimacy systems that function without revocability-based authority?',
    'If necessity claim holds: mountain classification is valid; the constraint is ε-minimal. If contingency confirmed: mountain is a false summit; the reading is a constructed constraint with moderate extractiveness reflecting institutional power asymmetries and exclusions; the constraint should be reclassified toward tangled_rope or snare depending on perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_reading_contingency, conceptual, 'Whether republican delegated consent is logically necessary or historically contingent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(republican_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(repub_tr_t0, republican_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(repub_tr_t2, republican_reading, theater_ratio, 2, 0.52).
narrative_ontology:measurement(repub_tr_t4, republican_reading, theater_ratio, 4, 0.55).
narrative_ontology:measurement(repub_tr_t6, republican_reading, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(repub_be_t0, republican_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(repub_be_t2, republican_reading, base_extractiveness, 2, 0.52).
narrative_ontology:measurement(repub_be_t4, republican_reading, base_extractiveness, 4, 0.45).
narrative_ontology:measurement(repub_be_t6, republican_reading, base_extractiveness, 6, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(republican_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(republican_reading, monarchical_reading).
narrative_ontology:affects_constraint(republican_reading, constitutional_hybrid_reading).
narrative_ontology:affects_constraint(republican_reading, suffrage_expansion_conflict).

% DUAL FORMULATION NOTE:
% The republican reading is decomposed from the contested kernel 'sovereign_legitimacy'. Sibling readings (monarchical_reading, constitutional_hybrid_reading) are separate constraint files with different ε values, beneficiary/victim structures, and cs_structure axioms. This reading's network edges show influence: the republican reading affects both sibling readings by establishing an alternative legitimacy grounding; the suffrage_expansion_conflict is downstream of this reading (the tension between claimed popular consent and actual exclusion).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(republican_reading, powerless, 0.95).
constraint_indexing:directionality_override(republican_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
