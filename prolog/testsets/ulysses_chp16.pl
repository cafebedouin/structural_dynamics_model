% ============================================================================
% CONSTRAINT STORY: ulysses_chp16
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ulysses_chp16, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ulysses_chp16
 *   human_readable: The Exhausted Coordination (Cabman's Shelter)
 *   domain: social/economic/linguistic
 *
 * SUMMARY:
 *   In Chapter 16 of *Ulysses*, Leopold Bloom and Stephen Dedalus seek refuge
 *   in a cabman's shelter near Butt Bridge in Dublin. The shelter is a space
 *   that ostensibly provides coordination — warmth, food, company, rest for
 *   nocturnal wanderers and cabmen. But the coordination function is
 *   intertwined with extraction: inflated prices for stale food, performative
 *   hospitality masking indifference, a ritualized social space that has lost
 *   its original purpose but persists through institutional inertia. The
 *   constraint exhibits the structural signature of Tangled Rope: genuine
 *   coordination need (exhausted men need warmth) solved through an
 *   extractive mechanism (paying premium prices for degraded goods). The
 *   wanderers' language becomes stale and clichéd in the shelter, suggesting
 *   that the constraint's extraction mechanism includes cognitive and
 *   linguistic degradation. The cabmen's guild that once coordinated fare
 *   wars and collective defense has become a performative social club, making
 *   the shelter a Piton from the guild's perspective. The analytical observer
 *   risks naturalizing this as the inevitable exhaustion of human language
 *   itself — a false mountain that obscures the shelter's role in producing
 *   the degradation.
 *
 * KEY AGENTS:
 *   - Leopold Bloom and Stephen Dedalus: Primary victims (powerless/trapped) — exhausted wanderers with no viable alternative refuge; bear full extraction through overpriced, degraded goods and stale social environment
 *   - The Shelter Proprietor: Primary beneficiary (institutional/arbitrage) — extracts rent from both cabmen (regular customers) and wanderers (desperate customers); net extractor through positioning between two clienteles
 *   - The Cabmen's Collective: Secondary beneficiary/piton (institutional/arbitrage) — once coordinated collective action; now perform guild identity through shelter gathering with minimal functional leverage
 *   - Dublin's Nocturnal Poor: Structural victim (powerless/trapped) — broader class of wanderers dependent on shelter's overpriced refuge; no organized exit options
 *   - Linguistic and Cognitive Authenticity: Abstract victim (powerless/trapped) — the shelter's degraded language, tired metaphors, and stale conversation represent extraction of conceptual freshness from users
 *   - Social Reform Observer: Organized external agent (organized/constrained) — sees the shelter's extraction as temporary, resolvable through structural economic change (better housing, public facilities, electric transport)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ulysses_chp16, 0.55).
domain_priors:suppression_score(ulysses_chp16, 0.48).
domain_priors:theater_ratio(ulysses_chp16, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ulysses_chp16, extractiveness, 0.55).
narrative_ontology:constraint_metric(ulysses_chp16, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(ulysses_chp16, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ulysses_chp16, tangled_rope).
narrative_ontology:human_readable(ulysses_chp16, "The Exhausted Coordination (Cabman's Shelter)").
narrative_ontology:topic_domain(ulysses_chp16, "social/economic/linguistic").

domain_priors:requires_active_enforcement(ulysses_chp16).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ulysses_chp16, shelter_proprietor).
narrative_ontology:constraint_beneficiary(ulysses_chp16, cabmen_collective).
narrative_ontology:constraint_victim(ulysses_chp16, transient_seekers).
narrative_ontology:constraint_victim(ulysses_chp16, linguistic_authenticity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE EXHAUSTED WANDERER (SNARE) — Bloom and Stephen are trapped by fatigue, cold, and lack of alternatives. The shelter offers minimum refuge but extracts through overpriced food, stale air, and performative hospitality. Cannot exit without greater hardship. Maximum experienced extraction — the constraint binds tightest on those with nowhere else to go.
constraint_indexing:constraint_classification(ulysses_chp16, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE OCCASIONAL CUSTOMER (TANGLED ROPE) — Customers benefit from the shelter's coordination function (warmth, food, company of fellow travelers) but bear extraction through inflated prices and degraded quality. They have some exit options (other establishments, home) but these are constrained by Dublin's nocturnal geography and their social position. Mixed experience: genuine coordination need met through extractive mechanism.
constraint_indexing:constraint_classification(ulysses_chp16, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: THE SHELTER PROPRIETOR (ROPE) — Benefits from coordination: provides meeting place for cabmen and travelers, extracting rent from both groups. Experiences the constraint as effective coordination mechanism — solves the problem of where nocturnal Dublin gathers. Net beneficiary through arbitrage positioning between two clienteles.
constraint_indexing:constraint_classification(ulysses_chp16, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: THE DUBLIN CABMEN'S GUILD (PITON) — The shelter once served genuine coordination function for cabmen: a headquarters for work sharing, fare negotiation, collective defense against railway competition. But by 1904, the function is largely performative — the guild persists through institutional inertia and social tradition rather than economic necessity. Theater ratio high: the shelter's ritual gatherings maintain guild identity but lack functional leverage. Extraction persists despite degraded purpose.
constraint_indexing:constraint_classification(ulysses_chp16, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 5: THE SOCIAL REFORM OBSERVER (SCAFFOLD) — From the vantage of early 20th-century social reform movements (temperance, workers' rights, housing reform), the shelter's extraction is seen as a temporary market failure with a sunset clause: as urban infrastructure improves (electric trams, gas lighting, cheaper lodging houses), the shelter's monopolistic pricing loses force. The constraint appears as a transitional problem being solved by structural economic change. Suppression is high but declining as alternatives emerge.
constraint_indexing:constraint_classification(ulysses_chp16, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: THE ANALYTICAL OBSERVER (FALSE MOUNTAIN) — From the civilizational view of linguistic analysis, the shelter's degraded speech — its 'worn language, stale metaphors, performed authenticity' — might appear as an immutable property of human exhaustion itself: when people are tired and cold, their language atrophies toward cliché and theater. But the structural data reveals this as naturalization of a contingent institutional constraint: the constraint's extraction mechanism includes linguistic degradation as a side effect, and the 'universal exhaustion' framing obscures the shelter's role in producing that exhaustion.
constraint_indexing:constraint_classification(ulysses_chp16, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_chp16_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ulysses_chp16, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_chp16, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ulysses_chp16, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ulysses_chp16, TR),
    TR >= 0.70.

:- end_tests(ulysses_chp16_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate-high. The shelter extracts through pricing power (inflated cost for degraded goods), scarcity (limited alternatives for nocturnal refuge in Dublin 1904), and cognitive extraction (the space produces linguistic and intellectual degradation in its users). But the extraction is not maximal — the shelter does provide genuine coordination: warmth, food, and social space that wanderers need. The proprietor's arbitrage positioning between cabmen and transients creates mixed incentives: the proprietor benefits from both groups, but the coordination serves both groups' needs. The measured value reflects that genuine coordination is bundled with extraction, not pure extraction. Suppression (0.48): Moderate. Suppression mechanisms include: Dublin's geography and class-based access barriers (where else could Bloom and Stephen go at 4 AM?), the shelter's social monopoly on nocturnal gathering, and the cognitive degradation produced by the environment itself. But suppression is not total — the constraint is local (only applies to those seeking nocturnal refuge in that area) and not backed by explicit coercion, only by circumstance and social convention. Theater ratio (0.68): High. The shelter's social rituals (the gathering of cabmen, the proprietor's hospitality performance, the stale conversation) are substantially performative. The cabmen's guild once had genuine economic function; by 1904 its meetings are identity ritual. The proprietor performs hospitality while providing degraded service. Even the wanderers perform wakefulness and engagement in an exhausted state. Theater has increased over the constraint's interval as the guild's economic function declined.
 *
 * PERSPECTIVAL GAP:
 *   The widest gap is between the proprietor/beneficiary (who sees Rope — effective coordination and arbitrage profit) and the wanderer/victim (who sees Snare — desperate refuge with no alternatives). The cabmen's guild occupies a middle position: they benefit from the shelter as a gathering place (Rope from their perspective) but experience it as increasingly performative (Piton from the guild's collective perspective). The social reform observer sees a Scaffold — the constraint is temporary, resolvable through structural urban change. The analytical observer risks a false mountain, naturalizing the exhaustion as inherent to human condition rather than produced by institutional constraint. The perspectival gap reveals that the same physical space operates with completely different structural properties depending on the observer's position and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is determined by beneficiary/victim status and exit options. Bloom and Stephen: beneficiaries of warmth/food (low d baseline) but victims of extraction and linguistic degradation (high d from victim status) and trapped exit (highest d from exit constraint) → net high d → high f(d) → high experienced extractiveness chi. The proprietor: beneficiary through arbitrage (low d) and mobile exit (lowest d) → net low d → negative/zero chi. The cabmen's collective: beneficiary from gathering space (low d) but constrained exit (they keep coming despite degradation) → moderate d → moderate chi. The shelter space itself enforces directionality through physical dependency: if you need warmth and are trapped by geography/class, you accept extraction. The linguistic degradation visible in the text signals high d: the wanderers' language becomes defensive, clichéd, performative — exactly the speech pattern of high-d agents in snare constraints.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by distinguishing genuine coordination function from extractive mechanism. The error would be to classify as pure Snare (ignoring that wanderers do benefit from warmth and food) or as pure Rope (ignoring that prices are inflated and alternatives are blocked). Tangled Rope is correct: the constraint simultaneously solves a coordination problem (where nocturnal Dublin gathers) and extracts from vulnerable populations (those with no alternative). The theater ratio (0.68) reveals the secondary mandatrophy: the cabmen's guild performance has become decoupled from economic function. The guild gathers in the shelter, but this gathering no longer coordinates fare policy or collective action — it is pure identity maintenance. This degradation from Rope (the guild once coordinated) to Piton (the guild now performs) is tracked through the measurements: as the guild's functional leverage declined, theater ratio rose and the proprietor's extraction increased. The mandatrophy is resolved by recognizing that Tangled Rope and Piton are both accurate perspectives, operating on different aspects of the same constraint system: Tangled Rope captures the wanderer-proprietor dynamic; Piton captures the guild's institutional degradation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    shelter_alternative_viability,
    'Would alternative overnight refuges (workhouses, cheaper lodging, private homes) have been structurally accessible to Bloom and Stephen at that moment in Dublin?',
    'Historical analysis of Dublin''s 1904 nocturnal infrastructure, class-based access barriers, and documented paths of comparable wanderers',
    'If alternatives existed: exit is less trapped than stated, classification shifts toward Tangled Rope from powerless perspective. If alternatives were effectively blocked: trapped exit is correct, Snare classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(shelter_alternative_viability, empirical, 'Accessibility of alternative nocturnal refuge in Dublin 1904').

omega_variable(
    coordination_function_degradation_timeline,
    'When did the cabmen''s shelter transition from genuine guild coordination mechanism to performative social space? Was this transition complete by 1904?',
    'Historical records of cabmen''s guild meeting minutes, fare-setting evidence, collective action against competition, correlation with decline of horse-cab traffic after 1900',
    'If transition complete: Piton classification with high theater is accurate. If incomplete: the shelter retains genuine coordination function mixed with extraction (Tangled Rope from cabmen perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_degradation_timeline, empirical, 'Timeline of cabmen''s guild coordination function degradation').

omega_variable(
    linguistic_exhaustion_causality,
    'Is the shelter''s degraded language (in the text) a symptom of the wanderers'' exhaustion or a product of the shelter''s extractive mechanism itself?',
    'Textual comparison of Bloom''s language before/after shelter entry; analysis of speech patterns correlated with other degradation signals (food quality, atmosphere); linguistic markers of institutional constraint vs personal fatigue',
    'If primarily exhaustion symptom: the false mountain perspective has some validity — the constraint reveals underlying human limits. If primarily institutional product: the constraint actively degrades its victims'' capacity, and the false summit detection is confirmed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(linguistic_exhaustion_causality, conceptual, 'Causality of linguistic degradation in shelter context').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_chp16, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eumaeus_tr_t0, ulysses_chp16, theater_ratio, 0, 0.35).
narrative_ontology:measurement(eumaeus_tr_t2, ulysses_chp16, theater_ratio, 2, 0.52).
narrative_ontology:measurement(eumaeus_tr_t4, ulysses_chp16, theater_ratio, 4, 0.68).

% Extraction over time
narrative_ontology:measurement(eumaeus_be_t0, ulysses_chp16, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(eumaeus_be_t2, ulysses_chp16, base_extractiveness, 2, 0.42).
narrative_ontology:measurement(eumaeus_be_t4, ulysses_chp16, base_extractiveness, 4, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ulysses_chp16, resource_allocation).
narrative_ontology:affects_constraint(ulysses_chp16, dublin_nocturnal_infrastructure).
narrative_ontology:affects_constraint(ulysses_chp16, cabmen_guild_economic_decline).

% DUAL FORMULATION NOTE:
% The exhausted coordination operates on two structurally distinct levels: (1) immediate coordination between wanderers and proprietor (Tangled Rope, ε=0.55), and (2) long-term degradation of the cabmen's guild's functional purpose (Piton, ε=0.35). These are linked through the shelter space but could be decomposed into separate constraints. The first constraint has higher extractiveness because it directly affects immediate welfare; the second has higher theater because it is institutional performance without function.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
