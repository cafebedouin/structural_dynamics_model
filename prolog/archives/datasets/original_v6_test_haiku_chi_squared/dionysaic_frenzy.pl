% ============================================================================
% CONSTRAINT STORY: dionysaic_frenzy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dionysaic_frenzy, []).

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
 *   constraint_id: dionysaic_frenzy
 *   human_readable: Dionysiac Religious Possession
 *   domain: religious/social
 *
 * SUMMARY:
 *   Dionysiac religious possession represents a structural constraint on
 *   female autonomy masked by the rhetoric of divine ecstasy. Women
 *   designated as maenads or bacchae were expected to abandon household
 *   duties, city boundaries, and legal protections to participate in mountain
 *   rites organized by a Dionysian priesthood and observed by aristocratic
 *   male audiences. The constraint combines genuine religious coordination
 *   (ecstatic communal experience, alternative consciousness) with extraction
 *   of female labor and vulnerability. The base extractiveness (0.52)
 *   reflects moderate net extraction — women gain religious legitimacy and
 *   temporary social inversion but lose household status, face sexual
 *   vulnerability, and cannot refuse participation without social penalty.
 *   Suppression is high (0.68) because exit is socially sanctioned at male
 *   discretion: the priesthood and authorities determine when rites occur and
 *   how long participants remain obligated. The theater ratio (0.58)
 *   indicates that while the initial ecstatic experience may be genuine, the
 *   institutional organization increasingly formalizes and ritualizes the
 *   phenomenon, converting lived transcendence into prescribed performance
 *   over generational timescales. This constraint exemplifies how indexical
 *   classification reveals the perspectival structure of an apparently
 *   natural phenomenon: what appears to male spectators as pure religious
 *   coordination (Rope) appears to the possessed women as extraction with
 *   constrained exit (Tangled Rope), and to the truly powerless as a snare
 *   with no legitimate escape. The civilization-spanning analytical
 *   perspective risks naturalizing the constraint as an inevitable feature of
 *   human religious consciousness — a false summit that obscures the
 *   contingent institutional arrangements maintaining it.
 *
 * KEY AGENTS:
 *   - Maenad / Possessed Woman: Primary victim (powerless/trapped) — compelled to abandon household and social role; faces sexual vulnerability and social ostracism
 *   - Dionysian Priesthood: Primary beneficiary (institutional/arbitrage) — commands religious authority, legitimacy, and organizational power through ritual control
 *   - Aristocratic Male Audience: Secondary beneficiary (powerful/arbitrage) — spectates ritual transgression that reaffirms their authority; witnesses female autonomy only at their sanction
 *   - City-State Authorities: Regulating institutional actor (organized/constrained) — balance coordination function (channeling religious energy) against extraction risk (female loss of household labor); actively enforce temporal and spatial boundaries
 *   - Peripheral Participants: Secondary victims (moderate/constrained) — experience both genuine religious coordination and extraction; constrained exit due to social expectations
 *   - Ancient Ritual Form: Institutional process (institutional/arbitrage) — degrades from genuine ecstatic practice into formalized theater over generational timescales
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dionysaic_frenzy, 0.52).
domain_priors:suppression_score(dionysaic_frenzy, 0.68).
domain_priors:theater_ratio(dionysaic_frenzy, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dionysaic_frenzy, extractiveness, 0.52).
narrative_ontology:constraint_metric(dionysaic_frenzy, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(dionysaic_frenzy, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dionysaic_frenzy, tangled_rope).
narrative_ontology:human_readable(dionysaic_frenzy, "Dionysiac Religious Possession").
narrative_ontology:topic_domain(dionysaic_frenzy, "religious/social").

domain_priors:requires_active_enforcement(dionysaic_frenzy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dionysaic_frenzy, dionysian_priesthood).
narrative_ontology:constraint_beneficiary(dionysaic_frenzy, aristocratic_male_audiences).
narrative_ontology:constraint_victim(dionysaic_frenzy, possessed_women).
narrative_ontology:constraint_victim(dionysaic_frenzy, social_hierarchy_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MAENAD / POSSESSED WOMAN (SNARE) — Compelled by religious mandate to abandon household, social role, and legal standing. No socially sanctioned exit. Bears full cost of ritual possession: loss of status, vulnerability to violence, social ostracism upon return. d≈0.92, f(d)≈1.38, σ=0.9 → χ≈0.65.
constraint_indexing:constraint_classification(dionysaic_frenzy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: PERIPHERAL PARTICIPANT (TANGLED ROPE) — Participates in mountain rites with both genuine religious experience and constrained agency. Experiences coordination function (communal ritual, collective ecstasy) but also extraction: mandatory participation, loss of household time, sexual vulnerability. Constrained exit due to social expectation and religious authority. d≈0.68, f(d)≈0.98, σ=0.9 → χ≈0.46.
constraint_indexing:constraint_classification(dionysaic_frenzy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: DIONYSIAN PRIESTHOOD (ROPE) — Organizes mountain rites as religious coordination mechanism. Benefits from ritual authority, social legitimacy, and regular gatherings that reinforce institutional power. Experiences the constraint as pure coordination: managing the ecstatic collective, channeling religious energy, maintaining ritual structure. d≈0.08, f(d)≈-0.08, σ=0.9 → χ≈-0.04. Net beneficiary through institutional legitimacy.
constraint_indexing:constraint_classification(dionysaic_frenzy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: ARISTOCRATIC MALE AUDIENCE (ROPE) — Spectates mountain rites as religious entertainment and demonstration of power. Watches women abandon social constraints at male authority's sanction. Experiences constraint as pure coordination: the ritual provides legitimate channel for social transgression (women outside household control) that reinforces male authority rather than threatening it. d≈0.10, f(d)≈-0.06, σ=0.9 → χ≈-0.03. Net beneficiary through spectacle and reaffirmed control.
constraint_indexing:constraint_classification(dionysaic_frenzy, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: CITY-STATE AUTHORITIES (TANGLED ROPE) — Recognize both coordination function (channeling disruptive religious energy, integrating women into civic ritual) and extraction risk (loss of household labor, potential for female insurgency, temporary inversion of social hierarchy). Active enforcement required: regulate when rites occur, control spatial boundaries, limit participation duration. Sees tension between legitimizing Dionysian cult (social stability) and constraining female autonomy (power consolidation). d≈0.45, f(d)≈0.48, σ=0.9 → χ≈0.22.
constraint_indexing:constraint_classification(dionysaic_frenzy, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANCIENT RITUAL FORM (PITON) — Over centuries, Dionysian possession becomes increasingly theatrical and formalized. The initial ecstatic frenzy (where women genuinely experienced alternative consciousness) degrades into ritualized performance: prescribed movements, costume, schedule. Theater ratio rises as the original religious experience becomes institutional theater. Maintained through cultural inertia despite loss of spontaneity. theater_ratio=0.58 is borderline piton (gate at 0.70); trajectory analysis suggests degradation toward full piton by later Hellenistic period. d≈0.08, f(d)≈-0.08, σ=0.9 → χ≈-0.04.
constraint_indexing:constraint_classification(dionysaic_frenzy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURALIZED VIEW (MOUNTAIN) — From civilization-spanning perspective, Dionysian possession appears as an immutable property of human consciousness: altered states, ecstatic experience, and periodic transgression of social norms are universal features of religious life. The constraint would seem to be 'inherent to religion itself.' However, the base properties (ε=0.52, suppression=0.68, active enforcement required) contradict true mountain classification. This perspective risks naturalizing a contingent institutional arrangement (male-controlled ritualization of female transgression) as an inevitable feature of human nature.
constraint_indexing:constraint_classification(dionysaic_frenzy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dionysaic_frenzy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dionysaic_frenzy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dionysaic_frenzy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(dionysaic_frenzy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(dionysaic_frenzy, TR),
    TR >= 0.70.

:- end_tests(dionysaic_frenzy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, reflecting a mix of genuine coordination benefit (ecstatic experience, religious legitimacy) and extraction (loss of household status, labor, legal protection, vulnerability to harm). The constraint extracts more than pure coordination would require, but less than pure predation. Initial extractiveness was lower (~0.28); it increases over the interval as the ritual becomes more institutionalized and formalized. Suppression (0.68): High. The mandatory nature of participation for designated women, the social ostracism for refusal, the legal vulnerability during the rite, and the inability to unilaterally exit once the priesthood declares a possession session all constitute significant suppression. Theater ratio (0.58): Moderate, trending toward high. The initial Dionysian frenzy may have involved genuine ecstatic experience and spontaneous behavior. Over time, as the ritual becomes institutionalized, prescribed movements and ceremonial formalization increase. By the later classical period, the theater ratio approaches 0.70 (piton threshold). The upward trajectory suggests degradation from spontaneous religious experience toward performative ritual maintained through institutional inertia. Active enforcement: True. The priesthood and city-state authorities actively enforce participation, temporal boundaries, and spatial restrictions. Without institutional enforcement, the constraint would collapse — women would simply not participate.
 *
 * PERSPECTIVAL GAP:
 *   This constraint reveals a profound perspectival gap between beneficiaries and victims. The Dionysian priesthood perceives pure coordination (Rope) — they are solving the legitimate problem of organizing ecstatic collective religious experience and channeling disruptive religious energy in a way that maintains social order. Aristocratic male observers perceive the same constraint as coordination plus spectacle (Rope) — they see the rite as a perfectly controlled transgression where women's autonomy is demonstrated only at male discretion, reinforcing patriarchal authority. But the possessed women perceive extraction with constrained exit (Tangled Rope or Snare) — they experience both the genuine religious coordination and the loss of household status, the vulnerability, the legal exposure, and the inability to refuse without severe social penalty. The city-state authorities perceive the mixed character most clearly (Tangled Rope) — they recognize both the stabilizing coordination function and the extraction risk. The peripheral women (non-elite participants) perceive Tangled Rope: genuine communal religious experience mixed with extraction and constrained exit. From a sufficiently distant analytical perspective, the constraint risks appearing as a natural law of religious consciousness (Mountain) — an immutable feature of how humans practice ecstatic religion — but this naturalizing view obscures the contingent institutional enforcement that makes the constraint possible.
 *
 * DIRECTIONALITY LOGIC:
 *   Maenad / Possessed Woman: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. Woman is compelled to participate, has no legitimate exit option, and bears the costs of possession (status loss, vulnerability, labor loss). Peripheral Participants: Victim + constrained → d≈0.68, f(d)≈0.98. Significant extraction but not maximum. These women can theoretically refuse at cost (social ostracism, religious damnation), so exit is constrained rather than absolutely trapped. Dionysian Priesthood: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Net beneficiary. They command the ritual, benefit from institutional legitimacy and power, and can exit or restructure the constraint at will (they are the ones who control when rites occur). Aristocratic Male Audience: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.06. Net beneficiary. They spectate without obligation, benefit from the reinforcement of patriarchal authority, and have complete exit optionality. City-State Authorities: Mixed + constrained → d≈0.45, f(d)≈0.48. Moderate extraction exposure. Authorities benefit from the stability function but risk loss of household labor and potential female insurgency if the ritual becomes too transgressive. Their constrained exit reflects that they cannot simply ban the Dionysian cult without risking religious backlash, but they can regulate it. No overrides are necessary — the structural derivation captures the relationships accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy analysis for Dionysiac possession focuses on whether the constraint is primarily coordination or primarily extraction, and whether the high suppression level is necessary to maintain the function or is anxiety-driven overlay. The base properties (ε=0.52, suppression=0.68, active enforcement required, beneficiaries and victims both present) satisfy the Tangled Rope gates: there IS a coordination function (organizing ecstatic religious experience, channeling collective religious energy, integrating women into civic ritual), and there IS asymmetric extraction (women lose household status, face vulnerability, cannot exit). The mandatrophy is resolved by recognizing that BOTH elements are genuine. The constraint is not mislabeled coordination pretending to be extraction, nor extraction pretending to be coordination — it is a genuine hybrid where the coordination function and the extraction mechanism are structurally entangled. The city-state cannot maintain the coordination without the suppression, and the suppression cannot be maintained without invoking the coordination function as justification. The puzzle is whether the suppression level (0.68) is intrinsic to the coordination or reflects male anxiety. Omega variables ('necessity_of_suppression', 'female_agency_within_possession') address this directly. If future evidence shows that the ritual could function with much lower suppression (women participate voluntarily), then the classification might shift toward Scaffold (temporary coordination with declining suppression) or pure Rope (low-extraction coordination). For the current state of evidence, Tangled Rope with high suppression is the most accurate classification: both coordination and extraction are structural features, not artifacts of measurement perspective.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_ecstasy_vs_performed,
    'To what extent did participants experience genuine altered consciousness (divinely induced madness) versus socially sanctioned performance?',
    'Ethnographic comparison with modern possession rituals; pharmacological analysis of substances used in rites; textual analysis of ecstatic descriptions vs narrative framings by male observers',
    'If mostly genuine: constraint is primarily a coordination mechanism for alternative consciousness (Rope from more perspectives). If mostly performed: constraint is extraction of female labor/visibility under religious cover (Snare from more perspectives).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(genuine_ecstasy_vs_performed, empirical, 'Whether Dionysian ecstasy was genuine altered consciousness or performed').

omega_variable(
    female_agency_within_possession,
    'Did the state of possession provide genuine autonomy and agency to women, or did it create an illusion of autonomy while reinforcing male control?',
    'Analysis of women''s choices during rites (participation timing, spatial movement, refusal patterns); comparison with women''s agency in non-ritual contexts; longitudinal tracking of women''s social status post-possession across generations',
    'If genuine autonomy: constraint has stronger coordination function (Rope/Scaffold). If illusory: constraint is pure extraction masked by religious framing (Snare/Piton).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(female_agency_within_possession, conceptual, 'Whether possession provided genuine female agency').

omega_variable(
    necessity_of_suppression,
    'Was the high suppression level (mandatory participation, social ostracism for refusal, legal vulnerability) necessary to maintain the ritual, or did it reflect male anxiety about autonomous female mobility?',
    'Comparative analysis of voluntary versus enforced participation; investigation of punishment for non-participation; archaeological evidence of sanctuary spaces for women; textual analysis of male anxiety about female independence',
    'If necessary for function: suppression is intrinsic to coordination (Tangled Rope). If anxiety-driven: suppression is pure extraction overlaid on optional coordination (Snare with coercive augmentation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_of_suppression, conceptual, 'Whether suppression was functionally necessary or anxiety-driven').

omega_variable(
    societal_stability_function,
    'Did Dionysian possession rituals stabilize the city-state by providing sanctioned outlet for female transgression, or did they destabilize patriarchal order by temporarily inverting gender hierarchy?',
    'Correlation analysis between frequency of Dionysian rites and periods of social unrest; investigation of actual riots or property damage during mountain rites; comparison of city-states with strong vs weak Dionysian traditions; study of male anxiety in literature during periods of intense Dionysian practice',
    'If stabilizing: constraint is primarily coordination (Rope from authorities'' perspective). If destabilizing: constraint is tension between extraction and resistance (Tangled Rope), with risk of constraint breakdown.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(societal_stability_function, empirical, 'Whether Dionysian rites stabilized or destabilized patriarchal order').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dionysaic_frenzy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dion_tr_t0, dionysaic_frenzy, theater_ratio, 0, 0.35).
narrative_ontology:measurement(dion_tr_t5, dionysaic_frenzy, theater_ratio, 5, 0.47).
narrative_ontology:measurement(dion_tr_t10, dionysaic_frenzy, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(dion_be_t0, dionysaic_frenzy, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(dion_be_t5, dionysaic_frenzy, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(dion_be_t10, dionysaic_frenzy, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dionysaic_frenzy, enforcement_mechanism).
narrative_ontology:affects_constraint(dionysaic_frenzy, patriarchal_household_control).
narrative_ontology:affects_constraint(dionysaic_frenzy, religious_authority_legitimacy).

% DUAL FORMULATION NOTE:
% Dionysiac possession decomposes into two structurally distinct claims: (1) the genuine ecstatic religious experience (ε≈0.15, coordination mechanism, ε-invariant across observers), and (2) the institutional enforcement of mandatory participation with constrained exit (ε≈0.52, extraction mechanism dependent on male authority structure). These are often conflated in classical sources. The story presented here addresses claim (2): the constraint as an institutional apparatus. A separate story addressing claim (1) would examine the phenomenology of possession across cultures, with much lower extractiveness and no suppression. The two stories are linked because institutional enforcement of claim (2) justifies itself through the religious legitimacy of claim (1).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
