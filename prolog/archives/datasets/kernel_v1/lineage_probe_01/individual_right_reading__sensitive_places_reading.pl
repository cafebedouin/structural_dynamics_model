% ============================================================================
% CONSTRAINT STORY: individual_right_reading__sensitive_places_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_individual_right_reading__sensitive_places_reading, []).

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
 *   constraint_id: individual_right_reading__sensitive_places_reading
 *   human_readable: The Sensitive Places Reading: Geographic Metering of the Second Amendment Right
 *   domain: constitutional_law/gun_regulation
 *
 * SUMMARY:
 *   The sensitive places reading of the individual right to bear arms
 *   interprets the constitutional protection as geographically bounded.
 *   Schools, courthouses, and polling places stand outside the right, and the
 *   doctrinal fight has moved to how far the category of 'sensitive places'
 *   can stretch. This constraint represents one reading of the contested
 *   kernel: the Second Amendment's scope. Other readings (Bruen methodology,
 *   Heller core) offer structurally distinct frameworks. The sensitive places
 *   reading treats the right as inherently spatial—not a yes/no question of
 *   whether the right exists, but a map question of where it applies. This
 *   produces a tangled rope structure: genuine coordination needs
 *   (institutions require some measure of safety control at decision points)
 *   exist alongside asymmetric extraction (regulatory authorities gain
 *   unilateral power to redraw the sensitive zones boundary, and carry
 *   claimants bear continuous litigation costs). The constraint's
 *   extractiveness (0.52) reflects that the doctrine meters the right through
 *   geography rather than eliminating it, and suppression (0.68) reflects
 *   that the boundary is uncertain and continuously litigated.
 *
 * KEY AGENTS:
 *   - Carry-Everywhere Claimants: Primary victim (powerless/trapped) — structurally excluded from schools, courthouses, polling places; cannot exit these destinations without forfeiting functional rights
 *   - Location-Based Regulation Authority: Primary beneficiary (institutional/arbitrage) — gains unilateral power to declare zones sensitive; can adjust the map in response to litigation
 *   - Institutional Operators (Schools, Courts, Polling Places): Secondary beneficiary (moderate/constrained) — benefit from carry restrictions enabling institutional security; bear costs of alternative security measures
 *   - Gun Rights Coalition: Organized victim (organized/constrained) — can litigate boundary cases and push back against zone expansion, but cannot foreclose the doctrine itself
 *   - Regulatory Authority (Legislatures, Courts): Primary beneficiary (institutional/arbitrage) — experience the doctrine as a governance tool enabling reconciliation of competing interests
 *   - Doctrine-as-Tradition System: Institutional observer (institutional/analytical) — the sensitive places category survives through institutional inertia and reduces transaction costs of decision-making
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(individual_right_reading__sensitive_places_reading, 0.52).
domain_priors:suppression_score(individual_right_reading__sensitive_places_reading, 0.68).
domain_priors:theater_ratio(individual_right_reading__sensitive_places_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(individual_right_reading__sensitive_places_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(individual_right_reading__sensitive_places_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(individual_right_reading__sensitive_places_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(individual_right_reading__sensitive_places_reading, tangled_rope).
narrative_ontology:human_readable(individual_right_reading__sensitive_places_reading, "The Sensitive Places Reading: Geographic Metering of the Second Amendment Right").
narrative_ontology:topic_domain(individual_right_reading__sensitive_places_reading, "constitutional_law/gun_regulation").

domain_priors:requires_active_enforcement(individual_right_reading__sensitive_places_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(individual_right_reading__sensitive_places_reading, '52c3f043-a6c3-4c4a-8482-83811de34792').
narrative_ontology:cs_kernel_codification('52c3f043-a6c3-4c4a-8482-83811de34792', fixed_text).
narrative_ontology:cs_authority_grounding('52c3f043-a6c3-4c4a-8482-83811de34792', lineage).
narrative_ontology:cs_interpretation_layer_present('52c3f043-a6c3-4c4a-8482-83811de34792').
narrative_ontology:cs_reading_relation('52c3f043-a6c3-4c4a-8482-83811de34792', individual_right_reading__bruen_methodology_reading, coexists_with).
narrative_ontology:cs_reading_relation('52c3f043-a6c3-4c4a-8482-83811de34792', individual_right_reading__heller_core_reading, influences).
narrative_ontology:cs_axiom('52c3f043-a6c3-4c4a-8482-83811de34792', foundational, rights_have_geography).
narrative_ontology:cs_axiom_status(rights_have_geography, holdable).
narrative_ontology:cs_axiom_grounding('52c3f043-a6c3-4c4a-8482-83811de34792', rights_have_geography, conventional).
narrative_ontology:cs_axiom('52c3f043-a6c3-4c4a-8482-83811de34792', secondary, map_boundary_determinacy).
narrative_ontology:cs_axiom_status(map_boundary_determinacy, holdable).
narrative_ontology:cs_axiom_grounding('52c3f043-a6c3-4c4a-8482-83811de34792', map_boundary_determinacy, instrumental).
narrative_ontology:cs_reference_frame('52c3f043-a6c3-4c4a-8482-83811de34792', longstanding_sensitive_places_tradition).
narrative_ontology:cs_drift_state('52c3f043-a6c3-4c4a-8482-83811de34792', post_bruen_expansion_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('52c3f043-a6c3-4c4a-8482-83811de34792', '2026-02-26T14:32:17Z').
narrative_ontology:cs_kernel_id(individual_right_reading__sensitive_places_reading, individual_right_reading).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(individual_right_reading__sensitive_places_reading, location_based_regulation_authority).
narrative_ontology:constraint_beneficiary(individual_right_reading__sensitive_places_reading, designated_sensitive_institution_operators).
narrative_ontology:constraint_victim(individual_right_reading__sensitive_places_reading, carry_everywhere_claimants).
narrative_ontology:constraint_victim(individual_right_reading__sensitive_places_reading, boundary_ambiguity_sufferers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CARRY-EVERYWHERE CLAIMANT (SNARE) — Structurally trapped. The sensitive places doctrine geometrically excludes carry in schools, courthouses, polling places, and whatever additional zones courts expand. The claimant cannot exit—these are ordinary destinations (schools for work or visiting family, courthouses for jury duty or legal proceedings, polling places for voting). The doctrine forces choice between constitutional claim and functional necessity. No alternative recourse. Maximum suppression: the regulatory boundary is continuously redrawn through litigation, and the claimant bears uncertainty cost.
constraint_indexing:constraint_classification(individual_right_reading__sensitive_places_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INSTITUTIONAL OPERATOR (TANGLED ROPE) — Schools, courts, and polling places have genuine coordination needs: managing safety during high-stakes gatherings, controlling access to secure facilities, preventing weapons at decision points where armed conflict could escalate. The sensitive places doctrine enables this coordination. But extraction is present: the institution captures unilateral authority to declare a zone sensitive without independent oversight, and the geographic exclusion creates a map-based asymmetry where one party's interest (institutional security) overrides another's (carry rights). Constrained exit for operators—alternatives (security personnel, magnetometers) exist but are costly and incomplete.
constraint_indexing:constraint_classification(individual_right_reading__sensitive_places_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REGULATORY AUTHORITY (ROPE) — Courts and legislatures experience the sensitive places doctrine as coordination: it provides a formal mechanism to reconcile competing interests (carry rights + institutional security) without wholesale bans or unlimited permit discretion. The Bruen framework left sensitive places explicitly open, enabling regulatory arbitrage—legislators can expand or contract the sensitive zones list, and courts can litigate boundary cases. Arbitrage exit: regulatory authorities can adjust the map in response to litigation pressure. Low extraction from this perspective because the authority has unilateral control of the doctrine's application and benefits from its existence as a governance tool.
constraint_indexing:constraint_classification(individual_right_reading__sensitive_places_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DOCTRINE-AS-TRADITION (PITON) — From a long historical view, the sensitive places concept is a vestigial element of pre-Heller jurisprudence. Heller (2008) and McDonald (2010) nominally fixed the core right (handguns in the home) but left the 'longstanding regulations' category undefined, creating space for sensitive places to persist. But the actual function of this category is increasingly performative: courts invoke 'sensitive places' as shorthand for avoiding Bruen's historical-analogue test rather than as a distinct constitutional doctrine. The theater ratio reflects this: the opinion-writing about sensitive places fills judicial bandwidth (theater = 0.58) but the actual legal work is done by Bruen's methodology. The doctrine survives through institutional inertia—it is familiar to judges and legislatures, reducing transaction costs of decision-making—not because it solves a coordination problem better than alternatives would.
constraint_indexing:constraint_classification(individual_right_reading__sensitive_places_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

% PERSPECTIVE 5: GUN RIGHTS COALITION (TANGLED ROPE) — Organized carry advocates have agency through litigation and lobbying, but constrained by the doctrine's foundational acceptance in Bruen itself. The coalition can fight boundary cases (how far does 'sensitive' stretch?) and push back against expansion of the sensitive zones list, but cannot foreclose the doctrine entirely—it is written into constitutional orthodoxy. Genuine coordination function exists: the sensitive places doctrine does settle SOME disputes (wholesale bans are off the table, but some exclusions are permissible). But extraction is real: the doctrine's boundaries are litigated through a process where institutional actors control definitions, and organized claimants bear the cost of continuous boundary-defense.
constraint_indexing:constraint_classification(individual_right_reading__sensitive_places_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: NATURAL LAW VIEW (MOUNTAIN) — From a foundational analytical perspective, there may appear to be an unchangeable tension: rights have edges, boundaries require definition, and some places (courthouses, voting centers) have irreducible coordination needs that conflict with unlimited carry. This perspective views the sensitive places doctrine as an immutable consequence of political geometry—you cannot have an unqualified right to carry everywhere AND maintain institutional integrity of places where decisions are made. However, this mountain classification is a false summit: the structural beneficiary data (location-based regulation authority, institutional operators) reveals this as a contingent institutional settlement, not a natural law. The framework will detect FSM and route this to mandatrophy review.
constraint_indexing:constraint_classification(individual_right_reading__sensitive_places_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(individual_right_reading__sensitive_places_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(individual_right_reading__sensitive_places_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(individual_right_reading__sensitive_places_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(individual_right_reading__sensitive_places_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(individual_right_reading__sensitive_places_reading, TR),
    TR >= 0.70.

:- end_tests(individual_right_reading__sensitive_places_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): The sensitive places doctrine does not eliminate the right—it partitions it geographically. Carry is permitted in most places and prohibited in designated zones. The extractiveness reflects the cost borne by claimants who must navigate the boundary, manage continuous litigation risk, and make functional choices (can I carry to vote? To work at a school?). The value is moderate, not severe, because the core right is nominally preserved in its home/self-defense dimension. Theater ratio (0.58): Post-Bruen opinion-writing about sensitive places has become increasingly performative. Courts invoke 'sensitive places' as shorthand reasoning that avoids engaging with Bruen's historical-analogue test. The phrase fills opinions (theater) without doing substantial analytical work (function). The rising trajectory (0.42 → 0.58) reflects post-Bruen doctrinal reliance on the category. Suppression (0.68): High but not total. The sensitive places doctrine creates significant uncertainty: claimants cannot know in advance whether a destination will be deemed sensitive (pending litigation), and institutional operators continuously expand or defend boundaries. The rising trajectory reflects post-Bruen uncertainty amplification as courts adjudicate new sensitive zones.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a significant perspectival gap between the trap experienced by individual claimants and the coordination flexibility experienced by regulatory authorities. The carry-everywhere claimant perceives snare (no exit, maximum suppression). The regulatory authority perceives rope (functional coordination mechanism with unilateral control). The institutional operator perceives tangled_rope (genuine coordination needs + unilateral authority asymmetry). The gun rights coalition perceives constrained resistance to an entrenched doctrine. The piton perspective reveals that judicial language about sensitive places is increasingly theater rather than law. The analytical natural law perspective risks seeing geographic exclusion as immutable (false summit—structural beneficiaries reveal it as contingent regulation). The mandatrophy appears as: is the doctrine a legitimate geographic reconciliation of competing interests, or is it a backdoor to interest-balancing that Bruen nominally rejected?
 *
 * DIRECTIONALITY LOGIC:
 *   Carry-everywhere claimants are structurally trapped by the doctrine: they cannot exit the destinations covered by sensitive places restrictions without forfeiting functional rights (voting, legal representation, children's education). Their directionality (d ≈ 0.95) is maximal—they bear extraction with no exit. Regulatory authorities are structurally beneficiaries with arbitrage options: they control the doctrine's application and can adjust boundaries, giving them low directionality (d ≈ 0.08). Institutional operators occupy a middle position: they benefit from carry restrictions (constraining their security burden) but cannot fully escape carry-related risk if broader zones remain open, giving them moderate directionality (d ≈ 0.52). The gun rights coalition has organized power and litigation access, reducing their experienced extraction relative to individual powerless claimants, but they cannot foreclose the doctrine (d ≈ 0.55). The doctrinal tradition perspective is analytical and sees the constraint as a performative category (piton classification).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy for this constraint is: does the sensitive places doctrine represent a legitimate geographic reconciliation of competing interests (carry rights + institutional security), or does it function as a back-door interest-balancing mechanism that Bruen's historical-analogue test nominally rejected? If the former, the doctrine is tangled_rope (genuine coordination + asymmetric extraction). If the latter, it is snare (regulatory escape hatch from constitutional constraint). The empirical resolution mechanisms in the omegas (boundary drift, institutional security necessity, Heller coherence, Bruen escape-hatch function) would determine which classification is correct. The false summit risk is real: the natural law perspective (mountain) risks naturalizing a contingent institutional settlement as an immutable feature of political geometry. The doctrine's theater ratio rising trajectory suggests it is becoming increasingly performative (piton drift), which would indicate the coordination function is atrophying.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sensitive_place_definition_drift,
    'What constitutes a ''sensitive place''? Is it a bounded constitutional category (schools, courthouses, polling places + narrow additions) or a open-ended regulatory variable?',
    'Track judicial decisions post-Bruen: how many zones have been added to the sensitive places list? What criteria did courts use to expand or contract? Is there convergence on a stable boundary or continuous expansion?',
    'If bounded: sensitive places is a stable doctrine allocating carry rights geographically. If open-ended: sensitive places functions as a regulatory escape hatch from Bruen''s historical-analogue test—extractiveness rises to 0.65+ and classification shifts toward snare. Mandatrophy consequence: is the doctrine a legitimate coordination mechanism or a backdoor interest balancing dressed as geography?',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sensitive_place_definition_drift, empirical, 'Whether sensitive places is a bounded category or open-ended regulatory variable').

omega_variable(
    institutional_security_necessity,
    'Do the coordination needs of courthouses, polling places, and schools actually require carry restrictions, or can these be met through alternative security measures (personnel, magnetometers, access control)?',
    'Comparative institutional analysis: which institutions have eliminated carry restrictions and substituted alternative security? Do incidents increase or remain stable? Are there non-carry-restrictive models in other democratic contexts?',
    'If alternatives are sufficient: sensitive places is extraction without genuine coordination need—reclassify snare from organized perspective. If alternatives are insufficient: coordination function is real, tangled_rope classification confirmed across perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_security_necessity, empirical, 'Whether institutional security requires carry restrictions or can use alternatives').

omega_variable(
    heller_core_vs_sensitive_places_reconciliation,
    'Is the sensitive places doctrine coherent with Heller''s core right (handguns in the home for self-defense), or does geographic exclusion from ordinary destinations erode the right to a hollow core?',
    'Historical tracking: how have courts justified the compatibility of Heller''s core with expanding sensitive zones? At what point does geographic restriction contradict core-protection logic?',
    'If coherent: sensitive places is a legitimate reading of Heller + Bruen. If incoherent: the doctrine represents doctrinal drift toward interest balancing—extractiveness rises, mandatrophy emerges, false summit exposed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(heller_core_vs_sensitive_places_reconciliation, conceptual, 'Logical compatibility of sensitive places doctrine with Heller''s core-right framework').

omega_variable(
    bruen_methodology_escape_hatch,
    'Does the sensitive places doctrine function as an escape hatch from Bruen''s historical-analogue test, allowing courts to uphold modern regulations without finding founding-era parallels?',
    'Doctrinal analysis: in post-Bruen cases upholding sensitive zones, how often did courts skip the historical-analogue analysis and rely on ''sensitive places'' categorization alone? Is this methodologically distinct from Bruen or a back-door interest balancing?',
    'If escape hatch: the doctrine is performative (theater ratio justified). Bruen_methodology_reading forecloses sensitive_places_reading—the two cannot coexist as independent analytic frameworks. If not: the doctrine is methodologically honest within Bruen, and coexists_with relation holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bruen_methodology_escape_hatch, conceptual, 'Whether sensitive places doctrine functions as methodological escape hatch from Bruen').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(individual_right_reading__sensitive_places_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(senspl_tr_t0, individual_right_reading__sensitive_places_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(senspl_tr_t3, individual_right_reading__sensitive_places_reading, theater_ratio, 3, 0.5).
narrative_ontology:measurement(senspl_tr_t6, individual_right_reading__sensitive_places_reading, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(senspl_be_t0, individual_right_reading__sensitive_places_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(senspl_be_t3, individual_right_reading__sensitive_places_reading, base_extractiveness, 3, 0.44).
narrative_ontology:measurement(senspl_be_t6, individual_right_reading__sensitive_places_reading, base_extractiveness, 6, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(senspl_su_t0, individual_right_reading__sensitive_places_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(senspl_su_t3, individual_right_reading__sensitive_places_reading, suppression_requirement, 3, 0.64).
narrative_ontology:measurement(senspl_su_t6, individual_right_reading__sensitive_places_reading, suppression_requirement, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(individual_right_reading__sensitive_places_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(individual_right_reading__sensitive_places_reading, individual_right_reading__bruen_methodology_reading).
narrative_ontology:affects_constraint(individual_right_reading__sensitive_places_reading, individual_right_reading__heller_core_reading).

% DUAL FORMULATION NOTE:
% The sensitive_places_reading is one of three structurally distinct readings of the Second Amendment's scope within the kernel individual_right_reading. Each reading instantiates a different constraint with different ε values and perspectives. The sensitive_places_reading treats the right as geographically bounded (ε=0.52); the bruen_methodology_reading treats the right as method-bounded (historical analogue requirement); the heller_core_reading treats the right as identity-centered (handguns in the home). Network edges link these as sibling readings within the same contestation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(individual_right_reading__sensitive_places_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
