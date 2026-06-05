% ============================================================================
% CONSTRAINT STORY: liturgical_vernacularization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_liturgical_vernacularization, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: liturgical_vernacularization
 *   human_readable: Liturgical Vernacularization and Roman Authority
 *   domain: religious_institutional_change/ecclesiology
 *
 * SUMMARY:
 *   Vatican II (1962-1965) authorized the vernacularization of the Latin
 *   Mass, transferring liturgical composition authority from the Roman Curia
 *   to national episcopal conferences. This constraint exhibits a crucial
 *   tension: it presents itself as genuine decentralization (Rope) while
 *   retaining papal doctrinal monopoly over what counts as 'authentic'
 *   translation and liturgical practice (Tangled Rope/Snare). The
 *   constraint's classification depends entirely on the observer's structural
 *   position. For lay Catholics, vernacular access improved intelligibility
 *   but subordinated them to episcopal authority structures. For episcopal
 *   conferences, devolution appeared to grant pastoral autonomy while Rome
 *   retained veto power over translations and practice. For the Roman Curia,
 *   vernacularization solved the coordination problem of universal liturgy in
 *   a multilingual Church while preserving Rome's ultimate doctrinal
 *   authority. For the Vatican II reform coalition (progressive bishops,
 *   theological movements), the constraint was scaffolding toward
 *   democratized theology and reduced papal monopoly — but the sunset never
 *   occurred. For the ecclesiastical apparatus itself, vernacularization
 *   became a performative theater: Latin Mass restrictions and allowances
 *   oscillate, debates over 'authentic' translations persist, and much
 *   institutional energy flows into legitimating whichever linguistic choice
 *   prevails. The analytical observer risks seeing linguistic pluralism as a
 *   natural law of ritual practice, naturalizing what is actually a contested
 *   institutional choice rooted in 20th-century Church politics. The
 *   constraint's extractiveness has declined from 0.72 (pre-Vatican II Latin
 *   monopoly) to 0.58 (post-Vatican II mixed system), while theater has
 *   increased from 0.15 to 0.68, suggesting the constraint is degrading
 *   toward Piton classification.
 *
 * KEY AGENTS:
 *   - Lay Catholic Community: Primary victim (powerless/trapped) — dependent on ecclesiastical authority for sacramental access; extractiveness manifests as linguistic gatekeeping and suppression of lay theological agency
 *   - National Episcopal Conferences: Primary beneficiary AND constrained actor (organized/constrained) — gain pastoral authority over liturgy but remain subordinate to Rome's doctrinal veto; dual directionality creates Tangled Rope experience
 *   - Roman Curia/Vatican: Primary beneficiary (institutional/arbitrage) — maintains doctrinal monopoly while appearing to grant decentralization; experiences constraint as pure coordination mechanism enabling Rome to retain ultimate authority
 *   - Vatican II Reform Coalition: Organized secondary actor (organized/constrained) — progressive bishops, theological movements, lay organizations viewing vernacularization as sunset scaffolding toward democratic theology; experience constraint as degraded after sunset fails
 *   - Latin Tradition Communities: Secondary victim (powerful/constrained) — lose institutional support for Latin liturgy post-Vatican II; exit options degrade through seminary defunding and Latin instruction decline
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing institutional choice (linguistic pluralism) as immutable principle, creating false-summit classification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liturgical_vernacularization, 0.58).
domain_priors:suppression_score(liturgical_vernacularization, 0.65).
domain_priors:theater_ratio(liturgical_vernacularization, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liturgical_vernacularization, extractiveness, 0.58).
narrative_ontology:constraint_metric(liturgical_vernacularization, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(liturgical_vernacularization, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liturgical_vernacularization, tangled_rope).
narrative_ontology:human_readable(liturgical_vernacularization, "Liturgical Vernacularization and Roman Authority").
narrative_ontology:topic_domain(liturgical_vernacularization, "religious_institutional_change/ecclesiology").

domain_priors:requires_active_enforcement(liturgical_vernacularization).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(liturgical_vernacularization, fixed_text).
narrative_ontology:cs_authority_grounding(liturgical_vernacularization, lineage).
narrative_ontology:cs_interpretation_layer_present(liturgical_vernacularization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liturgical_vernacularization, lay_participation).
narrative_ontology:constraint_beneficiary(liturgical_vernacularization, national_episcopal_conferences).
narrative_ontology:constraint_beneficiary(liturgical_vernacularization, linguistic_communities).
narrative_ontology:constraint_victim(liturgical_vernacularization, latin_language_tradition).
narrative_ontology:constraint_victim(liturgical_vernacularization, liturgical_uniformity).
narrative_ontology:constraint_victim(liturgical_vernacularization, papal_doctrinal_monopoly).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MONOLINGUAL LAY CATHOLICS (SNARE) — Trapped in liturgical participation without intelligibility. Pre-Vatican II, the Latin requirement suppressed understanding; post-Vatican II vernacular implementation varies by region, creating uneven access. Exit is impossible — the constraint governs access to the sacraments themselves. Powerless agents bear the suppression cost of ecclesiastical authority's control over linguistic legitimacy.
constraint_indexing:constraint_classification(liturgical_vernacularization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NATIONAL EPISCOPAL CONFERENCE (TANGLED ROPE) — Genuinely benefits from vernacularization (pastoral authority at the local level, liturgical adaptation). But constrained by Rome's retention of doctrinal oversight and missal approval authority. The constraint coordinates pastoral adaptation AND enforces papal control over how adaptation occurs. Extraction flows both ways: authority devolved to conferences, but also regulated by Rome.
constraint_indexing:constraint_classification(liturgical_vernacularization, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ROMAN CURIA (ROPE) — Benefits from vernacularization as a coordination mechanism that appears to devolve power while maintaining Rome's ultimate doctrinal authority. The constraint solves a real coordination problem (liturgy must be in languages people speak). Rome experiences this as pure coordination: the illusion of pluralism while reserving doctrinal judgment.
constraint_indexing:constraint_classification(liturgical_vernacularization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: VATICAN II REFORM COALITION (SCAFFOLD) — Organized agents (progressive bishops, theological movements, lay organizations) viewed vernacularization as a temporary corrective to clericalism with an intended sunset: democratization of theology would eventually create peer authority structures, reducing papal monopoly. But the sunset never occurred — the constraint became permanent institutional feature. Reform coalition sees the constraint as degraded, theater-heavy performance of change without substantive authority devolution.
constraint_indexing:constraint_classification(liturgical_vernacularization, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: POST-VATICAN II ECCLESIASTICAL APPARATUS (PITON) — The vernacularization constraint persists through institutional inertia, theater, and accommodation. The authentic coordination function (making liturgy intelligible) coexists with persistent theatrical elements: Latin Mass resurgence (Traditionalist Mass allowance and recent restrictions), debates over liturgical 'authenticity,' and continued papal micromanagement of translation details. Theater ratio (0.68) reflects that much activity is legitimation theater (defending the chosen vernacular) rather than functional coordination.
constraint_indexing:constraint_classification(liturgical_vernacularization, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / UNIVERSAL PRINCIPLE VIEW (MOUNTAIN) — From civilizational scope, some degree of linguistic translation in liturgy appears as a natural law: any ritual must be in a language participants comprehend. This perspective risks naturalizing what is actually a contingent institutional choice (Latin hegemony 1600-1960 was itself a choice; return to Latin-only would be a choice; vernacular pluralism is a choice). The engine's false-summit detection should flag this as naturalization of the institutional arrangement.
constraint_indexing:constraint_classification(liturgical_vernacularization, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(liturgical_vernacularization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(liturgical_vernacularization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(liturgical_vernacularization, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(liturgical_vernacularization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(liturgical_vernacularization, TR),
    TR >= 0.70.

:- end_tests(liturgical_vernacularization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): The constraint exhibits moderate extraction because multiple beneficiaries capture asymmetric value. Pre-Vatican II (t=0: ε=0.72), Latin monopoly extracted by suppressing lay understanding and episcopal pastoral authority. Post-Vatican II (t=6: ε=0.58), extraction reduced because vernacularization genuinely improves lay intelligibility and grants episcopal conferences real (though constrained) authority. But extraction persists because Rome retains doctrinal oversight, enabling selective enforcement of 'authentic' translations and preventing the reform coalition's intended decentralization. Suppression (0.65): Structural barriers remain high. Lay Catholics cannot exit sacramental participation, creating trapped-exit dependency. Episcopal conferences face career risk (papal disfavor) and resource constraints (Rome controls canonical texts, training materials). Latin tradition communities lost institutional support (seminaries shifted to vernacular, Latin instruction declined). Theater ratio (0.68): High and increasing. Pre-Vatican II theater was low (0.15) — Latin obscurity was the extraction mechanism, not performance. Post-Vatican II theater rose through: oscillating restrictions/allowances (2007 allowance for Traditional Latin Mass, 2019 restrictions), performative debates over translation 'authenticity,' and legitimation rituals around chosen vernacular. Theater increased because the actual power structure (papal doctrinal monopoly) diverged from the apparent structure (episcopal decentralization), requiring theatrical performance to reconcile the gap. The increasing theater_ratio (0.15 → 0.68) combined with declining extractiveness (0.72 → 0.58) is diagnostic of Piton degradation: the constraint's primary function (coordinating multilingual liturgy) persists, but atrophied authority (Vatican II's failed sunset) now requires theatrical maintenance.
 *
 * PERSPECTIVAL GAP:
 *   The maximum perspectival gap occurs between the Roman Curia (Rope, χ ≈ -0.007) and the lay Catholic (Snare, χ ≈ 0.99) — a 99-unit difference in experienced extraction from identical structural properties. This gap is diagnostic of a hybrid coordination-extraction constraint. The Curia genuinely solves a coordination problem (multilingual liturgy requires local adaptation) and benefits from preserving doctrinal monopoly (Rome retains ultimate authority). The lay Catholic faces trapped participation in a system they cannot understand and cannot exit. Both experiences are structurally accurate; they are viewing the same constraint from opposite power positions. The gap reveals that Vatican II is not a neutral coordination mechanism but a structural rearrangement that preserved Rome's power while appearing to devolve it.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality structure reflects Vatican II's core ambiguity: it presents as decentralization (episcopal conferences gain liturgical authority) while preserving centralization (Rome retains doctrinal veto). This duality creates the Tangled Rope classification. Beneficiaries (national episcopal conferences, lay participation, linguistic communities) gain real authority over liturgical form but remain subordinate to Rome's judgment of doctrinal correctness. Victims (Latin tradition, liturgical uniformity, papal monopoly aspiration) lose institutional support — but these are not powerless agents; they are institutional structures that lose value. The papal monopoly on doctrine persists through the apparent devolution: Rome approves or rejects episcopal conference translations, can restrict or allow Latin Mass use, and maintains the authority to redefine what counts as 'authentic' Catholic practice. The Tangled Rope classification holds because the constraint genuinely coordinates (solving the multilingual liturgy problem) AND genuinely extracts (Rome retains doctrinal monopoly while devolving administrative burden). The constraint's persistence through theater ratio increase (0.15 → 0.68) and extractiveness decline (0.72 → 0.58) suggests institutional degradation: the original extraction mechanism (Latin incomprehensibility) weakened, so theater (performative debates, oscillating allowances) became necessary to maintain the structure. This is the diagnostic signature of Piton classification mixed with Tangled Rope.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy — the tension between treating it as pure coordination (Rope) and pure extraction (Snare) — by revealing that it is genuinely both. The Roman Curia's perspective (Rope) and the lay Catholic's perspective (Snare) are not different readings of the same reality but observations from structurally opposite positions within the constraint. The Tangled Rope classification acknowledges that vernacularization simultaneously coordinates (makes liturgy intelligible, enables local pastoral adaptation) and extracts (Rome retains ultimate doctrinal authority, suppresses Latin tradition, prevents the reform coalition's intended democratization). The mandatrophy is resolved not by choosing one type but by recognizing that the constraint has a genuine coordination function (beneficiaries list: lay participation, episcopal conferences, linguistic communities) AND a genuine extraction function (victims list: Latin tradition, liturgical uniformity, papal monopoly dissolution prevented). The increasing theater ratio (0.15 → 0.68) indicates the constraint is degrading: as the original extraction mechanism (Latin monopoly) weakened, institutional theater became necessary to maintain the hybrid structure. The Piton classification from the ecclesiastical apparatus perspective (perspective 5) observes exactly this degradation — the constraint persists through inertia and theater, not functional necessity. The mandatrophy resolution maps to the scaffold perspective's failed sunset: Vatican II intended to devolve authority permanently (creating a stable new coordination structure), but Rome's retention of doctrinal veto prevented the sunset, leaving a hybrid that requires increasing theater to maintain. The constraint's true type is contextual: it is Rope for Rome, Tangled Rope for episcopal conferences, Snare for lay Catholics, degraded Scaffold for reformers, and Piton for the Church apparatus itself. No single type captures it — the presheaf of classifications IS the constraint's structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vatican_ii_kernel_unity,
    'Is Vatican II a single reinterpretation of Catholic tradition generating one coherent constraint, or multiple independent doctrinal shifts that produced structurally different constraints bundled together?',
    'Structural decomposition: Does liturgical vernacularization have the same ε, beneficiary/victim structure, and suppression as collegiality reforms, scripture translation authority disputes, and lay participation directives? If ε values differ significantly across these domains, Vatican II is multiple constraints, not one.',
    'If single reading: constraint family has one story (liturgical_vernacularization). If multiple readings: each doctrinal shift should decompose into separate constraint stories (liturgical_authority_devolution vs. papal_doctrinal_monopoly vs. lay_theological_agency), linked by network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vatican_ii_kernel_unity, conceptual, 'Whether Vatican II is one reading or multiple simultaneous readings').

omega_variable(
    papal_authority_retention,
    'Does Rome''s retention of doctrinal oversight over vernacular translations represent active enforcement of papal authority or performative theater masking actual pastoral devolution?',
    'Historical analysis: track instances where Vatican overruled episcopal conference translation choices (ICEL controversy, gender-language debates). Count instances of enforcement vs. deference. If Rome acts on <10% of conference submissions, theater dominates. If >50%, enforcement is genuine.',
    'If enforcement: suppression (0.65) and extraction (0.58) are understated; reclassify toward Snare. If theater: theater_ratio should be higher; reclassify toward Piton. If mixed: Tangled Rope stands.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(papal_authority_retention, empirical, 'Whether papal doctrinal retention is enforced or theatrical').

omega_variable(
    latin_tradition_exit_options,
    'Did vernacularization suppress the Latin tradition as a victim group, or did it preserve Latin as an option while expanding access—changing exit options rather than removing them?',
    'Longitudinal data on Latin Mass availability, seminary Latin instruction, Latin liturgical training. If access expanded post-Vatican II: Latin not a victim (exit_options improved). If access declined: Latin is a victim (exit_options degraded).',
    'If Latin expanded: Latin tradition is not a victim; reclassify vernacularization as pure coordination (Rope). If Latin declined: Latin tradition is a genuine victim; Tangled Rope/Snare stands. Recent Traditionalist Mass allowance (pre-2019) suggests expansion; then rescission (2019+) suggests degradation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(latin_tradition_exit_options, empirical, 'Whether vernacularization suppressed or preserved Latin tradition').

omega_variable(
    reform_coalition_sunset_failure,
    'Was the Scaffold classification''s intended sunset (democratic theology reducing papal monopoly) structurally impossible from the start, or did institutional resistance prevent a feasible outcome?',
    'Comparative ecclesiology: examine Protestant denominations that decentralized doctrine authority post-1960s. If successful decentralization exists elsewhere, sunset was structurally possible but Rome prevented it. If no denomination achieved stable decentralization, sunset may be structurally impossible.',
    'If preventable: Rome enforced the sunset failure; extraction (0.58) understates intentional monopoly preservation. If structural: sunset failure reflects impossible bargain; constraint is genuinely hybrid (Tangled Rope) rather than degraded Scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_coalition_sunset_failure, conceptual, 'Whether the intended Scaffold sunset was structurally feasible').

omega_variable(
    linguistic_pluralism_coordination,
    'Does genuine liturgical coordination require linguistic pluralism (vernacular diversity at local levels) or does Roman uniformity serve an underlying coordination function that pluralism undermines?',
    'Analysis of schism rates, doctrine disputes, and institutional stability pre/post-vernacularization. If schisms increase post-Vatican II, uniform language may have coordinated doctrine. If stability improves or schisms decrease, pluralism coordinates better.',
    'If uniform language coordinated doctrine: extractiveness (0.58) reflects Snare properties (Rome used Latin to suppress doctrinal dissent); reclassify as pure extraction. If pluralism coordinates: beneficiary/victim structure inverts; reclassify as Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(linguistic_pluralism_coordination, empirical, 'Whether linguistic pluralism or uniformity better coordinates doctrine').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liturgical_vernacularization, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_pre_vatican_ii, liturgical_vernacularization, theater_ratio, 0, 0.15).
narrative_ontology:measurement(theater_post_vatican_transition, liturgical_vernacularization, theater_ratio, 3, 0.45).
narrative_ontology:measurement(theater_mid_implementation, liturgical_vernacularization, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(extractiveness_pre_vatican_ii, liturgical_vernacularization, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(extractiveness_post_vatican_transition, liturgical_vernacularization, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(extractiveness_mid_implementation, liturgical_vernacularization, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liturgical_vernacularization, enforcement_mechanism).
narrative_ontology:affects_constraint(liturgical_vernacularization, papal_doctrinal_monopoly).
narrative_ontology:affects_constraint(liturgical_vernacularization, episcopal_conference_authority).
narrative_ontology:affects_constraint(liturgical_vernacularization, lay_theological_agency).

% DUAL FORMULATION NOTE:
% Liturgical vernacularization is downstream of the fundamental question of whether Vatican II represents one coherent reinterpretation of Catholic tradition or multiple independent doctrinal shifts bundled together. If Vatican II decomposes into separate constraints (liturgical authority devolution vs. doctrinal pluralism vs. lay agency), this story focuses on the liturgical constraint specifically. The other constraints would have different ε values, beneficiary/victim structures, and time courses. All three stories should link to each other via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(liturgical_vernacularization, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
