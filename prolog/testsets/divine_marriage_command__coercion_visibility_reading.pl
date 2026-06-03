% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__coercion_visibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_marriage_command_coercion_visibility_reading, []).

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
 *   constraint_id: divine_marriage_command__coercion_visibility_reading
 *   human_readable: Divine Marriage Command (Coercion Visibility Reading)
 *   domain: religious_authority/commitment_systems/political_theology
 *
 * SUMMARY:
 *   The 1890 Manifesto represents an institutional response to federal
 *   coercion — arrest, property seizure, dissolution threats applied to a
 *   religious institution whose core doctrinal claim was plural marriage.
 *   This reading frames the constraint as a legitimacy problem: how does an
 *   institution maintain authority claims when its doctrinal positions are
 *   modified under external coercion? The constraint exhibits tangled rope
 *   structure because it combines genuine coordination functions (the
 *   institution must respond to external pressure to survive) with asymmetric
 *   extraction (faithful members who lived the original doctrine bear costs;
 *   institutional leadership and federal authorities gain benefits from the
 *   resolution). The theater ratio rises over the measurement interval
 *   because the hermeneutic apparatus required to frame coerced doctrinal
 *   change as theologically coherent becomes increasingly visible and
 *   performative as historians document the causal chain: federal pressure →
 *   institutional capitulation → theological rationalization. The suppression
 *   requirement also rises as the institution must work harder to maintain
 *   the reading's coherence against historical evidence that coercion was
 *   operative. The constraint differs from the continuationist reading (which
 *   denies coercion was decisive) by ADMITTING coercion; it differs from the
 *   substitutionist reading (which claims new revelation) by grounding
 *   legitimacy in institutional survival necessity rather than in new divine
 *   disclosure.
 *
 * KEY AGENTS:
 *   - Institutional Survival Apparatus: Primary beneficiary (institutional/arbitrage) — leadership coordinating compliance with federal pressure; experiences constraint as coordination mechanism for survival
 *   - Federal Enforcement Apparatus: Secondary beneficiary/enforcer (powerful/constrained) — applies coercive pressure; benefits from doctrinal change reducing resistance; coordinates institutional compliance
 *   - Faithful Polygamists: Primary victim (powerless/trapped) — those who enacted the original doctrine as divine command; face legitimacy collapse and theological limbo
 *   - Doctrinal Continuity Faction: Mixed (moderate/constrained) — institutional actors maintaining the hermeneutic reading; constrained by loyalty requirements but benefit from flexibility the reading provides
 *   - Covenant Theology Integrity: Victim (powerless/trapped, civilizational) — abstract collective good; the theological claim that divine commands are immutable
 *   - Hermeneutic Theater System: Institutional actor (institutional/arbitrage, civilizational) — maintains coherence-rationalization apparatus; experiences degrading functionality as historical visibility increases
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing contingent institutional coercion response as immutable law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__coercion_visibility_reading, 0.58).
domain_priors:suppression_score(divine_marriage_command__coercion_visibility_reading, 0.68).
domain_priors:theater_ratio(divine_marriage_command__coercion_visibility_reading, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__coercion_visibility_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__coercion_visibility_reading, "Divine Marriage Command (Coercion Visibility Reading)").
narrative_ontology:topic_domain(divine_marriage_command__coercion_visibility_reading, "religious_authority/commitment_systems/political_theology").

domain_priors:requires_active_enforcement(divine_marriage_command__coercion_visibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__coercion_visibility_reading, 'be5c6db1-de89-4fc0-b099-92273c3046d8').
narrative_ontology:cs_kernel_codification('be5c6db1-de89-4fc0-b099-92273c3046d8', formalized).
narrative_ontology:cs_authority_grounding('be5c6db1-de89-4fc0-b099-92273c3046d8', extraction).
narrative_ontology:cs_interpretation_layer_present('be5c6db1-de89-4fc0-b099-92273c3046d8').
narrative_ontology:cs_reading_relation('be5c6db1-de89-4fc0-b099-92273c3046d8', divine_marriage_command__continuationist_reading, coexists_with).
narrative_ontology:cs_reading_relation('be5c6db1-de89-4fc0-b099-92273c3046d8', divine_marriage_command__substitutionist_reading, influences).
narrative_ontology:cs_axiom('be5c6db1-de89-4fc0-b099-92273c3046d8', foundational, exogenous_pressure_operative_and_admitted).
narrative_ontology:cs_axiom_status(exogenous_pressure_operative_and_admitted, holdable).
narrative_ontology:cs_axiom_grounding('be5c6db1-de89-4fc0-b099-92273c3046d8', exogenous_pressure_operative_and_admitted, empirically_contingent).
narrative_ontology:cs_axiom('be5c6db1-de89-4fc0-b099-92273c3046d8', foundational, institutional_survival_necessity_as_theological_ground).
narrative_ontology:cs_axiom_status(institutional_survival_necessity_as_theological_ground, holdable).
narrative_ontology:cs_axiom_grounding('be5c6db1-de89-4fc0-b099-92273c3046d8', institutional_survival_necessity_as_theological_ground, instrumental).
narrative_ontology:cs_reference_frame('be5c6db1-de89-4fc0-b099-92273c3046d8', divine_command_immutability_framework).
narrative_ontology:cs_drift_state('be5c6db1-de89-4fc0-b099-92273c3046d8', post_manifesto_historical_scholarship, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('be5c6db1-de89-4fc0-b099-92273c3046d8', '').
narrative_ontology:cs_kernel_id(divine_marriage_command__coercion_visibility_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__coercion_visibility_reading, institutional_survival_apparatus).
narrative_ontology:constraint_beneficiary(divine_marriage_command__coercion_visibility_reading, doctrinal_continuity_faction).
narrative_ontology:constraint_victim(divine_marriage_command__coercion_visibility_reading, covenant_theology_integrity).
narrative_ontology:constraint_victim(divine_marriage_command__coercion_visibility_reading, epistemic_consistency_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE FAITHFUL POLYGAMIST (SNARE) — Those who lived covenantal polygamy as divine command face a legitimacy collapse: the doctrine they enacted becomes framed as suspended-but-not-rescinded, leaving their families in theological limbo. Exit would require rejecting the institutional authority that granted the original command. Zero degrees of freedom — trapped between fidelity to the original command and institutional necessity.
constraint_indexing:constraint_classification(divine_marriage_command__coercion_visibility_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE DOCTRINAL CONTINUITY FACTION (TANGLED_ROPE) — Institutional actors who maintain that the command remains valid doctrinally while being prudentially suspended. Constrained by institutional loyalty requirements and career consequences of doctrinal dissent, but also benefit from the hermeneutic flexibility the reading provides: they can claim theological fidelity while complying with federal pressure. Mixed extraction and coordination.
constraint_indexing:constraint_classification(divine_marriage_command__coercion_visibility_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE INSTITUTIONAL SURVIVAL APPARATUS (ROPE) — The leadership coordinating institutional compliance with federal coercion. Experiences the constraint as coordination: the Manifesto solves the collective action problem of survival under pressure. Net beneficiary — the reading legitimizes what would otherwise be capitulation by framing it as prudential, not doctrinal. High institutional agency and exit options.
constraint_indexing:constraint_classification(divine_marriage_command__coercion_visibility_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE FEDERAL ENFORCEMENT APPARATUS (TANGLED_ROPE) — State power applying coercive pressure (arrest, property seizure, institutional dissolution threats). Experiences the constraint as both enforcement mechanism and coordination problem: suppressing polygamy requires sustained institutional coordination. The Manifesto reduces enforcement costs by legitimizing compliance within the institution's own authority frame. Moderate extraction from the coerced institution combined with genuine coordination function.
constraint_indexing:constraint_classification(divine_marriage_command__coercion_visibility_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: COVENANT THEOLOGY INTEGRITY (SNARE) — The abstract structural claim that divine commands are immutable. If coercive pressure can suspend or nullify divine commands, the entire legitimacy structure of the covenant collapses. No exit option — the theological commons cannot organize or defend itself. Maximum extraction.
constraint_indexing:constraint_classification(divine_marriage_command__coercion_visibility_reading, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 6: THE HERMENEUTIC THEATER SYSTEM (PITON) — The interpretive apparatus that framed the Manifesto as 'revelation' or 'prudence' rather than 'coerced capitulation.' This theater (sustained theological language, revelation claims, continuationist framing) is substantially performative — maintaining the reading as coherent when its structural basis is acknowledged coercion. Theater persists through institutional inertia; the performative function has degraded over time as historians documented the actual causal chain.
constraint_indexing:constraint_classification(divine_marriage_command__coercion_visibility_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: THE ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, institutional survival under coercion is a brute structural fact: any institution facing existential pressure will adapt its doctrinal claims to ensure survival. This perspective sees the constraint as an immutable law of institutional dynamics — coercion → doctrinal flexibility → rationalized coherence-maintenance. However, the structural data reveals this as a false summit: the adaptation is contingent on specific federal coercive mechanisms, institutional vulnerability, and theological commitments that could have taken different forms.
constraint_indexing:constraint_classification(divine_marriage_command__coercion_visibility_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_marriage_command__coercion_visibility_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(divine_marriage_command__coercion_visibility_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(divine_marriage_command__coercion_visibility_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_marriage_command__coercion_visibility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(divine_marriage_command__coercion_visibility_reading, TR),
    TR >= 0.70.

:- end_tests(divine_marriage_command__coercion_visibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Elevated. The constraint combines real institutional benefits from coercion-induced doctrinal change (survival, continued operation, maintained authority structure) with real costs borne by faithful members whose doctrinal commitments are reframed as suspended-but-not-rescinded. The 0.58 value reflects that this is not pure snare extraction — there is a genuine coordination function (the institution survives, enabling continued practice and community) — but the extraction is substantial because the institutional benefits (authority, continuity) flow asymmetrically. The measurement trajectory (0.35 → 0.50 → 0.58) shows extraction accumulating as the hermeneutic apparatus required to defend the reading against historical evidence becomes more elaborate. Suppression (0.68): High. Multiple suppression mechanisms maintain the reading: (a) exclusion of historical documentation showing federal pressure was decisive; (b) theological language framing coerced change as coherent with prior doctrine; (c) institutional penalties for openly questioning the Manifesto's theological legitimacy; (d) denial or reframing of the exogenous pressure claim in official narratives. The rising trajectory (0.52 → 0.62 → 0.68) reflects increasing suppression effort as historical record accumulates. Theater ratio (0.64): Elevated. The hermeneutic apparatus required to frame coerced doctrinal change as theologically legitimate is substantially performative — maintaining appearances of coherence when the actual causal mechanism (federal pressure) contradicts the theological framing (divine command remains valid, only suspended). The theater rises as the gap between official narrative and historical record becomes more visible.
 *
 * PERSPECTIVAL GAP:
 *   The richest perspectival gap lies between the institutional beneficiary (rope experience: coordination problem solved, survival secured) and the faithful polygamist victim (snare experience: trapped in theological limbo, original covenant reframed as suspended). The institutional apparatus experiences the constraint as enabling; the faithful experience it as betrayal. The doctrinal continuity faction occupies a middle position (tangled rope): they maintain hermeneutic flexibility but constrained by institutional loyalty. The analytical observer risks naturalizing the whole dynamic as an immutable law of institutional response to coercion, when in fact different theological frameworks and different institutional structures could have produced different responses. The covenant theology integrity perspective (snare) identifies the true victim: the abstract claim that divine commands are immutable is structurally exposed and cannot defend itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional survival apparatus: beneficiary status + arbitrage exit options → low directionality (d ≈ 0.15) → negative f(d) → low/negative chi. Federal enforcement apparatus: powerful/constrained (not full arbitrage) + enforcer role → moderate directionality (d ≈ 0.50) → moderate f(d) → moderate chi. Faithful polygamists: victim status + trapped exit → high directionality (d ≈ 0.92) → high f(d) → high chi. Doctrinal continuity faction: mixed (victims of coercion but beneficiaries of flexibility) + constrained exit → moderate-high directionality (d ≈ 0.60) → moderate-high f(d) → moderate chi. The derived d values flow from the structural relationship declarations (beneficiary/victim) plus exit options; they are not arbitrary. The engine computes f(d) and applies the scope modifier σ(S=national)=1.0 to produce effective extractiveness chi for each perspective. The beneficiary's negative chi makes the rope classification legitimate; the victim's high chi makes the snare classification legitimate; the analytical observer's high d (analytical power → canonical d ≈ 0.72) → high f(d) produces the high chi that supports the mountain false summit risk.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by showing that coercion-induced doctrinal change is genuinely a tangled rope: real coordination function (institutional survival) + real asymmetric extraction (doctrinal reframing benefits institutional apparatus, harms faithful members). The institutional beneficiary legitimately perceives rope (coordination). The victim legitimately perceives snare (extraction). The challenge is that the reading admits the coercion is operative, which creates a potential legitimacy crisis: if human coercion can modify divine commands, does the authority structure lose its divine warrant? This crisis is managed through the hermeneutic apparatus (theater), which maintains that the command remains valid even while suspended. The piton perspective identifies this theater as degrading functionality over time as historical evidence accumulates. The analytical observer's mountain perspective risks naturalizing the institutional adaptation as immutable, when in fact the constraint depends on specific federal coercive mechanisms and theological commitments that are historically contingent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coercion_validity_as_input,
    'If coercion is acknowledged as a valid input to doctrinal change, does this dissolve the divine legitimacy claim for the doctrine itself (both the original command and the Manifesto)?',
    'Theological analysis of whether divine commands can be suspended by external coercion without losing their claim to divine origin. Examination of how the institution resolves the contradiction between ''divine and immutable'' and ''suspended under pressure.''',
    'If coercion invalidates divine legitimacy: the reading forecloses the continuationist reading (both cannot hold within a single coherence frame). If coercion is compatible with divine legitimacy: the readings coexist (different factions adopt different hermeneutic frames). If the institution denies coercion was operative: the reading loses its structural foundation entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coercion_validity_as_input, conceptual, 'Whether acknowledged coercion is compatible with divine legitimacy claims').

omega_variable(
    institutional_survival_necessity_doctrine,
    'Can ''institutional survival necessity'' function as a legitimate theological ground for doctrinal change within this tradition''s own frameworks?',
    'Historical analysis of how the institution justified the Manifesto in contemporary theological language. Comparison with other doctrinal changes justified on institutional grounds vs. revelation grounds. Analysis of whether ''survival necessity'' is treated as analogous to revelation or as subordinate to it.',
    'If institutional necessity is treated as a theological ground: the reading is internally coherent (tangled rope). If institutional necessity is treated as merely prudential and non-theological: the reading reduces to coerced compliance disguised in theological language (snare). If institutional necessity is rejected as a theological ground: the reading is unstable and eventually rejected by the tradition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_survival_necessity_doctrine, conceptual, 'Whether institutional survival necessity is a valid theological ground for doctrinal change').

omega_variable(
    m_set_closure_mechanism,
    'How does the institution maintain coherence after closing the M-set gap (admitting exogenous pressure shaped the doctrine)? What prevents the coherence-maintenance mechanism itself from becoming visible as theater?',
    'Longitudinal analysis of how doctrinal explanations evolved post-Manifesto. Analysis of whether hermeneutic flexibility expanded to accommodate other doctrinal changes, or whether this case remained isolated. Study of whether historical documentation of coercion visibility led to institutional legitimacy crisis.',
    'If M-set closure remains stable: the constraint maintains its tangled-rope structure indefinitely. If M-set closure degrades: the theater becomes visible (piton status), potentially leading to reclassification. If institutional authority is threatened: the constraint shifts toward snare (suppression mechanisms intensify to defend doctrinal coherence).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(m_set_closure_mechanism, empirical, 'Stability of coherence-maintenance after M-set gap closure').

omega_variable(
    false_summit_coercion_law,
    'Is the analytical ''natural law'' perspective (institutional coercion → doctrinal flexibility) actually a contingent institutional arrangement naturalized as immutable? Or is it a genuine structural law of how institutions under pressure respond?',
    'Comparative institutional history: analysis of how other institutions under similar coercive pressure (other religious bodies, professional associations, state apparatuses) resolved parallel doctrinal contradictions. Identification of cases where institutions maintained doctrinal immutability despite severe coercion.',
    'If true natural law: the mountain classification holds. If contingent arrangement: the mountain is a false summit, and the constraint should reclassify to tangled_rope or snare depending on beneficiary visibility and suppression level.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(false_summit_coercion_law, empirical, 'Whether institutional coercion-induced doctrinal flexibility is natural law or contingent arrangement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__coercion_visibility_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dmc_coercion_theater_t0, divine_marriage_command__coercion_visibility_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(dmc_coercion_theater_t5, divine_marriage_command__coercion_visibility_reading, theater_ratio, 5, 0.58).
narrative_ontology:measurement(dmc_coercion_theater_t10, divine_marriage_command__coercion_visibility_reading, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(dmc_coercion_extract_t0, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(dmc_coercion_extract_t5, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(dmc_coercion_extract_t10, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(dmc_coercion_suppress_t0, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(dmc_coercion_suppress_t5, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(dmc_coercion_suppress_t10, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_marriage_command__coercion_visibility_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(divine_marriage_command__coercion_visibility_reading, divine_marriage_command__continuationist_reading).
narrative_ontology:affects_constraint(divine_marriage_command__coercion_visibility_reading, divine_marriage_command__substitutionist_reading).
narrative_ontology:affects_constraint(divine_marriage_command__coercion_visibility_reading, federal_religious_coercion_apparatus).
narrative_ontology:affects_constraint(divine_marriage_command__coercion_visibility_reading, nineteenth_century_marriage_doctrine).

% DUAL FORMULATION NOTE:
% The divine_marriage_command is a contested kernel with three structural readings instantiated as three separate constraint stories. This reading (coercion_visibility) differs from continuationist (denies coercion was decisive) and substitutionist (claims new revelation) in its admission of exogenous pressure and grounding in institutional survival necessity rather than revelation. Each reading has its own ε value, its own perspectives, and its own beneficiary/victim structure. The three stories are linked via network.affects_constraints and clarified through cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(divine_marriage_command__coercion_visibility_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
