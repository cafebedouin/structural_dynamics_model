% ============================================================================
% CONSTRAINT STORY: composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_composite_overdetermination_reading, []).

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
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: composite_overdetermination_reading
 *   human_readable: Vatican II as Composite Overdetermination: Incompatible Doctrinal Shifts and Structural Ambiguity
 *   domain: theology/ecclesiology/religious_authority
 *
 * SUMMARY:
 *   Vatican II (1962–1965) is presented in institutional magisterial
 *   discourse as a unified, coherent ecumenical council that clarified
 *   doctrine and renewed the Church's engagement with modernity. This reading
 *   contests that narrative. Vatican II was produced by factional compromise
 *   between progressive and conservative blocs that embedded incompatible
 *   theological rationales into the Council's texts. The documents claim to
 *   resolve ancient tensions (tradition vs. development, papal authority vs.
 *   episcopal collegiality, Latin liturgy vs. vernacular worship, Church
 *   authority vs. religious freedom) but actually leave them ambiguous — not
 *   because the language is unclear, but because the Council refused to
 *   choose between fundamentally different theological visions.
 *   Post-conciliar history is structured by attempts to interpret the Council
 *   in directions the texts simultaneously support and forbid.
 *   Traditionalists cite Vatican II to justify preserving pre-conciliar
 *   liturgy and doctrine; progressives cite the same Council to justify
 *   radical renewal. Rome claims authoritative interpretive power while the
 *   documents resist univocal reading. This constraint is not a problem to be
 *   solved (either by choosing continuity or rupture) but a structural
 *   feature of how Church authority sustains legitimacy through controlled
 *   ambiguity.
 *
 * KEY AGENTS:
 *   - Local parish priests: Primary victims (powerless/trapped) — must implement contradictory directives with no escape
 *   - Institutional magisterium (Rome): Primary beneficiary (institutional/arbitrage) — concentrates interpretive authority through gatekeeping of Council meaning
 *   - National bishops' conferences: Secondary victims/beneficiaries (moderate/constrained) — constrained by Rome but also benefit from flexibility to apply Council selectively
 *   - Theological scholarship: Secondary beneficiary (analytical/analytical) — creates permanent research agenda from the overdetermination
 *   - Post-conciliar reform movements: Organized beneficiaries (organized/mobile) — extract doctrinal authority to enable renewal, benefit from Council's progressive texts
 *   - Traditionalist resistance: Organized victims (organized/constrained) — extract alternative authority to preserve pre-conciliar forms, bear extraction from progressive implementation
 *   - Postconciliar ecclesial community: Distributed victim (moderate/constrained) — lives within irreducible ambiguity without resolution capacity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(composite_overdetermination_reading, 0.58).
domain_priors:suppression_score(composite_overdetermination_reading, 0.52).
domain_priors:theater_ratio(composite_overdetermination_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(composite_overdetermination_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(composite_overdetermination_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(composite_overdetermination_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(composite_overdetermination_reading, "Vatican II as Composite Overdetermination: Incompatible Doctrinal Shifts and Structural Ambiguity").
narrative_ontology:topic_domain(composite_overdetermination_reading, "theology/ecclesiology/religious_authority").

domain_priors:requires_active_enforcement(composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(composite_overdetermination_reading, '39f7c226-6f3c-4b5e-9d8f-6e2a22dd5703').
narrative_ontology:cs_created_at('39f7c226-6f3c-4b5e-9d8f-6e2a22dd5703', '').
narrative_ontology:cs_kernel_codification('39f7c226-6f3c-4b5e-9d8f-6e2a22dd5703', formalized).
narrative_ontology:cs_authority_grounding('39f7c226-6f3c-4b5e-9d8f-6e2a22dd5703', extraction).
narrative_ontology:cs_interpretation_layer_present('39f7c226-6f3c-4b5e-9d8f-6e2a22dd5703').
narrative_ontology:cs_kernel_id(composite_overdetermination_reading, vatican_ii_authority).
narrative_ontology:cs_reading_relation('39f7c226-6f3c-4b5e-9d8f-6e2a22dd5703', vatican_ii_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('39f7c226-6f3c-4b5e-9d8f-6e2a22dd5703', vatican_ii_rupture_reading, forecloses).
narrative_ontology:cs_axiom('39f7c226-6f3c-4b5e-9d8f-6e2a22dd5703', foundational, council_texts_encode_factional_compromise).
narrative_ontology:cs_axiom_status(council_texts_encode_factional_compromise, holdable).
narrative_ontology:cs_axiom('39f7c226-6f3c-4b5e-9d8f-6e2a22dd5703', foundational, ambiguity_is_structural_not_resolvable).
narrative_ontology:cs_axiom_status(ambiguity_is_structural_not_resolvable, holdable).
narrative_ontology:cs_reference_frame('39f7c226-6f3c-4b5e-9d8f-6e2a22dd5703', univocal_council_authority).
narrative_ontology:cs_drift_state('39f7c226-6f3c-4b5e-9d8f-6e2a22dd5703', contemporary_recognition_of_overdetermination, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(composite_overdetermination_reading, theological_scholarship_recognizing_complexity).
narrative_ontology:constraint_beneficiary(composite_overdetermination_reading, council_minority_factions_preserving_alternative_framings).
narrative_ontology:constraint_victim(composite_overdetermination_reading, institutional_magisterium_authority_claims).
narrative_ontology:constraint_victim(composite_overdetermination_reading, postconciliar_ecclesial_unity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOCAL PARISH PRIEST (SNARE) — Trapped between incompatible directives: traditional Mass form vs. vernacular liturgy, authority of local bishop vs. Rome, modernized catechesis vs. doctrinal continuity. No exit — the priest must live within the ambiguity without resolving it. Maximum experienced extraction because the institutional contradictions are structural and unavoidable within the bounded parish context.
constraint_indexing:constraint_classification(composite_overdetermination_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NATIONAL BISHOPS' CONFERENCE (TANGLED ROPE) — Constrained by Vatican oversight and divided internal factions, but also benefits from the ambiguity: can implement Vatican II selectively to accommodate regional constituencies. The overdetermination creates coordination problems (who speaks authoritatively?) but also provides flexibility in application. Mixed extraction and coordination — not all costs, not all benefits.
constraint_indexing:constraint_classification(composite_overdetermination_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ROMAN CURIA (ROPE) — Experiences the constraint as coordination mechanism: managing post-conciliar implementation requires interpreting ambiguous texts, which concentrates interpretive authority in Rome. The overdetermination actually strengthens Rome's gatekeeping function — disputes about Council interpretation flow to the Curia for settlement. Net beneficiary of the constraint's ambiguity, though cast as neutral administrator.
constraint_indexing:constraint_classification(composite_overdetermination_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: POST-CONCILIAR REFORM MOVEMENT (TANGLED ROPE) — Organized agents (progressive bishops, theologians, liturgists) see the constraint as enabling genuine doctrinal development and pastoral renewal, while bearing extraction costs in resistance from traditionalists and Rome's conservative re-interpretation. Mobile exit option (can leave the institutional church or work within secular academia) moderates the extraction experienced, but the organized character gives them agency to shape interpretation.
constraint_indexing:constraint_classification(composite_overdetermination_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: TRADITIONALIST RESISTANCE (TANGLED ROPE) — Organized counter-movement (Lefebvre, FSSX, traditionalist societies) sees the constraint as an extractive violation of doctrinal continuity. Extraction: loss of pre-conciliar liturgy, doctrine, ecclesiology. Coordination: they are coordinating around alternative interpretive authority to preserve continuity. Constrained exit (schism risks excommunication and institutional status loss) balances against coordination benefits of maintaining traditionalist unity.
constraint_indexing:constraint_classification(composite_overdetermination_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: THEOLOGICAL SCHOLARSHIP (ROPE) — Academic theology benefits from the overdetermination: it creates a permanent research agenda (interpreting the Council, resolving contradictions, tracing theological genealogies). Scholarly extraction is minimal because researchers can investigate freely; the ambiguity is the resource. Pure coordination of interpretation — the scholarly community sees the constraint as enabling theological work rather than constraining it.
constraint_indexing:constraint_classification(composite_overdetermination_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: CONCILIAR DOCUMENT APPARATUS (PITON) — The official Vatican II texts and their commentary apparatus are largely performative: they claim to resolve what they actually leave ambiguous. The apparatus maintains its authority through ritual invocation ('the Council teaches...') while actual interpretation remains contested. Theater ratio high because the documents cannot settle the disputes they appear to adjudicate — the ritual of citing Vatican II masks fundamental incompleteness.
constraint_indexing:constraint_classification(composite_overdetermination_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: UNIVOCAL INTERPRETATION (MOUNTAIN — FALSE SUMMIT CANDIDATE) — The institutional magisterium and some conservative theologians treat Vatican II as a single, interpretable event with univocal meaning — the constraint appears as natural law of language: texts have determinate meanings that authorities can discern and teach. However, the structural data contradicts this: the Council itself was produced by factional compromise; the texts encode incompatible theological rationales; competing interpretations are not errors but legitimate readings of ambiguous documents. The engine's false summit detector flags this as naturalization of what is actually a contested, constructed constraint.
constraint_indexing:constraint_classification(composite_overdetermination_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(composite_overdetermination_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(composite_overdetermination_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(composite_overdetermination_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(composite_overdetermination_reading, TR),
    TR >= 0.70.

:- end_tests(composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-to-high. The Council's ambiguous directives create asymmetric extraction: Rome extracts interpretive authority by remaining as arbiter of Council meaning; progressive movements extract doctrinal permission for renewal; traditionalists extract alternative authority to preserve continuity; parish-level actors extract nothing but bear all costs. The value increased from 0.35 (immediately post-Council, when ambiguity was not yet recognized as structural) to 0.58 (contemporary, when decades of unresolved disputes reveal the overdetermination cannot be disambiguated by interpretation). Suppression (0.52): Moderate. Significant barriers exist to explicitly rejecting Vatican II or its authority — the Council is binding magisterial act. But significant space exists for alternative interpretations, traditional practice preservation, and organized dissent. Suppression is structural (cannot exit Church authority) but not total (can work within interpretation). Theater ratio (0.68): Moderate-to-high. The official narrative of Vatican II as a coherent, univocal event is performative — Council scholarship, magisterial pronouncements, and institutional communications maintain the fiction of unified meaning while reality is factionally contested. The theater has increased over 60 years as the gap between claimed univocity and actual interpretive diversity has widened. The Conciliar documents themselves are performative: they claim to settle what they leave ambiguous.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence on the same structural data. Rome sees Rope — coordination requiring interpretive authority. Progressive movements see Tangled Rope — coordination toward renewal amid traditionalist resistance. Traditionalists see Tangled Rope — coordination toward preservation amid progressive implementation. Local priests see Snare — impossible contradictions with no resolution. Theological scholarship sees Rope — intellectual opportunity. The analytical observer sees a Piton (performative Council apparatus) or false-summit Mountain (univocal-interpretation narrative). No perspective sees the constraint the same way because the beneficiary/victim profile differs radically by structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values reflect agents' structural relationships to the extraction flow. The magisterium benefits from ambiguity (low d, beneficiary position). Local priests are trapped victims (high d). Progressive movements have some exit capacity (mobile) and benefit from progressive texts (moderate d). Traditionalists have organized capacity but constrained exit (schism costs), bearing extraction from implementation (elevated d but moderated by organization). Theological scholarship has highest exit capacity (analytical) and benefits from research opportunity (low d, beneficiary position). The parish community's d approaches 1.0 (trapped, no exit, bearing full cost of institutional contradictions). The engine computes effective extractiveness χ = ε × f(d) × σ(S) across these positions, scaling by global scope (σ = 1.2) to reflect how Vatican II's ambiguity affects the entire worldwide Church.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves the mandatrophy by showing that Vatican II cannot be univocally classified because it is not a single coherent event but a composite of incompatible doctrinal shifts. The question is not 'which type is Vatican II?' but 'which factional interpretation are you measuring?' Measured from Rome's perspective (beneficiary with interpretive authority), it approaches Rope. Measured from traditionalist resistance (organized victim with constrained exit), it is Tangled Rope. Measured from parish-level implementation (powerless trapped agent), it is Snare. The false-summit candidate (Mountain view of univocal authority) is revealed as naturalization of a contingent institutional narrative. The constraint's existence depends on maintaining the fiction that incompatible directives are actually coherent — the performative work of the institutional Church. This reading forecloses the univocal-continuity interpretation and the univocal-rupture interpretation by showing that both assume a single Council when the reality is a factionally compromised document producing incompatible obligations.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    factional_compromise_vs_doctrinal_development,
    'Are Vatican II''s textual ambiguities the result of genuine doctrinal indeterminacy and legitimate theological development, or artifacts of unresolved factional compromise that should (in principle) be disambiguated?',
    'Historical analysis of Council debates, voting records, and text-redaction processes; comparison of initial schemata vs. final documents to identify compromise passages; theological analysis of whether the ambiguous formulations enable genuine development or merely defer resolution',
    'If doctrinal development: the constraint is a Rope (coordination enabling growth). If factional compromise: the constraint is a Snare (structural inability to resolve contradictions). If both: the constraint is a Tangled Rope (development discourse covers compromise reality).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(factional_compromise_vs_doctrinal_development, empirical, 'Whether Vatican II ambiguities encode genuine development or factional compromise').

omega_variable(
    interpretive_authority_legitimacy,
    'Does the magisterium have hermeneutical authority to settle Vatican II''s ambiguities, or is interpretive authority permanently distributed across schools, bishops, theologians, and the faithful?',
    'Doctrinal analysis of who in Catholic ecclesiology can authoritatively interpret Council texts; examination of post-conciliar magisterial attempts to settle disputed interpretations and their acceptance/rejection in the broader church; comparison to theological precedents (e.g., how Trent''s ambiguities were resolved)',
    'If magisterium has univocal authority: the constraint approaches Mountain (natural law of ecclesiastical hierarchy). If authority is distributed: the constraint is embedded in Snare or Tangled Rope (extraction from those trapped in alternative interpretations). If authority is genuinely contested: the constraint is irreducibly ambiguous.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_authority_legitimacy, conceptual, 'Whether interpretive authority is concentrated or distributed').

omega_variable(
    postconciliar_conflict_necessity,
    'Are post-conciliar ecclesial conflicts (traditionalist schism, liturgical wars, doctrinal disputes) failures to implement the Council properly, or structural necessities produced by incompatible directives?',
    'Counterfactual analysis: could different management or interpretation have prevented schism and conflict? Comparative case study with prior councils whose ambiguities were resolved without major schism (or weren''t resolved and produced permanent splits). Analysis of whether traditionalist and progressive readings can both legitimately claim Council support.',
    'If conflicts are contingent failures: the constraint is a Scaffold with poor management (temporary, solvable). If structural necessities: the constraint is a Snare (conflicts are unavoidable extractions from the overdetermination). If both: the constraint is a Tangled Rope (coordination amid genuine structural contradictions).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(postconciliar_conflict_necessity, empirical, 'Whether post-conciliar conflicts are contingent or structural').

omega_variable(
    reading_incompatibility_vs_perspectival_plurality,
    'Are the continuity reading, rupture reading, and composite overdetermination reading genuinely incompatible (only one can be true), or are they legitimate different perspectives on the same ambiguous reality?',
    'Logical analysis of whether each reading''s core claim rules out the others'' core claims; examination of whether an intelligent, well-informed interpreter could hold multiple readings simultaneously; comparison to theological precedents for holding tension between apparently contradictory doctrines',
    'If genuinely incompatible: one reading forecloses the others (this reading forecloses univocal continuity or rupture). If perspectival: all readings coexist (this reading coexists with continuity and rupture readings). This determines the cs_structure.reading_relations values.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_incompatibility_vs_perspectival_plurality, conceptual, 'Whether Vatican II readings are incompatible or perspectival').

omega_variable(
    institutional_authority_preservation,
    'What material or legitimacy interests does the institutional magisterium preserve by maintaining the overdetermination ambiguity rather than attempting to resolve it?',
    'Analysis of magisterial statements attempting to settle Vatican II interpretations and their reception; observation of how Rome uses ambiguity to maintain flexibility in doctrine; comparison of what would be lost if Vatican II were interpreted univocally in either continuity or rupture direction',
    'If Rome benefits from ambiguity: the constraint is a Tangled Rope with Rome as beneficiary extracting through gatekeeping authority. If Rome would benefit from clarity: Rome is also trapped in the overdetermination (shared Snare). This determines whether Rome experiences the constraint as Rope (beneficiary) or Tangled Rope (mixed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_authority_preservation, empirical, 'Whether institutional magisterium benefits from or suffers under ambiguity').

omega_variable(
    composite_vs_single_constraint,
    'Is Vatican II best modeled as a single overdetermined constraint, or as a constraint family of multiple distinct doctrinal constraints (liturgy, ecclesiology, ecumenism, authority) each with different ε values and beneficiary/victim profiles?',
    'Comparative structural analysis: do liturgical ambiguities map onto the same beneficiaries/victims as ecumenical ambiguities? Do they respond to the same measures? Do they resolve on the same timescale? Application of ε-invariance test: if measuring different domains produces dramatically different ε values, decompose.',
    'If single constraint: present story is correct. If constraint family: decompose into separate stories (liturgical coordination, ecclesiological authority, ecumenical relations, doctrinal development) linked via network.affects_constraints. This determines whether to maintain current structure or split.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(composite_vs_single_constraint, empirical, 'Whether Vatican II is one constraint or a constraint family').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(composite_overdetermination_reading, 1965, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_over_tr_t0, composite_overdetermination_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(comp_over_tr_t5, composite_overdetermination_reading, theater_ratio, 5, 0.55).
narrative_ontology:measurement(comp_over_tr_t10, composite_overdetermination_reading, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(comp_over_be_t0, composite_overdetermination_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(comp_over_be_t5, composite_overdetermination_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(comp_over_be_t10, composite_overdetermination_reading, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(composite_overdetermination_reading, identity_coordination).
narrative_ontology:affects_constraint(composite_overdetermination_reading, vatican_ii_continuity_reading).
narrative_ontology:affects_constraint(composite_overdetermination_reading, vatican_ii_rupture_reading).
narrative_ontology:affects_constraint(composite_overdetermination_reading, liturgical_traditionalism_constraint).
narrative_ontology:affects_constraint(composite_overdetermination_reading, episcopal_collegiality_implementation).
narrative_ontology:affects_constraint(composite_overdetermination_reading, ecumenical_authority_asymmetry).

% DUAL FORMULATION NOTE:
% The composite overdetermination reading is one of three kernel readings of Vatican II authority. The continuity reading (emphasizing doctrinal consistency across pre- and post-conciliar teaching) and the rupture reading (emphasizing genuine doctrinal development and change) are structurally distinct constraints that should be authored as separate stories. This reading differs by asserting that neither continuity nor rupture framework adequately captures the overdetermined structure — the Council is not a coherent event with a univocal meaning but a factionally compromised text encoding incompatible theological rationales. All three readings link via network.affects_constraints to show they are sibling interpretations of the same contested kernel, not competing descriptions of objective reality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(composite_overdetermination_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
