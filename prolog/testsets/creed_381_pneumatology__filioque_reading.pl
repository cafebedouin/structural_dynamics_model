% ============================================================================
% CONSTRAINT STORY: creed_381_pneumatology__filioque_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_creed_381_pneumatology__filioque_reading, []).

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
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: creed_381_pneumatology__filioque_reading
 *   human_readable: Filioque Doctrine and Papal Magisterial Authority (Tangled Rope Reading)
 *   domain: historical_theology/ecclesiastical_authority
 *
 * SUMMARY:
 *   The Filioque doctrine — that the Holy Spirit proceeds from Father and Son
 *   jointly — represents a critical juncture in Western Christological
 *   theology and ecclesiastical authority. Originating as a Spanish/Frankish
 *   theological clarification in the 6th century, the Filioque was
 *   unilaterally inserted into the Western Creed by the Latin church without
 *   Eastern conciliar participation. By the 11th century, the divergence had
 *   crystallized into a fundamental schism over both theology and
 *   ecclesiology: the East denied the Filioque's orthodoxy; the West claimed
 *   papal authority to define trinitarian doctrine authoritatively. This
 *   constraint instantiates a specific reading of the disputed kernel
 *   (Niceno-Constantinopolitan Creed on pneumatology): that papal/conciliar
 *   magisterium possesses legitimate authority to clarify implicit
 *   trinitarian doctrine, even against Eastern objections. The constraint
 *   exhibits extraction (asymmetric doctrinal authority), coordination
 *   (Western theological unity), and suppression (enforcement of Filioque
 *   acceptance in Western liturgy and education). The measurement trajectory
 *   shows extractiveness rising from modest theological disagreement (589) to
 *   acute schismatic rupture (1054) to institutionalized magisterial
 *   assertion (1215 Fourth Lateran Council), then declining slightly (1965
 *   Vatican II ecumenical gestures) as the West acknowledged the Filioque as
 *   a legitimate but not essential development.
 *
 * KEY AGENTS:
 *   - Papal See / Roman Magisterium: Primary beneficiary (institutional/arbitrage) — exercises authority to define doctrine; consolidates Western theological jurisdiction
 *   - Eastern Orthodox Hierarchy: Primary victim (powerless/trapped initially, moderate/constrained post-Schism) — loses voice in trinitarian doctrine; forced to choose between capitulation and schism
 *   - Western Episcopate: Secondary actor (moderate/constrained) — enforces Filioque in liturgy and education; benefits from doctrinal clarity but constrained by papal authority
 *   - Ecumenical Movement: Organized agent (organized/constrained) — negotiates Filioque's theological status; seeks reunion by reframing doctrine as negotiable development
 *   - Western Faithful: Distributed agents (powerless/trapped) — absorb Filioque in liturgical creed without theological choice; constrained by institutional religious formation
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing a contingent Western doctrinal choice as immutable trinitarian logic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(creed_381_pneumatology__filioque_reading, 0.58).
domain_priors:suppression_score(creed_381_pneumatology__filioque_reading, 0.65).
domain_priors:theater_ratio(creed_381_pneumatology__filioque_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(creed_381_pneumatology__filioque_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(creed_381_pneumatology__filioque_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(creed_381_pneumatology__filioque_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(creed_381_pneumatology__filioque_reading, tangled_rope).
narrative_ontology:human_readable(creed_381_pneumatology__filioque_reading, "Filioque Doctrine and Papal Magisterial Authority (Tangled Rope Reading)").
narrative_ontology:topic_domain(creed_381_pneumatology__filioque_reading, "historical_theology/ecclesiastical_authority").

domain_priors:requires_active_enforcement(creed_381_pneumatology__filioque_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(creed_381_pneumatology__filioque_reading, '8bda4b2d-4c54-4bf6-a212-5b7ddc85a818').
narrative_ontology:cs_kernel_codification('8bda4b2d-4c54-4bf6-a212-5b7ddc85a818', fixed_text).
narrative_ontology:cs_authority_grounding('8bda4b2d-4c54-4bf6-a212-5b7ddc85a818', lineage).
narrative_ontology:cs_interpretation_layer_present('8bda4b2d-4c54-4bf6-a212-5b7ddc85a818').
narrative_ontology:cs_reading_relation('8bda4b2d-4c54-4bf6-a212-5b7ddc85a818', creed_381_pneumatology__monoprocession_reading, forecloses).
narrative_ontology:cs_reading_relation('8bda4b2d-4c54-4bf6-a212-5b7ddc85a818', creed_381_pneumatology__ecumenical_reunion_reading, influences).
narrative_ontology:cs_axiom('8bda4b2d-4c54-4bf6-a212-5b7ddc85a818', foundational, papal_magisterium_trinitarian_clarification_authority).
narrative_ontology:cs_axiom_status(papal_magisterium_trinitarian_clarification_authority, holdable).
narrative_ontology:cs_axiom_grounding('8bda4b2d-4c54-4bf6-a212-5b7ddc85a818', papal_magisterium_trinitarian_clarification_authority, conventional).
narrative_ontology:cs_axiom('8bda4b2d-4c54-4bf6-a212-5b7ddc85a818', foundational, filioque_orthodox_trinitarian_development).
narrative_ontology:cs_axiom_status(filioque_orthodox_trinitarian_development, holdable).
narrative_ontology:cs_axiom_grounding('8bda4b2d-4c54-4bf6-a212-5b7ddc85a818', filioque_orthodox_trinitarian_development, deontological).
narrative_ontology:cs_reference_frame('8bda4b2d-4c54-4bf6-a212-5b7ddc85a818', papal_magisterial_trinitarian_authority).
narrative_ontology:cs_created_at('8bda4b2d-4c54-4bf6-a212-5b7ddc85a818', '').
narrative_ontology:cs_kernel_id(creed_381_pneumatology__filioque_reading, creed_381_pneumatology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(creed_381_pneumatology__filioque_reading, papal_see).
narrative_ontology:constraint_beneficiary(creed_381_pneumatology__filioque_reading, latin_christendom).
narrative_ontology:constraint_victim(creed_381_pneumatology__filioque_reading, eastern_orthodox_autonomy).
narrative_ontology:constraint_victim(creed_381_pneumatology__filioque_reading, ecumenical_unity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EASTERN ORTHODOX THEOLOGIAN (SNARE) — Trapped within the ecclesiastical order. The unilateral insertion of Filioque into the Western Creed, then declared dogma through papal authority, forecloses theological autonomy without Eastern participation. Cannot exit the constraint (schism is already the result) or reconcile without doctrinal capitulation. Bears full cost of doctrinal subordination to Western magisterium. Maximum extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(creed_381_pneumatology__filioque_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: LOCAL WESTERN BISHOP (TANGLED ROPE) — Constrained by papal magisterial authority but also benefits from doctrinal clarity and institutional stability that centralized authority provides. Must enforce the Filioque in liturgy and doctrine (suppression), yet gains coordination benefit from unified Western theology. Exit is costly (removal, excommunication) but theoretically possible. Mixed extraction and coordination.
constraint_indexing:constraint_classification(creed_381_pneumatology__filioque_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: PAPAL MAGISTERIUM (ROPE) — Benefits directly from the authority to clarify and define doctrine. The Filioque decision consolidates Western theological unity under Roman jurisdiction and demonstrates papal power to authoritatively interpret Scripture and Tradition. Experiences the constraint as coordination mechanism: it solves the West's internal doctrinal question by settling the Pneumatology with papal fiat. Arbitrage position: can exit from other constraints, benefits maximally here.
constraint_indexing:constraint_classification(creed_381_pneumatology__filioque_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: ECUMENICAL REUNION MOVEMENT (SCAFFOLD) — Sees the Filioque constraint as a surmountable doctrinal barrier. Post-Vatican II ecumenical efforts treat the Filioque as a negotiable clarification rather than an irrevocable dogma — many Latin theologians acknowledge it as a legitimate theological development but not essential to trinitarian orthodoxy. The constraint has a sunset: if reunion is achieved, the Filioque's coercive function dissolves. Organized agents see exit path and agency.
constraint_indexing:constraint_classification(creed_381_pneumatology__filioque_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: WESTERN LITURGICAL INSTITUTION (PITON) — The Filioque persists in Western creeds and liturgy primarily through institutional inertia. The theological function — clarifying Spirit's procession — is minimal; the constraint's primary function is now symbolic maintenance of doctrinal continuity. Theater ratio is moderate because the Filioque appears functionally in liturgy, but its theological necessity is largely ceremonial. The liturgical system maintains the constraint because it always has, not because active enforcement is currently required.
constraint_indexing:constraint_classification(creed_381_pneumatology__filioque_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From an extreme analytical distance, the Filioque appears as an immutable logical necessity: if the Son fully participates in the divine nature (orthodox trinitarian commitment), then the Spirit's procession must involve the Son necessarily — this is a theological logical law, not a contingent doctrinal choice. However, this naturalizes a contested theological claim as mathematical. The engine will flag this as a false summit: the constraint is actually a contingent outcome of medieval Western Christology, not a logical necessity.
constraint_indexing:constraint_classification(creed_381_pneumatology__filioque_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(creed_381_pneumatology__filioque_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(creed_381_pneumatology__filioque_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(creed_381_pneumatology__filioque_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(creed_381_pneumatology__filioque_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(creed_381_pneumatology__filioque_reading, TR),
    TR >= 0.70.

:- end_tests(creed_381_pneumatology__filioque_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The Filioque constraint extracts significant doctrinal authority from the East and consolidates it in the Roman see. The extraction is not maximal (0.70+) because the constraint also provides genuine coordination benefit to Western Christianity — it does settle a real theological question (whether the Spirit proceeds from Father alone or from Father and Son) with institutional clarity. The measurement trajectory shows extractiveness rising during the medieval period as papal authority strengthens, then declining in the modern ecumenical era as the West acknowledges the Filioque as a theological development rather than an irrevocable definition. Suppression (0.65): Moderate-high. The constraint enforces compliance through multiple mechanisms: liturgical repetition of the Filioque-bearing creed, theological education mandating acceptance, excommunication threats against dissidents, and institutional control over doctrine. Suppression is not maximal because there is legitimate theological debate even within Western tradition about the Filioque's necessity. Theater ratio (0.48): Moderate. The Filioque serves both functional (theological clarification) and ceremonial (liturgical creedal recitation) roles. The theater is not high (0.70+) because the constraint does address a real theological question about pneumatology; it is not minimal (0.20-) because much of the enforcement is now symbolic continuation rather than active theological necessity.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates why indexical classification is essential for understanding doctrinal disputes. The papal magisterium sees coordination and doctrinal clarity (Rope). The Eastern church sees unilateral authority imposition (Snare). The ecumenical movement sees a surmountable barrier (Scaffold). The local bishop sees mixed enforcement and benefit (Tangled Rope). The analytical observer risks seeing logical necessity (Mountain) when examining the structure of trinitarian theology. Each perspective is structurally coherent — the gap reveals not error but the reality that this constraint serves radically different functions for different actors. The false summit detection is critical: the 'logical necessity' framing (Mountain) naturalizes what is actually a contingent outcome of Western ecclesial authority consolidation.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural relationship to the constraint. The papal see benefits directly (d ≈ 0.05, beneficiary + arbitrage → low d → negative χ, experienced as Rope). The Eastern Orthodox bear costs without benefits (d ≈ 0.95, victim + trapped initially, then constrained post-Schism → high d → high χ, experienced as Snare then Tangled Rope). Western bishops are constrained beneficiaries — they gain doctrinal clarity but must enforce compliance (d ≈ 0.50-0.60, beneficiary + constrained → moderate d → moderate χ, experienced as Tangled Rope). The ecumenical movement has agency and exit paths (d ≈ 0.40, organized + constrained → moderate d, experienced as Scaffold with lower χ). The analysis applies the sigmoid f(d) to map directionality to experienced extractiveness.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy of the Filioque is resolved by recognizing it as a contested authority claim about doctrine, not a settled empirical or logical fact. The 'mandatrophy' is not 'is the Filioque true?' but 'who has authority to clarify trinitarian doctrine?' This reading claims the papal magisterium has that authority; the monoprocession reading claims it does not (or that Eastern conciliar authority is equally valid); the reunion reading claims both claims can coexist under unified ecclesiology. The constraint's classification as Tangled Rope rather than pure Rope (coordination) or pure Snare (extraction) reflects that the Filioque does solve a real theological question (coordination function) while simultaneously imposing unilateral authority (extraction function). The presence of both functions is what makes Tangled Rope the correct classification — the constraint cannot be reduced to pure coordination (because it subordinates Eastern theology) or pure extraction (because it genuinely addresses theological ambiguity). The false summit (Mountain) is the risk: treating the Filioque as a logical necessity of trinitarian orthodoxy rather than as a contingent Western development that benefits papal authority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    filioque_logical_necessity_vs_theological_contingency,
    'Is the Filioque a logical consequence of trinitarian orthodoxy or a contingent Western theological development?',
    'Historical analysis of pre-Filioque trinitarian doctrine in Eastern Christianity; examination of whether Eastern formula (monoprocession) satisfies the same logical constraints; contemporary philosophical trinitarian theology assessing necessity.',
    'If logical necessity: mountain classification confirmed; Eastern rejection becomes logically incoherent. If contingent: constraint is tangled_rope (mixed extraction and coordination); false summit detected; Eastern autonomy in theology is legitimate alternative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(filioque_logical_necessity_vs_theological_contingency, conceptual, 'Whether Filioque is logically necessary or theologically contingent').

omega_variable(
    magisterial_authority_scope_ambiguity,
    'Does papal magisterium possess authority to unilaterally reformulate ecumenical creeds without Eastern conciliar participation?',
    'Canonical historical analysis of ecumenical council procedures; examination of East-West ecclesiology regarding authority distribution; post-Schism papal claims vs. pre-Schism conciliar precedent.',
    'If yes: papal authority is supreme; constraint classification as tangled_rope (with papal arbitrage) confirmed. If no: constraint becomes snare (unilateral imposition) or represents authority transgression; mandatrophy intensifies.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(magisterial_authority_scope_ambiguity, conceptual, 'Scope of papal magisterial authority over ecumenical doctrine').

omega_variable(
    ecumenical_reunion_feasibility,
    'Can the Filioque be de-emphasized or reframed sufficiently to permit reunion without doctrinal capitulation by either party?',
    'Post-Vatican II ecumenical dialogue outcomes; examination of proposed theological compromises (e.g., treating Filioque as legitimate development but not essential); empirical test: whether reunion agreements treat Filioque as negotiable.',
    'If feasible: scaffold perspective confirmed; constraint has real sunset. If not feasible: scaffold is aspirational; constraint remains indefinitely as snare or tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecumenical_reunion_feasibility, empirical, 'Whether reunion is feasible despite Filioque divergence').

omega_variable(
    filioque_reading_vs_monoprocession_reading_foreclosure,
    'Does this reading''s core axiom (papal authority to clarify trinitarian doctrine) logically foreclose the Eastern monoprocession reading, or do they coexist as live options?',
    'Formal logical analysis: Can both ''papal magisterium is authoritative for trinitarian doctrine'' and ''Eastern conciliar authority is equally valid for trinitarian doctrine'' be held within a single ecclesiology? Or do they contradict?',
    'If forecloses: the readings are mutually exclusive; reunion requires one reading to be abandoned. If coexist: both readings can persist under different jurisdictional frameworks (Pope in West, Patriarch in East); reunion requires no doctrinal surrender.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(filioque_reading_vs_monoprocession_reading_foreclosure, conceptual, 'Whether Filioque and monoprocession readings logically foreclose each other').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(creed_381_pneumatology__filioque_reading, 589, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(filioque_theater_t589, creed_381_pneumatology__filioque_reading, theater_ratio, 589, 0.25).
narrative_ontology:measurement(filioque_theater_t1054, creed_381_pneumatology__filioque_reading, theater_ratio, 1054, 0.42).

% Extraction over time
narrative_ontology:measurement(filioque_extractiveness_t589_first_insertion, creed_381_pneumatology__filioque_reading, base_extractiveness, 589, 0.15).
narrative_ontology:measurement(filioque_extractiveness_t1054_great_schism, creed_381_pneumatology__filioque_reading, base_extractiveness, 1054, 0.42).
narrative_ontology:measurement(filioque_extractiveness_t1215_fourth_lateran, creed_381_pneumatology__filioque_reading, base_extractiveness, 1215, 0.58).
narrative_ontology:measurement(filioque_extractiveness_t1965_vatican_ii, creed_381_pneumatology__filioque_reading, base_extractiveness, 1965, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(filioque_suppression_t589, creed_381_pneumatology__filioque_reading, suppression_requirement, 589, 0.3).
narrative_ontology:measurement(filioque_suppression_t1054, creed_381_pneumatology__filioque_reading, suppression_requirement, 1054, 0.65).
narrative_ontology:measurement(filioque_suppression_t1215, creed_381_pneumatology__filioque_reading, suppression_requirement, 1215, 0.72).
narrative_ontology:measurement(filioque_suppression_t1965, creed_381_pneumatology__filioque_reading, suppression_requirement, 1965, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(creed_381_pneumatology__filioque_reading, identity_coordination).
narrative_ontology:affects_constraint(creed_381_pneumatology__filioque_reading, creed_381_pneumatology__monoprocession_reading).
narrative_ontology:affects_constraint(creed_381_pneumatology__filioque_reading, creed_381_pneumatology__ecumenical_reunion_reading).
narrative_ontology:affects_constraint(creed_381_pneumatology__filioque_reading, great_schism_ecclesial_rupture).
narrative_ontology:affects_constraint(creed_381_pneumatology__filioque_reading, papal_magisterial_supremacy).

% DUAL FORMULATION NOTE:
% The Filioque is one reading of the creed_381_pneumatology kernel. The monoprocession_reading instantiates an alternative interpretation of the same fixed text (the creed) from the Eastern theological tradition. The ecumenical_reunion_reading proposes a synthesis framework where both readings coexist. Each reading has a distinct constraint_id and its own ε value because they represent structurally different claims: (1) Does the creed implicitly permit/require Filioque? (2) Does it prohibit Filioque? (3) Can both be orthodox? The three stories are linked by kernel identity and by network.affects_constraints, not by logical dependency. The Great Schism constraint is downstream (result of Filioque doctrine conflict); Papal Supremacy is upstream (authority structure enabling Filioque assertion).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(creed_381_pneumatology__filioque_reading, institutional, 0.08).
constraint_indexing:directionality_override(creed_381_pneumatology__filioque_reading, moderate, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
