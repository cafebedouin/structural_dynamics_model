% ============================================================================
% CONSTRAINT STORY: vatican_ii_authority__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_authority__composite_overdetermination_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: vatican_ii_authority__composite_overdetermination_reading
 *   human_readable: Vatican II Authority: Composite Overdetermination Reading
 *   domain: theology/ecclesiology/religious_authority
 *
 * SUMMARY:
 *   Vatican II (1962–1965) and its aftermath present a distinctive structural
 *   constraint: the Council's documents embed multiple doctrinal shifts with
 *   incompatible theological rationales, producing ambiguities that cannot be
 *   resolved into either pure continuity or pure rupture. The constraint is
 *   not a communication failure or a temporary interpretive problem — it is a
 *   structural feature of how the magisterium managed theological innovation
 *   while preserving institutional authority through factional compromise.
 *   The composite overdetermination reading holds that Vatican II is not a
 *   single interpretable event but a layered assembly of distinct commitments
 *   (collegiality + papal primacy, aggiornamento + deposit of faith as
 *   unchanging, ecumenical openness + exclusive claim to full ecclesial
 *   reality, religious freedom + Church establishment). Each shift can be
 *   presented as continuous with tradition (continuity reading) or as
 *   substantively novel (rupture reading), but the incompatibilities between
 *   them cannot be resolved without abandoning at least one faction's
 *   theological investment. Post-conciliar conflicts are therefore
 *   structural, not accidental — they emerge from the constraint's core
 *   structure, not from misunderstandings or incomplete implementation.
 *
 * KEY AGENTS:
 *   - Institutional Magisterium (Rome): Primary beneficiary (institutional/arbitrage) — preserves authority through ambiguity; avoids adjudication between factional theologies; maintains claim to univocal teaching despite internal contradiction.
 *   - Theological Scholarship: Primary beneficiary-victim (organized/constrained) — benefits from conciliar innovation and intellectual freedom; bears cost of maintaining both intellectual honesty and institutional loyalty; produces hermeneutical cover for magisterial authority.
 *   - The Faithful (Identity-Locked): Primary victim (powerless/identity_locked) — structurally mobile but identity-fused with the Church's teaching authority; receives contradictory magisterial signals without a unified interpretive frame; cannot organize to demand clarity without stepping outside identity frame.
 *   - Progressive and Conservative Hierarchies: Intermediate victims-beneficiaries (moderate/constrained) — factions divided by conciliar interpretation; benefit from ambiguity's preservation (permits factional competition without formal schism); constrained from resolving differences (institutional authority unbroken).
 *   - The Vatican II Hermeneutic Apparatus: Institutional actor (institutional/arbitrage) — commissions, synods, theological bodies tasked with interpretation; maintains performative labor that obscures rather than resolves contradictions.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees the constraint as either an immutable feature of conciliar politics (false summit) or a contingent institutional choice to preserve authority through ambiguity.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_authority__composite_overdetermination_reading, 0.58).
domain_priors:suppression_score(vatican_ii_authority__composite_overdetermination_reading, 0.62).
domain_priors:theater_ratio(vatican_ii_authority__composite_overdetermination_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_authority__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_authority__composite_overdetermination_reading, "Vatican II Authority: Composite Overdetermination Reading").
narrative_ontology:topic_domain(vatican_ii_authority__composite_overdetermination_reading, "theology/ecclesiology/religious_authority").

domain_priors:requires_active_enforcement(vatican_ii_authority__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_authority__composite_overdetermination_reading, 'cea0f975-3359-433e-8b74-b6578bcdb06b').
narrative_ontology:cs_kernel_codification('cea0f975-3359-433e-8b74-b6578bcdb06b', formalized).
narrative_ontology:cs_authority_grounding('cea0f975-3359-433e-8b74-b6578bcdb06b', extraction).
narrative_ontology:cs_interpretation_layer_present('cea0f975-3359-433e-8b74-b6578bcdb06b').
narrative_ontology:cs_reading_relation('cea0f975-3359-433e-8b74-b6578bcdb06b', vatican_ii_authority__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('cea0f975-3359-433e-8b74-b6578bcdb06b', vatican_ii_authority__rupture_reading, coexists_with).
narrative_ontology:cs_axiom('cea0f975-3359-433e-8b74-b6578bcdb06b', foundational, vatican_ii_contains_incompatible_theological_rationales).
narrative_ontology:cs_axiom_status(vatican_ii_contains_incompatible_theological_rationales, holdable).
narrative_ontology:cs_axiom_grounding('cea0f975-3359-433e-8b74-b6578bcdb06b', vatican_ii_contains_incompatible_theological_rationales, empirically_contingent).
narrative_ontology:cs_axiom('cea0f975-3359-433e-8b74-b6578bcdb06b', foundational, ambiguities_are_products_of_factional_compromise_not_communication_failure).
narrative_ontology:cs_axiom_status(ambiguities_are_products_of_factional_compromise_not_communication_failure, holdable).
narrative_ontology:cs_axiom_grounding('cea0f975-3359-433e-8b74-b6578bcdb06b', ambiguities_are_products_of_factional_compromise_not_communication_failure, empirically_contingent).
narrative_ontology:cs_reference_frame('cea0f975-3359-433e-8b74-b6578bcdb06b', unified_magisterial_teaching_authority).
narrative_ontology:cs_drift_state('cea0f975-3359-433e-8b74-b6578bcdb06b', contemporary_hermeneutic_impasse, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('cea0f975-3359-433e-8b74-b6578bcdb06b', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(vatican_ii_authority__composite_overdetermination_reading, vatican_ii_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_authority__composite_overdetermination_reading, theological_scholars).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__composite_overdetermination_reading, ecumenical_actors).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, institutional_magisterium).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, postconciliar_unity_claims).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE FAITHFUL (SNARE) — Catholic laity and parish clergy whose identity is constituted through the Church's teaching authority. Identity-locked exit: structurally mobile (could leave Catholicism) but identity fused with the tradition. Receives contradictory magisterial signals post-Vatican II (liturgy, ecumenism, religious freedom, collegiality) without a unified interpretive frame. Bears the cost of navigating incompatible directives. Cannot organize collectively to resolve the ambiguity because doing so would require stepping outside the identity frame. Maximum experienced extraction — the constraint's function is to preserve institutional authority at the cost of coherence.
constraint_indexing:constraint_classification(vatican_ii_authority__composite_overdetermination_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: PROGRESSIVE AND CONSERVATIVE HIERARCHIES (TANGLED ROPE) — Bishops and Roman Curia officers divided into factions by conciliar interpretation. Constrained exit: cannot formally reject Vatican II (institutional authority) but can (and do) implement reforms selectively. Experience genuine coordination benefit (ecumenical dialogue, pastoral innovation, internal debate) alongside asymmetric extraction: progressive interpreters gain institutional cover for innovation; conservatives preserve traditional gatekeeping. Both benefit from the ambiguity's preservation — it permits factional competition without formal schism. Moderate extraction because both factions have real agency and real institutional power.
constraint_indexing:constraint_classification(vatican_ii_authority__composite_overdetermination_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: INSTITUTIONAL MAGISTERIUM / ROME (ROPE) — The papal authority and Roman Curia. Arbitrage exit: can reinterpret Vatican II documents, issue hermeneutical directives (Benedict XVI's 'hermeneutic of continuity'), or move to new councils. Experience the constraint as coordination mechanism: Vatican II's ambiguities permit Rome to claim universal authority (conciliarity + papal primacy framed as compatible through ambiguity). The constraint enables the magisterium to appear unified despite internal theological divisions. Rome benefits from the constraint's preservation — clarifying it would require adjudicating among factions, exposing power distributions. Low effective extraction from Rome's perspective because Rome is the primary architect of the ambiguity.
constraint_indexing:constraint_classification(vatican_ii_authority__composite_overdetermination_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THEOLOGICAL SCHOLARSHIP (ORGANIZED INTERPRETERS) (TANGLED ROPE) — Academic theologians, theological journals, university theology faculties. Constrained exit: institutional positions depend on maintaining relationship with magisterium and Church authority, but genuine intellectual integrity requires acknowledging unresolved contradictions. Genuine coordination function: scholarship develops analytical frameworks that map the ambiguities, trains new theologians, enables pastoral application. Asymmetric extraction: scholars bear the cost of maintaining both intellectual honesty and institutional loyalty (a real psychological and professional burden), while providing the magisterium with the hermeneutical cover (sophisticated frameworks justifying apparent contradictions) that preserves magisterial authority. Moderate-to-high extraction because the constraint forces scholars into performative rationalization.
constraint_indexing:constraint_classification(vatican_ii_authority__composite_overdetermination_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: THE VATICAN II HERMENEUTIC APPARATUS (PITON) — The institutional systems (commissions, synods, theological advisory bodies) tasked with interpreting Vatican II documents post-council. Theater ratio 0.68: much of the hermeneutical labor is performative — the underlying theological contradictions are not resolved, merely re-articulated in increasingly sophisticated language. The apparatus persists through institutional inertia and the weight of conciliar authority, not because it has delivered interpretive clarity. Documents are treated as sacred texts requiring exegetical elaboration rather than as communications with determinate meaning that could be clarified. The degradation is structural: the more the apparatus elaborates, the more the core ambiguities are obscured without being resolved.
constraint_indexing:constraint_classification(vatican_ii_authority__composite_overdetermination_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, Vatican II's overdetermination appears as a necessary feature of conciliar politics: any council that attempts doctrinal innovation while maintaining institutional continuity must produce ambiguities. The constraint appears as an immutable law of how authority systems manage theological change without fracture. However, this natural-law framing is a false summit: Vatican II's specific overdeterminations are contingent outcomes of factional compromise, not inevitable features of conciliar authority. The 'mountain' perspective naturalizes what is actually an institutional choice — the choice to preserve magisterial authority through ambiguity rather than through transparent resolution.
constraint_indexing:constraint_classification(vatican_ii_authority__composite_overdetermination_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_authority__composite_overdetermination_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(vatican_ii_authority__composite_overdetermination_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(vatican_ii_authority__composite_overdetermination_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_authority__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(vatican_ii_authority__composite_overdetermination_reading, TR),
    TR >= 0.70.

:- end_tests(vatican_ii_authority__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-to-high. The magisterium and supporting scholarship extract significant benefit from the ambiguities: Rome preserves institutional authority without resolving internal theological divisions; scholarship maintains institutional position while enjoying intellectual freedom through interpretive play; both benefit from appearing unified despite factional division. However, extraction is not maximal (would be ≥0.66 for snare) because the faithful (primary victims) have some agency — they can selectively adopt progressive or conservative interpretations, pastoral communities can implement reforms or preserve traditions, theologians can publish critical work. The constraint is active enforcement (requires ongoing hermeneutical labor, Rome's explicit defenses of ambiguity, suppression of certain readings through appointment and publication gatekeeping). Suppression (0.62): Moderate-to-high. Significant barriers to resolving ambiguities: institutional investment in treating Vatican II as univocal teaching, magisterial resistance to admitting contradictions, career consequences for scholars openly claiming irresolvable tension, identity fusion of the faithful preventing organized demand for clarity, publication bias in Church-affiliated journals toward harmonization frameworks. Suppression has increased over the interval (0.48 → 0.62) as Rome has issued hermeneutical defenses (Benedict XVI's continuity language) that reframe ambiguities as coherent complexity rather than acknowledging contradiction. Theater ratio (0.68): High and rising. The hermeneutical apparatus increasingly performs exegetical labor that elaborates rather than resolves contradictions. Vatican II documents are treated as sacred texts requiring interpretive unfolding rather than as communications whose meaning could be clarified. Each magisterial statement on Vatican II's 'proper' interpretation generates more theological commentary, which obscures the core question: Are the documents contradictory or not? The theatrical performance is the constraint's primary mechanism — sustained ambiguity through interpretive elaboration.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals why the six classification types all appear legitimate from their respective viewing positions. Rome genuinely experiences the ambiguity as coordination (Rope) because it enables them to claim unified authority. But the faithful genuinely experience it as extraction (Snare) because they are suppressed from demanding clarity. Both observations are structurally accurate — the constraint has a dual nature. It functions as coordination for institutional authority and as extraction for the faithful. This is the defining feature of tangled_rope: genuine coordination function coupled with asymmetric extraction. The gap between magisterium's Rope perspective and the faithful's Snare perspective is not a disagreement about facts — it is a structural difference in how the constraint affects agents with different power and exit capacity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) in this constraint are derived from structural position rather than canonical power atoms. The magisterium's d ≈ 0.15 (beneficiary + arbitrage exit) produces low effective extraction (f(d) ≈ -0.01); they experience the constraint as enabling. Theological scholarship's d ≈ 0.60 (mixed beneficiary-victim status + constrained exit) produces moderate effective extraction (f(d) ≈ 0.80); they experience genuine tension between intellectual and institutional loyalty. The faithful's d ≈ 0.92 (victim + identity_locked exit) produces high effective extraction (f(d) ≈ 1.36); they experience maximum suppression because exiting would require abandoning identity constituted through the Church. Progressive and conservative hierarchies occupy d ≈ 0.55 (mixed status + constrained exit), producing moderate extraction (f(d) ≈ 0.75). The analytical observer's canonical d ≈ 0.73 (observer position), producing f(d) ≈ 1.15. These are not arbitrary values — they reflect how the constraint's structure differentially affects agents by their exit capacity and beneficiary status. The faithful cannot arbitrage out; the magisterium can; the scholars are caught between. The scope modifier σ(S) = 1.2 (global) amplifies extractiveness — the constraint operates at planetary scale through the worldwide Catholic institutional network. Effective extraction χ for the faithful = 0.58 × 1.36 × 1.2 ≈ 0.95 (severe, approaching snare territory), which matches the empirical observation that the faithful face maximum experienced constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY STATUS: UNRESOLVED. The constraint does not clearly resolve into a single type that exhausts its nature. The classification Tangled Rope is provisional — it captures the dual nature (genuine coordination + asymmetric extraction) from an intermediate perspective but does not settle the fundamental question: Is the overdetermination functional complexity (coordination framed as complexity) or extractive ambiguity (extraction disguised as theological sophistication)? The faithful experience it as pure extraction (Snare). The magisterium experiences it as pure coordination (Rope). Both perspectives are empirically grounded. The constraint's ε is stable (0.58), but the classification is perspectival-dependent in a way that suggests the underlying theological structure is genuinely contested, not merely the classification framework. The mandatrophy is not resolved because resolving it would require the magisterium to acknowledge that Vatican II's ambiguities are neither coordination nor natural law but a deliberate choice to preserve authority through unresolved theological tension. That acknowledgment would undermine the authority claim itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compatibility_or_contradiction,
    'Are Vatican II''s apparent contradictions (collegiality vs papal primacy, religious freedom vs Church establishment, ecumenical openness vs deposit of faith as unchanging) genuinely incompatible theological positions, or can they be harmonized through sufficiently sophisticated theological interpretation?',
    'Systematic theological analysis of whether proposed harmonies require only reframing or require semantic evasion (equivocation on key terms); comparison of harmony frameworks against the documents'' operative magisterial claims across pontificates; empirical test: do post-conciliar papal and episcopal actions consistently treat the apparent contradictions as resolved or as requiring ongoing interpretive management?',
    'If genuinely incompatible: the constraint is structural — ambiguity cannot be resolved into clarity without abandoning one faction''s theological commitments. Classification stable as Tangled Rope/Snare. If harmonizable: the constraint is a temporary problem of communication — clearer formulation would reveal underlying unity. Classification would shift toward Rope (pure coordination problem) from most perspectives. This is the operative question that divides continuity and rupture readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compatibility_or_contradiction, conceptual, 'Whether Vatican II contradictions are genuine incompatibilities or resolvable tensions').

omega_variable(
    factional_compromise_vs_organic_development,
    'Did Vatican II''s contradictions arise from factional compromise (competing theological agendas each securing textual language without resolution) or from genuine doctrinal development (organic refinement of unchanging deposit that produced real innovation)?',
    'Historical analysis of conciliar debates (Acta Synodalia archives); identification of passages where competing factions negotiated language that could be read both ways; empirical test: do subsequent papal hermeneutical choices explicitly prefer one faction''s reading, thereby revealing the compromise structure?',
    'If compromise: the overdetermination is intentional structural preservation of factional authority — the magisterium chose ambiguity over adjudication. Supports tangled_rope classification; magisterium is beneficiary. If development: the ambiguities are unfortunate side effects of genuine theological growth — supports rope or scaffold perspectives. This distinction anchors the reading''s claim that post-conciliar conflicts are structural, not accidental.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(factional_compromise_vs_organic_development, empirical, 'Whether contradictions reflect factional compromise or organic doctrinal development').

omega_variable(
    magisterial_intent_to_preserve_ambiguity,
    'Did the magisterium (Pope Paul VI, conciliar fathers, Roman Curia) intend to preserve specific ambiguities as a feature of Vatican II, or are the ambiguities unintended consequences?',
    'Evidence from papal speeches, Curia memoranda, and conciliar diary entries (Congar, Chenu, Prignon archives); explicit magisterial statements about how competing frameworks (collegiality vs primacy) should coexist; observation of whether subsequent popes actively defended the ambiguities or attempted to resolve them.',
    'If intentional preservation: the constraint is engineered — magisterial authority chose stability through ambiguity. Supports beneficiary classification (magisterium). If unintended: the magisterium failed to notice or resolve contradictions in its own teaching — reputational cost, but does not change structural classification. Evidence: John Paul II and Benedict XVI''s explicit hermeneutical defenses of specific ambiguities suggest intentionality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(magisterial_intent_to_preserve_ambiguity, empirical, 'Whether magisterium deliberately preserved ambiguities').

omega_variable(
    alternative_interpretive_closure,
    'Could post-Vatican II scholarship or magisterial direction have achieved interpretive consensus (closure of the ambiguities into a single coherent framework) if pursued with sustained institutional effort?',
    'Counterfactual historical analysis: modeling John Paul II or Benedict XVI choosing to adjudicate between progressive and conservative readings rather than defending the ambiguities; examining whether the hermeneutic of continuity was theoretically capable of absorbing all conciliar innovations or whether it required selective reading.',
    'If closure was possible: the persistence of ambiguity is a policy choice — Rome could have enforced interpretive closure but chose not to. Strengthens tangled_rope and snare perspectives (extraction via imposed ambiguity). If closure was impossible (genuine contradictions resist harmonization): the overdetermination is structural and irreducible. Supports the reading''s claim that ambiguities are not technical communication failures but theological facts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_interpretive_closure, conceptual, 'Whether interpretive closure was possible or contradictions are irreducible').

omega_variable(
    reading_foreclosure_dynamics,
    'Does the composite_overdetermination_reading logically foreclose the continuity_reading or the rupture_reading, or do all three readings coexist as live options held by different parties within the institutional Church?',
    'Logical analysis of whether accepting the overdetermination thesis (genuine incompatible theological rationales) logically requires rejecting continuity framing or rupture framing. Empirical test: do individuals and institutional actors maintain multiple readings simultaneously?',
    'If foreclosure: the composite reading is the more complete analytical frame and the other readings are incomplete or defensive postures. If coexistence: all three readings remain live — they represent genuine theological factions. This is the ''kernel contest'' core ambiguity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_dynamics, conceptual, 'Whether this reading forecloses or coexists with sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_authority__composite_overdetermination_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vat2_comp_theater_t0, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(vat2_comp_theater_t10, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 10, 0.63).
narrative_ontology:measurement(vat2_comp_theater_t20, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(vat2_comp_extract_t0, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(vat2_comp_extract_t10, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(vat2_comp_extract_t20, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(vat2_comp_suppress_t0, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(vat2_comp_suppress_t10, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(vat2_comp_suppress_t20, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 20, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_authority__composite_overdetermination_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vatican_ii_authority__composite_overdetermination_reading, 0.12).
narrative_ontology:affects_constraint(vatican_ii_authority__composite_overdetermination_reading, vatican_ii_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__composite_overdetermination_reading, vatican_ii_authority__rupture_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__composite_overdetermination_reading, catholic_magisterial_coherence).
narrative_ontology:affects_constraint(vatican_ii_authority__composite_overdetermination_reading, post_conciliar_hermeneutic_closure).

% DUAL FORMULATION NOTE:
% Vatican II authority operates through three distinct constraint stories representing the contested kernel. The composite_overdetermination_reading (this file) represents the analytical claim that Vatican II's ambiguities are irreducible theological contradictions from factional compromise. The continuity_reading represents the magisterium's framework (Vatican II as organic development). The rupture_reading represents the traditionalist critique (Vatican II as substantive break). Each story has its own ε value reflecting different empirical scopes: continuity assumes ambiguities are resolvable through interpretation; rupture assumes they reflect genuine error; overdetermination assumes they are structural. The three stories are linked as readings of the same kernel — they are not alternative measurements of the same constraint, but competing institutional framings of the same authority claim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_authority__composite_overdetermination_reading, analytical, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
