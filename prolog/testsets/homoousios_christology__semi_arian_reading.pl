% ============================================================================
% CONSTRAINT STORY: homoousios_christology__semi_arian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_homoousios_christology__semi_arian_reading, []).

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
 *   constraint_id: homoousios_christology__semi_arian_reading
 *   human_readable: Homoiousios (Semi-Arian) Christology: Christ of Similar Substance
 *   domain: historical_theology/ecclesiastical_politics/commitment_systems
 *
 * SUMMARY:
 *   The homoiousios (of similar substance) Christology represents the
 *   Semi-Arian compromise position that dominated the Eastern church from 357
 *   (Council of Sirmium) through 381 (Constantinople II), when the Pro-Nicene
 *   position was formally affirmed as the imperial standard. This constraint
 *   exemplifies how a doctrinal formula can function simultaneously as a
 *   genuine coordination mechanism (enabling empire-wide ecclesiastical
 *   unity), an extraction mechanism (suppressing minority theological
 *   positions), a performative ritual (theater maintaining social
 *   conformity), and eventually as institutional inertia (persisting long
 *   after functional purpose). The semi-Arian reading occupies a contested
 *   middle ground: it affirms that Christ is divine (against strict Arianism,
 *   which reduced Christ to creature status) while stopping short of claiming
 *   absolute identity of substance with the Father (against strict Nicene
 *   doctrine, which required homoousios — identical substance). The formula's
 *   ε value (0.38) reflects genuine coordination (bishops could genuinely
 *   affirm it, avoiding schism) paired with significant extraction (doctrinal
 *   minorities were forced into conformity, and the formula obscured
 *   unresolved theological questions). The measurements capture the
 *   constraint's lifecycle: extractiveness rises from low during initial
 *   consensus-building (t=0) to peak during Constantius II's enforcement
 *   (t=3–6), then falls post-381 as Pro-Nicene doctrine becomes institutional
 *   standard and semi-Arianism degrades into a performative historical
 *   artifact. Theater ratio rises monotonically as the formula becomes more
 *   performative and less functionally necessary. Suppression requirement
 *   peaks during the enforcement period, then moderates as institutional
 *   pressure from above becomes less necessary.
 *
 * KEY AGENTS:
 *   - Moderate Bishops (organized/constrained): Primary beneficiary — the semi-Arian formula protects their position as acceptable middle ground; it coordinates the Eastern episcopate around a shared vocabulary that avoids explicit schism.
 *   - Imperial Authority (institutional/arbitrage): Primary beneficiary — Constantius II especially benefits from doctrinal formula enabling empire-wide church management without theological wars; pure coordination gain, minimal extraction cost.
 *   - Doctrinal Purists (powerless/trapped): Primary victim — strict Arians find the formula too Nicene; strict Nicenes find it too accommodating to Arianism. Both are forced into conformity or explicit schism with no cognitive middle ground.
 *   - Doctrinal Clarity (abstract): Victim — the formula postpones rather than resolves the underlying theological questions. Unresolved tensions accumulate and are eventually addressed (inadequately) at later councils.
 *   - Pro-Nicene Coalition (organized/constrained): Downstream victim — the semi-Arian formula constrains Pro-Nicene bishops from fully expressing their position; they must conform to the compromise language while secretly organizing for eventual dominance.
 *   - Analytical Observer (analytical/analytical): Witnesses the constraint's naturalization as logical necessity rather than political compromise. Risks treating the formula as a discovered theological truth rather than a constructed institutional solution.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_christology__semi_arian_reading, 0.38).
domain_priors:suppression_score(homoousios_christology__semi_arian_reading, 0.48).
domain_priors:theater_ratio(homoousios_christology__semi_arian_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_christology__semi_arian_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(homoousios_christology__semi_arian_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(homoousios_christology__semi_arian_reading, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_christology__semi_arian_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_christology__semi_arian_reading, "Homoiousios (Semi-Arian) Christology: Christ of Similar Substance").
narrative_ontology:topic_domain(homoousios_christology__semi_arian_reading, "historical_theology/ecclesiastical_politics/commitment_systems").

domain_priors:requires_active_enforcement(homoousios_christology__semi_arian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_christology__semi_arian_reading, 'c5191172-5402-4a3e-9d51-cb5e2440c315').
narrative_ontology:cs_kernel_codification('c5191172-5402-4a3e-9d51-cb5e2440c315', formalized).
narrative_ontology:cs_authority_grounding('c5191172-5402-4a3e-9d51-cb5e2440c315', lineage).
narrative_ontology:cs_interpretation_layer_present('c5191172-5402-4a3e-9d51-cb5e2440c315').
narrative_ontology:cs_reading_relation('c5191172-5402-4a3e-9d51-cb5e2440c315', homoousios_christology__pro_nicene_reading, coexists_with).
narrative_ontology:cs_reading_relation('c5191172-5402-4a3e-9d51-cb5e2440c315', homoousios_christology__arian_reading, coexists_with).
narrative_ontology:cs_axiom('c5191172-5402-4a3e-9d51-cb5e2440c315', foundational, christ_genuinely_intermediate_nature).
narrative_ontology:cs_axiom_status(christ_genuinely_intermediate_nature, holdable).
narrative_ontology:cs_axiom_grounding('c5191172-5402-4a3e-9d51-cb5e2440c315', christ_genuinely_intermediate_nature, deontological).
narrative_ontology:cs_axiom('c5191172-5402-4a3e-9d51-cb5e2440c315', foundational, similarity_preserves_divinity).
narrative_ontology:cs_axiom_status(similarity_preserves_divinity, holdable).
narrative_ontology:cs_axiom_grounding('c5191172-5402-4a3e-9d51-cb5e2440c315', similarity_preserves_divinity, theological).
narrative_ontology:cs_reference_frame('c5191172-5402-4a3e-9d51-cb5e2440c315', imperial_church_unity_via_doctrinal_compromise).
narrative_ontology:cs_drift_state('c5191172-5402-4a3e-9d51-cb5e2440c315', post_constantinople_ii_381, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('c5191172-5402-4a3e-9d51-cb5e2440c315', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(homoousios_christology__semi_arian_reading, homoousios_christology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_christology__semi_arian_reading, moderate_bishops).
narrative_ontology:constraint_beneficiary(homoousios_christology__semi_arian_reading, political_stabilizers).
narrative_ontology:constraint_victim(homoousios_christology__semi_arian_reading, doctrinal_clarity).
narrative_ontology:constraint_victim(homoousios_christology__semi_arian_reading, theological_minorities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DOCTRINAL MINORITY (SNARE) — Neither fully Arian nor fully Nicene, the minority positions find themselves trapped by the compromise position. They cannot exit without explicit schism; they cannot stay without cognitive dissonance. The Semi-Arian formula locks them into a liminal status with no institutional exit.
constraint_indexing:constraint_classification(homoousios_christology__semi_arian_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: MODERATE BISHOPS (TANGLED ROPE) — The semi-Arian position genuinely coordinates the Eastern church around a shared formula that avoids explicit condemnation of either Arianism or Nicene doctrine. But it also extracts institutional loyalty through implied threat: deviation toward either pole triggers accusation of extremism. High coordination value + asymmetric enforcement = tangled rope. The formula requires active enforcement to maintain the middle position against pressure from both sides.
constraint_indexing:constraint_classification(homoousios_christology__semi_arian_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: IMPERIAL AUTHORITY (ROPE) — From the emperor's standpoint (Constantius II, especially), the semi-Arian formula is pure coordination: it provides a shared doctrinal language that enables empire-wide church council and prevents civil war over theology. The imperial authority benefits from the constraint as a peaceful coordination mechanism. No meaningful extraction — the formula solves the collective action problem of managing religious factionalism without imperial violence.
constraint_indexing:constraint_classification(homoousios_christology__semi_arian_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: INSTITUTIONAL CHURCH / POST-381 RETROSPECT (PITON) — The semi-Arian formula persists in institutional memory and practice long after its functional role has been supplanted by Pro-Nicene dominance post-381. The formula becomes a vestigial doctrine maintained through ceremonial conformity rather than active belief. High theater ratio reflects the performative repetition of homoiousios language by bishops who have already cognitively committed to Pro-Nicene doctrine. The constraint degrades into institutional inertia.
constraint_indexing:constraint_classification(homoousios_christology__semi_arian_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: ANALYTICAL / THEOLOGICAL NATURALIZATION (MOUNTAIN) — From a purely logical standpoint, the homoiousios formula appears to be a mathematical necessity: if Christ is fully divine and fully human (Chalcedon), then the relationship between the Father and Son must be analyzable through substance categories. 'Similar substance' appears as an inevitable logical category between 'identical substance' and 'different substance.' This perspective risks naturalizing what is actually a political compromise as a discovery of necessary theological truth.
constraint_indexing:constraint_classification(homoousios_christology__semi_arian_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: COUNCIL COALITION / SUNSET VIEW (SCAFFOLD) — Multiple church councils (Sirmium 357, Constantinople 360) treated the semi-Arian formula as a temporary bridge formula with an explicit sunset: once a more comprehensive ecumenical council could be convened (eventually the 381 Constantinople II council), the compromise would be superseded by a more definitive doctrine. The formula was designed as a coordination mechanism to hold the church together during the post-Nicene fragmentation period (350–381), with the understanding that it would eventually be absorbed into a larger ecclesial settlement. Theater ratio relatively low (0.62) because the formula's performative function was openly acknowledged — it was consciously a compromise, not claimed as discovered truth.
constraint_indexing:constraint_classification(homoousios_christology__semi_arian_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_christology__semi_arian_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(homoousios_christology__semi_arian_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(homoousios_christology__semi_arian_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(homoousios_christology__semi_arian_reading, TR),
    TR >= 0.70.

:- end_tests(homoousios_christology__semi_arian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The semi-Arian formula achieves genuine coordination — bishops from diverse theological positions can affirm homoiousios as a shared commitment that avoids explicit condemnation of either Arianism or Nicene doctrine. This is authentic coordination value, not extraction disguised as coordination. However, significant extraction occurs through the enforced suppression of purist positions: minorities (strict Arians, strict Nicenes) cannot openly dissent without risking accusations of extremism or schism. The formula also extracts through temporal postponement — it defers resolution of the underlying theological question, locking the church into a transitional state. Suppression (0.48): Moderate-high. The semi-Arian formula is not maintained through overt coercion (no mass executions or exiles like earlier Arian persecution), but through institutional pressure: bishops are expected to sign conciliar documents affirming the formula, and deviation is treated as either extremism (toward Arianism) or intransigence (toward Nicene rigidity). The suppression is highest during Constantius II's reign (t=3–6, measured at 0.52) when imperial authority explicitly enforces homoiousios language, and moderates before (t=0, consensus-building phase, 0.30) and after (t=10, post-381 when Pro-Nicene has won, 0.35 as the formula becomes ceremonial). Theater ratio (0.62): Moderate-high, rising over time. Early councils (t=0–3) debate the formula's theological adequacy genuinely — bishops engage in substantive christological argument. By the mid-period (t=3–6) during Constantius II's enforcement, conformity becomes increasingly performative: bishops sign the documents to maintain ecclesiastical position but harbor private reservations. Post-381 (t=10), the theater ratio rises to 0.71 as the formula becomes purely ceremonial — bishops affirm homoiousios language out of institutional habit while their real doctrinal commitment is Pro-Nicene. Claimed type (Tangled Rope): The formula exhibits both genuine coordination (genuine shared commitment to avoiding schism, genuine theological interest in the middle position) and asymmetric extraction (minorities forced into conformity, temporal postponement of resolution, institutional pressure to conform). Both elements are structurally present. The tangled rope classification captures this hybrid: neither pure coordination nor pure extraction, but inextricably blended.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals the constraint's dual character. From the moderate bishops' perspective, the formula is a triumph of coordination (Rope or Tangled Rope with moderate extraction perceived) — it solves the coordination problem of holding the empire's church together. From the imperial authority's perspective, it is pure coordination (Rope) — a policy that enables peace without cost. From the doctrinal minority's perspective, it is pure extraction (Snare) — they are forced into silence or schism with no acceptable third option. From the institutional church's post-381 perspective, it is degraded ritual (Piton) — the formula persists as ceremonial language long after its functional role is gone. From the analytical observer's perspective, it appears as logical necessity (Mountain) — the formula seems to be a discovered truth about Christ's nature rather than a political compromise. From the council coalition's perspective during the active negotiation period, it is a temporary bridge (Scaffold) — explicitly designed as a sunset formula to be superseded by a more comprehensive ecumenical settlement. No single perspective captures the full structure; the constraint is legitimately multiple types depending on structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is computed from the agent's structural relationship to the constraint. Moderate bishops as beneficiaries with constrained exit: d ≈ 0.35 (beneficiaries, but with real costs to their position if they deviate). Imperial authority as beneficiary with arbitrage exit: d ≈ 0.08 (maximum beneficiary; the emperor can abandon the formula if it becomes inconvenient, no cost). Doctrinal minorities as victims with trapped exit: d ≈ 0.92 (pure victims with no structural exit; conformity is the only option). The derived d values map naturally to the perspectives: beneficiaries experience lower χ (more rope-like), victims experience higher χ (more snare-like). The analytical observer's d is derived from their analytical position at civilizational scope (d ≈ 0.72), producing a perspective that risks naturalizing the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the Semi-Arian constraint operates at the intersection of genuine coordination and extractive suppression. The coordinating function is real: the formula genuinely enables empire-wide church unity and provides a shared vocabulary that avoids explicit condemnation of minority positions (at least externally). The extractive function is also real: minorities are forced into conformity or schism, and the formula postpones resolution of underlying theological questions by design. The constraint is legitimately Tangled Rope — both functions are operative. The piton perspective shows the degradation of the constraint post-381 when its coordinating function is superseded by Pro-Nicene dominance and the formula survives as institutional theater rather than functional mechanism. The analytical observer's mountain perspective is a false summit: the formula appears as discovered theological truth (an inevitable logical category between identity and difference), but it is actually a contingent institutional arrangement that benefits the bishops' coordination and the empire's political stability. The perspectival spread (Snare to Rope to Piton to Mountain) demonstrates that indexical classification correctly captures the constraint's multivalent character — no single type is adequate, and the presheaf of perspectives across different structural positions is the complete description.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substance_definition_under_determination,
    'What does ''substance'' (ousia/substantia) mean in the christological context? Does the semi-Arian formula refer to an ontological property or an epistemic category?',
    'Textual analysis of Council Sirmium (357) debates and bishop correspondence; reconstruction of the implicit metaphysics underlying homoiousios language. Contemporary medieval scholastics (Aquinas, Bonaventure) explicitly codified substance metaphysics, enabling retroactive disambiguation.',
    'If ontological: Semi-Arian formula makes a specific metaphysical claim about Christ''s nature (moderate extraction, genuine coordination). If epistemic only: Formula is a performative boundary-marker with no truth-conditional content (higher theater, lower coordination value). Classification swings between Tangled Rope (ontological) and Piton (epistemic-only).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substance_definition_under_determination, conceptual, 'Whether homoiousios refers to ontological or epistemic substance').

omega_variable(
    council_binding_authority,
    'What was the binding authority of Council Sirmium (357) and the ''Dated Creed'' of 360? Were these binding empire-wide doctrine or advisory expressions of episcopal consensus?',
    'Analysis of imperial edict language (Constantius II''s enforcement mechanisms); tracking which sees and dioceses treated homoiousios as binding vs. hortatory. Post-381 rejection of semi-Arianism provides negative evidence.',
    'If binding: Semi-Arian formula is enforced doctrine with high suppression and extraction (Snare from minority view). If advisory: Formula operates as consensus-seeking without coercion (higher Rope, lower Snare). Affects directionality calculation for the ''doctrinal minority'' perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(council_binding_authority, empirical, 'Binding authority of semi-Arian councils').

omega_variable(
    genuine_coordination_vs_enforced_conformity,
    'Did the semi-Arian formula genuinely solve the collective action problem (both Arians and Nicenes could affirm it as acceptable), or did it merely impose a shared language that masked unresolved disagreement?',
    'Close reading of bishop signatures and subsequent dissent on councils. Compare regional variation: did Eastern bishops (more amenable to semi-Arianism) and Western bishops (more Nicene-leaning) diverge after the empire split? Tracking the pattern of conformity-followed-by-reversal suggests enforced rather than genuine coordination.',
    'If genuine coordination: Moderate extraction, genuine functional coordination (Tangled Rope with lower suppression). If enforced conformity: High extraction hidden behind ritual agreement (Snare disguised as Rope, or Piton with coercive substrate). The constraint''s charter classification may shift from Tangled Rope to Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_coordination_vs_enforced_conformity, empirical, 'Whether semi-Arian formula enabled genuine coordination or enforced conformity').

omega_variable(
    reading_foreclosure_post_381,
    'Did the Pro-Nicene reading (affirmed at Constantinople II 381) logically foreclose the semi-Arian reading, or merely politically suppress it?',
    'Examine whether Pro-Nicene doctrine (affirming homoousios identity, not just similarity) logically contradicts semi-Arianism at the level of core premises. If the readings are logically coherent with different metaphysical commitments, foreclosure is political (suppression) rather than logical (foreclosed). If they are logically contradictory, foreclosure is structural.',
    'If logically foreclosed: The reading_relations atom should be ''forecloses'' and the Semi-Arian reading''s status post-381 is inevitably ''overridden'' (structurally defeated). If politically suppressed: The relation is ''coexists_with'' and Semi-Arian axioms remain ''holdable'' in principle (merely repressed). Changes the cs_structure assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_post_381, conceptual, 'Whether Pro-Nicene doctrine logically forecloses Semi-Arian reading').

omega_variable(
    measurement_moment_ambiguity,
    'At what historical moment should the semi-Arian constraint be measured? During active council negotiations (357–360, high coordination)? During Constantius II''s reign (high enforcement)? Post-381 (degraded piton)? Or across the entire period as a single historical entity?',
    'Separate measurement profiles for each period. Track extractiveness: rises during enforcement (Constantius II), stabilizes during council consensus-building, collapses during Pro-Nicene dominance post-381.',
    'Single-moment classification obscures the constraint''s lifecycle. The measurement section captures this by showing extractiveness and theater rising during enforcement, then both dropping post-381. The claimed_type (Tangled Rope) represents the active period (357–381); the piton perspective represents the post-381 degradation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(measurement_moment_ambiguity, empirical, 'Historical moment for measurement and classification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_christology__semi_arian_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homoousios_semi_theater_t0, homoousios_christology__semi_arian_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(homoousios_semi_theater_t3, homoousios_christology__semi_arian_reading, theater_ratio, 3, 0.58).
narrative_ontology:measurement(homoousios_semi_theater_t6, homoousios_christology__semi_arian_reading, theater_ratio, 6, 0.62).
narrative_ontology:measurement(homoousios_semi_theater_t10, homoousios_christology__semi_arian_reading, theater_ratio, 10, 0.71).

% Extraction over time
narrative_ontology:measurement(homoousios_semi_extract_t0, homoousios_christology__semi_arian_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(homoousios_semi_extract_t3, homoousios_christology__semi_arian_reading, base_extractiveness, 3, 0.38).
narrative_ontology:measurement(homoousios_semi_extract_t6, homoousios_christology__semi_arian_reading, base_extractiveness, 6, 0.45).
narrative_ontology:measurement(homoousios_semi_extract_t10, homoousios_christology__semi_arian_reading, base_extractiveness, 10, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(homoousios_semi_supp_t0, homoousios_christology__semi_arian_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(homoousios_semi_supp_t3, homoousios_christology__semi_arian_reading, suppression_requirement, 3, 0.52).
narrative_ontology:measurement(homoousios_semi_supp_t6, homoousios_christology__semi_arian_reading, suppression_requirement, 6, 0.48).
narrative_ontology:measurement(homoousios_semi_supp_t10, homoousios_christology__semi_arian_reading, suppression_requirement, 10, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_christology__semi_arian_reading, attachment_coordination).
narrative_ontology:affects_constraint(homoousios_christology__semi_arian_reading, homoousios_christology__pro_nicene_reading).
narrative_ontology:affects_constraint(homoousios_christology__semi_arian_reading, homoousios_christology__arian_reading).
narrative_ontology:affects_constraint(homoousios_christology__semi_arian_reading, nicene_creed_institutional_authority).

% DUAL FORMULATION NOTE:
% The homoousios_christology kernel has three constraint stories: pro_nicene_reading (strict identity, ε≈0.12, Mountain), semi_arian_reading (this story, compromise, ε≈0.38, Tangled Rope), and arian_reading (creature status, ε≈0.72, Snare from minority perspective). Each reading has its own ε because the observables are structurally distinct: Pro-Nicene is a fixed logical position with low empirical contestation; Semi-Arian is a compromise with genuine coordination but also extraction; Arian reading instantiates pure extraction for the minority holding it. The readings are linked by network.affects_constraints to show their structural interdependence: Semi-Arian exists only as a middle ground between Pro-Nicene and Arian, and its post-381 degradation is driven by Pro-Nicene institutional dominance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
