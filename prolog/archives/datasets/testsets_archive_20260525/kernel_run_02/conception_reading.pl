% ============================================================================
% CONSTRAINT STORY: conception_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_conception_reading, []).

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
 *   constraint_id: conception_reading
 *   human_readable: Moral Status Begins at Conception (Personhood Reading)
 *   domain: moral_philosophy/bioethics/constitutional_law
 *
 * SUMMARY:
 *   The conception-reading of personhood — the claim that moral and legal
 *   personhood begins at fertilization, and therefore the embryo/fetus
 *   possesses full personhood rights from conception — is one instantiation
 *   of a contested kernel about the personhood boundary. This reading
 *   codifies into legal prohibition on abortion and subordinates pregnant
 *   persons' autonomy to fetal personhood claims. The constraint exhibits
 *   high extractiveness (0.68) because it imposes maximum suppression on
 *   reproductive autonomy and medical decision-making, concentrates
 *   institutional power in religious authority structures, and creates
 *   cascading legal consequences (criminal liability for abortion,
 *   restrictions on medical practice, surveillance of pregnancy outcomes).
 *   The constraint's low theater ratio (0.35) indicates that the reading's
 *   mechanisms are largely functional rather than performative — the legal
 *   prohibition on abortion is directly enforced, not merely symbolically
 *   performed. The extractiveness has increased slightly over the interval
 *   (0.55 → 0.68) as legal enforcement has become more sophisticated and
 *   comprehensive (e.g., fetal personhood bills, heartbeat restrictions,
 *   enforcement via civil suits and bounties).
 *
 * KEY AGENTS:
 *   - Pregnant Persons: Primary victims (powerless/trapped) — face maximum suppression of bodily autonomy and medical decision-making once conception occurs
 *   - Medical Abortion Access: Primary victim system (moderate/constrained) — providers face criminal liability, institutional barriers, and enforcement mechanisms
 *   - Reproductive Autonomy (Abstract Collective): Primary victim abstraction (powerless/identity_locked) — institutional good that cannot organize; those recognizing extraction are identity-locked within frameworks that delegitimize their agency
 *   - Religious Authority Structures: Primary beneficiaries (institutional/arbitrage) — benefit from legal codification of theological claims; experience the constraint as coordination of their institutional authority
 *   - Fetal Life Absolutist Movements: Secondary beneficiaries (organized/constrained) — mobilized by the constraint's codification; benefit from legal framing while constrained by movement ideology
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — can see the full extraction mechanism and its constructed character
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(conception_reading, 0.68).
domain_priors:suppression_score(conception_reading, 0.75).
domain_priors:theater_ratio(conception_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(conception_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(conception_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(conception_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(conception_reading, snare).
narrative_ontology:human_readable(conception_reading, "Moral Status Begins at Conception (Personhood Reading)").
narrative_ontology:topic_domain(conception_reading, "moral_philosophy/bioethics/constitutional_law").

domain_priors:requires_active_enforcement(conception_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(conception_reading, fixed_text).
narrative_ontology:cs_authority_grounding(conception_reading, lineage).
narrative_ontology:cs_interpretation_layer_present(conception_reading).
narrative_ontology:cs_kernel_id(conception_reading, personhood_boundary).
narrative_ontology:cs_reading_relation(conception_reading, viability_reading, coexists_with).
narrative_ontology:cs_reading_relation(conception_reading, birth_reading, forecloses).
narrative_ontology:cs_axiom(conception_reading, foundational, personhood_at_fertilization).
narrative_ontology:cs_axiom_status(personhood_at_fertilization, holdable).
narrative_ontology:cs_axiom_grounding(conception_reading, personhood_at_fertilization, deontological).
narrative_ontology:cs_axiom(conception_reading, secondary, genetic_individuation_implies_moral_status).
narrative_ontology:cs_axiom_status(genetic_individuation_implies_moral_status, holdable).
narrative_ontology:cs_axiom_grounding(conception_reading, genetic_individuation_implies_moral_status, theological).
narrative_ontology:cs_reference_frame(conception_reading, theological_personhood_from_conception).
narrative_ontology:cs_drift_state(conception_reading, contemporary_post_roe_repeal, gap(revival_pressure, substantial, true)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(conception_reading, religious_authority_structures).
narrative_ontology:constraint_beneficiary(conception_reading, fetal_life_absolutist_movements).
narrative_ontology:constraint_victim(conception_reading, pregnant_persons).
narrative_ontology:constraint_victim(conception_reading, medical_abortion_access).
narrative_ontology:constraint_victim(conception_reading, reproductive_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PREGNANT PERSON (SNARE) — Trapped by the constraint's legal and existential force. Once conception occurs, the pregnant person is treated as a vessel for fetal personhood with equivalent or subordinate legal status. Exit options are minimal: pregnancy is a biological state that cannot be abandoned without violating the constraint's prohibition on abortion (framed as homicide). Extraction is maximal — bodily autonomy, medical decision-making authority, and life trajectory are subordinated to fetal personhood claims.
constraint_indexing:constraint_classification(conception_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MEDICAL ABORTION ACCESS (SNARE) — Constrained by the constraint's framing of abortion as homicide. Medical providers face criminal liability, conscience objections, and regulatory barriers. Abortion access is suppressed through legal prohibition, enforcement mechanisms, and institutional redesign. Some exit exists (crossing state/national borders, medication abortion, private providers), but at significant cost. Extraction flows toward religious authority structures and fetal life movements that benefit from access restrictions.
constraint_indexing:constraint_classification(conception_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REPRODUCTIVE AUTONOMY AS ABSTRACT COLLECTIVE (SNARE) — Identity-locked because this collective good is constituted through the constraint itself. Those who internalize the conception-reading's framework have fused their identity with fetal personhood claims and cannot exit without abandoning a core self-concept. The collective cannot organize because it lacks institutional expression; those who recognize extraction do so from within frameworks that delegitimize their own agency. Maximum extraction with internalized suppression.
constraint_indexing:constraint_classification(conception_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 4: RELIGIOUS AUTHORITY STRUCTURES (ROPE) — Institutional beneficiaries. The conception-reading aligns with Catholic, evangelical Christian, and some Orthodox theological frameworks. These structures experience the constraint as pure coordination: it codifies their theology into civil law and strengthens their institutional authority over reproduction and sexuality. Exit is available through doctrinal reinterpretation or disestablishment, but carries institutional risk. Effective extraction is moderate for this agent — they benefit substantially from legal entrenchment, but the constraint also binds their doctrinal flexibility (e.g., no accommodation for fetal anomaly exceptions without theological crisis).
constraint_indexing:constraint_classification(conception_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: FETAL LIFE ABSOLUTIST MOVEMENTS (ROPE) — Organized agents that benefit from the constraint's codification. The conception-reading provides the core claim that animates their mobilization and resource acquisition. They experience coordination benefits: the constraint enables collective action frames, legal strategy, and institutional alliance-building. Exit is constrained by movement identity — backing away from conception-absolutism risks coalition fragmentation and loss of moral authority. Extraction and benefit flow in the same direction, making this rope classification stable.
constraint_indexing:constraint_classification(conception_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From a civilizational analytical perspective, this constraint's extractiveness is substantial and sustained. The conception-reading imposes maximum suppression on reproductive autonomy, codifies religious authority into law, concentrates power over bodies and life decisions, and creates cascading prohibitions on medical practice. The constraint's framing as 'natural law' or 'inherent personhood' obscures its constructed character — it is not that fetuses are naturally persons (a metaphysical claim), but that the reading *imposes personhood status as a binding institutional fact*. The analytical observer can see the full extraction mechanism.
constraint_indexing:constraint_classification(conception_reading, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(conception_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(conception_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(conception_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(conception_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(conception_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The conception-reading imposes maximum suppression on reproductive autonomy — pregnant persons lose medical decision-making authority over pregnancy continuation, abortion becomes legally equivalent to homicide, and bodily integrity is subordinated to fetal personhood claims. The extraction is severe and asymmetric: religious authority structures and fetal life movements benefit substantially from legal entrenchment, while pregnant persons and medical providers bear concentrated costs. The extractiveness is not maximal (0.70+) because some pregnant persons retain options (border crossing, medication abortion, private provision) and some jurisdictions permit medical exceptions, limiting total suppression. Suppression (0.75): Very high. The constraint suppresses reproductive autonomy through legal prohibition, enforcement mechanisms (criminal penalties, civil suits, institutional redesign), and internalized moral framing. Pregnant persons face: criminal liability for seeking abortion, provider criminalization, mandatory waiting periods, ultrasound requirements, and moral stigmatization. Exit options are severely constrained, making this a high-suppression snare. Theater ratio (0.35): Low. The reading's enforcement mechanisms are largely functional rather than performative. Legal prohibition on abortion is directly enforceable; enforcement does not rely primarily on symbolic performance or legitimacy theater. The low theater indicates that the constraint is a snare (not a piton) — it extracts real resources and restricts real autonomy, not merely through performative compliance.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is maximal across this constraint. Pregnant persons experience it as a complete deprivation of autonomy (snare, trapped, no exit); religious authority structures experience it as the codification of their theology into law (rope, beneficiary, arbitrage exit); analytical observers see the extraction mechanism clearly (snare, analytical, can see full structure). The gap between beneficiary and victim perspectives is stark: for institutional religious actors, this is pure coordination (their theology becomes law); for pregnant persons, this is pure extraction (autonomy is removed). The identity_locked perspective reveals a critical mechanism: those who have internalized the conception-reading's framing cannot exit even when structural barriers are removed — they are bound by their identity fusion with fetal personhood claims. This is the reading's most effective suppression mechanism, independent of legal enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation flows from structural positions. Pregnant persons: victims + trapped → d = 0.95 → f(d) = 1.42 → high experienced extraction. Medical abortion access: victim system + constrained → d ≈ 0.85 → f(d) ≈ 1.15 → high experienced extraction. Reproductive autonomy collective: victim + identity_locked + powerless → d ≈ 0.89 → f(d) ≈ 1.28 → high experienced extraction with internalized suppression. Religious authority structures: beneficiaries + arbitrage → d ≈ 0.05 → f(d) ≈ -0.12 → negative/minimal extraction (they benefit). Fetal life movements: beneficiaries + constrained (by ideology) → d ≈ 0.35 → f(d) ≈ 0.20 → low experienced extraction. Analytical observer: d ≈ 0.72 → f(d) ≈ 1.15 → sees high extraction structurally. The reading's power derives from concentrating benefits on organized institutional actors (religious authorities) while distributing costs across powerless/trapped agents (pregnant persons) with high suppression preventing mobilization.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: This constraint exhibits a pure snare classification across multiple perspectives (pregnant persons, medical access, abstract collective, analytical observer). The Tangled Rope category would apply only if genuine coordination benefits existed for the constrained agents, but the structure provides none — pregnant persons receive no coordination benefit from fetal personhood framing; medical providers receive no coordination benefit from abortion prohibition. The constraint is not hybrid extraction-coordination; it is pure extraction with religious authority structures as the sole beneficiaries. The rope classification for religious authority structures is legitimate — they do experience coordination (codification of theology into law) — but this does not make the overall constraint tangled rope. The reading's mandatrophy is resolved by recognizing that it genuinely is a snare from the perspectives of those bearing costs, and a rope from the perspective of those benefiting.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    personhood_criterion_grounding,
    'What metaphysical fact grounds personhood at conception? Is it genetic continuity, ensoulment, potential agency, relational status, or a conventional institutional designation?',
    'Philosophical analysis of the conception-reading''s foundational axioms and their grounding type. Cross-reading comparison: does viability-reading or birth-reading use incoherent personhood criteria, or do they use the same criteria applied to different markers?',
    'If personhood criterion is metaphysically contingent (not logically necessitated by conception alone), the constraint is a conventional reading that *could* have chosen other thresholds. If personhood is metaphysically inherent at conception, the constraint is a natural law reading. The grounding type determines whether the reading is holdable or foreclosed by empirical discovery.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(personhood_criterion_grounding, conceptual, 'What metaphysical or philosophical ground justifies personhood at conception specifically').

omega_variable(
    sibling_reading_logical_consistency,
    'Can the conception-reading''s core axioms coexist with the viability-reading and birth-reading in a single coherent framework, or do they foreclose each other?',
    'Formal logical analysis: identify the specific premise(es) each reading commits to. Test whether denying the conception-reading''s foundational premise (e.g., ''personhood begins at fertilization'') logically entails rejecting viability/birth readings, or whether the readings simply disagree on the threshold without contradicting each other''s core logic.',
    'If forecloses: only one reading can be true within any single moral/legal framework; the others are not merely wrong but incoherent. If coexists: readings disagree on factual or axiological premises but do not logically eliminate each other. Foreclosure vs. coexistence determines the reading_relations structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_logical_consistency, conceptual, 'Logical relationship between conception-reading axioms and sibling readings').

omega_variable(
    authority_grounding_durability,
    'Religious authority structures ground this reading''s legitimacy. Does religious doctrine about conception-personhood remain stable, or has reinterpretation history shown it susceptible to doctrinal drift?',
    'Historical analysis of doctrine evolution in Catholic, evangelical, and Orthodox traditions. Identify instances where the same religious traditions have shifted positions on pregnancy, personhood, or abortion access. Quantify doctrinal stability vs. drift across the 2,000-year history of Christian theology.',
    'If stable: the reading''s authority grounding is robust and unlikely to shift via doctrinal pressure. If susceptible to drift: the reading''s institutional authority is contingent on maintaining a particular theological consensus that could realign. Impacts confidence in the ''holdable'' status of the foundational axioms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_durability, empirical, 'Whether religious authority grounding for conception-personhood is historically stable or subject to doctrinal reinterpretation').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.75) primarily structural (legal prohibition, enforcement barriers) or internalized (pregnant persons have accepted the reading''s framing as legitimate, believe fetal personhood is natural, feel guilt/shame rather than coercion)?',
    'Survey data on reproductive autonomy acceptance; analysis of language in abortion restriction discourse (framing as ''protection'' vs. ''prohibition''); longitudinal tracking of whether suppression persists or weakens after legal restrictions are removed in jurisdictions that have decriminalized abortion.',
    'If suppression is internalized: the constraint''s extraction mechanism survives legal/structural change — pregnant persons carry the suppression with them even if laws shift. If structural: removal of legal barriers will substantially reduce suppression. Affects how sustainable the constraint is against political shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs. internalized suppression mechanism in conception-reading enforcement').

omega_variable(
    medical_exception_boundary,
    'Does the conception-reading logically permit exceptions to its abortion prohibition (e.g., fetal anomaly incompatible with life, maternal health emergencies, rape/incest)? Or does holding the core axiom require absolute prohibition?',
    'Textual analysis of the reading''s formulation and its institutional deployment. Identify whether religious authority structures that defend the reading permit medical exceptions, and whether they treat exceptions as logical extensions of the axioms or as pragmatic concessions to political pressure.',
    'If exceptions are logically permitted: the reading is more nuanced and less extreme than maximalist framing. If exceptions require abandoning the core axiom: the reading''s logical consistency is fragile under real-world conditions. Affects whether the reading is genuinely holdable or requires suppression of counterfactuals to maintain coherence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medical_exception_boundary, conceptual, 'Logical relationship between conception-personhood axiom and medical exception boundaries').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(conception_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(conception_theater_t0, conception_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(conception_theater_t15, conception_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement(conception_theater_t30, conception_reading, theater_ratio, 30, 0.35).

% Extraction over time
narrative_ontology:measurement(conception_extract_t0, conception_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(conception_extract_t15, conception_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(conception_extract_t30, conception_reading, base_extractiveness, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(conception_reading, identity_coordination).
narrative_ontology:affects_constraint(conception_reading, viability_reading).
narrative_ontology:affects_constraint(conception_reading, birth_reading).
narrative_ontology:affects_constraint(conception_reading, abortion_prohibition_enforcement).
narrative_ontology:affects_constraint(conception_reading, fetal_personhood_legal_status).

% DUAL FORMULATION NOTE:
% The conception-reading is part of the personhood_boundary kernel family. Three structurally distinct constraints decompose from the single natural-language question 'when does personhood begin?': conception_reading (ε=0.68, snare), viability_reading (ε≈0.40-0.50, tangled rope), birth_reading (ε≈0.20-0.30, rope or scaffold). Each has different beneficiaries, victims, and extraction mechanisms. The reading_relations structure in cs_structure maps the logical dependencies between them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(conception_reading, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
