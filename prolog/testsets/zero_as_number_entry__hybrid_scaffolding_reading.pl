% ============================================================================
% CONSTRAINT STORY: zero_as_number_entry__hybrid_scaffolding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zero_as_number_entry__hybrid_scaffolding_reading, []).

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
 *   constraint_id: zero_as_number_entry__hybrid_scaffolding_reading
 *   human_readable: Zero-as-Number: Hybrid Scaffolding Reading
 *   domain: history_of_mathematics/philosophy_of_mathematics/conceptual_history
 *
 * SUMMARY:
 *   Zero-as-number presents a paradox: zero is mathematically simple (the
 *   additive identity in positional notation), yet historically elusive.
 *   Hindu mathematicians operationalized zero by the 5th century CE, European
 *   mathematicians resisted it until the 13th century despite contact via
 *   Islamic intermediaries. This constraint models the mechanism of that
 *   delay as a coordination problem, not an ignorance problem. Greek and
 *   European traditions had developed robust geometric algebra (magnitudes,
 *   geometric construction) that ontologically excluded zero: zero has no
 *   magnitude, cannot be geometrically constructed, cannot be a proper
 *   quantity under Aristotelian metaphysics. Hindu traditions approached
 *   algebra differently: quantities were not necessarily magnitudes but could
 *   be abstract numerical symbols. In this framework, zero was not a
 *   violation—it was an empty placeholder, already philosophically intuitive
 *   (connected to Buddhist/Jain concepts of śūnyatā, void, infinite
 *   cosmological cycles). The constraint is the incompatibility between these
 *   frameworks. Zero was latent—demanded by positional notation's logical
 *   structure—but operationalizing it required a conceptual scaffold. Hindu
 *   mathematics had that scaffold; Greek-European tradition did not. Contact
 *   provided the scaffold, but transmission was delayed by institutional
 *   resistance (scholastic authority defending magnitude-only quantity) and
 *   by the identity-lock of geometric algebraic practice. Theater ratio shows
 *   how this manifests: early suppression was high and theatrical (elaborate
 *   metaphysical arguments against zero despite zero's mathematical utility),
 *   later theater declined as the utility became undeniable and the geometric
 *   identity gradually revised. Extractiveness rose as the resolution
 *   proceeded—early, Europeans could claim ignorance; later, they had to
 *   explicitly acknowledge dependency on Hindu mathematics, creating
 *   asymmetric knowledge credit.
 *
 * KEY AGENTS:
 *   - Hindu Algebraic Tradition: Primary beneficiary (institutional/arbitrage) — compatible philosophical scaffolding (quantity-as-symbol, śūnyatā) enabled early operationalization of zero. Benefit: computational utility, epistemological alignment.
 *   - Greek Geometric Algebra Tradition: Primary victim (powerless/identity_locked) — foundational commitment to magnitude-only quantity creates identity lock. Exit requires abandoning geometric ontology. Trapped by intellectual identity, not by material barriers.
 *   - European Medieval Scholasticism: Secondary victim (institutional/trapped) — doctrinal authority invested in Aristotelian quantity categories. Institutional suppression of zero persists despite mathematical utility. Theater high: defending boundaries that practice has crossed.
 *   - Post-Contact European Mathematicians: Moderate agent (moderate/constrained) — intermediate position. Contact provides scaffold, but institutional resistance constrains adoption. Mixed benefit (utility of zero) and cost (dependence on external source, revision of identity).
 *   - Global Mathematical Practice: Beneficiary-across-traditions (organized/mobile) — zero solves genuine coordination problem. Once scaffold is shared, constraint dissolves into pure coordination benefit.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent scaffolding as structural inevitability (false summit: 'zero is inevitable because of positional notation'). True structure: latent, but operationalization requires contingent scaffolding.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zero_as_number_entry__hybrid_scaffolding_reading, 0.38).
domain_priors:suppression_score(zero_as_number_entry__hybrid_scaffolding_reading, 0.42).
domain_priors:theater_ratio(zero_as_number_entry__hybrid_scaffolding_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zero_as_number_entry__hybrid_scaffolding_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(zero_as_number_entry__hybrid_scaffolding_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zero_as_number_entry__hybrid_scaffolding_reading, rope).
narrative_ontology:human_readable(zero_as_number_entry__hybrid_scaffolding_reading, "Zero-as-Number: Hybrid Scaffolding Reading").
narrative_ontology:topic_domain(zero_as_number_entry__hybrid_scaffolding_reading, "history_of_mathematics/philosophy_of_mathematics/conceptual_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zero_as_number_entry__hybrid_scaffolding_reading, '782a3d60-afce-4f84-a9c5-8caf1619af04').
narrative_ontology:cs_kernel_codification('782a3d60-afce-4f84-a9c5-8caf1619af04', distributed).
narrative_ontology:cs_authority_grounding('782a3d60-afce-4f84-a9c5-8caf1619af04', expertise).
narrative_ontology:cs_interpretation_layer_present('782a3d60-afce-4f84-a9c5-8caf1619af04').
narrative_ontology:cs_reading_relation('782a3d60-afce-4f84-a9c5-8caf1619af04', zero_as_number_entry__universal_discovery_reading, coexists_with).
narrative_ontology:cs_reading_relation('782a3d60-afce-4f84-a9c5-8caf1619af04', zero_as_number_entry__contingent_thinkability_reading, influences).
narrative_ontology:cs_axiom('782a3d60-afce-4f84-a9c5-8caf1619af04', foundational, mathematical_structure_latency).
narrative_ontology:cs_axiom_status(mathematical_structure_latency, holdable).
narrative_ontology:cs_axiom_grounding('782a3d60-afce-4f84-a9c5-8caf1619af04', mathematical_structure_latency, empirically_contingent).
narrative_ontology:cs_axiom('782a3d60-afce-4f84-a9c5-8caf1619af04', foundational, scaffolding_contingency_operationalization).
narrative_ontology:cs_axiom_status(scaffolding_contingency_operationalization, holdable).
narrative_ontology:cs_axiom_grounding('782a3d60-afce-4f84-a9c5-8caf1619af04', scaffolding_contingency_operationalization, empirically_contingent).
narrative_ontology:cs_reference_frame('782a3d60-afce-4f84-a9c5-8caf1619af04', mathematical_structure_contains_zero_latently).
narrative_ontology:cs_drift_state('782a3d60-afce-4f84-a9c5-8caf1619af04', post_contact_european_adoption, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('782a3d60-afce-4f84-a9c5-8caf1619af04', '').
narrative_ontology:cs_kernel_id(zero_as_number_entry__hybrid_scaffolding_reading, zero_as_number_entry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zero_as_number_entry__hybrid_scaffolding_reading, hindu_algebraic_tradition).
narrative_ontology:constraint_victim(zero_as_number_entry__hybrid_scaffolding_reading, greek_geometric_tradition).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GREEK GEOMETRIC PRACTITIONERS (SNARE) — Identity fused with geometric-only algebraic framework. Exit would require abandoning foundational epistemic commitments (algebra as geometry, all quantities as magnitudes). Cannot operationally think zero within their framework because zero has no geometric magnitude. Trapped by identity lock — intellectually mobile yet unable to perceive the alternative frame from within their geometric worldview.
constraint_indexing:constraint_classification(zero_as_number_entry__hybrid_scaffolding_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 2: HINDU ALGEBRAIC INSTITUTIONS (ROPE) — Institutional actors (mathematical schools, court patronage structures) benefit from early operationalization of zero. Zero enables arithmetic operations on abstract quantities, not tied to magnitude. This is coordination: the tradition benefits from aligning on zero's computational utility. No suppression of alternatives—merely the positive utility of the new operational framework. Lower extraction, alignment of interest.
constraint_indexing:constraint_classification(zero_as_number_entry__hybrid_scaffolding_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: POST-CONTACT EUROPEAN MATHEMATICIANS (TANGLED ROPE) — Moderate power, constrained exit. Contact with Hindu zero (via Islamic intermediaries) provides the scaffolding but creates asymmetric outcomes: early adopters gain computational advantage, but institutional resistance (geometric tradition's authority, scholastic skepticism about quantity-without-magnitude) constrains adoption. Mixed: coordination benefit from zero's utility AND extraction through cultural/intellectual dependency (learned a tool from external source, must credit/adopt). Constraining: existing geometric identity must be revised.
constraint_indexing:constraint_classification(zero_as_number_entry__hybrid_scaffolding_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: GLOBAL MATHEMATICS COMMUNITIES (ROPE) — Organized, mobile at civilizational timescale. Zero solved a genuine coordination problem: how to represent and manipulate quantities without grounding in magnitude. This is pure coordination across traditions. The constraint is the scaffolding gap itself—the problem of operationalizing latent structure. Once scaffold is shared, constraint dissolves. Low extraction, high coordination utility.
constraint_indexing:constraint_classification(zero_as_number_entry__hybrid_scaffolding_reading, rope,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: MEDIEVAL SCHOLASTIC AUTHORITY (PITON) — Institutional actors maintaining doctrinal authority over what 'number' and 'quantity' mean. Zero violates foundational categories: it is not a magnitude (Aristotelian metaphysics), not a proper quantity (scholastic logic). The constraint persists through institutional inertia long after mathematical utility is demonstrated. Theater high: elaborate arguments for why zero cannot be a true number, even as mathematical practice uses it. Function degraded: defending a boundary that mathematical practice has already crossed.
constraint_indexing:constraint_classification(zero_as_number_entry__hybrid_scaffolding_reading, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / LATENT STRUCTURE (MOUNTAIN) — From civilizational/universal perspective, zero is latent in ANY positional notation system. Once you have place-value representation, zero is structurally inevitable—the gap in the sequence must be filled. This reading treats zero's eventual operationalization as natural law: the mathematical structure demanded it. However, the extracted structural data (identity_locked exit, suppression via geometric framework, temporal contingency on contact) contradicts pure naturalness—reveals this as false summit, naturalizing what is actually a scaffolding coordination problem.
constraint_indexing:constraint_classification(zero_as_number_entry__hybrid_scaffolding_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zero_as_number_entry__hybrid_scaffolding_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(zero_as_number_entry__hybrid_scaffolding_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(zero_as_number_entry__hybrid_scaffolding_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(zero_as_number_entry__hybrid_scaffolding_reading, TR),
    TR >= 0.70.

:- end_tests(zero_as_number_entry__hybrid_scaffolding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. This reading treats zero as a genuine coordination problem (low ε base), not pure extraction. The beneficiary (Hindu tradition) has a compatible framework that enables operationalization, but this is alignment of interest, not suppression of alternatives. The victim (Greek geometric tradition) is locked by identity, but exits are intellectually available at high cost—not impossible, just identity-dissolving. Post-contact extraction (dependency on Hindu source) elevates ε beyond pure coordination, but not to snare levels because utility is undeniable and adoption is voluntary (constrained, not trapped). Suppression (0.42): Moderate. Early scholastic suppression of zero (high theater, elaborate arguments) is real but not total—mathematical utility drives adoption despite resistance. Institutional inertia maintains suppression even after utility is clear (piton dynamic). By end of interval, suppression is minimal; geometric tradition has revised to accommodate zero. Theater ratio (0.35, declining from 0.62): High-to-moderate trajectory. Early phase: extensive scholastic arguments defending magnitude-only quantity, defending against zero (theatrical suppression, high ε-irrelevant cost). Later phase: zero accepted, arguments fade, theater declines as identity revises. The trajectory models how theaters persist through inertia even after their function ends.
 *
 * PERSPECTIVAL GAP:
 *   DIAGNOSTIC EXEMPLAR of hybrid scaffolding coordination. Geometric practitioners see a Snare: incompatible framework forces them to abandon foundational commitments (identity-locked exit). Hindu institutions see Rope: zero is coordination benefit aligned with their framework. Post-contact Europeans see Tangled Rope: genuine utility (coordination) AND asymmetric extraction (dependency, institutional pressure to revise identity). Scholastic authority sees Piton: defending doctrinal boundaries that mathematical practice has already crossed—theater persists through inertia after function ends. Global mathematics sees Rope: once scaffold is shared, constraint dissolves into pure coordination. Analytical observer risks Mountain: treating latency of zero in positional notation as inevitability, naturalizing what is actually a scaffolding coordination problem. The perspectival gap reveals that the constraint is not 'is zero inevitable?' but 'how does operationalizing latent structure require compatible scaffolding?' The six types are different angles on that single structural question.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) tracks each agent's structural position in the extraction flow. Hindu institutions: d ≈ 0.15 (beneficiary with compatible framework, low cost to operationalize—negative effective extraction). Geometric practitioners: d ≈ 0.85 (victim locked by identity, high cost to exit—near-maximal experienced extraction). Post-contact Europeans: d ≈ 0.55 (moderate: benefit from utility, cost from dependency and identity revision). The piton perspective reflects institutional inertia, not high d—the engine computes d from structural position; theater ratio determines piton classification independently (theater ≥ 0.70 gates piton). The mountain perspective's d ≈ 0.72 (analytical observer) reflects the observer position; the false summit detector will flag this if beneficiaries are declared and the natural-law gates (accessibility_collapse, resistance, emerges_naturally) are tested. In this reading, Hindu tradition is declared as beneficiary, so FSM will trigger and reclassify mountain to tangled_rope via signature override if natural-law metrics are absent or insufficient.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves mandatrophy by showing that zero-as-number is a genuine coordination problem (Rope base) with institutional resistance (Snare from victim perspective, Piton from scholastic authority perspective). The mandatrophy—is it structure or contingency?—is answered: it is BOTH. Structure: positional notation latently contains zero. Contingency: operationalizing latent structure requires conceptual scaffolding that different traditions provide at different times. The constraint is the gap between latency and operationalization. Resolution: once scaffolding is shared (contact, translation, adoption), constraint dissolves. The hybrid reading unifies the universal discovery reading (yes, latent structure) with the contingent thinkability reading (yes, requires specific scaffolding) into a single claim: latent-plus-contingent. This prevents collapse into false summit (pure naturalism) while respecting structural reality (zero IS implicit in positional notation).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    latency_vs_contingency,
    'Is zero latent (structurally demanded by positional notation) or contingent (required specific philosophical scaffolding to become thinkable)?',
    'Historical analysis: did any culture with positional notation develop zero independently? Did cultures with geometric-only algebra approach zero? If latency true, independent discovery should be near-universal; if contingency true, discovery clusters around compatible philosophical frameworks.',
    'If latent: classify as Mountain (structural inevitability). If contingent: classify as Rope (coordination problem requiring shared scaffolding). If hybrid: Tangled Rope (latent structure + contingent operationalization).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(latency_vs_contingency, empirical, 'Whether zero is structurally latent in positional notation or contingent on conceptual scaffolding').

omega_variable(
    scaffolding_transmission_mechanism,
    'Did contact transmit zero as a ''concept,'' or did it trigger recognition of a latent structure already demanded by mathematical practice?',
    'Textual analysis of early European adoption: explicit intellectual debt vs. independent rediscovery claims. Measurement of adoption timeline relative to contact events (Islamic intermediaries). Comparison of pre-contact hints of zero-like ideas in European tradition.',
    'If concept-transmission: Snare (dependency, suppression of local development). If structure-recognition: Rope (coordination). If hybrid: Tangled Rope with significant identity_locked component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffolding_transmission_mechanism, conceptual, 'Mechanism of zero transmission: concept import vs. latent structure recognition').

omega_variable(
    geometric_tradition_exit_cost,
    'What was the actual intellectual and institutional cost for Greek/European geometric tradition to abandon magnitude-only algebra?',
    'Historical reconstruction of scholastic resistance: how many major arguments were deployed against zero? How long did resistance persist despite demonstrable utility? Did individual mathematicians experience this as identity loss or merely academic inconvenience?',
    'If cost was primarily identity-fusion: identity_locked exit is correct classification for geometric tradition. If cost was primarily institutional-structural: constrained exit more appropriate. Reshapes whether victim experience was Snare or Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geometric_tradition_exit_cost, empirical, 'Exit cost for geometric tradition to abandon magnitude-only framework').

omega_variable(
    hindu_scaffolding_source,
    'Did Hindu mathematical/philosophical tradition deliberately construct scaffolding for zero, or did zero emerge from existing philosophical commitments (e.g., Buddhist śūnyatā, numerical infinity in cosmology)?',
    'Textual analysis of Hindu mathematical treatises: explicit philosophical grounding for zero vs. pragmatic introduction. Chronology of philosophical developments (Buddhist logic, Jain infinity) relative to zero-in-mathematics. Comparison of zero''s conceptual role in Vedic vs. post-Buddhist mathematical contexts.',
    'If deliberately constructed: Hindu institutions are active beneficiaries of intentional scaffolding. If emergent from philosophy: beneficiary status remains but less directed. Either way, Hindu tradition benefits from compatible philosophical framework—affects magnitude of extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hindu_scaffolding_source, empirical, 'Source of scaffolding in Hindu tradition: deliberate construction vs. philosophical emergence').

omega_variable(
    false_summit_naturalness,
    'Is the ''inevitability'' of zero (mountain classification) an observation about mathematical structure or a naturalization of a contingent historical process?',
    'Counterfactual: could positional notation have developed without zero operationalization? If yes, then inevitability is false—the mountain classification naturalizes contingency. If no, then some inevitability claim holds—but the thesis of this reading is that inevitability is mediated by contingent scaffolding, not direct.',
    'Confirms or disconfirms false summit detection. If mountain classification is detected as false summit by signature chain, this reading''s hybrid claim (latent structure + contingent scaffolding) is validated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_naturalness, conceptual, 'Whether inevitability of zero is structural (true mountain) or contingent naturalization (false summit)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zero_as_number_entry__hybrid_scaffolding_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zero_scaffold_theater_t0, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 0, 0.62).
narrative_ontology:measurement(zero_scaffold_theater_t5, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 5, 0.48).
narrative_ontology:measurement(zero_scaffold_theater_t10, zero_as_number_entry__hybrid_scaffolding_reading, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(zero_scaffold_extract_t0, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(zero_scaffold_extract_t5, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(zero_scaffold_extract_t10, zero_as_number_entry__hybrid_scaffolding_reading, base_extractiveness, 10, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(zero_scaffold_supp_t0, zero_as_number_entry__hybrid_scaffolding_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(zero_scaffold_supp_t5, zero_as_number_entry__hybrid_scaffolding_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(zero_scaffold_supp_t10, zero_as_number_entry__hybrid_scaffolding_reading, suppression_requirement, 10, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zero_as_number_entry__hybrid_scaffolding_reading, information_standard).
narrative_ontology:affects_constraint(zero_as_number_entry__hybrid_scaffolding_reading, zero_as_number_entry__universal_discovery_reading).
narrative_ontology:affects_constraint(zero_as_number_entry__hybrid_scaffolding_reading, zero_as_number_entry__contingent_thinkability_reading).
narrative_ontology:affects_constraint(zero_as_number_entry__hybrid_scaffolding_reading, positional_notation_adoption).
narrative_ontology:affects_constraint(zero_as_number_entry__hybrid_scaffolding_reading, geometric_algebra_identity_lock).

% DUAL FORMULATION NOTE:
% The zero_as_number_entry kernel decomposes into three sibling constraint stories, each a distinct reading: universal_discovery (zero is inevitable structure), contingent_thinkability (zero is pure accident), hybrid_scaffolding (zero is latent structure requiring contingent scaffolding). Each reading has its own ε: universal_discovery ≈ 0.12 (mountain, structure inevitable), contingent_thinkability ≈ 0.55 (tangled_rope, concept + suppression), hybrid_scaffolding ≈ 0.38 (rope, coordination problem). The network links them as a family; each reading's cs_structure.reading_relations declares logical relationships (coexists_with in this case—all three are live scholarly positions).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
