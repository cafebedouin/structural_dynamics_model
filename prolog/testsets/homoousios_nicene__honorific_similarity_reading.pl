% ============================================================================
% CONSTRAINT STORY: homoousios_nicene__honorific_similarity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_homoousios_nicene__honorific_similarity_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: homoousios_nicene__honorific_similarity_reading
 *   human_readable: Homoousios as Honorific Similarity (Nicene Kernel Reading)
 *   domain: theological/ecclesiastical
 *
 * SUMMARY:
 *   The homoousios honorific-similarity reading interprets the Nicene
 *   formula's key term as permitting functional unity and honorific identity
 *   without requiring strict metaphysical reduction of the Son's being to the
 *   Father's. This reading emerges from bishops and theologians (especially
 *   in the East) who want to affirm Nicene orthodoxy—to condemn
 *   Arianism—while preserving theological schools that teach graduated
 *   subordination or apophatic restraint about the divine essence. The
 *   constraint's operation is distinctive: it does not suppress articulate
 *   objection to its reading (resistance stays high) but rather manages the
 *   boundary between licit and illicit interpretation by shifting the
 *   criterion from essence-identity to pastoral intent. Theater ratio rises
 *   over the interval as enforcement increasingly focuses on monitoring
 *   bishops' *use* of homoousios language rather than their metaphysical
 *   beliefs, making the constraint's operation more dependent on performative
 *   assent than on genuine metaphysical conversion.
 *
 * KEY AGENTS:
 *   - Semi-Arian moderates — beneficiaries seeking a reading that permits gradualist metaphysics while maintaining Nicene standing
 *   - Apophatic traditions — beneficiaries aligned with the reading's theological humility and resistance to metaphysical overreach
 *   - Local episcopal authority — beneficiary-agenda-setters who gain discretion to interpret doctrine regionally
 *   - Strict Nicene enforcers — payers who see the reading as doctrinal retreat and must expend effort to exclude it
 *   - Hard subordinationists — payers threatened by the reading's implicit heresy-boundary against pure subordinationism
 *   - Ecumenical councils — institutional agenda-setters who maintain canonical authority over homoousios meaning
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_nicene__honorific_similarity_reading, 0.38).
domain_priors:suppression_score(homoousios_nicene__honorific_similarity_reading, 0.45).
domain_priors:theater_ratio(homoousios_nicene__honorific_similarity_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_nicene__honorific_similarity_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(homoousios_nicene__honorific_similarity_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(homoousios_nicene__honorific_similarity_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_nicene__honorific_similarity_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(homoousios_nicene__honorific_similarity_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_nicene__honorific_similarity_reading, rope).
narrative_ontology:human_readable(homoousios_nicene__honorific_similarity_reading, "Homoousios as Honorific Similarity (Nicene Kernel Reading)").
narrative_ontology:topic_domain(homoousios_nicene__honorific_similarity_reading, "theological/ecclesiastical").

domain_priors:requires_active_enforcement(homoousios_nicene__honorific_similarity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_nicene__honorific_similarity_reading, '56a08b63-06df-4be6-b247-e521b2cbef82').
narrative_ontology:cs_kernel_codification('56a08b63-06df-4be6-b247-e521b2cbef82', fixed_text).
narrative_ontology:cs_authority_grounding('56a08b63-06df-4be6-b247-e521b2cbef82', lineage).
narrative_ontology:cs_interpretation_layer_present('56a08b63-06df-4be6-b247-e521b2cbef82').
narrative_ontology:cs_reading_relation('56a08b63-06df-4be6-b247-e521b2cbef82', homoousios_nicene__metaphysical_equality_reading, coexists_with).
narrative_ontology:cs_reading_relation('56a08b63-06df-4be6-b247-e521b2cbef82', homoousios_nicene__subordinationist_reading, coexists_with).
narrative_ontology:cs_axiom('56a08b63-06df-4be6-b247-e521b2cbef82', foundational, honorific_similarity_suffices_for_orthodoxy).
narrative_ontology:cs_axiom_status(honorific_similarity_suffices_for_orthodoxy, holdable).
narrative_ontology:cs_axiom_grounding('56a08b63-06df-4be6-b247-e521b2cbef82', honorific_similarity_suffices_for_orthodoxy, conventional).
narrative_ontology:cs_axiom('56a08b63-06df-4be6-b247-e521b2cbef82', foundational, metaphysical_identity_not_required_for_anti_arianism).
narrative_ontology:cs_axiom_status(metaphysical_identity_not_required_for_anti_arianism, holdable).
narrative_ontology:cs_axiom_grounding('56a08b63-06df-4be6-b247-e521b2cbef82', metaphysical_identity_not_required_for_anti_arianism, instrumental).
narrative_ontology:cs_axiom('56a08b63-06df-4be6-b247-e521b2cbef82', secondary, episcopal_discretion_preserves_communion).
narrative_ontology:cs_axiom_status(episcopal_discretion_preserves_communion, holdable).
narrative_ontology:cs_axiom_grounding('56a08b63-06df-4be6-b247-e521b2cbef82', episcopal_discretion_preserves_communion, conventional).
narrative_ontology:cs_reference_frame('56a08b63-06df-4be6-b247-e521b2cbef82', nicene_formula_as_heresy_boundary).
narrative_ontology:cs_drift_state('56a08b63-06df-4be6-b247-e521b2cbef82', post_chalcedon_metaphysical_tightening, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('56a08b63-06df-4be6-b247-e521b2cbef82', '').
narrative_ontology:cs_kernel_id(homoousios_nicene__honorific_similarity_reading, homoousios_nicene).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_nicene__honorific_similarity_reading, semi_arian_moderates).
narrative_ontology:constraint_beneficiary(homoousios_nicene__honorific_similarity_reading, apophatic_traditions).
narrative_ontology:constraint_beneficiary(homoousios_nicene__honorific_similarity_reading, local_episcopal_authority).
narrative_ontology:constraint_victim(homoousios_nicene__honorific_similarity_reading, strict_nicene_enforcers).
narrative_ontology:constraint_victim(homoousios_nicene__honorific_similarity_reading, hard_subordinationists).
narrative_ontology:constraint_vindicates(homoousios_nicene__honorific_similarity_reading, apophatic_theology_primacy).
narrative_ontology:constraint_vindicates(homoousios_nicene__honorific_similarity_reading, episcopal_interpretive_discretion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold that Homoousios can accommodate functional unity without requiring strict metaphysical identity of essence. They interpret the term as a bridge formula permitting both honorific language and graduated subordination. This reading allows their position to remain within Nicene councils without accepting full ontological equality, giving them orthodox standing while preserving gradualist metaphysics.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, semi_arian_moderates, beneficiary,
    organized, generational, constrained, continental).

% Maintain that the divine essence is fundamentally unknowable and that precise ontological claims about the nature of homoousios exceed human conceptual reach. The honorific-similarity reading aligns with their emphasis on doctrinal humility and negative theology, allowing Nicene assent without metaphysical overreach. They benefit from interpretive flexibility that does not demand they assert what they claim is theologically improper—precise essentialist claims.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, apophatic_traditions, beneficiary,
    moderate, civilizational, mobile, continental).

% Gains discretion to interpret homoousios according to local pastoral contexts and theological traditions. The honorific reading permits bishops to mediate disputes without enforcing uniform metaphysical commitments, preserving episcopal authority to arbitrate doctrine within their sees. This shifts interpretive power away from centralized councils and toward episcopal judgment.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, local_episcopal_authority, beneficiary,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(homoousios_nicene__honorific_similarity_reading, local_episcopal_authority, agenda_setter).

% Committed to homoousios as strict metaphysical equality of essence and reject any reading that permits subordinationist interpretation. They experience the honorific-similarity reading as a retreat from Nicene doctrine, a softening that reintroduces the very errors Nicaea was called to condemn. They must expend enforcement effort to prevent this reading from taking hold, and their victory at the council appears undermined if similarity language becomes theologically licensed.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, strict_nicene_enforcers, payer,
    institutional, generational, trapped, continental).

% Maintain that the Son is genuinely subordinate in being to the Father and read homoousios as compatible with this subordination. The honorific-similarity reading threatens them by appearing to concede that homoousios does *not* unambiguously permit subordinationism—it becomes a boundary constraint that excludes purely subordinationist metaphysics even if nominally honorific. They experience this reading as heresy-exclusion language that deprives them of the very ambiguity they rely on.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, hard_subordinationists, payer,
    organized, generational, constrained, continental).

% Establish and maintain the canonical Nicene formula through successive councils (Nicaea 325, Constantinople 381, Chalcedon 451). They decide which readings of homoousios are licit and which constitute heresy. Their authority rests on the claim that the term has a determinate meaning they have the power to adjudicate. The honorific-similarity reading challenges that determinacy.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, ecumenical_councils, agenda_setter,
    institutional, civilizational, analytical, continental).

% Believe that any precise metaphysical definition of homoousios—whether strict identity, similarity, or subordination—commits the church to false certainty about the unknowable divine nature. They are formally excluded from the conciliar conversation: apophatic objections to the entire project of defining homoousios are not seated at the councils. Their position would dissolve the constraint by rejecting its premise.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, apophatic_skeptics, excluded,
    moderate, civilizational, identity_locked, continental).

% Analyze homoousios through Aristotelian metaphysical categories—substance, essence, accidents—and ask whether similarity and identity are coherent in terms of ousia/essence. They do not adjudicate doctrine but provide the conceptual machinery in which the dispute is conducted. Their philosophical frameworks make the honorific reading harder to sustain as the councils proceed.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, philosophical_aristotelians, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(homoousios_nicene__honorific_similarity_reading, ecumenical_councils).
narrative_ontology:fixing_cost_class(homoousios_nicene__honorific_similarity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Homoousios coordinates Nicene affiliation across bishops of different theological schools by permitting a single canonical term to accommodate functional unity (the Son's will united with the Father's in the economy of salvation) without requiring all parties to commit to identical metaphysical claims about essence. It solves the coordination problem: how to maintain conciliar unity while admitting theological pluralism.
% TRANSFER_FUNCTION: Transfers interpretive authority from the universal church (through councils) toward local episcopal discretion. Also transfers the burden of heresy exclusion from precise metaphysical definition toward pastoral judgment about whether a bishop's use of homoousios language shows adequate adherence to Nicene *intent* rather than Nicene *metaphysics*.
% ABSENT_VOICES: Apophatic skeptics who would object that any precise definition of homoousios—whether similarity or identity—exceeds the bounds of what can be known about the divine nature and should not be concilially mandated. Lay believers whose understanding of homoousios derives from liturgical usage (honorific language) rather than metaphysical precision are also absent: their practice underwrites the reading but they do not speak in councils. Non-Greek theological traditions (Syriac, Armenian, Coptic) whose conceptual machinery does not neatly map to Greek substance-language are marginalized.
% DISAPPEARANCE_RATIONALE: If this honorific-similarity reading were to disappear, the church would polarize into metaphysically incompatible camps that cannot mutually recognize homoousios assent as sufficient for communion. Strict Nicene enforcers would demand literal identity language; subordinationists would openly avow subordination; apophatic skeptics would press their objections. The councils' authority to establish a single orthodox position would collapse, replaced by competing canonical readings.
% FOUNDING_PROBLEM: The Council of Nicaea (325 CE) needed to condemn Arianism—the view that the Son is a creature and subordinate in being to the Father—without either imposing metaphysical uniformity that exceeded Christian tradition or using terms (homoousios) that lacked clear precedent. Homoousios was introduced as a term that could exclude Arianism while remaining intelligible across different theological schools.
% FOUNDING_PROBLEM_CORROBORATION: The councils themselves attest the need to condemn Arianism remains live; they also introduce successive clarifications of homoousios (Constantinople 381, Chalcedon 451), suggesting the term alone does not settle the matter. Semi-Arian moderates and apophatic theologians attest that strict metaphysical readings of homoousios strain the founding problem—which was to exclude Arianism, not to compel metaphysical identity. Historians outside the benefiting parties (both strict-equality and subordinationist parties) note that the honorific reading reflects how homoousios was actually interpreted in many regional churches for centuries after Nicaea, suggesting the founding problem was operationally solved by this reading rather than by strict metaphysical doctrine.
narrative_ontology:disappearance_verdict(homoousios_nicene__honorific_similarity_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_nicene__honorific_similarity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_nicene__honorific_similarity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(homoousios_nicene__honorific_similarity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_nicene__honorific_similarity_reading_tests).
:- end_tests(homoousios_nicene__honorific_similarity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is LOW (0.38) because this reading distributes interpretive authority downward to bishops rather than concentrating it in universal conciliar doctrine—it is a decentralizing arrangement that *reduces* extraction by councils at the cost of introducing ambiguity. Suppression is moderate (0.45) because the constraint persists through enforcement of conciliar authority, not through blocking exit—theologians can still hold gradualist views if they use Nicene language. The reading is CLAIMED as rope because the beneficiaries (semi-Arian moderates, apophatic traditions, local bishops) form a coordination coalition that gains from the ambiguity the reading permits. Theater ratio rises from 0.35 to 0.52 over the interval because enforcement shifts: initially (Nicaea) it focuses on the metaphysical content; by Chalcedon (451) councils increasingly police the *language* of assent rather than the underlying theology, making the theater of compliance more salient than the substance. Accessibility collapse is moderate (0.62): alternatives to homoousios language remain theoretically available (pure subordinationism, pure egalitarianism), but they are progressively excluded as heretical, so the alternatives collapse. Resistance stays high (0.71) because strict Nicene enforcers and subordinationists continue to object loudly rather than acquiesce; the constraint's persistence depends on councils' institutional power, not on persuading the parties.
 *
 * PERSPECTIVAL GAP:
 *   From the semi-Arian moderates' and local bishops' seats, this reading is a genuine coordination solution: it permits theological schools to remain in communion despite metaphysical differences. From the strict Nicene enforcers' seat, it is a failure of nerve, a retreat that reintroduces the subordinationism Nicaea was meant to exclude. From the hard subordinationists' seat, it is a heresy-boundary that deprives them of the very ambiguity they depend on. The engine should compute these seats differently: the beneficiary seats should show low-to-negative effective extraction (they gain from the ambiguity); the strict-enforcer and subordinationist seats should show high effective extraction (they bear the cost of the boundary). The theater ratio rising suggests that as the councils proceed, enforcement becomes more about policing language than doctrine, making the constraint operate increasingly as performative coordination rather than substantive agreement.
 *
 * DIRECTIONALITY LOGIC:
 *   Semi-Arian moderates and apophatic traditions are structural beneficiaries (d near 0.2–0.3): they retain theological autonomy while gaining Nicene recognition. Local episcopal authority benefits from discretionary power (d near 0.25). Strict Nicene enforcers and hard subordinationists are structural targets (d near 0.7–0.8): they must either suppress their metaphysical positions or accept heresy charges. The councils themselves occupy an ambiguous seat: they set the agenda (agenda_setter role) but their own stated authority is increasingly dependent on enforcing a reading whose metaphysical content they cannot uniformly articulate, creating internal pressure (d near 0.5). The directionality derivation should weight the identity_locked exit options heavily: theologians cannot exit the church community without losing their professional/spiritual identity, and subordinationists cannot exit Nicene orthodoxy without accepting heresy status. The constraint's persistence depends on that lock-in.
 *
 * MANDATROPHY ANALYSIS:
 *   The honorific-similarity reading avoids a key mandatrophy trap: if homoousios were strictly defined as metaphysical identity, bishops and theologians operating in apophatic or gradualist traditions would face increasing pressure to either affirm the metaphysics (intellectual dishonesty) or accept heresy status (institutional rupture). The honorific reading permits a *functional* solution (the Son's will is at one with the Father's; the economy of salvation is unified) without demanding metaphysical precision. This preserves the constraint's founding purpose—excluding Arianism—while avoiding the mandatrophy that would arise from enforcing uniform metaphysical doctrine. However, the rising theater_ratio suggests mandatrophy may be emerging in a different form: as councils proceed, enforcement becomes increasingly dependent on policing the *language* of homoousios assent rather than the underlying theology, creating a situation where bishops perform orthodoxy without genuine agreement about what the term means. This is the characteristic signature of a constraint whose real function (excluding subordinationism) begins to diverge from its stated function (affirming metaphysical identity), and the difference is managed through theatrical compliance rather than substantive persuasion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    honorific_vs_metaphysical_boundary,
    'Can homoousios function as an effective heresy boundary if interpreted as honorific similarity rather than metaphysical identity? Does the boundary still exclude Arianism?',
    'Historical analysis of how homoousios-affirming theologians actually responded to Arianism: did bishops who used honorific-similarity language maintain anti-Arian commitments, or did the interpretive flexibility permit neo-Arian accommodation?',
    'If the boundary holds under honorific interpretation (Arianism is excluded even if similarity rather than identity is affirmed), the reading satisfies its founding purpose and is a genuine coordination solution. If the boundary collapses (graduated subordination re-introduces Arianism by another name), the reading fails its function and becomes a false compromise—the metaphysical-equality reading becomes necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honorific_vs_metaphysical_boundary, empirical, 'Whether the honorific-similarity reading preserves the anti-Arian boundary that justifies Nicaea.').

omega_variable(
    reading_identity_fusion_bishops,
    'Do bishops'' professional and spiritual identities become fused with the honorific-similarity reading such that changing readings would constitute an identity rupture they avoid even when they doubt the reading''s truth?',
    'Historical evidence of bishops who privately held different metaphysical views but publicly affirmed honorific-similarity language to maintain their position and avoid heresy charges. Post-council analysis of whether bishops could revise their reading without losing institutional standing.',
    'If identity fusion occurs, the constraint''s suppression is partly internalized—bishops police their own language and belief to maintain their reading''s compatibility with their self-image as orthodox teachers. This would elevate the constraint''s effective suppression beyond the structural measure (0.45) and create a residual suppression that persists even if enforcement relaxes. It would also suggest the reading operates partly as a snare (identity-locked targets accepting extractive language to maintain their identity) and not purely as rope (coordination with net benefit).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_identity_fusion_bishops, empirical, 'Whether the reading''s persistence depends on identity fusion that makes revision costly for its bearers.').

omega_variable(
    apophatic_theology_incompleteness,
    'Does the apophatic tradition''s theological humility about the divine essence genuinely support the honorific-similarity reading, or does apophatic theology entail that *no* precise definition of homoousios (whether identity, similarity, or subordination) should be concilially enforced?',
    'Analysis of apophatic theologians'' actual writings: do they support honorific-similarity as the right interpretation of homoousios, or do they reject the entire conciliar project of defining it?',
    'If apophatic theology supports honorific-similarity, the reading gains a powerful philosophical ally and the beneficiary set is coherent. If apophatic theology entails rejection of all precise definitions, the apophatic-tradition beneficiary status is questionable—they might be using the honorific reading instrumentally while actually denying its premises. This would fragment the beneficiary coalition and reduce the reading''s stability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(apophatic_theology_incompleteness, conceptual, 'Whether apophatic theology genuinely endorses the honorific reading or rejects the concilial premise underlying it.').

omega_variable(
    kernel_underdetermination_reading_plurality,
    'Is this reading one coherent interpretation of the Nicene text, or is it a family of related but distinct readings (early-honorific vs. late-diplomatic vs. apophatic-adjacent) that get bundled under ''honorific-similarity'' for analytical convenience?',
    'Fine-grained textual and historical analysis: trace the specific bishops and theologians who articulate this reading, map the differences in their formulations, check whether they recognize each other as saying the same thing or holding incompatible positions.',
    'If the reading is genuinely coherent, it is one reading of the kernel and analysis proceeds normally. If it is a family of distinct readings, the constraint story is actually a cluster of constraints, each with different beneficiary/victim structures and different ε values. This would require decomposition per the ε-invariance principle: write separate constraint stories for (e.g.) early-honorific homoousios, late-diplomatic homoousios, and apophatic-adjacent homoousios, linked by network.affects_constraints.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_underdetermination_reading_plurality, conceptual, 'Whether honorific-similarity is a unitary reading or a family of distinct readings bundled for convenience.').

omega_variable(
    metaphysical_equality_reading_triumph,
    'Why does the metaphysical-equality reading (strict homoousios = ontological identity) eventually come to dominate conciliar doctrine, displacing the honorific-similarity reading? What structural conditions made the strict reading more durable?',
    'Historical periodization: map the councils (Nicaea 325, Constantinople 381, Chalcedon 451, and beyond) and track when the metaphysical-equality reading begins to be formalized as the required interpretation, at the expense of the honorific reading. Identify institutional, political, and theological pressures that shifted authority toward strict equality.',
    'If the metaphysical-equality reading triumphs because of theological arguments about the coherence of the boundary (the honorific reading fails to exclude subordinationism), that is evidence that the honorific reading''s founding purpose is actually unmet. If it triumphs for institutional reasons (centralized doctrinal authority, political pressure from imperial theology), the constraint dynamics shift: the strict reading becomes extractive (enforced by concentrated power) rather than coordinative. This would suggest the honorific reading was always a transitional arrangement, not a stable equilibrium—a rope that was displaced by a snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(metaphysical_equality_reading_triumph, empirical, 'The long-term dynamics of the kernel''s reading plurality and which readings achieve institutional dominance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_nicene__honorific_similarity_reading, 325, 451).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_tr_t325, homoousios_nicene__honorific_similarity_reading, theater_ratio, 325, 0.35).
narrative_ontology:measurement_basis(homo_tr_t325, observed).
narrative_ontology:measurement(homo_tr_t360, homoousios_nicene__honorific_similarity_reading, theater_ratio, 360, 0.41).
narrative_ontology:measurement_basis(homo_tr_t360, observed).
narrative_ontology:measurement(homo_tr_t381, homoousios_nicene__honorific_similarity_reading, theater_ratio, 381, 0.48).
narrative_ontology:measurement_basis(homo_tr_t381, observed).
narrative_ontology:measurement(homo_tr_t415, homoousios_nicene__honorific_similarity_reading, theater_ratio, 415, 0.5).
narrative_ontology:measurement_basis(homo_tr_t415, observed).
narrative_ontology:measurement(homo_tr_t451, homoousios_nicene__honorific_similarity_reading, theater_ratio, 451, 0.52).
narrative_ontology:measurement_basis(homo_tr_t451, observed).

% Extraction over time
narrative_ontology:measurement(homo_be_t325, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 325, 0.22).
narrative_ontology:measurement_basis(homo_be_t325, observed).
narrative_ontology:measurement(homo_be_t360, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 360, 0.28).
narrative_ontology:measurement_basis(homo_be_t360, observed).
narrative_ontology:measurement(homo_be_t381, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 381, 0.35).
narrative_ontology:measurement_basis(homo_be_t381, observed).
narrative_ontology:measurement(homo_be_t415, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 415, 0.37).
narrative_ontology:measurement_basis(homo_be_t415, observed).
narrative_ontology:measurement(homo_be_t451, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 451, 0.38).
narrative_ontology:measurement_basis(homo_be_t451, observed).

% Suppression requirement over time
narrative_ontology:measurement(homo_su_t325, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 325, 0.3).
narrative_ontology:measurement_basis(homo_su_t325, observed).
narrative_ontology:measurement(homo_su_t360, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 360, 0.37).
narrative_ontology:measurement_basis(homo_su_t360, observed).
narrative_ontology:measurement(homo_su_t381, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 381, 0.42).
narrative_ontology:measurement_basis(homo_su_t381, observed).
narrative_ontology:measurement(homo_su_t415, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 415, 0.44).
narrative_ontology:measurement_basis(homo_su_t415, observed).
narrative_ontology:measurement(homo_su_t451, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 451, 0.45).
narrative_ontology:measurement_basis(homo_su_t451, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_nicene__honorific_similarity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(homoousios_nicene__honorific_similarity_reading, 0.18).
narrative_ontology:affects_constraint(homoousios_nicene__honorific_similarity_reading, homoousios_nicene__metaphysical_equality_reading).
narrative_ontology:affects_constraint(homoousios_nicene__honorific_similarity_reading, homoousios_nicene__subordinationist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the homoousios_nicene kernel. The other readings (metaphysical_equality_reading and subordinationist_reading) instantiate different structural arrangements with different beneficiary/victim sets, ε values, and persistence mechanisms. All three share the same concilial text but diverge on what the text means, which parties benefit from each interpretation, and what enforcement cost is required to exclude alternatives. The three readings are not perspectives on one constraint—they are three distinct constraints instantiated by the same contested kernel. Network links establish that this reading's institutional pressure (rising theater_ratio, increasing reliance on language-policing) influences both sibling readings: the metaphysical-equality reading responds by progressively excluding honorific language, and the subordinationist reading responds by using language that mimics homoousios while preserving gradualist metaphysics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(homoousios_nicene__honorific_similarity_reading, organized, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
