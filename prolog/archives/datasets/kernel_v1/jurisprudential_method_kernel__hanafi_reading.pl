% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__hanafi_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__hanafi_reading, []).

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
 *   constraint_id: jurisprudential_method_kernel__hanafi_reading
 *   human_readable: Hanafi Jurisprudential Method: Qiyas, Ra'y, and Istihsan Framework
 *   domain: islamic_jurisprudence/legal_theory/institutional_pluralism
 *
 * SUMMARY:
 *   The Hanafi jurisprudential method represents one coherent reading of how
 *   Islamic law should be interpreted and applied. This reading privileges
 *   analogical reasoning (qiyas) and juristic reason (ra'y) as primary tools
 *   for legal deduction, with juristic preference (istihsan) serving as a
 *   secondary but valid source for overriding strict analogical conclusions
 *   when social welfare or textual harmony demands it. This framework has
 *   historically enabled the Hanafi school to adapt Islamic law across
 *   diverse geographic contexts (Ottoman Empire, South Asian subcontinent,
 *   contemporary Muslim-majority nations) and evolving social circumstances.
 *   The constraint exhibits structural tension: the same flexibility that
 *   enables legal adaptation across contexts also creates scope for jurists
 *   to reach preferred conclusions under the methodological guise of
 *   analogical reasoning, potentially extracting institutional power from
 *   those committed to stricter textual literalism. The Hanafi reading
 *   coexists with three sibling readings (Hanbali literalism, Maliki
 *   practice-based reasoning, Shafi'i standardized hierarchy), each rooted in
 *   the same textual sources but organized around different epistemic
 *   hierarchies and different accounts of what counts as valid reasoning.
 *
 * KEY AGENTS:
 *   - Hanafi Juristic Institution (institutional/arbitrage): Benefits from methodological flexibility enabling adaptation and institutional power across diverse contexts and time periods
 *   - Literalist Textualists (moderate/constrained): Face suppression through the elevation of analogical reasoning; experience the framework as constraining their preferred interpretive method but remain within tradition-bound contexts
 *   - Textual Purists (powerless/trapped): Completely unable to exit within dominant Islamic legal traditions; the Hanafi framework structurally privileges discretion over literalism; bear maximum extraction
 *   - Modernizing Reformers (organized/mobile): See Hanafi flexibility as historical scaffold for legal adaptation but believe modern institutions require replacement with more systematic, transparent methods
 *   - Scholastic Tradition Carriers (institutional/constrained): Maintain Hanafi method through institutional continuity; experience the framework as increasingly performative rather than functionally necessary
 *   - Analytical Observer (analytical/analytical): Risks naturalizing a contingent institutional arrangement as an inherent feature of jurisprudential reasoning
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__hanafi_reading, 0.35).
domain_priors:suppression_score(jurisprudential_method_kernel__hanafi_reading, 0.38).
domain_priors:theater_ratio(jurisprudential_method_kernel__hanafi_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__hanafi_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__hanafi_reading, "Hanafi Jurisprudential Method: Qiyas, Ra'y, and Istihsan Framework").
narrative_ontology:topic_domain(jurisprudential_method_kernel__hanafi_reading, "islamic_jurisprudence/legal_theory/institutional_pluralism").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__hanafi_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__hanafi_reading, 'a3259dc5-3701-4924-8075-ff82f4ffe142').
narrative_ontology:cs_kernel_codification('a3259dc5-3701-4924-8075-ff82f4ffe142', formalized).
narrative_ontology:cs_authority_grounding('a3259dc5-3701-4924-8075-ff82f4ffe142', lineage).
narrative_ontology:cs_interpretation_layer_present('a3259dc5-3701-4924-8075-ff82f4ffe142').
narrative_ontology:cs_reading_relation('a3259dc5-3701-4924-8075-ff82f4ffe142', jurisprudential_method_kernel__hanbali_reading, coexists_with).
narrative_ontology:cs_reading_relation('a3259dc5-3701-4924-8075-ff82f4ffe142', jurisprudential_method_kernel__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('a3259dc5-3701-4924-8075-ff82f4ffe142', jurisprudential_method_kernel__shafii_reading, influences).
narrative_ontology:cs_axiom('a3259dc5-3701-4924-8075-ff82f4ffe142', foundational, analogical_reasoning_as_primary_epistemic_tool).
narrative_ontology:cs_axiom_status(analogical_reasoning_as_primary_epistemic_tool, holdable).
narrative_ontology:cs_axiom_grounding('a3259dc5-3701-4924-8075-ff82f4ffe142', analogical_reasoning_as_primary_epistemic_tool, conventional).
narrative_ontology:cs_axiom('a3259dc5-3701-4924-8075-ff82f4ffe142', foundational, juristic_discretion_as_legitimate_adaptation_mechanism).
narrative_ontology:cs_axiom_status(juristic_discretion_as_legitimate_adaptation_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('a3259dc5-3701-4924-8075-ff82f4ffe142', juristic_discretion_as_legitimate_adaptation_mechanism, deontological).
narrative_ontology:cs_reference_frame('a3259dc5-3701-4924-8075-ff82f4ffe142', textually_grounded_juristic_flexibility).
narrative_ontology:cs_drift_state('a3259dc5-3701-4924-8075-ff82f4ffe142', contemporary_islamic_legal_modernization, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a3259dc5-3701-4924-8075-ff82f4ffe142', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanafi_reading, hanafi_jurists).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanafi_reading, institutional_flexibility).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanafi_reading, textual_literalists).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanafi_reading, standardized_method_advocates).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LITERALIST CRITIC (TANGLED ROPE) — Hanafi flexibility is experienced as both enabling (one can find juristic solutions to novel problems) and extractive (the scope for discretion allows jurists to reach predetermined conclusions under the guise of analogical reasoning). The critic faces significant barriers to exit — within the Hanafi tradition, the methodological commitments are embedded in centuries of jurisprudence — but also experiences the constraint as partially coordinating legitimate problem-solving in Islamic law.
constraint_indexing:constraint_classification(jurisprudential_method_kernel__hanafi_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 2: HANAFI INSTITUTION (ROPE) — The Hanafi jurisprudential method benefits from the flexibility built into qiyas, ra'y, and istihsan. This framework creates institutional power to adapt Islamic law to diverse contexts and changing conditions. The institution experiences the constraint as primarily coordinating: the method enables solving novel legal problems across Ottoman, South Asian, and contemporary Muslim-majority contexts. Arbitrage exit reflects institutional capacity to apply the method across multiple jurisdictions and time periods.
constraint_indexing:constraint_classification(jurisprudential_method_kernel__hanafi_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: TEXTUALIST VICTIM (SNARE) — From the position of a literalist scholar committed to strict textual adherence, the Hanafi framework appears as pure extraction. The victim cannot exit within the tradition — Hanafi jurisprudence is the dominant method across much of the Islamic world — and experiences the framework as suppressing literalist interpretations while elevating jurist discretion. Maximum experienced extraction: the framework structurally disadvantages those who reject analogical reasoning and juristic preference.
constraint_indexing:constraint_classification(jurisprudential_method_kernel__hanafi_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 4: MODERNIZING REFORMER (SCAFFOLD) — Contemporary Islamic legal reformers organized around adapting Islamic law to modern contexts see the Hanafi method as a temporary scaffold: the flexibility of qiyas and istihsan historically enabled jurisprudential evolution, but in the modern context, this should be replaced by more systematic, transparent, and participatory methods (e.g., juristic consensus committees, evidence-based methodology). The reformer experiences lower effective extraction because the framework is perceived as sunsetting — it is being superseded by more institutionalized approaches to Islamic legal pluralism.
constraint_indexing:constraint_classification(jurisprudential_method_kernel__hanafi_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: SCHOLASTIC TRADITION (PITON) — At the civilizational time horizon, the Hanafi method appears as an institutional inertia: the methodological framework is maintained because it is the received tradition, not primarily because it provides solutions unavailable through alternative methods. The theater ratio is high — much scholastic writing rehearses the inherited framework rather than generating genuinely novel legal reasoning. The constraint persists through institutional momentum, not functional necessity.
constraint_indexing:constraint_classification(jurisprudential_method_kernel__hanafi_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, the Hanafi framework might be seen as reflecting an immutable feature of jurisprudential reasoning itself: any legal system must balance textual fidelity with contextual adaptation, and some form of analogical reasoning (qiyas) and juristic discretion (ra'y) are inherent to law-application across diverse circumstances. However, the presence of identified beneficiaries (Hanafi institutions) triggers false-summit detection: the 'natural law' framing may naturalize a particular institutional arrangement that benefits specific juristic communities.
constraint_indexing:constraint_classification(jurisprudential_method_kernel__hanafi_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__hanafi_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(jurisprudential_method_kernel__hanafi_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(jurisprudential_method_kernel__hanafi_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(jurisprudential_method_kernel__hanafi_reading, TR),
    TR >= 0.70.

:- end_tests(jurisprudential_method_kernel__hanafi_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The Hanafi framework's flexibility in qiyas and ra'y creates genuine scope for juristic discretion that benefits Hanafi institutions. However, the discretion is not unconstrained — it operates within bounded analogical reasoning and is justified through textual reasoning rather than pure will. The measurement trajectory shows rising extractiveness over the interval (0.18 → 0.35), reflecting historical accumulation of jurisprudential refinements that progressively constrained the scope of analogical reasoning while elevating the sophistication of justifications. This is an extraction-accumulation signal: the framework becomes more extractive as jurisprudential elaboration makes the scope for discretion less transparent. Suppression (0.38): Moderate. Textual literalists face suppression through institutional elevation of analogical reasoning, but the suppression is not total — literalist scholars maintain positions within the tradition and their interpretations retain scholarly status. The bid'ah (innovation) charge serves as a suppression mechanism against those who explicitly reject the framework's core commitments. Theater ratio (0.45): Moderate. The Hanafi method contains genuine functional reasoning (solving novel legal problems through structured analogical extension) but also increasing performative content (elaborate justifications of conclusions that are sometimes predetermined). The trajectory shows rising theater over the interval, suggesting historical drift toward greater performative content as jurisprudential tradition becomes more elaborate.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap in this constraint is fundamental and structured. The Hanafi institution sees the framework as primarily coordinating (Rope) — enabling legal adaptation across diverse contexts. The literalist critic sees mixed coordination and extraction (Tangled Rope) — the framework enables solutions but also enables discretion. The textualist victim sees pure extraction (Snare) — complete suppression of preferred interpretive method with no exit. The modernizing reformer sees a temporary scaffold (Scaffold) — historically enabling but now being replaced by more systematic methods. The scholastic tradition sees institutional inertia (Piton) — the method persists through momentum, not necessity. The analytical observer risks seeing natural law (Mountain) — an inevitable feature of jurisprudential reasoning. This range reflects the constraint's genuine structural complexity: is the Hanafi method a coordination mechanism enabling legal pluralism, an extraction mechanism enabling juristic power, a temporary scaffold for legal modernization, an inert tradition, or a natural law? The answer depends on the observer's position and commitments.
 *
 * DIRECTIONALITY LOGIC:
 *   The Hanafi institution benefits from the methodological flexibility (beneficiary status, arbitrage exit → low d → negative χ from their perspective). Literalist critics experience constraints on their preferred method but with significant surmountable costs (moderate power, constrained exit → moderate d → moderate χ). Textualists committed to literalism are trapped by institutional embeddedness (powerless, trapped → high d → high χ). Modernizing reformers have exit pathways through institutional reform (organized, mobile → lower d → lower χ). The scholastic tradition is constrained but quasi-beneficiary through institutional position (institutional, constrained → moderate d). The analytical observer's position is ambiguous: if they naturalize the method as inevitable, they occupy a false-summit position where institutional beneficiaries hide behind universal necessity claims.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    qiyas_vs_shafii_strict_hierarchy,
    'Does Hanafi acceptance of qiyas and ra''y as primary sources constitute genuine methodological pluralism, or is it ultimately constrained by the same Quranic-Hadith foundation as Shafi''i methodology?',
    'Comparative analysis of divergent rulings produced by Hanafi vs Shafi''i methodologies on identical legal questions; assessment of whether the divergences derive from differing epistemic weightings (Hanafi flexibility) or from authentic differences in transmitted hadith and textual interpretation',
    'If genuine pluralism: Hanafi method is a distinct constraint with independent extraction mechanisms. If illusory: the appearance of flexibility conceals subordination to the same textual hierarchy, and the ''flexibility'' is theater masking deterministic application of rules.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(qiyas_vs_shafii_strict_hierarchy, conceptual, 'Whether Hanafi methodological flexibility is genuine pluralism or constrained by Quranic-Hadith hierarchy').

omega_variable(
    istihsan_as_juristic_preference_vs_coordinating_principle,
    'Is istihsan (juristic preference) a valid coordinating tool for adapting law to social welfare, or is it a mechanism for extractive juristic override of textual constraints?',
    'Historical analysis of istihsan applications: tracking instances where istihsan produced rulings that deviate from literal qiyas outcomes; assessing whether deviations consistently serve documented social welfare purposes or reflect jurist preferences unmoored from public benefit',
    'If coordinating principle: istihsan is rope-function enabling legal adaptation to real circumstances. If extractive override: istihsan is snare-function allowing jurists to reach predetermined conclusions. Classification shifts between tangled_rope (mixed) and snare (pure extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(istihsan_as_juristic_preference_vs_coordinating_principle, empirical, 'Whether istihsan functions as coordination or extraction mechanism').

omega_variable(
    hanafi_flexibility_and_bidah_charge_asymmetry,
    'Does the Hanafi framework''s broader acceptance of analogical reasoning and juristic discretion create asymmetric vulnerability to bid''ah (innovation) charges compared to literalist methodologies?',
    'Comparative historical documentation of bid''ah accusations leveled against Hanafi scholars vs literalist scholars; tracking the frequency and severity of innovation charges and their institutional consequences across methodological schools',
    'If asymmetry confirmed: broader victim set for Hanafi flexibility (textual conservatives) than for literalist constraints (method conservatives); suppression mechanism is the bid''ah charge itself. If no asymmetry: the framework''s flexibility does not create differential vulnerability, and the ''broader victim set'' is a mischaracterization.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hanafi_flexibility_and_bidah_charge_asymmetry, empirical, 'Asymmetric vulnerability of Hanafi flexibility to innovation charges').

omega_variable(
    kernel_reading_contestation_resolution,
    'Does this Hanafi reading genuinely foreclose, coexist with, or influence the alternative readings (Hanbali literalism, Maliki practice-based reasoning, Shafi''i standardized hierarchy)?',
    'Historical and contemporary jurisprudential analysis: examining whether jurists holding different methodological commitments can coexist within unified Islamic legal frameworks, whether one reading logically rules out another, or whether readings create structural pressures on each other without logical foreclosure',
    'If forecloses: the Hanafi reading is incompatible with alternatives; institutional unification requires choosing one. If coexists: methodological pluralism is structurally possible. If influences: one reading changes conditions for others without eliminating them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation_resolution, conceptual, 'Logical and structural relationship between Hanafi reading and sibling jurisprudential methodologies').

omega_variable(
    false_summit_naturalization_check,
    'Is the Hanafi methodological framework a natural, inevitable feature of jurisprudential reasoning (mountain), or is it a contingent institutional arrangement that benefits specific juristic communities (false summit)?',
    'Comparative jurisprudence: examining non-Islamic legal systems for analogous flexibility/rigidity profiles; assessing whether analogical reasoning (qiyas) and juristic discretion (ra''y) are inherent to all legal systems or are specific institutional choices that could be otherwise',
    'If natural law: the framework is unchangeable and mountain classification stands. If institutional arrangement: false-summit signature fires, reclassifying to tangled_rope and revealing naturalizing discourse as extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_naturalization_check, conceptual, 'Whether Hanafi method is natural law of jurisprudence or institutional arrangement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__hanafi_reading, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hanafi_juris_tr_t0, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(hanafi_juris_tr_t4, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 4, 0.4).
narrative_ontology:measurement(hanafi_juris_tr_t8, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 8, 0.45).

% Extraction over time
narrative_ontology:measurement(hanafi_juris_be_t0, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(hanafi_juris_be_t4, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 4, 0.28).
narrative_ontology:measurement(hanafi_juris_be_t8, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 8, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(hanafi_juris_su_t0, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(hanafi_juris_su_t4, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 4, 0.32).
narrative_ontology:measurement(hanafi_juris_su_t8, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 8, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__hanafi_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel__hanbali_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel__shafii_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel__maliki_reading).

% DUAL FORMULATION NOTE:
% The Hanafi reading is one component of the jurisprudential_method_kernel constraint family. Each reading (Hanafi, Hanbali, Maliki, Shafi'i) is instantiated as a separate constraint story with its own ε value, structural relationships, and institutional effects. The network edges link them as members of a family where each reading both coexists and influences others. The constraint family exhibits a presheaf structure: different readings occupy different institutional positions with different extraction profiles, yet they all organize themselves around the same underlying kernel (valid Islamic jurisprudential authority). The Hanafi reading's ε=0.35 reflects moderate mixed extraction-coordination; sibling readings are expected to show different ε values reflecting their different institutional effects and beneficiary/victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jurisprudential_method_kernel__hanafi_reading, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
