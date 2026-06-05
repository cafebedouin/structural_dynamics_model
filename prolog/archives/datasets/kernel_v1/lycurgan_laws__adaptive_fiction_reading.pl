% ============================================================================
% CONSTRAINT STORY: lycurgan_laws__adaptive_fiction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lycurgan_laws__adaptive_fiction_reading, []).

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
 *   constraint_id: lycurgan_laws__adaptive_fiction_reading
 *   human_readable: Lycurgan Laws as Adaptive Fiction (Institutional Flexibility Masking Immutability Claim)
 *   domain: political_philosophy/constitutional_theory/commitment_systems
 *
 * SUMMARY:
 *   The Lycurgan Laws of Sparta present a canonical paradox in constitutional
 *   theory: a legal system claimed as immutable and divinely ordained yet
 *   documented as undergoing continuous adaptive reinterpretation by
 *   magistrates (ephors and kings) across centuries. This constraint reading
 *   argues that the immutability claim itself is a functional institutional
 *   technology — a noble lie that provides legitimacy cover for the flexible
 *   governance required to respond to crises (wars, demographic shifts,
 *   economic pressures) while maintaining the identity and unity of the
 *   Spartan polity. Unlike the sacral fidelity reading (which sees the laws
 *   as genuinely enforced despite attempts at subversion) or the demographic
 *   trap reading (which sees rigidity as structurally fatal), the adaptive
 *   fiction reading identifies the constraint as a tangled rope: the system
 *   coordinates genuine collective benefits (military stability, social
 *   cohesion, durable institutional identity) while extracting asymmetric
 *   gains for the ruling coalition (ephors and kings who control
 *   reinterpretation) and the homoioi class (who benefit from subordination
 *   of periokoi under the guise of timeless law). The constraint's
 *   extractiveness (0.38) is moderate — not extraction through mere force,
 *   but extraction through controlled reinterpretation of the law's meaning
 *   combined with suppression of transparent debate about what is being
 *   reinterpreted. Theater ratio (0.68) reflects that the performative
 *   dimension has increased over time: by the Hellenistic period, invocations
 *   of Lycurgan immutability are largely disconnected from actual adaptive
 *   practice, suggesting the constraint has degraded toward piton status.
 *
 * KEY AGENTS:
 *   - Spartan Ruling Coalition (Ephors & Kings): institutional/arbitrage — Primary beneficiary. Controls reinterpretation of Lycurgan law; hidden adaptation allows them to govern flexibly without fracturing legitimacy claims. Benefits from immutability fiction that provides cover for de facto authority.
 *   - Homoioi (Spartan Citizen Body): moderate/constrained — Secondary beneficiary and victim. Experience coordination benefits (military effectiveness, institutional stability, identity binding) alongside extraction (elaborate constraints on property, mobility, familial autonomy) justified by appeal to immutable law. Identity locked into Spartan citizenship; exit is social death.
 *   - Periokoi (Subject Population): powerless/trapped — Primary victim. Subordinated under laws presented as immutable yet reinterpreted away by ruling coalition. Extraction through subordination; no transparent mechanism to contest reinterpretation.
 *   - Classical Philosophical Tradition (Aristotle, Plutarch): organized/arbitrage — Institutional observer documenting both immutability claim and covert adaptations. Benefits from interpretive authority over Lycurgan meaning; coordination function (preserving classical knowledge).
 *   - Spartan Demographic Sustainability: powerless/trapped — Structural victim. Demographic decline (30,000 → 3,000 homoioi) reflects either enforcement rigidity (sacral fidelity reading) or failure to adapt openly (this reading's claim that covert adaptation prevented transparent structural reform).
 *   - Legislative Transparency: institutional/trapped — Systemic victim. The adaptive fiction mechanism suppresses open debate about what laws mean and how they change, preventing democratic deliberation on constitutional meaning.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lycurgan_laws__adaptive_fiction_reading, 0.38).
domain_priors:suppression_score(lycurgan_laws__adaptive_fiction_reading, 0.62).
domain_priors:theater_ratio(lycurgan_laws__adaptive_fiction_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lycurgan_laws__adaptive_fiction_reading, tangled_rope).
narrative_ontology:human_readable(lycurgan_laws__adaptive_fiction_reading, "Lycurgan Laws as Adaptive Fiction (Institutional Flexibility Masking Immutability Claim)").
narrative_ontology:topic_domain(lycurgan_laws__adaptive_fiction_reading, "political_philosophy/constitutional_theory/commitment_systems").

domain_priors:requires_active_enforcement(lycurgan_laws__adaptive_fiction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lycurgan_laws__adaptive_fiction_reading, '60d86aa7-dab9-47e9-ae56-81b3695e7f22').
narrative_ontology:cs_kernel_codification('60d86aa7-dab9-47e9-ae56-81b3695e7f22', fixed_text).
narrative_ontology:cs_authority_grounding('60d86aa7-dab9-47e9-ae56-81b3695e7f22', extraction).
narrative_ontology:cs_interpretation_layer_present('60d86aa7-dab9-47e9-ae56-81b3695e7f22').
narrative_ontology:cs_reading_relation('60d86aa7-dab9-47e9-ae56-81b3695e7f22', lycurgan_laws__sacral_fidelity_reading, forecloses).
narrative_ontology:cs_reading_relation('60d86aa7-dab9-47e9-ae56-81b3695e7f22', lycurgan_laws__demographic_trap_reading, influences).
narrative_ontology:cs_axiom('60d86aa7-dab9-47e9-ae56-81b3695e7f22', foundational, lycurgan_flexibility_by_design).
narrative_ontology:cs_axiom_status(lycurgan_flexibility_by_design, holdable).
narrative_ontology:cs_axiom_grounding('60d86aa7-dab9-47e9-ae56-81b3695e7f22', lycurgan_flexibility_by_design, empirically_contingent).
narrative_ontology:cs_axiom('60d86aa7-dab9-47e9-ae56-81b3695e7f22', foundational, immutability_claim_as_legitimacy_cover).
narrative_ontology:cs_axiom_status(immutability_claim_as_legitimacy_cover, holdable).
narrative_ontology:cs_axiom_grounding('60d86aa7-dab9-47e9-ae56-81b3695e7f22', immutability_claim_as_legitimacy_cover, deontological).
narrative_ontology:cs_reference_frame('60d86aa7-dab9-47e9-ae56-81b3695e7f22', lycurgan_immutability_doctrine).
narrative_ontology:cs_drift_state('60d86aa7-dab9-47e9-ae56-81b3695e7f22', hellenistic_period_atrophy, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('60d86aa7-dab9-47e9-ae56-81b3695e7f22', '').
narrative_ontology:cs_kernel_id(lycurgan_laws__adaptive_fiction_reading, lycurgan_laws).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, spartan_ruling_coalition).
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, ephorate_magistracy).
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, kingly_authority).
narrative_ontology:constraint_victim(lycurgan_laws__adaptive_fiction_reading, periokoi_underclass).
narrative_ontology:constraint_victim(lycurgan_laws__adaptive_fiction_reading, spartan_demographic_sustainability).
narrative_ontology:constraint_victim(lycurgan_laws__adaptive_fiction_reading, legislative_transparency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PERIOKOI CLASS (SNARE) — Bound by laws presented as immutable natural order yet adapted away by ruling coalition through interpretive manipulation. No avenue to contest reinterpretation; extraction through subordination justified by appeal to unchangeable constitutional structure. High suppression (unable to question the legal framework) combined with hidden extractive adjustments (benefits flow to rulers through reinterpreted provisions).
constraint_indexing:constraint_classification(lycurgan_laws__adaptive_fiction_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: SPARTAN CITIZEN BODY (TANGLED ROPE) — Experience genuine coordination benefit from Lycurgan stability rhetoric (identity binding, military cohesion, institutional continuity) alongside asymmetric extraction (elaborate rules constrain personal property and mobility while benefiting the ruling core). The citizen body has constrained exits — abandoning Spartan identity carries generational social death. Mixed experience: real coordination, real extraction.
constraint_indexing:constraint_classification(lycurgan_laws__adaptive_fiction_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: RULING COALITION / EPHORATE & KINGSHIP (ROPE) — Primary beneficiary. Experience the constraint as pure coordination: Lycurgan immutability rhetoric provides cover for adapting laws through interpretation without fracturing legitimacy claims. The coalition has arbitrage exits (can reinterpret Lycurgan law, shift between literal and figurative readings, or appeal to necessity exceptions). Effective extraction runs toward this agent — low experienced extractiveness because they control the constraint's meaning.
constraint_indexing:constraint_classification(lycurgan_laws__adaptive_fiction_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: CLASSICAL PHILOSOPHICAL TRADITION (TANGLED ROPE) — Organized scholarly observers document both the immutability claim and the covert adaptations, creating an institutional-level tension. The tradition benefits from the Lycurgan paradox as a canonical case study (coordination function: models how constitutional systems balance stability with adaptation) while also extracting from it (interpretive authority over Spartan meaning). This perspective shows the constraint's pull across multiple institutional domains.
constraint_indexing:constraint_classification(lycurgan_laws__adaptive_fiction_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: LYCURGAN RHETORIC MACHINE (PITON) — The performance of immutability persists through institutional theater even as adaptive reinterpretation is ongoing. Modern invocations of 'Lycurgan stability' are largely disconnected from actual Spartan practice (which was fluid). The theater ratio is high (0.68) because the symbolic function of Lycurgan immutability claim persists even after its operational core has been hollowed out by adaptation. This is a degraded constraint maintained by narrative inertia.
constraint_indexing:constraint_classification(lycurgan_laws__adaptive_fiction_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / CONSTITUTIONAL PARADOX (MOUNTAIN) — From a universal/civilizational perspective, any constitution faces an immutable tension: claims to permanence yet requires adaptation to survive. This perspective treats the Lycurgan paradox as revealing a fundamental structural law of constitutional systems — immutability rhetoric is inherent to legitimacy, yet no system can actually remain rigid. However, this mountain classification may be a false summit: the constraint is not a law of nature but a specific institutional arrangement where the ruling coalition deliberately chose to hide adaptation behind immutability rhetoric. The perspectival gap reveals whether this is structural necessity or contingent strategy.
constraint_indexing:constraint_classification(lycurgan_laws__adaptive_fiction_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lycurgan_laws__adaptive_fiction_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(lycurgan_laws__adaptive_fiction_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(lycurgan_laws__adaptive_fiction_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(lycurgan_laws__adaptive_fiction_reading, TR),
    TR >= 0.70.

:- end_tests(lycurgan_laws__adaptive_fiction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts asymmetric benefit for the ruling coalition (ephors, kings) through controlled reinterpretation while the broader citizen body experiences mixed benefits and costs. This is not pure extraction because genuine coordination benefits exist (military stability, institutional durability, collective identity). The extractiveness is masked by the immutability claim, which suppresses transparent accounting of who benefits from which reinterpretations. Suppression (0.62): Moderate-high. Structural barriers to challenging the constraint include the sacredness of Lycurgan law, the integration of the laws into civic identity and education, the concentration of interpretive authority in the magistracy, and the difficulty of organically changing or openly contesting rules presented as immutable. The periokoi face the highest suppression (trapped, no mechanisms for contesting subordination); the homoioi face medium suppression (constrained by identity lock; can question within Spartan frameworks but exit is impossible). Theater ratio (0.68): Moderate-high. The performative dimension has increased over the interval (from 0.55 to 0.68) as adaptations have become more frequent and the disjunction between immutability claim and flexible practice has widened. By the Hellenistic period, modern scholarship recognizes that Lycurgan law is largely a constructed narrative (Plutarch's Life of Lycurgus is more hagiography than history), yet the theater of immutability persists in institutional memory.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exemplifies how the same structural mechanism generates radically different classifications depending on observer position. The ruling coalition sees pure coordination (Rope) — they are solving the legitimate problem of adapting governance while maintaining legitimacy and identity. The periokoi see pure extraction (Snare) — subordination justified by appeal to law they cannot contest. The citizen body sees tangled rope (mixed benefits and extraction, identity-locked exit). The philosophical tradition sees tangled rope (genuine knowledge coordination alongside interpretive authority). The piton perspective shows that the constraint's performative dimension has outgrown its functional core. The analytical/mountain perspective risks naturalizing what is actually a contingent institutional strategy as a law of constitutional necessity. The false summit detector identifies the beneficiaries (ruling coalition) and triggers reclassification from mountain to tangled rope at the analytical level.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality (d value) for each agent derives from their structural position: beneficiary/victim status, power level, and exit capacity. The ruling coalition are beneficiaries with arbitrage (can interpret law, can appeal to necessity exceptions, can shift between readings) → low d → low experienced extractiveness (they see this as coordination). The homoioi are mixed beneficiary-victims with identity_locked exit (benefit from institutional stability and security, but constrained by elaborate rules and identity fusion) → moderate d → moderate experienced extractiveness. The periokoi are pure victims with trapped exit (no power to contest reinterpretation, no alternative social position in Spartan framework) → high d → high experienced extractiveness. The ruling coalition's arbitrage exit option is critical: they can change what the law means through interpretive fiat, so they experience the constraint as enabling rather than binding. The periokoi have no such reinterpretative power, so they experience the constraint as fixed and extractive.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the six-type distribution is not a classification error but a revelation of structural differentiation. The same set of base properties (extractiveness 0.38, suppression 0.62, theater 0.68) produces six distinct classifications because the observer positions are genuinely different. The beneficiaries experience rope (coordination); the primary victims experience snare (extraction); the secondary victims experience tangled rope (mixed); the organized observers experience tangled rope (both coordination and interpretive authority); the institutional theater (piton) reflects degradation; the analytical perspective risks mountain (false summit). The mandatrophy is resolved not by choosing one type but by recognizing that the presheaf of classifications across observation sites is the answer — this is what it looks like when an institutional system extracts asymmetrically while maintaining a coordination fiction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    immutability_vs_documented_adaptation,
    'Are the documented adaptive reinterpretations (ephoral expansion, kingly treaty modifications, property law relaxation) genuine deviations from Lycurgan original intent, or were such adaptations inscribed in the original code''s ambiguity by design?',
    'Textual analysis of primary sources (Plutarch''s Life of Lycurgus, Aristotle''s Constitution of Athens) for explicit language permitting or prohibiting interpretation; comparison with fragmentary inscriptional evidence; analysis of whether adaptations cluster at moments of external pressure (war, demographic crisis) or occur continuously',
    'If inscribed by design: the constraint is deliberately crafted rope (coordination through structured flexibility), not tangled rope (hidden extraction behind immutability lie). Reclassification → Rope across multiple perspectives. If genuine deviations: constraint is indeed tangled rope (extraction masked by immutability fiction). If indeterminate: remains tangled rope with unresolved ambiguity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(immutability_vs_documented_adaptation, empirical, 'Whether adaptive reinterpretations are deviations or inscribed flexibility').

omega_variable(
    demographic_cause_chain,
    'Did demographic decline (drop from ~30,000 homoioi in classical period to <3,000 by Hellenistic period) result from enforcement rigidity (the sacral fidelity reading''s claim) or from adaptive failures (this reading''s claim that covert adaptation prevented facing structural problems directly)?',
    'Longitudinal demographic modeling including birth rates, adoption restrictions, land concentration effects; correlation analysis between documented adaptive reinterpretations and demographic inflection points; comparative analysis with other Greek poleis that faced similar constraints without demographic collapse',
    'If rigidity: sacral fidelity reading is correct — laws were enforced as immutable with demographic cost. If adaptive failures: this reading is correct — hidden adaptation prevented structural reform, creating long-term unsustainability. If both mechanisms operated at different periods: temporal decomposition needed (different constraints at different epochs).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_cause_chain, empirical, 'Whether demographic decline resulted from enforcement rigidity or adaptive failure').

omega_variable(
    contemporaneous_awareness,
    'Did Spartan elites and the general citizen body understand the distinction between Lycurgan immutability claim and actual adaptive practice? Or was the adaptation so subtle (or the rhetorical control so complete) that the population experienced immutability as genuine?',
    'Analysis of private correspondence, trial records, and documentary evidence for explicit discussion of reinterpretation; examination of whether challengers to ephoral or kingly decisions appealed to Lycurgan immutability against adaptation claims; study of whether educational curriculum and public rhetoric emphasized different aspects of the law to different audiences',
    'If widely understood: constraint is extraction mechanism (beneficiaries knowingly deceive); theater ratio should be lower. If genuinely believed: constraint is coordination mechanism with high theater (the deception is institutional-level cognitive capture). If segmented by class: different constraints for different populations.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(contemporaneous_awareness, empirical, 'Contemporary Spartan awareness of adaptation behind immutability claim').

omega_variable(
    reading_frame_ambiguity,
    'Is the adaptive fiction reading claiming that Lycurgas intended hidden flexibility (the law was designed with covert adaptation baked in) or that later rulers subverted Lycurgas''s immutable design through reinterpretation?',
    'This is a committer-frame question: the reading must declare its epistemic stance. The resolution is not empirical but definitional — the reading must explicitly ground whether ''adaptive fiction'' means ''Lycurgus designed the system for covert flexibility'' or ''later rulers corrupted an originally rigid system.'' Both positions are empirically underdetermined by the sources.',
    'If Lycurgus designed flexibility: constraint is Rope (intentional coordination through reinterpretable language). If rulers corrupted: constraint is Tangled Rope (extraction through deceptive adaptation of original immutability). Reading must clarify which axiom it holds (see cs_structure.axioms).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_frame_ambiguity, conceptual, 'Whether adaptive fiction is Lycurgan design or later subversion').

omega_variable(
    false_summit_constitutional_necessity,
    'Is the mountain classification at the analytical level capturing a genuine structural law (all constitutions require hidden adaptation to survive) or naturalizing a contingent institutional choice (Spartan rulers chose to hide adaptation)?',
    'Comparative constitutional analysis: do other rigid constitutions achieve stability through explicit amendment procedures rather than hidden reinterpretation? Can constitutions maintain legitimacy while openly adapting? Does the necessity for hidden adaptation correlate with specific institutional structures (sacral grounding, anti-democratic framing) or appear universally?',
    'If universal necessity: mountain classification is warranted. If contingent: mountain is a false summit (naturalized institutional strategy). False summit detection triggers when beneficiaries (ruling coalition) are identified as benefiting from the immutability claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_constitutional_necessity, conceptual, 'Whether constitutional adaptation necessity is universal law or institutional contingency').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lycurgan_laws__adaptive_fiction_reading, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lycurg_adapt_theater_0, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(lycurg_adapt_theater_200, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 200, 0.62).
narrative_ontology:measurement(lycurg_adapt_theater_400, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 400, 0.68).

% Extraction over time
narrative_ontology:measurement(lycurg_adapt_extract_0, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(lycurg_adapt_extract_200, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 200, 0.35).
narrative_ontology:measurement(lycurg_adapt_extract_400, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 400, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(lycurg_adapt_suppress_0, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(lycurg_adapt_suppress_200, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 200, 0.62).
narrative_ontology:measurement(lycurg_adapt_suppress_400, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 400, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lycurgan_laws__adaptive_fiction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(lycurgan_laws__adaptive_fiction_reading, 0.18).
narrative_ontology:affects_constraint(lycurgan_laws__adaptive_fiction_reading, lycurgan_laws__sacral_fidelity_reading).
narrative_ontology:affects_constraint(lycurgan_laws__adaptive_fiction_reading, lycurgan_laws__demographic_trap_reading).
narrative_ontology:affects_constraint(lycurgan_laws__adaptive_fiction_reading, constitutional_amendment_via_interpretation).

% DUAL FORMULATION NOTE:
% The three Lycurgan readings (adaptive_fiction, sacral_fidelity, demographic_trap) decompose a single contested kernel into three distinct constraint stories with different ε values. Adaptive fiction (this story, ε=0.38) models coordination through hidden flexibility. Sacral fidelity (sibling, ε≈0.25) models attempted enforcement of rigidity despite subversion. Demographic trap (sibling, ε≈0.72) models structural unsustainability of rigidity. Each reading instantiates a different axiom about what the constraint IS (intentional flexibility, sacred immutability, fatal inflexibility). Network edges preserve the kernel family structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(lycurgan_laws__adaptive_fiction_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
