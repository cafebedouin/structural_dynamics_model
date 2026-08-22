% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_function__hybrid_transformation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_function__hybrid_transformation_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: catastrophe_memory_function__hybrid_transformation_reading
 *   human_readable: Catastrophe Memory Function: Hybrid Mourning-Survival Ritual Encoding
 *   domain: religious/social/cognitive
 *
 * SUMMARY:
 *   Passover and similar catastrophe-memory rituals encode a dual function:
 *   they preserve explicit loss-memory and group boundary-identity (D1/D4
 *   mourning practice — bitter herbs, fasting restrictions, recitation of
 *   displacement trauma) while simultaneously transmitting
 *   institutional-survival knowledge (D5 — decentralized household-level
 *   leadership, flexibility under external threat, institutional persistence
 *   without central authority). This hybrid reading claims that the ritual is
 *   neither pure mourning-practice nor pure survival-competence instruction,
 *   but a single constraint that solves both functions through one
 *   coordinated annual enactment. The bitter herbs evoke grief and loss; the
 *   seder structure (household-led, no temple required, no hierarchical
 *   authority) encodes the institutional blueprint for surviving without
 *   centralized power. Grief and adaptation are bound together in a single
 *   ritual act — the community grieves the catastrophe AND rehearses how to
 *   persist through catastrophic institutional loss. The reading is
 *   contested: sibling readings emphasize either the mourning-function
 *   (boundary maintenance, identity preservation through memorial obligation)
 *   or the survival-competence function (transmission of adaptive
 *   institutional capacity) as primary. This constraint story instantiates
 *   the hybrid reading that claims both functions are integral and mutually
 *   supporting.
 *
 * KEY AGENTS:
 *   - ritual_practitioners: those who enact the ritual annually, carrying both emotional labor (mourning) and knowledge transmission (adaptive mechanisms)
 *   - memory_bearers: communities and families bound by genealogy and identity to carry forward the loss-narrative and institutional wisdom
 *   - adaptive_institutions: rabbinic structures, synagogues, community councils that benefit from the implicit institutional-design knowledge encoded in the decentralized ritual form
 *   - catastrophe_survivors: those who directly experienced the events — structurally excluded from voice in the ritual's contemporary form; their testimony is mediated through liturgical language
 *   - rival_memory_frameworks: secular, nationalist, trauma-psychology, or non-commemorative readings of the same historical events — excluded from legitimacy within the ritual's truth-making apparatus
 *   - religious_authority: maintains the ritual's interpretive canon, decides which innovations and updates are legitimate, adjudicates performance disputes
 *   - observer_ritual_theorist: analyzes the dual function and its coherence, examines how the hybrid encoding structures the community's cognitive and institutional relationship to catastrophe
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__hybrid_transformation_reading, 0.38).
domain_priors:suppression_score(catastrophe_memory_function__hybrid_transformation_reading, 0.12).
domain_priors:theater_ratio(catastrophe_memory_function__hybrid_transformation_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__hybrid_transformation_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_function__hybrid_transformation_reading, "Catastrophe Memory Function: Hybrid Mourning-Survival Ritual Encoding").
narrative_ontology:topic_domain(catastrophe_memory_function__hybrid_transformation_reading, "religious/social/cognitive").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__hybrid_transformation_reading, '09d34650-d3c0-4a13-a6d1-68501c9102fb').
narrative_ontology:cs_kernel_codification('09d34650-d3c0-4a13-a6d1-68501c9102fb', fixed_text).
narrative_ontology:cs_authority_grounding('09d34650-d3c0-4a13-a6d1-68501c9102fb', lineage).
narrative_ontology:cs_interpretation_layer_present('09d34650-d3c0-4a13-a6d1-68501c9102fb').
narrative_ontology:cs_reading_relation('09d34650-d3c0-4a13-a6d1-68501c9102fb', catastrophe_memory_function__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('09d34650-d3c0-4a13-a6d1-68501c9102fb', catastrophe_memory_function__survival_competence_reading, coexists_with).
narrative_ontology:cs_axiom('09d34650-d3c0-4a13-a6d1-68501c9102fb', foundational, grief_and_adaptation_unified).
narrative_ontology:cs_axiom_status(grief_and_adaptation_unified, holdable).
narrative_ontology:cs_axiom_grounding('09d34650-d3c0-4a13-a6d1-68501c9102fb', grief_and_adaptation_unified, deontological).
narrative_ontology:cs_axiom('09d34650-d3c0-4a13-a6d1-68501c9102fb', secondary, institutional_survival_requires_memory).
narrative_ontology:cs_axiom_status(institutional_survival_requires_memory, holdable).
narrative_ontology:cs_axiom_grounding('09d34650-d3c0-4a13-a6d1-68501c9102fb', institutional_survival_requires_memory, instrumental).
narrative_ontology:cs_reference_frame('09d34650-d3c0-4a13-a6d1-68501c9102fb', ritual_as_dual_epistemic_container).
narrative_ontology:cs_drift_state('09d34650-d3c0-4a13-a6d1-68501c9102fb', contemporary_secular_assimilation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('09d34650-d3c0-4a13-a6d1-68501c9102fb', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__hybrid_transformation_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, community_continuity).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, collective_memory_preservation).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, adaptive_institutional_capacity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, ritual_practitioners).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, memory_bearers).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, adaptive_institutions).
narrative_ontology:constraint_victim(catastrophe_memory_function__hybrid_transformation_reading, ritual_practitioners).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__hybrid_transformation_reading, ritual_dual_epistemic_function).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__hybrid_transformation_reading, loss_and_adaptation_simultaneity).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__hybrid_transformation_reading, embodied_transmission_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate in the ritual annually, enacting both the commemoration (mourning the exodus losses and displacement trauma) and the rehearsal (performing institutional survival and adaptive transformation protocols). They shoulder the time cost and emotional labor of sustaining the ritual; they also absorb its dual epistemic content — both grieving and learning institutional flexibility.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, ritual_practitioners, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_function__hybrid_transformation_reading, ritual_practitioners, payer).

% Carry forward the narrative of collective catastrophe and institutional transformation. They are bound to the memory-transmission function by family, community, and identity — the ritual constrains them to remember in a specific structured form (bitter herbs as mourning marker, seder as institutional-survival blueprint), but this constraint is also the mechanism by which the loss does not vanish from collective consciousness.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, memory_bearers, beneficiary,
    moderate, generational, identity_locked, global).

% The ritual encodes institutional flexibility and decentralized continuity as survival mechanisms — no temple, no centralized authority, distributed household performance. Institutions (synagogue, rabbinic authority structures, community councils) benefit from the implicit institutional-design wisdom the ritual rehearses: how to persist through catastrophe without hierarchical bottlenecks. The seder's decentralized structure is both commemoration strategy and institutional blueprint.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, adaptive_institutions, beneficiary,
    institutional, civilizational, mobile, global).

% Those who directly experienced the catastrophic event (exodus, diaspora, pogroms, genocide) have no voice in the ritual's contemporary form — the ritual mediates their experience rather than amplifying it. Their testimony is channeled through structured liturgical language rather than direct survivor witness. Post-catastrophe generations appropriate their memory-work within a pre-set ritual frame.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, catastrophe_survivors, excluded,
    powerless, biographical, trapped, local).

% Alternative readings of the same historical events (secular nationalist narratives, trauma-psychology framings, non-commemorative institutional models) are structurally outside the ritual's truth-making apparatus. Those seeking to transmit adaptive institutional knowledge or commemorate loss outside the ritual's specific encoding are excluded from its coordinating function — the ritual's duality constrains which other memory-practices can coexist legitimately within the community.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, rival_memory_frameworks, excluded,
    moderate, generational, constrained, global).

% Maintains the ritual's interpretive canon, decides which innovations are legitimate (e.g., contemporary political references, women's ritual roles, diaspora innovations), and adjudicates disputes over proper performance. The agenda-setter role is distributed across rabbinic lineages, community leaders, and textual traditions, but the function — authorizing what counts as correct mourning and correct institutional-survival rehearsal — concentrates the power to update the ritual's encoding.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, religious_authority, agenda_setter,
    institutional, civilizational, mobile, global).

% Analyzes the ritual's dual function: how it simultaneously encodes loss-memory (D1/D4 mourning, boundary preservation) and adaptive institutional capacity (D5 survival competence), what makes this duality coherent as a single constraint rather than two separable rituals, and how the hybrid encoding structures the community's cognitive relationship to catastrophe.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, observer_ritual_theorist, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_function__hybrid_transformation_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_function__hybrid_transformation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Encodes and transmits two interlocked functions: (1) structured commemoration of catastrophic loss and displacement, maintaining boundary-identity and grief-memory within a normative form; (2) embodied rehearsal of institutional adaptation and decentralized continuity — solving for how institutions persist through centrality collapse and external threat. The ritual coordinates community memory (everyone performs the same grief act at the same time) and institutional memory (everyone rehearses the same survival protocol).
% TRANSFER_FUNCTION: Moves grieving-labor and rehearsal-labor from individual or spontaneous memory to collective, regularized, textually-encoded performance. It also moves institutional knowledge (decentralized leadership, household autonomy, flexibility under threat) from oral tradition into embodied ritual that can be transmitted without specialized scribal class or centralized authority.
% ABSENT_VOICES: Direct survivors of the catastrophe itself are structurally excluded — the ritual speaks FOR them in mediated form rather than amplifying their testimony. Secular or non-commemorative framings of the same historical events are structurally ineligible within the ritual's truth-making apparatus. Those who would separate mourning-practice from institutional-survival instruction (seeking to commemorate without the adaptive function, or to teach institutional flexibility without grief-memory) find the hybrid encoding constrains their legitimacy within the community's memory-practice.
% DISAPPEARANCE_RATIONALE: If this ritual structure vanished, the community's organized transmission of both loss-memory and institutional-survival knowledge would fragment into separate channels (formal mourning rites disconnected from adaptive-institutional teaching; institutional innovation separated from loss-narrative). The catastrophe might still be remembered, but without the annual coordinated enactment that binds grief and adaptation into a single epistemic frame. The decentralized institutional architecture the ritual encodes would lose its most effective transmission mechanism — the survival-competence would survive only in scattered households, not as coordinated community knowledge.
% FOUNDING_PROBLEM: After catastrophic institutional collapse (temple destruction, diaspora dispersal, genocide), how do two critical community functions survive without centralized authority: (1) memory of loss and boundary-identity (preventing erasure and assimilation), and (2) institutional knowledge about persisting as a decentralized, non-hierarchical community. The founding problem is not just psychological (how do we grieve together?) but epistemic and institutional (how do we transmit both loss-consciousness and adaptive capacity in a form that does not depend on a center that has been destroyed?).
% FOUNDING_PROBLEM_CORROBORATION: Historians of catastrophic diaspora and institutional resilience (comparative genocide scholarship, collective memory studies, institutional anthropology) corroborate that communities face exactly this dual challenge: maintaining loss-identity without institutional collapse, and transmitting adaptive institutional knowledge without centralized transmission channels. The ritual's structure — dual encoding in a single decentralized performance — is documented as a successful solution across multiple diaspora contexts. Religious authority and practitioners attest the problem is still live because contemporary institutional threats (assimilation, fragmentation, loss of transmission channels) continue to demand the same dual function the ritual was built to solve.
narrative_ontology:disappearance_verdict(catastrophe_memory_function__hybrid_transformation_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_function__hybrid_transformation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__hybrid_transformation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_function__hybrid_transformation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_function__hybrid_transformation_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_function__hybrid_transformation_reading_tests).
:- end_tests(catastrophe_memory_function__hybrid_transformation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38) and relatively flat across the interval, with a very gentle upward drift (0.28→0.38 over 50 years). This reflects the constraint's nature as a coordination of memory and institutional knowledge: participants benefit from the ritual's coordinating function (organized grief prevents isolation, shared knowledge transmission prevents forgetting and institutional fragmentation) even as they bear the time and emotional cost. The constraint is not extractive in the sense of coercive transfer from payer to beneficiary — practitioners ARE the beneficiaries, though they also bear the maintenance cost. Theater_ratio rises modestly (0.08→0.22) as the ritual moves further from its founding catastrophe: the original seder may have been primarily functional (actually rehearsing survival strategies for communities under active threat); contemporary seders increasingly emphasize commemorative performance and identity-marking over explicit survival-skill transmission. The drift upward suggests that as immediate survival necessity recedes, the ritual's theater-ratio increases — more performative, less functional. Suppression_requirement is very low (0.05→0.12) and reflects the minimal external coercion needed: the constraint persists primarily because practitioners choose to participate (identity-lock is structural and internalized, not enforced). The gentle upward drift in suppression may reflect increasing secular assimilation pressure requiring the ritual to become more actively defended against abandonment. The accessibility_collapse score (0.65) reflects moderate constraint on alternatives: practitioners cannot easily choose a secular memory-practice or institutional form that provides both grief-processing and institutional-knowledge transmission; the hybrid encoding forecloses clean separations. Resistance is low (0.28) because the ritual is not experienced primarily as oppressive — it is identity-constituting rather than identity-violating, even when it constrains alternatives.
 *
 * PERSPECTIVAL GAP:
 *   From the practitioners' seat, the constraint is experienced as both a burden (time, emotional labor, identity-lock) and a necessity (without it, the loss-memory would dissolve and the institutional knowledge would scatter). From religious authority's seat, it is a tool for community cohesion and transmission of wisdom they are invested in preserving. From the survivor seat (excluded from voice), the ritual would likely feel like mediation and appropriation of their direct testimony. From the observer-theorist's analytical seat, the constraint is a coherent solution to a genuine dual problem: how to keep both memory and adaptive capacity alive in a decentralized community without centralized authority. The engine computes these divergent classifications from the structural differences (identity-lock vs. mobile exit options, powerless vs. institutional power, biographical vs. civilizational time horizons) — the practitioners and authority figures sit in genuinely different structural positions relative to the constraint's coordination function.
 *
 * DIRECTIONALITY LOGIC:
 *   Practitioners (organized, identity-locked) occupy an asymmetric position: they benefit from the ritual's coordinating function (grief is processed communally, institutional knowledge is transmitted regularly) but bear the maintenance cost (annual time investment, emotional exposure, identity-lock that constrains exit). Their directionality is near 0.5 — symmetric costs and benefits. Religious authority has higher d (toward beneficiary, ~0.25-0.35) because they maintain the ritual's form, control its interpretation, and benefit institutionally from the transmission channel the ritual provides; their exit cost is lower (they can reinterpret or innovate within authority). Memory-bearers have higher d (toward target, ~0.55-0.65) because identity-lock is deepest for those whose family genealogy and self-concept are most fused with the loss-narrative; they have fewer alternatives and higher emotional cost. Catastrophe-survivors have the highest target-position (d near 1.0) because they are excluded from voice in the ritual's contemporary mediation of their own testimony — the ritual constrains how their experience is remembered and represented, and they have no power within the ritual's authority structure to change that. Rival memory frameworks are constrained rather than trapped: secular practitioners can exit and use alternative frameworks (d~0.4), but they lose community coherence and institutional connection.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy through dual function: if either the grief-memory or the institutional-survival transmission function were the sole purpose, the ritual could become vestigial as historical distance increases. But because the functions are integral (grief alone is not enough — institutional knowledge is how survival is enacted; institutional knowledge alone is not enough without the memory-grief that makes survival necessary), the mandate remains live. Contemporary challenges to mandatrophy come from secular assimilation (the grief-memory function is displaced by psychology and historiography; the institutional-survival function is displaced by modern institutional theory) and from the identity-lock mechanism becoming experienced as burden rather than necessity (especially for younger generations for whom the catastrophe is historical rather than living). If the ritual were to split into mourning-only and institutional-training-only, it would likely become two separate, optional practices, and the coordinating function that binds grief and adaptation would vanish — this is the mandatrophy risk. The hybrid encoding is what prevents each function from becoming dispensable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hybrid_function_unity,
    'Is the hybrid encoding of mourning + institutional-survival a single coherent function, or two separable functions that happen to share a ritual vessel?',
    'Comparative analysis of sibling ritual readings that separate these functions (mourning-only vs. institutional-only variants) and assess whether separated performance loses explanatory power or transmission efficacy. Or: longitudinal tracking of communities that attempt to unbundle the functions — do they still transmit both, or does separation degrade one or both?',
    'If the functions are structurally unified (grief-processing uniquely enables institutional-flexibility learning, or vice versa), the hybrid reading is the correct constraint and the separation-oriented siblings are misframings. If separable, the readings are genuinely distinct constraints with different ε values and beneficiary structures, and the kernel contest is about which decomposition captures the real mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_function_unity, conceptual, 'Whether mourning and institutional-survival encoding are structurally unified or separable.').

omega_variable(
    adaptive_mechanism_operationalization,
    'What specific institutional-adaptive competencies does the ritual actually transmit? Is the S-pattern (decentralized household leadership, flexibility under external threat) actually coded in the seder structure, or is this reading projecting adaptive intentionality onto a commemorative form?',
    'Ethnographic documentation of explicit institutional knowledge transmission via the ritual (do practitioners and leaders describe it as teaching adaptability, or only as commemorating?); comparison with documented institutional survival in diaspora communities that practice the ritual vs. those that do not; analysis of ritual innovations during actual institutional crises (do communities modify the seder specifically to rehearse adaptation to new threats?).',
    'If the adaptive function is active and explicit, the hybrid reading stands as a live constraint encoding real institutional knowledge. If the adaptation story is retrospective interpretation, the constraint is primarily mourning-practice and the survival-competence is a reading imposed by contemporary observers, not a function the ritual encodes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(adaptive_mechanism_operationalization, empirical, 'Whether the ritual actually transmits institutional-adaptive mechanisms or only appears to via retrospective analysis.').

omega_variable(
    identity_lock_mechanism,
    'Is participation in this ritual identity-locking because it encodes irreplaceable memory and institutional knowledge (the hybrid reading''s structural claim), or because it is enforced by community exclusion and family obligation (suppression mechanism)?',
    'Post-exit trajectories of practitioners who leave the ritual: do they maintain the loss-memory and institutional knowledge through other channels, or does the separation of ritual from memory/knowledge cause the memory to fade and adaptive capacity to dissipate? Do secular or alternative-narrative practitioners maintain institutional resilience without the ritual form?',
    'If the identity-lock is primarily structural (the ritual is the only effective encoding and transmission channel), it is a rope-type coordination with minimal suppression. If lock is primarily enforced, it is a snare or tangled-rope with high suppression built into family/community coercion. The classification depends on whether the lock is about irreplaceability of function or enforcement of conformity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether identity-lock is structural irreplaceability or enforced conformity.').

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the catastrophe_memory_function kernel. The sibling readings (mourning_practice_reading, survival_competence_reading) emphasize one function or the other. Which reading best captures what the ritual actually does? Or do all three readings describe real, simultaneously-operative aspects of the constraint?',
    'Ethnographic and historical analysis documenting how practitioners and authorities describe the ritual''s purpose and function; examination of ritual innovations and disputes to see which function is privileged when the two come into tension; longitudinal observation of which function communities emphasize during different historical periods (crisis vs. stability, institutional threat vs. cultural integration).',
    'If the hybrid reading is the coherent one, the siblings are partial readings that miss the actual structure. If all three are equally live (practitioners genuinely hold different emphases), the kernel is genuinely contested and all three readings remain legitimate constraint instantiations. If one reading clearly dominates (e.g., the ritual is genuinely about mourning and the institutional-adaptation is secondary), the constraint should be reclassified under that reading''s constraint_id.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of the catastrophe_memory_function kernel is structurally accurate, or whether all remain live.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__hybrid_transformation_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(cata_tr_t0, observed).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement_basis(cata_tr_t10, observed).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement_basis(cata_tr_t20, observed).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 30, 0.19).
narrative_ontology:measurement_basis(cata_tr_t30, observed).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 40, 0.21).
narrative_ontology:measurement_basis(cata_tr_t40, observed).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 50, 0.22).
narrative_ontology:measurement_basis(cata_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(cata_be_t0, observed).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 10, 0.31).
narrative_ontology:measurement_basis(cata_be_t10, observed).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement_basis(cata_be_t20, observed).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 30, 0.37).
narrative_ontology:measurement_basis(cata_be_t30, observed).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement_basis(cata_be_t40, observed).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 50, 0.38).
narrative_ontology:measurement_basis(cata_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement_basis(cata_su_t0, observed).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 10, 0.06).
narrative_ontology:measurement_basis(cata_su_t10, observed).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 20, 0.08).
narrative_ontology:measurement_basis(cata_su_t20, observed).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 30, 0.09).
narrative_ontology:measurement_basis(cata_su_t30, observed).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 40, 0.11).
narrative_ontology:measurement_basis(cata_su_t40, observed).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 50, 0.12).
narrative_ontology:measurement_basis(cata_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__hybrid_transformation_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_function__hybrid_transformation_reading, 0.14).
narrative_ontology:affects_constraint(catastrophe_memory_function__hybrid_transformation_reading, catastrophe_memory_function__mourning_practice_reading).
narrative_ontology:affects_constraint(catastrophe_memory_function__hybrid_transformation_reading, catastrophe_memory_function__survival_competence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the catastrophe_memory_function kernel. The sibling readings decompose the dual function into separate constraints: mourning_practice_reading emphasizes D1/D4 memorial obligation and boundary preservation; survival_competence_reading emphasizes D5 institutional-adaptive transmission. The hybrid reading claims these functions are structurally unified — neither can effectively operate without the other in the ritual's actual structure. All three readings share the same historical referent (catastrophic institutional collapse and diaspora) but differ in which function they privilege as primary. The network edges (affects_constraints) link this reading to its siblings, enabling contention analysis and reading-competition measurement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_function__hybrid_transformation_reading, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
