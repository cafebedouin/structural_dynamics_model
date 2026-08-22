% ============================================================================
% CONSTRAINT STORY: territorial_sovereignty_legitimacy__covenant_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_sovereignty_legitimacy__covenant_continuity_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: territorial_sovereignty_legitimacy__covenant_continuity_reading
 *   human_readable: Territorial Sovereignty via Covenant-Continuity Legitimacy
 *   domain: political/international_relations
 *
 * SUMMARY:
 *   This constraint is one reading of the contested kernel: territorial
 *   sovereignty legitimacy. The covenant-continuity reading grounds Israeli
 *   sovereignty in an ancient divine promise (biblical covenant), combined
 *   with continuous (though dispersed) Jewish historical presence and modern
 *   international recognition (Balfour Declaration, UN Partition Plan, 1948
 *   establishment). This reading frames the territory as promised, validated
 *   by history, and internationally recognized—making current Israeli control
 *   a return to ancient right rather than a new creation. The sibling
 *   readings (self-determination and existential-matrix) ground legitimacy
 *   differently and generate incompatible ε values for the same territorial
 *   arrangement. The claim (tangled_rope) is deliberate: the reading
 *   coordinates diaspora return and Jewish collective identity while
 *   simultaneously extracting from and suppressing Palestinian rights claims.
 *   The metrics are authored independently from the claim; the engine
 *   computes per-seat types and reveals where the readings diverge most
 *   sharply.
 *
 * KEY AGENTS:
 *   - jewish_diasporic_communities (beneficiary, organized power) — gain legitimacy narrative for return and collective identity
 *   - israeli_institutional_apparatus (agenda_setter, institutional power) — administers the reading through law, settlement, military control
 *   - palestinian_arabs_displaced_post_1948 (payer, powerless) — bore displacement cost; excluded from legitimacy framework
 *   - palestinian_arabs_resident_under_military_rule (payer, powerless, constrained exit) — subject to occupation justified by this reading
 *   - competing_self_determination_reading (excluded, organized power) — would ground legitimacy in modern Arab demographics; structurally excluded from this reading's framework
 *   - international_law_authorities (observer, institutional power) — document the contestation and note incommensurable frameworks
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.68).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.72).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__covenant_continuity_reading, tangled_rope).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__covenant_continuity_reading, "Territorial Sovereignty via Covenant-Continuity Legitimacy").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__covenant_continuity_reading, "political/international_relations").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__covenant_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__covenant_continuity_reading, 'ae49fe58-f9ac-4356-b8ee-69c81a7bdec8').
narrative_ontology:cs_kernel_codification('ae49fe58-f9ac-4356-b8ee-69c81a7bdec8', fixed_text).
narrative_ontology:cs_authority_grounding('ae49fe58-f9ac-4356-b8ee-69c81a7bdec8', lineage).
narrative_ontology:cs_interpretation_layer_present('ae49fe58-f9ac-4356-b8ee-69c81a7bdec8').
narrative_ontology:cs_reading_relation('ae49fe58-f9ac-4356-b8ee-69c81a7bdec8', territorial_sovereignty_legitimacy__self_determination_reading, forecloses).
narrative_ontology:cs_reading_relation('ae49fe58-f9ac-4356-b8ee-69c81a7bdec8', territorial_sovereignty_legitimacy__existential_matrix_reading, coexists_with).
narrative_ontology:cs_axiom('ae49fe58-f9ac-4356-b8ee-69c81a7bdec8', foundational, ancient_covenant_territorial_promise).
narrative_ontology:cs_axiom_status(ancient_covenant_territorial_promise, holdable).
narrative_ontology:cs_axiom_grounding('ae49fe58-f9ac-4356-b8ee-69c81a7bdec8', ancient_covenant_territorial_promise, theological).
narrative_ontology:cs_axiom('ae49fe58-f9ac-4356-b8ee-69c81a7bdec8', foundational, diasporic_continuity_preserves_rights).
narrative_ontology:cs_axiom_status(diasporic_continuity_preserves_rights, holdable).
narrative_ontology:cs_axiom_grounding('ae49fe58-f9ac-4356-b8ee-69c81a7bdec8', diasporic_continuity_preserves_rights, deontological).
narrative_ontology:cs_axiom('ae49fe58-f9ac-4356-b8ee-69c81a7bdec8', secondary, modern_recognition_vindicates_ancient_claim).
narrative_ontology:cs_axiom_status(modern_recognition_vindicates_ancient_claim, holdable).
narrative_ontology:cs_axiom_grounding('ae49fe58-f9ac-4356-b8ee-69c81a7bdec8', modern_recognition_vindicates_ancient_claim, conventional).
narrative_ontology:cs_reference_frame('ae49fe58-f9ac-4356-b8ee-69c81a7bdec8', ancient_covenant_unbroken_claim).
narrative_ontology:cs_drift_state('ae49fe58-f9ac-4356-b8ee-69c81a7bdec8', contemporary_post_occupation_occupation_hardening, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ae49fe58-f9ac-4356-b8ee-69c81a7bdec8', '').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__covenant_continuity_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__covenant_continuity_reading, jewish_diasporic_communities).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__covenant_continuity_reading, israeli_institutional_apparatus).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_arabs_displaced_post_1948).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_arabs_resident_under_military_rule).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain legitimacy claim to a territorial homeland grounded in ancient covenant and continuous (though dispersed) historical presence. This legitimacy framework validates return migration and justifies institutional preference for Jewish majority and character of the state. Communities benefit from the existence of a state claiming to represent their collective history and interests.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, jewish_diasporic_communities, beneficiary,
    organized, generational, mobile, global).

% Sets, administers, and enforces the legitimacy claim through law, education, settlement patterns, and military control. Frames the state's existence and territorial scope as fulfillment of covenant and historical right. Enforces the reading against competing narratives through institutional control, legal frameworks (Law of Return, settlement policy), and military administration of territories.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, israeli_institutional_apparatus, agenda_setter,
    institutional, generational, trapped, national).

% Bore the primary cost of the sovereignty claim's instantiation: displacement from homes, loss of property, restriction to refugee camps and diaspora. The covenant-continuity reading treats their presence in the territory as historical fact but not as generating territorial rights that supersede the covenant claim. Their displacement is structurally rationalized as the necessary cost of actualizing the prior (ancient) right.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_arabs_displaced_post_1948, payer,
    powerless, biographical, trapped, regional).

% Live under military administration in territories claimed under the same sovereignty framework. Subject to settlement expansion justified by the covenant-continuity reading, occupation law, and restrictions on movement and property. Their continued residence is tolerated but not incorporated into the legitimacy framework as a source of rights; their presence is treated as subordinate to the sovereign claim.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_arabs_resident_under_military_rule, payer,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_arabs_resident_under_military_rule, excluded).

% A non-agent historical commitment: the British imperial framing of Palestine as a territory available for Jewish national home-building. The covenant-continuity reading invokes this declaration as modern international recognition vindicating the ancient claim, bridging divine covenant and 20th-century geopolitical legitimacy.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, balfour_declaration_legacy, beneficiary,
    analytical, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(territorial_sovereignty_legitimacy__covenant_continuity_reading, balfour_declaration_legacy).

% A non-agent historical commitment: UN Resolution 181 (1947) recommending partition and Jewish state establishment. The covenant-continuity reading treats this as modern international legal recognition of the claim, not as creation of a new right but as acknowledgment of a pre-existing one.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, un_partition_plan_legacy, beneficiary,
    analytical, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(territorial_sovereignty_legitimacy__covenant_continuity_reading, un_partition_plan_legacy).

% Would claim Arab self-determination based on modern demographic majority and continuous residence during the 19th-20th centuries. This reading is structurally excluded from the covenant-continuity framing because the latter treats historical legitimacy as grounded in ancient (pre-modern) covenant and continuous dispersed presence, not in modern demographic majoritarianism. The two readings are incompatible within a single legal/legitimacy framework.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, competing_self_determination_reading, excluded,
    organized, generational, trapped, regional).

% Examine the legitimacy claim through frameworks of self-determination, historical title, UN law, and human rights. They document the contestation: covenant-continuity reading vs. self-determination reading, and note that international law inherited competing legitimacy traditions with no canon for resolving prior-claim disputes when demographics and occupation change.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, international_law_authorities, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_sovereignty_legitimacy__covenant_continuity_reading, israeli_institutional_apparatus).
narrative_ontology:fixing_cost_class(territorial_sovereignty_legitimacy__covenant_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for diaspora return and collective political identity: the covenant-continuity reading provides diaspora communities with a legitimacy narrative for return migration and a justification for majority-Jewish institutional character of the state, coordinating dispersed communities around a shared historical claim and territorial project.
% TRANSFER_FUNCTION: Moves territorial control, political authority, and property rights from Arab residents to Jewish/Israeli institutional control. The covenant-continuity reading legitimizes this transfer by framing Arab displacement and subordination not as violation of prior Arab rights but as necessary realization of an ancient Jewish right that supersedes modern residence and demographics.
% ABSENT_VOICES: Palestinian self-determination advocates, international humanitarian organizations, and Arab state governments would argue that modern self-determination principle and continuous Arab residence during the colonial/modern period generate rights that supersede ancient covenant claims. They are structurally excluded from the covenant-continuity framing because that reading treats ancient lineage as dispositive over modern demographics.
% DISAPPEARANCE_RATIONALE: If the covenant-continuity legitimacy claim vanished, the institutional basis for Jewish-majority preference, settlement expansion, and the exclusion of Palestinian rights from the sovereignty claim would collapse. The state's territorial scope and demographic policy are directly grounded in this reading; its disappearance would force renegotiation of core constitutional premises.
% FOUNDING_PROBLEM: Jewish diaspora communities required a territorial homeland to escape persecution, statelessness, and cultural dispersion. The covenant-continuity reading frames this need through the lens of ancient promise: the land was promised, it was continuous in Jewish memory and dispersed presence, and international recognition (Balfour, Partition) validated the ancient claim.
% FOUNDING_PROBLEM_CORROBORATION: Jewish diaspora communities and Israeli institutional actors attest the founding problem is live and ongoing. International law scholars and Palestinian advocates attest the founding problem is a narrative framing that obscures the displacement of a contemporaneous Arab population. Historical evidence (biblical texts, diasporic records, Arab demographic presence) is cited by both sides but interpreted through incompatible legitimacy frameworks — corroboration from outside the benefiting parties supports the reading-contested status.
narrative_ontology:disappearance_verdict(territorial_sovereignty_legitimacy__covenant_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_sovereignty_legitimacy__covenant_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__covenant_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__covenant_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_sovereignty_legitimacy__covenant_continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__covenant_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_sovereignty_legitimacy__covenant_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.42 (at establishment, when the reading's coordination function is strong) to 0.68 (contemporaneously, as the extractive cost-structure becomes clear). Theater rises from 0.28 to 0.44: early institutional energy went into physical return and state-building; later energy maintains the legitimacy narrative against mounting evidence of incompatibility with Palestinian rights. Suppression rises steadily from 0.58 to 0.72 as the occupation deepens and the covenant-continuity reading must defend itself against accumulated counter-claims. The plateau at t=75-100 indicates the reading has stabilized into a state-maintained institutional orthodoxy with steady suppressive cost. Accessibility_collapse (0.62) reflects that once the reading is accepted by state institutions and major diaspora organizations, alternatives become institutionally invisible despite remaining politically live. Resistance (0.78) reflects substantial Palestinian, Arab, and international law opposition that prevents the reading from achieving consensus.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (diaspora, Israeli institutions) perceive this as genuine coordination of a dispersed people around a historical-legal claim, with suppression being necessary enforcement against external challenges to legitimacy. The payer seats (displaced and occupied Palestinians) perceive it as a cover story for territorial seizure and demographic domination, with suppression being systematic erasure of competing rights claims. The observer seat (international law) documents both: the reading accomplishes real coordination (Jews now have a state; diaspora return was enabled) AND extracts substantial cost from Palestinians (displacement, subordination, property loss). The divergence is structural and permanent because the two sides inhabit incompatible legitimacy frameworks. The engine should compute this as seat-level type divergence: rope from the institutional beneficiary seat, snare from the Palestinian payer seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish diaspora communities are structural beneficiaries (low d, subsidized by the claim). Israeli institutions are agenda-setters collecting rents from enforcement (moderate-to-high d depending on institutional seat, but benefiting from the legitimacy monopoly). Displaced Palestinians have high d (full targets: bear displacement cost, excluded from remedy). Occupied Palestinians have high d (targets of suppression, constrained exit). The beneficiaries gain both material (property, control) and immaterial (state, identity) benefits. The victims bear material (displacement, property loss, subordination) and immaterial (delegitimacy, exclusion from historical narrative) costs. Directionality overrides are not needed; the beneficiary/victim declarations map cleanly to power atoms and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Jewish statelessness, diaspora vulnerability) was live and pressing in 1948. The founding_problem_status is contested: Israeli institutions attest it remains live (persistent anti-Semitism, need for Jewish majority state); diaspora communities attest it is solved (state now exists, return is enabled); international law scholars and Palestinian advocates attest it was solved decades ago and the sovereignty claim now persists as rent-seeking without founding justification. The constraint prevents mislabeling because it declares both the coordination function (genuine: return and identity) AND the extraction cost (genuine: Palestinian subordination and property loss). A pure-rope reading would ignore the extraction. A pure-snare reading would ignore the real coordination benefit. Tangled-rope captures the hybrid: the constraint solves the diaspora coordination problem while simultaneously extracting from Palestinians, with the extraction defended through the legitimacy claim itself. Mandatrophy emerges if the founding problem becomes consensus-dead (which the constraint resists by redefining it as ongoing security necessity) and the constraint persists solely through institutional inertia and suppression.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    covenant_vs_modern_law_grounding,
    'Can legitimacy be grounded simultaneously in ancient divine covenant and modern international law frameworks that emerged from self-determination principles incompatible with covenant reasoning?',
    'Meta-legal analysis: trace the logical structure of each grounding framework and examine whether they are reducible to a common principle or fundamentally incommensurable. Examine institutional choices about which framework is invoked in which context (property law vs. military administration vs. demographic policy).',
    'If incommensurable, the constraint''s legitimacy claim is internally contradictory and vulnerable to erosion once the contradiction is exposed. If reducible, the unified framework strengthens the constraint''s coherence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(covenant_vs_modern_law_grounding, conceptual, 'Whether covenant and modern international law can ground the same sovereignty claim.').

omega_variable(
    continuity_through_demographic_absence,
    'Does dispersed diasporic presence constitute ''continuous presence'' sufficient to preserve territorial rights through two millennia of demographic absence and Arab occupation?',
    'Comparative legal history: examine other cases where dispersed diasporic communities claim rights to territories they abandoned or were expelled from. Philosophical analysis: what counts as continuous presence for rights preservation — physical presence, cultural memory, institutional claim, or dispersed communities returning?',
    'If continuity requires physical presence, the covenant claim is weaker than stated and depends on supplementary modern recognition (Balfour, Partition). If memory and institutional claim suffice, the reading survives demographic facts and supports stronger property/settlement claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuity_through_demographic_absence, conceptual, 'Whether diasporic presence preserves territorial rights through demographic absence.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of Palestinian rights claims structurally enforced (military occupation, administrative law, property seizure) or partially internalized (acceptance of the covenant-continuity framing as legitimate by some Palestinian institutional actors, international law scholars)?',
    'Post-enforcement trajectory: if suppression persists after military administration ends, it has become internalized (institutional norms, educational systems, international recognition patterns). If it dissolves, it was primarily structural.',
    'If structural, the constraint''s persistence depends on continued military/institutional enforcement. If internalized, it persists through normalization and becomes resistant to simple legal remedies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression is structural enforcement or internalized legitimacy acceptance.').

omega_variable(
    kernel_reading_committer_ambiguity,
    'Is this constraint the proper reading of the kernel (territorial sovereignty legitimacy), or does it over-commit to one faction''s interpretation (Jewish diasporic continuity) and obscure the kernel''s internal contestation?',
    'Compare this JSON-authored constraint with the sibling readings (self_determination_reading, existential_matrix_reading) when they are generated. The engine should compute different ε and chi values for the same territorial arrangements viewed through each reading. If this reading''s ε is substantially higher than siblings'', it indicates this framing is more extractive (more beneficiary gain, more victim cost) than alternative readings of the same situation.',
    'If ε diverges significantly across readings of the same kernel, the reading-choice is consequential and the choice is not neutral. If ε converges, the readings are describing the same underlying constraint structure and the choice is interpretive rather than structural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_ambiguity, conceptual, 'Whether this reading accurately instantiates the kernel or reflects factional preference.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t0, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(terr_tr_t12, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 12, 0.32).
narrative_ontology:measurement(terr_tr_t25, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 25, 0.38).
narrative_ontology:measurement(terr_tr_t50, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement(terr_tr_t75, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 75, 0.44).
narrative_ontology:measurement(terr_tr_t100, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 100, 0.44).

% Extraction over time
narrative_ontology:measurement(terr_be_t0, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(terr_be_t12, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 12, 0.51).
narrative_ontology:measurement(terr_be_t25, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 25, 0.6).
narrative_ontology:measurement(terr_be_t50, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 50, 0.65).
narrative_ontology:measurement(terr_be_t75, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 75, 0.68).
narrative_ontology:measurement(terr_be_t100, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 100, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t0, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(terr_su_t12, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 12, 0.64).
narrative_ontology:measurement(terr_su_t25, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 25, 0.68).
narrative_ontology:measurement(terr_su_t50, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 50, 0.71).
narrative_ontology:measurement(terr_su_t75, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 75, 0.72).
narrative_ontology:measurement(terr_su_t100, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 100, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__covenant_continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.12).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__covenant_continuity_reading, territorial_sovereignty_legitimacy__self_determination_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__covenant_continuity_reading, territorial_sovereignty_legitimacy__existential_matrix_reading).

% DUAL FORMULATION NOTE:
% The territorial-sovereignty-legitimacy kernel admits three structurally distinct constraint stories: covenant-continuity (this constraint), self-determination, and existential-matrix. Each reading generates a different ε and χ by site. All three are linked via network.affects_constraints to model the kernel's internal contest. The ε-invariance principle (DP-001) requires separate stories because measuring the constraint through each reading's own legitimacy framework yields substantially different extraction values—not measurement-basis variation, but structural-model variation. Covenant-continuity ε≈0.68 (high extraction when viewed through Palestinian rights framework, significant coordination when viewed through diaspora framework). Self-determination would produce ε≈0.75+ (treating the same territorial arrangement as pure displacement). Existential-matrix would produce ε≈0.55-0.60 (treating the arrangement as tragic but necessary zero-sum outcome). These are not observer-relative reframings of one ε; they are three incompatible constraint models applied to one contested reality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_sovereignty_legitimacy__covenant_continuity_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
