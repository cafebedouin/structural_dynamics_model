% ============================================================================
% CONSTRAINT STORY: animal_status__welfare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status__welfare_reading, []).

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
 *   constraint_id: animal_status__welfare_reading
 *   human_readable: Animal Sentience with Instrumental Use Exemption (Welfare Reading)
 *   domain: applied_ethics/legal_philosophy/political_economy
 *
 * SUMMARY:
 *   The welfare reading of animal status acknowledges that animals are
 *   sentient beings with morally relevant interests in avoiding suffering,
 *   maintaining social bonds, and expressing natural behaviors. However, it
 *   exempts instrumental human uses from the prohibition that would normally
 *   follow from such sentience when those uses are deemed 'necessary' or
 *   'justified' by human benefit. This reading sits between the property
 *   reading (no independent moral standing) and the abolitionist reading
 *   (sentience entails prohibition of all instrumental use). The welfare
 *   reading's core structural feature is the exemption: animals have
 *   interests that matter morally, but their interests can be overridden when
 *   human benefit clears an institutional threshold. This creates a tangled
 *   rope — genuine coordination function (acknowledging sentience prevents
 *   moral blank-checks) coupled with asymmetric extraction (animals bear the
 *   costs of the exemption, users collect the benefits). The constraint is
 *   extractive (ε = 0.45) because the exemption structures allow systematic
 *   use of sentient beings in ways those beings would not consent to if they
 *   had exit options.
 *
 * KEY AGENTS:
 *   - farmed_animals: sentient victims of the arrangement, trapped without exit, bearing bodily and social costs
 *   - experimental_animals: sentient victims used in research, immediate time-horizon, suffering authorized by benefit calculus
 *   - entertainment_animals: sentient victims confined for amusement, constrained within the bounds set by welfare rules
 *   - animal_users (agricultural, research, entertainment industries): institutional agenda-setters and primary beneficiaries, operating within welfare exemption boundaries
 *   - welfare_enforcement_bodies: secondary beneficiaries, derive institutional authority from governing the use-permissibility boundary
 *   - individual_consumers: tertiary beneficiaries, access cheap animal-derived products via the exemption
 *   - abolitionist_advocates: excluded voice, argue the reading is incoherent — sentience that can be overridden is not genuine moral standing
 *   - comparative_biology: observer providing empirical grounds for sentience claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__welfare_reading, 0.45).
domain_priors:suppression_score(animal_status__welfare_reading, 0.62).
domain_priors:theater_ratio(animal_status__welfare_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__welfare_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(animal_status__welfare_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(animal_status__welfare_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__welfare_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(animal_status__welfare_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__welfare_reading, tangled_rope).
narrative_ontology:human_readable(animal_status__welfare_reading, "Animal Sentience with Instrumental Use Exemption (Welfare Reading)").
narrative_ontology:topic_domain(animal_status__welfare_reading, "applied_ethics/legal_philosophy/political_economy").

domain_priors:requires_active_enforcement(animal_status__welfare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__welfare_reading, 'f4aa1ff3-bed1-429a-b960-bb8750bf0d65').
narrative_ontology:cs_kernel_codification('f4aa1ff3-bed1-429a-b960-bb8750bf0d65', fixed_text).
narrative_ontology:cs_authority_grounding('f4aa1ff3-bed1-429a-b960-bb8750bf0d65', lineage).
narrative_ontology:cs_interpretation_layer_present('f4aa1ff3-bed1-429a-b960-bb8750bf0d65').
narrative_ontology:cs_reading_relation('f4aa1ff3-bed1-429a-b960-bb8750bf0d65', animal_status__abolitionist_reading, forecloses).
narrative_ontology:cs_reading_relation('f4aa1ff3-bed1-429a-b960-bb8750bf0d65', animal_status__property_reading, coexists_with).
narrative_ontology:cs_axiom('f4aa1ff3-bed1-429a-b960-bb8750bf0d65', foundational, sentience_generates_moral_constraints).
narrative_ontology:cs_axiom_status(sentience_generates_moral_constraints, holdable).
narrative_ontology:cs_axiom_grounding('f4aa1ff3-bed1-429a-b960-bb8750bf0d65', sentience_generates_moral_constraints, deontological).
narrative_ontology:cs_axiom('f4aa1ff3-bed1-429a-b960-bb8750bf0d65', foundational, instrumental_use_compatible_with_sentience).
narrative_ontology:cs_axiom_status(instrumental_use_compatible_with_sentience, holdable).
narrative_ontology:cs_axiom_grounding('f4aa1ff3-bed1-429a-b960-bb8750bf0d65', instrumental_use_compatible_with_sentience, deontological).
narrative_ontology:cs_reference_frame('f4aa1ff3-bed1-429a-b960-bb8750bf0d65', sentience_with_justified_exemption).
narrative_ontology:cs_drift_state('f4aa1ff3-bed1-429a-b960-bb8750bf0d65', contemporary_post_cognitive_revolution, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f4aa1ff3-bed1-429a-b960-bb8750bf0d65', '2026-06-12T14:23:47Z').
narrative_ontology:cs_kernel_id(animal_status__welfare_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, animal_users).
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, welfare_enforcement_bodies).
narrative_ontology:constraint_victim(animal_status__welfare_reading, farmed_animals).
narrative_ontology:constraint_victim(animal_status__welfare_reading, experimental_animals).
narrative_ontology:constraint_victim(animal_status__welfare_reading, entertainment_animals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, individual_consumers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sentient beings bred, raised, and killed for food production. Their interests in avoiding pain, maintaining social bonds (separated at birth or regularly), and expressing natural behaviors (grazing, roaming, social hierarchies) are acknowledged in the welfare reading but systematically overridden through confinement, transportation, and slaughter. They bear the direct costs of animal agriculture without voice in the use-justification calculation. Exit is impossible — the constraint defines their entire existence as a resource.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, farmed_animals, payer,
    powerless, biographical, trapped, global).

% Sentient beings used in laboratory research for medical, pharmaceutical, cosmetic, and behavioral science purposes. The welfare reading acknowledges their capacity for pain and distress but authorizes suffering when institutional review boards determine the expected human benefit justifies it. They have no representation in the benefit calculation; their suffering is a cost authorized by others' judgment of necessity. Exit from research protocols is impossible by definition.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, experimental_animals, payer,
    powerless, immediate, trapped, regional).

% Sentient beings confined and trained for human amusement in zoos, circuses, rodeos, racing, and performance. Their interests in natural behavior, autonomy, and freedom of movement are constrained by enclosure and training regimes. Welfare rules govern the style of constraint (minimum enclosure size, training methods) but the fact of constraint is non-negotiable. Confinement persists throughout their biological lifespan.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, entertainment_animals, payer,
    powerless, biographical, trapped, global).

% Agricultural producers, pharmaceutical and cosmetic companies, research institutions, zoos, entertainment operators, and other industries that extract economic value from animal bodies and labor. They set the boundaries of permissible use by determining what counts as 'necessary' or 'justified' — the exemption that allows instrumental use under the welfare reading. They benefit from a framework that acknowledges animal sentience (preventing moral blank-checks and abolitionist pressure) while authorizing continued use under welfare constraints. They operate within welfare rules but do not operate under them — the rules constrain their methods, not their ability to use animals.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, animal_users, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(animal_status__welfare_reading, animal_users, beneficiary).

% Regulatory agencies, inspection authorities, and non-profit animal welfare organizations that administer and enforce welfare standards. They derive institutional authority and operational budgets from governing the use-permissibility boundary. They set inspection standards, issue certifications, and manage enforcement priorities. They benefit from the welfare framework by occupying a governance niche that would not exist if animals had full rights (abolition) or no standing (property). They enforce welfare constraints but do not question the exemption itself.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, welfare_enforcement_bodies, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(animal_status__welfare_reading, welfare_enforcement_bodies, beneficiary).

% Consumers of animal-derived products (meat, dairy, eggs, leather, cosmetics, entertainment, medicine). They benefit from access to these products at prices justified by the welfare exemption — production costs are lower when welfare standards are regulatory minima rather than market-driven. They have some choice to consume alternatives but face cultural, economic, and convenience constraints on exit. They do not directly set welfare rules but benefit from the framework that makes such products available.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, individual_consumers, beneficiary,
    organized, biographical, constrained, global).

% Animal rights advocates, liberation organizations, and philosophers who argue that sentience entails prohibition of instrumental use, not merely welfare constraints. They argue the welfare reading is incoherent — acknowledging that animals suffer and have interests should prohibit overriding those interests for human convenience. Their position is structurally excluded from most policy conversations; welfare advocates and industry users do not treat abolition as a live policy option worthy of serious engagement. Excluding abolition from the legitimate policy space is part of how the welfare exemption is maintained.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, abolitionist_advocates, excluded,
    moderate, generational, constrained, global).

% The scientific community studying animal behavior, cognition, and neurobiology — primatology, cetology, ornithology, entomology, and comparative neuroscience. They provide empirical grounds for the welfare reading's core claim that animals are sentient, that they feel pain, that they have social bonds and cognitive sophistication. Their research enables the acknowledgment of sentience but does not determine how that sentience should be interpreted normatively. They do not set policy or benefit from use, but their evidence constrains the available readings — the property reading becomes increasingly indefensible as evidence of sentience accumulates.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, comparative_biology, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status__welfare_reading, animal_users).
narrative_ontology:fixing_cost_class(animal_status__welfare_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the moral coordination problem created by conflicting imperatives: acknowledge empirical evidence of animal sentience (scientifically unavoidable) while preserving economically and culturally embedded animal-use systems (agriculture, research, entertainment). The welfare reading coordinates around a shared framework: animals matter morally, but instrumental uses can be justified when deemed necessary, and welfare standards can govern the treatment within those uses. This allows diverse users to operate under a single rule-set that prevents moral blank-checks (pure property use) while preventing moral prohibition (abolition) from foreclosing animal-based industries.
% TRANSFER_FUNCTION: Transfers animal bodies, labor, reproductive capacity, and suffering from animals to human users across three domains: agricultural production (for food, fiber, labor), scientific research (for medical and behavioral study), and entertainment (for amusement and competition). The welfare standards that govern the transfer do not eliminate it — they regulate how animals are confined, transported, used, and killed. Welfare enforcement bodies extract administrative value and institutional authority from managing the use-permissibility boundary. Users extract economic and scientific value from animal bodies. Individual consumers extract use-value from animal-derived products at prices that reflect the welfare exemption rather than prohibition.
% ABSENT_VOICES: Animals themselves have no representation in the discourse that defines what uses count as 'necessary' or 'justified.' Their interests are supposedly 'represented' by welfare bodies, but those bodies have institutional incentives to permit the uses they regulate — an inherent conflict of interest. Abolitionist advocates argue that their position is not merely absent but structurally excluded from legitimacy: the welfare framework treats abolition as categorically off the table rather than as a live policy option worthy of engagement. The scientific evidence of animal sentience is present, but animal agency in the conversation is wholly absent.
% DISAPPEARANCE_RATIONALE: If the welfare reading's constraint vanished overnight, the global agriculture, research, entertainment, and consumer-product industries would face immediate reorganization under one of two alternatives: either reversion to the property reading (unrestricted use, sentience irrelevant to permissibility) or shift to the abolitionist reading (prohibition of instrumental use, sentience entails rights). Supply chains, research protocols, regulatory frameworks, and consumer practices are built around the sentience-with-exemption framework. Its disappearance would force realignment across all three industries, with economic disruption in agriculture (diet changes, alternative production methods) and research (alternative methodologies, reduced animal use, or shift to unrestricted property rules). The constraint is not a natural fact — removing it would cause observable rearrangement of human and animal life arrangements.
% FOUNDING_PROBLEM: Industrial animal use proceeded for centuries with little recognition of animal suffering. As zoological and ethological research accumulated evidence of animal consciousness, pain perception, social intelligence, and emotional sophistication, legal and ethical systems faced a legitimacy crisis: how to acknowledge empirically evident sentience while preserving animal-based industries that predated that acknowledgment? The constraint emerged as a compromise: yes, animals are sentient and deserve protection from gratuitous harm, but no, sentience does not entail prohibition of instrumental use — it entails welfare constraints on the conduct of use.
% FOUNDING_PROBLEM_CORROBORATION: The welfare reading attests the founding problem is live: comparative biology continues to generate evidence of animal sentience, and welfare standards must continuously evolve to reflect new understanding of animal needs and capacities. Animal users and welfare bodies attest that the problem has been solved through the welfare framework — sentience is acknowledged, suffering is minimized, use is justified and constrained. Abolitionist critics and philosophers attest the problem has NOT been solved but rather captured: we now know with overwhelming clarity that animals feel pain and have interests in self-direction, and those facts should entail prohibition of instrumental use, not merely welfare constraints. The empirical core of the founding problem (animals are sentient) is now uncontested; the contested element is its normative interpretation (does sentience entail constraint or prohibition?).
narrative_ontology:disappearance_verdict(animal_status__welfare_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__welfare_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__welfare_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(animal_status__welfare_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status__welfare_reading, 0.45, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status__welfare_reading_tests).
:- end_tests(animal_status__welfare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.45) because the constraint operates through exemption: animals' acknowledged interests are systematically overridden for human benefit. The exemption is not justified by animal consent (they have no voice in setting the threshold) or by symmetry (users benefit while animals bear costs) — it is justified by a human-determined calculation that the instrumental use is 'necessary' or 'justified.' Suppression is high (0.62) because the reading requires active enforcement to maintain the boundary between permitted and prohibited uses; without consistent enforcement, the exemption would collapse into either unrestricted use (property reading) or prohibition (abolitionist reading). Theater is moderate (0.41) because welfare enforcement generates real constraints on use styles (confinement size, slaughter methods, etc.) but also performs legitimacy for the exemption itself: welfare improvements are publicly visible signs that the system cares about animal interests, which forestalls the abolitionist critique that acknowledging sentience requires abolition. Accessibility collapse is moderate-high (0.68) because once the sentience-with-exemption reading is established as the legal and ethical framework, alternatives (property, abolition) appear either incoherent (property denies evident sentience) or radically disruptive (abolition). Resistance is moderate (0.58) because abolitionist and animal-rights advocates mount sustained resistance to the exemption, but their counterargument (sentience should prohibit use, not merely constrain it) is structurally excluded from the frameworkmakers' considered options. The measurement series show extractiveness rising slightly in the early interval and plateauing (0.38 → 0.45 over 25 time units, then stable), reflecting incremental welfare improvements that modestly reduce extraction without eliminating the exemption itself. Theater_ratio rises throughout (0.28 → 0.41), indicating more regulatory and certification activity relative to functional constraint — the welfare apparatus has become increasingly visible and complex relative to the actual reduction in animal suffering achieved.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional users' and welfare bodies' seats, the reading is a genuine coordination achievement: it acknowledges sentience (avoiding moral blank-checks) while preserving economically vital uses under rational constraints. From the animals' seats (if we could occupy them), the reading is extractive structure dressed as constraint: their interests are recognized and then systematically overridden for others' benefit. From the abolitionist observer's seat, the reading is logically incoherent — sentience that justifies 'welfare constraints' should justify prohibition, not merely regulated use. The engine should compute different types from these three positions: a beneficiary seat (users, welfare bodies) may see rope-like genuine coordination; victim seats (farmed, experimental animals) compute the extraction directly; excluded seats (abolitionists) see the incoherence of the framework itself. The authored metrics reflect the structure as the animals and abolitionists perceive it — a tangled rope where coordination functions (preventing moral blank-checks) are coupled with systematic extraction (overriding interests for human benefit).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality flows from beneficiary/victim declarations and exit options. Farmed, experimental, and entertainment animals are trapped victims: they bear bodily costs (confinement, pain, death), have no voice in the exemption calculation, and have no exit options. Their directionality is near 1.0 (full targets). Animal users and welfare bodies are beneficiaries with strong exit options (they can shift to alternative business models, though at cost; they can relocate to jurisdictions with weaker welfare rules). Their directionality is near 0.0 (beneficiaries). Individual consumers are organized moderate-power actors with constrained exit — they benefit from cheap animal products but have some choice to consume alternatives; their d sits near 0.3-0.4 (slight net benefit). Abolitionist advocates are explicitly excluded from the frameworkmaking, trapped at the boundary by the constraint's definitional choice to exempt uses. Their exit would require changing the entire framework, so they are also trapped, but in a different way — they cannot exit the conversation, only be unheard within it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how to acknowledge emerging evidence of animal sentience while preserving animal-based industries — has NOT been resolved. The welfare reading solves it only through an exemption that declares the problem solved by definition: 'We acknowledge sentience and permit use through welfare constraints; therefore the coordination is achieved.' But the abolitionist critique identifies a mandatrophy: if sentience truly matters morally, then welfare constraints are not a solution — they are a perpetuation of the problem under a new name. The constraint persists not because the founding problem lives (it has evolved; the evidence of sentience is now overwhelming), but because the industries built on the exemption have political and economic power to maintain the boundary. The theater_ratio rise (0.28 → 0.41) models this: as abolitionists mount increasing pressure, the system responds not by reconsidering the exemption but by performing welfare improvements. These improvements are real in their effect on animal suffering but also functional in their effect on the constraint's legitimacy: each welfare certification, each inspection, each cruelty prosecution becomes a visible sign that 'the system cares,' which forestalls the deeper question of whether sentience-based exemptions are coherent at all.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sentience_vs_exemption_coherence,
    'Is the reading logically coherent? Can a being be genuinely sentient (capable of suffering, having interests in self-direction and social bonds) and yet have those interests systematically overridable for others'' convenience?',
    'Philosophical analysis of the sentience concept and its moral implications. Abolitionist and welfare advocates disagree on whether acknowledging sentience logically entails prohibition or permits welfare-constrained use. No empirical test resolves this — it is a question of moral reasoning.',
    'If the reading is incoherent (sentience entails prohibition), the constraint is a false-summit mountain or a snare (using sentience acknowledgment as cover for continued use). If coherent, it is a genuine tangled rope (coordination function + asymmetric extraction both real). This omega fundamentally determines the type classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sentience_vs_exemption_coherence, conceptual, 'Whether acknowledgment of animal sentience is logically compatible with instrumental use exemptions.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.62) primarily structural (external barriers preventing use-prohibition from becoming policy) or internalized (the framework has become naturalized in how people think about animal interests)?',
    'Post-shift studies: in jurisdictions that moved to abolition or stricter property readings, did suppression persist after legal barriers were removed? If suppression persists after structural barriers fall, internalization is substantial.',
    'High internalization means the constraint''s effective suppression is higher than the 0.62 measure suggests — the victims and excluded voices carry the suppression with them even when structural barriers dissolve. Low internalization suggests the suppression is primarily institutional, maintainable through policy and enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs. internalized suppression in the welfare exemption framework.').

omega_variable(
    necessity_calculation_opacity,
    'What counts as a ''necessary'' or ''justified'' use under the welfare reading? Is the boundary set by explicit principle, or determined ad-hoc by frameworkmakers?',
    'Comparative policy analysis across jurisdictions: are necessity thresholds consistent, derivable from stated principles, or do they vary according to industry pressure and political economy?',
    'If ad-hoc and pressure-responsive, the exemption is indefensible on principled grounds and the constraint is more snare-like (cover story justifying continued use). If explicit and principled, the tangled-rope framing holds and the extraction is at least bounded by stated rules.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(necessity_calculation_opacity, empirical, 'Whether the necessity exemption is principle-based or pressure-responsive.').

omega_variable(
    welfare_improvements_actual_impact,
    'Do measured welfare improvements actually reduce animal suffering, or do they primarily serve a legitimacy function, generating the appearance of constraint while use patterns persist unchanged?',
    'Comparative ethological and welfare-science studies: do animals subject to improved welfare regulations show measurable reductions in stress markers, behavioral abnormality, and suffering relative to pre-improvement baselines?',
    'If welfare improvements materially reduce suffering, extraction is modestly lower than the ε metric suggests. If improvements are primarily theatrical, theater_ratio is understated and the constraint is closer to piton (institutional performance with minimal functional effect). The trajectory matters: rising theater_ratio with stagnant extraction suggests the latter.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_improvements_actual_impact, empirical, 'Actual vs. performative welfare improvement outcomes.').

omega_variable(
    alternative_reading_foreclosure,
    'Are the property and abolitionist readings genuinely excluded from consideration in policy discourse, or are they live alternatives that the welfare reading is competing with?',
    'Discourse analysis of policy forums, legislative debates, and institutional standard-setting: how often do property and abolitionist readings appear as serious policy options versus rhetorical strawmen dismissed without engagement?',
    'If excluded (treated as non-starters rather than live options), the constraint involves higher suppression than 0.62 (active foreclosure of alternatives). If in live competition, the suppression is lower (the alternatives are available for choice; the welfare reading wins because it is preferred, not because others are unavailable).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_reading_foreclosure, empirical, 'Whether alternative readings are structurally excluded or live policy options.').

omega_variable(
    kernel_reading_identity,
    'Which reading of the animal_status kernel is this constraint instantiating, and how does the welfare reading''s core axiom differ structurally from the abolitionist and property readings'' axioms?',
    'Defined by design: this constraint is the welfare reading. The distinction is ontological — a reading is a way of interpreting a single persisting kernel such that different interpretations emit different constraint structures. The welfare reading''s axiom is: ''Sentience generates moral constraints on use, but not prohibition of use.'' The abolitionist axiom is: ''Sentience entails prohibition of instrumental use.'' The property axiom is: ''Sentience generates no independent moral constraints on use; only human-imposed welfare statutes matter.'' These three axioms are mutually exclusive interpretations of what the same empirical fact (sentience) entails normatively.',
    'This omega serves documentation rather than resolution — it records that the committer-frame structure is in place and that this story is one reading among three siblings. The engine will compute relative positioning across the three stories'' metrics to test whether the readings do indeed instantiate the structural delta expected (abolitionist: high extraction + no use victims; property: high extraction + no sentience acknowledgment; welfare: moderate extraction + sentience + use exemption).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Identity and structural distinction of the welfare reading within the animal_status kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__welfare_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status__welfare_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(anim_tr_t5, animal_status__welfare_reading, theater_ratio, 5, 0.31).
narrative_ontology:measurement(anim_tr_t10, animal_status__welfare_reading, theater_ratio, 10, 0.34).
narrative_ontology:measurement(anim_tr_t15, animal_status__welfare_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement(anim_tr_t20, animal_status__welfare_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(anim_tr_t25, animal_status__welfare_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(anim_tr_t30, animal_status__welfare_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(anim_tr_t40, animal_status__welfare_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status__welfare_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(anim_be_t5, animal_status__welfare_reading, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(anim_be_t10, animal_status__welfare_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(anim_be_t15, animal_status__welfare_reading, base_extractiveness, 15, 0.43).
narrative_ontology:measurement(anim_be_t20, animal_status__welfare_reading, base_extractiveness, 20, 0.44).
narrative_ontology:measurement(anim_be_t25, animal_status__welfare_reading, base_extractiveness, 25, 0.45).
narrative_ontology:measurement(anim_be_t30, animal_status__welfare_reading, base_extractiveness, 30, 0.45).
narrative_ontology:measurement(anim_be_t40, animal_status__welfare_reading, base_extractiveness, 40, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status__welfare_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(anim_su_t5, animal_status__welfare_reading, suppression_requirement, 5, 0.57).
narrative_ontology:measurement(anim_su_t10, animal_status__welfare_reading, suppression_requirement, 10, 0.59).
narrative_ontology:measurement(anim_su_t15, animal_status__welfare_reading, suppression_requirement, 15, 0.6).
narrative_ontology:measurement(anim_su_t20, animal_status__welfare_reading, suppression_requirement, 20, 0.61).
narrative_ontology:measurement(anim_su_t25, animal_status__welfare_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement(anim_su_t30, animal_status__welfare_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(anim_su_t40, animal_status__welfare_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__welfare_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(animal_status__welfare_reading, 0.12).
narrative_ontology:affects_constraint(animal_status__welfare_reading, animal_status__abolitionist_reading).
narrative_ontology:affects_constraint(animal_status__welfare_reading, animal_status__property_reading).

% DUAL FORMULATION NOTE:
% The animal_status kernel decomposes into three constraint stories, each instantiating a different reading of what animal sentience entails. The welfare reading (this file) acknowledges sentience and permits instrumental use under welfare constraints (ε ≈ 0.45). The abolitionist reading treats sentience as entailing prohibition of all instrumental use (expected ε higher, ≈0.65+, with animals not in victim set for any institutional use). The property reading treats sentience as generating no independent moral standing, only human-welfare statutes (expected ε high, ≈0.70+, with no animal interest acknowledgment). All three readings share the kernel (empirical fact of sentience) but diverge on its normative implications. The three stories are linked via network.affects_constraints to show they are competing interpretations of a single contested claim, not independent constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(animal_status__welfare_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
