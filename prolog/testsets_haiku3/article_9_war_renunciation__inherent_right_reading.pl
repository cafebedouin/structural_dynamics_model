% ============================================================================
% CONSTRAINT STORY: article_9_war_renunciation__inherent_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_9_war_renunciation__inherent_right_reading, []).

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
 *   constraint_id: article_9_war_renunciation__inherent_right_reading
 *   human_readable: Article 9 Inherent Right Self-Defense Reading
 *   domain: constitutional/security/institutional
 *
 * SUMMARY:
 *   Japan's Article 9 renounces 'war' and 'armed forces', yet maintains the
 *   Self-Defense Forces (SDF) justified by an inherent sovereign right to
 *   self-defense. This reading—the inherent-right interpretation—holds that
 *   Article 9 prohibits aggressive war and offensive military action but does
 *   not eliminate the state's structural right to defend against attack,
 *   provided defensive forces remain 'minimum necessary'. This is a READING
 *   of a contested kernel (Article 9 itself), not a natural law or political
 *   deal. The reading enables a constitutionally legitimate military while
 *   preserving post-war pacifist identity. Institutionally dominant for 75
 *   years, it is now pressured by rising regional threat narratives and
 *   formal moves toward collective defense (the 2015 legislation). The
 *   constraint is classified as tangled_rope: genuine coordination
 *   (reconciling constitutional legitimacy with security reality) paired with
 *   asymmetric extraction (pacifists bear costs of a reading they reject;
 *   neighboring states have no say in the interpretation that affects their
 *   security).
 *
 * KEY AGENTS:
 *   - japanese_state_apparatus: agenda-setter, institutional power; interprets Article 9, sets SDF doctrine, controls constitutional narrative. Moderate exit (cannot formally repeal without new consensus, but can reinterpret indefinitely).
 *   - us_security_alliance: beneficiary, institutional power; gains Japanese military contribution to regional deterrence without forcing Japan to formally repeal pacifism. Mobile exit (can reposition to other allies, but Japan's location is unique; constrained in practice).
 *   - pacifist_constitutional_interpreters: payers, organized power; hold strict reading of Article 9 as categorical prohibition, bear cost of living under a reinterpreted text. Constrained exit (litigation, public protest ineffective for 75 years).
 *   - sdf_military_institution: beneficiary, organized power; derives legitimacy and resources from the inherent-right reading. Trapped exit (institution would not exist under pacifist reading; entire existence contingent on this interpretation).
 *   - regional_security_skeptics (South Korea, China, Taiwan, ASEAN): payers, moderate power; excluded from the interpretation process, identity-locked to historical suspicion of Japanese militarism. Constrained exit (diplomatic protest, but cannot formally override Japanese constitutional interpretation).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_war_renunciation__inherent_right_reading, 0.38).
domain_priors:suppression_score(article_9_war_renunciation__inherent_right_reading, 0.52).
domain_priors:theater_ratio(article_9_war_renunciation__inherent_right_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_war_renunciation__inherent_right_reading, tangled_rope).
narrative_ontology:human_readable(article_9_war_renunciation__inherent_right_reading, "Article 9 Inherent Right Self-Defense Reading").
narrative_ontology:topic_domain(article_9_war_renunciation__inherent_right_reading, "constitutional/security/institutional").

domain_priors:requires_active_enforcement(article_9_war_renunciation__inherent_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__inherent_right_reading, '6d6d44b2-8184-44b7-b76e-3ea08f87cca4').
narrative_ontology:cs_kernel_codification('6d6d44b2-8184-44b7-b76e-3ea08f87cca4', fixed_text).
narrative_ontology:cs_authority_grounding('6d6d44b2-8184-44b7-b76e-3ea08f87cca4', extraction).
narrative_ontology:cs_interpretation_layer_present('6d6d44b2-8184-44b7-b76e-3ea08f87cca4').
narrative_ontology:cs_reading_relation('6d6d44b2-8184-44b7-b76e-3ea08f87cca4', article_9_war_renunciation__strict_pacifist_reading, coexists_with).
narrative_ontology:cs_reading_relation('6d6d44b2-8184-44b7-b76e-3ea08f87cca4', article_9_war_renunciation__collective_self_defense_reading, influences).
narrative_ontology:cs_axiom('6d6d44b2-8184-44b7-b76e-3ea08f87cca4', foundational, inherent_sovereign_right_to_self_defense).
narrative_ontology:cs_axiom_status(inherent_sovereign_right_to_self_defense, holdable).
narrative_ontology:cs_axiom_grounding('6d6d44b2-8184-44b7-b76e-3ea08f87cca4', inherent_sovereign_right_to_self_defense, deontological).
narrative_ontology:cs_axiom('6d6d44b2-8184-44b7-b76e-3ea08f87cca4', foundational, proportionality_threshold_minimum_necessary).
narrative_ontology:cs_axiom_status(proportionality_threshold_minimum_necessary, holdable).
narrative_ontology:cs_axiom_grounding('6d6d44b2-8184-44b7-b76e-3ea08f87cca4', proportionality_threshold_minimum_necessary, instrumental).
narrative_ontology:cs_reference_frame('6d6d44b2-8184-44b7-b76e-3ea08f87cca4', postwar_peace_identity_with_structural_defense_capacity).
narrative_ontology:cs_drift_state('6d6d44b2-8184-44b7-b76e-3ea08f87cca4', contemporary_east_asian_strategic_competition, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6d6d44b2-8184-44b7-b76e-3ea08f87cca4', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__inherent_right_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, japanese_state_apparatus).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, us_security_alliance).
narrative_ontology:constraint_victim(article_9_war_renunciation__inherent_right_reading, pacifist_constitutional_interpreters).
narrative_ontology:constraint_victim(article_9_war_renunciation__inherent_right_reading, regional_security_skeptics).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, sdf_military_institution).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, collective_defense_proponents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Article 9 to permit 'minimum necessary' Self-Defense Forces (SDF) for territorial defense while maintaining formal renunciation of 'war'. Sets the scope of permitted military activity through Cabinet decisions, SDF doctrine, and budget allocations. Benefits from the interpretation by preserving both constitutional legitimacy and de facto military capacity. Faces sustained interpretive pressure from pacifist reading holders who view any armed forces as violating the text.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, japanese_state_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from Japan's capacity to contribute to regional security within a deniable constitutional frame. The inherent-right reading permits Japan to develop military capability that supports US alliance interests (particularly regarding China, North Korea, Russia) without Japan formally repealing Article 9. US presence underwrites the interpretation's plausibility: collective self-defense doctrine extension (via the 2015 legislation) rests on the inherent-right reading.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, us_security_alliance, beneficiary,
    institutional, generational, mobile, global).

% Hold the strict-pacifist reading: Article 9's language 'never be maintained' is categorical. They bear the cost of living under a constitutional text they regard as systematically reinterpreted away from its plain meaning. Their objections are heard in constitutional court cases and public discourse but do not constrain state apparatus doctrine. Exit options are limited to litigation, public protest, and electoral pressure—tools that have not shifted the consensus interpretation in 75+ years.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, pacifist_constitutional_interpreters, payer,
    organized, biographical, constrained, national).

% Governments and publics in South Korea, China, Taiwan, and Southeast Asia who view Japanese military buildup with historical suspicion. The inherent-right reading legitimates SDF expansion within Japan's constitutional frame while skeptics have limited formal voice in that interpretation process. Their security concerns are acknowledged rhetorically but SDF capabilities continue to grow under the inherent-right framework. Identity-locked: regional security narratives are structured around historical memory of Japanese militarism; accepting the inherent-right reading would require reconstituting that identity frame.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, regional_security_skeptics, payer,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(article_9_war_renunciation__inherent_right_reading, regional_security_skeptics, excluded).

% Operates under the inherent-right reading as the legitimate institutional embodiment of Japan's 'minimum necessary' defensive capacity. Derives organizational legitimacy, budgetary resources, and doctrinal autonomy from the reading. The constraint bounds them: formal doctrine claims no offensive capability, no power-projection beyond territorial defense. In practice, capability creep and US alliance integration have expanded the SDF's actual reach. Trapped: the SDF's entire institutional existence depends on the inherent-right reading; a shift to strict pacifist reading would terminate the institution.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, sdf_military_institution, beneficiary,
    organized, generational, trapped, national).

% Have repeatedly upheld the inherent-right interpretation in landmark cases (Sunakawa, Ienaga, successive SDF constitutionality challenges). They adjudicate disputes about what counts as 'minimum necessary' but have not questioned the basic framework. Their role is to maintain the interpretive stability of the reading rather than to innovate.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% Policy advocates, security scholars, and some politicians who argue that the inherent-right reading should be extended to permit collective self-defense (the 2015 legislation moved in this direction). They benefit from the reading's flexibility—it provides the doctrinal opening through which collective-defense expansion enters. They remain organized but constrained by public opposition and constitutional text.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, collective_defense_proponents, beneficiary,
    moderate, biographical, constrained, national).

% External observers (UN bodies, IHL scholars, human rights organizations) who assess whether Japan's military activities comply with international law. They do not adjudicate the Article 9 reading but do observe and report on the constraints the reading places on Japanese military action. Their assessments feed back into pacifist arguments that the inherent-right reading has become a fiction.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, international_humanitarian_law_community, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reconciles Japan's constitutional renunciation of war with the structural reality of state self-defense rights: permits Japan to maintain military capacity for territorial defense without repealing Article 9, thereby preserving post-war constitutional identity while acknowledging unavoidable security needs. Coordinates between constitutional legitimacy (peace clause) and geopolitical security (alliance, deterrence).
% TRANSFER_FUNCTION: Moves interpretive authority from the text (literal reading) to the state apparatus (institutional reading). The reading transfers legitimacy from 'what the text says' to 'what the state apparatus does while claiming constitutional compliance'. It also transfers security burden to Japan—the inherent-right reading permits Japan to bear military responsibility rather than relying entirely on US nuclear umbrella.
% ABSENT_VOICES: Voices that would demand strict adherence to Article 9's categorical language are present (pacifist movement, constitutional scholars) but are systematically out-weighted in state doctrine formation. Voices from neighboring states skeptical of Japanese military expansion are heard diplomatically but excluded from the formal constitutional interpretation process. Their absence is structural: regional security concerns do not formally constrain Japanese constitutional reading.
% DISAPPEARANCE_RATIONALE: If this reading disappeared overnight and the strict-pacifist reading took institutional hold, Japan's constitutional framework would require immediate disarmament (SDF dissolution or radical restructuring), triggering region-wide security recalibration, possible US strategic repositioning in East Asia, and shifts in alliance structures. If the reading persisted but the interpretation shifted to permit collective defense unambiguously, Japan's military role in regional affairs would expand—a different world-rearrangement. The reading is not a natural fact; its institutional dominance shapes state capacity.
% FOUNDING_PROBLEM: Post-1945 Japan faced a dilemma: Article 9 was imposed as renunciation of militarism, but Cold War geopolitics required defensive military capacity. The founding problem was to maintain constitutional legitimacy (peace identity) while acquiring practical security means.
% FOUNDING_PROBLEM_CORROBORATION: The Japanese state apparatus (Diet, Cabinet, courts) attests the founding problem remains live and the inherent-right reading solves it. Pacifist scholars and movements attest the founding problem was political pressure to rearm, not legitimate security need, and that the 'inherent right' reading is a fiction obscuring constitutional violation. International observers (UN Secretariat, regional governments) note Japan's military capacity has grown far beyond 'minimum necessary' for territorial defense (the reading's own metric), confirming the skeptical reading. No neutral corroborating source outside the state apparatus affirms the founding problem still justifies the reading.
narrative_ontology:disappearance_verdict(article_9_war_renunciation__inherent_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_9_war_renunciation__inherent_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__inherent_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_9_war_renunciation__inherent_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_9_war_renunciation__inherent_right_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_9_war_renunciation__inherent_right_reading_tests).
:- end_tests(article_9_war_renunciation__inherent_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-low (0.38 at interval end, rising from 0.15 at start) because the reading solves a genuine coordination problem—how to maintain defense without violating constitutional identity. But extraction rises over 75 years (from 0.15 to 0.38) as SDF capability grows far beyond 'minimum necessary' and the reading becomes increasingly stretched—the metric for theater_ratio shows the same pattern, rising from 0.18 to 0.41, indicating growing share of performance (rhetorical insistence on 'minimum necessary') relative to function (actual capability). Suppression is moderate (0.52) because pacifist interpretation is present and vocal but institutionally powerless—not silenced, but consistently out-weighted in policy. Accessibility of alternatives collapses significantly (0.68 at interval end) because the state apparatus's exclusive interpretive authority prevents pacifists or regional skeptics from making their reading institutional—once the state interpreter settles on inherent right, alternatives exist only as marginal positions. Resistance remains substantial (0.72) because the pacifist movement has not acquiesced; suppression has not internalized the reading as natural. The time series on measurements shows a secular increase in extraction and theater ratio while suppression plateaus—the reading is increasingly theatrical (capability creep beyond 'minimum necessary') and the suppressive machinery required to maintain it stabilizes rather than intensifying. This pattern is consistent with a constraint evolving from genuine tangled_rope (coordination + extraction) toward piton (performance maintaining an atrophied founding principle).
 *
 * PERSPECTIVAL GAP:
 *   From the japanese_state_apparatus seat: Article 9 is a framework for reconciling constitutional peace identity with geopolitical security—the inherent-right reading is a reasonable interpretation that permits necessary defense. From the pacifist seat: Article 9 is a categorical prohibition that has been systematically violated; the inherent-right reading is institutional gaslighting. From the us_security_alliance seat: the reading is an elegant frame that permits Japanese contribution to regional security while Japan retains political cover from Article 9 repeal—the coordination is real. From the regional_skeptics seat: the reading is a cover story that permits Japanese militarization that threatens regional stability—the interpretation itself is a source of insecurity. The engine should compute different types per seat: the state apparatus and US alliance perceive rope or tangled rope (genuine coordination); pacifists and regional skeptics perceive snare (false legitimacy for power accumulation). The structural asymmetry: the state apparatus controls the interpretation (has institutional power), so its framing dominates; pacifists and skeptics have perception but no institutional power to shift the reading.
 *
 * DIRECTIONALITY LOGIC:
 *   japanese_state_apparatus: d near 0.25-0.35 (moderate beneficiary; benefits from interpretive authority and military legitimacy, but constrained by constitutional text and public reluctance to repeal it; exit is constrained—cannot simply abandon the reading without losing legitimacy). us_security_alliance: d near 0.15-0.25 (beneficiary; gains military contribution and deterrence partner, pays no constitutional cost, but mobile—could pursue other allies; Japan's location makes exit costly for Japan, not for US). pacifist_constitutional_interpreters: d near 0.75-0.85 (targets; bear cost of textual reinterpretation they reject, constrained exit—litigation and protest have failed; identity-locked because rejecting the state's reading means accepting constitutional violation). sdf_military_institution: d near 0.20-0.30 (beneficiary; legitimized and resourced by the reading, but trapped—institution exists only via this interpretation). regional_security_skeptics: d near 0.70-0.80 (targets; excluded from interpretation process, identity-locked to security anxiety, bear diffuse costs of Japanese military expansion without voice in the constraint). The directionality pattern: agenda-setter and both beneficiaries are beneficiaries or symmetric; both victims and excluded skeptics are targets. This is a textbook asymmetric extraction pattern.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits nascent mandatrophy: the founding problem was Cold War geopolitics requiring Japanese rearmament without formal repeal of Article 9. That problem is no longer live—Japan's security environment has shifted to rising powers (China, Russia) and alliance evolution (collective-defense moves), not the 1950s Cold War. The founding_problem_status is CONTESTED, and the disappearance_verdict is WORLD_REARRANGES, which together suggest mandatrophy: if the founding problem died but the constraint persists, it is a zombie. The 2015 collective-defense legislation is the evidence: it extends the reading toward collective defense (a different constraint) without repealing Article 9 (the reading persists). Theater ratio rising (0.41) and extraction plateauing (0.38) suggest the constraint is increasingly performance. Suppression is stable (0.52) but not rising—the pacifist reading is not gaining institutional ground, but pacifist resistance is not being crushed either; it is being routinized and out-weighted indefinitely. The constraint is not yet piton (SDF still serves a real function in deterrence), but it is moving toward piton if the founding problem is truly dead. The mandatrophy question turns on omega 2: is the inherent-right reading a constitutional interpretation or institutional construction? If construction, mandatrophy is already present (the constraint persists for institutional reasons, not founding-problem reasons). If interpretation, mandatrophy is nascent (the constraint serves real coordination needs but is being extended toward collective defense, morphing into a different constraint).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    minimum_necessary_definition,
    'What constitutes ''minimum necessary'' defensive capacity under Article 9''s inherent-right reading? At what point does SDF capability expansion breach the threshold and become offensive capacity?',
    'Comparative analysis: measure SDF force structure, doctrine, and deployments against peer armed forces in the region (South Korea, Taiwan) and universally recognized defensive vs. offensive systems (e.g., power-projection aircraft, overseas basing, amphibious assault capability). If SDF capabilities exceed those of smaller neighbors or include systems absent from acknowledged defensive arsenals, the reading''s own metric fails.',
    'If ''minimum necessary'' is operationalized and SDF is found to exceed it, the reading is falsified by its own terms—the constraint becomes a snare disguised as tangled rope. If the definition remains vague, the reading can absorb capability creep indefinitely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(minimum_necessary_definition, conceptual, 'The inherent-right reading''s metric is internally undefined; measurement would test whether the constraint is stable or has become incoherent.').

omega_variable(
    reading_vs_constructed_legitimacy,
    'Is the inherent-right reading a genuine constitutional interpretation that flows from Article 9''s text and context, or a constructed institutional cover story that permits rearmament while preserving post-war identity?',
    'Textual-historical analysis: examine parliamentary records of Article 9''s drafting (Allied occupation, Japanese input), the actual language (''never be maintained''), its translation history, and compare with how similar constitutional war renunciations have been interpreted elsewhere (Germany, Costa Rica). If the drafting evidence shows intentional categorical prohibition (not the state apparatus reading), the reading is institutional construction.',
    'If the reading is construction, the constraint is a snare (false legitimacy for rearmament) rather than tangled rope (genuine coordination of safety and security). If the reading is genuine interpretation, classification as tangled rope holds. The mandatrophy question turns on this.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_constructed_legitimacy, empirical, 'Whether the inherent-right reading is constitutional or institutional invention.').

omega_variable(
    us_alliance_extraction_mechanism,
    'Does the inherent-right reading benefit Japan as a self-governing state, or does it primarily serve US strategic interests in East Asia by legitimating Japanese rearmament within a deniable constitutional frame?',
    'Structural analysis of alliance gains: compare Japanese security payoffs (deterrence against whom? what threat?) with US benefits (forward military capability, defense treaty binding Japan to US interests). Interview US strategic-planning documents (declassified), Japanese Defense Ministry assessments, and analyze whether the reading permits Japan to pursue independent security or locks Japan into US strategic dependence. If SDF doctrine and capability are consistently integrated with US operational planning (which they are), the arrangement transfers security agency to the US.',
    'If the reading serves primarily US interests, the beneficiary classification shifts: japan_state_apparatus is partly a payer (constrained by alliance), and us_security_alliance is the sole beneficiary. The constraint moves toward snare (disguised subordination) rather than tangled rope (coordinated mutual benefit). Suppression would need to increase (Japanese pacifist objections are suppressed to maintain alliance).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(us_alliance_extraction_mechanism, empirical, 'Whether the reading serves Japan''s autonomous security interests or locks Japan into US strategic dependence.').

omega_variable(
    collective_defense_extension_logic,
    'If the inherent right to self-defense permits ''minimum necessary'' unilateral defense, does the same logic permit collective self-defense when an ally is attacked? Can the reading coherently resist the 2015 collective-defense extension?',
    'Logical analysis: if the reading grounds military capacity in an inherent sovereign right (not a constitutional permission that can be narrowed), then the scope of self-defense (individual vs. collective) cannot be constrained by Article 9 alone—it would be constrained only by international law and strategic judgment. If the reading cannot resist collective-defense extension on logical grounds, it is a stepping stone to a different constraint (the collective_self_defense_reading), not a stable endpoint.',
    'If the inherent-right reading logically leads to collective-defense extension, then the distinction between this reading and the sibling collective_defense_reading becomes unstable—they may be the same constraint at different phases of institutional development. The reading''s claimed type (tangled_rope coordinating autonomy and security) may collapse into snare (the coordination is illusory; institutional expansion is the real function).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(collective_defense_extension_logic, conceptual, 'Whether the inherent-right reading is logically stable or a way-station toward collective defense.').

omega_variable(
    pacifist_suppression_mechanism,
    'Is the suppression of the pacifist reading structural (the state apparatus has institutional power to override textual reading) or internalized (pacifists have come to accept the state interpretation as legitimate)?',
    'Post-constraint analysis: If Article 9 were genuinely repealed (formally, not reinterpreted) and Japan explicitly renounced war renunciation, would pacifist resistance spike, or has 75 years of institutional suppression created normalization? If resistance remains high and coordinated, suppression is structural. If resistance dissipates, suppression is partially internalized (pacifists have absorbed the state''s framing as normal).',
    'If suppression is structural, the constraint''s persistence depends on active institutional enforcement (supporting the tangled_rope claim). If suppression is internalized, the constraint has deeper hold than metrics show—the theater ratio would underestimate inertial persistence. Either way, suppression is higher than the scalar 0.52 suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pacifist_suppression_mechanism, empirical, 'Whether suppression of pacifist interpretation is structural enforcement or internalized acquiescence.').

omega_variable(
    kernel_reading_contest_structure,
    'Which of the three readings of the Article 9 kernel is institutionally dominant, and what mechanism preserves that dominance?',
    'Institutional audit: examine which reading controls SDF doctrine, constitutional court rulings, Diet legislation, international commitments. This reading (inherent right) is institutionally dominant. The mechanism is the state apparatus''s exclusive authority over constitutional interpretation + public reluctance to formally repeal a peace clause + US alliance interests in supporting Japanese rearmament. Dominance is not logical (the pacifist reading has textual support); it is institutional (the state apparatus has the power to interpret).',
    'The dominance is contestable, not settled. The 2015 collective-defense legislation and rising regional tensions make the collective_defense_reading a live institutional competitor. If Japan formally amends Article 9 or adopts collective-defense doctrine unambiguously, this reading collapses. The reading''s stability depends on the state apparatus maintaining its interpretive authority—a political fact, not a constitutional one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_structure, preference, 'The kernel contest is not resolved; institutional dominance of this reading is contingent on political maintenance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__inherent_right_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_9_war_renunciation__inherent_right_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(arti_tr_t15, article_9_war_renunciation__inherent_right_reading, theater_ratio, 15, 0.24).
narrative_ontology:measurement(arti_tr_t30, article_9_war_renunciation__inherent_right_reading, theater_ratio, 30, 0.31).
narrative_ontology:measurement(arti_tr_t45, article_9_war_renunciation__inherent_right_reading, theater_ratio, 45, 0.38).
narrative_ontology:measurement(arti_tr_t60, article_9_war_renunciation__inherent_right_reading, theater_ratio, 60, 0.4).
narrative_ontology:measurement(arti_tr_t75, article_9_war_renunciation__inherent_right_reading, theater_ratio, 75, 0.41).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(arti_be_t15, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 15, 0.22).
narrative_ontology:measurement(arti_be_t30, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 30, 0.32).
narrative_ontology:measurement(arti_be_t45, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 45, 0.37).
narrative_ontology:measurement(arti_be_t60, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 60, 0.38).
narrative_ontology:measurement(arti_be_t75, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 75, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(arti_su_t15, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 15, 0.42).
narrative_ontology:measurement(arti_su_t30, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 30, 0.48).
narrative_ontology:measurement(arti_su_t45, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 45, 0.51).
narrative_ontology:measurement(arti_su_t60, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 60, 0.52).
narrative_ontology:measurement(arti_su_t75, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 75, 0.52).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=75
narrative_ontology:measurement(arti_grid_01, article_9_war_renunciation__inherent_right_reading, accessibility_collapse(class), 0, 0.72).
narrative_ontology:measurement(arti_grid_02, article_9_war_renunciation__inherent_right_reading, accessibility_collapse(class), 75, 0.78).
narrative_ontology:measurement(arti_grid_03, article_9_war_renunciation__inherent_right_reading, accessibility_collapse(individual), 0, 0.68).
narrative_ontology:measurement(arti_grid_04, article_9_war_renunciation__inherent_right_reading, accessibility_collapse(individual), 75, 0.74).
narrative_ontology:measurement(arti_grid_05, article_9_war_renunciation__inherent_right_reading, accessibility_collapse(organizational), 0, 0.58).
narrative_ontology:measurement(arti_grid_06, article_9_war_renunciation__inherent_right_reading, accessibility_collapse(organizational), 75, 0.68).
narrative_ontology:measurement(arti_grid_07, article_9_war_renunciation__inherent_right_reading, accessibility_collapse(structural), 0, 0.62).
narrative_ontology:measurement(arti_grid_08, article_9_war_renunciation__inherent_right_reading, accessibility_collapse(structural), 75, 0.72).
narrative_ontology:measurement(arti_grid_09, article_9_war_renunciation__inherent_right_reading, resistance(class), 0, 0.78).
narrative_ontology:measurement(arti_grid_10, article_9_war_renunciation__inherent_right_reading, resistance(class), 75, 0.76).
narrative_ontology:measurement(arti_grid_11, article_9_war_renunciation__inherent_right_reading, resistance(individual), 0, 0.72).
narrative_ontology:measurement(arti_grid_12, article_9_war_renunciation__inherent_right_reading, resistance(individual), 75, 0.74).
narrative_ontology:measurement(arti_grid_13, article_9_war_renunciation__inherent_right_reading, resistance(organizational), 0, 0.68).
narrative_ontology:measurement(arti_grid_14, article_9_war_renunciation__inherent_right_reading, resistance(organizational), 75, 0.72).
narrative_ontology:measurement(arti_grid_15, article_9_war_renunciation__inherent_right_reading, resistance(structural), 0, 0.58).
narrative_ontology:measurement(arti_grid_16, article_9_war_renunciation__inherent_right_reading, resistance(structural), 75, 0.62).
narrative_ontology:measurement(arti_grid_17, article_9_war_renunciation__inherent_right_reading, stakes_inflation(class), 0, 0.28).
narrative_ontology:measurement(arti_grid_18, article_9_war_renunciation__inherent_right_reading, stakes_inflation(class), 75, 0.42).
narrative_ontology:measurement(arti_grid_19, article_9_war_renunciation__inherent_right_reading, stakes_inflation(individual), 0, 0.32).
narrative_ontology:measurement(arti_grid_20, article_9_war_renunciation__inherent_right_reading, stakes_inflation(individual), 75, 0.48).
narrative_ontology:measurement(arti_grid_21, article_9_war_renunciation__inherent_right_reading, stakes_inflation(organizational), 0, 0.38).
narrative_ontology:measurement(arti_grid_22, article_9_war_renunciation__inherent_right_reading, stakes_inflation(organizational), 75, 0.52).
narrative_ontology:measurement(arti_grid_23, article_9_war_renunciation__inherent_right_reading, stakes_inflation(structural), 0, 0.45).
narrative_ontology:measurement(arti_grid_24, article_9_war_renunciation__inherent_right_reading, stakes_inflation(structural), 75, 0.58).
narrative_ontology:measurement(arti_grid_25, article_9_war_renunciation__inherent_right_reading, suppression(class), 0, 0.58).
narrative_ontology:measurement(arti_grid_26, article_9_war_renunciation__inherent_right_reading, suppression(class), 75, 0.64).
narrative_ontology:measurement(arti_grid_27, article_9_war_renunciation__inherent_right_reading, suppression(individual), 0, 0.52).
narrative_ontology:measurement(arti_grid_28, article_9_war_renunciation__inherent_right_reading, suppression(individual), 75, 0.58).
narrative_ontology:measurement(arti_grid_29, article_9_war_renunciation__inherent_right_reading, suppression(organizational), 0, 0.38).
narrative_ontology:measurement(arti_grid_30, article_9_war_renunciation__inherent_right_reading, suppression(organizational), 75, 0.52).
narrative_ontology:measurement(arti_grid_31, article_9_war_renunciation__inherent_right_reading, suppression(structural), 0, 0.42).
narrative_ontology:measurement(arti_grid_32, article_9_war_renunciation__inherent_right_reading, suppression(structural), 75, 0.56).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_war_renunciation__inherent_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_9_war_renunciation__inherent_right_reading, 0.12).
narrative_ontology:affects_constraint(article_9_war_renunciation__inherent_right_reading, article_9_war_renunciation__strict_pacifist_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__inherent_right_reading, article_9_war_renunciation__collective_self_defense_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the contested Article 9 kernel into three structurally distinct constraint stories. The inherent-right reading (this file) serves as the institutional-baseline reading; the strict-pacifist reading represents the textual-literalist alternative; the collective-defense reading represents the institutional-expansion trajectory. The readings are not different observables of one constraint—they are different constraints grounded in different normative premises about Article 9's scope. The inherent-right reading influences both siblings: it is institutionally dominant (affects what the pacifist reading must argue against), and it provides the logical opening through which collective-defense extension enters (the 2015 legislation relies on the inherent-right reading's proportionality logic). Link via network.affects_constraints to enable contamination analysis: if this reading is delegitimized (e.g., by international law scholarship showing the 'minimum necessary' standard is incoherent), the pacifist reading gains ground and the collective-defense reading loses its grounding. Each reading carries independent ε and beneficiary/victim data; the ε-invariance principle requires separate files. The kernel itself (Article 9, the text) is the shared reference; the readings compete for institutional dominance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_9_war_renunciation__inherent_right_reading, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
