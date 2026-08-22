% ============================================================================
% CONSTRAINT STORY: montevideo_statehood_criteria__constitutive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_montevideo_statehood_criteria__constitutive_reading, []).

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
 *   constraint_id: montevideo_statehood_criteria__constitutive_reading
 *   human_readable: Constitutive Recognition Rule for Statehood
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   The constitutive reading of the Montevideo Convention's statehood
 *   criteria asserts that recognition by the existing community of states is
 *   a constitutive requirement for statehood—a polity cannot become a state
 *   unless recognized, regardless of whether it meets the Montevideo
 *   Convention's four objective criteria (defined territory, permanent
 *   population, effective government, capacity to enter relations). Under
 *   this reading, statehood is not a discovered fact but a created status:
 *   the existing state system can withhold recognition indefinitely, using it
 *   as a gatekeeping mechanism and geopolitical leverage. This reading
 *   assigns extraction to the constraint because it grants the existing state
 *   community veto power over self-determination and new entrants,
 *   conditioning access to international legal status on political factors
 *   beyond objective capacity. The reading is contested by the declaratory
 *   reading (which treats objective criteria as determinative) and the hybrid
 *   reading (which adds normative legitimacy criteria). This constraint story
 *   generates ONLY the constitutive reading as a clean constraint, not the
 *   hybrid or all three together.
 *
 * KEY AGENTS:
 *   - existing_state_community: the institutionalized set of recognized states that collectively enforce statehood standards through recognition voting, UN admission, and treaty participation gatekeeping
 *   - unrecognized_polities: political entities (e.g., Northern Cyprus, Kosovo, Palestine, Somaliland) meeting Montevideo criteria but denied recognition
 *   - aspiring_state_movements: independence movements within existing states seeking sovereign status through recognition acquisition
 *   - great_powers: permanent UN Security Council members with effective veto over recognition of strategically significant polities
 *   - international_law_scholars: analytical observers who interpret the rule and sometimes advocate for reform
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__constitutive_reading, 0.68).
domain_priors:suppression_score(montevideo_statehood_criteria__constitutive_reading, 0.71).
domain_priors:theater_ratio(montevideo_statehood_criteria__constitutive_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__constitutive_reading, tangled_rope).
narrative_ontology:human_readable(montevideo_statehood_criteria__constitutive_reading, "Constitutive Recognition Rule for Statehood").
narrative_ontology:topic_domain(montevideo_statehood_criteria__constitutive_reading, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(montevideo_statehood_criteria__constitutive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__constitutive_reading, '989055f4-9627-4094-94ea-b3179b9f0aff').
narrative_ontology:cs_kernel_codification('989055f4-9627-4094-94ea-b3179b9f0aff', formalized).
narrative_ontology:cs_authority_grounding('989055f4-9627-4094-94ea-b3179b9f0aff', extraction).
narrative_ontology:cs_interpretation_layer_present('989055f4-9627-4094-94ea-b3179b9f0aff').
narrative_ontology:cs_reading_relation('989055f4-9627-4094-94ea-b3179b9f0aff', montevideo_statehood_criteria__declaratory_reading, forecloses).
narrative_ontology:cs_reading_relation('989055f4-9627-4094-94ea-b3179b9f0aff', montevideo_statehood_criteria__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('989055f4-9627-4094-94ea-b3179b9f0aff', foundational, recognition_constitutes_statehood).
narrative_ontology:cs_axiom_status(recognition_constitutes_statehood, holdable).
narrative_ontology:cs_axiom_grounding('989055f4-9627-4094-94ea-b3179b9f0aff', recognition_constitutes_statehood, deontological).
narrative_ontology:cs_axiom('989055f4-9627-4094-94ea-b3179b9f0aff', foundational, existing_state_community_retains_admission_veto).
narrative_ontology:cs_axiom_status(existing_state_community_retains_admission_veto, holdable).
narrative_ontology:cs_axiom_grounding('989055f4-9627-4094-94ea-b3179b9f0aff', existing_state_community_retains_admission_veto, instrumental).
narrative_ontology:cs_reference_frame('989055f4-9627-4094-94ea-b3179b9f0aff', constitutive_recognition_authority).
narrative_ontology:cs_drift_state('989055f4-9627-4094-94ea-b3179b9f0aff', contemporary_self_determination_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('989055f4-9627-4094-94ea-b3179b9f0aff', '').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__constitutive_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__constitutive_reading, existing_state_community).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__constitutive_reading, unrecognized_polities).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__constitutive_reading, aspiring_state_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__constitutive_reading, aspiring_state_movements).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__constitutive_reading, great_powers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The established community of recognized states collectively defines and enforces statehood standards through diplomatic recognition, treaty participation admission, and UN membership voting. They retain the structural power to admit new states or withhold recognition indefinitely. This gatekeeping function preserves institutional stability and allows existing states to condition recognition on political outcomes they prefer (alignment, border acceptance, governance standards). Recognition itself becomes a transferable good the existing community controls.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, existing_state_community, agenda_setter,
    institutional, generational, arbitrage, universal).

% Political entities meeting the Montevideo Convention's objective criteria (defined territory, permanent population, effective government, capacity to enter relations) but denied recognition by the existing state community. They are excluded from UN participation, treaty signature, international borrowing, diplomatic immunity, and formal economic access. Statehood exists as a recognition-dependent status beyond their control. Exit would mean dissolution or absorption by a recognized state.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, unrecognized_polities, payer,
    powerless, generational, trapped, universal).

% Independence movements within existing states seeking sovereign status. They must negotiate both objective territorial and institutional prerequisites AND recognition from the state system. Recognition becomes conditional on political factors (great-power strategic interests, regional stability narratives, majority-state voting coalitions). Some movements gain effective statehood de facto before de jure recognition; others remain indefinitely blocked. They bear the cost of institutional exclusion while working to satisfy gatekeepers.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, aspiring_state_movements, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(montevideo_statehood_criteria__constitutive_reading, aspiring_state_movements, beneficiary).

% Veto power holders (UN Security Council members) who can effectively block recognition of entities in their spheres of influence or whose recognition would constrain great-power interests. Recognition becomes a bargaining chip: recognizing or withholding recognition of disputed polities (Northern Cyprus, Kosovo, Taiwan, Palestine) directly advances or constrains geopolitical competition. The constitutive rule gives them institutional tools to shape the state system.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, great_powers, beneficiary,
    powerful, generational, analytical, universal).

% Theorists and legal commentators who interpret the constitutive rule, debate its justification, and assess whether particular cases meet the recognition criterion. They document the rule's operation and sometimes produce scholarship advocating for reform (e.g., automaticity thresholds, depoliticized recognition criteria). Their analysis does not control state practice but shapes the narrative framing of recognition decisions.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, international_law_scholars, observer,
    analytical, biographical, analytical, universal).

% Residents of unrecognized polities or independence-seeking territories who are excluded from international legal participation, diplomatic representation, and formal economic structures. They live under effective government structures that lack statehood's legal status and its attendant protections (treaty-based rights, ICJ access, formal military law). They have no seat in the recognition debate despite bearing the lived consequences of exclusion.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, territorial_populations, excluded,
    powerless, biographical, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(montevideo_statehood_criteria__constitutive_reading, existing_state_community).
narrative_ontology:fixing_cost_class(montevideo_statehood_criteria__constitutive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The existing state community coordinates on a shared definition of statehood and operates a single admission pathway for new members, preventing fragmentation of the international legal system into competing standards. Each state gets consistent criteria for recognizing peers and a unified framework for treaty participation and diplomatic relations.
% TRANSFER_FUNCTION: Existing states collectively transfer recognition authority to themselves, extracting the power to determine statehood and conditioning recognition on political factors (alignment, territorial acceptance, governance preferences) beyond the Montevideo Convention's objective criteria. Unrecognized polities transfer sovereignty submission—they must satisfy gatekeepers whose interests diverge from objective statehood criteria. Great powers extract geopolitical leverage: they weaponize recognition as a bargaining chip.
% ABSENT_VOICES: Territorial populations in unrecognized polities are structurally excluded—they live under government structures denied statehood status but have no formal voice in recognition decisions. International law scholars arguing for automaticity thresholds or depoliticized criteria are heard but not binding. Alternative statehood theorists (declaring statehood unilaterally, functional criteria decoupled from diplomatic recognition) are treated as illegitimate within the existing community's framework and remain excluded.
% DISAPPEARANCE_RATIONALE: If constitutive recognition were suddenly replaced by automaticity (any polity meeting Montevideo criteria is ipso facto a state), the international system would reorganize dramatically: existing states would lose gatekeeping power, treaty and UN participation would become automatic, and geopolitical recognition leverage would evaporate. The state system would fragment or recompact around the new rules. Great-power veto over new states would cease. Unrecognized polities would gain standing. This is a reorganizing shift.
% FOUNDING_PROBLEM: The 19th-century state system lacked formal criteria for statehood admission. Existing states risked destabilization from territorial fragmentation, contested sovereignties, and uncontrolled new state creation. Recognition by the existing community provided a stabilizing mechanism: admission required consensus (or at least non-opposition by major powers), preventing chaotic proliferation and preserving the closed-set nature of the state system.
% FOUNDING_PROBLEM_CORROBORATION: Realist international law scholars and statesmen attests the founding problem—destabilization from uncontrolled secession and competing statehood claims—remains live and justifies gatekeeping. Self-determination advocates and independence movements attest the founding problem is solved (borders are now fixed, international law has developed conflict-of-laws machinery) and constitutive recognition persists as structural veto power over self-determination. UN fact-finding missions and independent legal analysis support both readings, depending on interpretation. No corroboration from outside the existing state community on the necessity of the current gatekeeping.
narrative_ontology:disappearance_verdict(montevideo_statehood_criteria__constitutive_reading, world_rearranges).
narrative_ontology:founding_problem_status(montevideo_statehood_criteria__constitutive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(montevideo_statehood_criteria__constitutive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(montevideo_statehood_criteria__constitutive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(montevideo_statehood_criteria__constitutive_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(montevideo_statehood_criteria__constitutive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(montevideo_statehood_criteria__constitutive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(montevideo_statehood_criteria__constitutive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.68) because the constitutive rule grants the existing state community discretionary control over statehood—recognition is not automatic on objective criteria but conditional on political assessment and consensus-building controlled by incumbents. The measurement series shows extractiveness rising from 0.55 to 0.68 over the first 25 time points, then plateauing, tracking the historical accumulation of unrecognized polities (Kosovo 2008, South Sudan 2011, then stagnation) as the constraint's gatekeeping function solidifies. Suppression is high (0.71) because enforcement of the constitutive rule depends on coordinated non-recognition by major states—great powers must sustain the veto, and rival polities' attempts to declare unilateral statehood must be delegitimized (not recognized). Theater is moderate (0.42) and rising: existing states justify the rule as system-stabilizing (preventing chaotic secession) and legitimacy-protecting (ensuring that new states meet civilizational standards), but the actual function is increasingly transparent as geopolitical leverage—the gap between stated justification (stability) and observed practice (recognition withheld for strategic reasons) grows, hence theater_ratio rises from 0.25 to 0.42. Accessibility_collapse is moderate (0.62): alternative statehood paths (unilateral declaration, functional international participation without recognition, regional union memberships) exist but are delegitimized and lack the legal standing recognition provides. Resistance is substantial (0.58): independence movements, unrecognized polities, and self-determination advocates actively contest the rule; decolonization in the 1960s-70s produced dozens of cases of recognized statehood against imperial resistance, and ongoing conflicts (Palestine, Taiwan, Kurdish independence movements) sustain resistance.
 *
 * PERSPECTIVAL GAP:
 *   From the existing-state-community seat, the constitutive rule is coordination: all states agree on admission criteria and the stabilizing benefit of gatekeeping. From the unrecognized-polity and aspiring-movement seats, it is pure extraction: statehood status is withheld despite meeting objective criteria, and exit is impossible (you cannot cease to exist to escape the constraint—dissolution is the only alternative). Great powers experience it as leverage: recognition becomes a bargaining tool in geopolitical contests. The engine should compute the rule as tangled_rope (coordination function for incumbents, asymmetric extraction from new entrants) with substantial divergence between seats. The existing community's seat and the unrecognized polities' seat have opposite directionalities: the community sits near full beneficiary (d ≈ 0.2), unrecognized polities sit near full target (d ≈ 0.85).
 *
 * DIRECTIONALITY LOGIC:
 *   The existing state community is the primary beneficiary: they define the rules, control admission, retain veto power, and extract geopolitical leverage by conditioning recognition on strategic outcomes. They face low extraction cost (no one can withhold recognition from them); their directionality is near beneficiary (d ≈ 0.15-0.25). Unrecognized polities are the primary targets: they bear the cost of exclusion from treaty participation, UN membership, formal economic access, and diplomatic immunity, with no power to alter the gatekeeping standard. Their directionality is near full target (d ≈ 0.80-0.90). Great powers are moderate beneficiaries: they get leverage over strategic polities (controlling recognition of competitors or allies), but they also bear some cost (maintaining the fiction that recognition is based on objective criteria requires performative justifications; strategic competitors' recognition blocks consume diplomatic capital). Great-power directionality sits near 0.35. Aspiring movements face binary extraction: before independence, they are constrained by the existing state they are leaving; after independence, they are constrained by the existing state community's recognition gatekeeping. Their directionality shifts from 0.50 (symmetric within-state) to 0.80 (post-independence target) once the movement crosses the independence threshold.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—preventing chaotic territorial fragmentation and uncontrolled secession—was live in the 19th century when the state system was consolidating. By the late 20th century, borders had stabilized and international law developed conflict-of-laws machinery; the objective need for gatekeeping declined. The constraint's persistence through the 21st century is driven not by the founding problem (which has substantially solved) but by (1) incumbent-state interest in controlling admission and (2) geopolitical leverage extraction from recognition-withheld on unrecognized polities. The founding problem / disappearance_verdict mismatch (status=contested, verdict=world_rearranges) correctly signals mandatrophy: the founding justification is contested or dead, but the constraint persists. The rising theater_ratio (0.25 to 0.42) tracks the widening gap between the stated justification (stability) and observed practice (strategic leverage). If the constraint vanished (replaced by automaticity: any entity meeting Montevideo criteria is ipso facto a state), the world would reorganize substantially—existing states would lose veto power, unrecognized polities would gain standing, and great-power leverage would evaporate. This is consistent with a tangled-rope / zombie classification: the coordination function has atrophied, but the extraction machinery persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    recognition_as_constitutive_vs_declarative,
    'Is recognition a constitutive requirement (statehood exists only when recognized) or a declaratory mechanism (statehood is constituted by objective criteria, recognition is documentary)?',
    'State practice and treaty jurisprudence: do unrecognized polities gain statehood upon meeting objective criteria even without recognition, or must they obtain recognition to enter the state system? The International Court of Justice has issued opinions treating recognition as both constitutive and declarative in different contexts; the tension remains unresolved.',
    'If recognition is purely declarative, the Montevideo criteria become determinative and existing states lose gatekeeping veto power over new states. If constitutive, the constraint''s extraction mechanism (statehood denial despite objective capacity) is justified. This is the core interpretive dispute.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(recognition_as_constitutive_vs_declarative, conceptual, 'Whether statehood is created by recognition or merely documented by it.').

omega_variable(
    gatekeeping_necessity_and_stability,
    'Is recognition gatekeeping necessary for international system stability, or has the state system''s borders and institutions stabilized sufficiently that automaticity would not cause chaotic fragmentation?',
    'Counterfactual analysis comparing regions with strong gatekeeping (UN Security Council veto over new states) to regions with weaker gatekeeping (EU expansion to new democracies, MERCOSUR). Does automaticity correlate with system destabilization or peaceful expansion?',
    'If automaticity produces stable expansion (as EU experience suggests), the founding problem has substantially solved and constitutive recognition persists as pure extraction. If gatekeeping prevents destabilization that would occur under automaticity, the coordination function remains live and the constraint is genuinely tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeping_necessity_and_stability, empirical, 'Whether the constraint''s gatekeeping function remains necessary for international stability.').

omega_variable(
    geopolitical_leverage_masquerade,
    'To what extent is the constitutive rule used for geopolitical leverage (withholding recognition of competitors, conditioning recognition on alignment) versus objective assessment of statehood capacity?',
    'Pattern analysis of recognition decisions: do states consistently recognize polities that meet objective criteria? Do they withhold recognition from objective-criteria-meeting polities when strategic interests diverge? The rising theater_ratio in measurements suggests geopolitical leverage is growing as a proportion of enforcement activity.',
    'If leverage is substantial and growing, the constraint is increasingly transparent as extraction (snare or piton, not rope). If recognition decisions correlate primarily with objective capacity, the constraint is legitimately tangled_rope. The theater measurement trajectory is diagnostic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geopolitical_leverage_masquerade, empirical, 'The proportion of recognition decisions driven by geopolitical leverage versus objective statehood capacity.').

omega_variable(
    unilateral_declaration_alternative,
    'Can a polity effectively escape the constitutive rule by unilaterally declaring statehood and building de facto state capacity (government, borders, international participation) without formal recognition?',
    'Observational study of de facto states (Northern Cyprus, Transnistria, Somaliland, Kosovo pre-recognition): do they gain functional statehood status and international participation despite formal non-recognition? Do they eventually secure recognition once de facto capacity is demonstrated?',
    'If unilateral declaration enables functional statehood and eventual recognition, the constitutive rule is performative—recognition follows de facto capacity rather than constituting it. If unilateral declaration produces indefinite exclusion (trapped de facto states), the rule''s suppression mechanism is effective and the constraint''s extraction is real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unilateral_declaration_alternative, empirical, 'Whether unilateral statehood declaration provides an effective exit from recognition gatekeeping.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__constitutive_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mont_tr_t0, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(mont_tr_t5, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(mont_tr_t10, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement(mont_tr_t15, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(mont_tr_t20, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(mont_tr_t25, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement(mont_tr_t30, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(mont_tr_t35, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 35, 0.42).
narrative_ontology:measurement(mont_tr_t40, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(mont_be_t0, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(mont_be_t5, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(mont_be_t10, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(mont_be_t15, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(mont_be_t20, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(mont_be_t25, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(mont_be_t30, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(mont_be_t35, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement(mont_be_t40, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(mont_su_t0, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(mont_su_t5, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 5, 0.64).
narrative_ontology:measurement(mont_su_t10, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(mont_su_t15, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(mont_su_t20, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(mont_su_t25, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(mont_su_t30, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(mont_su_t35, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 35, 0.71).
narrative_ontology:measurement(mont_su_t40, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(montevideo_statehood_criteria__constitutive_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(montevideo_statehood_criteria__constitutive_reading, 0.12).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__constitutive_reading, montevideo_statehood_criteria__declaratory_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__constitutive_reading, montevideo_statehood_criteria__hybrid_reading).

% DUAL FORMULATION NOTE:
% The montevideo_statehood_criteria kernel decomposes into three structurally distinct constraints corresponding to three live readings: (1) constitutive_reading (this story): recognition is a constitutive requirement; existing states retain gatekeeping veto; unrecognized polities are trapped. ε=0.68, type=tangled_rope. (2) declaratory_reading: objective criteria are determinative; recognition is documentary; existing states cannot deny statehood to objective-criteria-meeting polities. ε≈0.15, type=rope. (3) hybrid_reading: both objective criteria AND normative legitimacy required; adds ideological gatekeeping (democracy, human rights, non-aggression). ε≈0.72, type=snare. The three readings have incompatible ε values because they define the referent (what statehood IS) differently. The constitutive reading treats the existing state community's veto power as the constraint's primary feature; the declaratory reading treats the veto as absent; the hybrid reading treats the veto as present but justified by normative criteria. Each reading is a valid interpretation of the same kernel; each generates a different constraint story. The stories are linked by network.affects_constraints to model the interpretive contest: the constitutive reading's gatekeeping power (recognition required) directly constrains the declaratory reading's automaticity and the hybrid reading's legitimacy bar.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(montevideo_statehood_criteria__constitutive_reading, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
