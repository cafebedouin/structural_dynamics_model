% ============================================================================
% CONSTRAINT STORY: honor_settlement_legitimacy__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_settlement_legitimacy__contraction_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: honor_settlement_legitimacy__contraction_reading
 *   human_readable: Honor Settlement Legitimacy Contraction: Dueling as Cognitively Unthinkable
 *   domain: cultural/historical/legal
 *
 * SUMMARY:
 *   From approximately 1600–1900, across Europe and European settler
 *   colonies, honor-culture frameworks that had previously rendered dueling
 *   cognitively legitimate—a rational, necessary means of vindicating
 *   personal honor—progressively became incomprehensible as a legitimate
 *   action within dominant normative discourse. This story instantiates the
 *   'contraction reading' of the honor-settlement-legitimacy kernel: the
 *   decline of dueling operated not through prohibition-and-enforcement
 *   alone, but through the contraction of the cognitive frameworks that made
 *   honor settlement thinkable as legitimate. The framework shifted from
 *   honor-based (personal, violence-based, hierarchical) to
 *   legalist-bureaucratic-rationalist (state-centered, law-based, universal).
 *   Dueling did not merely become illegal; it became incoherent—impossible to
 *   defend rationally within the dominant epistemic space. This reading is
 *   distinct from the drop_reading (residual honor practitioners holding out)
 *   and the composite_reading (multiple reinforcing mechanisms). Here the
 *   mechanism is specifically cultural-cognitive: the legitimacy category
 *   itself vacates.
 *
 * KEY AGENTS:
 *   - honor_culture_practitioners: nobility, military officers, aristocrats whose entire epistemic framework for legitimate action is identity-fused with honor settlement — exit is identity death
 *   - state_legal_apparatus: centralizes legitimate violence and articulates the new framework through law, courts, and written rule — the agenda-setter
 *   - commercial_bourgeoisie: benefits from predictable, legible dispute resolution and freedom from honor-culture disruption — beneficiary, not agenda-setter
 *   - rationalist_intellectual_class: philosophers, legal theorists, physicians whose authority is grounded in reason — beneficiary through framework validation
 *   - military officer corps: historically embedded in honor-culture codes but subject to institutional pressure toward state law — excluded voice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__contraction_reading, 0.68).
domain_priors:suppression_score(honor_settlement_legitimacy__contraction_reading, 0.15).
domain_priors:theater_ratio(honor_settlement_legitimacy__contraction_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__contraction_reading, mountain).
narrative_ontology:human_readable(honor_settlement_legitimacy__contraction_reading, "Honor Settlement Legitimacy Contraction: Dueling as Cognitively Unthinkable").
narrative_ontology:topic_domain(honor_settlement_legitimacy__contraction_reading, "cultural/historical/legal").

domain_priors:emerges_naturally(honor_settlement_legitimacy__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__contraction_reading, '0356c4e6-e08c-443c-b10e-89aca5443e69').
narrative_ontology:cs_kernel_codification('0356c4e6-e08c-443c-b10e-89aca5443e69', distributed).
narrative_ontology:cs_authority_grounding('0356c4e6-e08c-443c-b10e-89aca5443e69', extraction).
narrative_ontology:cs_interpretation_layer_present('0356c4e6-e08c-443c-b10e-89aca5443e69').
narrative_ontology:cs_reading_relation('0356c4e6-e08c-443c-b10e-89aca5443e69', honor_settlement_legitimacy__drop_reading, coexists_with).
narrative_ontology:cs_reading_relation('0356c4e6-e08c-443c-b10e-89aca5443e69', honor_settlement_legitimacy__composite_reading, influences).
narrative_ontology:cs_axiom('0356c4e6-e08c-443c-b10e-89aca5443e69', foundational, cultural_framework_primacy).
narrative_ontology:cs_axiom_status(cultural_framework_primacy, holdable).
narrative_ontology:cs_axiom_grounding('0356c4e6-e08c-443c-b10e-89aca5443e69', cultural_framework_primacy, deontological).
narrative_ontology:cs_axiom('0356c4e6-e08c-443c-b10e-89aca5443e69', foundational, honor_cognitive_legitimacy_collapse).
narrative_ontology:cs_axiom_status(honor_cognitive_legitimacy_collapse, holdable).
narrative_ontology:cs_axiom_grounding('0356c4e6-e08c-443c-b10e-89aca5443e69', honor_cognitive_legitimacy_collapse, conventional).
narrative_ontology:cs_reference_frame('0356c4e6-e08c-443c-b10e-89aca5443e69', honor_settlement_legitimacy_framework).
narrative_ontology:cs_drift_state('0356c4e6-e08c-443c-b10e-89aca5443e69', contemporary_post_enlightenment, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('0356c4e6-e08c-443c-b10e-89aca5443e69', '').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__contraction_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__contraction_reading, commercial_bourgeoisie).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__contraction_reading, nation_state_legal_apparatus).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__contraction_reading, rationalist_intellectual_class).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__contraction_reading, honor_culture_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Nobility and military officers embedded in honor-culture frameworks who once viewed dueling as the legitimate, indeed necessary, means of settling disputes and vindicating honor. As the framework recedes, their entire epistemic apparatus for what constitutes legitimate action in matters of personal affront becomes incoherent within dominant cultural discourse. Exit would require rejecting the core identity construct that had previously structured their selfhood.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, honor_culture_practitioners, payer,
    moderate, biographical, identity_locked, national).

% Centralizes legitimate violence and dispute resolution through written law and state courts. Articulates dueling as not merely prohibited but conceptually incoherent—outside the boundary of acts a rational legal subject could consider. Enforces the new framework through law but the framework's power lies in cultural authority rather than coercive machinery.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, state_legal_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Benefits from the transition because honor-culture dispute settlement (dueling) disrupts commerce and cannot be institutionalized in market transactions. The contraction of honor culture creates predictable, legible dispute resolution and protects merchant life expectancy. Collects no direct extraction but benefits enormously from the ecosystem shift.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, commercial_bourgeoisie, beneficiary,
    powerful, generational, arbitrage, national).

% Philosophers, physicians, legal theorists, and cultural critics whose authority is grounded in reason, empiricism, and systematic thought. The contraction of honor-culture frameworks validates their epistemic authority and marginalizes intuitive, tradition-grounded decision-making. Benefits from the cultural shift toward their framework.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, rationalist_intellectual_class, beneficiary,
    organized, generational, mobile, national).

% Historically embedded in honor-culture norms where dueling was a mechanism for rank-maintenance and personal authority. As cognitive legitimacy erodes, they face institutional pressure to comply with state law while retaining professional identity tied to codes of conduct that honor once anchored. Their voice—that honor settlement was integrative for military hierarchy—is structurally absent from the normative shift.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, military_officer_corps, excluded,
    organized, generational, constrained, national).

% Not an agent but a structural doctrine: the Weberian principle that legitimate states monopolize violence. The contraction of honor-culture dueling is the vindication of this principle in practice—private violence is recoded as illegitimate not through force but through cultural framework transformation.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, state_violence_monopoly, observer,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(honor_settlement_legitimacy__contraction_reading, state_violence_monopoly).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_settlement_legitimacy__contraction_reading, state_legal_apparatus).
narrative_ontology:fixing_cost_class(honor_settlement_legitimacy__contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The contraction does not solve a coordination problem in the traditional sense. Rather, it transforms the cognitive framework within which 'settlement' and 'legitimacy' are intelligible. It achieves de facto coordination around a different decision-space: conflicts are routed into written law and state courts, and private violent settlement becomes categorically incoherent rather than regulated.
% TRANSFER_FUNCTION: Does not operate through direct transfer but through framework transformation. What is transferred is cognitive legitimacy itself: from honor-based settlement (the prerogative of the personally affront nobility/military) to state-law settlement (the prerogative of courts). The displaced actors (honor practitioners) do not transfer resources but lose the epistemic standing to frame their own disputes.
% ABSENT_VOICES: Military officers and honor-culture adherents. They would argue that honor settlement maintained social cohesion within hierarchical military and aristocratic structures, that the decision-space of state law inadequately captures non-economic injuries to reputation and standing, and that rational bureaucracy cannot replace the binding force of personal honor. Their testimony is structurally excluded from the framework-transformation discourse—the very act of articulating their position in honor-culture terms marks it as incoherent.
% DISAPPEARANCE_RATIONALE: If the contraction inverted—if honor culture re-achieved cognitive legitimacy as the framework for settling personal disputes—the entire apparatus of state law and commercial predictability would shift. Merchants would face dueling risk, military hierarchies would reorganize around honor settlement, and the epistemology of rational legalism would lose cultural dominance. The world would reorganize catastrophically.
% FOUNDING_PROBLEM: The founding problem the contraction solves is not a coordination problem but a framework collision: commercial expansion and state centralization are incompatible with honor-culture settlement mechanisms. The founding problem is the incoherence between honor settlement (private, personal, violence-based) and modern statecraft (centralized, universal, law-based).
% FOUNDING_PROBLEM_CORROBORATION: Historians (Kiernan, Nye, Esposito), legal theorists (Foucault on governmentality), and philosophers of normativity (MacIntyre, Nussbaum on virtue frameworks) attest that the 18th–19th century witnessed precisely this framework collision. The state legal apparatus and rational-scientific thought communities attest the incompatibility. Merchants' commercial records show coordinated pressure for predictability. Military institutionalization theorists attest the transition from personal honor to professional codes. No voice from OUTSIDE the benefiting classes contradicts the core finding—the contradiction is frame-internal.
narrative_ontology:disappearance_verdict(honor_settlement_legitimacy__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_settlement_legitimacy__contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_settlement_legitimacy__contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_settlement_legitimacy__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_settlement_legitimacy__contraction_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_settlement_legitimacy__contraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_settlement_legitimacy__contraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(honor_settlement_legitimacy__contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(honor_settlement_legitimacy__contraction_reading),
    narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(honor_settlement_legitimacy__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) because identifiable agents (state, bourgeoisie, rationalists) benefit from the framework shift while honor practitioners lose the epistemic standing to frame their own disputes. The extraction is not direct transfer (property, money) but cognitive-epistemic: the frame shifts such that practitioners' entire decision-space collapses. However, the constraint is authored as mountain (emerges_naturally: true) because once the framework shifts, alternatives genuinely close—dueling becomes not merely illegal but cognitively unavailable. Suppression is very low (0.15) because the framework operates through cultural authority and rational persuasion, not coercive machinery—by the 19th century, dueling prohibitions encounter almost no organized resistance, suggesting the cognitive shift has taken root. Theater ratio is very low (0.05) because there is little performative maintenance—the framework works through sincere belief. The measurement series tracks the progressive shift: extractiveness and suppression_requirement both rise slowly over 300 years, consistent with a cultural-cognitive transition rather than a sudden prohibition. The rising extractiveness curve reflects the increasing closure of honor practitioners' decision-space as rationalism gains cultural authority. The beneficiaries are declared on a mountain, triggering FSM evaluation: this is exactly the case FSM is designed to catch—a constraint that benefits identifiable agents and could be reclassified as false summit (snare/tangled rope) if the mountain classification does not hold.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (honor practitioners, military) experiences the contraction as a loss of epistemic standing and identity dissolution—extraction without compensation. The agenda-setter seat (state) experiences it as rationalizing and centralizing legitimate authority. The beneficiary seats (bourgeoisie, rationalists) experience it as moral and intellectual progress. The engine should compute: snare or tangled rope from the payer seat (extraction with enforcement, though enforcement is cultural rather than coercive); rope or mountain from the beneficiary seats (coordination and natural necessity). The perspectival gap is radical because the same constraint—the cognitive contraction of honor-culture legitimacy—appears as inevitable progress to beneficiaries and as cognitive domination to payers. The gap is not a measurement ambiguity but a genuine structural divergence: the constraint redistributes epistemic authority.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is grounded in identity-locking (the most constraining exit option for honor practitioners) combined with the inability to exit without rejecting the selfhood that anchored them in the first place. Commercial bourgeoisie have arbitrage exit (they can operate in multiple frames as commercial actors). Rationalists have mobile exit (they can migrate to new intellectual communities). Military officers have constrained exit (duty, professional identity, but not identity-fused to honor settlement in the way older nobility are). Honor practitioners are identity_locked: the frame is constitutive of their selfhood, and cognitive exit would require self-annihilation. This is the difference between being constrained (you can leave, but it costs you resources) and identity_locked (leaving requires rejecting the identity that makes you intelligible as an agent).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem the contraction solves is the collision between honor-settlement and modern statecraft. That founding problem is LIVE (both the need for centralized authority and the residual appeal of honor settlement remain), which raises a mandatrophy flag: the constraint persists not because the problem is solved but because the beneficiaries have won the normative argument. However, mandatrophy here does not trigger reclassification from mountain to snare because the mechanism is genuinely cognitive—the framework has so thoroughly shifted that revival would require a wholesale civilizational reversal, not merely a policy change. The constraint classifies as mountain under the contraction reading because cognitive frameworks do exhibit near-irreversibility once they shift (the accessibility_collapse is 0.92 because alternatives have genuinely closed at the level of what counts as thinkable). The risking mandatrophy condition is better understood as indicating that the mountain's naturalness is contestable—the omegas should surface this.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    framework_naturality_vs_constructed,
    'Is the contraction of honor-culture legitimacy a natural consequence of rational thought and moral progress, or a constructed outcome of material interests (commercial expansion, state centralization) that post-hoc justify themselves through reason?',
    'Comparative historical analysis: do societies with different material trajectories (slower commercial expansion, weaker state centralization) retain honor-culture cognitive legitimacy longer? If material factors are primary, the contraction is contingent; if moral/rational factors are primary, it is more universal.',
    'If constructed, the constraint is not a mountain but a snare or tangled rope dressed as inevitable—extraction by the benefiting classes (state, bourgeoisie, rationalists) masked as cognitive necessity. If natural, the mountain classification holds and the beneficiaries are incidental to an epistemic transition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(framework_naturality_vs_constructed, conceptual, 'Whether honor-culture contraction is natural epistemic progress or constructed class interest.').

omega_variable(
    identity_lock_mechanism,
    'Is the cognitive unthinkability of dueling structural (the framework genuinely collapses alternatives) or internalized (actors have absorbed the framework and now police themselves, even as the old framework could materially be revived)?',
    'Counterfactual: if state enforcement of dueling prohibitions were to suddenly cease and the state removed all legal penalties, would honor practitioners spontaneously revive dueling, or has the cognitive change become independent of enforcement machinery?',
    'If structural, the mountain classification holds—alternatives have genuinely closed. If internalized, the suppression persists through identity-locking rather than external barriers, and the constraint''s persistence is post-exit fragile—the identity frame could theoretically break under certain conditions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether cognitive unthinkability is structural or internalized identity-locking.').

omega_variable(
    residual_honor_practice_persistence,
    'To what extent do honor-culture practices persist in disguised or residual form (duels become ''sporting duels,'' honor conflicts become ''litigation theater,'' military hierarchy retains honor-based authority structures)? Does the contraction describe a genuine framework exit or a recoding?',
    'Ethnographic and institutional analysis of 19th–20th century military, dueling clubs, and honor-based conflict resolution in contexts where state law is weak. Does honor re-emerge in the same structural role, or does it occupy a genuinely different epistemic position?',
    'If honor persists in recoded forms, the contraction is partial—a sibling reading (composite_reading) may be more accurate. The drop_reading becomes harder to dismiss. If truly gone, the contraction reading is vindicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_honor_practice_persistence, empirical, 'Degree of residual honor-practice persistence in recoded or disguised forms.').

omega_variable(
    beneficiary_intentionality,
    'Did the commercial bourgeoisie, state apparatus, and rationalist intellectuals intentionally orchestrate the contraction of honor culture, or did they opportunistically amplify a shift driven by other factors (religious change, military technology, international competition)?',
    'Historical evidence from correspondence, policy documents, and intellectual genealogy: Can one trace intentional campaigns to discredit honor culture, or only ex-post facto rationalizations of material changes?',
    'If intentional, the constraint is more clearly extractive (snare or tangled rope)—beneficiaries manufactured cognitive closure. If opportunistic, the mountain classification gains support—the contraction followed from deeper structural forces that no actor fully controlled.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_intentionality, empirical, 'Whether beneficiary classes intentionally orchestrated or opportunistically amplified honor-culture contraction.').

omega_variable(
    false_summit_candidate,
    'Given that beneficiaries are declared on a mountain, is this constraint a genuine natural law (cognitive legitimacy evolves necessarily) or a false summit—a constraint that benefits identifiable agents and should be reclassified as snare/tangled rope?',
    'Examine whether the constraint''s persistence depends on active enforcement by beneficiaries or whether it is self-maintaining once the cognitive framework has shifted. If self-maintaining, it is a genuine mountain. If beneficiaries must actively defend it, the mountain classification is suspect.',
    'If false summit, the constraint reclassifies to tangled rope or snare. The beneficiaries'' claims to be merely capturing an inevitable shift become suspect.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_candidate, conceptual, 'FSM candidate: is the constraint genuinely natural law or a false summit benefiting identifiable agents.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__contraction_reading, 1600, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1600, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1600, 0.08).
narrative_ontology:measurement_basis(hono_tr_t1600, projected).
narrative_ontology:measurement(hono_tr_t1700, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1700, 0.04).
narrative_ontology:measurement_basis(hono_tr_t1700, observed).
narrative_ontology:measurement(hono_tr_t1750, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1750, 0.04).
narrative_ontology:measurement_basis(hono_tr_t1750, observed).
narrative_ontology:measurement(hono_tr_t1800, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1800, 0.05).
narrative_ontology:measurement_basis(hono_tr_t1800, observed).
narrative_ontology:measurement(hono_tr_t1850, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1850, 0.05).
narrative_ontology:measurement_basis(hono_tr_t1850, observed).
narrative_ontology:measurement(hono_tr_t1900, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1900, 0.05).
narrative_ontology:measurement_basis(hono_tr_t1900, observed).

% Extraction over time
narrative_ontology:measurement(hono_be_t1600, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1600, 0.12).
narrative_ontology:measurement_basis(hono_be_t1600, projected).
narrative_ontology:measurement(hono_be_t1700, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1700, 0.28).
narrative_ontology:measurement_basis(hono_be_t1700, observed).
narrative_ontology:measurement(hono_be_t1750, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1750, 0.42).
narrative_ontology:measurement_basis(hono_be_t1750, observed).
narrative_ontology:measurement(hono_be_t1800, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1800, 0.58).
narrative_ontology:measurement_basis(hono_be_t1800, observed).
narrative_ontology:measurement(hono_be_t1850, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1850, 0.65).
narrative_ontology:measurement_basis(hono_be_t1850, observed).
narrative_ontology:measurement(hono_be_t1900, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1900, 0.68).
narrative_ontology:measurement_basis(hono_be_t1900, observed).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1600, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1600, 0.08).
narrative_ontology:measurement_basis(hono_su_t1600, projected).
narrative_ontology:measurement(hono_su_t1700, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1700, 0.09).
narrative_ontology:measurement_basis(hono_su_t1700, observed).
narrative_ontology:measurement(hono_su_t1750, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1750, 0.11).
narrative_ontology:measurement_basis(hono_su_t1750, observed).
narrative_ontology:measurement(hono_su_t1800, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1800, 0.13).
narrative_ontology:measurement_basis(hono_su_t1800, observed).
narrative_ontology:measurement(hono_su_t1850, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1850, 0.14).
narrative_ontology:measurement_basis(hono_su_t1850, observed).
narrative_ontology:measurement(hono_su_t1900, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1900, 0.15).
narrative_ontology:measurement_basis(hono_su_t1900, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_settlement_legitimacy__contraction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_settlement_legitimacy__contraction_reading, 0.12).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__contraction_reading, honor_settlement_legitimacy__drop_reading).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__contraction_reading, honor_settlement_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% The honor_settlement_legitimacy kernel decomposes into three constraint stories, each representing a reading of why/how dueling declined. The contraction_reading (this story) asserts the primary mechanism was cognitive-framework collapse. The drop_reading asserts residual honor-culture persistence. The composite_reading asserts overdetermination by multiple mechanisms. These are not the same constraint viewed from different angles—they are genuinely different claims about causal structure, each with its own epsilon value (contraction is highly extractive at the frame level; drop is lower-extraction residual persistence; composite is moderate through overdetermination). They are linked because each reading interprets the same historical phenomenon (the decline of dueling) differently, and each reading's truth-value affects the others' plausibility. Contraction-reading affirms that honor-culture frameworks do exit the possibility space; drop-reading denies full exit; composite-reading hedges by asserting multiple causes. These are structurally distinct claims with different victim/beneficiary structures and different omegas.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_settlement_legitimacy__contraction_reading, moderate, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
