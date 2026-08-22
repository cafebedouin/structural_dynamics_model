% ============================================================================
% CONSTRAINT STORY: article_51_self_defense__narrow_armed_attack_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_51_self_defense__narrow_armed_attack_reading, []).

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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: article_51_self_defense__narrow_armed_attack_reading
 *   human_readable: Article 51 Narrow Armed-Attack Reading (State-Attributable Trigger)
 *   domain: legal/geopolitical
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the Article 51 self-defense
 *   kernel: the narrow armed-attack reading, under which a state may lawfully
 *   use force in self-defense only in response to an actual or imminent armed
 *   attack by a state attributable under international law. Non-state actor
 *   violence does not trigger the right unless attributed to a host state;
 *   preventive force against merely emerging threats is outside the right
 *   entirely. The colloquial label 'self-defense under international law'
 *   decomposes, per the epsilon-invariance principle, into three structurally
 *   distinct claims — this narrow reading, the expansive preventive reading,
 *   and the unable-unwilling doctrine reading — each with its own epsilon,
 *   beneficiary/victim structure, and classification; they are linked as a
 *   constraint family through network.affects_constraints. The claim/metric
 *   gap is deliberate: the constraint is CLAIMED as tangled_rope (genuine
 *   coordination function plus asymmetric burden) while the metrics are
 *   authored independently as describing a moderately extractive, actively
 *   defended arrangement — the engine measures the divergence per seat; the
 *   claim is not reconciled to the metrics.
 *
 * KEY AGENTS:
 *   - - great_power_military_establishments: Primary target (powerful/constrained) — bears the removal of uniquely available force options; retains veto-protected partial arbitrage inside the framework
 *   - - small_and_medium_powers: Primary beneficiary (organized/trapped) — collect legal protection they cannot self-supply; exit means exposure to capability-based politics
 *   - - un_security_council: Agenda-setting institution (institutional/identity_locked) — administers the trigger determination and collects authority from its gatekeeper role
 *   - - international_court_of_justice: Agenda-setting institution (institutional/identity_locked) — fixes the attribution and imminence standards through doctrinal rulings
 *   - - frontline_states_facing_nonstate_attacks: Dual-positioned payer (moderate/trapped) — pays on the non-state-attack axis, collects on the great-power-predation axis
 *   - - host_states_of_nonstate_groups and territorial_host_communities: Excluded seats — liability and risk assigned without their participation
 *   - - international_law_scholars: Analytical observer — sees the full structure and supplies the doctrinal arguments all seats borrow
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__narrow_armed_attack_reading, 0.55).
domain_priors:suppression_score(article_51_self_defense__narrow_armed_attack_reading, 0.58).
domain_priors:theater_ratio(article_51_self_defense__narrow_armed_attack_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__narrow_armed_attack_reading, tangled_rope).
narrative_ontology:human_readable(article_51_self_defense__narrow_armed_attack_reading, "Article 51 Narrow Armed-Attack Reading (State-Attributable Trigger)").
narrative_ontology:topic_domain(article_51_self_defense__narrow_armed_attack_reading, "legal/geopolitical").

domain_priors:requires_active_enforcement(article_51_self_defense__narrow_armed_attack_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__narrow_armed_attack_reading, '400d0430-13d5-416a-b339-531d4c0662c0').
narrative_ontology:cs_kernel_codification('400d0430-13d5-416a-b339-531d4c0662c0', fixed_text).
narrative_ontology:cs_authority_grounding('400d0430-13d5-416a-b339-531d4c0662c0', lineage).
narrative_ontology:cs_interpretation_layer_present('400d0430-13d5-416a-b339-531d4c0662c0').
narrative_ontology:cs_reading_relation('400d0430-13d5-416a-b339-531d4c0662c0', article_51_self_defense__expansive_preventive_reading, forecloses).
narrative_ontology:cs_reading_relation('400d0430-13d5-416a-b339-531d4c0662c0', article_51_self_defense__unable_unwilling_doctrine_reading, forecloses).
narrative_ontology:cs_axiom('400d0430-13d5-416a-b339-531d4c0662c0', foundational, imminence_requirement_excludes_preventive_force).
narrative_ontology:cs_axiom_status(imminence_requirement_excludes_preventive_force, holdable).
narrative_ontology:cs_axiom_grounding('400d0430-13d5-416a-b339-531d4c0662c0', imminence_requirement_excludes_preventive_force, conventional).
narrative_ontology:cs_axiom('400d0430-13d5-416a-b339-531d4c0662c0', foundational, state_attribution_prerequisite_for_self_defense).
narrative_ontology:cs_axiom_status(state_attribution_prerequisite_for_self_defense, holdable).
narrative_ontology:cs_axiom_grounding('400d0430-13d5-416a-b339-531d4c0662c0', state_attribution_prerequisite_for_self_defense, conventional).
narrative_ontology:cs_reference_frame('400d0430-13d5-416a-b339-531d4c0662c0', caroline_charter_narrow_trigger).
narrative_ontology:cs_drift_state('400d0430-13d5-416a-b339-531d4c0662c0', contemporary_asymmetric_conflict_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('400d0430-13d5-416a-b339-531d4c0662c0', '').
narrative_ontology:cs_kernel_id(article_51_self_defense__narrow_armed_attack_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, small_and_medium_powers).
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, un_security_council).
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, international_court_of_justice).
narrative_ontology:constraint_victim(article_51_self_defense__narrow_armed_attack_reading, great_power_military_establishments).
narrative_ontology:constraint_victim(article_51_self_defense__narrow_armed_attack_reading, frontline_states_facing_nonstate_attacks).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, frontline_states_facing_nonstate_attacks).
narrative_ontology:constraint_vindicates(article_51_self_defense__narrow_armed_attack_reading, charter_article_2_4_force_prohibition).
narrative_ontology:constraint_vindicates(article_51_self_defense__narrow_armed_attack_reading, caroline_necessity_and_immediacy_formula).
narrative_ontology:constraint_vindicates(article_51_self_defense__narrow_armed_attack_reading, nicaragua_effective_control_attribution_standard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Fifteen-member body that administers the collective-security framework: it determines when an armed attack has occurred, authorizes collective responses, and its debates are the primary forum where the meaning of 'armed attack' is contested. Five permanent members hold vetoes shaping which violations draw consequences. Its authority depends on remaining the gatekeeper between unilateral and authorized force; it collects deference and legitimacy from that role and cannot abandon it without dissolving its own purpose.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, un_security_council, agenda_setter,
    institutional, generational, identity_locked, global).

% Principal judicial organ whose rulings (Nicaragua 1986, Oil Platforms 2003, DRC v Uganda 2005) fix the attribution and imminence standards defining the rule's content. It compels no one to appear but its doctrine is the reference point every state argues from. It gains centrality when its standards are invoked and loses relevance if states route around it; its jurisdiction and standing are constituted by the framework it interprets.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, international_court_of_justice, agenda_setter,
    institutional, generational, identity_locked, global).

% States without force-projection militaries. They cannot defend themselves against major-power attack by arms; their principal protection is the legal rule forbidding attack except upon actual or imminent armed attack, backed by collective machinery. They contribute votes, basing access, and treaty membership but supply little enforcement muscle. Leaving the framework would mean accepting a world where capability decides.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, small_and_medium_powers, beneficiary,
    organized, generational, trapped, global).

% States with global force-projection capacity and their defense establishments. The rule removes from their option sets the uses of force they alone can execute: striking emerging threats before maturity, crossing borders after non-state attackers, intervening on self-judged necessity. They retain veto-protected positions inside the framework and can absorb reputational cost when they defect, but the verification standard is written against their discretion. Permanent Council seats give them partial arbitrage inside a system they cannot exit.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, great_power_military_establishments, payer,
    powerful, generational, constrained, global).

% States suffering cross-border raids from non-state armed groups based in neighboring territory. On one axis they pay: the rule denies them a lawful response until they prove the host state's attribution, a standard their intelligence often cannot meet. On another axis they collect: the same rule shields them from the major powers surrounding them. Exiting would relieve the first burden by exposing them on the second.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, frontline_states_facing_nonstate_attacks, payer,
    moderate, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(article_51_self_defense__narrow_armed_attack_reading, frontline_states_facing_nonstate_attacks, beneficiary).

% States on whose territory non-state armed groups operate, whether through weakness, complicity, or tolerance. The liability rules hardening around them were built largely over their objections; they were rarely parties when attribution standards settled. Their exposure grows each time a neighbor invokes self-defense against groups they host.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, host_states_of_nonstate_groups, excluded,
    moderate, biographical, constrained, regional).

% Civilian populations in border regions where non-state armed groups operate. The framework assigns them the risks of both the groups and any cross-border response, while giving them no seat in the forums where response rules are set. Their protection depends on arrangements negotiated entirely above them.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, territorial_host_communities, excluded,
    powerless, immediate, trapped, local).

% Irregular forces — insurgencies and transnational militant networks operating across borders. The framework treats them as objects of attribution analysis rather than participants; they would dispute both the liability rules and the state monopoly on force the framework encodes, but no part of the machinery receives their input.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, nonstate_armed_groups, excluded,
    organized, immediate, mobile, regional).

% Academic and doctrinal community producing the commentary through which the rule's meaning stabilizes or shifts. Neither collects nor pays directly; its treatises and case notes supply the arguments every other seat borrows, and its internal debates preview where the doctrine may move.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, international_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_51_self_defense__narrow_armed_attack_reading, diffuse).
narrative_ontology:fixing_cost_class(article_51_self_defense__narrow_armed_attack_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the interstate-anarchy collective-action problem: without a shared, verifiable definition of lawful self-defense, each state's unilateral judgment of necessity triggers security-dilemma spirals. The narrow reading fixes the trigger point — actual or imminent armed attack by an attributable state — so force decisions become checkable and disputes become channelable to collective organs.
% TRANSFER_FUNCTION: Moves strategic freedom from militarily capable states to the collective-security framework and to states incapable of self-protection; moves decision authority over war from individual capitals to the Charter architecture; converts powerful states' unilateral discretion into weaker states' legal entitlement.
% ABSENT_VOICES: Territorial communities hosting non-state armed groups have no seat — the framework assigns them risk without representation. Host states of non-state groups were not consulted when attribution standards hardened, though the standards impose liability on them. Non-state armed groups themselves are objects, not subjects, of the framework. All three would object if admitted; their absence is structural, not incidental.
% DISAPPEARANCE_RATIONALE: If the narrow reading vanished overnight, capable states would fill the vacuum with unilateral necessity judgments, each advancing its own preventive standard; weak states would lose their principal legal shield and reorganize security around alliances and clientage; the Council and Court would lose the trigger determination that constitutes their authority; and the contest would collapse into whichever sibling reading the strongest actors preferred — the interstate order rearranges around raw capability.
% FOUNDING_PROBLEM: The interwar failure: Kellogg-Briand's absolute renunciation of war proved unenforceable because it left no lawful escape for genuine self-defense, inviting either hypocrisy or collapse. The Charter drafters needed a rule that outlawed aggression while preserving a narrow, verifiable right of last resort — how to prohibit war without disarming the victim of aggression.
% FOUNDING_PROBLEM_CORROBORATION: Diplomatic historians and the Nuremberg tribunal record attest the founding problem and its continued salience from outside the beneficiary set; strategic-studies literature documents the interwar collapse the drafters designed against; neutral-state foreign ministries continue to invoke the narrow standard in ways premised on the problem being unresolved. No reliance on beneficiary-only attestation.
narrative_ontology:disappearance_verdict(article_51_self_defense__narrow_armed_attack_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_51_self_defense__narrow_armed_attack_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__narrow_armed_attack_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_51_self_defense__narrow_armed_attack_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_51_self_defense__narrow_armed_attack_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_51_self_defense__narrow_armed_attack_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_51_self_defense__narrow_armed_attack_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_51_self_defense__narrow_armed_attack_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.55: the arrangement removes specific, high-value options from capable states (striking emerging threats early, crossing borders after non-state attackers, self-judged necessity) while leaving large lawful latitude (collective self-defense, Council authorization, consent-based operations) — substantial but bounded extraction. Suppression 0.58: near-universal treaty membership, the prohibition's peremptory character, and reputational machinery hold the arrangement, but defection remains physically possible at recognized cost, so alternatives are pressured rather than eliminated. Theater 0.34: real functional content exists (ICJ rulings with doctrinal bite, observable restraint behavior, authorization practice) diluted by unenforced condemnations and ritual assembly debate. Accessibility_collapse 0.40: the sibling readings remain live, argued, and partly practiced — alternatives do not vanish on understanding the rule. Resistance 0.65: continuous doctrinal counter-offensive by capable states (preventive-war doctrines, unable-or-unwilling advocacy) is the visible signature of the burden. The temporal series runs on one shared eight-point grid (all three metrics authored at every point). The series is not monotonic: theater and enforcement strain peaked during Cold War Council paralysis (1975), fell as judicial doctrine crystallized and post-Cold-War enforcement revived (1986-1995), then climbed again as the terrorism era raised the rule's cost to targeted states (2001-2011) — the oscillation tracks enforcement-regime cycles, not intermittent reinforcement. Suppression_requirement is tracked because the story specifically traces enforcement-capacity change across those regimes.
 *
 * PERSPECTIVAL GAP:
 *   From the Council and Court seats the arrangement is the constitution of interstate peace — the thing that makes force decisions verifiable and disputable instead of autonomic. From the great-power military seat the same structure is a shackle that adversaries who ignore it exploit, and its verification standard reads as if written against their discretion specifically. From the small-state seat it is existential protection, the only shield that does not depend on their own armies. Frontline states experience both faces at once: the rule that denies them response to cross-border raiders is the same rule that restrains the powers around them. The engine computes these divergent per-seat types from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Small and medium powers are declared beneficiaries with trapped exit: the arrangement subsidizes them with security they cannot purchase, driving their derived directionality toward the beneficiary pole. The Council and Court are declared beneficiaries as institutions whose authority the arrangement preserves — they collect deference and doctrinal centrality, sitting near the beneficiary end. Great-power military establishments are declared victims with constrained exit: the burden lands exactly on their distinctive capabilities, and their partial inside-arbitrage (veto seats, absorption of reputational cost) keeps them short of full-target but far toward it. One directionality override is authored: moderate-power seats are set to d=0.62 because the automatic derivation from the victims array alone would push them toward full-target, misreading their dual position — frontline states collect predation-protection on a second axis, and host states bear attribution liability while enjoying the same prohibition against their own neighbors. The override corrects a known blind spot of the derivation chain for same-power, opposite-axis agents.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — outlawing war while preserving a verifiable last resort for the victim of attack — remains live: interstate armed attack still occurs, and the problem the arrangement was built for has not been solved away. Mandatrophy is therefore not resolved, and no sunset clause applies. The classification guards against two mislabelings: reading the arrangement as pure extraction from great powers erases the genuine coordination function (a shared, verifiable trigger that suppresses security-dilemma spirals); reading it as pure coordination erases the asymmetric burden (the rule binds precisely those able to project force, while its principal collectors supply little enforcement muscle). The tangled_rope claim holds both. The watch condition for decay is recorded in the omegas: if the dominant threat environment permanently displaces interstate attack in favor of transnational non-state violence, the founding problem's salience dies while the arrangement persists on inertia — at that point the theater ratio and the dead-status mismatch flag become the diagnostic path toward degraded-operation classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_51_kernel_reading_contestation,
    'This constraint instantiates the narrow_armed_attack_reading of the article_51_self_defense kernel; what structural changes would adoption of the sibling readings (expansive_preventive_reading, unable_unwilling_doctrine_reading) produce?',
    'Track ICJ jurisprudence, Security Council debate records, and patterns of state-practice acquiescence or protest for migration of the trigger conditions toward either sibling.',
    'Under the expansive reading the restraint on great powers drops sharply and weak states lose their principal shield, inverting the beneficiary/victim sets; under the unable-unwilling reading frontline states flip from payers to licensees while host states become targets — per-seat classifications recompute from changed structural data.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_51_kernel_reading_contestation, conceptual, 'Committer structure: sibling readings would relocate the trigger condition and redistribute who pays and who collects.').

omega_variable(
    effective_control_threshold,
    'How much control must a host state exercise over a non-state armed group before the group''s attack becomes attributable to it — Nicaragua''s ''effective control'', the ICTY''s ''overall control'', or something lower?',
    'Doctrinal convergence in ICJ and ILC practice, plus evidentiary outcomes in concrete attribution contests.',
    'A lower threshold converts many non-state attacks into attributable ones, shrinking this reading''s payer set without formally abandoning it; a higher threshold widens the protection gap that fuels the sibling readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effective_control_threshold, empirical, 'The attribution threshold determines how much of the non-state threat universe falls inside the narrow trigger.').

omega_variable(
    imminence_boundary_location,
    'Where does ''imminence'' end — does last-window-of-opportunity logic (mass-casualty weapons, cyber operations) stretch imminence into prevention?',
    'Comparative analysis of claimed-imminence episodes (Osirak 1981, post-2001 assertions) and whether subsequent doctrine absorbed or rejected them.',
    'If imminence stretches, this reading converges toward the expansive sibling and measured extraction from potential target states rises; if held firm, the reading stays structurally distinct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imminence_boundary_location, conceptual, 'The imminent/emerging-threat boundary is the load-bearing wall separating this reading from its expansive sibling.').

omega_variable(
    compliance_basis_internalized_vs_enforced,
    'Does great-power restraint under this reading reflect internalized legality or situational interest, and would restraint survive a period of low reputational cost for defection?',
    'Behavioral comparison of force decisions across reputational-cost regimes, drawing on archival decision records and alliance consultations.',
    'If restraint is purely interest-driven, the arrangement''s persistence tracks power distributions rather than the rule, pushing its operation toward inertial maintenance; if internalized, the coordination function is robust to enforcement slack.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_basis_internalized_vs_enforced, empirical, 'The basis of compliance determines whether the arrangement holds when enforcement attention moves elsewhere.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__narrow_armed_attack_reading, 1945, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1945, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 1945, 0.18).
narrative_ontology:measurement(arti_tr_t1960, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 1960, 0.24).
narrative_ontology:measurement(arti_tr_t1975, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 1975, 0.36).
narrative_ontology:measurement(arti_tr_t1986, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 1986, 0.29).
narrative_ontology:measurement(arti_tr_t1995, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 1995, 0.21).
narrative_ontology:measurement(arti_tr_t2001, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 2001, 0.27).
narrative_ontology:measurement(arti_tr_t2011, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 2011, 0.37).
narrative_ontology:measurement(arti_tr_t2026, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 2026, 0.34).

% Extraction over time
narrative_ontology:measurement(arti_be_t1945, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 1945, 0.32).
narrative_ontology:measurement(arti_be_t1960, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 1960, 0.37).
narrative_ontology:measurement(arti_be_t1975, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 1975, 0.39).
narrative_ontology:measurement(arti_be_t1986, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 1986, 0.47).
narrative_ontology:measurement(arti_be_t1995, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 1995, 0.43).
narrative_ontology:measurement(arti_be_t2001, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 2001, 0.53).
narrative_ontology:measurement(arti_be_t2011, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 2011, 0.56).
narrative_ontology:measurement(arti_be_t2026, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 2026, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1945, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 1945, 0.42).
narrative_ontology:measurement(arti_su_t1960, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 1960, 0.47).
narrative_ontology:measurement(arti_su_t1975, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 1975, 0.53).
narrative_ontology:measurement(arti_su_t1986, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 1986, 0.61).
narrative_ontology:measurement(arti_su_t1995, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 1995, 0.49).
narrative_ontology:measurement(arti_su_t2001, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 2001, 0.57).
narrative_ontology:measurement(arti_su_t2011, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 2011, 0.64).
narrative_ontology:measurement(arti_su_t2026, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 2026, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_51_self_defense__narrow_armed_attack_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_51_self_defense__narrow_armed_attack_reading, expansive_preventive_reading).
narrative_ontology:affects_constraint(article_51_self_defense__narrow_armed_attack_reading, unable_unwilling_doctrine_reading).

% DUAL FORMULATION NOTE:
% 'Self-defense under international law' is a colloquial label covering three structurally distinct claims about the Article 51 trigger; per the epsilon-invariance principle each reading is authored as its own story with its own epsilon, beneficiary/victim structure, and classification, linked as a constraint family. This narrow reading is the upstream member — highest doctrinal entrenchment through ICJ jurisprudence constant — and the contested siblings are downstream: challengers must argue from or against its attribution and imminence standards, so its operation shapes their legitimacy conditions without resolving the contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_51_self_defense__narrow_armed_attack_reading, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
