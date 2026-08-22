% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__absolute_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalian_sovereignty__absolute_sovereignty, []).

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
 *   constraint_id: westphalian_sovereignty__absolute_sovereignty
 *   human_readable: Absolute State Sovereignty and Non-Interference Doctrine
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint embodies the absolute sovereignty reading of the
 *   contested Westphalian kernel: the doctrine that states possess
 *   unconditional authority over their domestic affairs and external
 *   interference is categorically illegitimate. The reading originated
 *   post-1648 (Peace of Westphalia) as protection against religious/dynastic
 *   intervention; it was formalized post-1945 (UN Charter Article 2(7)) as
 *   protection against hegemonic intervention in the post-colonial order.
 *   This reading asserts that sovereignty is binary (either absolute or not)
 *   and that interference admits no legitimate exception. The constraint
 *   benefits authoritarian regimes by providing a legal shield against
 *   accountability; it extracts from populations trapped within those regimes
 *   who have no legal recourse to external remedy. The constraint is claimed
 *   as tangled rope (coordination function: preventing hegemonic
 *   intervention; asymmetric extraction: authoritarian regimes benefit from
 *   non-interference shield while repressed populations bear the costs). The
 *   measurement series shows extractiveness and suppression rising from 1945
 *   (founding) to 2010, then plateauing, reflecting the constraint's
 *   increasing capture by authoritarian regimes and the emergence of
 *   competing doctrines (Responsibility to Protect, humanitarian
 *   intervention, conditional sovereignty) that have constrained but not
 *   replaced the absolute sovereignty reading.
 *
 * KEY AGENTS:
 *   - Authoritarian state regimes: primary beneficiaries (d ≈ 0.1, nearly full beneficiary); institutional power, arbitrage exit options, global scope
 *   - Populations under repressive regimes: primary victims (d ≈ 0.95, nearly full target); powerless, trapped exit, biographical horizon
 *   - Liberal democratic states: agenda-setters (d ≈ 0.5, symmetric to slightly beneficiary); institutional power, arbitrage exit, global scope. Ambiguous: they endorse absolute sovereignty while conducting selective humanitarian intervention
 *   - Human rights advocates & NGOs: excluded (would dispute the reading); moderate power, constrained exit, global scope
 *   - Post-colonial states: secondary beneficiaries (d ≈ 0.35, moderate beneficiary); organized power, mobile exit, global scope. Benefit from non-interference shield against great-power domination but are also harmed when their own governments use the shield against accountability to their populations
 *   - International law scholars: observers (analytical seat); moderate power, analytical exit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__absolute_sovereignty, 0.52).
domain_priors:suppression_score(westphalian_sovereignty__absolute_sovereignty, 0.71).
domain_priors:theater_ratio(westphalian_sovereignty__absolute_sovereignty, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, extractiveness, 0.52).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__absolute_sovereignty, tangled_rope).
narrative_ontology:human_readable(westphalian_sovereignty__absolute_sovereignty, "Absolute State Sovereignty and Non-Interference Doctrine").
narrative_ontology:topic_domain(westphalian_sovereignty__absolute_sovereignty, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(westphalian_sovereignty__absolute_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__absolute_sovereignty, 'a8b5a47b-e213-4086-a159-344b84f231ad').
narrative_ontology:cs_kernel_codification('a8b5a47b-e213-4086-a159-344b84f231ad', fixed_text).
narrative_ontology:cs_authority_grounding('a8b5a47b-e213-4086-a159-344b84f231ad', lineage).
narrative_ontology:cs_interpretation_layer_present('a8b5a47b-e213-4086-a159-344b84f231ad').
narrative_ontology:cs_reading_relation('a8b5a47b-e213-4086-a159-344b84f231ad', westphalian_sovereignty__conditional_sovereignty, coexists_with).
narrative_ontology:cs_reading_relation('a8b5a47b-e213-4086-a159-344b84f231ad', westphalian_sovereignty__graduated_sovereignty, influences).
narrative_ontology:cs_axiom('a8b5a47b-e213-4086-a159-344b84f231ad', foundational, sovereignty_is_binary_and_absolute).
narrative_ontology:cs_axiom_status(sovereignty_is_binary_and_absolute, holdable).
narrative_ontology:cs_axiom_grounding('a8b5a47b-e213-4086-a159-344b84f231ad', sovereignty_is_binary_and_absolute, deontological).
narrative_ontology:cs_axiom('a8b5a47b-e213-4086-a159-344b84f231ad', foundational, non_interference_is_categorically_illegitimate).
narrative_ontology:cs_axiom_status(non_interference_is_categorically_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('a8b5a47b-e213-4086-a159-344b84f231ad', non_interference_is_categorically_illegitimate, conventional).
narrative_ontology:cs_axiom('a8b5a47b-e213-4086-a159-344b84f231ad', secondary, state_consent_sole_gateway_to_international_accountability).
narrative_ontology:cs_axiom_status(state_consent_sole_gateway_to_international_accountability, overridden).
narrative_ontology:cs_axiom_grounding('a8b5a47b-e213-4086-a159-344b84f231ad', state_consent_sole_gateway_to_international_accountability, conventional).
narrative_ontology:cs_reference_frame('a8b5a47b-e213-4086-a159-344b84f231ad', westphalian_non_interference_principle).
narrative_ontology:cs_drift_state('a8b5a47b-e213-4086-a159-344b84f231ad', post_responsibility_to_protect_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a8b5a47b-e213-4086-a159-344b84f231ad', '2026-06-12T14:37:22Z').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__absolute_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, authoritarian_state_regimes).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, sovereignty_doctrine_adherents).
narrative_ontology:constraint_victim(westphalian_sovereignty__absolute_sovereignty, populations_under_repressive_regimes).
narrative_ontology:constraint_victim(westphalian_sovereignty__absolute_sovereignty, exile_and_diaspora_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, liberal_democratic_states).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, unaligned_non_aligned_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Use the absolute sovereignty doctrine as a shield against external scrutiny and intervention. Invoke non-interference to prevent sanctions, ICC jurisdiction, humanitarian intervention, and human rights monitoring. The doctrine legitimates their claim that internal repression is categorically outside the international community's purview. They benefit from the constraint's suppression of alternatives (conditional sovereignty, humanitarian intervention doctrine).
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, authoritarian_state_regimes, beneficiary,
    institutional, generational, arbitrage, global).

% Bear the direct costs of state repression without access to international legal remedies, protection, or asylum pathways that might operate if sovereignty were conditional on rights-respecting governance. They are trapped: exit through internal flight is often impossible; international intervention is ruled categorically illegitimate by the absolute sovereignty doctrine they are subject to. Their domestic suffering is redefined as sovereign prerogative beyond external accountability.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, populations_under_repressive_regimes, payer,
    powerless, biographical, trapped, global).

% Formally endorse and enforce the absolute sovereignty doctrine through UN Charter Article 2(7) while simultaneously maintaining humanitarian intervention capacity as residual option. They set and enforce the constraint through diplomatic recognition, ICJ jurisdiction acceptance, and treaty frameworks that treat state consent as inviolable. They benefit from the constraint when it protects their own internal affairs from scrutiny (surveillance, colonial legacy, resource extraction) while reserving humanitarian intervention as exceptional prerogative for geopolitically strategic situations.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, liberal_democratic_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__absolute_sovereignty, liberal_democratic_states, beneficiary).

% Are excluded from binding legal channels when they attempt to invoke international law against state-level atrocities. They can document, publicize, and advocate, but the absolute sovereignty doctrine forecloses the legal remedies their moral claims would activate under a conditional-sovereignty regime. They would dispute the reading and advocate for graduated or conditional sovereignty framings but lack the institutional power to reframe the doctrine unilaterally.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, human_rights_advocates_and_ngos, excluded,
    moderate, biographical, constrained, global).

% Produce competing readings of the UN Charter and customary international law. The absolute sovereignty reading is authoritative within mainstream IR and international law pedagogy, but scholars also articulate conditional and graduated alternatives. They observe the constraint's operation and the pressure from human rights advocates, technological surveillance, and transnational crises (pandemics, climate, migration) that strain its logic.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, international_law_scholars, observer,
    moderate, biographical, analytical, global).

% Benefit from absolute sovereignty as protection against great-power intervention. Post-colonial states invoke the doctrine to resist Western conditionality on aid and trade. However, they also experience its suppression of intervention capacity when neighboring states commit atrocities or when internal repression threatens their own populations; their benefit is conditional on not being themselves targeted by external pressure disguised as humanitarian concern.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, unaligned_non_aligned_states, beneficiary,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalian_sovereignty__absolute_sovereignty, authoritarian_state_regimes).
narrative_ontology:fixing_cost_class(westphalian_sovereignty__absolute_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates interstate non-interference by establishing that each state's internal governance is beyond the legitimate domain of external actors. Solves the collective-action problem: if every state retained the right to judge and intervene in others' domestic affairs, the international system would dissolve into constant intervention and no state's borders would be secure. Absolute sovereignty creates a stable non-intervention zone that all states can rely on.
% TRANSFER_FUNCTION: Transfers immunity-from-scrutiny from the international legal system to repressive state regimes. It redistributes the cost of internal repression: instead of distributing accountability costs evenly across regimes (through conditional sovereignty frameworks), the doctrine concentrates the protective benefit on states willing to invoke it most aggressively (typically authoritarian regimes), while concentrating costs on populations unable to exit or appeal externally.
% ABSENT_VOICES: Populations under repressive regimes are structurally excluded from the doctrine's framing — they are the sites of its application, not its authors. Their voices would reframe sovereignty as conditional on rights protection. Also excluded: future generations affected by path-dependent state decisions, ecosystems treated as sovereign property with no standing, and subnational peoples (minorities, indigenous groups, dissidents) whose claims are foreclosed by treating the state as the only legitimate international actor.
% DISAPPEARANCE_RATIONALE: If absolute sovereignty vanished and conditional/graduated sovereignty replaced it, the international legal order would reorganize: humanitarian intervention would become legitimate under specified conditions, ICC jurisdiction would expand, state consent would no longer be the sole gateway to international accountability, aid and trade agreements would carry enforceable governance conditions, and asylum/protection regimes would shift as internal repression became actionable internationally. The cost-distribution would flip: regimes currently protected would face exposure; populations currently trapped would gain remedy channels.
% FOUNDING_PROBLEM: Post-WWII and post-colonial international order needed a principle that would prevent great-power hegemony and protect newly sovereign states from external domination. Absolute sovereignty answered this: by treating the state as the ultimate legitimate authority and external interference as categorically illegitimate, the doctrine protects smaller and newly independent states from conquest, colonialism, and coercive regime change justified as humanitarian concern.
% FOUNDING_PROBLEM_CORROBORATION: Mainstream international law and IR scholarship (Waltz, Jackson, Krasner) attest the founding problem — preventing hegemonic intervention — remains live. Post-colonial states and Global South leaders attest the non-interference shield remains essential to prevent re-colonization through conditional sovereignty framing. However, human rights advocates, liberal democracies conducting selective intervention, and scholars of humanitarian law attest the founding problem (preventing hegemony) has been progressively decoupled from the enforcement mechanism (absolute non-interference); the mechanism now protects authoritarian regimes more effectively than it protects smaller states from hegemony. The Responsibility to Protect doctrine (2005) represents systematic external corroboration that the founding problem has shifted: preventing genocide and mass atrocities is now asserted as higher-order than preventing intervention.
narrative_ontology:disappearance_verdict(westphalian_sovereignty__absolute_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalian_sovereignty__absolute_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__absolute_sovereignty, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(westphalian_sovereignty__absolute_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalian_sovereignty__absolute_sovereignty, 0.52, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalian_sovereignty__absolute_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalian_sovereignty__absolute_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalian_sovereignty__absolute_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52) because the constraint does provide genuine coordination value (prevents constant intervention by powerful states) but is substantially captured by authoritarian regimes to prevent accountability. Suppression is high (0.71) because the doctrine explicitly forecloses alternatives (humanitarian intervention doctrine, conditional sovereignty framing, ICC jurisdiction over domestic matters) and delegates enforcement to state consent — enforcement machinery must actively maintain the non-interference principle against competing doctrines. Theater is moderate (0.42) because significant enforcement activity is performative: liberal democracies invoke absolute sovereignty while conducting selective humanitarian intervention (Kosovo, Libya, Syria cases show the doctrine is invoked asymmetrically). Accessibility collapse is substantial (0.68): once the absolute sovereignty doctrine is understood, the alternatives are largely invisible — one must actively contest the doctrine to access conditional sovereignty framings. Resistance is moderate-to-high (0.58): human rights advocates, ICC prosecutors, and scholars of conditional sovereignty provide consistent resistance, but the institutional entrenchment of absolute sovereignty in UN Charter limits their effectiveness. The measurement series show extractiveness and suppression rising from 1945-2010 as authoritarian regimes increasingly wielded the doctrine against accountability pressure, then plateauing as competitive doctrines (R2P, ICC jurisdiction expansion, sanctions regimes) have prevented further centralization of the absolute reading but have not replaced it. Theater ratio rises in parallel, reflecting growing disconnect between stated commitment to non-interference and actual intervention practices.
 *
 * PERSPECTIVAL GAP:
 *   From the authoritarian regime's seat: 'This is necessary coordination — without absolute sovereignty, powerful states would dominate smaller states and the international order would collapse into perpetual intervention.' From the repressed population's seat: 'This is protection of my oppressor — I am trapped, unheard, and this doctrine forecloses my access to international remedy.' From the liberal democracy's seat: 'This is legitimate coordination we helped design — and we can invoke humanitarian intervention exceptions when geopolitically necessary.' The engine computes these divergent classifications from the structural data; they are not reconcilable by further analysis. The gap IS the measurement the constraint corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary directionality: Authoritarian regimes are declared beneficiaries because they directly collect the extraction (immunity from international accountability, suppression of intervention alternatives). d ≈ 0.1 (full beneficiary): institutional power, arbitrage exit options (can threaten to leave the international system or invoke counter-doctrines), global scope. Liberal democracies are secondary beneficiaries but with ambiguous directionality (d ≈ 0.5, symmetric): they benefit from the doctrine when it protects their own internal affairs but pay costs when it shields allies or competitors from accountability; they have arbitrage exit (can conduct selective intervention while formally invoking absolute sovereignty). Post-colonial states have moderate directionality (d ≈ 0.35, moderate beneficiary): they benefit from protection against hegemonic intervention but pay costs when the doctrine is used against accountability to their own populations. Victim directionality: Populations under repressive regimes are victims (d ≈ 0.95, full target): powerless, trapped exit (cannot leave the state or access international remedy), biographical time horizon, subject to the constraint's suppression. They pay the highest cost (suppression of remedy channels) with no benefit. The constraint's extraction concentrates on powerless, trapped agents (high d agents pay high χ); liberal democracies can arbitrage (formal commitment to non-interference while conducting selective intervention), reducing their effective d below what their beneficiary status might suggest.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing hegemonic intervention) is contested — some parties attest it remains live, others attest it has been progressively solved and the constraint now primarily protects authoritarian regimes rather than maintaining international order. The disappearance verdict is world_rearranges: if absolute sovereignty vanished, the international legal order would reorganize around conditional/graduated sovereignty, humanitarian intervention would become legitimate under specified conditions, ICC jurisdiction would expand, state consent would no longer be the sole gateway to international accountability. This mandatrophy structure (founding problem contested, disappearance impacts real) suggests the constraint is in the transition zone: it started as coordination (tangled rope protecting all states from hegemony) but has been progressively captured by authoritarian regimes to prevent accountability, shifting it toward snare characteristics. The theater ratio rising (performative non-interference while conducting selective intervention) signals degradation. The measurement plateau after 2010 suggests institutional inertia — the doctrine persists because it is codified in UN Charter but its functional justification is increasingly contested and selective intervention has created a performative exception. The constraint is not yet a full piton (it still coordinates non-interference for some states) but shows piton characteristics (theatrical maintenance, captured function, atrophied original justification).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_doctrine_reading_ambiguity,
    'Is absolute sovereignty a structural feature of international law (a natural coordination principle), or a constructed doctrine benefiting powerful states and authoritarian regimes?',
    'Historical analysis of doctrine''s origin and adoption: was it discovered as natural necessity or strategically promulgated by founding powers? Counterfactual: would a cooperative international order be possible under graduated or conditional sovereignty?',
    'If constructed, the constraint is a tangled rope or snare favoring state elites; if natural, it is a mountain or rope coordinating legitimate non-interference. This reading asserts constructed status; sibling readings (conditional, graduated) challenge that assertion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_doctrine_reading_ambiguity, conceptual, 'Whether absolute sovereignty is a discovered natural principle or a constructed doctrine.').

omega_variable(
    humanitarian_intervention_foreclosure,
    'Does absolute sovereignty logically foreclose humanitarian intervention and conditional sovereignty, or do they coexist through exception and residual prerogative?',
    'Doctrinal analysis: can a state simultaneously endorse absolute sovereignty and Responsibility to Protect without internal contradiction? Practice analysis: do states that invoke Responsibility to Protect formally revise or abandon absolute sovereignty, or treat R2P as an ''exceptional'' exception that preserves the doctrine?',
    'If foreclosed: this reading is a genuine mountain; if coexisting: the reading is complicit in a cover-story permitting selective intervention disguised as non-interference. Affects whether the constraint is tangled rope or snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(humanitarian_intervention_foreclosure, conceptual, 'Logical relationship between absolute sovereignty and humanitarian intervention doctrine.').

omega_variable(
    post_colonial_sovereignty_capture,
    'Do post-colonial states that invoke absolute sovereignty benefit from the doctrine, or has the doctrine been captured by authoritarian regimes to neutralize post-colonial accountability to their own populations?',
    'Survey of post-colonial state positions: do leaders invoke absolute sovereignty primarily to resist great-power intervention, or primarily to resist domestic accountability? Compare exit options for post-colonial states that endorse conditional sovereignty versus those that endorse absolute sovereignty.',
    'If captured: authoritarian regimes are primary beneficiaries and post-colonial states are secondary beneficiaries in fragile coalition; if genuinely protective: post-colonial states are primary beneficiaries. Affects beneficiary/victim directionality and whether the constraint''s enforcement is symmetric or asymmetrically serves authoritarian regimes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_colonial_sovereignty_capture, empirical, 'Whether absolute sovereignty benefits post-colonial states equally or disproportionately protects authoritarian regimes.').

omega_variable(
    competing_kernels_sibling_classification,
    'Does this reading of westphalian_sovereignty foreclose conditional_sovereignty and graduated_sovereignty readings, or do all three coexist as live positions in the contemporary international system?',
    'Doctrinal audit: UN practices, ICJ decisions, treaty language, and state practice over 1945-2026. If absolute sovereignty is routinely violated for humanitarian purposes (R2P invocations, ICC interventions, sanctions on rights-violating regimes), then foreclosure is partial and conditional/graduated readings coexist. If absolute sovereignty has successfully excluded conditional readings from formal doctrine, foreclosure is structural.',
    'If coexists: this reading and its siblings are competing positions held by different power coalitions — relation is coexists_with. If foreclosed: this reading''s foundational axioms directly contradict the siblings — relation is forecloses (rare). Current evidence suggests coexistence through performative contradiction (states invoke absolute sovereignty while conducting selective intervention).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competing_kernels_sibling_classification, conceptual, 'Logical and institutional relationship between absolute sovereignty and sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__absolute_sovereignty, 1945, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t1945, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 1945, 0.25).
narrative_ontology:measurement_basis(west_tr_t1945, projected).
narrative_ontology:measurement(west_tr_t1975, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 1975, 0.32).
narrative_ontology:measurement_basis(west_tr_t1975, observed).
narrative_ontology:measurement(west_tr_t1995, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 1995, 0.38).
narrative_ontology:measurement_basis(west_tr_t1995, observed).
narrative_ontology:measurement(west_tr_t2010, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 2010, 0.41).
narrative_ontology:measurement_basis(west_tr_t2010, observed).
narrative_ontology:measurement(west_tr_t2020, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 2020, 0.42).
narrative_ontology:measurement_basis(west_tr_t2020, observed).
narrative_ontology:measurement(west_tr_t2026, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 2026, 0.42).
narrative_ontology:measurement_basis(west_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(west_be_t1945, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 1945, 0.35).
narrative_ontology:measurement_basis(west_be_t1945, projected).
narrative_ontology:measurement(west_be_t1975, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 1975, 0.41).
narrative_ontology:measurement_basis(west_be_t1975, observed).
narrative_ontology:measurement(west_be_t1995, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 1995, 0.48).
narrative_ontology:measurement_basis(west_be_t1995, observed).
narrative_ontology:measurement(west_be_t2010, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 2010, 0.5).
narrative_ontology:measurement_basis(west_be_t2010, observed).
narrative_ontology:measurement(west_be_t2020, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 2020, 0.51).
narrative_ontology:measurement_basis(west_be_t2020, observed).
narrative_ontology:measurement(west_be_t2026, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 2026, 0.52).
narrative_ontology:measurement_basis(west_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t1945, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 1945, 0.55).
narrative_ontology:measurement_basis(west_su_t1945, projected).
narrative_ontology:measurement(west_su_t1975, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 1975, 0.62).
narrative_ontology:measurement_basis(west_su_t1975, observed).
narrative_ontology:measurement(west_su_t1995, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 1995, 0.68).
narrative_ontology:measurement_basis(west_su_t1995, observed).
narrative_ontology:measurement(west_su_t2010, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement_basis(west_su_t2010, observed).
narrative_ontology:measurement(west_su_t2020, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 2020, 0.71).
narrative_ontology:measurement_basis(west_su_t2020, observed).
narrative_ontology:measurement(west_su_t2026, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 2026, 0.71).
narrative_ontology:measurement_basis(west_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__absolute_sovereignty, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(westphalian_sovereignty__absolute_sovereignty, 0.12).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, westphalian_sovereignty__conditional_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, westphalian_sovereignty__graduated_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, humanitarian_intervention_doctrine).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, responsibility_to_protect).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, icc_jurisdiction_and_immunity).

% DUAL FORMULATION NOTE:
% Three readings of the westphalian_sovereignty kernel: absolute_sovereignty (this file, ε ≈ 0.52, tangled rope favoring authoritarian regimes), conditional_sovereignty (sibling, ε ≈ 0.48, tangled rope favoring human rights enforcement), and graduated_sovereignty (sibling, ε ≈ 0.42, rope/tangled rope with spectrum entry/exit). Each reading instantiates the same textual kernel (UN Charter Article 2(7)) but produces structurally distinct constraints with different extraction patterns, beneficiary/victim alignments, and enforcement mechanisms. The readings are related through genealogy (absolute sovereignty is the historically prior reading, formalized post-WWII; conditional and graduated sovereignty are contemporary challenges to absolute reading) and through institutional coexistence (liberal democracies formally endorse absolute sovereignty while conducting selective intervention based on conditional/graduated principles). Network edges flow from absolute (upstream, formally authoritative) to conditional and graduated (downstream, emergent challenges); from humanitarian intervention doctrine and R2P (which are descendants of conditional reading); and to ICC jurisdiction debates (which are downstream consequences of conditional/graduated readings).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(westphalian_sovereignty__absolute_sovereignty, organized, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
