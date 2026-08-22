% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__sovereignty_maximalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rbio_sovereignty_maximalist, []).

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
 *   constraint_id: rbio_practice_norm_complex__sovereignty_maximalist_reading
 *   human_readable: RBIO Norm Complex: Sovereignty Maximalist Reading
 *   domain: international_relations/international_law
 *
 * SUMMARY:
 *   This constraint instantiates the sovereignty-maximalist reading of the
 *   contested RBIO (rights-based international order) norm complex. The
 *   reading asserts that state sovereignty is absolute and inviolable; RBIO
 *   norms (humanitarian intervention, human rights conditionality,
 *   international accountability) are legitimate only when they protect state
 *   sovereignty against external interference. Humanitarian exceptions are
 *   interpreted as pretexts for powerful states to justify regime change and
 *   territorial interference under moral cover. The reading benefits
 *   authoritarian and post-colonial regimes by blocking external intervention
 *   and accountability mechanisms; it harms populations trapped under
 *   repressive governments with no external recourse. The constraint is NOT
 *   claimed here as a natural law (sovereignty is not a physical fact) but as
 *   a contested doctrinal position institutionalized through UNSC veto
 *   patterns, diplomatic coalitions, and legal doctrine. The reading competes
 *   with liberal-institutional readings (which treat humanitarian norms as
 *   universally legitimate when consent-based and multilaterally authorized)
 *   and hegemonic-extraction readings (which treat RBIO norms as a frozen
 *   hegemonic project formally revisable but practically impossible to
 *   amend). This story generates the constraint as it operates under the
 *   sovereignty-maximalist interpretation — the referent ε is assessed by
 *   that reading's own lights, not by the alternative readings.
 *
 * KEY AGENTS:
 *   - authoritarian_regime_governments: institutional power; benefit from intervention immunity; agenda-setters of the norm through diplomatic coalitions and UNSC veto
 *   - populations_under_repressive_rule: powerless; trapped; bear costs of intervention immunity through exposure to state violence without external recourse
 *   - minority_groups_facing_atrocities: powerless; immediate time horizon; trapped; face both state violence and norm-based immunity from humanitarian intervention
 *   - permanent_security_council_members: institutional power; enforce the norm through veto and coordinate with authoritarian beneficiaries
 *   - liberal_democratic_states: powerful but constrained; excluded from enforcement of humanitarian norms they prioritize; absorb diplomatic costs
 *   - international_humanitarian_institutions: moderate power, constrained exit; can document abuses but cannot independently intervene without state cooperation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.68).
domain_priors:suppression_score(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.72).
domain_priors:theater_ratio(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__sovereignty_maximalist_reading, tangled_rope).
narrative_ontology:human_readable(rbio_practice_norm_complex__sovereignty_maximalist_reading, "RBIO Norm Complex: Sovereignty Maximalist Reading").
narrative_ontology:topic_domain(rbio_practice_norm_complex__sovereignty_maximalist_reading, "international_relations/international_law").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__sovereignty_maximalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__sovereignty_maximalist_reading, '5bc3678d-f368-46dc-b899-d64f1e8615b0').
narrative_ontology:cs_kernel_codification('5bc3678d-f368-46dc-b899-d64f1e8615b0', formalized).
narrative_ontology:cs_authority_grounding('5bc3678d-f368-46dc-b899-d64f1e8615b0', extraction).
narrative_ontology:cs_interpretation_layer_present('5bc3678d-f368-46dc-b899-d64f1e8615b0').
narrative_ontology:cs_reading_relation('5bc3678d-f368-46dc-b899-d64f1e8615b0', rbio_practice_norm_complex__liberal_institutional_reading, coexists_with).
narrative_ontology:cs_reading_relation('5bc3678d-f368-46dc-b899-d64f1e8615b0', rbio_practice_norm_complex__hegemonic_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('5bc3678d-f368-46dc-b899-d64f1e8615b0', foundational, state_sovereignty_absolute_and_unconditional).
narrative_ontology:cs_axiom_status(state_sovereignty_absolute_and_unconditional, holdable).
narrative_ontology:cs_axiom_grounding('5bc3678d-f368-46dc-b899-d64f1e8615b0', state_sovereignty_absolute_and_unconditional, deontological).
narrative_ontology:cs_axiom('5bc3678d-f368-46dc-b899-d64f1e8615b0', foundational, humanitarian_exception_claims_are_regime_change_pretexts).
narrative_ontology:cs_axiom_status(humanitarian_exception_claims_are_regime_change_pretexts, holdable).
narrative_ontology:cs_axiom_grounding('5bc3678d-f368-46dc-b899-d64f1e8615b0', humanitarian_exception_claims_are_regime_change_pretexts, empirically_contingent).
narrative_ontology:cs_reference_frame('5bc3678d-f368-46dc-b899-d64f1e8615b0', mutual_non_interference_sovereignty_protection).
narrative_ontology:cs_drift_state('5bc3678d-f368-46dc-b899-d64f1e8615b0', contemporary_liberal_humanitarian_pressure_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5bc3678d-f368-46dc-b899-d64f1e8615b0', '').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__sovereignty_maximalist_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__sovereignty_maximalist_reading, authoritarian_regime_governments).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__sovereignty_maximalist_reading, populations_under_repressive_rule).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__sovereignty_maximalist_reading, minority_groups_facing_atrocities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__sovereignty_maximalist_reading, liberal_democratic_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Regimes that exercise repressive control over their populations benefit from the sovereignty maximalist norm: it shields them from international intervention, humanitarian accountability, and external pressure to reform. They actively invoke the norm in diplomatic forums, coordinate defensive rhetoric with peer regimes, and selectively reference it to block interventions while pursuing their own security interests. They exit by joining coalitions that affirm the norm and by leveraging institutional veto power where available (e.g., UNSC permanent membership).
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, authoritarian_regime_governments, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__sovereignty_maximalist_reading, authoritarian_regime_governments, agenda_setter).

% Citizens in states claiming sovereignty maximalism bear the costs of the norm directly: they cannot exit their state of residence without state permission, cannot appeal to international mechanisms without being labeled traitors or separatists, and have no recourse when domestic governance becomes abusive. The norm insulates the state from external pressure that might otherwise constrain repression.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, populations_under_repressive_rule, payer,
    powerless, immediate, trapped, local).

% Ethnic, religious, and political minorities targeted by state violence face the same entrenchment, but with acute urgency. They cannot organize internationally without state security apparatus interception, cannot seek asylum without violating the sovereignty norm (which treats refuge as interference), and have no mechanism to trigger humanitarian exception without first experiencing atrocity levels of violence — by which point exit may be blocked.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, minority_groups_facing_atrocities, payer,
    powerless, immediate, trapped, local).

% States that prioritize humanitarian norms and liberal international law are structurally constrained by sovereignty maximalism: they can mount interventions only on narrow self-defense or UNSC grounds, not on humanitarian imperative. They absorb diplomatic costs when enforcing humanitarian norms against sovereignty-maximalist opposition. They have institutional voice (UNGA, UNHRC) but lack enforcement power when authoritarian blocs align against humanitarian exceptions.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, liberal_democratic_states, payer,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__sovereignty_maximalist_reading, liberal_democratic_states, excluded).

% The International Committee of the Red Cross, UN humanitarian agencies, and human rights bodies operate under sovereignty constraints: they can document abuses but cannot independently intervene, can only appeal to state actors for access, and can be expelled by regimes invoking sovereignty. Their effective power is advisory and conditional on state cooperation.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, international_humanitarian_institutions, excluded,
    moderate, biographical, constrained, global).

% P5 states (especially those with authoritarian governance structures or close ties to authoritarian regimes) enforce the sovereignty norm by wielding veto power in the UNSC: they block humanitarian interventions and enforcement actions, cite sovereignty in opposing sanctions regimes, and coordinate with authoritarian beneficiaries to maintain the norm. They exit the norm's constraints through veto power and through military intervention in their own spheres when sovereignty maximalism serves their interests.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, permanent_security_council_members, agenda_setter,
    institutional, generational, arbitrage, global).

% International lawyers, scholars, and doctrine-setting institutions (ICJ, treaty bodies, legal academies) witness the norm's operation and interpret its meaning. The sovereignty maximalist reading competes with liberal and hegemonic readings for doctrinal authority. Doctrinal bearers do not directly participate in enforcement but shape the legitimate language in which the norm is invoked.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, international_legal_doctrine_bearers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rbio_practice_norm_complex__sovereignty_maximalist_reading, authoritarian_regime_governments).
narrative_ontology:fixing_cost_class(rbio_practice_norm_complex__sovereignty_maximalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a system of mutual state non-interference: each state recognizes others' sovereign authority over territory and population in exchange for immunity from external intervention. This solves the collective action problem of preventing powerful states from intervening in weaker states' affairs under various justifications.
% TRANSFER_FUNCTION: Transfers intervention authority and humanitarian exception power from the international community back to the state: in exchange for respecting non-interference, states gain immunity from humanitarian intervention, conditional assistance, and external accountability mechanisms. The flow is directional: from populations toward state authority, and from international institutions toward P5 veto-holders.
% ABSENT_VOICES: Trapped populations and asylum seekers cannot participate in the international forums where the norm is articulated because the norm itself prevents external representation of their interests. Humanitarian organizations and human rights bodies are formally present (UNHRC, UNGA) but are systematically outvoted by sovereignty-maximalist coalitions. Stateless persons have no seat at all.
% DISAPPEARANCE_RATIONALE: If sovereignty maximalism vanished and humanitarian intervention authority was legitimized, the institutional distribution of power would shift dramatically. Authoritarian regimes would face intervention threats and accountability mechanisms. Populations would gain appeal channels and asylum pathways. International institutions would exercise enforcement authority currently blocked by veto and doctrine. The governance structure of international relations would reorganize around humanitarian conditionality rather than absolute sovereignty.
% FOUNDING_PROBLEM: Prevention of hegemonic intervention: powerful states using humanitarian, civilizing, or moral pretexts to justify territorial conquest, regime change, and resource extraction from weaker states. Historical referent: European imperialism justified as civilizing mission; Cold War interventions justified as preventing communist expansion; post-Cold War interventions justified as humanitarian necessity.
% FOUNDING_PROBLEM_CORROBORATION: The sovereignty-maximalist reading affirms the founding problem as live and worsening: Iraq 2003 (WMD pretense for regime change), Libya 2011 (humanitarian intervention leading to state collapse and proxy war), Syria interventions (all parties cite humanitarian language to justify geopolitically motivated operations). Post-colonial states, BRICS+ members, and sovereignty-maximalist-aligned doctrine bearers consistently attest to the problem's persistence. The liberal-institutional reading contests this: it acknowledges Iraq and Libya as instances of hegemonic misuse but argues the problem is not humanitarian intervention authority itself but selective enforcement by hegemonic powers — the solution is stronger institutional constraints (UNSC consensus, time limits, post-intervention accountability) rather than rejection of humanitarian exception doctrine. Hegemonic-extraction reading affirms the problem but locates it differently: the problem is not humanitarian intervention but the frozen structure of the UNSC that permits P5 to block interventions selectively. Outside the benefiting parties, post-colonial scholars and diplomats consistently affirm the founding problem's persistence; liberal Western scholars split on whether the problem has been solved or requires reforms.
narrative_ontology:disappearance_verdict(rbio_practice_norm_complex__sovereignty_maximalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(rbio_practice_norm_complex__sovereignty_maximalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rbio_practice_norm_complex__sovereignty_maximalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rbio_practice_norm_complex__sovereignty_maximalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rbio_practice_norm_complex__sovereignty_maximalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rbio_practice_norm_complex__sovereignty_maximalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rbio_practice_norm_complex__sovereignty_maximalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The sovereignty-maximalist reading measures as a tangled rope because it solves a genuine coordination problem (preventing hegemonic intervention under humanitarian pretexts) while simultaneously enabling asymmetric extraction (blocking humanitarian recourse for populations). Base extractiveness (0.68) reflects the reading's core claim: the norm extracts immunity for authoritarian regimes at the cost of trapped populations and blocked interventions. Suppression (0.72) is high because maintaining the norm requires active UNSC veto coordination, institutional position-taking, and counter-humanitarian doctrine to suppress intervention claims. Theater ratio (0.41) indicates that humanitarian language persists in UN discourse but actual enforcement decisions remain geopolitically determined — performance masks mechanism. Accessibility collapse (0.62) reflects that populations face highly collapsed alternatives: exit requires state permission (trapped), appeal requires violating sovereignty (blocked), and external assistance requires intervention (suppressed). Resistance (0.71) is substantial because liberal states, humanitarian organizations, and intervention advocates actively contest the norm — the measurement series documents increasing resistance as humanitarian exceptions attempt to break through. The measurement trajectory (1945–2025) shows extractiveness rising from 0.42 (when colonialism competed with sovereignty norms) to 0.68 (when BRICS+ coordination solidifies the maximalist reading). Suppression requirement rises from 0.35 (when enforcement machinery was weak) to 0.72 (when systematic veto and counter-doctrine are entrenched). Theater rises from 0.15 to 0.41 as humanitarian language becomes increasingly detached from enforcement outcomes. This is one shared time grid: every metric is authored at every examined time point (1945, 1965, 1990, 2003, 2015, 2025).
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (authoritarian regimes and P5 members) compute the norm as protective coordination: it shields their authority from external pressure and ensures mutual non-interference. From their position the extraction is a feature, not a bug — the norm exists to prevent intervention. The payer seats (trapped populations, minorities facing atrocities, liberal states constrained by the norm) compute it as enforced extraction: the norm operates to block their access to international mechanisms while beneficiaries are insulated from accountability. From their position the coordination function is overshadowed by the extraction mechanism. The engine should compute substantially different types across seats: from the P5 member's position the constraint may appear as rope or even scaffold (transitional coordination mechanism). From the powerless population's position it is snare (pure extraction with no genuine coordination benefit to them). The divergence is the measurement the framework exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Authoritarian regimes are full beneficiaries (d ≈ 0.0): they collect intervention immunity, block accountability mechanisms, and exercise veto power. P5 members are near-beneficiaries (d ≈ 0.2): they benefit from veto power and can enforce it selectively in their spheres of interest while invoking humanitarian exceptions when strategic interests align. Liberal democratic states are symmetric to slightly extractive (d ≈ 0.45–0.55): they benefit from sovereignty norm stability (it constrains rival powers) but are constrained from deploying humanitarian intervention authority they prioritize. Trapped populations are full targets (d ≈ 1.0): they bear the constraint through blocked exit, blocked appeal, and blocked external assistance. The directionality derivation flows from beneficiary/victim declarations plus the structural exit options: beneficiaries (authoritarian regimes) have arbitrage exit (they can coordinate coalitions to shift enforcement) and institutional power (they wield veto). Victims (trapped populations) have trapped or identity-locked exit options (they cannot leave the state or appeal to international authority without violating sovereignty). Liberal states have constrained exit (they can conduct interventions on narrow UNSC grounds but cannot establish humanitarian exception authority). This asymmetry in structural positions generates the directionality spread.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing hegemonic intervention under humanitarian pretexts) is contested: sovereignty-maximalist readings affirm it as live and growing; liberal readings argue it has been solved by institutional constraints (UNSC consensus requirement, time-limited mandates); hegemonic readings argue the problem is not humanitarian intervention but selective enforcement of humanitarian norms. The disappearance verdict is world_rearranges: if the sovereignty-maximalist norm vanished, authoritarian regimes would face intervention threats, populations would gain appeal mechanisms, and international accountability would expand. This divergence between founding_problem_status=contested and disappearance_verdict=world_rearranges indicates the constraint has not resolved its original mandate dispute — both sides marshal evidence that the constraint persists because it benefits some parties (authoritarian regimes, P5 members) rather than because it solves a live coordination problem. The classification as tangled_rope reflects this: it coordinates mutual non-interference (rope function) while extracting immunity for authoritarian actors (extraction function). The measuring machinery should detect whether the rope function or the extraction function is primary — if extraction dominates across seats, the classification could shift toward snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    humanitarian_exception_instrumentalism,
    'Do humanitarian exceptions from sovereignty represent genuine humanitarian commitment or instrumental deployment of humanitarian language to justify geopolitically motivated regime change?',
    'Comparative institutional analysis: examine which humanitarian crises trigger intervention and which do not. Cross-tabulate crisis severity, humanitarian urgency, and strategic interest; test whether strategic interest predicts intervention better than humanitarian severity. Post-intervention outcome analysis: measure whether interventions produce humanitarian improvement or geopolitical realignment.',
    'If humanitarian language masks instrumental geopolitical deployment, the sovereignty-maximalist claim (''humanitarian exceptions are pretexts for regime change'') is empirically validated and the extraction mechanism is confirmed. The constraint measures correctly as tangled rope with high extraction. If humanitarian interventions consistently produce humanitarian improvement regardless of strategic interest, the reading''s empirical claim weakens and the core objection to humanitarian exceptions must shift from ''they are pretexts'' to ''intervention violates sovereignty regardless of outcome,'' altering the reading''s theoretical foundation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(humanitarian_exception_instrumentalism, empirical, 'Instrumentalism of humanitarian exception language in justifying interventions.').

omega_variable(
    sovereignty_absolutism_conditionality,
    'Is absolute state sovereignty logically or historically unconditional, or does the sovereignty-maximalist doctrine itself contain hidden conditions (e.g., state capacity threshold, atrocity-scale violation threshold) that make absolutism conditional in practice?',
    'Doctrinal genealogy: examine the history of sovereignty doctrine in the maximalist tradition — do authoritarian state theorists ever acknowledge conditions under which sovereignty may be legitimately challenged? Institutional hypocrisy test: do sovereignty-maximalist-aligned states themselves invoke exception conditions when intervention serves their interests (e.g., failing state, atrocity prevention in spheres where they intervene)? If exceptions appear in their own practice, absolutism is conditional.',
    'If sovereignty absolutism is genuinely unconditional and historically unprecedented, the reading maintains logical coherence and can be held consistently. If it contains hidden or context-dependent conditions, then the extraction mechanism becomes visible: the reading permits humanitarian exception blocked only when it disadvantages the authoritarian beneficiaries. The reading would shift from coherent doctrine to incoherent doctrine-as-cover.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_absolutism_conditionality, conceptual, 'Whether absolute sovereignty is logically unconditional or contains hidden conditionality.').

omega_variable(
    kernel_reading_coherence,
    'Is the sovereignty-maximalist doctrine a coherent reading of the shared RBIO kernel (UN Charter + humanitarian law + UNSC structure), or is it a fundamentally different normative commitment that rejects the kernel''s premises?',
    'Examine the shared commitments across readings: do all three readings affirm state consent as primary legitimacy source? Do they share a common treaty referent (UN Charter) or do they derive from different foundational documents? If they share a kernel and read it differently, this is intra-doctrinal contest. If they reject each other''s foundational premises, this is a separate doctrine, not a reading.',
    'If sovereignty-maximalist is a coherent reading of the shared RBIO kernel, then the constraint family structure is correct: three readings of one kernel, each instantiating a different constraint. The constraint competes through doctrinal interpretation and institutional outcomes. If it is a separate doctrine rejecting the kernel''s premises, then it represents a fundamental rupture in international legal doctrine, not a reading-level disagreement. This omega documents whether the committer frame (treating this as a kernel reading) is appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_coherence, conceptual, 'Coherence of sovereignty-maximalist as a reading vs. as a separate doctrine.').

omega_variable(
    trapped_population_suppression_mechanism,
    'Is the measured suppression (0.72) of trapped populations structurally enforced (border controls, legal barriers) or internalized (populations have internalized state authority; suppression persists psychologically after structural barriers are removed)?',
    'Refugee flow analysis: compare emigration rates in high-suppression states against stated desire to emigrate (polling, demonstrated preference in exit behavior when barriers lower). Post-regime-change observation: when structural suppression is removed (regime fall, border opening), do exit and appeal behaviors change rapidly (indicating structural suppression) or persist (indicating internalized suppression)? Syrian refugee behavior post-2011: rapid exit when crossing became possible indicates structural suppression was primary mechanism.',
    'If suppression is structurally enforced, the effective suppression reported (0.72) accurately reflects the constraint''s extraction mechanism. If suppression is substantially internalized, the true suppression is higher than measured — populations carry the constraint with them after exit. This affects severity assessment and the long-term trajectory of the constraint: internalized suppression is harder to dissolve through regime change alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trapped_population_suppression_mechanism, empirical, 'Structural vs. internalized suppression mechanism for trapped populations.').

omega_variable(
    rigged_kernel_reading_selection,
    'Is the sovereignty-maximalist reading selected as the true interpretation of the RBIO kernel because it accurately reflects the kernel''s meaning, or because authoritarian actors have successfully rigged institutional interpretation in their favor through UNSC veto and diplomatic coalitions?',
    'Historical textual analysis: examine the UN Charter''s original intent, drafting history, and foundational documents — what did the architects intend regarding humanitarian exception authority? Comparative doctrine: what do international law schools teach as the standard interpretation? Which reading aligns with the charter''s text when read neutrally? Institutional power analysis: has the maximalist reading gained ground through superior argument or through increasing institutional power of its beneficiaries?',
    'If the reading accurately reflects the kernel''s meaning, then this story correctly instantiates one true interpretation of RBIO. If institutional power has rigged the interpretation, then the reading is a constraint (yes, it operates and extracts) but the kernel''s true meaning may differ — the constraint measures as snare or tangled rope but the kernel reading is false. This omega addresses whether the committer frame correctly attributes the reading to the kernel or whether power has corrupted the interpretive process.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rigged_kernel_reading_selection, conceptual, 'Whether the sovereignty-maximalist reading reflects true kernel meaning or institutional power capture of interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__sovereignty_maximalist_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_sov_max_tr_t1945, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 1945, 0.15).
narrative_ontology:measurement(rbio_sov_max_tr_t1965, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 1965, 0.22).
narrative_ontology:measurement(rbio_sov_max_tr_t1990, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 1990, 0.28).
narrative_ontology:measurement(rbio_sov_max_tr_t2003, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 2003, 0.36).
narrative_ontology:measurement(rbio_sov_max_tr_t2015, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(rbio_sov_max_tr_t2025, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 2025, 0.41).

% Extraction over time
narrative_ontology:measurement(rbio_sov_max_be_t1945, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 1945, 0.42).
narrative_ontology:measurement(rbio_sov_max_be_t1965, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 1965, 0.48).
narrative_ontology:measurement(rbio_sov_max_be_t1990, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(rbio_sov_max_be_t2003, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 2003, 0.62).
narrative_ontology:measurement(rbio_sov_max_be_t2015, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 2015, 0.65).
narrative_ontology:measurement(rbio_sov_max_be_t2025, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(rbio_sov_max_su_t1945, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 1945, 0.35).
narrative_ontology:measurement(rbio_sov_max_su_t1965, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 1965, 0.52).
narrative_ontology:measurement(rbio_sov_max_su_t1990, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 1990, 0.58).
narrative_ontology:measurement(rbio_sov_max_su_t2003, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 2003, 0.68).
narrative_ontology:measurement(rbio_sov_max_su_t2015, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 2015, 0.7).
narrative_ontology:measurement(rbio_sov_max_su_t2025, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rbio_practice_norm_complex__sovereignty_maximalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.12).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__sovereignty_maximalist_reading, rbio_practice_norm_complex__liberal_institutional_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__sovereignty_maximalist_reading, rbio_practice_norm_complex__hegemonic_extraction_reading).

% DUAL FORMULATION NOTE:
% The rbio_practice_norm_complex kernel decomposes into three constraint stories, each instantiating one reading of the contested RBIO norm. Sovereignty maximalist (this story) asserts absolute sovereignty and interprets humanitarian exceptions as pretexts. Liberal institutional asserts universal humanitarian norms legitimized through consent-based multilateral processes. Hegemonic extraction asserts RBIO norms as a frozen hegemonic project. Each reading has different ε (0.68 for maximalist, lower for liberal, higher for hegemonic), different beneficiary/victim structures, and different computed types. The stories are linked through network.affects_constraints and share references to a common kernel (UN Charter, UNSC structure, humanitarian law treaties) but instantiate fundamentally different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rbio_practice_norm_complex__sovereignty_maximalist_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
