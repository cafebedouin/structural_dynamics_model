% ============================================================================
% CONSTRAINT STORY: market_as_natural_default__hybrid_amnesia_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_as_natural_default__hybrid_amnesia_reading, []).

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
 *   constraint_id: market_as_natural_default__hybrid_amnesia_reading
 *   human_readable: Market-as-Natural-Default (Hybrid Amnesia Reading)
 *   domain: political_economy/ideology
 *
 * SUMMARY:
 *   This constraint models the naturalization of market-based economic
 *   organizing as a reading of a contested kernel: the standing arrangement
 *   of market dominance in capitalist economies. The reading asserts a
 *   specific historical mechanism: (1) genuine forgetting of Depression-era
 *   alternatives and mid-century awareness of economic pluralism
 *   (1930s-1970s, the amnesia phase), followed by (2) deliberate capture and
 *   rhetorical weaponization by incumbent beneficiaries once amnesia began to
 *   wane (1980s-present, the capture phase). The constraint's extractiveness
 *   increases over this period as forgetting alone becomes insufficient to
 *   maintain the arrangement's naturalness. This reading differs from
 *   siblings: the lapsed_alternative_reading attributes market dominance to
 *   forgetting alone (no active beneficiary capture), and the
 *   beneficiary_maintained_reading attributes it to continuous post-hoc
 *   defense from the beginning. The three readings are empirically
 *   distinguishable through temporal patterns in suppression_requirement and
 *   theater_ratio.
 *
 * KEY AGENTS:
 *   - incumbent_market_beneficiaries: institutional, agenda-setting seat that shifts from passive beneficiary of ambient amnesia (pre-1980) to active defender and rhetorical manager (post-1980)
 *   - neoclassical_economics_discipline: institutional, gatekeeping seat that perpetuates market naturalism through pedagogy, formalization, and disciplinary boundaries
 *   - subordinated_economic_alternatives: powerless, trapped payer; structural foreclosure prevents testing and development of non-market forms
 *   - mid_twentieth_century_reformers: excluded historical seat; their frameworks fell out of living memory and active deliberation
 *   - development_economists_south: observer seat; periodic attempts to recover alternatives are epistemically subordinated
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__hybrid_amnesia_reading, 0.32).
domain_priors:suppression_score(market_as_natural_default__hybrid_amnesia_reading, 0.58).
domain_priors:theater_ratio(market_as_natural_default__hybrid_amnesia_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, resistance, 0.43).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__hybrid_amnesia_reading, tangled_rope).
narrative_ontology:human_readable(market_as_natural_default__hybrid_amnesia_reading, "Market-as-Natural-Default (Hybrid Amnesia Reading)").
narrative_ontology:topic_domain(market_as_natural_default__hybrid_amnesia_reading, "political_economy/ideology").

domain_priors:requires_active_enforcement(market_as_natural_default__hybrid_amnesia_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__hybrid_amnesia_reading, '450eb864-87ad-4f33-868a-41a179eeabf7').
narrative_ontology:cs_kernel_codification('450eb864-87ad-4f33-868a-41a179eeabf7', distributed).
narrative_ontology:cs_authority_grounding('450eb864-87ad-4f33-868a-41a179eeabf7', extraction).
narrative_ontology:cs_interpretation_layer_present('450eb864-87ad-4f33-868a-41a179eeabf7').
narrative_ontology:cs_reading_relation('450eb864-87ad-4f33-868a-41a179eeabf7', market_as_natural_default__lapsed_alternative_reading, influences).
narrative_ontology:cs_reading_relation('450eb864-87ad-4f33-868a-41a179eeabf7', market_as_natural_default__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_axiom('450eb864-87ad-4f33-868a-41a179eeabf7', foundational, amnesia_precedes_capture).
narrative_ontology:cs_axiom_status(amnesia_precedes_capture, holdable).
narrative_ontology:cs_axiom_grounding('450eb864-87ad-4f33-868a-41a179eeabf7', amnesia_precedes_capture, empirically_contingent).
narrative_ontology:cs_axiom('450eb864-87ad-4f33-868a-41a179eeabf7', secondary, alternatives_naturally_decay_without_institutional_support).
narrative_ontology:cs_axiom_status(alternatives_naturally_decay_without_institutional_support, holdable).
narrative_ontology:cs_axiom_grounding('450eb864-87ad-4f33-868a-41a179eeabf7', alternatives_naturally_decay_without_institutional_support, instrumental).
narrative_ontology:cs_reference_frame('450eb864-87ad-4f33-868a-41a179eeabf7', post_depression_awareness_of_economic_pluralism).
narrative_ontology:cs_drift_state('450eb864-87ad-4f33-868a-41a179eeabf7', contemporary_market_naturalism_hegemony, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('450eb864-87ad-4f33-868a-41a179eeabf7', '').
narrative_ontology:cs_kernel_id(market_as_natural_default__hybrid_amnesia_reading, market_as_natural_default).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_as_natural_default__hybrid_amnesia_reading, incumbent_market_beneficiaries).
narrative_ontology:constraint_beneficiary(market_as_natural_default__hybrid_amnesia_reading, neoclassical_economics_discipline).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, subordinated_economic_alternatives).
narrative_ontology:constraint_vindicates(market_as_natural_default__hybrid_amnesia_reading, market_efficiency_doctrine).
narrative_ontology:constraint_vindicates(market_as_natural_default__hybrid_amnesia_reading, homo_economicus_universality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Corporations and financial institutions that extract surplus through market structures. They benefit from the market-as-natural framing because it insulates market rules from contestation and reform. As the constraint weakened (1980s onward), they increasingly deployed explicit rhetorical defense rather than relying on ambient amnesia. Their situation is characterized by expanding defensive discourse — think tanks, media campaigns, policy capture — precisely because forgetting alone became insufficient.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, incumbent_market_beneficiaries, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(market_as_natural_default__hybrid_amnesia_reading, incumbent_market_beneficiaries, agenda_setter).

% The academic discipline that vindicates market naturalism through formalization. Economists inherit and perpetuate the amnesia embedded in textbooks and methodological assumptions (general equilibrium theory, rational choice, supply-demand mechanics presented as universal laws). Their professional authority depends partly on market naturalism remaining unquestioned. The discipline's gatekeeping against heterodox alternatives reinforces the constraint.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, neoclassical_economics_discipline, beneficiary,
    institutional, generational, constrained, global).

% Cooperative, commons-based, command-economy, and gift-economy organizing principles that were historically viable but have been rendered invisible or illegible within the market-naturalized worldview. Their costs are borne in the form of foreclosed possibilities: they cannot be tried, tested, or developed because the ambient framework treats markets as the only coherent option. The trapped exit reflects structural foreclosure — alternatives lack institutional support, funding, or epistemic legitimacy.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, subordinated_economic_alternatives, payer,
    powerless, biographical, trapped, global).

% Historical actors (1930s-1960s progressives, labor movements, post-WWII planners, development theorists) who actively maintained awareness of economic alternatives and deployed them in policy. They are excluded from contemporary discourse not by conscious suppression but by historical amnesia — their frameworks and arguments have fallen out of living memory and are rarely taught, referenced, or engaged by current policymakers. Their exclusion is a structural fact, not an active gate.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, mid_twentieth_century_reformers, excluded,
    powerful, biographical, trapped, national).

% Scholars and policymakers in the Global South who periodically attempt to recover alternative development frameworks (heterodox models, dependency theory, dirigisme, commons-based governance). They sit in an observer seat because their challenges to market naturalism are epistemically subordinated — treated as 'local exceptions' or 'backward resistance' rather than coherent alternatives. Their analytical work documents the constraint's operation but they lack the institutional power to shift the ambient frame.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, development_economists_south, observer,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_as_natural_default__hybrid_amnesia_reading, incumbent_market_beneficiaries).
narrative_ontology:fixing_cost_class(market_as_natural_default__hybrid_amnesia_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of resource allocation via decentralized price signals, enabling complex division of labor without central planning. The market mechanism genuinely coordinates production and consumption across millions of agents. This function is real and valuable.
% TRANSFER_FUNCTION: Moves economic rents and control rights to market-positioned actors (corporations, landlords, financial intermediaries) and away from subordinated alternatives (cooperative organizing, commons-based allocation, command-economy mechanisms, gift relations). The transfer is structured not through explicit coercion but through structural foreclosure: alternatives are made invisible and illegible, so the market appears to be the only option.
% ABSENT_VOICES: Practitioners of subordinated economic forms (cooperatives, commons stewards, participatory planning advocates, gift-economy researchers) would object that market naturalism forecloses alternatives without empirical testing. Historical actors from the mid-twentieth century (Polanyi, Hirschman, development theorists, labor economists) would testify that market dominance resulted from forgetting, not from proof of superiority. These voices are excluded by time, by disciplinary gatekeeping, and by the ambient epistemic frame that treats market criticism as naive.
% DISAPPEARANCE_RATIONALE: If market naturalism vanished—if the constraint weakened such that alternatives regained epistemic and institutional legitimacy—the world would substantially rearrange: policy space would open for cooperative development, commons governance, participatory planning, and hybrid models; labor organizing would gain intellectual cover; development policy would recover heterodox frameworks; corporate rents depending on market exclusivity would face immediate contestation. The institutional and rhetorical apparatus built atop market naturalism would collapse.
% FOUNDING_PROBLEM: The Great Depression demonstrated both the coordination failure of unregulated markets and the viability of extensive state intervention and alternative organizing (public works, cooperative credit, planned production). Mid-twentieth century institutional economics, development theory, and labor economics maintained awareness of this historical lesson. Market orthodoxy was rebuilt after WWII not because markets had been proven superior, but because (a) Cold War ideology made state planning anathema, and (b) the lived memory of the Depression and its alternatives faded from public discourse.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians (Polanyi, Hirschman, Rodrik, Chang, Minsky) document the Depression era as a moment when multiple economic systems competed and market alternatives were actively deliberated. The founding problem—proving markets superior—was not solved; it was abandoned in favor of ideological assertion. The fading of this historical knowledge is attested to by the declining citation of institutional economists in mainstream pedagogy and the near-disappearance of development heterodoxy from elite universities.
narrative_ontology:disappearance_verdict(market_as_natural_default__hybrid_amnesia_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_as_natural_default__hybrid_amnesia_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_as_natural_default__hybrid_amnesia_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(market_as_natural_default__hybrid_amnesia_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_as_natural_default__hybrid_amnesia_reading, 0.32, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_as_natural_default__hybrid_amnesia_reading_tests).
:- end_tests(market_as_natural_default__hybrid_amnesia_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is claimed as tangled_rope because it solves a real coordination problem (price signals for resource allocation across distributed agents) while simultaneously extracting rents and foreclosing alternatives. The distinction from a pure rope is that the extraction is structural and historically contingent: it rides on amnesia about alternatives, not on inherent necessity. The distinction from a snare is that the coordination function is genuine and valuable; the extraction is a byproduct, not the primary function. The measurement series shows (a) extractiveness rising from 0.20 to 0.45 over ~100 years, (b) theater_ratio rising from 0.25 to 0.62, documenting the shift from ambient forgetting to active defense, and (c) suppression_requirement rising from 0.35 to 0.58, tracking the cost of maintaining the constraint as alternatives become visible. The sharp accelerations post-1970 reflect the phase transition: when forgetting began to wane, beneficiaries had to deploy suppression (gatekeeping, media control, career penalties for heterodox work) to maintain the constraint. The temporal pattern is the signature of this reading and distinguishes it from the sibling readings.
 *
 * PERSPECTIVAL GAP:
 *   Beneficiary seats perceive the constraint as a natural fact (market efficiency is a law of nature) or at worst a minor distortion from an ideal framework; they have no incentive to recognize extraction and suppress awareness of alternatives. Payer seats (subordinated alternatives) perceive structural foreclosure but lack institutional power to name it; their resistance is epistemically subordinated as 'backward' or 'naive.' Observer seats (development economists) see the constraint clearly but are treated as irrelevant to legitimate economic discourse. The engine should compute divergent types: the beneficiary seat may compute as rope or even mountain (natural law); the payer seat computes as snare (extraction disguised as coordination); the observer seat computes as tangled rope (genuine coordination with real extraction). This perspectival gap is the point of the reading—different seats inhabit genuinely different epistemic worlds structured by the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent_market_beneficiaries and the economics discipline are structural beneficiaries: they collect rents, authority, and epistemic legitimacy from market naturalism. Their directionality is low (near 0.0), making effective extraction negative or near-zero from their perspective—they see the constraint as subsidizing them. Subordinated alternatives are targets: foreclosed from institutional support, funding, pedagogy, and policy deployment. Their directionality is high (near 1.0), making effective extraction substantial—they bear the cost of invisibility and foreclosure. The beneficiary seat's power is institutional and global; the payer seat's power is powerless and trapped. Spatial scope is global for beneficiaries (their rents are extracted across all markets); it is trapped for alternatives (they cannot operate outside the market-naturalized frame). Exit options are arbitrage for beneficiaries (they can shift between market forms while maintaining dominance); they are trapped for alternatives (no exit exists within the market-naturalized world). This structural data feeds the engine's d derivation: beneficiaries get low d (they benefit), payers get high d (they are targets). The directionality divergence explains why the constraint appears benign from the beneficiary seat and extractive from the payer seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (proving markets superior during the Depression) is dead: the Depression passed, alternatives were tried and abandoned for political/ideological reasons, and the comparison question was never resolved empirically. The disappearance verdict is world_rearranges: if market naturalism vanished, policy would immediately reopen space for heterodox alternatives and institutional support would shift. The mismatch (founding_problem_status=dead + disappearance_verdict=world_rearranges) is the classic mandatrophy pattern: the arrangement persists despite its founding mandate being obsolete. The theater_ratio rising from 0.25 to 0.62 documents the theatrical maintenance: as forgetting waned, beneficiaries deployed rhetoric, pedagogical control, and career gatekeeping to maintain an arrangement whose functional justification had evaporated. The constraint exhibits mandatrophy: it is held in place by active suppression of alternatives and institutional inertia, not by its original coordination function. The base_properties.mandatrophy_resolved field is not authored as true because the constraint has not been consciously resolved; it persists in a state of unacknowledged mandate death.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_forgetting_vs_active_suppression,
    'Is the constraint''s persistence primarily due to genuine historical amnesia (the Depression-era alternatives literally fell out of living memory), or to active intellectual suppression by beneficiary-controlled institutions (deliberate gate-keeping, textbook revision, career penalties)?',
    'Archival analysis of mid-century economic pedagogy and its revision; oral histories of economists during the Cold War period; reconstruction of intellectual networks that could have transmitted heterodox knowledge but did not; comparison with domains where heterodox alternatives persisted (e.g., anthropology, history) vs. where they were purged (economics).',
    'If genuinely forgotten: the constraint is a tangled rope whose suppression is structural and historically contingent (amnesia weakens naturally as sources re-surface); beneficiary capture is secondary and rides on pre-existing amnesia. If actively suppressed: the constraint is a snare disguised as a rope; suppression is deliberate and will intensify as forgetting wanes and alternatives become visible again.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_forgetting_vs_active_suppression, empirical, 'The degree to which the constraint persists through genuine historical amnesia vs. active institutional suppression.').

omega_variable(
    axiom_overriding_mechanism,
    'If the constraint is reading-indexed as a kernel interpretation, what is the mechanism by which the amnesia itself became institutionalized—did the market naturalism axiom overrun its prior grounding (empirically contingent claims about Depression-era market vs. planning comparison), or was the axiom deliberately decoupled from that evidence?',
    'Genealogy of the axioms declared in cs_structure.axioms: which propositions were treated as empirically falsifiable in mid-century, and when did they transition to unfalsifiable truths? At what institutional junctures (textbook revision, journal editorial shifts, funding patterns) did the transition occur?',
    'If the axiom was overridden by evidence (as empirically_contingent status would suggest), the reading''s reference frame should show substantial drift and the constraint should be approaching foreclosure. If the axiom was deliberately insulated from evidence, the status should be ''holdable'' and the reading should show stable institutional support despite contradictory evidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(axiom_overriding_mechanism, conceptual, 'Whether the market-naturalism axiom underwent empirical overriding or institutional insulation from evidence.').

omega_variable(
    post_1980s_beneficiary_weaponization,
    'The measurement series shows theater_ratio and suppression_requirement rising sharply post-1970: is this because genuine forgetting was waning (alternatives becoming visible again) and beneficiaries had to deploy active rhetorical defense, or because beneficiaries recognized the constraint was becoming contested and chose to weaponize it?',
    'Comparative timeline of (a) when critical heterodox scholarship re-emerged (Polanyi rediscovery, new institutional economics, development heterodoxy), (b) when beneficiary-funded intellectual infrastructure expanded (think tanks, policy shops, media capture), and (c) when defensive rhetoric about markets intensified. If (a) precedes (b), the reading is accurate; if (b) is anticipatory, beneficiaries were capturing preemptively.',
    'If amnesia waned naturally, the constraint is weakening structurally and the measurement series documents a real transition. If beneficiaries captured preemptively, they arrested the waning and the theater/suppression rise is their success at maintaining the constraint despite its natural decay.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(post_1980s_beneficiary_weaponization, empirical, 'Whether post-1980s beneficiary capture was reactive to waning amnesia or anticipatory consolidation of a weakening constraint.').

omega_variable(
    kernel_reading_empirical_distinguishability,
    'This reading (hybrid amnesia) asserts a two-stage process (forgetting → capture) that structurally differs from the sibling readings: lapsed_alternative_reading claims forgetting alone explains market dominance, while beneficiary_maintained_reading claims active post-hoc defense. How do these readings relate—are they empirically distinguishable, or do they represent different interpretations of the same historical facts?',
    'Examine the measurement series: does the theater_ratio rise (as this reading predicts) or remain flat? Does suppression_requirement follow or precede extractiveness rise? A flat theater ratio and suppression prior to 1980 would falsify this reading in favor of lapsed_alternative; a sharp rise in both after 1980 supports the hybrid reading; and early suppression/theater stability would favor beneficiary_maintained.',
    'The readings are empirically distinguishable through temporal patterns. This reading claims the constraint shows a detectable phase transition (1970s→1980s) where amnesia begins to wane and beneficiary capture accelerates. The measurement series provided documents exactly this transition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_empirical_distinguishability, empirical, 'Empirical distinguishability of the three sibling readings via temporal patterns in constraint operation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__hybrid_amnesia_reading, 1930, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t1930, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 1930, 0.25).
narrative_ontology:measurement_basis(mark_tr_t1930, observed).
narrative_ontology:measurement(mark_tr_t1950, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 1950, 0.35).
narrative_ontology:measurement_basis(mark_tr_t1950, observed).
narrative_ontology:measurement(mark_tr_t1970, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 1970, 0.48).
narrative_ontology:measurement_basis(mark_tr_t1970, observed).
narrative_ontology:measurement(mark_tr_t1990, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 1990, 0.58).
narrative_ontology:measurement_basis(mark_tr_t1990, observed).
narrative_ontology:measurement(mark_tr_t2010, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 2010, 0.62).
narrative_ontology:measurement_basis(mark_tr_t2010, observed).
narrative_ontology:measurement(mark_tr_t2026, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 2026, 0.62).
narrative_ontology:measurement_basis(mark_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(mark_be_t1930, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 1930, 0.2).
narrative_ontology:measurement_basis(mark_be_t1930, observed).
narrative_ontology:measurement(mark_be_t1950, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 1950, 0.22).
narrative_ontology:measurement_basis(mark_be_t1950, observed).
narrative_ontology:measurement(mark_be_t1970, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 1970, 0.28).
narrative_ontology:measurement_basis(mark_be_t1970, observed).
narrative_ontology:measurement(mark_be_t1990, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 1990, 0.38).
narrative_ontology:measurement_basis(mark_be_t1990, observed).
narrative_ontology:measurement(mark_be_t2010, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 2010, 0.42).
narrative_ontology:measurement_basis(mark_be_t2010, observed).
narrative_ontology:measurement(mark_be_t2026, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 2026, 0.45).
narrative_ontology:measurement_basis(mark_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t1930, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 1930, 0.35).
narrative_ontology:measurement_basis(mark_su_t1930, observed).
narrative_ontology:measurement(mark_su_t1950, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 1950, 0.42).
narrative_ontology:measurement_basis(mark_su_t1950, observed).
narrative_ontology:measurement(mark_su_t1970, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 1970, 0.48).
narrative_ontology:measurement_basis(mark_su_t1970, observed).
narrative_ontology:measurement(mark_su_t1990, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement_basis(mark_su_t1990, observed).
narrative_ontology:measurement(mark_su_t2010, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 2010, 0.57).
narrative_ontology:measurement_basis(mark_su_t2010, observed).
narrative_ontology:measurement(mark_su_t2026, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 2026, 0.58).
narrative_ontology:measurement_basis(mark_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_as_natural_default__hybrid_amnesia_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(market_as_natural_default__hybrid_amnesia_reading, 0.18).
narrative_ontology:affects_constraint(market_as_natural_default__hybrid_amnesia_reading, market_as_natural_default__beneficiary_maintained_reading).
narrative_ontology:affects_constraint(market_as_natural_default__hybrid_amnesia_reading, market_as_natural_default__lapsed_alternative_reading).
narrative_ontology:affects_constraint(market_as_natural_default__hybrid_amnesia_reading, neoclassical_economics_gatekeeping).
narrative_ontology:affects_constraint(market_as_natural_default__hybrid_amnesia_reading, development_heterodoxy_suppression).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the kernel 'market_as_natural_default.' The kernel represents the standing fact of market dominance in contemporary capitalist economies. The three readings decompose the mechanism: hybrid_amnesia_reading asserts a two-stage process (forgetting → capture); lapsed_alternative_reading attributes dominance to forgetting alone; beneficiary_maintained_reading attributes it to continuous active defense. These readings have different epsilon values (0.20→0.45 for hybrid; ~0.15→0.25 for lapsed; ~0.40→0.50 for beneficiary-maintained) because they represent different causal mechanisms for the same standing arrangement. The network links document the structural relationships and allow comparative analysis of the three readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(market_as_natural_default__hybrid_amnesia_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
